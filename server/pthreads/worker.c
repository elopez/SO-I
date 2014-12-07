#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "clist.h"
#include "spawner.h"
#include "hashtable.h"

#define MODULE "worker: "
#define BUFF_SIZE 8192
#define writeconst(con, str) write((con), (str), strlen((str)))

#if __GNUC__ > 2
# define unused __attribute__((__unused__))
#else
# define unused
#endif

static HashTable *files;
static pthread_mutex_t files_lock = PTHREAD_MUTEX_INITIALIZER;

static CList *workers = CLIST_INITIALIZER;
static pthread_mutex_t workers_ipc_lock = PTHREAD_MUTEX_INITIALIZER;
static int worker_qty = 0;

static int fd_used = 0;

static pthread_mutex_t worker_mode_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t worker_mode_enabled;
static int worker_mode = 0;

enum {
	/* user operations */
	OP_CRE,	/* create a file */
	OP_OPN, /* open a file */
	OP_REA, /* read from a file */
	OP_WRT, /* write to a file */
	OP_CLO, /* close a file */
	OP_LSD, /* list directory */
	OP_DEL, /* delete a file */
	OP_BYE, /* finalizes a connection */
	/* worker operations */
	WO_WMO, /* enter worker mode */
	WO_EWM, /* exit worker mode */
	WO_WLN, /* link filename on another worker */
	WO_WOP, /* open file on another worker */
	WO_WRR, /* request a read on another worker */
	WO_WRW, /* request a write on another worker */
	WO_WCL, /* close file on another worker */
	WO_WDE, /* delete file on another worker */
	/* dispatcher operations */
	DI_SEC, /* enable secure mode for this connection */
};

struct filedata {
	char *name;		/* file name, also key on the HT */
	char *data;		/* file storage */
	unsigned int open;	/* fd, 0 if closed, -1 if remote */
	unsigned int pos;	/* read position or worker fd if remote */
	unsigned int used;	/* # of used chars from data */
	unsigned int size;	/* # of chars available on data, -1 if remote */
};

#define IS_STORED_REMOTELY(data)	((data)->size == -1U)

static int operation(const char *line)
{
	if (strncmp("CRE", line, 3) == 0)
		return OP_CRE;
	else if (strncmp("OPN", line, 3) == 0)
		return OP_OPN;
	else if (strncmp("REA", line, 3) == 0)
		return OP_REA;
	else if (strncmp("WRT", line, 3) == 0)
		return OP_WRT;
	else if (strncmp("CLO", line, 3) == 0)
		return OP_CLO;
	else if (strncmp("LSD", line, 3) == 0)
		return OP_LSD;
	else if (strncmp("DEL", line, 3) == 0)
		return OP_DEL;
	else if (strncmp("BYE", line, 3) == 0)
		return OP_BYE;
	else if (strncmp("WMO", line, 3) == 0)
		return WO_WMO;
	else if (strncmp("EWM", line, 3) == 0)
		return WO_EWM;
	else if (strncmp("WLN", line, 3) == 0)
		return WO_WLN;
	else if (strncmp("WOP", line, 3) == 0)
		return WO_WOP;
	else if (strncmp("WRR", line, 3) == 0)
		return WO_WRR;
	else if (strncmp("WRW", line, 3) == 0)
		return WO_WRW;
	else if (strncmp("WCL", line, 3) == 0)
		return WO_WCL;
	else if (strncmp("WDE", line, 3) == 0)
		return WO_WDE;
	else if (strncmp("SEC", line, 3) == 0)
		return DI_SEC;

	return -1;
}

/**
 * Sends a query to all workers in the same order as they were loaded.
 * Workers must be in worker mode if query is not "WMO\n"
 * @param[in]	query	Query to send to all workers
 * @param[in]	rwmode	0 = query and return error replies
 * 			1 = write mode, return fd used
 * 			2 = read mode, return fd used and copy answer to out
 * @param[out]	out	fd to copy the answer to if rwmode == 2
 * @return see rwmode
 */
static int query_all_workers(const char *query, int rwmode, int out)
{
	int fd, res, ret, len;
	char buff[BUFF_SIZE];
	CList *head = workers;
	CList *curr = workers;

	len = strlen(query);
	ret = 0;

	do {
		fd = clist_data(curr, int);

		write(fd, query, len);

		res = 0;
		buff[0] = '\0';
		while (memchr(buff, '\n', res) == NULL)
			res += read(fd, buff+res, BUFF_SIZE-res);

		if (strncmp("OK", buff, 2) != 0) {
			ret += 1;
		} else if (rwmode) {
			if (rwmode == 2)
				write(out, buff, res);
			return fd;
		}

		curr = clist_next(curr);
	} while (head != curr);

	return ret;
}

/**
 * Switches all workers to worker mode
 */
static inline void all_to_worker_mode(void)
{
	/* IPC sockets are shared among threads, using them concurrently
	 * is asking for trouble :) */
	pthread_mutex_lock(&workers_ipc_lock);
	query_all_workers("WMO\n", 0, 0);
}

/**
 * Switches all workers to client mode
 */
static inline void all_to_client_mode(void)
{
	query_all_workers("EWM\n", 0, 0);
	pthread_mutex_unlock(&workers_ipc_lock);
}

static void worker_request_write(struct filedata *data, char *content, unsigned int len)
{
	int buflen = (len + 100) * sizeof(char);
	int startlen;
	int res = 0;
	char *buff = malloc(buflen); /* TODO */

	startlen = snprintf(buff, buflen, "WRW %s SIZE %u ", data->name, len);
	memcpy(buff + startlen, content, len);
	memcpy(buff + startlen + len, "\n\0", 2);

	/* fast path - we know who holds the file */
	if (data->pos) {
		write(data->pos, buff, startlen + len + 1);
		buff[0] = '\0';
		while (memchr(buff, '\n', res) == NULL)
			res += read(data->pos, buff+res, BUFF_SIZE-res);
	} else { /* slow path - let's ask around (and cache it for next write) */
		data->pos = query_all_workers(buff, 1, 0);
	}

	free(buff);
	return;
}

static void worker_request_read(int conn, struct filedata *data, unsigned int len)
{
	char buff[BUFF_SIZE]; /* todo */
	int buflen;
	int res = 0;

	buflen = snprintf(buff, 100, "WRR %s SIZE %u\n", data->name, len);

	/* fast path - we know who holds the file */
	if (data->pos) {
		write(data->pos, buff, buflen);
		buff[0] = '\0';
		while (memchr(buff, '\n', res) == NULL)
			res += read(data->pos, buff+res, BUFF_SIZE-res);
		write(conn, buff, res);
	} else { /* slow path - let's ask around (and cache it for next write) */
		data->pos = query_all_workers(buff, 2, conn);
	}
}

static void worker_create_file(int conn, char *name)
{
	struct filedata *data;
	char buff[BUFF_SIZE];

	all_to_worker_mode();

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);
	pthread_mutex_unlock(&files_lock);

	if (data != NULL) {
		writeconst(conn, "ERROR 17 EEXIST\n");
	} else {
		/* the filename hashtable is kept in sync so if we don't
		 * have it then nobody does. Let's create it. */

		snprintf(buff, BUFF_SIZE, "WLN %s\n", name);
		if (query_all_workers(buff, 0, 0)) {
			fprintf(stderr, "FILESYSTEM INTEGRITY HAS BEEN COMPROMISED\n");
		}

		/* we just talked with ourselves too, so let's make us
		 * the file hoster now */

		pthread_mutex_lock(&files_lock);
		data = hash_table_lookup(files, name);
		data->data = malloc(8192 * sizeof(char));
		data->size = 8192;
		pthread_mutex_unlock(&files_lock);

		writeconst(conn, "OK\n");
	}

	all_to_client_mode();
}

static void worker_create_link(int conn, char *name)
{
	struct filedata *data;
	char *namep = strdup(name);

	data = calloc(1, sizeof(*data)); /* TODO */
	data->size = -1;
	data->name = namep;

	pthread_mutex_lock(&files_lock);
	hash_table_insert(files, namep, data);
	pthread_mutex_unlock(&files_lock);

	writeconst(conn, "OK\n");
}

static void worker_open_file(int conn, HashTable *fds, char *name)
{
	int len;
	struct filedata *data;
	char buff[BUFF_SIZE];

	all_to_worker_mode();

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);

	if (data == NULL) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		writeconst(conn, "ERROR 2 ENOENT\n");
		return;
	}

	if (data->open != 0) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		writeconst(conn, "ERROR 1 EPERM\n");
		return;
	}

	pthread_mutex_unlock(&files_lock);

	snprintf(buff, BUFF_SIZE, "WOP %s\n", name);
	if (query_all_workers(buff, 0, 0)) {
		fprintf(stderr, "FILESYSTEM INTEGRITY HAS BEEN COMPROMISED\n");
	}

	all_to_client_mode();

	/* We just talked with outselves, let's set the correct FD now */
	pthread_mutex_lock(&files_lock);
	data->open = ++fd_used;
	hash_table_insert(fds, &data->open, data);
	pthread_mutex_unlock(&files_lock);

	len = snprintf(buff, BUFF_SIZE, "OK FD %u\n", fd_used);
	write(conn, buff, len);
}

static void worker_mark_as_open(int conn, char *name)
{
	struct filedata *data;

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);
	data->open = -1;
	pthread_mutex_unlock(&files_lock);

	writeconst(conn, "OK\n");
}

static int worker_close_data(struct filedata *data)
{
	char buff[BUFF_SIZE];

	all_to_worker_mode();

	pthread_mutex_lock(&files_lock);

	if (data == NULL) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		return 0;
	}

	snprintf(buff, BUFF_SIZE, "WCL %s\n", data->name);
	pthread_mutex_unlock(&files_lock);

	if (query_all_workers(buff, 0, 0)) {
		fprintf(stderr, "FILESYSTEM INTEGRITY HAS BEEN COMPROMISED\n");
	}

	all_to_client_mode();

	return 1;
}

static void worker_close_file(int conn, HashTable *fds, char *cfd)
{
	unsigned int fd;
	struct filedata *data;

	sscanf(cfd, "FD %u", &fd);

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(fds, &fd);
	pthread_mutex_unlock(&files_lock);

	if (worker_close_data(data)) {
		/* we just talked with ourselves and closed the file,
		 * let's remove the stale fd now */
		pthread_mutex_lock(&files_lock);
		hash_table_remove(fds, &fd);
		pthread_mutex_unlock(&files_lock);
		writeconst(conn, "OK\n");
	} else {
		writeconst(conn, "ERROR 77 EBADFD\n");
	}
}

static void worker_mark_as_closed(int conn, char *name)
{
	struct filedata *data;

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);
	data->open = 0;
	data->pos = 0;
	pthread_mutex_unlock(&files_lock);

	writeconst(conn, "OK\n");

}

static void worker_delete_file(int conn, char *name)
{
	char buff[BUFF_SIZE];
	struct filedata *data;

	all_to_worker_mode();

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);

	if (data == NULL) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		writeconst(conn, "ERROR 2 ENOENT\n");
		return;
	}

	if (data->open != 0) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		writeconst(conn, "ERROR 16 EBUSY\n");
		return;
	}

	pthread_mutex_unlock(&files_lock);

	snprintf(buff, BUFF_SIZE, "WDE %s\n", name);
	query_all_workers(buff, 0, 0);

	all_to_client_mode();

	writeconst(conn, "OK\n");
}

static void worker_process_delete(int conn, char *name)
{
	struct filedata *data;

	pthread_mutex_lock(&files_lock);

	data = hash_table_lookup(files, name);
	hash_table_remove(files, name);

	if (!IS_STORED_REMOTELY(data))
		free(data->data);
	free(data->name);
	free(data);

	pthread_mutex_unlock(&files_lock);
	writeconst(conn, "OK\n");
}

static void read_from_file(int conn, struct filedata *data, unsigned int size)
{
	char buff[100];

	if (data->pos + size > data->used)
		size = data->used - data->pos;

	snprintf(buff, 100, "OK SIZE %u ", size);
	write(conn, buff, strlen(buff));
	write(conn, data->data + data->pos, size);
	writeconst(conn, "\n");
	data->pos += size;
}

static void worker_read_file(int conn, HashTable *fds, char *cfd)
{
	unsigned int fd;
	unsigned int size;
	struct filedata *data;

	sscanf(cfd, "FD %u SIZE %u", &fd, &size);

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		pthread_mutex_unlock(&files_lock);
		return;
	}

	if (IS_STORED_REMOTELY(data)) { /* remote read */
		pthread_mutex_unlock(&files_lock);
		worker_request_read(conn, data, size);
	} else {
		read_from_file(conn, data, size);
		pthread_mutex_unlock(&files_lock);
	}
}

static void worker_process_read(int conn, char *line)
{
	unsigned int size;
	char name[BUFF_SIZE];
	struct filedata *data;

	sscanf(line, "%s SIZE %u", name, &size);

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);

	if (!data || IS_STORED_REMOTELY(data)) { /* bad/remote read */
		writeconst(conn, "ERROR 77 EBADFD\n");
		pthread_mutex_unlock(&files_lock);
		return;
	} else {
		read_from_file(conn, data, size);
	}

	pthread_mutex_unlock(&files_lock);
}

static void append_to_file(struct filedata *data, char *content, unsigned int size)
{
	if (data->used + size >= data->size) {
		data->size += 8192;
		data->data = realloc(data->data, data->size * sizeof(char));
	}

	memcpy(data->data + data->used, content, size);
	data->used += size;
}

static void worker_write_file(int conn, HashTable *fds, char *line)
{
	unsigned int fd;
	unsigned int size;
	unsigned int len;
	struct filedata *data;
	char buff[100];
	char *content;

	sscanf(line, "FD %u SIZE %u", &fd, &size);
	len = snprintf(buff, 100, "FD %u SIZE %u ", fd, size);
	content = line + len;

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		pthread_mutex_unlock(&files_lock);
		return;
	}

	if (IS_STORED_REMOTELY(data)) { /* remote write */
		pthread_mutex_unlock(&files_lock);
		worker_request_write(data, content, size);
	} else { /* local write */
		append_to_file(data, content, size);
		pthread_mutex_unlock(&files_lock);
	}

	writeconst(conn, "OK\n");
}

static void worker_process_write(int conn, char *line)
{
	char name[BUFF_SIZE];
	char buff[BUFF_SIZE];
	unsigned int size;
	unsigned int len;
	struct filedata *data;
	char *content;

	sscanf(line, "%s SIZE %u ", name, &size);

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(files, name);

	if (!data || IS_STORED_REMOTELY(data)) { /* bad/remote write */
		writeconst(conn, "ERROR 77 EBADFD\n");
		pthread_mutex_unlock(&files_lock);
		return;
	}

	len = snprintf(buff, BUFF_SIZE, "%s SIZE %u ", name, size);
	content = line + len;

	append_to_file(data, content, size);

	writeconst(conn, "OK\n");

	pthread_mutex_unlock(&files_lock);
}

static void visitor_lsd(void *namep, unused void *value, void *fdp)
{
	char *name = namep;
	int fd = *(int*)fdp;
	int len;
	char buff[BUFF_SIZE];

	len = snprintf(buff, BUFF_SIZE, " %s", name);
	write(fd, buff, len);
}

static void worker_list_directory(int conn)
{
	writeconst(conn, "OK");

	pthread_mutex_lock(&files_lock);
	hash_table_foreach(files, visitor_lsd, &conn); /* todo */
	pthread_mutex_unlock(&files_lock);

	if (hash_table_size(files) == 0)
		writeconst(conn, " \n");
	else
		writeconst(conn, "\n");
}

static void visitor_closefd(unused void *key, void *data, unused void *extra)
{
	worker_close_data(data);
}

static void worker_finalize(int conn, HashTable *fds, const char *msg)
{
	/* TODO: lock */
	hash_table_foreach(fds, visitor_closefd, NULL);
	hash_table_destroy(fds);

	write(conn, msg, strlen(msg));
	close(conn);
}

static void worker_enter_worker_mode(int conn)
{
	pthread_mutex_lock(&worker_mode_lock);
	while (worker_mode)
		pthread_cond_wait(&worker_mode_enabled, &worker_mode_lock);
	worker_mode = 1;
	writeconst(conn, "OK\n");
	pthread_mutex_unlock(&worker_mode_lock);
}

static void worker_exit_worker_mode(int conn)
{
	pthread_mutex_lock(&worker_mode_lock);
	worker_mode = 0;
	pthread_cond_broadcast(&worker_mode_enabled);
	writeconst(conn, "OK\n");
	pthread_mutex_unlock(&worker_mode_lock);
}

static int process_incoming_line(int conn, HashTable *fds, int *secure_mode, char *line)
{
	int op = operation(line);

	if (*secure_mode && op >= WO_EWM) {
		close(conn);
		fprintf(stderr, MODULE "Untrusted client attempted a secure operation! disconnected\n");
		return 0;
	}

	if (op < WO_EWM) {
		pthread_mutex_lock(&worker_mode_lock);
		while (worker_mode) {
			pthread_cond_wait(&worker_mode_enabled, &worker_mode_lock);
		}
		pthread_mutex_unlock(&worker_mode_lock);
	}

	switch (op) {
	case OP_CRE:
		worker_create_file(conn, line+4);
		break;
	case OP_OPN:
		worker_open_file(conn, fds, line+4);
		break;
	case OP_REA:
		worker_read_file(conn, fds, line+4);
		break;
	case OP_WRT:
		worker_write_file(conn, fds, line+4);
		break;
	case OP_CLO:
		worker_close_file(conn, fds, line+4);
		break;
	case OP_LSD:
		worker_list_directory(conn);
		break;
	case OP_DEL:
		worker_delete_file(conn, line+4);
		break;
	case OP_BYE:
		worker_finalize(conn, fds, "OK\n");
		return 0;
		break;
	case WO_WMO:
		worker_enter_worker_mode(conn);
		break;
	case WO_EWM:
		worker_exit_worker_mode(conn);
		break;
	case WO_WLN:
		worker_create_link(conn, line+4);
		break;
	case WO_WOP:
		worker_mark_as_open(conn, line+4);
		break;
	case WO_WRR:
		worker_process_read(conn, line+4);
		break;
	case WO_WRW:
		worker_process_write(conn, line+4);
		break;
	case WO_WCL:
		worker_mark_as_closed(conn, line+4);
		break;
	case WO_WDE:
		worker_process_delete(conn, line+4);
		break;
	case DI_SEC:
		*secure_mode = 1;
		break;
	default:
		worker_finalize(conn, fds, "ERROR 71 EPROTO\n");
		fprintf(stderr, MODULE "Client issued invalid command! \"%s\", disconnected\n", line);
		return 0;
	}

	return 1;
}

static void *handle_line_protocol(void *arg)
{
	HashTable *fds;
	char buff[BUFF_SIZE];
	char *buffer = (char*)buff;
	struct params *parameters = arg;
	int res = 0, ret, i;
	int id = parameters->id;
	int conn = parameters->conn;
	int security = 0;
	free(parameters);

	fds = hash_table_new_extended(hash_table_integers_equal, hash_table_int_hash);

	fprintf(stdout, MODULE "New client %d connected\n", id);

	while (1) {
		ret = read(conn, buffer+res, BUFF_SIZE-res-1);
		if (ret <= 0) {
			close(conn);
			break;
		}
		res += ret;
		buffer[res] = '\0';

		for (i = 0; i < res; i++) {
			if (buff[i] == '\n') {
				buff[i] = '\0';
				if (!process_incoming_line(conn, fds, &security, buffer))
					break;
				buffer = &buff[i+1];
			}
		}

		if (buffer != buff + res) {
			res = strlen(buffer);
			memmove(buff, buffer, res);
		} else {
			res = 0;
		}

		buffer = (void*)buff;
	}

	fprintf(stdout, MODULE "Client %d disconnected %s\n", id,
		res+1 == BUFF_SIZE ? "due to overflowed buffer" : "normally");

	return NULL;
}

static int connect_to_worker(char *host, int port)
{
	int sock = startClient(host, port);

	if (sock < 0)
		return 0;

	workers = clist_insert(workers, sock);
	return sock;
}

static void *interconnect_workers(void *arg)
{
	char **workers = (char**)arg;
	char *chost;
	char *cport;
	int tries;

	while (*++workers) {
		chost = strtok(*workers, ":");
		cport = strtok(NULL, ":");

		if (!chost || !cport) {
			fprintf(stderr, MODULE "Error getting connection data\n");
			return NULL;
		}

		tries = 5;
		while (tries--) {
			if (!connect_to_worker(chost, atoi(cport)))
				sleep(1);
			else
				break;
		}

		if (!tries) {
			fprintf(stderr, MODULE "Error connecting to worker %s:%s\n",
				chost, cport);
			return NULL;
		}

		worker_qty++;
	}

	return NULL;
}

int main(int argc, char **argv)
{
	pthread_t interconnect;
	int port;

	files = hash_table_new_extended(hash_table_strings_equal, hash_table_string_hash);

	if (argc < 2) {
		fprintf(stderr, MODULE "Did not specify port. Exiting.\n");
		return -1;
	}

	port = atoi(*++argv);

	pthread_create(&interconnect, NULL, interconnect_workers, argv);
	return startSpawner(port, handle_line_protocol, &interconnect);
}
