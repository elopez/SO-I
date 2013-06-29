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

static HashTable *files;
static pthread_mutex_t files_lock = PTHREAD_MUTEX_INITIALIZER;

static CList *workers = CLIST_INITIALIZER;
static int worker_qty = 0;

static int fd_used = 0;

static pthread_mutex_t worker_mode_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t worker_mode_enabled;
static int worker_mode = 0;

enum {
	/* user and worker operations */
	OP_CRE,
	OP_OPN,
	OP_REA,
	OP_WRT,
	OP_CLO,
	OP_LSD,
	OP_DEL,
	/* worker operations */
	WO_WMO,
	WO_EWM,
	WO_WLN,
	WO_WOP,
	WO_WCL,
};

struct filedata {
	char *name;		/* file name, also key on the HT */
	char *data;		/* file storage */
	unsigned int open;	/* fd or 0 if closed */
	unsigned int pos;	/* read position */
	unsigned int used;	/* # of used chars from data */
	unsigned int size;	/* # of chars available on data */
};

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
	else if (strncmp("WMO", line, 3) == 0)
		return WO_WMO;
	else if (strncmp("EWM", line, 3) == 0)
		return WO_EWM;
	else if (strncmp("WLN", line, 3) == 0)
		return WO_WLN;
	else if (strncmp("WOP", line, 3) == 0)
		return WO_WOP;
	else if (strncmp("WCL", line, 3) == 0)
		return WO_WCL;

	return -1;
}

static int query_all_workers(char *query)
{
	int fd, res, ret, len;
	char buff[BUFF_SIZE] = "";
	CList *head = workers;
	CList *curr = workers;

	len = strlen(query);
	ret = 0;

	do {
		fd = clist_data(curr);

		write(fd, query, len);

		res = 0;
		buff[0] = '\0';
		while (strchr(buff, '\n') == NULL)
			res += read(fd, buff+res, BUFF_SIZE-res);

		if (strncmp("OK", buff, 2) != 0) {
			ret += 1;
			break;
		}

		curr = clist_next(curr);
	} while (head != curr);

	return ret;
}

static int all_to_worker_mode(void)
{
	if (query_all_workers("WMO\n"))
		fprintf(stderr, MODULE "Error locking!");

	return 0;
}

static int all_to_client_mode(void)
{
	if (query_all_workers("EWM\n"))
		fprintf(stderr, MODULE "Error unlocking!");

	return 0;
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
		if (query_all_workers(buff)) {
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
	if (query_all_workers(buff)) {
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

static void worker_close_file(int conn, HashTable *fds, char *cfd)
{
	unsigned int fd;
	struct filedata *data;
	char buff[BUFF_SIZE];

	sscanf(cfd, "FD %u", &fd);

	all_to_worker_mode();

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		pthread_mutex_unlock(&files_lock);
		all_to_client_mode();
		writeconst(conn, "ERROR 77 EBADFD\n");
		return;
	}

	snprintf(buff, BUFF_SIZE, "WCL %s\n", data->name);
	pthread_mutex_unlock(&files_lock);

	if (query_all_workers(buff)) {
		fprintf(stderr, "FILESYSTEM INTEGRITY HAS BEEN COMPROMISED\n");
	}

	all_to_client_mode();

	/* we just talked with ourselves and closed the file,
	 * let's remove the stale fd now */
	pthread_mutex_lock(&files_lock);
	hash_table_remove(fds, &fd);
	pthread_mutex_unlock(&files_lock);

	writeconst(conn, "OK\n");
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
	struct filedata *data;
	data = hash_table_lookup(files, name);

	if (data == NULL) {
		writeconst(conn, "ERROR 2 ENOENT\n");
		return;
	}

	if (data->open != 0) {
		writeconst(conn, "ERROR 16 EBUSY\n");
		return;
	}

	hash_table_remove(files, name);
	free(data->data);
	free(data);

	writeconst(conn, "OK\n");
}

static void worker_read_file(int conn, HashTable *fds, char *cfd)
{
	unsigned int fd;
	unsigned int size;
	struct filedata *data;
	char buff[100];

	sscanf(cfd, "FD %u SIZE %u", &fd, &size);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		return;
	}

	if (data->pos + size > data->used)
		size = data->used - data->pos;

	snprintf(buff, 100, "OK SIZE %u ", size);
	write(conn, buff, strlen(buff));
	write(conn, data->data + data->pos, size);
	writeconst(conn, "\n");
	data->pos += size;
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
	struct filedata *data;
	char buff[100];
	char *content;

	sscanf(line, "FD %u SIZE %u", &fd, &size);
	snprintf(buff, 100, "FD %u SIZE %u ", fd, size);
	content = line + strlen(buff);

	pthread_mutex_lock(&files_lock);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		pthread_mutex_unlock(&files_lock);
		return;
	}

	if (data->size == -1) {
		/* remote TODO */
	} else {
		append_to_file(data, content, size);
		writeconst(conn, "OK\n");
	}

	pthread_mutex_unlock(&files_lock);
}

static void visitor_lsd(void *key, void *value, void *names)
{
	strcat(names, " ");
	strcat(names, key);
}

static void worker_list_directory(int conn)
{
	char buffer[BUFF_SIZE] = "OK";
	unsigned int len;

	hash_table_foreach(files, visitor_lsd, buffer); /* todo */
	len = strlen(buffer);

	if (len == 2) {
		strcat(buffer, " \n");
		len += 2;
	} else {
		strcat(buffer, "\n");
		len++;
	}

	write(conn, buffer, len);
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

static void process_incoming_line(int conn, HashTable *fds, char *line)
{
	int op = operation(line);

	if (op < WO_EWM) {
		pthread_mutex_lock(&worker_mode_lock);
		while (worker_mode) {
			fprintf(stderr, "Locked here.. %u\n", getpid());
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
	case WO_WCL:
		worker_mark_as_closed(conn, line+4);
		break;
	default:
		writeconst(conn, "ERROR 71 EPROTO\n");
		printf("WAS: %s\n", line);
	}
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
	free(parameters);

	fds = hash_table_new_extended(hash_table_integers_equal, hash_table_int_hash);

	fprintf(stdout, MODULE "New client %d connected\n", id);

	while (1) {
		ret = read(conn, buffer+res, BUFF_SIZE-res);
		if (ret <= 0) {
			close(conn);
			break;
		}
		res += ret;
		buffer[res] = '\0';

		for (i = 0; i < res; i++) {
			if (buff[i] == '\n') {
				buff[i] = '\0';
				process_incoming_line(conn, fds, buffer);
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
		res == BUFF_SIZE ? "due to overflowed buffer" : "normally");

	return NULL;
}

int connect_to_worker(char *host, int port)
{
	int sock = startClient(host, port);

	if (sock < 0)
		return 0;

	workers = clist_insert(workers, sock);
	return sock;
}

void *interconnect_workers(void *arg)
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
