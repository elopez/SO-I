#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "spawner.h"
#include "hashtable.h"

#define MODULE "worker: "
#define BUFF_SIZE 8192
#define writeconst(con, str) write((con), (str), strlen((str)))

HashTable *files;
HashTable *fds;
int fd_used = 0;

enum {
	OP_CRE,
	OP_OPN,
	OP_REA,
	OP_WRT,
	OP_CLO,
	OP_LSD,
	OP_DEL,
};

struct filedata {
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

	return -1;
}

static void worker_create_file(int conn, char *name)
{
	struct filedata *data;

	if (hash_table_lookup(files, name) != NULL) {
		writeconst(conn, "ERROR 17 EEXIST\n");
	} else {
		data = calloc(1, sizeof(*data)); /* TODO */
		data->data = malloc(8192 * sizeof(char));
		data->size = 8192;
		hash_table_insert(files, strdup(name), data);
		writeconst(conn, "OK\n");
	}
}

static void worker_open_file(int conn, char *name)
{
	struct filedata *data = hash_table_lookup(files, name);
	char msg[100];

	if (data == NULL) {
		writeconst(conn, "ERROR 2 ENOENT\n");
		return;
	}

	if (data->open != 0) {
		writeconst(conn, "ERROR 1 EPERM\n");
		return;
	}

	data->open = ++fd_used;
	hash_table_insert(fds, &data->open, data);
	snprintf(msg, 100, "OK FD %u\n", fd_used);
	write(conn, msg, strlen(msg));
}

static void worker_close_file(int conn, char *cfd)
{
	unsigned int fd;
	struct filedata *data;

	sscanf(cfd, "FD %u", &fd);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		return;
	}

	hash_table_remove(fds, &fd);
	data->open = 0;
	data->pos = 0;

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

static void worker_read_file(int conn, char *cfd)
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

static void worker_write_file(int conn, char *line)
{
	unsigned int fd;
	unsigned int size;
	struct filedata *data;
	char buff[100];
	char *content;

	sscanf(line, "FD %u SIZE %u", &fd, &size);
	data = hash_table_lookup(fds, &fd);

	if (data == NULL) {
		writeconst(conn, "ERROR 77 EBADFD\n");
		return;
	}

	if (data->used + size >= data->size) {
		data->size += 8192;
		data->data = realloc(data->data, data->size * sizeof(char));
	}

	snprintf(buff, 100, "FD %u SIZE %u ", fd, size);
	content = line + strlen(buff);
	memcpy(data->data + data->used, content, size);
	data->used += size;

	writeconst(conn, "OK\n");
}

static void worker_list_directory(int conn)
{
	/* placeholder */
	writeconst(conn, "OK\n");
}

static void process_incoming_line(int conn, char *line)
{
	switch (operation(line)) {
	case OP_CRE:
		worker_create_file(conn, line+4);
		break;
	case OP_OPN:
		worker_open_file(conn, line+4);
		break;
	case OP_REA:
		worker_read_file(conn, line+4);
		break;
	case OP_WRT:
		worker_write_file(conn, line+4);
		break;
	case OP_CLO:
		worker_close_file(conn, line+4);
		break;
	case OP_LSD:
		worker_list_directory(conn);
		break;
	case OP_DEL:
		worker_delete_file(conn, line+4);
		break;
	default:
		writeconst(conn, "ERROR 71 EPROTO\n");
	}
}

static void *handle_line_protocol(void *arg)
{
	char buff[BUFF_SIZE];
	char *buffer = (void*)buff;
	struct params *parameters = arg;
	int res = 0, ret;
	int id = parameters->id;
	int conn = parameters->conn;
	free(parameters);

	fprintf(stdout, MODULE "New client %d connected\n", id);

	while (1) {
		ret = read(conn, buffer+res, BUFF_SIZE-res);
		if (ret <= 0) {
			close(conn);
			break;
		}
		res += ret;
		buffer[res] = '\0';

		while ((buffer = strtok(buffer, "\n")) != NULL) {
			if (strchr(buffer, '\0') == buff + BUFF_SIZE)
				break;
			process_incoming_line(conn, buffer);
			buffer = NULL;
		}

		if (buffer != NULL) {
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

static int strings_equal(const void *a, const void *b)
{
	return !strcmp(a, b);
}

static int integers_equal(const void *a, const void *b)
{
	return *(unsigned int*)a == *(unsigned int*)b;
}

int main(int argc, char **argv)
{
	files = hash_table_new_extended(strings_equal, hash_table_string_hash);
	fds = hash_table_new_extended(integers_equal, hash_table_int_hash);

	return startSpawner(8000, handle_line_protocol);
}
