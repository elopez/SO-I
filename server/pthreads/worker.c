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
	char *data;
	unsigned int open;
	unsigned int size;
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

static void worker_create_file(int conn, const char *name)
{
	struct filedata *data;

	if (hash_table_lookup(files, name) != NULL) {
		writeconst(conn, "ERROR 17 EEXIST\n");
	} else {
		data = malloc(sizeof(*data)); /* TODO */
		data->data = NULL;
		data->open = 0;
		data->size = 0;
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
		writeconst(conn, "ERROR 1 DEMO\n");
		break;
	case OP_WRT:
		writeconst(conn, "ERROR 1 DEMO\n");
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
