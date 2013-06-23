#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "clist.h"
#include "spawner.h"

#define MODULE "dispatcher: "
#define BUFF_SIZE 1024
#define writeconst(con, str) write((con), (str), strlen((str)))

CList *workers = CLIST_INITIALIZER;

static void *handle_client_connection(void *arg)
{
	char buffer[BUFF_SIZE];
	struct params *parameters = arg;
	int res = 0;
	int id = parameters->id;
	int conn = parameters->conn;
	free(parameters);

	fprintf(stdout, MODULE "New client %d connected\n", id);

	/* read the first CON\n before finding a worker */
	while (res != 4)
		res += read(conn, buffer + res, 4 - res);

	if (!strncmp("CON", buffer, 3) == 0) {
		writeconst(conn, "ERROR 71 EPROTO\n");
		close(conn);
		return NULL;
	}

	writeconst(conn, "OK ID 1\n");

	while (1) {
		res = read(conn, buffer, BUFF_SIZE);
		if (res <= 0) {
			close(conn);
			break;
		}
		buffer[res] = '\0';
		write(conn, buffer, strlen(buffer));
	}

	fprintf(stdout, MODULE "Client %d disconnected\n", id);

	return NULL;
}

int connect_to_worker(char *host, int port)
{
	/* TODO */
	return 0;
}

int main(int argc, char **argv)
{
	char *host;
	char *port;

	if (argc == 1) {
		fprintf(stderr, MODULE "No workers specified\n");
		return -1;
	}

	while (*++argv) {
		host = strtok(*argv, ":");
		port = strtok(NULL, ":");

		if (!host || !port) {
			fprintf(stderr, MODULE "Error getting connection data\n");
			return -1;
		}

		if (!connect_to_worker(host, atoi(port))) {
			fprintf(stderr, MODULE "Error connecting to worker %s:%s\n",
				host, port);
			return -1;
		}
	}

	return startSpawner(8000, handle_client_connection);
}
