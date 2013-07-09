#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "clist.h"
#include "spawner.h"

#define MODULE "dispatcher: "
#define BUFF_SIZE 1024
#define writeconst(con, str) write((con), (str), strlen((str)))

struct worker_data {
	char *host;
	int port;
};

/* list of struct worker_data */
CList *workers = CLIST_INITIALIZER;

int connect_to_worker(void)
{
	int sock;
	struct worker_data *data;

	data = clist_data(workers, struct worker_data *);
	sock = startClient(data->host, data->port);

	if (sock < 0)
		return 0;

	workers = clist_next(workers);

	return sock;
}

static void *handle_client_connection(void *arg)
{
	char buffer[BUFF_SIZE];
	struct params *parameters = arg;
	int res = 0;
	int id = parameters->id;
	int conn = parameters->conn;
	int worker;
	fd_set fds;
	struct timeval t = { .tv_sec = 0, .tv_usec = 0};
	int maxfd;
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

	worker = connect_to_worker();

	maxfd = (conn > worker ? conn : worker) + 1;

	/* flush the fd in case a previous client left garbage on it */
	FD_ZERO(&fds);
	FD_SET(worker, &fds);
	while (select(maxfd, &fds, NULL, NULL, &t) > 0)
		read(worker, buffer, BUFF_SIZE);

	while (1) {
		FD_ZERO(&fds);
		FD_SET(conn, &fds);
		FD_SET(worker, &fds);

		if (select(maxfd, &fds, NULL, NULL, NULL) < 0) {
			close(conn);
			close(worker);
			break;
		}

		if (FD_ISSET(conn, &fds)) {
			res = read(conn, buffer, BUFF_SIZE);
			if (res <= 0) {
				close(conn);
				break;
			}
			write(worker, buffer, res);
		}

		if (FD_ISSET(worker, &fds)) {
			res = read(worker, buffer, BUFF_SIZE);
			if (res <= 0) {
				fprintf(stderr, MODULE "A worker has disconnected.");
				close(worker);
				break;
			}
			write(conn, buffer, res);
		}
	}

	fprintf(stdout, MODULE "Client %d disconnected\n", id);
	close(worker);

	return NULL;
}

int main(int argc, char **argv)
{
	char *host;
	char *port;
	struct worker_data *data;

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

		data = malloc(sizeof(*data)); /* TODO */
		data->host = host;
		data->port = atoi(port);
		workers = clist_insert(workers, data);
	}

	return startSpawner(8000, handle_client_connection, NULL);
}
