#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "spawner.h"

#define MODULE "dispatcher: "
#define BUFF_SIZE 1024
#define writeconst(con, str) write((con), (str), strlen((str)))

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

int main(int argc, char **argv)
{
	return startSpawner(8000, handle_client_connection);
}
