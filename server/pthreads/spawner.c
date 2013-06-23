#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

#include "spawner.h"

#define MODULE "spawner: "

int startSpawner(unsigned int port, void *(*handler)(void *))
{
	unsigned int id = 0;
	int sock, conn;
	struct sockaddr_in servaddr;
	struct params *parameters;
	pthread_t thread;

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		fprintf(stderr, MODULE "Error creating listening socket.\n");
		return -1;
	}

	/* listen on all interfaces on the specified port */
	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port = htons(port);

	if (bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
		fprintf(stderr, MODULE "Error calling bind()\n");
		return -1;
	}

	/* wait for clients, keep at most 10 in backlog */
	if (listen(sock, 10) < 0) {
		fprintf(stderr, MODULE "Error calling listen()\n");
		return -1;
	}

	/* accept client and spawn a thread to handle it */
	while (1) {
		if ((conn = accept(sock, NULL, NULL)) < 0) {
			fprintf(stderr, MODULE "Error calling accept()\n");
			return -1;
		}

		/* to be free()'d by client */
		parameters = malloc(sizeof(*parameters));
		if (parameters == NULL) {
			perror("malloc");
			return -1;
		}
		parameters->id = id++;
		parameters->conn = conn;

		pthread_create(&thread, NULL, handler, parameters);
		pthread_detach(thread);
	};

	return 0;
}
