#ifndef __SPAWNER_H__
#define __SPAWNER_H__

struct params {
	unsigned int id;
	int conn;
};

int startSpawner(unsigned int port, void *(*handler)(void *), pthread_t *ready);

int startClient(char *host, int port);

#endif
