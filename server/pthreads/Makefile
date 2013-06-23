CC=gcc
CFLAGS=-pthread -Wall -Wextra
.PHONY: clean all

all: dispatcher worker

dispatcher: dispatcher.o spawner.o
	$(CC) -o $@ $^ $(CFLAGS)

worker: worker.o spawner.o hashtable.o
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm *.o dispatcher worker
