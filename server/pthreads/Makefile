CC=gcc
CFLAGS=-Wall -Wextra -g
LDFLAGS=-lpthread
.PHONY: clean all

all: dispatcher worker

dispatcher: dispatcher.o spawner.o clist.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

worker: worker.o spawner.o hashtable.o clist.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f *.o dispatcher worker
