CFLAGS=-L. -ldfs -g
CC?=gcc
all: cp cat ls create clientbot


clean:
	rm *.o *.a
	rm cp cat ls create clientbot

libdfs.a: libdfs.c libdfs.h
	$(CC)  $(CFLAGS) -c libdfs.c -o libdfs.o
	ar rcs libdfs.a libdfs.o

cp: cp.c libdfs.a
	$(CC)  cp.c $(CFLAGS) -o cp

cat: cat.c libdfs.a
	$(CC) cat.c $(CFLAGS) -o cat

ls: ls.c libdfs.a
	$(CC) ls.c $(CFLAGS)  -o ls

create: create.c libdfs.a
	$(CC) create.c $(CFLAGS) -o create

clientbot: clientbot.c libdfs.a
	$(CC) clientbot.c $(CFLAGS) -o clientbot
