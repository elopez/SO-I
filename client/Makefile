CFLAGS = -L. -ldfs -g 
all: cp cat ls create clientbot


clean:
	rm *.o *.a
	rm cp cat ls create clientbot

libdfs.a: libdfs.c libdfs.h
	gcc  $(CFLAGS) -c libdfs.c -o libdfs.o
	ar rcs libdfs.a libdfs.o

cp: cp.c libdfs.a
	gcc  cp.c $(CFLAGS) -o cp

cat: cat.c libdfs.a
	gcc cat.c $(CFLAGS) -o cat

ls: ls.c libdfs.a
	gcc ls.c $(CFLAGS)  -o ls

create: create.c libdfs.a
	gcc create.c $(CFLAGS) -o create

clientbot: clientbot.c libdfs.a
	gcc clientbot.c $(CFLAGS) -o clientbot
