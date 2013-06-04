#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include "libdfs.h"

int main(int argc, char**argv)
{
	FD src;
	int file=1;
	char c;
	int secs, seed;
	time_t start, end;
	if (argc<4)
		return -1;
	if (dfs_connect(argv[1])==-1) // Conectarse al servidor
		return -1;
	sscanf(argv[2], "%d", &secs);
	sscanf(argv[3], "%d", &seed);
	srand(seed);
	end = start = time(NULL);
	while (end-start<secs) {
		int sel = rand()%3;
		switch (sel) {
			case 0:
				/*crear, escribir, borrar */
				{
					char nombre[128];
					char txt[257];
					int i, i1 = rand(), i2 = rand();
					FD f;
					printf("CASO 0\n");
					sprintf(nombre, "file%d%d.txt", i1, i2);
					dfs_rm(nombre);
					dfs_create(nombre);
					f = dfs_open(nombre);
					for (i=0; i<64; i++) {
						int num = rand() % 10000;
						sprintf(&(txt[i*4]), "%04d", num);
					}
					dfs_write(f, 256, txt);
					dfs_close(f);
					dfs_rm(nombre);
				}
				break;
			case 1:
				/*ls*/
				{
					printf("CASO 1\n");
					char buff[BUFF_SIZE];
					dfs_ls(buff);
					printf("ls: %s\n", buff);
				}
				break;
			case 2:
				/*ls, tomo primero, abro, leo, cierro*/
				{
					FD f;
					char buff[BUFF_SIZE], *ptr;
					printf("CASO 2\n");
					dfs_ls(buff);
					printf("archivos: %s\n", buff);
					ptr = strtok(buff, " ");
					if (ptr) {
						printf("Abro: %s\n", ptr);
						f = dfs_open(ptr);
						dfs_read(f, 1, buff);
						dfs_close(f);
					}
				}
		}
		usleep(rand()%1000000);
		end = time(NULL);
	}
	dfs_disconnect();

	return 0;
}
