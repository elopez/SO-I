#include <stdio.h>
#include "libdfs.h"

int main(int argc, char**argv)
{
  FD src;
  int file=1;
  char c;
  if (argc<2) 
    return -1;
  if (dfs_connect("127.0.0.1")==-1) // Conectarse al servidor
    return -1;
  while (file<argc) {
    src=dfs_open(argv[file]); // Abrir el archivo origen
    while (dfs_read(src,1,&c)!=EOF)
    {
      putchar(c);
    }
    dfs_close(src);
    file++;
  }
  printf("\n");
  // Cerrar archivos
  // Cerrar conexion
  dfs_disconnect();
  return 0;
}

