#include <stdio.h>
#include "libdfs.h"

int main(int argc, char**argv)
{
  FD src;
  int file=1;
  int c;
  if (argc<2) 
    return -1;
  if (dfs_connect("127.0.0.1")==-1) // Conectarse al servidor
    return -1;
  dfs_rm(argv[1]);
  dfs_create(argv[1]);
  src=dfs_open(argv[1]); // Abrir el archivo origen
  while ((c=getchar())!=EOF)
    dfs_write(src,1,&c);
  // Cerrar archivo
  dfs_close(src);
  // Cerrar conexion
  dfs_disconnect();
  return 0;
}

