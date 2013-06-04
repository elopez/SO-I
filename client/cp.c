#include <stdio.h>
#include "libdfs.h"

int main(int argc, char**argv)
{
  if (argc<3) 
    return -1;
  if (dfs_connect("127.0.0.1")==-1) // Conectarse al servidor
    return -1;
  FD src=dfs_open(argv[1]); // Abrir el archivo origen
  dfs_rm(argv[2]); // Borro el archivo por si existiera
  dfs_create(argv[2]);
  FD dst=dfs_open(argv[2]); // Abrir destino
  char c;
  // Copiar el contenido
  while (dfs_read(src,1,&c)!=EOF)
  {
    dfs_write(dst,1,&c);
  }
  // Cerrar archivos
  dfs_close(dst);
  dfs_close(src);
  // Cerrar conexion
  dfs_disconnect();
  return 0;
}

