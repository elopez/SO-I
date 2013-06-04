#include <stdio.h>
#include "libdfs.h"

int main(int argc, char**argv)
{
  char buff[BUFF_SIZE];
  if (dfs_connect("127.0.0.1")==-1) // Conectarse al servidor
    return -1;
  dfs_ls(buff);
  printf("%s\n",buff);
  // Cerrar conexion
  dfs_disconnect();
  return 0;
}

