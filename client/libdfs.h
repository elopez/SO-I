#define BUFF_SIZE 8092
typedef int FD ;
int dfs_connect(char*); 
int dfs_create(char*); 
FD dfs_open(char*);
int dfs_read(FD, int, void*);
int dfs_write(FD, int, void*);
int dfs_close(FD);
int dfs_disconnect();
int dfs_ls(char*);
int dfs_rm(char*);
