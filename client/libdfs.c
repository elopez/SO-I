#include <stdio.h>
#include "libdfs.h"

#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>        
#include <string.h>    

int conn;

typedef struct sockaddr *sin;

int dfs_connect(char *addr) 
{
  struct sockaddr_in servaddr;
  if (conn!=0)
    return -1;
  conn = socket(AF_INET, SOCK_STREAM, 0);
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family      = AF_INET;
  inet_aton(addr,&servaddr.sin_addr);
  servaddr.sin_port        = htons(8000);   
  if (connect(conn,(sin)&servaddr,sizeof(servaddr))<0) 
    return -1;
  return 0;
}

int dfs_disconnect()
{
  char buffout[BUFF_SIZE];
  if (conn==0)
    return -1;
  request("BYE",3); 
  if (read_reply(buffout)!=0) {
      close(conn);
      return -1;
  }
  if (close(conn)<0)
    return -1;
  conn=0;
  return 0;
}

int request(char *req, int size)
{
 return  write(conn,req,size);
}

int read_reply(char *buffout)
{
  int i=0;
  char buff[BUFF_SIZE],*s;
  do 
    read(conn,buff+i,1);
  while (buff[i++]!='\0');
  if (strncmp(buff,"OK ",3)==0) {
    memcpy(buffout,buff+3,i-3);
    return 0;	
  }
  if (strncmp(buff,"ERROR ",6)==0) {
    memcpy(buffout,buff+6,i-6);
    return -1;	
  }
  return -2;
}

FD dfs_open(char *filename) {
  char buff[BUFF_SIZE];
  int i;
  sprintf(buff,"OPN %s", filename);
  request(buff,strlen(buff));
  if (read_reply(buff)!=0)
    return -1;   
  if (sscanf(buff,"FD %d",&i)!=1)
    return -1;     
  return i;
}

FD dfs_create(char *filename) {
  char buff[BUFF_SIZE];
  int i;
  sprintf(buff,"CRE %s", filename);
  request(buff,strlen(buff));
  if (read_reply(buff)!=0)
    return -1;   
  return 0;
}


int dfs_close(FD f)
{
  char buff[BUFF_SIZE];
  sprintf(buff,"CLO FD %d",f);
  request(buff,strlen(buff));
  return read_reply(buff);
}

int dfs_ls(char *b)
{
  request("LSD",3);
  return read_reply(b);}

int dfs_rm(char *filename)
{
  char buff[BUFF_SIZE];
  sprintf(buff,"DEL %s",filename);
  request(buff,strlen(buff));
  return read_reply(buff);
}

int read_reply_space(char *buffout)
{
  int i=0;
  char buff[BUFF_SIZE],*s;
  do 
    read(conn,buff+i,1);
  while (buff[i++]!=' ');
  buff[i]='\0';
  strcpy(buffout,buff);
  return 0;
}

int read_reply_size(char *buff, int size)
{
	int res = read(conn,buff,size);
  // Collectar resultados hasta llegar a size
  return res;
}

int dfs_read(FD f, int size, void *b)
{
  char buff[BUFF_SIZE];
  sprintf(buff,"REA FD %d SIZE %d",f,size);
  request(buff,strlen(buff));
  read_reply_space(buff);
  if (strcmp(buff,"OK ")!=0) return -1;
  /* OK */  
  read_reply_space(buff);
  /* SIZE  */  
  read_reply_space(buff);
  /* [NSIZE]  */  
  sscanf(buff,"%d",&size);
  if (size<=0)
    return EOF;
  read_reply_size((char*)b,size+1);
  return size;
}

int dfs_write(FD f, int size, void *b)
{
  char buff[BUFF_SIZE];
  int len;
  sprintf(buff,"WRT FD %d SIZE %d ",f,size);
  len=strlen(buff);
  memcpy(buff+len,b,size);
  request(buff,len+size);
  return read_reply(buff); 
}



