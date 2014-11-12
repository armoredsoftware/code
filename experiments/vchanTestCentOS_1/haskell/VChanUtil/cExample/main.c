/**
 * The client of the vchanTestCentOS_1. 
 * Put this client executable in a VM of its own.
 *
 * Usage: clientExp1
 *
 * NOTE: For this application to work the zen kernel modules
 * xen_gntalloc and xen_gntdev must be loaded prior to running the application,
 * otherwise you will get an error of "No such file or directory."
 *
 */

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libxenvchan.h>
#include <exp1Common.h>

void sendForever(struct libxenvchan * chan){
   char * tmp = (char *) malloc(256 * sizeof(char));
   while (1){
     printf("Enter text to send:\n");
     fgets(tmp, 256, stdin);
     vchan_send(chan,tmp, strlen(tmp)); 
   }

}

void receiveForever(struct libxenvchan *chan){
  int size;
  char * msg;
  int i = 0;

  while(1){ 
    libxenvchan_wait(chan);
    msg = vchan_receive(chan,&size);
    printf("Received: ");
    for(i = 0; i< size; i++){
      printf("%c",msg[i]);
    }
     printf("\n");
  }

}


//####################################################################

int main(int argc, char **argv)
{
  struct libxenvchan *chan = 0;
  int selfId; // domainID of this node;
  int otherId;
  int client= 0;
  xentoollog_logger_stdiostream * xc_logger;

  xc_logger = createDebugLogger();

  selfId =getDomId();
  fprintf(stdout,"Client: Domain Id: %d\n", selfId);

  if(argc != 3){
   fprintf(stderr,"Error: Usage ./Client [server-0, client-1] [other domId]\n");
   exit(1);
  }  

  sscanf(argv[1],"%d",&client);
  sscanf(argv[2],"%d",&otherId);

  if (!client){
     chan = vchan_server_init((xentoollog_logger *) xc_logger,otherId);
  }else{
     chan = vchan_client_init((xentoollog_logger *)xc_logger, otherId);
  }
   
  if (!client){
    sendForever(chan);

  }else{
    receiveForever(chan);
  } 
    
  
 

  libxenvchan_close(chan);


  return 0;
}
