/**
 * Usage: commonly used functions
 *
 * NOTE: For this application to work the xen kernel modules
 * xen_gntalloc and xen_gntdev must be loaded prior to running the application,
 * otherwise you will get an error of "No such file or directory."
 *
 */

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <xenstore.h>
#include <libxenvchan.h>
#include <unistd.h>
#include "exp1Common.h"
#include <sys/mman.h>

int getDomId(void){
  struct xs_handle   *handle= 0;
  int selfId = -1;
  handle = xs_open(XS_OPEN_READONLY);
  if ( handle == NULL || handle < 0){
    fprintf(stderr, "Unabled to open an interface\n");
    perror("getDomId: xs_open Failed to obtain a handle.");
    return -1;
  }
  char * s = (char *)xs_read(handle, XBT_NULL,"domid",NULL);

  if (s == NULL){
    fprintf(stderr, "Unabled to open an interface\n");
    perror("getDomId: xs_open Failed to obtain a handle.");
    return -1;
  }

  selfId =(int) strtol(s,(char **)NULL,10);
  return selfId;

}


int isNull(struct libxenvchan * chan){
  if( chan == NULL)
     return 1;
  return 0; 

}

struct libxenvchan * vchan_client_init(xentoollog_logger * logger, int srvId){
   int clientId = getDomId();
   return createTransmitChanP(logger, srvId, clientId, "data/serverVchan");

}

struct libxenvchan * vchan_server_init(xentoollog_logger *logger, int clientId){
    return createReceiveChanP(logger, clientId, "data/serverVchan");
}

struct libxenvchan * vchan_maybe_client_init(xentoollog_logger * logger, int srvId){
   int sourceId = getDomId();
   int destId = srvId;
   struct libxenvchan *txCtrl=0;
   char serverRxXS [256]; // xenStore path for the server's receive.
   char  p[256];
   char *path = "data/serverVchan";
   
   
   if (!path){
     sprintf (p,"%s",SERV_REL_RX_XS_PATH);
   }else{
     sprintf (p,"%s",path);
   }

   sprintf(p, "%s_%d",p,sourceId);
   sprintf(serverRxXS, "/local/domain/%d/%s", destId,
           p);

   // We act as a client so the servers Rx is our Tx.
   fprintf(stdout, "transmitChan: vchan init for xs=%s to domId=%d,\n",
           serverRxXS, sourceId );
   txCtrl = libxenvchan_client_init((xentoollog_logger *)logger,
                                    destId, serverRxXS);
 
   if(txCtrl == NULL) {
      return NULL;
   }
   return txCtrl;
}


xentoollog_logger_stdiostream * createDebugLogger(){

  return xtl_createlogger_stdiostream(stdout, XTL_DEBUG, 0);
}


/** Read the domainID of the subExp1 from the mgrExp1 over the
 * vChan.
 * This routine will block until the domainID is retrieved from 
 * the mgr.
 * return - the domainID. On any failure exit(1) is called.
 **/
int readSubExp1DomainID( xentoollog_logger * xc_logger, struct libxenvchan * ctrl) {
  char buf[DOMAIN_ID_CHAR_LEN + 1];
  int size = DOMAIN_ID_CHAR_LEN;
  char * invalidChar;
  int domainID;

  // Read the subExp1 domain ID from mgrExp1 vchan client.
  // The domainID has to be right justified for the following error
  // checking to work. So the domainID number must be preceeded with
  // white space or '0's.
  size = libxenvchan_recv(ctrl, buf, size);

  // Was there a system error?
  if (size < 0) {
    // There was a significant error. Abort.
    fprintf(stderr, "libxenvchan_read return=%d.\n", size);
    perror("readSubDomain: read failed for mgrExp1.");
    libxenvchan_close(ctrl);
    exit(1);
  }

  // Did we get all of the characters of the domainID number string?
  if (size != DOMAIN_ID_CHAR_LEN) {
    fprintf(stderr, "readSubDomain: We expected to read %d characters for the"
	    "subExp1 domain ID but got %d from mgrExp1\n",
	    DOMAIN_ID_CHAR_LEN, size);
    libxenvchan_close(ctrl);
    exit(1);
  }

  buf[DOMAIN_ID_CHAR_LEN] = 0; // put null at end of string so we have a valid C string.
  // Convert the string to an integer.
  domainID = strtol(buf, &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "readSubDomain: There was an invalid character in the domainID. The invalid portion of the domainID is '%s'.\n", invalidChar);
    libxenvchan_close(ctrl);
    exit(1);
    
  }

  return domainID;
}
//####################################################################

struct libxenvchan * createReceiveChan (xentoollog_logger * xc_logger, int id){
  return createReceiveChanP(xc_logger,id,NULL);
}

//####################################################################
struct libxenvchan * createReceiveChanP (xentoollog_logger * xc_logger, int id, char * path){
  struct libxenvchan *rxCtrl=0;
  char  p[256];

  if (!path){
   sprintf(p,"%s", SERV_REL_RX_XS_PATH);
  }else{
    sprintf (p,"%s",path);
  }
  sprintf(p, "%s_%d",p,id);

  // We act as a server for our RX.
  fprintf(stdout, "receiveChan: vchan init for xs=%s to domId=%d,\n",
	  p, id );
  rxCtrl = libxenvchan_server_init(xc_logger,
                                   id, p, 0, 0);

  if(rxCtrl == NULL) {
    // We had an error trying to initialise the server vchan.
    char * lclErrStr = strerror(errno);
    fprintf(stderr, "Error: %s: libxenvchan_server_init: domId=%d, xsPath=%s.\n",
            lclErrStr, id, p);
    if(errno == ENOENT) {
      fprintf(stderr, "    kernel module xen_gntalloc (/dev/xen/gntalloc) or xen_evtchn (/dev/xen/evtchn) may not be running.\n");
    }
    exit(1);
  }
 // rxCtrl->blocking = 1; // Block for each vchan IO ?
  
  return rxCtrl;

}
//####################################################################

struct libxenvchan * createTransmitChan(xentoollog_logger * xc_logger, int destId){
  int sourceId = getDomId();
  return createTransmitChanP(xc_logger,destId, sourceId,NULL);
}

//####################################################################

struct libxenvchan * createTransmitChanP(xentoollog_logger * xc_logger, int destId, int sourceId, char * path){
  struct libxenvchan *txCtrl=0;
  char serverRxXS [256]; // xenStore path for the server's receive.
  char  p[256];
  if (!path){
    sprintf (p,"%s",SERV_REL_RX_XS_PATH);
  }else{
    sprintf (p,"%s",path);
  }
  
  sprintf(p, "%s_%d",p,sourceId);
  sprintf(serverRxXS, "/local/domain/%d/%s", destId,
          p);

  // We act as a client so the servers Rx is our Tx.
  fprintf(stdout, "transmitChan: vchan init for xs=%s to domId=%d,\n",
          serverRxXS, sourceId );
  txCtrl = libxenvchan_client_init((xentoollog_logger *)xc_logger,
                                   destId, serverRxXS);

  if(txCtrl == NULL) {
    // We had an error trying to initialise the client vchan.
    char * lclErrStr = strerror(errno);
    fprintf(stderr, "Error: %s: libxenvchan_client_init: domId=%d, xsPath=%s.\n",
            lclErrStr, destId, serverRxXS);
    if(errno == ENOENT) {
      fprintf(stderr, "    kernel module xen_gntalloc (/dev/xen/gntalloc) or xen_evtchn (/dev/xen/evtchn) may not be running.\n");
    }
    exit(1);
  }
//  txCtrl->blocking = 1; // Block for each vchan IO ?

return txCtrl;

}

//####################################################################

int sendClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl,
                       char * msg, int size ) {
  int result = 0;
  int writeSize;
  //char fmt[256];
  // Create the format for writing the message. 
  // This really should be done once during initialization

  /*fprintf(stdout,"HEXDUMP\n");
  for (i = 0; i < size; i++){
   fprintf(stdout,"%02x ",msg[i]);
  }
   fprintf(stdout,"\n");
*/

  writeSize = libxenvchan_send(txCtrl, msg, size);
  if (writeSize < 0) {
    perror("vchan to serverExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write serverExp1 size=0?");
    exit(1);
  }
  if (writeSize != size) {
    fprintf(stdout,"wrote %d totalsize %d\n",writeSize,size);
  //  perror("write writeExp1 failed to write whole buffer.");
   // exit(1);
  }


  return result;
}

//####################################################################

int sendChunkedMessage(xentoollog_logger * xc_logger, 
struct libxenvchan * txCtrl, char * msg, int size ) {

  int headerSize = 8;
  int maxSize = 1024;
  int availSize = maxSize - headerSize;
  char *buf = 0;
  char *chunk;
  int writeSize = 0;
  int result = 0;
  int i = 0;
  int idx = 0;
  int n = 1;

  //If this fails we need to change the header size to have more digits
  if ( size > 9999999){
    fprintf(stderr, "Message size was too large (maxSize: (%d), will need to change the header size to send a message of that size: %d\n",9999999, size);
    perror("sendChunkedMessage: send failed.");
    exit(1);
  }

  //We can send a maximum of 1024, do we need to send the message in chunks?
  if (size > availSize){
    buf = (char*)malloc(sizeof(char) * maxSize);

    if ( buf == NULL){
      fprintf(stderr, "Error: Failed to allocate space for buffer\n");
      perror("sendChunkedMessage: send failed.");
      exit(1);
    }

    buf[0] = (char) 255; //chunked
    sprintf(buf+1,"%7d",size);
    sprintf(buf+headerSize,"%7d", availSize-headerSize+1);
    memcpy(buf+(2*headerSize -1),msg,(maxSize - (2*headerSize -1)));
  
  // Can send the entire message
  }else{

    buf = (char *)malloc(size * sizeof(char)+ sizeof(char) * (2*headerSize -1));
    if ( buf == NULL){
      fprintf(stderr, "Error: Failed to allocate space for buffer\n");
      perror("sendChunkedMessage: send failed.");
      exit(1);
    }
   
    buf[0] = (char) 254; // not chunked
    sprintf(buf+1,"%7d",size);
    memcpy(buf+headerSize,msg,size);
/*    printf("HexDump\n");
    for (i = 0; i < size+headerSize;i++){
        printf("%2x ",buf[i]);
    }
    printf("\n");
*/
  }
  
  //chunk
  if (size > availSize){
/*    printf("HexDump\n");
    for (i = 0; i < maxSize;i++){
        printf("%2x ",buf[i]);
    }
    printf("\n");
*/
    // chunked totalsize payloadSize payload
    // loop until we have enough space
    while(libxenvchan_buffer_space(txCtrl) <maxSize){
      sleep(0.1);
    }
    writeSize = libxenvchan_write(txCtrl,buf,maxSize); 

    free(buf);
    n++;
    idx+=availSize-headerSize+1;

    chunk = (char*)malloc(sizeof(char) * maxSize);

    if ( chunk == NULL){
      fprintf(stderr, "Error: Failed to allocate space for buffer\n");
      perror("sendChunkedMessage: send failed.");
      exit(1);
    }

    chunk[0] = (char) 255;
    sprintf(chunk+1,"%7d",availSize);

    for ( i = 1; i < (size/availSize);i++){
      
      memcpy(chunk+headerSize, msg+(idx), availSize);
      idx+=availSize;
/*      printf("Chunk\n");
      for (j = 0; j < maxSize;j++){
          printf("%2x ",chunk[j]);
      }
      printf("\n");
*/
      //wait until there is enough space
      while(libxenvchan_buffer_space(txCtrl) < maxSize){
          sleep(0.1);
//         printf("Waiting for space to write\n");
      }
      //send chunk
      writeSize += libxenvchan_write(txCtrl,chunk, maxSize);
      n++;
    }
    //send leftover chunk

//    printf("Sending Leftover chunk\n");
    sprintf(chunk+1,"%7d", size-idx);
    memcpy(chunk+headerSize, msg+(idx),size -idx); 
/*    for (j = 0; j < size -idx + headerSize; j++){
      printf("%2x ",chunk[j]);
    }
     printf("\n");
*/
    while(libxenvchan_buffer_space(txCtrl) < maxSize){
         sleep(0.1);
//         printf("Waiting for space to write\n");
      }
    writeSize+= libxenvchan_write(txCtrl,chunk,size-idx+headerSize );
    free(chunk);
  //no chunk
  }else{
    while(libxenvchan_buffer_space(txCtrl) < maxSize){
         sleep(0.1);
//         printf("Waiting for space to write\n");
      }
    writeSize = libxenvchan_write(txCtrl, buf, size+headerSize);
    free(buf);
  }
  if (writeSize < 0) {
    perror("vchan to serverExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write serverExp1 size=0?");
    exit(1);
  }
  if (writeSize != size+(n*headerSize)+7) {
    fprintf(stdout,"wrote %d totalsize %d\n",writeSize,size+(n*headerSize)+7);
  //  perror("write writeExp1 failed to write whole buffer.");
  //  exit(1);
  }

  return result;
}

int isChunked(char *header){
  if ( header == NULL){
    return -1;
  }
return header[0] == (char)255;
}
int getSize(char *header){
  int sum = 0;
  int i = 0;
  if (header == NULL){
    return -1;
  }
  for (i = 0; i < 7; i++){
      if (header[i]== ' '){
         continue;
      }
      if (header[i] >='0' && header[i] <='9'){
        sum = sum* 10 + (int) (header[i]-'0');
      }else{
        break;
      }
   }

   return sum;
}


//####################################################################

char * readChunkedMessage(xentoollog_logger *xc_logger, struct libxenvchan *ctrl, int *sz){
   int headerSize = 8;
   char * header = (char *) malloc ( headerSize * sizeof(char));
   char * mesg;
   int size;
   //int i = 0;
   char * chunk;
   int chunkSize = 0;
   char * p;
   int messageSize = 0;
   
   if ( header == NULL){
      fprintf(stderr, "Error: Failed to allocate space for buffer\n");
      perror("readChunkedMessage: read failed.");
      exit(1);
   }

   //get Header
   while(!libxenvchan_data_ready(ctrl)){
     sleep(0.10);
   }
   size = libxenvchan_read(ctrl,header, headerSize);  
   if ( size != headerSize){
    fprintf(stderr, "Error: libxenvchan_read return=%d. should be %d\n", size, headerSize);
    perror("readChunkedMessage: read failed 1.");
    exit(1);
   }
    
    messageSize = getSize(header+1);
  //  printf("messageSize: %d\n",messageSize);
    *sz = messageSize;
/*
    printf("HexDump\n");
    for (i = 0; i < 5;i++){
        printf("%2x ",header[i]);
    }
    printf("\n");
*/
   //allocate space for entire message 
   mesg = (char *)malloc(sizeof(char) * messageSize);   
    if(mesg == NULL) {
         char * lclErrStr = strerror(errno);
         fprintf(stderr, "Error: %s: readChunkedMessage failed\n",
             lclErrStr);
         perror("readChunkedMessage: read failed");
    }
   p = mesg;
   
   if (isChunked(header) ){
       
/*     printf("Chunked Data\n");
     printf("DataReady: %d\n",libxenvchan_data_ready(ctrl));
*/
   while(!libxenvchan_data_ready(ctrl)){
      sleep(0.10);
   }
     //get second size
     size = libxenvchan_read(ctrl,header, headerSize-1);
     //printf("finished read\n");

     if ( size != headerSize-1){
       fprintf(stderr, "libxenvchan_read return=%d. should be %d\n", size, headerSize-1);
       perror("readChunkedMessage: read failed 2.");
       //exit(1);
     }
     
     //printf("getting size\n");
     chunkSize = getSize(header);
     //printf("done with size: %d\n",chunkSize);
     
     //allocate chunk
     chunk = (char *)malloc(sizeof(char) *chunkSize); 
     if ( chunk == NULL){
        fprintf(stderr, "Error: Failed to allocate space for chunk\n");
        perror("readChunkedMessage: read failed.");
        exit(1);
     }
 
     //read chunk
     while(!libxenvchan_data_ready(ctrl)){
       sleep(0.10);
     }
     size = libxenvchan_read(ctrl,chunk, chunkSize);
     //printf("Reading Chunk\n");
     
     if ( size != chunkSize){
       fprintf(stderr, "libxenvchan_read return=%d. should be %d\n", size, chunkSize);
       perror("readChunkedMessage: read failed 3.");
       exit(1);
     }
     memcpy(p,chunk,chunkSize);
     p= p+chunkSize;

     free(chunk);
/*     printf("HexDump\n");
     for (i = 0; i < messageSize;i++){
       printf("%2x ",mesg[i]);
     }
     printf("\n");
*/
     while(p - mesg < messageSize){
        //printf("DataReady: %d\n",libxenvchan_data_ready(ctrl));
        if (libxenvchan_data_ready(ctrl)>headerSize){
          size = libxenvchan_read(ctrl,header, headerSize);  


          if ( size != headerSize){
            fprintf(stderr, "libxenvchan_read return=%d. should be %d\n", size, headerSize);
            perror("readChunkedMessage: read failed 4.");
            exit(1);
          }
          //printf("getting size2\n");
          chunkSize = getSize(header+1);
          //printf("done with size2: %d\n",chunkSize);
          
          //allocate chunk
          chunk = (char *)malloc(sizeof(char) *chunkSize); 
          if ( chunk == NULL){
             fprintf(stderr, "Error: Failed to allocate space for chunk\n");
             perror("readChunkedMessage: read failed.");
             exit(1);
          }

          
          //loop until we have data ready
          while (libxenvchan_data_ready(ctrl)<chunkSize){
            sleep(0.10);
          };
          size = libxenvchan_read(ctrl,chunk, chunkSize);
          if ( size != chunkSize){
            fprintf(stderr, "libxenvchan_read return=%d. should be %d\n", size, chunkSize);
            perror("readChunkedMessage: read failed 5.");
            exit(1);
          }
          memcpy(p,chunk,chunkSize);
          p= p+chunkSize;

          free(chunk);
/*          printf("Math: %d\n",p-mesg);
          printf("HexDump\n");
          for (i = 0; i < messageSize;i++){
            printf("%2x ",mesg[i]);
          }
*/
        }
     }
   }else{
     //printf("Not Chunked\n");
     size = libxenvchan_read(ctrl,mesg,messageSize);
   }
     free(header);
/*     printf("Size Data: %d\n",messageSize);

     for (i = 0; i < messageSize; i++){
        printf("%c",mesg[i]);
     }
     printf ("\n");
     printf("HexDump\n");
     for (i = 0; i < messageSize;i++){
        printf("%2x ",mesg[i]);
     }
     printf("\n");
*/
  return mesg;
}

//####################################################################
int readClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * ctrl,
                        char * msg, int * sz) {
  int result = 0;
  int size;
  //char * invalidChar;
  //char * buf = (char *) malloc (sizeof(char) * (*sz));
  //int i = 0;
/*
  fprintf(stdout,"size:%d\n", *sz);
*/  
  size = libxenvchan_recv(ctrl, msg, *sz);
/*
  fprintf(stdout,"Receive strlen: %d\n",size);
  fprintf(stdout,"%s",msg);
  fprintf(stdout,"HEXDUMP\n");
  for (i = 0; i < size; i++){
   fprintf(stdout,"%02x ",msg[i]);
  }
   fprintf(stdout,"\n");
*/
  *sz = size;
  // Was there a system error?
  if (size < 0) {
    // There was a significant error. Abort.
    fprintf(stderr, "libxenvchan_recv return=%d.\n", size);
    perror("serverExp1: read failed for clientExp1.");
    exit(1);
  }
  /*
  // Did we get all of the characters in the message.
  if (size != EXP1_MSG_LEN) {
    fprintf(stderr, "serverExp1: We expected to read %d characters for the"
            "clientExp1 but got %d from clientExp1\n",
            EXP1_MSG_LEN, size);
    exit(1);
  }
*/
//  msg[size] = 0; // put null at end of string so we have a valid C string.
  // Convert the string to an integer.
  return result;
}

//Simple Functions
//####################################################################
/*
struct libxenvchan * vchan_client_init (int serverId){
  xentoollog_logger_stdiostream * logger =  createDebugLogger();
  int clientId = getDomId();
  struct libxenvchan * chan  =createTransmitChanP((xentoollog_logger *)logger, serverId, clientId, "data/serverVchan");
  
  xtl_logger_destroy((xentoollog_logger *)logger);

  return chan;
}

//####################################################################

struct libxenvchan * vchan_server_init (int clientId){
  xentoollog_logger_stdiostream * logger =  createDebugLogger();
  struct libxenvchan * chan  =createReceiveChanP((xentoollog_logger *)logger, clientId, "data/serverVchan");

  xtl_logger_destroy((xentoollog_logger *)logger);

  return chan;
}
*/
//####################################################################

int vchan_send(struct libxenvchan * chan, char * message, int size){
  xentoollog_logger_stdiostream * logger =  createDebugLogger();

  int res =  sendChunkedMessage((xentoollog_logger *)logger, chan, message, size);
  xtl_logger_destroy((xentoollog_logger *)logger);
  return res;
}

//####################################################################

char * vchan_receive(struct libxenvchan * chan, int* size){
  xentoollog_logger_stdiostream * logger =  createDebugLogger();

  char * msg=readChunkedMessage((xentoollog_logger *)logger,chan,size); 

  xtl_logger_destroy((xentoollog_logger *)logger);
  return msg;
}
