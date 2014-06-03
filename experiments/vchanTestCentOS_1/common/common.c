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

#include "../include/exp1Common.h"

int getDomId(void){
  struct xs_handle   *handle= 0;
  int selfId = -1;
  handle = xs_open(XS_OPEN_READONLY);
  if ( handle == NULL){
    fprintf(stderr, "Unabled to open an interface\n");
    perror("getDomId: xs_open Failed to obtain a handle.");
    return -1;
  }
  selfId =(int) strtol((char *)xs_read(handle, XBT_NULL,"domid",NULL),(char **)NULL,10);
  return selfId;
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
    // We had an error trying to initialise the client vchan.
    char * lclErrStr = strerror(errno);
    fprintf(stderr, "Error: %s: libxenvchan_client_init: domId=%d, xsPath=%s.\n",
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

struct libxenvchan * createTransmitChan(xentoollog_logger * xc_logger, int destId, int sourceId){
  return createTransmitChanP(xc_logger,destId,sourceId,NULL);
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


/*
// ###################################################################
// Get a count from a server over a vchan.
// xc_logger - may be null
// rxCtrl - vchan control structure created by call to libxenvchan_server_init
// servCount - the msg count read from the vchan server.
// return - non 0 is an error.
int receiveCountFromServer(xentoollog_logger * xc_logger,
			   struct libxenvchan * rxCtrl,  int * servCount) {
  int result = 0;
  char buf[256];
  int size;
  char * invalidChar;

  size = EXP1_MSG_LEN;

  // Read the correct number of characters from the server.
  size = libxenvchan_read(rxCtrl, buf, size);

  // Was there a system error?
  if (size < 0) {
    // There was a significant error. Abort.
    fprintf(stderr, "libxenvchan_read return=%d.\n", size);
    perror("clientExp1: read failed for serverExp1.");
    exit(1);
  }

  // Did we get all of the characters in the message.
  if (size != EXP1_MSG_LEN) {
    fprintf(stderr, "clientExp1: We expected to read %d characters for the"
	    "serverExp1 but got %d from serverExp1\n",
	    EXP1_MSG_LEN, size);
    exit(1);
  }

  buf[EXP1_MSG_LEN] = 0; // put null at end of string so we have a valid C string.
  // Convert the string to an integer.
  *servCount = strtol(buf, &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "clientExp1: There was an invalid character in the msg Count. The invalid portion of the msg count is '%s'.\n", invalidChar);
    exit(1);
    
  }

  return result;
}
*/
// ##################################################################
// Send a count to server over a vchan.
// xc_logger - may be null
// txCtrl - vchan control structure created by call to libxenvchan_server_init
// clientCount - the msg count write to  the vchan server.
// return - non 0 is an error.
int sendClientResponse(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl,
		       int clientCount) {
  int result = 0;
  int writeSize;
  char buf[EXP1_MSG_LEN + 1];
  char fmt[256];

  // Create the format for writing the message. 
  // This really should be done once during initialization
  sprintf(fmt, "%% %dd", EXP1_MSG_LEN); 

  sprintf(buf, fmt, clientCount);

  writeSize = libxenvchan_write(txCtrl, buf, EXP1_MSG_LEN);
  if (writeSize < 0) {
    perror("vchan to serverExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write serverExp1 size=0?");
    exit(1);
  }
  if (writeSize != EXP1_MSG_LEN) {
    perror("write writeExp1 failed to write whole buffer.");
    exit(1);
  }

  return result;
}

//####################################################################

int sendClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl,
                       char * msg, int size ) {
  int result = 0;
  int writeSize;
  char buf[size+1];
  //char fmt[256];
  int i = 0;

  // Create the format for writing the message. 
  // This really should be done once during initialization
  //sprintf(fmt, "%% %dd", EXP1_MSG_LEN);

  // sprintf(buf, fmt, clientCount);
  fprintf(stdout,"HEXDUMP\n");
  for (i = 0; i < size; i++){
   fprintf(stdout,"%02x ",msg[i]);
  }
   fprintf(stdout,"\n");
  strncpy(buf,msg,size);

  writeSize = libxenvchan_write(txCtrl, buf, size+1);
  if (writeSize < 0) {
    perror("vchan to serverExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write serverExp1 size=0?");
    exit(1);
  }/*
  if (writeSize != size+1) {
    fprintf(stdout,"wrote %d totalsize %d\n",writeSize,size);
  //  perror("write writeExp1 failed to write whole buffer.");
   // exit(1);
  }
*/

  return result;
}


//####################################################################
//####################################################################
int readClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * ctrl,
                        char * msg, int * sz) {
  int result = 0;
  int size;
  //char * invalidChar;
  int i = 0;

  fprintf(stdout,"size:%d\n", *sz);
  
  size = libxenvchan_read(ctrl, msg, *sz);
  fprintf(stdout,"Receive strlen: %d\n",size);
  fprintf(stdout,"HEXDUMP\n");
  for (i = 0; i < size; i++){
   fprintf(stdout,"%02x ",msg[i]);
  }
   fprintf(stdout,"\n");

  *sz = size;
  // Was there a system error?
  if (size < 0) {
    // There was a significant error. Abort.
    fprintf(stderr, "libxenvchan_read return=%d.\n", size);
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

//####################################################################

int checkClientResponse(xentoollog_logger * xc_logger, struct libxenvchan * ctrl,
                        int * count) {
  int result = 0;
  int size;
  char buf[256];
  char * invalidChar;

  size = EXP1_MSG_LEN;

  size = libxenvchan_read(ctrl, buf, size);

  // Was there a system error?
  if (size < 0) {
    // There was a significant error. Abort.
    fprintf(stderr, "libxenvchan_read return=%d.\n", size);
    perror("serverExp1: read failed for clientExp1.");
    exit(1);
  }

  // Did we get all of the characters in the message.
  if (size != EXP1_MSG_LEN) {
    fprintf(stderr, "serverExp1: We expected to read %d characters for the"
            "clientExp1 but got %d from clientExp1\n",
            EXP1_MSG_LEN, size);
    exit(1);
  }

  buf[EXP1_MSG_LEN] = 0; // put null at end of string so we have a valid C string.
  // Convert the string to an integer.
  *count = strtol(buf, &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "serverExp1: There was an invalid character in the msg count. The invalid portion of the msg count is '%s'.\n", invalidChar);
    exit(1);

  }

  return result;
}

