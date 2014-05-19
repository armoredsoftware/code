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
#include <unistd.h>
#include <time.h>

#include <libxenvchan.h>

#include <exp1Common.h>

/*
int libxenvchan_write_all(struct libxenvchan *ctrl, char *buf, int size)
{
	int written = 0;
	int ret;
	while (written < size) {
		ret = libxenvchan_write(ctrl, buf + written, size - written);
		if (ret <= 0) {
			perror("write");
			exit(1);
		}
		written += ret;
	}
	return size;
}

int write_all(int fd, char *buf, int size)
{
	int written = 0;
	int ret;
	while (written < size) {
		ret = write(fd, buf + written, size - written);
		if (ret <= 0) {
			perror("write");
			exit(1);
		}
		written += ret;
	}
	return size;
}


#define BUFSIZE 5000
char buf[BUFSIZE];
void reader(struct libxenvchan *ctrl)
{
	int size;
	for (;;) {
		size = rand() % (BUFSIZE - 1) + 1;
		size = libxenvchan_read(ctrl, buf, size);
		fprintf(stderr, "#");
		if (size < 0) {
			perror("read vchan");
			libxenvchan_close(ctrl);
			exit(1);
		}
		size = write_all(1, buf, size);
		if (size < 0) {
			perror("stdout write");
			exit(1);
		}
		if (size == 0) {
			perror("write size=0?\n");
			exit(1);
		}
	}
}

void writer(struct libxenvchan *ctrl)
{
	int size;
	for (;;) {
		size = rand() % (BUFSIZE - 1) + 1;
		size = read(0, buf, size);
		if (size < 0) {
			perror("read stdin");
			libxenvchan_close(ctrl);
			exit(1);
		}
		if (size == 0)
			break;
		size = libxenvchan_write_all(ctrl, buf, size);
		fprintf(stderr, "#");
		if (size < 0) {
			perror("vchan write");
			exit(1);
		}
		if (size == 0) {
			perror("write size=0?\n");
			exit(1);
		}
	}
}
*/

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

  /* Create the format for writing the message. */
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

int main(int argc, char **argv)
{
  struct libxenvchan *rxCtrl = 0;
  struct libxenvchan *txCtrl = 0;
  int mgrDomainID = MGR_DOMAIN_ID;
  int serverExp1DomainId; // domainID of the serverExp1 VM. provided by mgrExp1.
  xentoollog_logger_stdiostream * xc_logger;
  int servCount;
  int clientCount;
  int vchanStatus;

  if (argc != 1) {
    fprintf(stdout, "ERROR: clientExp1: no arguments are given to this application.\n");
  }

  fprintf(stderr, "clientExp1: starting\n");

  fprintf(stdout, "clientExp1: mgrExp1 domainID=%d.\n", mgrDomainID);
  fprintf(stdout, "clientExp1: xs path='%s'.\n", MGR_REL_XS_PATH );

  // Make a logger for debug and warning information from the
  // libxc
  xc_logger = xtl_createlogger_stdiostream(stdout, XTL_DEBUG, 0);
  if(xc_logger == NULL) {
    printf("!!! Error Failed to create libxc logger.");
    exit(1);
  }

  // Get the domID of the server that we get vchan messages from.
  serverExp1DomainId = readSubExp1DomainID((xentoollog_logger *)xc_logger);

  fprintf(stdout, "clientExp1: serverExp1 domainID=%d.\n", serverExp1DomainId);

  // Give the server a chance to get the server side of the channel setup.

  rxCtrl = createReceiveChan((xentoollog_logger *) xc_logger, serverExp1DomainId); 

  sleep(2);

  txCtrl = createTransmitChan((xentoollog_logger *)xc_logger, serverExp1DomainId); 


  // Do forever.
  for(;;) {
    // Send the count number to the client.
    vchanStatus = receiveCountFromServer((xentoollog_logger *)xc_logger, rxCtrl, &servCount);
    fprintf(stdout, "clientExp1: received %d from server.\n", servCount);

    if (vchanStatus != 0) {
      fprintf(stderr, "Failed to receive message from server.\n");
      exit(1);
    }

    clientCount = servCount + 100000;

    fprintf(stdout, "clientExp1: send %d to server.\n\n", clientCount);    

    vchanStatus = sendClientResponse((xentoollog_logger *)xc_logger, txCtrl, clientCount);
    if (vchanStatus != 0) {
      fprintf(stderr, "Failed to snd message to server.\n");
      exit(1);
    }


  }

  libxenvchan_close(txCtrl);
  libxenvchan_close(rxCtrl);

  xtl_logger_destroy((xentoollog_logger *)xc_logger);

  return 0;
}
