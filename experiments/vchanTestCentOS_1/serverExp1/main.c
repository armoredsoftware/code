/**
 * The main source file for the serverExp1 app of the 
 * vchanTestCentOS_1. experiement.
 *
 * Usage: serverExp1
 * 
 * NOTE: For this application to work the zen kernel modules
 * xen_gntalloc and xen_evtchn must be loaded prior to running the application,
 * otherwise you will get an error of "No such file or directory."
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
//###################################################################

/**
 * xc_logger - may be null.
 * ctrl - libvchan server control structure that has been initialized.
 * count - the number to send to the client.
 */
int sendCountToClient(xentoollog_logger * xc_logger, struct libxenvchan * ctrl,
		      int count) {

  int writeSize;
  char buf[EXP1_MSG_LEN + 1];
  char fmt[256];
  int result = 0;

  /* Create the format for writing the message. */
  // This really should be done once during initialization
  sprintf(fmt, "%% %dd", EXP1_MSG_LEN); 

  sprintf(buf, fmt, count);
  buf[EXP1_MSG_LEN] = '\0';

  writeSize = libxenvchan_write(ctrl, buf, EXP1_MSG_LEN);
  if (writeSize < 0) {
    perror("vchan to clientExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write clientExp1 size=0?");
    exit(1);
  }
  if (writeSize != EXP1_MSG_LEN) {
    perror("write clientExp1 failed to write whole buffer.");
    exit(1);
  }

  return result;
}

//###########################################################################

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


//####################################################################

int main(int argc, char **argv)
{
  int clientExp1DomainId; // domainID of the clientExp1 VM. provided by mgrExp1.
  xentoollog_logger_stdiostream * xc_logger;
  struct libxenvchan * txCtrl = 0;
  struct libxenvchan * rxCtrl = 0;
  int servCount;
  int clientCount;
  int vchanStatus;

  if (argc != 1) {
    fprintf(stdout, "ERROR: serverExp1: no arguments are given to this application.\n");
  }

  fprintf(stdout, "serverExp1: starting\n");


  // Make a logger for debug and warning information from the
  // libxc
  xc_logger = xtl_createlogger_stdiostream(stdout, XTL_DEBUG, 0);
  if(xc_logger == NULL) {
    printf("!!! Error Failed to create libxc logger.");
    exit(1);
  }

  // Get the domID of the clientExp1 from the mgrExp1
  clientExp1DomainId = readSubExp1DomainID((xentoollog_logger *)xc_logger);

  //setup receive chan as server
  rxCtrl = createReceiveChan((xentoollog_logger *)xc_logger, clientExp1DomainId); 


  sleep(2);

  //setup transmit to client's receive chan as a client
  txCtrl = createTransmitChan((xentoollog_logger *)xc_logger, clientExp1DomainId); 

  // Do forever.
  servCount = 1;
  for(;;) {

    sleep(1);

    // Send the count number to the client.
    fprintf(stdout, "servExp1: send %d to client.\n", servCount);
    vchanStatus = sendCountToClient((xentoollog_logger *)xc_logger, txCtrl, servCount);

    if (vchanStatus != 0) {
      fprintf(stderr, "Failed to send message to client.\n");
      exit(1);
    }

    vchanStatus = checkClientResponse((xentoollog_logger *)xc_logger, rxCtrl, &clientCount);
    if (vchanStatus != 0) {
      fprintf(stderr, "Failed to receive message from client.\n");
      exit(1);
    }

    fprintf(stdout, "servExp1: received %d from client.\n\n", clientCount);    

    servCount++;
  }

  libxenvchan_close(txCtrl);
  libxenvchan_close(rxCtrl);

  xtl_logger_destroy((xentoollog_logger *)xc_logger);

  return 0;
}
