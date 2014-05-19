/*
 * The main source file of the manager application that runs on dom0
 * for the vchanTestCentOS_1 experiment.
 *
 * Usage: mgrExp1 <server DomainID> <client DomainID>
 */

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <libxenvchan.h>

#include <exp1Common.h>

// #####################################################################


//######################################################################
// Structure for holding command line arguments.
typedef struct {
  int serverDomID; // domain id of server VM
  int clientDomID; // domain id of client VM
} cmdArgs;


//######################################################################

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

//--------------------------------------------------------------------
/**
 * Usage
 */

void usage(void) {
  printf("Usage: mgrExp1 <server DomainID> <clinet DomainID>\n");
}

//--------------------------------------------------------------------
/**
 * Process the command line arguments.
 */
void processArgs(int argc, char **argv, cmdArgs * cmdArgsVal) {

  char * invalidChar;

  // Do we have the correct number of arguments.
  if ( argc != 3)  {
    fprintf(stderr, "Need 3 arguments, got %d args.\n", argc);
    usage();
    exit(1);
  }

  // Get the server domain ID.
  cmdArgsVal->serverDomID = strtol(argv[1], &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "mgrExp1: There was an invalid character in the server domainID. The invalid portion of the domainID is '%s'.\n", invalidChar);
    exit(1);
    
  }

  // Get the server domain ID.
  cmdArgsVal->clientDomID = strtol(argv[2], &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "mgrExp1: There was an invalid character in the client domainID. The invalid portion of the domainID is '%s'.\n", invalidChar);
    exit(1);
    
  }

  
}

//####################################################################

int main(int argc, char **argv)
{
  struct libxenvchan *ctrlServExp = 0;
  struct libxenvchan *ctrlClientExp = 0;
  char xsServerPath[256];
  char xsClientPath[256];
  cmdArgs cmdArgsVal;
  char domIdStr[DOMAIN_ID_CHAR_LEN + 1];
  char domIdFmt[5];
  int writeSize;

  fprintf(stderr, "mgrExp1: starting...\n");

  // Get the command line arguments.
  processArgs(argc, argv, &cmdArgsVal);

  fprintf(stdout, "mgrExp1: service domID=%d, client domID=%d.\n", 
	  cmdArgsVal.serverDomID, cmdArgsVal.clientDomID);

  sprintf(domIdFmt, "%%% dd", DOMAIN_ID_CHAR_LEN);

  /* The Mgr acts as a client to the serverExp1 and clientExp1 to give them
   * the domain Ids of each other.
   */

  // Setup server connection
  sprintf(xsServerPath, "/local/domain/%d/%s", cmdArgsVal.serverDomID,
	  MGR_REL_XS_PATH);
  ctrlServExp = libxenvchan_client_init(NULL, cmdArgsVal.serverDomID,
					xsServerPath);

  // Handle any error from libxenvchan_client_init.
  if (!ctrlServExp) {
    char * lclErrStr = strerror(errno);
    fprintf(stderr, "Error: %s: libxenvchan_client_init: domId=%d, xsPath=%s.\n",
	    lclErrStr, cmdArgsVal.serverDomID, xsServerPath);
    exit(1);
  }
  ctrlServExp->blocking = 1; // Block for each vchan IO ?

  // Setup Client connection
  sprintf(xsClientPath, "/local/domain/%d/%s", cmdArgsVal.clientDomID,
	  MGR_REL_XS_PATH);
  ctrlClientExp = libxenvchan_client_init(NULL, cmdArgsVal.clientDomID,
  					  xsClientPath);

  // Handle any error from libxenvchan_client_init.
  if (!ctrlClientExp) {
    char * lclErrStr = strerror(errno);
    fprintf(stderr, "Error: %s: libxenvchan_client_init: domId=%d, xsPath=%s.\n",
	    lclErrStr, cmdArgsVal.clientDomID, xsClientPath);
    libxenvchan_close(ctrlServExp);
    exit(1);
  }
  ctrlClientExp->blocking = 1; // Block for each vchan IO ?

  // Tell the serverExp1 what the clientExp1 domain id is.
  sprintf(domIdStr, domIdFmt, cmdArgsVal.clientDomID);
  domIdStr[DOMAIN_ID_CHAR_LEN] = 0; // terminate the string.
  writeSize = libxenvchan_write(ctrlServExp, domIdStr, DOMAIN_ID_CHAR_LEN);
  if (writeSize < 0) {
    perror("vchan serverExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write serverExp1 size=0?\n");
    exit(1);
  }
  if (writeSize != DOMAIN_ID_CHAR_LEN) {
    perror("write clientExp1 failed to write whole buffer.\n");
    exit(1);
  }

  // Tell the clientExp1 what the serverExp1 domain id is.
  sprintf(domIdStr, domIdFmt, cmdArgsVal.serverDomID);
  domIdStr[DOMAIN_ID_CHAR_LEN] = 0; // terminate the string.
  writeSize = libxenvchan_write(ctrlClientExp, domIdStr, DOMAIN_ID_CHAR_LEN);
  if (writeSize < 0) {
    perror("vchan clientExp1 write");
    exit(1);
  }
  if (writeSize == 0) {
    perror("write clientExp1 size=0?\n");
    exit(1);
  }
  if (writeSize != DOMAIN_ID_CHAR_LEN) {
    perror("write clientExp1 failed to write whole buffer.\n");
    exit(1);
  }


  // Clean up before exit.
  libxenvchan_close(ctrlServExp);
  libxenvchan_close(ctrlClientExp);

  return 0;
}
