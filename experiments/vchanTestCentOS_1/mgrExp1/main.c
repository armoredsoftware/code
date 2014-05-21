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
#include <xenctrl.h>

#include <exp1Common.h>

// #####################################################################


//######################################################################
// Structure for holding command line arguments.
typedef struct {
  int serverDomID; // domain id of server VM
  int clientDomID[2]; // domain id of client VM
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
  if ( argc != 4)  {
    fprintf(stderr, "Need 4 arguments, got %d args.\n", argc);
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
  cmdArgsVal->clientDomID[0] = strtol(argv[2], &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "mgrExp1: There was an invalid character in the client domainID. The invalid portion of the domainID is '%s'.\n", invalidChar);
    exit(1);
    
  }

  cmdArgsVal->clientDomID[1] = strtol(argv[3], &invalidChar, 10);

  if ( *invalidChar != '\0' ) {
    fprintf(stderr, "mgrExp1: There was an invalid character in the client domainID. The invalid portion of the domainID is '%s'.\n", invalidChar);
    exit(1);
    
  }
}

//####################################################################
void sendId(struct libxenvchan * chan, char * domIdStr){
  int writeSize;
  writeSize = libxenvchan_write(chan, domIdStr, DOMAIN_ID_CHAR_LEN);
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
}

//####################################################################

int main(int argc, char **argv)
{
  struct libxenvchan **txClientExp;
  char domIdStr[DOMAIN_ID_CHAR_LEN + 1];
  char domIdFmt[5];
  char path[128];
  int i,largestFd=-1, tmp=0, j;
  xc_interface * interface;
  int maxNumDoms= 0;
  int currentNumDoms;
  xc_dominfo_t *domains; 
  fd_set readfds;
  int val = 10;

  fprintf(stderr, "mgrExp1: starting...\n");

   sprintf(domIdFmt, "%%% dd", DOMAIN_ID_CHAR_LEN);
   printf("%%% dd", DOMAIN_ID_CHAR_LEN);

   // open interface to get data
   interface = xc_interface_open(NULL,NULL,XC_OPENFLAG_NON_REENTRANT);

   //find how many domains can be supported
   maxNumDoms = xc_get_max_nodes(interface);
   fprintf(stdout,"Maximum number of doms: %d\n", maxNumDoms);

   domains = (xc_dominfo_t *)malloc(maxNumDoms * sizeof(xc_dominfo_t));
   txClientExp = (struct libxenvchan **)malloc(maxNumDoms * sizeof(struct libxenvchan*));

   //get currently running domain information
   currentNumDoms = xc_domain_getinfo(interface,1,maxNumDoms, domains);
   fprintf(stdout,"Current number of Doms: %d\n",currentNumDoms);

   //Act as a client to each of the /local/data/domain/[id]/mgrVchan_0
   for (i = 0; i < currentNumDoms; i++){
      txClientExp[i] = createTransmitChanP(NULL,domains[i].domid, MGR_DOMAIN_ID, MGR_REL_XS_PATH);
      fprintf(stdout,"client init for %d\n",domains[i].domid);
      tmp = libxenvchan_fd_for_select(txClientExp[i]);
      FD_SET(tmp, &readfds);

      if (tmp > largestFd){
        largestFd = tmp;
      }
      sleep(2);
      sprintf(domIdStr, domIdFmt, val);
      domIdStr[DOMAIN_ID_CHAR_LEN] = 0; // terminate the string.
      fprintf(stdout,"MGR: Sending %s to %d\n",domIdStr, domains[i].domid);
      sendId(txClientExp[i],domIdStr);
   }
  /* 
   //wait until something happens
   for(;;){
      tmp = select(largestFd+1, &readfds,NULL,NULL,NULL);
      printf("We received something!\n"); 
      sleep(2);
   }   
*/
  
  exit(1);
  /*
  // Tell the serverExp1 what the clientExp1 domain id is.
  for (i =0; i< 2; i++){
      sprintf(domIdStr, domIdFmt, domains[(i+1)% currentNumDoms].domid);
      domIdStr[DOMAIN_ID_CHAR_LEN] = 0; // terminate the string.
      fprintf(stdout,"MGR: Sending %s to %d\n",domIdStr, domains[i].domid);
      sendId(txClientExp[i],domIdStr);
  }
  // Tell the clientExp1 what the serverExp1 domain id is.
  sprintf(domIdStr, domIdFmt, cmdArgsVal.serverDomID);
  domIdStr[DOMAIN_ID_CHAR_LEN] = 0; // terminate the string.
  for (i = 0; i < 2; i++){
    sendId(rxClientExp[i], domIdStr);
  }

  // Clean up before exit.
  libxenvchan_close(ctrlServExp);
  libxenvchan_close(rxClientExp[0]);
  libxenvchan_close(rxClientExp[1]);
*/
  return 0;
}
