#ifndef EXPCOMMON_H

#define EXPCOMMON_H

// The grant reference path for the serverExp1 and the clientExp1
// that the mgrExp1 uses to communicate with the serverExp1 an clientExp1
// over the vchan.
// The mgrExp1 (in domain0) acts as a vchan client.
// Both the serverExp1 and clientExp1 act as vchan servers when communicating
// with mgrExp1
#define MGR_REL_XS_PATH "data/mgrVchan"

// The grant reference path for the serverExp1 vchan server. This
// is the Tx channel from the servers point of view. Rx for the client point
// of view.
#define SERV_REL_TX_XS_PATH "data/serverVchanTx"
// The grant reference path for the serverExp1 vchan server. This
// is the Rx channel from the servers point of view. Tx for the client
// point of view.
#define SERV_REL_RX_XS_PATH "data/serverVchanRx"

// The domainID that the mgrExp1 is running in.
#define MGR_DOMAIN_ID 0

// The number of digits to allocate for the domain ids.
#define DOMAIN_ID_CHAR_LEN 6


// The number of characters to allocate for messages.
#define EXP1_MSG_LEN 8

int readSubExp1DomainID( xentoollog_logger * xc_logger, struct libxenvchan * ctrl);
int isNull(struct libxenvchan * chan);
struct libxenvchan * createReceiveChan (xentoollog_logger * xc_logger, int id);
struct libxenvchan * createReceiveChanP (xentoollog_logger * xc_logger, int id, char * optional_rel_path);

struct libxenvchan * createTransmitChan(xentoollog_logger * xc_logger, int destId);
struct libxenvchan * createTransmitChanP(xentoollog_logger * xc_logger, int destId, int sourceId, char * optional_rel_path);
int sendClientResponse(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl, int val);
int sendClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl, char * msg, int size );

int checkClientResponse(xentoollog_logger * xc_logger, struct libxenvchan * ctrl, int * count);
int readClientMessage(xentoollog_logger * xc_logger, struct libxenvchan * ctrl, char * msg, int * size);

char * readChunkedMessage(xentoollog_logger *xc_logger, struct libxenvchan *ctrl, int *size);
int sendChunkedMessage(xentoollog_logger * xc_logger, struct libxenvchan * txCtrl, char * msg, int size ); 

int getDomId(void);
xentoollog_logger_stdiostream * createDebugLogger(void);
struct libxenvchan * vchan_client_init(xentoollog_logger * xc_logger, int serverId);
struct libxenvchan * vchan_maybe_client_init(xentoollog_logger * xc_logger, int serverId);
struct libxenvchan * vchan_server_init(xentoollog_logger * xc_logger, int clientId);
int vchan_send(struct libxenvchan * chan, char * message, int size);
char * vchan_receive(struct libxenvchan * chan, int* size );

#endif
