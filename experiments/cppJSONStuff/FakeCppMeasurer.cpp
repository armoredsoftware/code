//============================================================================
// Name        : FakeCppMeasurer.cpp
// Author      : Paul Kline
// Version     :
// Copyright   : 2014
// Description : A fake measurer made in cpp. Fake meaning performs dummmy measurments., Ansi-style
//============================================================================

#include <iostream>
#include <fstream>
#include "json.h"
#include "EvidenceDescriptor.h"
#include "EvidencePiece.h"
#include <cstring>

extern "C"
{
   #include "exp1Common.h"  
}
#include <sys/mman.h>

using namespace std;

EvidenceDescriptor getEvidenceDescriptorFromJSON(string jsonString){
  Json::Value parsedFromString;
  Json::Reader reader; 
  bool parsingSuccessful = reader.parse(jsonString, parsedFromString);
  if(!parsingSuccessful){
    cout <<"Error parsing: " << jsonString << "\n\n";
    exit;
  }                                                                                                                      
  if(parsedFromString["tag"].asString() != "EvidenceDescriptorW"){
																  cout << "Wrong type sent. Expected 'EvidenceDescriptorW' for field 'tag' but instead found " << parsedFromString["tag"] <<            "\n";
    exit;														      
  }
  EvidenceDescriptor ed(parsedFromString["getEvidenceDescriptor"].asString());   
  return ed;
}

void respondToEd(EvidenceDescriptor evDes, struct libxenvchan * chan){
  //cout<< "Here is the contents of the evDes passed to respondToEd: " << evDes.getEvidenceDescriptor();
  int* evPieceContents;
  int size = 0;
  string constructor="";

  //The evPieceContents is the "made up" measurement taking place.
  if(evDes.getEvidenceDescriptor() == "D0"){
    evPieceContents = new int[1];
    evPieceContents[0]=0;
    size =1;
    constructor = "M0";
  }else if(evDes.getEvidenceDescriptor() == "D1"){
    evPieceContents = new int[2];
    evPieceContents[0]=0;
    evPieceContents[1]=1;
    size=2;
    constructor= "M1";
  }else if(evDes.getEvidenceDescriptor() == "D2"){
    evPieceContents = new int[3];
    evPieceContents[0]=0;
    evPieceContents[1]=1;
    evPieceContents[2]=2;
    size = 3;
    constructor= "M2";
  }
  EvidencePiece evPiece(evPieceContents,size,constructor);
  cout << "\nhere are the contents I have chosen: ";
  for(int i =0; i< evPiece.mysize;i++){
    cout << evPiece.mycontents[i] << ", ";
  }

  //yes, this string/char* stuff is neccessary to use send(...)
  string message = evPiece.toJSON().toStyledString();
  char * sendableMessage = new char[message.size()+1];
  strcpy(sendableMessage,message.c_str());
  cout << "sendable message: " << sendableMessage;
  send(chan, sendableMessage , message.size()); 
  

}
int main() {
	int clientID;
	cout << "AttesterID: ";
	cin >> clientID;
	struct xentoollog_logger* mrLog =(xentoollog_logger*)createDebugLogger();
	struct libxenvchan* vchan= server_init(mrLog, clientID);
while(true){	
	//wait for a message from the client.
	int waitValue=-999;
	if(libxenvchan_data_ready_wrapper(vchan) == 0){
	  waitValue = libxenvchan_wait_wrapper(vchan);
	}
        //just curious really.
	//	cout << "Wait value: " << waitValue << "\n";

	
	//This method returns the size of the data ready to read.
	int size = libxenvchan_data_ready_wrapper(vchan);
	char * message = receive(vchan, &size);
        //to see if it worked!
	cout << "I received this: " << message << "\n\n";

	EvidenceDescriptor ed;
   	ed = getEvidenceDescriptorFromJSON(message);
	respondToEd(ed, vchan);
 }

}


