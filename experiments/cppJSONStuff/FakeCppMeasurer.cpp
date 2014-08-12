//============================================================================
// Name        : cppJSONTest.cpp
// Author      : Paul Kline
// Version     :
// Copyright   : Hello
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
#include <fstream>
//#include <string>
#include "json.h"
#include "EvidenceDescriptor.h"
#include "EvidencePiece.h"
using namespace std;


string readJSONFrom(const char* input){

	std::string line;
	std::string result;
	  ifstream myfile(input);
	  if (myfile.is_open())
	  {
	    while ( getline (myfile,line) )
	    {
	      std::cout << line << '\n';
	      result += line;
	    }
	    myfile.close();
	  }

	  return result;
}



int main() {
	//1. wait to receive
	//2. receive an EvidenceDescriptorW


//	Json::Value parsedFromString;
//	Json::Reader reader;
//
//	std::string jsonMessage = readJSONFrom("haskell_out_evidenceDescriptorW");
//	bool parsingSuccessful = reader.parse(jsonMessage, parsedFromString);
//
//	EvidenceDescriptor ed2(parsedFromString["evidenceDescriptor"].asString());
//	std::cout << "I read this in now as an object: ";
//	std::cout << ed2.getEvidenceDescriptor();
//	std::cout << endl << "Next I shall try to read in an evidencePiece" << endl;
//
//
//	Json::Value parsedFromString2;
//		std::string jsonMessage2 = readJSONFrom("haskell_out_evidencePieceW");
//		//std::cout << jsonMessage2;
//		bool parsingSuccessful2 = reader.parse(jsonMessage2, parsedFromString2);
//
//		//std::cout << parsedFromString2["tag"];
//		EvidencePiece ep(parsedFromString2);
//
//



}


