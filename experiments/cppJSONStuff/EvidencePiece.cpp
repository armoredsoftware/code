/*
 * EvidencePiece.cpp
 *
 *  Created on: Aug 8, 2014
 *      Author: paulkline
 */

#include "EvidencePiece.h"
#include "json.h"
#include <iostream>
#include <string>
#include <vector>
using namespace std;

EvidencePiece::EvidencePiece() {
	// TODO Auto-generated constructor stub

}
std::string mytype;
std::string myconstructor;
int *mycontents;
int mysize;
EvidencePiece::EvidencePiece(int *contents, int size, string constructorr){
  mycontents=contents;
  mysize = size;
  constructor=constructorr;

}
EvidencePiece::EvidencePiece(Json::Value & value){


	mytype=value["tag"].asString();
	std::string expected = "EvidencePieceW";
	//std::vector<char> contents;

	if(mytype.compare(expected)!=0){
	  std::cout << endl << endl << "ABSOLUTE FAILURE. Expected: " << expected << " actual: " << mytype;
		return;
	}

	//std::cout <<endl << "type: " << mytype << endl;


	//get the data type
	Json::Value jsonEvidencePiece = value["evidencePiece"];
	constructor= jsonEvidencePiece["tag"].asString();
	std::cout <<"constructor: " << constructor << endl;

	//std::cout <<jsonEvidencePiece["m0Rep_EvidencePiece"];


	//get the contents as ints I guess..
	Json::Value array = jsonEvidencePiece["m0Rep_EvidencePiece"];
	mysize = array.size();
	std::cout << "size the contents will be: " << mysize << endl;
	mycontents=(int *) malloc(array.size()*sizeof(int));
	std::cout << "mycontents: [";
	for (int i = 0; i < array.size(); ++i) {
		Json::ArrayIndex index =i;
		mycontents[i] =array.get(index,-999).asInt();
		std::cout << mycontents[i] << ", ";
	}
	std::cout << "]"<< endl;



	//mycontents=jsonEvidencePiece["m0Rep_EvidencePiece"].as);

}

Json::Value EvidencePiece::toJSON(){
	Json::Value jsonValueW;
	Json:: Value jsonValue;
	Json::Value array;
	for (int i = 0; i < mysize; ++i) {
		array.append(mycontents[i]);
	}
	jsonValue["tag"] = constructor;
	cout << "this is the constructoer I am about to make m reps: " << constructor;
        if(constructor=="M0"){
	  jsonValue["m0Rep_EvidencePiece"] = array;
	}else if(constructor=="M1"){
          jsonValue["m1Rep_EvidencePiece"] = array;
        }else if(constructor=="M2"){
          jsonValue["m2Rep_EvidencePiece"] = array;
        }
	
	jsonValueW["tag"] = "EvidencePieceW";
	jsonValueW["getEvidencePiece"] = jsonValue;
	return jsonValueW;
}

EvidencePiece::~EvidencePiece() {
	// TODO Auto-generated destructor stub
}

