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
std::string type;
std::string constructor;
int *mycontents;
int mysize;
EvidencePiece::EvidencePiece(Json::Value & value){


	type=value["tag"].asString();
	std::string expected = "EvidencePieceW";
	//std::vector<char> contents;

	if(type.compare(expected)!=0){
		std::cout << endl << endl << "ABSOLUTE FAILURE. THIS IS NOT AN EVIDENCEPIECEWRAPPER";
		std::cout <<endl << "What are you trying to pull?"<< endl<<endl;


		return;
	}

	std::cout <<endl << "type: " << type << endl;


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

Json::Value toJSON(){
	Json::Value jsonValueW;
	Json:: Value jsonValue;
	Json::Value array;
	for (int i = 0; i < mysize; ++i) {
		array.append(mycontents[i]);
	}
	jsonValue["tag"] = constructor;
	jsonValue["m0Rep_EvidencePiece"] = array;
	jsonValueW["tag"] = "EvidencePieceW";
	jsonValueW["evidencePiece"] = jsonValue;
	return jsonValue;
}
std::string EvidencePiece::getRaw(){
	return type;
}
EvidencePiece::~EvidencePiece() {
	// TODO Auto-generated destructor stub
}

