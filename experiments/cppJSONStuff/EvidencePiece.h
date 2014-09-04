/*
 * EvidencePiece.h
 *
 *  Created on: Aug 8, 2014
 *      Author: paulkline
 */
#include <vector>
#include "json.h"
#ifndef EVIDENCEPIECE_H_
#define EVIDENCEPIECE_H_

class EvidencePiece {
public:
	std::string constructor;
	int* mycontents;
	int mysize;
	Json::Value toJSON(void);
	EvidencePiece(Json::Value & value);
	EvidencePiece(int *contents, int size,std::string constructorr);
	EvidencePiece();
	std::string getRaw();

	virtual ~EvidencePiece();
};

#endif /* EVIDENCEPIECE_H_ */
