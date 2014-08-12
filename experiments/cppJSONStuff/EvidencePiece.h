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
	std::vector<unsigned char> contents;
	EvidencePiece(Json::Value & value);
	EvidencePiece();
	std::string getRaw();

	virtual ~EvidencePiece();
};

#endif /* EVIDENCEPIECE_H_ */
