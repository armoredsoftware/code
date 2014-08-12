/*
 * EvidenceDescriptor.h
 *
 *  Created on: Aug 6, 2014
 *      Author: paulkline
 */
#include <iostream>
#ifndef EVIDENCEDESCRIPTOR_H_
#define EVIDENCEDESCRIPTOR_H_
using namespace std;

class EvidenceDescriptor {
public:
	EvidenceDescriptor();
	EvidenceDescriptor(std::string x);
	std::string getEvidenceDescriptor();
	virtual ~EvidenceDescriptor();
	void SetEvidenceDescriptor( std::string evidenceDes);
	};

#endif /* EVIDENCEDESCRIPTOR_H_ */
