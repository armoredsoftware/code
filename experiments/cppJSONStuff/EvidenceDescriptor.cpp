/*
 * EvidenceDescriptor.cpp
 *
 *  Created on: Aug 6, 2014
 *      Author: paulkline
 */

#include "EvidenceDescriptor.h"
std::string evidenceDescriptor;

EvidenceDescriptor::EvidenceDescriptor() {
	// TODO Auto-generated constructor stub

}
std::string EvidenceDescriptor::getEvidenceDescriptor(){
	return evidenceDescriptor;
}
EvidenceDescriptor::EvidenceDescriptor(std::string evidenceDes){
	evidenceDescriptor = evidenceDes;
}
void SetEvidenceDescriptor( std::string evidenceDes) {
 evidenceDescriptor = evidenceDes;
}

EvidenceDescriptor::~EvidenceDescriptor() {
	// TODO Auto-generated destructor stub
}

