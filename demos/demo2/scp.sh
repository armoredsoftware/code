#!/bin/bash
#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

scp Appraiser apprKeys.txt root@10.100.0.228: ;  #Appraiser
scp Attestation attKeys.txt root@10.100.0.247: ; #Attester
scp Measurer root@10.100.0.222: ; #Measurer

