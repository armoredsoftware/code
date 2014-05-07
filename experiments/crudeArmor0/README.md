This is an experiment to learn how to communicate between domains and to do some primitive
measurement of a target.

There will be 4 domains involved.
* appraisor (most likely domain-0)
* attest
* measure
* target

All domains run on the the same hypervisor (although the appraisor domain
could be on an diffrent host since it uses eithernet network to communicate with the
attestor.

## Communication

### appraisor - attest
### attest - measure 

## Measurement

### target present
This measurement determines of the target domain is running on the same hyporvisor
as the measurement domain.
