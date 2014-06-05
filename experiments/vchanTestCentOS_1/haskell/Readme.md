# VChan and Haskell #

There are Three Haskell Files to be aware of:

* VChanUtil.hs
* Mgr.hs
* Client.hs

## VChanUtil ##

VChanUtil handles all the Foreign Function Interface needed in order to connect
Haskell programs with the VChan Library written in C.

## Mgr ##
Example of an application that should be run on Dom-0, Currently only works if started after clients are created.
Application must be executed with sudo: sudo "./Mgr"

## Client ##
Example of an application to be run on a Dom-U VM that and receives messages form DOM-0


###Running the Applications ###

In order to run the applications in the repo:

* cd code/experiments/vChanTestCentOS_1/
* make
* cd haskell
* ghc Client.hs ../common/common.c -L/usr/lib64 -lxenvchan -lxenctrl
* ghc Mgr.hs ../common/common.c -L/usr/lib64 -lxenvchan -lxenctrl 
* scp Client root@[VM ip address]:

Login to the VM and run client
* ./Client

On Dom-0
* sudo ./Mgr

### VChan Notes ###

* Current setup requires Clients to be running before Dom-0 Haskell application
* Dom-0 <strong>cannot</strong> be used as a server for VChan, it can only act
as a client 
* VChan is used as a bi-directional communication mechanism between 2 dom-U VMs
using Xenstore from the Xen hypervisor
* When a VChan is created by the server entity, it gives permission for 1 client
to be able to read and write to the VChan. 
 * If a second Client tries to connect to the same VChan they will be rejected
 because they do not have permission 
 * If the server creates another VChan with the same name for the 2nd client the
first client will lose privileges on the VChan and will no longer be able to
read or write (Still need to verify this)


