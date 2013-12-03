Armored Software Compute Node Xen Setup
=======================================

This directory contains the scripts for configuring individual 'compute' nodes for
running the Xen hypervisor and XenServer.

It is assumed that the compute node has already been setup for networking behind a router connected to the Enterprise network. See ../networking/README.md.

One Time Xen Build
------------------

Xen only has to be built once. It can then be installed on all of the compute nodes.
    > sudo 

Xen Install on Compute Node
---------------------------

- Log into the cloud router node:
    > ssh smelt # or whatever the router's Enterprise host name is.
- Log into the compute node:
    > ssh compute1 # or whatever the compute nodes hostname is.
- cd <TOP_LEVEL>/compute/xen
- Install the Xen hypervisor and kernel with zen.
    > sudo ./xen.sh
    > sudo reboot
- Build the XAPI. If you do not have a tarball of the xencorebuild then do the following otherwise skip this step
    > sudo ./xencorebuild.sh
- If you have a xencorebuild tarball and have not installed it yet then do the following otherwise do this step:
    > sudo mkdir /xenserver-core-build
    > cd /xenserver-core-build
    > tar zxf ~/xenserver-core-latest.tgz # or whatever name you have for the tarball.
- Install the XAPI
    > sudo ./xencoreinstall.sh
- Answer the following questions
 - 