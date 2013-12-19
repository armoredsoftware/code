Introduction
============
The scripts and data under this directory and sub directories
are for configuration of a small cloud (3 to 5 nodes) of desktop computers
for development purposes.

The cloud computers are separated from the enterprize network by host used as a 'router'.

The 'router' host machine has 3 hardware Network Interfaces. 
1) Connects to Enterprise Network
2) Connects to cloud External and Managment Network switch
3) Connects to cloud Data Network switch.

The small cloud has 2 hardware networks:
1) External and Management Network - handles communication to Enterprize network and Cloud Managment data. 
2) Data Network - handles node to node data communications

Each computer in the cloud has 2 hardware Network Interfaces
1) Conects to cloud External and Managment Network switch.
2) Connects to clould Data Network switch.

Each 'Compute' node in the cloud is currently using CentOS6.5 for Domain0.
'Compute' nodes should have at least 8GB of memory to allow for extenstive
testing and development. Domain0 is limited to 3GB of memory.
A user named 'armored' is setup for each 'Compute' Domain0. The 'armored' user
is given ALL priveledges for Domain0.


Installation Instructions
=========================

The cloud can be setup for Xen alone or OpenStack can be setup on top of Xen.

1. First setup the 'router'. Follow the instructions in ./router/README.md.
2. Install the 'compute' node. Follow the instructions in ./compute/README.md.
3. Install the 'networking' node if OpenStack is to be used.
Follow the instructions in ./networking/README.md.
4. For each computing node in the cloud follow the instructions in ./compute/README.md.


Directory Information
=====================

This file summaries the subdirectores.

* control: directory contains scripts for setting up the control node.
* compute: directory contains scripts for setting up the compute nodes.
* networking: directory contains scripts for setting up cloud networking node.
* router: directory contains scripts for setting out a router/gateway between
        the CLOUD and ITTC networks. This must be done before anything else.
* util: dictory that contains some common script functions and environment value.
