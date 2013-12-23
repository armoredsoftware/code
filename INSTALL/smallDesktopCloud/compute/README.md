Armored Software Compute Node
=============================

This directory contains the scripts for configuring 'compute' nodes for
AmoredSoftware. We use Xen for hypervisor and OpenCloud for the computing 
services. 

There has to be at least 1 compute host in the cloud. Although some OpenStack clouds allow a 'control' node to also host a 'compute' node, we setup ArmoredSoftware cloud
with a dedicated 'control' node. There can be up to
99 compute hosts with the networking configuration used in these scripts.
For more compute nodes than 99 the IP addresses for the router, control and
networking hosts and then netmask and some scripts would have to be modified.

It is assumed that the cloud is to be a very small number of
hosts composed of Enterprise desktop PC hardware and software.

For ITTC hosts this consists of the ITTC LCAP, DHCP, sudoers, hosts, and other setup ITTC specific setups.

The cloud is on a network connected to the Enterprise network through a
router host. 

All of the 'compute' host machines must be the same exact hardware setup.
- Use the same motherboard and the same CPUs.
- Must have two NIC ports. All compute hosts must have the same NIC configuration. Example. One NIC on the motherboard and one NIC (with 1 port) in PCI slot 5.

Installation
============

There are 3 parts to the installation process:
- Start with a clean kickstart.
- Setup networking configuration to have the node on a network behind a router
that is connected to the Enterprise network.
- Setup the Xen software.
- Setup the OpenStack software (if wanted).

All instructions in this file will use <TOP_LEVEL> as the directory of the top
level of the installation script directory tree. To execute a scrip the current directory
must be the same as the directory the script is found.

## Kickstart the host for CentOS 6.5 ##
Connect the host computer to the ArmoredComputing network. 
Ensure the eth0 NIC (on the motherboard) is connected to the External/Managment network switch. Connect eth1 (the PCI plugin NIC) to the Data Network switch.
Perform a network boot of the host (if you don't know how then ask for help). Select the "Compute Node" option in the PXI boot menu. Hit the 'tab' key to edit the command line of the "Comute Node" option. Change the NODEID value to what ever you want for the node. It must be a number from 1 to 99 and must not be used by any other compute node. Hit return to begin the kickstart.

If you forget to change NODEID it is defaluted to '0'. This will be found in the %pre section of the kickstart and stop the install with an error.

## Configure host for the Armored network. ##

Follow the instructions in ./networking/README.md

## Configure the host for Xen. ##

Follow the instructions in ./xen/README.md

## Configure the host for OpenStack ##

Follow the intstructions in ./openstack/README.md.

