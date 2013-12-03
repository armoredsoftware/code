Armored Software Compute Node
=============================

This directory contains the scripts for configuring 'compute' nodes for
AmoredSoftware. We use Xen for hypervisor and Opencloud for the computing 
services. 

There has to be at least 1 compute host in the cloud. Although some OpenStack clouds allow a 'control' node to also host a 'compute' node, we setup ArmoredSoftware cloud
with a dedicated 'control' node. There can be up to
250 compute hosts with the networking configuration used in these scripts.
For more compute nodes than 250 the IP addresses for the router, control and
networking hosts and then netmask and some scripts would have to be modified.

It is assumed that the cloud is to be a very small number of
hosts composed of Enterprise desktop PC hardware and software.

For ITTC hosts this consists of the ITTC LCAP, DHCP, sudoers, hosts, and other setup ITTC specific setups.

The cloud is on a network connected to the Enterprise network through a
router host. ITTC users will have acces to their Home directory and to the
Projects directory. NOTE that the puppet service will be turned off on each
host in the cloud so NO changes to a user's groups, or sudo privledges made by
the ITTC sysadmins will make it to the the hosts on the cloud.

All of the 'compute' host machines must be the same exact hardware setup.
- Use the same motherboard and the same CPUs.
- Must have two NIC ports. All compute hosts must have the same NIC configuration. Example. One NIC on the motherboard and one NIC (with 1 port) in PCI slot 5.

Installation
============

There are 3 parts to the installation process:
- Start with a clean Enterprise/ITTC kickstart.
- Setup networking configuration to have the node on a network behind a router
that is connected to the ITTC network.
- Setup the Xen software.
- Setup the OpenStack software.

All instructions in this file will use <TOP_LEVEL> as the directory of the top
level of the installation script directory tree. To execute a scrip the current directory
must be the same as the directory the script is found.

## Kickstart the host for RHEL 6.5 ##

Ask the sysadmins for help doing this the first time.

Note: For RHEL the NIC in the plugin card must NOT be connected to the network
during the kickstart. If it is plugged into a network you will receive an
error during the kickstart process.

## Configure host for the Armored network. ##

Follow the instructions in ./networking/README.md

## Configure the host for Xen. ##

Follow the instructions in ./xen/README.md

## Configure the host for OpenStack ##

Follow the intstructions in ./openstack/README.md.

