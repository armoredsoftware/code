Introduction
============

This directory contians the scripts and files needed to configure the 'router' host to be a DNS server for the cloud external/mangement network and a gateway to the ITTC network.

The router sets up NAT to allow the cloud nodes to access the outside world. It also sets up a local DNS so that nodes within the cloud may refer to each other by host name instead of IP address. Also DNS lookup uses whatever the 'router' original /etc/resolv.conf was setup to use.

Hardware Setup
==============

The ArmoredSoftware cloud hardware configuation is composed of a 'router' node
that is used as a network router between the cloud and an enterprise network. The other nodes of the ArmoredSoftware cloud (control, networking, compute) are separted from the enterprise by the router.

It is permitted to have the 'router' host the ArmoredSoftware 'control' node.

The 'router must have 3 Network Interface ports. 
1) The port to connect to the enterprise network.
2) The port to connect to the cloud External Network (this is also used as the Management Network).
3) The port to connect to the cloud Private Network.

Two network switches are required
1) External/Managment Nework.
2) Private Network.

At this point you should at least connect the Enterprise NIC to an Enterprise switch.

Software Setup
==============

Edit the file ../util/params-network and set the following variables to
their appropriate values:
ITTC_NET_DEVICE
CLOUD_EXT_ROUTER_DEVICE
CLOUD_EXT_ROUTER_IPADDR
CLOUD_NUM_COMPUTE_NODES

You may also edit the following variables if you want their values
to differ from the default installation:
CLOUD_EXT_DOMAIN
CLOUD_EXT_ROUTER_HOSTNAME

To do the setup run the ./networking.sh script using the sudo command.
    > sudo ./networking.sh

NOTE: Do not try to run this script when your PWD is a different directory then the one networkgin.sh is in.

./armored_dnsmasq.d -
     Configuration file to setup the dnsmasq service. Some STRINGS in the file
     are replace withed the values from ${TOP_LEVEL}/util/params-network.

./armored_network.conf
     Configuration file for sysctl. Just enables forwarding.

To add a new compute host to DNS you must add it to
./hosts-armored and then run the networking.sh script.


Notes
=====

PEER_DNS is turned off for the Enterprise network interface so if the DNS servers change the /etc/resolv.conf will have to be changed by hand.
