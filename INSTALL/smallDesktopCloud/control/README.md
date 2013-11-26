This directory contains the scripts for configuring the 'control' host for
OpenStack. To do the entire install do the following:

> sudo INSTALL.sh

It sets up the following services:

- id (keystone)


If you want to only do portions of the install then you can run the 
following scripts independtly using sudo:

network.sh - setup networking so that NAT, DNS and DHCP are configured. 
