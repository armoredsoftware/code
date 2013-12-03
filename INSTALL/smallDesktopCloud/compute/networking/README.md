Armored Software Compute Node NIC Setup
=======================================

This directory contains the scripts for configuring individual 'compute' node
for running on a network connected to a router that is then connected to the
Enterprise network.

Each compute host must have its own unique number starting from 1. The current
scripts do not support a host number higher than 250. Do not use a number
higher than is set in the code/INSTALL/smallDesktopCloud/util/params-network
variable CLOUD_NUM_COMPUTE_NODES unless you have manually modified the
/etc/hosts-armored file to include the IP address for your chosen host number.

Setup
=====

The setup changes the network devices (CLOUD_EXT_COMPUTE_DEVICE and
CLOUD_DATA_COMPUTE_DEVICE) to static IP (from dhcp). The static IP address
is CLOUD_EXT_COMPUTE_IPADDR_PREFIX.<num> and CLOUD_DATA_COMPUTE_IPADDR_PREFIX.<num>

The setup also changes hostname to CLOUD_EXT_COMPUTE_HOSTNAME_PREFIX<num>

Then network device configuration does not take effect until the next reboot.


To setup networking:
- Ensure that the System Administraitors have given you ALL sudo permissions for the host computer.
- Connect the host computer to the Enterprise network and boot.
- Log into the host. You may want to use SSH for this.
- If you have not done so already clone the ArmoredSoftware code repository in 
  your home directory.
    > cd ~
    > mkdir armored
    > cd armored
    > git clone git@github.com:armoredsoftware/code.git
    > cd code/INSTALL/smallDesktopCloud/compute
- If nessary edit the file code/INSTALL/smallDesktopCloud/util/params-network and
modify the following variables to suite your situtation:
 - CLOUD_EXT_COMPUTE_DEVICE
 - CLOUD_DATA_COMPUTE_DEVICE
- Do the following command:
    > sudo ./networking.sh <num>
where <num> is the compute host number for this compute host number greater
than 1 but less than 250. No checks are performed to make sure that the
number has not already been used. 
- If there are no errors in the network setup then:
    > sudo shutdown now
- Connect the two host ports to the two cloud switchs. Typically the motherboard 
NIC is connected to the External/Management network switch. The PCI network card is connect to the Private/Data network switch.
- Boot the new computer host.
- Proceed to the Xen setup.
    cd ../xen
- Follow the instructions in README.md.



