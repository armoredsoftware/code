This directory contians the script and files needed to configure the router host to be a DNS/DHCP server and a gateway to the ITTC network.

To add a new compute host to DNS and DHCP you must add it to both
./hosts-armored and ./hosts-armored.d then run the networking.sh script.

To do the setup run the ./networking.sh script using the sudo command.

Files:
params-network - 
     Shell variable configuration file loaded by networking.sh
     and some other shell scripts. This file defines variables
     concerning the networking of the cloud networking/control host.
     You only need to change
     the contents of this file if you want to change the networking/control
     host name, domain name, or IP address.
     Or you change the NIC devices
     (maybe you change computer hardware so the devicenames change).
  
hosts-armored - 
     Additional hosts file used by dnsmasq. The strings 'CLOUD_PUBLIC_IPADDR',
     'CLOUD_PUBLIC_HOSTNAME', 'CLOUD_PUBLIC_DOMAIN' and 
     'COMPUTE_HOSTNAME_PREFIX' are replace with the values from the file
     ./params-network.

armored-dhcp-hosts.d - 
     Configuration file for dnsmasq. Contains the hosts DHCP is to expect.
     The string 'COMPUTE_HOSTNAME_PREFIX' will be replaced by the value found
     in the file ./params-network'.

armored_dnsmasq.d -
     Configuration file to setup the dnsmasq service. Some STRINGS in the file
     are replace withed the values from ./params-network.

armored_network.conf
     Configuration file for sysctl. Just enables forwarding.
