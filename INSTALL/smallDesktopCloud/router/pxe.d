# We need a range addresses that can be used for PXE booting.
dhcp-range=10.100.0.200,10.100.0.250,24H

# The first value is the location of the file in the tftp directory.
dhcp-boot=pxelinux.0,CLOUD_EXT_ROUTER_HOSTNAME.CLOUD_EXT_DOMAIN,CLOUD_EXT_ROUTER_IPADDR
log-queries
log-dhcp

