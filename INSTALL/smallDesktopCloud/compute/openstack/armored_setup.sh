#!/bin/sh
#
# Install and setup the openstack computing packages.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd
source_file ${UTIL_DIR}/openstack-fns

# We have to be the root user.
check_root_user

# We need the tenant name and password to perform neutron changes.
source_file /root/keystonerc_admin

# Add the SSH and ICMP protocols to the firewall.
neutron security-group-rule-create --tenant-id admin --direction ingress --ethertype IPv4 --protocol tcp --port-range-min -1 --port-range-max -1 --remote-ip-prefix 0.0.0.0/0 
neutron security-group-rule-create --tenant-id admin --direction ingress --ethertype IPv4 --protocol tcp --port-range-min 22 --port-range-max 22 --remote-ip-prefix 0.0.0.0/0 
