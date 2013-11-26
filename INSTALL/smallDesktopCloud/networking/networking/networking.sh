#!/bin/bash
# Setup the initial networking for our openstack network host.
# Assume that we are starting with a fresh ITTC Fedorda 19 installation
# and that we are currently connected to the ITTC network.
# We assume that there are 2 NICs on the control host. See the
# file 'params-network.sh' and set up the parameters appropriately.
# General setup:
# - Use parameters from the file './params-network'.

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

source_file ${UTIL_DIR}/network-fns
source_file ${UTIL_DIR}/params-network


#network_device_must_exist CLOUD_MGNT_CONTROL_DEVICE

setup_static_device_networking CLOUD_EXT_NETWORKING_DEVICE ${CLOUD_EXT_NETWORKING_IPADDR} ${CLOUD_EXT_NETMASK} ${CLOUD_EXT_ROUTER_IPADDR}

setup_static_device_networking CLOUD_DATA_NETWORKING_DEVICE ${CLOUD_DATA_NETWORKING_IPADDR} ${CLOUD_EXT_NETMASK}

# We need to disable puppet agent so that it will not overright some of
# our files like /etc/resolv.conf and /etc/sudoers
systemctl stop puppetagent.service
systemctl disable puppetagent.service

setup_resolv ${CLOUD_EXT_ROUTER_IPADDR} ${CLOUD_EXT_DOMAIN}

# We want the user to be able to
# still run as sudo after we change the hostname so we need to add the new host
# name to the users' entry in the sudoers file.
if [ "${SUDO_USER}" == "" ] ; then
  echo "!!!! This file was not run using sudo so we don't know the user's name"
  echo "  for editing the sudoers file."
  exit 1
fi
add_sudoer_host ${SUDO_USER} ${CLOUD_EXT_NETWORKING_HOSTNAME}

# Change the hostname to the compute node hostname
echo "# Changing host name to '${CLOUD_EXT_NETWORKING_HOSTNAME}.' "
hostnamectl --static set-hostname ${CLOUD_EXT_NETWORKING_HOSTNAME}
