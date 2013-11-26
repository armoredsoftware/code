#!/bin/bash
# Setup the initial networking for our openstack dedicated networking host.
# Assume that we are starting with a fresh ITTC Fedorda 19 installation
# and that we are currently connected to the ITTC network.
# We assume that there are 2 NICs on the control host. See the
# file 'params-network.sh' and set up the parameters appropriately.
# General setup:
# - Use parameters from the file '${TOP_LEVEL}/util/params-network'.

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/network-fns

# Setup static networking on the cloud external device.
if [ "${CLOUD_EXT_CONTROL_IPADDR}" != "${CLOUD_EXT_ROUTER_IPADDR}" ] ; then
  setup_static_device_networking CLOUD_EXT_CONTROL_DEVICE ${CLOUD_EXT_CONTROL_IPADDR} ${CLOUD_EXT_NETMASK} ${CLOUD_EXT_ROUTER_IPADDR}
  setup_resolv ${CLOUD_EXT_ROUTER_IPADDR} ${CLOUD_EXT_DOMAIN}
else
  # We don't set the gateway on the interface when the controler is running
  # on the router.
  setup_static_device_networking CLOUD_EXT_CONTROL_DEVICE ${CLOUD_EXT_CONTROL_IPADDR} ${CLOUD_EXT_NETMASK}
fi

# Setup the private network device.
setup_static_device_networking CLOUD_DATA_CONTROL_DEVICE ${CLOUD_DATA_CONTROL_IPADDR} ${CLOUD_EXT_NETMASK}

# We need to disable puppet agent so that it will not overwrite some of
# our files like /etc/resolv.conf and /etc/sudoers
systemctl stop puppetagent.service
systemctl disable puppetagent.service

# We want the user to be able to
# still run as sudo after we change the hostname so we need to add the new host
# name to the users' entry in the sudoers file.
if [ "${SUDO_USER}" == "" ] ; then
  echo "!!!! This file was not run using sudo so we don't know the user's name"
  echo "  for editing the sudoers file."
  exit 1
fi
add_sudoer_host ${SUDO_USER} ${CLOUD_EXT_CONTROL_HOSTNAME}

# We need to update the /etc/resolv.conf file.
setup_resolv ${CLOUD_EXT_ROUTER_IPADDR} ${CLOUD_EXT_DOMAIN}

# Change the hostname to the compute node hostname
if [ "${CLOUD_EXT_CONTROL_IPADDR}" != "${CLOUD_EXT_ROUTER_IPADDR}" ] ; then
  echo "# Changing host name to '${CLOUD_EXT_CONTROL_HOSTNAME}.' "
  hostnamectl --static set-hostname ${CLOUD_EXT_CONTROL_HOSTNAME}
fi
