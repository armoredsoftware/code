#!/bin/bash
# Setup the networking for our compute host.
# Assume that we are starting with a fresh ITTC Fedorda 19 installation
# We assume that there are 2 NICs on the compute host. See the
# file 'params-network.sh' and set up the parameters appropriately.
# General setup:
# - Use parameters from the file './params-network'.
# - configure NIC for Openstack public network.

usage () 
{
  echo "Usage: sudo ./networking.sh <hostNum>"
  echo "  hostNum - number greater then or equal to 1."
}

# Is the host number argument set?
if [ $# -lt 1 ] ; then
  echo "Missing compute host number argument to ./networking.sh."
  usage
  exit 1
fi

# ?
if [ $# -gt 1 ] ; then
  echo "Too many arguments to ./networking.sh."
  usage
  exit 1
fi

# Lets assign the argument to a reasonably named variable to make the
# script easier to read.
HOSTNUM=$1

#--------------------------------------------

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

source_file ${UTIL_DIR}/network-fns
source_file ${UTIL_DIR}/params-network

# ---------------------------------------------------------------

# What is the new hostname.
COMPUTE_HOSTNAME=${CLOUD_EXT_COMPUTE_HOSTNAME_PREFIX}${HOSTNUM}
COMPUTE_EXT_IPADDR=${CLOUD_EXT_COMPUTE_IPADDR_PREFIX}.${HOSTNUM}
COMPUTE_DATA_IPADDR=${CLOUD_DATA_COMPUTE_IPADDR_PREFIX}.${HOSTNUM}

# Make sure that the network devices specified in the param-network.sh
# really exist.
setup_static_device_networking CLOUD_EXT_COMPUTE_DEVICE ${COMPUTE_EXT_IPADDR} ${CLOUD_EXT_NETMASK} ${CLOUD_EXT_ROUTER_IPADDR}

setup_static_device_networking CLOUD_DATA_COMPUTE_DEVICE ${COMPUTE_DATA_IPADDR} ${CLOUD_EXT_NETMASK}

# Puppet must be disabled to prevent it from over writing sudoers are other files.
echo "Shutdown puppet."
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
add_sudoer_host ${SUDO_USER} ${COMPUTE_HOSTNAME}

# Change the hostname to the compute node hostname
echo "# Changing host name to '${COMPUTE_HOSTNAME}.' "
hostnamectl --static set-hostname ${COMPUTE_HOSTNAME}
