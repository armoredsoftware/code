#!/bin/bash
# 

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

source_file ${UTIL_DIR}/network-fns
source_file ${UTIL_DIR}/params-network

# ---------------------------------------------------------------

echo "# Shutdown the networkmanager service."
# Make sure the NetworkManger is disabled.
which systemctl 2> /dev/null
if [ $? -ne 0 ] ; then
  service NetworkManager status
  if [ $? -eq 0 ] ; then
    chkconfig NetworkManager off
    service NetworkManager stop
  fi
else
  systemctl stop  NetworkManager.service
  systemctl disable NetworkManager.service
fi

# Puppet must be disabled to prevent it from over writing sudoers are other files.
echo "# Shutdown puppet."
which systemctl 2> /dev/null
if [ $? -ne 0 ] ; then
  service puppet status > /dev/null
  if [ $? -eq 0 ] ; then
    chkconfig puppet off
    service puppet stop
  fi
else
  systemctl stop puppetagent.service
  systemctl disable puppetagent.service
fi

setup_resolv ${CLOUD_EXT_ROUTER_IPADDR} ${CLOUD_EXT_DOMAIN}

