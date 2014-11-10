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

# For now we are using the RDO allinone.
# See if RDO is already installed.
rpm -q openstack-packstack
if [ "$?" != "0" ] ; then
  install_rdo
  echo "##############################################################"
  echo " We installed openstack-packstack. You need to reboot and then"
  echo " run this script again to finish initial install."
  echo "#############################################################"
  exit 0
fi

packstack --allinone

if [ "$?" != "0" ] ; then
    echo "!!!!! Packstack --allinone failed. Stopping".
    exit
fi
