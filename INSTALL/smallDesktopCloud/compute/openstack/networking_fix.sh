#!/bin/sh
#
# The packstack --allinone (juno) does not correctly setup
# the network devices.
# So make the correct with this script.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd
source_file ${UTIL_DIR}/openstack-fns

# We have to be the root user.
check_root_user

# See if RDO is installed.
rpm -q openstack-packstack
if [ "$?" != "0" ] ; then
  echo "##############################################################"
  echo " You need to install rdo packstack."
  echo " run sudo ./install.sh"
  echo "#############################################################"
  exit 1
fi


#if [ "$?" != "0" ] ; then
#    echo "!!!!! Packstack --allinone failed. Stopping".
#    exit
#fi

# We need the tenatn name and password to perform neutron changes.
source_file /root/keystonerc_admin

remove_rdo_allinone_network_devices

fix_rdo_allinone_network_devices ${CLOUD_EXT_COMPUTE_DEVICE} yes br-ex
