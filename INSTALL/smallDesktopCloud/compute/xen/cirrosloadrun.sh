#!/bin/bash
#
# Load and run cirros in xen.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

CIRROS_VERSION=0.3.0


# We have to be the root user.
check_root_user

SHELL_START_DIR=`pwd`

# Do we have a directory for the image.
if [ ! -f /xenImages ] ; then
  mkdir /xenImages
fi

cd /xenImages
# Down load the raw disk image.
if ! -f cirros-${CIRROS_VERSION}-x86_64-rootfs.img ] ; then
  wget https://launchpad.net/cirros/trunk/${CIRROS_VERSION}/+download/cirros-${CIRROS_VERSION}-x86_64-rootfs.img.gz || exit 1
  gunzip cirros-${CIRROS_VERSION}-x86_64-rootfs.img.gz
fi

# 
cd ${SHELL_START_DIR}
xl create cirros-${CIRROS_VERSION}.x86_64.raw.pvlinux
echo "###################################################"
echo " To connect to the cirros console do the following"
echo "> sudo xl list"
echo "Look for the cirros domU and note its <ID>"
echo "> sudo xl console <ID>"
echo "##################################################"

