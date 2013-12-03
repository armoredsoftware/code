#!/bin/bash
#
# Install a third party xen hypervisor and xen kernel (for dom0) since
# RHEL no longer provides xen rpms (since RHEL 6).

# This install script is based on information found at:
# http://xen.crc.id.au/support/guides/install/

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# We have to be the root user.
check_root_user

# Make sure the NetworkManger is disabled.
which systemctl
if [ $? -ne 0 ] ; then
  chkconfig NetworkManager off
  service NetworkManager stop
else
  systemctl stop  NetworkManager.service
  systemctl disable NetworkManager.service
fi

# The kernel-xen-release-6-6.noarch depends on yum-plugin-fastestmerror
echo "# Get the yum-fastestmirror-plugin"
YUM_FAST_MIRROR_FILE=yum-plugin-fastestmirror-1.1.30-14.el6.noarch.rpm
OLDDIR=`pwd`
cd /tmp
wget ftp://fr2.rpmfind.net/linux/centos/6.4/os/x86_64/Packages/${YUM_FAST_MIRROR_FILE} || exit 1
yum -y install ${YUM_FAST_MIRROR_FILE}
cd ${OLDDIR}


echo "# Install bridge-utils."
yum -y install bridge-utils || exit 1

echo "# Install third party  Xen Repo."
yum -y install http://au1.mirror.crc.id.au/repo/el6/x86_64/kernel-xen-release-6-5.noarch.rpm || exit 1


echo "# Install xen hypervisor."
yum -y install xen || exit 1

echo "# Install xen virtual kernel."
yum -y install kernel-xen || exit 1


