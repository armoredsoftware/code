#!/bin/bash

# Install the xenserver core
# 
# For running without openstack: images, by default, are placed
# in /usr/share/xapi/images.
#

# We need 'mock' to do the install.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

BUILDDIR=/xenserver-core-build


# We have to be the root user.
check_root_user

# Puppet must be disabled to prevent it from over writing the passwd and group
# files since the mock user will be added and 
echo "# !!!! Shutdown puppet."
which systemctl 2> /dev/null
if [ $? -eq 0 ] ; then
  systemctl stop puppetagent.service
  systemctl disable puppetagent.service
else
  service puppet status
  if [ $? -eq 0 ] ; then
    service puppet stop
    chkconfig puppet off
  fi
fi

# We need the mock user and utility to do the install.
#yum install -y mock redhat-lsb-core || exit 1

# Mock will not run as super user.
#echo "# Adding mock group to user '${SUDO_USER}'"
#usermod -G mock ${SUDO_USER}

#echo "# Creating directory /xenserver_core for tar ball"
#if [ ! -f ${BUILDDIR} ] ; then
#  mkdir ${BUILDDIR}
#  chmod a+xwr ${BUILDDIR}
#fi

# We need to get the rpm tarball from somewhere.
#if [ $# -eq 1 ] ; then
#  # Remove the old dir
#  if [ -f ${BUILDDIR}/xenserver-core ] ; then
#    rm -rf ${BUILDDIR}/xenserver-core
#  fi
#  # Unpack the tarball.
#  tar -C ${BUILDDIR} -zxf $1 
#else
#  if [ -f ${BUILDDIR}/xenserver-core-latest.tgz ] ; then
#    if [ -f ${BUILDDIR}/xenserver-core ] ; then
#      rm -rf ${BUILDDIR}/xenserver-core
#    fi
#    cd ${BUILDDIR}
#    tar -zxf ${BUILDDIR}/xenserver-core-latest.tgz
#  else
#    echo "!!!! Could not find xenserver-core RPM tarball."
#    echo "Give it as the first argument to this script."
#    exit 1
#  fi
#fi


#cd ${BUILDDIR}/xenserver-core
#. scripts/rpm/install.sh

echo "Get the repo config files."

cd /tmp
wget http://router.ext.armored/armoredrepo/CentOS6.5/xen-c6-tweaked.repo
wget http://router.ext.armored/armoredrepo/CentOS6.5/epel.repo
wget http://router.ext.armored/armoredrepo/CentOS6.5/ArmoredConfig.repo
wget http://router.ext.armored/armoredrepo/CentOS6.5/RPM-GPG-KEY-EPEL-6

install -m 0644 xen-c6-tweaked.repo /etc/yum.repos.d/xen-c6-tweaked.repo
install -m 0644 epel.repo /etc/yum.repos.d/epel.repo
install -m 0644 ArmoredConfig.repo /etc/yum.repos.d/ArmoredConfig.repo

install -m 0644 RPM-GPG-KEY-EPEL-6 /etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-6


yum install -y xenserver-core

echo "#Setting up ld.so for libvhd.so.0"
touch /etc/ld.so.conf.d/blktap-x86_64.conf
echo "/usr/lib64/blktap/lib" >> /etc/ld.so.conf.d/blktap-x86_64.conf
ldconfig

echo "###############################################################"
echo "The xen hypervisor and kernal were installed. We need to reboot"
echo "to get them running."
echo "    > sudo reboot"
echo "###############################################################"
