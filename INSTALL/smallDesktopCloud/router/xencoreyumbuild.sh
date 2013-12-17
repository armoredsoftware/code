#!/bin/bash

# Build the yum repository for installing the xenserver-core.
# This script should be run on the 'router' host. 
# It requireds the xenserver-core-latest.tgz tarball that is built
# by ../compute/xen/xencorebuild.sh

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

BUILDDIR=/xenserver-core-build
REPO_DIR=./www_html/armoredrepo
REPO_CENTOS65=${REPO_DIR}/CentOS6.5



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


echo "# Creating directory /xenserver_core for tar ball"
if [ ! -f ${BUILDDIR} ] ; then
  mkdir ${BUILDDIR}
  chmod a+xwr ${BUILDDIR}
fi

# We need to get the rpm tarball from somewhere.
echo "# Untar the xen server core tarball."
if [ $# -eq 1 ] ; then
  # Remove the old dir
  if [ -f ${BUILDDIR}/xenserver-core ] ; then
    rm -rf ${BUILDDIR}/xenserver-core
  fi
  # Unpack the tarball.
  tar -C ${BUILDDIR} -zxf $1 
else
  if [ -f ${BUILDDIR}/xenserver-core-latest.tgz ] ; then
    if [ -f ${BUILDDIR}/xenserver-core ] ; then
      rm -rf ${BUILDDIR}/xenserver-core
    fi
    cd ${BUILDDIR}
    tar -zxf ${BUILDDIR}/xenserver-core-latest.tgz
  else
    echo "!!!! Could not find xenserver-core RPM tarball."
    echo "Give it as the first argument to this script."
    exit 1
  fi
fi

echo "# setup the xen server core yum repo."
cd ${BUILDDIR}/xenserver-core/scripts/rpm/
cp xen-c6-tweaked.repo ${REPO_CENTOS65}/xen-c6-tweaked.repo
cp epel.repo ${REPO_CENTOS65}/epel.repo
cp RPM-GPG-KEY-EPEL-6 ${REPO_CENTOS65}/RPM-GPG-KEY-EPEL-6

# Note that there is an ArmoredConfig.repo in git that is 
# manually created.

cd ${BUILDDIR}/xenserver-core/RPMS/
cp -r . ${REPO_CENTOS65}
# Get rid of some files that we done need.
cd ${REPO_CENTOS65} 
find . -name \*.src.rpm -exec rm {} ;
find . -name \*.log -exec rm {} ;

echo "#############################################################"
echo "Files in the directory '${${REPO_CENTOS65}' have been updated."
echo "Make sure that they are pushed to the git repository."
echo "#############################################################"



