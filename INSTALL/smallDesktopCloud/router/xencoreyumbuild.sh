#!/bin/bash

# Build the yum repository for installing the xenserver-core.
# This script should be run on the 'router' host. 
# It requireds the xenserver-core-latest.tgz tarball that is built
# by ../compute/xen/xencorebuild.sh

TOP_LEVEL=${TOP_LEVEL:-..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

REPO_DIR=${PWD}/www_html/armoredrepo
REPO_CENTOS65=${REPO_DIR}/CentOS6.5

if [ "${USERNAME}" == "root" ] ; then 
  echo "!!!! Do not run this script as user 'root'."
  exit 1;
fi


echo "# Creating directory for tar ball"
BUILDDIR=`mktemp -d`


# We need to get the rpm tarball from somewhere.
echo "# Untar the xen server core tarball."
if [ $# -eq 1 ] ; then
  OLDIDR=`pwd`
  cd ${BUILDDIR}
  tar  -zxf $1 
  cd ${OLDDIR}
else
  echo "!!!! Could not find xenserver-core RPM tarball."
  echo "Give it as the first argument to this script."
  exit 1
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
# Get rid of some files that we don't need.
cd ${REPO_CENTOS65} 
rm -rf repodata
find . -name available_pkgs -exec rm {} \;
find . -name installed_pkgs -exec rm {} \;
find . -name \*.src.rpm -exec rm {} \;
find . -name \*.log -exec rm {} \;

echo "#############################################################"
echo "Files in the directory '${REPO_CENTOS65}' have been updated."
echo "Make sure that they are pushed to the git repository."
echo "#############################################################"



