#!/bin/bash
#
# Build (not install) the xen core which should include the xen api. This should
# only have to be done once. A tarball is produced which is then
# used for all of the compute hosts.
#
# This script is based on information found 
# at https://github.com/xenserver/xenserver-core

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# We have to be the root user.
check_root_user

SHELLDIR=`pwd`
XENSERVER_SPEC_PATCHFILE=${SHELLDIR}/xenserver-install-wizard_SPECfix.patch
BUILDDIR=/xenserver-core-build


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


yum install -y mock redhat-lsb-core || exit 1

# Mock will not run as super user.
echo "# Adding mock group to user '${SUDO_USER}'"
usermod -G mock ${SUDO_USER}


echo "# Creating directory /xenserver_core for the git clone."
if [ ! -f ${BUILDDIR} ] ; then
  mkdir ${BUILDDIR}
  chmod a+xwr ${BUILDDIR}
fi

# Make sure git is installed.
yum -y install git


cd ${BUILDDIR}
echo "# Doing git clone."
sudo -u ${SUDO_USER} git clone git://github.com/xenserver/xenserver-core.git 


cd ${BUILDDIR}/xenserver-core

echo "# Modify the xenserver-install-wizard SPEC file to use our patch."
cd ${BUILDDIR}/xenserver-core/SPECS
patch -p1 xenserver-install-wizard.spec ${SHELLDIR}/xenserver-install-wizard-0.2.28.spec.patch
echo "# Apply the patch for the xenserver-install-wizard"
cp ${SHELLDIR}/xenserver-install-wizard-0.2.28.patch ${BUILDDIR}/xenserver-core/SOURCES
cd ${BUILDDIR}/xenserver-core


sudo -u ${SUDO_USER} ./configure.sh || exit 1

sudo -u ${SUDO_USER}  make  || exit 1

cd ..
XEN_TARBALL=xenserver-core-latest.tgz
sudo -u ${SUDO_USER} tar zcf ${XEN_TARBALL} xenserver-core/RPMS xenserver-core/scripts xenserver-core/deps xenserver-core/Makefile  xenserver-core/SPECS xenserver-core/xapi.repo

echo "#############################################################"
echo "The INSTALLATION tarball is at ${PWD}/${XEN_TARBALL}. "
echo "Move it to a location that you an retrieve it from for furture use."
echo "#############################################################"
