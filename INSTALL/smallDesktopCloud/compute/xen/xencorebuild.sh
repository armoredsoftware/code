#!/bin/bash
#
# Build (not install) the xen core which should include the xen api. This should
# only have to be done once. A tarball is produced which is then
# used for all of the compute hosts. See ../../router/README.md for
# what to do with the tarball.
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

yum install -y redhat-lsb-core || exit 1

# For CentOS we need the ELEL repositories.
if [ "$(lsb_release -is)" == "CentOS" ] ; then
  OLDPWD=`pwd`
  cd /tmp
  wget http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
  rpm -Uvh epel-release-6*.rpm

  #wget http://rpms.famillecollet.com/enterprise/remi-release-6.rpm
  #rpm -Uvh remi-release-6*.rpm 

  cd ${OLDPWD}
fi

yum install -y createrepo || exit 1
yum install -y mock || exit 1

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
sudo -u ${SUDO_USER} git clone https://github.com/xenserver/xenserver-core.git


cd ${BUILDDIR}/xenserver-core

echo "# Modify the xenserver-install-wizard SPEC file to use our patch."
cd ${BUILDDIR}/xenserver-core/SPECS
sudo -u ${SUDO_USER} patch -p1 xenserver-install-wizard.spec ${SHELLDIR}/xenserver-install-wizard-0.2.28.spec.patch
echo "# Apply the patch for the xenserver-install-wizard"
sudo -u ${SUDO_USER} cp ${SHELLDIR}/xenserver-install-wizard-0.2.28.patch ${BUILDDIR}/xenserver-core/SOURCES
sudo -u ${SUDO_USER} cp ${SHELLDIR}/xenserver-install-wizard-0.2.28.patch2 ${BUILDDIR}/xenserver-core/SOURCES
cd ${BUILDDIR}/xenserver-core


sudo -u ${SUDO_USER} ./configure.sh || exit 1

# In order for mock to work the version in /usr/bin must be found before the
# one in /usr/sbin
export PATH=/usr/bin:${PATH}
sudo -u ${SUDO_USER} PATH=${PATH} make  || exit 1

cd ..
XEN_TARBALL=xenserver-core-latest.tgz
sudo -u ${SUDO_USER} tar zcf ${XEN_TARBALL} xenserver-core/RPMS xenserver-core/scripts xenserver-core/deps xenserver-core/Makefile  xenserver-core/SPECS xenserver-core/xapi.repo xenserver-core/rpms/*.repo

echo "#############################################################"
echo "The INSTALLATION tarball is at ${PWD}/${XEN_TARBALL}. "
echo "Move it to a location that you an retrieve it from for furture use."
echo "#############################################################"
