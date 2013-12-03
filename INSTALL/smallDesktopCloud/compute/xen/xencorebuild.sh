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

# Puppet must be disabled to prevent it from over writing the passwd and group
# files since the mock user will be added and 
echo "# !!!! Shutdown puppet."
which systemctl > /dev/null
if [ $? -eq 0 ] ; then
  systemctl stop puppetagent.service
  systemctl disable puppetagent.service
else
  service puppet stop
  chkconfig puppet off
fi


yum install -y mock redhat-lsb-core || exit 1

# Mock will not run as super user.
echo "# Adding mock group to user '${SUDO_USER}'"
usermod -G mock ${SUDO_USER}

echo "# Creating directory /xenserver_core for the git clone."
if [ ! -f /xenserver-core-build ] ; then
  mkdir /xenserver-core-build
  chmod a+xwr /xenserver-core-build
fi

# Make sure git is installed.
yum -y install git

cd /xenserver-core-build
echo "# Doing git clone."
sudo -u ${SUDO_USER} git clone git://github.com/xenserver/xenserver-core.git || exit 1

cd ./xenserver-core

grep -e "RedHatEnterpriseServer" configure.sh > /dev/null
if [ $? -ne 0 ] ; then
echo "# Add RedHatEnterpriseServer to the rpm configuration."
  sed -i -e "/Fedora/s/]/-o `lsb_release -si` == \"RedHatEnterpriseServer\" ]/" configure.sh
fi

grep -e "red hat enterprise linux server" specdep.py > /dev/null
if [ $? -ne 0 ] ; then
echo "# Add \"red hat enterprise server\" to rpm distribution types."
  sed -i -e "/rhel_like =/s/]/, \"red hat enterprise linux server\"]/" specdep.py
fi

sudo -u ${SUDO_USER} ./configure.sh || exit 1
sudo -u ${SUDO_USER}  make  || exit 1

cd ..
sudo -u ${SUDO_USER} tar zcf xenserver-core-latest.tgz xenserver-core/RPMS xenserver-core/scripts xenserver-core/deps xenserver-core/Makefile  xenserver-core/SPECS xenserver-core/xapi.repo

echo "#############################################################"
echo "The INSTALLATION tarball is at $PWD/xenserver-core-built.tgz. "
echo "#############################################################"
