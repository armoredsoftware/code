#!/bin/bash

# install the xenserver core

# We need mock to do the install.

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

# We need the mock user and utility to do the install.
yum install -y mock redhat-lsb-core || exit 1

# Mock will not run as super user.
echo "# Adding mock group to user '${SUDO_USER}'"
usermod -G mock ${SUDO_USER}

# We need to get the rpm tarball from somewhere.

cd /xenserver-core-build
cd ./xenserver-core
make install

echo "#Setting up ld.so for libvhd.so.0"
touch /etc/ld.so.conf.d/blktap-x86_64.conf
echo "/usr/lib64/blktap/lib" >> /etc/ld.so.conf.d/blktap-x86_64.conf
ldconfig


# xenserver-install-wizard --yes-to-all