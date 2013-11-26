#!/bin/bash

# Install the openstack messaging service.
# Get some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

echo "# Setup the Messaging Service..."

# load the messaging service packages.
yum -y install qpid-cpp-server memcached || exit 1

# See if the auth= line is present in the file
grep "auth=" /etc/qpid/qpidd.conf > /dev/null
if [ "$?" == "0" ] ; then
  # is present, set its value to no.
  sed -i -e "/auth=/s/=.*$/=no/" /etc/qpid/qpidd.conf
else
  # append the auth=no the file.
  sed -i -e "$ a\
auth=no" /etc/qpid/qpidd.conf
fi

echo "# Starting QPIDD service."
systemctl restart qpidd.service
systemctl enable qpidd.service
