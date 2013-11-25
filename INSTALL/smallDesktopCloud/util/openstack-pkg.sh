#!/bin/bash

# This file sets up the yum repository for obgaining the openstack
# RPMs.

# Load some common functions.
# Get the generic functions.
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
. ${UTIL_DIR}/fns

# Are we the root user?
check_root_user

yum -y install http://repos.fedorapeople.org/repos/openstack/openstack-havana/rdo-release-havana-6.noarch.rpm
yum -y install openstack-utils
