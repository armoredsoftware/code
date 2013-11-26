#!/bin/bash

# Install openstack for the control node.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Make sure we are the root user.
check_root_user

# Setup mysql
./mysql.sh || exit 1

# Install the open stack YUM repository setup packages.
${UTIL_DIR}/openstack-pkgs.sh || exit 1

# Install the messaging service.
./messaging.sh || exit 1

# Install Keystone identity service.
./identityService.sh || exit 1

# Install Glance image service.
./imageService.sh || exit 1

# Install Nova compute control service.
./computeServices.sh || exit 1

./networkingService.sh || exit 1
