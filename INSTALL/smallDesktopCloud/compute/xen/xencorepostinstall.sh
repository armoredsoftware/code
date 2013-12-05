#!/bin/bash
#
# Finish the XAPI setup after installation and reboot has been done.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

BUILDDIR=/xenserver-core-build

# We have to be the root user.
check_root_user

# run the install wizard.
xenserver-install-wizard --yes-to-all


