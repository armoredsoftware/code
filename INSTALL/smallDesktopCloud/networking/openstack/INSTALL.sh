#!/bin/bash
# Install the neutron networking services on a dedicated node.
#

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

${UTIL_DIR}/openstack-pkg.sh || exit 1

./networkingServices.sh || exit 1
