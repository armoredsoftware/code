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

# We need to limit the amount of memory used by dom0 so that there
# is memory available for domU's.
grep -e "dom0_mem=" /boot/grub/grub.conf > /dev/null
if [ $? -ne 0 ] ; then
  sed -i -e "/kernel \/xen.gz/s/xen.gz/xen.gz dom0_mem=3072M,max:3072M/" /boot/grub/grub.conf
else
  sed -i -e "/dom0_mem=/s/dom0_mem=[^ ]*/dom0_mem=3072M,max:3072M/" /boot/grub/grub.conf
fi

echo "################################################################"
echo "In order to get the xen hypervisor running and dom0 running, reboot."
echo "> sudo reboot"
echo "###############################################################"