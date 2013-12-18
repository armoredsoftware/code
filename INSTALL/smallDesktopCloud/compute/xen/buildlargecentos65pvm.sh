#!/bin/bash
# Build a Paravirtual disk image.
# The images size is 6GB.
# It will be in the raw format.
# It's location will be directory /xenImages.
# Swap size is 1GB.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# Sizes are in Megabytes.
ROOT_SIZE=6144
SWAP_SIZE=1024

PVH_NAME=CentOS_65_server_PVH
IMAGES_DIR=/xenImages

PVH_IMAGE_FILE=${IMAGES_DIR}/${PVH_NAME}.img


# We have to be the root user.
check_root_user

if [ ! -f ${IMAGES_DIR} ] ; then
  mkdir ${IMAGES_DIR}
fi

SCRIPT_DIR=`pwd`

echo "# Create the root disk file in directory ${IMAGES_DIR}."

# Create a raw disk image kickstart the system into.
qemu-img create -f raw ${PVH_IMAGE_FILE} ${ROOT_SIZE}M

cp centos65x86_64_install.cfg ${IMAGES_DIR}
sed -i -e "s/@IMAGES_DIR@/${IMAGES_DIR}/g" ${IMAGES_DIR}/centos65x86_64_install.cfg
sed -i -e "s/@PVH_IMAGE_FILE@/${PVH_IMAGE_FILE}/g" ${IMAGES_DIR}/centos65x86_64_install.cfg

# Get the vmLinux and initrd files for kickstart booting.
cd ${IMAGES_DIR}
wget http://kickstart.ittc.ku.edu/mirror/centos/6.5/os/x86_64/isolinux/vmlinuz
wget http://kickstart.ittc.ku.edu/mirror/centos/6.5/os/x86_64/isolinux/initrd.img

