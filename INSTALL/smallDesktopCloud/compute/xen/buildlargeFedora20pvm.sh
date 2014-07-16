#!/bin/bash
# Build a Paravirtual disk image.
# The images size is 6GB.
# It will be in the raw format.
# It's location will be directory /xenImages.

TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# Sizes are in Megabytes.
ROOT_SIZE=6144

PVH_NAME=Fedora_20_server_PVH
IMAGES_DIR=/xenImages
CONFIG_FILE=fedora20x86_64_install.cfg
RUN_CONFIG_FILE=fedora20x86_64_run.cfg

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

cp ${CONFIG_FILE} ${IMAGES_DIR}
sed -i -e "s|@IMAGES_DIR@|${IMAGES_DIR}|g" ${IMAGES_DIR}/${CONFIG_FILE}
sed -i -e "s|@PVH_IMAGE_FILE@|${PVH_IMAGE_FILE}|g" ${IMAGES_DIR}/${CONFIG_FILE}

cp ${RUN_CONFIG_FILE} ${IMAGES_DIR}
sed -i -e "s|@PVH_IMAGE_FILE@|${PVH_IMAGE_FILE}|g" ${IMAGES_DIR}/${RUN_CONFIG_FILE}


# Get the vmLinux and initrd files for kickstart booting.
cd ${IMAGES_DIR}
wget http://kickstart.ittc.ku.edu/mirror/fedora/linux/releases/20/Fedora/x86_64/os/isolinux/vmlinuz
wget http://kickstart.ittc.ku.edu/mirror/fedora/linux/releases/20/Fedora/x86_64/os/isolinux/initrd.img

mv vmlinuz fedora20x86_64_vmlinuz
mv initrd.img fedora20x86_64_initrd.img

echo "# Start the kickstart of the empty disk image."
xl create ${IMAGES_DIR}/${CONFIG_FILE} -c
