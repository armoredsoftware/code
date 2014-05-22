#!/bin/bash
#
# Install a xenserver-core xen kernel since
# RHEL no longer provides xen rpms (since RHEL 6).

# This install script is based on information found at:
# https://github.com/xenserver/linux-3.x.pg/blob/master/master/INSTALL

#TOP_LEVEL=${TOP_LEVEL:-../..}
#UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
#{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns."# ; exit 1; }

# We have to be the root user.
# check_root_user

GITREPOPARENT=/tmp


echo "Get the yum packages needed to do the patch and build."
sudo yum -y install git guilt module-init-tool patch bash sh-utils tar bzip2 findutils gzip m4 perl make gcc binutils redhat-rpm-config || exit 1

# The following two git lines are need for guilt to work correctly
git config --global user.name "$USER"
git config --global user.email "$USER@ittc.ku.edu"


cd ${GITREPOPARENT}

echo "Get the originel kernel source."
git clone https://github.com/xenserver/linux-3.x.git || exit 1

echo "Get the xenserver-core patches to the kernel."
git clone https://github.com/xenserver/linux-3.x.pg.git linux-3.x/.git/patches || exit 1

echo "Need an empty guilt status file."
if [ -f linux-3.x/.git/patches/master/status ] ; then
  rm linux-3.x/.git/patches/master/status || exit 1
fi
touch linux-3.x/.git/patches/master/status


echo "Apply the kernel patches from xenserver-core."
cd linux-3.x
guilt push -a || exit 1

KERNEL_VERSION=3.10.27
LOCAL_VERSION=-ittc-1
KABI_VERSION=$(make kernelversion)

echo "KABI version=${KABI_VERSION}"

echo "Archive the patched kernel source."
git archive --format=tar --prefix=linux-${KERNEL_VERSION}/ HEAD | bzip2 > linux-${KERNEL_VERSION}-01.tar.bz2 || exit 1

echo "Put the version number into the kernel RPM spec file."
sed -e "s/@REPO_VERSION@/${KERNEL_VERSION}/g" \
        -e "s/@LINUX_VERSION@/${KERNEL_VERSION}-01/g" \
        -e "s/@XS_RELEASE@/1/g" \
        -e "s/@LINUX_KABI_VERSION@/${KABI_VERSION}${LOCAL_VERSION}/g" \
        -e "s/@PKG_VERSION@/${KERNEL_VERSION}/g" \
        -e "s/@PKG_RELEASE@/01/g" \
        -e "s/@COMPANY_NAME@/Citrix/g" \
        < mk/kernel.spec.in > kernel.spec || exit 1

echo "Copy the tarball specfile and config to RPM build location."
BUILDROOT=~/rpmbuild

mkdir -p ${BUILDROOT}/SOURCES
mkdir -p ${BUILDROOT}/SPECS
cp linux-${KERNEL_VERSION}-01.tar.bz2 ${BUILDROOT}/SOURCES
#cp buildconfigs/linux-defconfig_xen_x86_64 \
#        ${BUILDROOT}/SOURCES/kernel-x86_64.config
sed -e "s/^CONFIG_LOCALVERSION=.*/CONFIG_LOCALVERSION=\"${LOCAL_VERSION}\"/" \
          < buildconfigs/linux-defconfig_xen_x86_64 \
          > ${BUILDROOT}/SOURCES/kernel-x86_64.config || exit 1
cp kernel.spec ${BUILDROOT}/SPECS/kernel.spec

echo "Build the kernel RPMS"
cd ${BUILDROOT}/SPECS
rpmbuild -ba kernel.spec --target x86_64

echo "#################################################################"
echo "kernel RPM is in ${BUILDROOT}/RPMS/x86_64"
echo "#################################################################"