#!/bin/bash
#
# Setup a PXE server on the router.
# This script is derived from the information found at
# http://www.justnudge.com/2013/02/setting-up-a-pxe-boot-server-with-centos-and-dnsmasq/
# 

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

# Get the network parameters.
. ${UTIL_DIR}/params-network

# Are we the root user?
check_root_user

# Remember our current directory.
SHELLDIR=`pwd`

echo "# Setup the PXE server."

yum install syslinux tftp tftp-server -y || exit 1


SYSLINUX_DIR=/usr/share/syslinux
TFTPBOOT_DIR=/var/lib/tftpboot

CENTOS65_X8664_IMAGES=${TFTPBOOT_DIR}/images/CentOS6.5/x86_64

cp ${SYSLINUX_DIR}/menu.c32 ${TFTPBOOT_DIR}
cp ${SYSLINUX_DIR}/pxelinux.0 ${TFTPBOOT_DIR}
mkdir -p ${CENTOS65_X8664_IMAGES}

cd ${CENTOS65_X8664_IMAGES}

WGETFILE=http://kickstart.ittc.ku.edu/mirror/redhat/el/6.5/server/os/x86_64/images/pxeboot/initrd.img
wget -v ${WGETFILE} || exit 1

WGETFILE=http://kickstart.ittc.ku.edu/mirror/redhat/el/6.5/server/os/x86_64/images/pxeboot/vmlinuz
wget ${WGETFILE} || exit 1

echo "second wget"

mkdir -p ${TFTPBOOT_DIR}/pxelinux.cfg

cat > ${TFTPBOOT_DIR}/pxelinux.cfg/default <<EOF
timeout 100
default menu.c32
 
menu title ########## Armored Boot Menu ##########
label 1
   menu label ^1) Install CentOS 6.5
   kernel ${CENTOS65_X8664_IMAGES}/vmlinuz
   append initrd=${CENTOS65_X8664_IMAGES}/initrd.img devfs=nomount ks=http://${CLOUD_EXT_ROUTER_HOSTNAME}.${CLOUD_EXT_DOMAIN}/centos65.ks
 
label 2
   menu label ^2) Boot from local drive
   localboot
EOF

echo "# Enablethe TFTP service"
sed -i -e "/disable/s/=.*/= no/" /etc/xinetd.d/tftp

cd ${SHELLDIR}
echo "# Put the PXE values need for dnsmasq in its config directory."
sed -e "s/CLOUD_EXT_ROUTER_HOSTNAME/$CLOUD_EXT_ROUTER_HOSTNAME}/" -e "s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/" -e "s/CLOUD_EXT_ROUTER_IPADDR/$CLOUD_EXT_ROUTER_IPADDR}/" ./pxe.d > /etc/dnsmasq.d/pxe.d 

echo "# Setup the http service."

yum -y install httpd || exit 1 

systemctl enable xinetd.service
systemctl restart xinetd.service

systemctl enable httpd.service
systemctl restart httpd.service

echo "Put web files at /var/www/html"

