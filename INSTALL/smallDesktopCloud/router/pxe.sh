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
WWW_DIR=/var/www/html

echo "# Extract the TFTPBOOT files."
cd ${TFTPBOOT_DIR}
tar -zxf ${SHELLDIR}/tftpboot.tgz

#cp ${SYSLINUX_DIR}/menu.c32 ${TFTPBOOT_DIR}
#cp ${SYSLINUX_DIR}/pxelinux.0 ${TFTPBOOT_DIR}

echo "# Enablethe TFTP service"
sed -i -e "/disable/s/=.*/= no/" /etc/xinetd.d/tftp

cd ${SHELLDIR}
echo "# Put the PXE values need for dnsmasq in its config directory."
sed -e "s/CLOUD_EXT_ROUTER_HOSTNAME/$CLOUD_EXT_ROUTER_HOSTNAME}/" -e "s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/" -e "s/CLOUD_EXT_ROUTER_IPADDR/$CLOUD_EXT_ROUTER_IPADDR}/" ./pxe.d > /etc/dnsmasq.d/pxe.d 

echo "# Setup the http service."

yum -y install httpd || exit 1 

echo "# copy the kickstart files to the http service."
cd ${WWW_DIR}
tar -zxf ${SHELLDIR}/www_html.tgz

systemctl enable xinetd.service
systemctl restart xinetd.service

systemctl enable httpd.service
systemctl restart httpd.service

