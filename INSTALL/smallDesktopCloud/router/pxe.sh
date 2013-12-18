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

CURSHELLDIR=`pwd`

echo "# Extract the TFTPBOOT files."
cd tftpboot
cp -r * ${TFTPBOOT_DIR}
#cd ${TFTPBOOT_DIR}
#tar -zxf ${SHELLDIR}/tftpboot.tgz


echo "# Enablethe TFTP service"
sed -i -e "/disable/s/=.*/= no/" /etc/xinetd.d/tftp

cd ${SHELLDIR}
echo "# Put the PXE values need for dnsmasq in its config directory."
sed -e "s/CLOUD_EXT_ROUTER_HOSTNAME/$CLOUD_EXT_ROUTER_HOSTNAME}/" -e "s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/" -e "s/CLOUD_EXT_ROUTER_IPADDR/$CLOUD_EXT_ROUTER_IPADDR}/" ./pxe.d > /etc/dnsmasq.d/pxe.d 

echo "# Setup the http service."

yum -y install httpd || exit 1 

echo "# copy the kickstart and yum repository files to the http service."
cd www_html
cp -r * ${WWW_DIR}
#cd ${WWW_DIR}
#tar -zxf ${SHELLDIR}/www_html.tgz

cd ${WWW_DIR}/armoredrepo/CentOS6.5
createrepo .

# The kick start files have some network variables in them that need to be
# replaced with the real thing.
cd ${WWW_DIR}
KSFILES=`find . -name \*.ks`
echo "# Editing KSFILES=${KSFILES} for network parameters."
for ksFile in ${KSFILES} ; do 
  sed -i -e "s/@CLOUD_EXT_ROUTER_IPADDR@/${CLOUD_EXT_ROUTER_IPADDR}/g" ${ksFile}
  sed -i -e "s/@CLOUD_EXT_COMPUTE_IPADDR_PREFIX@/${CLOUD_EXT_COMPUTE_IPADDR_PREFIX}/g" ${ksFile}
  sed -i -e "s/@CLOUD_DATA_COMPUTE_IPADDR_PREFIX@/${CLOUD_DATA_COMPUTE_IPADDR_PREFIX}/g" ${ksFile}
  sed -i -e "s/@CLOUD_EXT_NETMASK@/${CLOUD_EXT_NETMASK}/g" ${ksFile}
done



systemctl enable xinetd.service
systemctl restart xinetd.service

systemctl enable httpd.service
systemctl restart httpd.service

