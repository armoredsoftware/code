#!/bin/bash
#
# Setup a PXE server on the router.
# This script is derived from the information found at
# http://www.justnudge.com/2013/02/setting-up-a-pxe-boot-server-with-centos-and-dnsmasq/
# 
# Usage sudo pxe.sh [ksonly]
#
# If the optional 'ksonly' is an argument then only copy the files that 
# are needed for updating the kickstart. These files are
# ./www_html/kickstart/*.ks
# ./tftpboot/pxelinux.cfg/default

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

KSONLY="no"

# see if we are only updating the kickstart files.
if [ $# -eq 1 ] ; then 
  if [ $1 = "ksonly" ] ; then
    echo "Only updating the kickstart files."
    KSONLY="yes"    
  else
    echo "Argument '$1' is invalid. Only 'ksonly' is valid."
    exit 1
  fi
fi

SYSLINUX_DIR=/usr/share/syslinux
TFTPBOOT_DIR=/var/lib/tftpboot
WWW_DIR=/var/www/html

CURSHELLDIR=`pwd`


if [ ! "${KSONLY}" = "yes" ] ; then
  echo "# Setup the PXE server."
  yum install syslinux tftp tftp-server -y || exit 1


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
else

  if [ ! -d ${TFTPBOOT_DIR} ] ; then
    echo "The directory '${TFTPBOOT_DIR}' does not exit. Has tftp and tftp-server been installed?"
    exit 1
  fi

  cd tftpboot/pxelinux.cfg
  cp -v * ${TFTPBOOT_DIR}/pxelinux.cfg

  cd ${SHELLDIR}

  cd tftpboot/images
  cp -v -r * ${TFTPBOOT_DIR}/images


  cd ${SHELLDIR}
  
  if [ ! -d ${WWW_DIR} ] ; then
    echo "The directory '${WWW_DIR}' does not exit. Has httpd been installed?"
    exit 1
  fi

  cd www_html/kickstart
  cp -v *.ks ${WWW_DIR}/kickstart
fi

cd ${SHELLDIR}

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
  sed -i -e "s/@CLOUD_EXT_DOMAIN@/${CLOUD_EXT_DOMAIN}/g" ${ksFile}
done

if [ ! "${KSONLY}" = "yes" ] ; then

  systemctl enable xinetd.service
  systemctl restart xinetd.service

  systemctl enable httpd.service
  systemctl restart httpd.service
fi

