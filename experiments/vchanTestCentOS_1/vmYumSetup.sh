#!/bin/sh

# Setup the yum repositorys in a VM that is CentOS.
#

# Make sure we are the arguments we want
if [ $# != 1 ] ; then
  echo "Usage: vmYumSetup.sh <VM ip addr>"
  exit;
fi

# Make sure that there is an argument that is an existing IP address.

ping -c 1 $1
if [ $? != 0 ] ; then 
  echo "Failed to ping VM at address" $1
  exit
fi

EPEL_RPM_LINK=epel-release-6-8.noarch.rpm

wget http://linux.mirrors.es.net/fedora-epel/6/i386/${EPEL_RPM_LINK}
scp ${EPEL_RPM_LINK} root@${1}:

scp /etc/yum.repos.d/ArmoredConfig.repo root@${1}:/etc/yum.repos.d
scp /etc/yum.repos.d/xen-c6-tweaked.repo root@${1}:/etc/yum.repos.d
#scp /etc/yum.repos.d/epel.repo root@${1}:/etc/yum.repos.d

