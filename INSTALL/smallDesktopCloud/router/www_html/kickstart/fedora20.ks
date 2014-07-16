#version=DEVEL
# System authorization information
text
auth --enableshadow --passalgo=sha512

# Use network installation
url --url="http://kickstart.ittc.ku.edu/mirror/fedora/linux/releases/20/Fedora/x86_64/os/"
# Run the Setup Agent on first boot
#firstboot --enable
firstboot --disable
ignoredisk --only-use=sda
# Keyboard layouts
keyboard --vckeymap=us --xlayouts='us'
# System language
lang en_US.UTF-8

services --disabled=NetworkManager,firstboot,firewalld
services --enabled=network,ntpdate,ntpd,iptables

# Network information

# Root password
rootpw --iscrypted $6$MJw2mo37T.E10y7v$VaQPcrRFeFI3xYL3luUDj1nY4gT8rvp4uynTW9hQRY8sHbE/xG.mafMRPXLAxG1haFe4qp3ZmohqhnM52xzKc.
# System timezone
timezone US/Central --isUtc

# System bootloader configuration
bootloader --location=mbr --boot-drive=sda
autopart --type=lvm
# Partition clearing information
clearpart --all --initlabel --drives=sda

# Services
selinux --disabled

# Reboot after install is complete.
reboot

#
%include /tmp/part.inc

%packages
@core
git

%end

%pre  ######################################################

# We want the person performing the Kickstart to specifiy the compute node
# ID on the kernel command line using the name 'NODEID'. We then use this
# as the last part of the IP address for eth0.

grep -q 'NODEID=[[:alnum:]]' /proc/cmdline

if [ $? -eq 0 ]; then
# Keep everything from '=' to next whitespace
    NODEID=$(sed 's/.* NODEID=\([^[:space:]]*\).*/\1/' /proc/cmdline)
else
    NODEID=0
fi

if [ ${NODEID} -eq 0 ] ; then
  exec < /dev/tty3 > /dev/tty3
  chvt 3
  echo "A NODEID=0 is not allowed. Change the NODEID from the PXE menu using <Tab> key."
  /bin/sh
  halt
fi

echo "network --onboot=yes --bootproto=dhcp --hostname=compute${NODEID}.@CLOUD_EXT_DOMAIN@" | tee -a /tmp/part.inc

%end




%post #####################################################

# Get most recent RPM updates.
yum -y update
yum -y redhat-lsb-core

# Add the armored user.
groupadd --gid 9100 armored
useradd --uid 9100 --gid 9100 --create-home armored
echo "armored" | passwd armored --stdin

# We need to give the user 'armored' sudo permissions.
echo "armored ALL=(root) ALL" | tee -a /etc/sudoers.d/armored

# Create a shell script that will clone the armored code repository.
GIT_ARMORED_CODE=~armored/git-armored-code.sh
cat > ${GIT_ARMORED_CODE} <<EOF
#!/bin/bash
# Use git to obtain a anonymous clone of the Armored Software 'code' repostiroty
# NOTE: This file is created in the kickstart file %post section.
git clone https://github.com/armoredsoftware/code.git
EOF

chmod a+x ${GIT_ARMORED_CODE}
chown armored ${GIT_ARMORED_CODE}

# Create a shell script that will change the user of the git clone
# so that a 'git push' may be done.
GIT_CHANGE_USER=~armored/git-change-user.sh
cat > ${GIT_CHANGE_USER} <<EOF
#!/bin/sh
#
# Change the username for the git push command to work.
# NOTE: This file is created in the kickstart file %post section.
# 
if [ ! \$# -eq 1 ] ; then 
  echo "You must provide 1 argument that is a github username that is a member of the armoredsoftware github group."
  exit 1
fi
git remote set-url origin https://\${1}@github.com/armoredsoftware/code.git
git config --global user.name "\${1}"
EOF

chmod a+x ${GIT_CHANGE_USER}
chown armored ${GIT_CHANGE_USER}

README_FILE=~armored/README.md
cat > ${README_FILE} <<EOF
ARMORED USER
============
The script '${GIT_ARMORED_CODE}' is used to make a clone of the Armored Software
git repository. It is any anonymous clone.
The script '${GIT_CHANGE_USER}' is used to change the git clone user to your
github user name (that must be a member of the ArmoredSoftware github group)
so that you may submit code revisions.
EOF

# Get the NODEID from the command line.
grep -q 'NODEID=[[:alnum:]]' /proc/cmdline
NODEID=$(sed 's/.* NODEID=\([^[:space:]]*\).*/\1/' /proc/cmdline)

EM1_CONFIG_FILE=/etc/sysconfig/network-scripts/ifcfg-em1
P5P1_CONFIG_FILE=/etc/sysconfig/network-scripts/ifcfg-p5p1

# Make the networking static.
#setup em1
# First get the UUID and the HWADDR from the device configurtion file setup
# by kickstart.
UUID=$(grep -e '^UUID=' ${EM1_CONFIG_FILE} | sed -e 's/UUID=//')
HWADDR=$(grep -e '^HWADDR=' ${EM1_CONFIG_FILE} | sed -e 's/HWADDR=//')

cat > /etc/sysconfig/network-scripts/ifcfg-em1 <<EOF
UUID=${UUID}
DNS1=@CLOUD_EXT_ROUTER_IPADDR@
BOOTPROTO=none
DEVICE=em1
ONBOOT=yes
IPV6INIT=no
HWADDR=${HWADDR}
TYPE=Ethernet
IPADDR=@CLOUD_EXT_COMPUTE_IPADDR_PREFIX@.${NODEID}
NETMASK=@CLOUD_EXT_NETMASK@
GATEWAY=@CLOUD_EXT_ROUTER_IPADDR@
DEFROUTE=yes
IPV4_FAILURE_FATAL=no
NAME="System em1"
EOF

UUID=$(grep -e '^UUID=' ${P5P1_CONFIG_FILE} | sed -e 's/UUID=//')
HWADDR=$(grep -e '^HWADDR=' ${P5P1_CONFIG_FILE} | sed -e 's/HWADDR=//')

cat > ${P5P1_CONFIG_FILE} <<EOF
UUID=${UUID}
BOOTPROTO=none
DEVICE=p5p1
ONBOOT=yes
IPV6INIT=no
HWADDR=${HWADDR}
TYPE=Ethernet
IPADDR=@CLOUD_DATA_COMPUTE_IPADDR_PREFIX@.${NODEID}
NETMASK=@CLOUD_EXT_NETMASK@
GATEWAY=@CLOUD_EXT_ROUTER_IPADDR@
IPV4_FAILURE_FATAL=no
NAME="System p5p1"
EOF

# Set up the name resolution file.
cat > /etc/resolv.conf <<EOF
domain		       @CLOUD_EXT_DOMAIN@
search		       @CLOUD_EXT_DOMAIN@
nameserver	@CLOUD_EXT_ROUTER_IPADDR@
EOF

# Bring up the interface.
ifdown em1
ifup em1

%end

