# Kickstart file automatically generated by anaconda.

#version=DEVEL
install
url --url=http://kickstart.ittc.ku.edu/mirror/centos/6.5/os/x86_64
lang en_US.UTF-8
keyboard us
rootpw  --iscrypted $6$BRSRiEH1ULNKcY5Z$PP4VuU.kW9hllPBZ/uq1e/K0LoesiiFpNpHwGln8Z.wZCAPfl.ryDSzv6epYO0/Yezk2tlOkWCqIciQKr5eQY0
#firewall --service=ssh
firewall --disabled
authconfig --enableshadow --passalgo=sha512
selinux --disabled
timezone --utc America/Chicago
bootloader --location=mbr --driveorder=sda --append="crashkernel=auto rhgb quiet"
# The following is the partition information you requested
# Note that any partitions you deleted are not expressed
# here so unless you clear all partitions first, this is
# not guaranteed to work
clearpart --all --drives=sda

part /boot --fstype=ext4 --size=500
part pv.008002 --grow --size=1

volgroup vg_compute2 --pesize=4096 pv.008002
logvol /home --fstype=ext4 --name=lv_home --vgname=vg_compute2 --grow --size=100
logvol / --fstype=ext4 --name=lv_root --vgname=vg_compute2 --grow --size=1024 --maxsize=51200
logvol swap --name=lv_swap --vgname=vg_compute2 --grow --size=8000 --maxsize=8000

reboot

repo --name="ITTC CentOS"  --baseurl=http://kickstart.ittc.ku.edu/mirror/centos/6.5/os/x86_64 --cost=100

# /tmp/part.inc is created in the %pre section.
%include /tmp/part.inc

%packages
@core
@server-policy
@workstation-policy
git
emacs
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

echo "network --onboot yes --device eth0 --bootproto static --ip @CLOUD_EXT_COMPUTE_IPADDR_PREFIX@.${NODEID} --netmask @CLOUD_EXT_NETMASK@ --gateway @CLOUD_EXT_ROUTER_IPADDR@ --noipv6 --nameserver @CLOUD_EXT_ROUTER_IPADDR@ --hostname compute${NODEID}.ext.armored" | tee -a /tmp/part.inc

echo "network --onboot yes --device eth1 --bootproto static --ip @CLOUD_DATA_COMPUTE_IPADDR_PREFIX@.${NODEID} --netmask @CLOUD_EXT_NETMASK@ --noipv6" | tee -a /tmp/part.inc


%end

%post #####################################################

# Get most recent RPM updates.
yum -y update

# Add the armored user.
groupadd --gid 9100 armored
useradd --uid 9100 --gid 9100 --create-home armored
echo "armored" | passwd armored --stdin

# We need to give the user 'armored' sudo permissions.
echo "armored ALL=(root) ALL" | tee -a /etc/sudoers.d/armored

# Create a shell script that will clone the armored code repository.
GIT_ARMORED_CODE=~armored/git-armored-code.sh
echo "#!/bin/bash" > ${GIT_ARMORED_CODE}
echo "# Use git to obtain a anonymous clone of the Armored Software 'code' repostiroty" >> ${GIT_ARMORED_CODE}
echo "git clone https://github.com/armoredsoftware/code.git" >> ${GIT_ARMORED_CODE}
chmod a+x ${GIT_ARMORED_CODE}
chown armored ${GIT_ARMORED_CODE}


# Create a shell script that will change the user of the git clone
# so that a 'git push' may be done.
GIT_CHANGE_USER=~armored/git-change-user.sh
cat > ${GIT_CHANGE_USER} <<EOF
#!/bin/sh
#
# Change the username for the git push command to work.
# 
if [ ! \$# -eq 1 ] ; then 
  echo "You must provide 1 argument that is a github username that is a member of the armoredsoftware github group."
  exit 1
fi
git remote set-url origin https://\${1}@github.com/armoredsoftware/code.git
EOF

chmod a+x ${GIT_CHANGE_USER}
chown armored ${GIT_CHANGE_USER}


%end