#!/bin/bash
# Install and setup xen 4.3 from RPMs.
#
# Usage: ./setupXen_43_fromRPM.sh [ flask ]
# The optional 'flask' argument we cause the flask to be enabled in the hypervisor.

TOP_LEVEL=${TOP_LEVEL:-../../../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }
{ . ${UTIL_DIR}/network-fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/network-fns." ; exit 1; }
{ . ${UTIL_DIR}/params-network ; } || { echo "!!! Failed to source file ${UTIL_DIR}/params-network." ; exit 1; }



# We have to be the root user.
check_root_user

# Get the RPMs
yum -y install xen xen-hypervisor xen-libs xen-runtime
yum -y install xen-devel

yum -y install openvswitch

# We are not comfortable yet with firewalld, openvswitch bridges and NAT yet so lets us iptables instead.
systemctl stop firewalld
systemctl disable firewalld
yum -y install iptables-services

# Create bridge config files and modify static network device config
# Note that this network device config setup makes it so that we do not have
# do do the following type of setup from the command line.:
# ovs-vsctl add-br xenbr0
# ovs-vsctl add-port xenbr0 eth0
move_ip_from_phy_to_bridge ${CLOUD_EXT_COMPUTE_DEVICE} yes ${CLOUD_EXT_COMPUTE_BRIDGE}
move_ip_from_phy_to_bridge ${CLOUD_DATA_COMPUTE_DEVICE} no ${CLOUD_DATA_COMPUTE_BRIDGE}

# setup some networking needed by xen-4.3
cp ./xen-4.3-network.conf /etc/sysctl.d
# Make it happen now.
sysctl -p /etc/sysctl.d/xen-4.3-network.conf

# Setup NAT.
iptables -t nat -A POSTROUTING -o ${CLOUD_EXT_COMPUTE_DEVICE} -j MASQUERADE
# Put the iptables on the persitance place
iptables-save > /etc/sysconfig/iptables

systemctl enable iptables.service

#firewall-cmd --permanent --zone=external --add-masquerade
#firewall-cmd --permanent --zone=external --change-interface=${CLOUD_EXT_COMPUTE_DEVICE}
#firewall-cmd --permanent --zone=internal --change-interface=${CLOUD_EXT_COMPUTE_BRIDGE}
#firewall-cmd --permanent --zone=external --change-interface=${CLOUD_DATA_COMPUTE_DEVICE}
#firewall-cmd --permanent --zone=internal --change-interface=${CLOUD_DATA_COMPUTE_BRIDGE}


# Setup the openvswitch
# Since Openstack uses openvswitch we might as well get used to it.
systemctl enable openvswitch.service
systemctl start openvswitch.service

systemctl restart network.service
systemctl start iptables.service

# setup some xen configuration.


# Enable the xen domain services.
systemctl enable xendomains.service

# We have a grub configuration script that will add more entries for xen hypervisors
# with xsm enabled in the grub configuration 
cp ./21_linux_xen_flask /etc/grub.d

# remake the grub configuration
grub2-mkconfig -o /boot/grub2/grub.cfg > /dev/null

if [ "x$1" == "xflask" ] ; then 
  # We want flask in xen
  echo "Setting up FLASK in xen hypervisor."

  # find the flask grub entries.
  flask_menu_entries=$(grep "^\s*menuentry" /boot/grub2/grub.cfg | cut -d "'" -f2 | grep "Xen.*Flask")

  if [ "x${flask_menu_entries}" != "x" ] ; then
     echo "####################################################"
     echo "Pick one of the xen with flask entries from grub for you default boot entry."
     echo "${flask_menu_entries}"
     defaultEntry=$(echo -n "${flask_menu_entries}" | head -n 1 | tr -d '\n' )
     echo "> sudo grub2-set-default '${defaultEntry}'"
     echo "> sudo grub2-editenv list"
     echo "> sudo grub2-mkconfig -o /boot/grub2/grub.cfg"
     echo "####################################################"
  else
     echo "No Xen with Flask entries were found in the grub configuration file."
     echo "!!!!!!!!! You need to find out what went wrong. !!!!!!!!!"
  fi
else
  # dont' use flask. 
  grub2-set-default 'Fedora, with Xen hypervisor'
  grub2-editenv list
  grub2-mkconfig -o /boot/grub2/grub.cfg
  
  echo "####################################################"
  echo "Reboot to start the xen hypervisor."
  echo "####################################################"
fi
