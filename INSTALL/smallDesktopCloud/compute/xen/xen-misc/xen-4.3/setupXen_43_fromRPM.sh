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

# Create bridge config files and modify static network device config
move_ip_from_phy_to_bridge ${CLOUD_EXT_COMPUTE_DEVICE} yes ${CLOUD_EXT_COMPUTE_BRIDGE}
move_ip_from_phy_to_bridge ${CLOUD_DATA_COMPUTE_DEVICE} no ${CLOUD_DATA_COMPUTE_BRIDGE}

# Setup the openvswitch
# Since Openstack uses openvswitch we might as well get used to it.
systemctl enable openvswitch.service
systemctl start openvswitch.service


#ovs-vsctl add-br ${CLOUD_EXT_COMPUTE_BRIDGE}
#ovs-vsctl add-port ${CLOUD_EXT_COMPUTE_BRIDGE} ${CLOUD_EXT_COMPUTE_DEVICE}

#ovs-vsctl add-br ${CLOUD_DATA_COMPUTE_BRIDGE}
#ovs-vsctl add-port ${CLOUD_DATA_COMPUTE_BRIDGE} ${CLOUD_DATA_COMPUTE_DEVICE}


exit 0

systemctl restart network.service


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
