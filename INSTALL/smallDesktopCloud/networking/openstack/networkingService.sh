#!/bin/bash
# Install the networking services for a dedicated networking node.
#
# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# ------------------------

# For neutron networking we want to move the static address of a physical
# device to a bridge.
# Usage: move_ip_from_phy_to_bridge <phyDeivceName> <promisc> <bridgeName>
# <phyDeviceName> - like 'em1'.
# <promisc>       - promiscuous mode 'yes' or 'no'.
# <bridgeName>    - name of the bridge device like 'br-ex'
move_ip_from_phy_to_bridge () {

  if [ $# -ne 3 ] ; then
    echo "Need 3 arguments to function ${FUNCNAME[0]}."
    exit 1
  fi

  # Config files for the network devices.
  IFCFG_DEVICE=/etc/sysconfig/network-scripts/ifcfg-$1
  IFCFG_BRIDGE=/etc/sysconfig/network-scripts/ifcfg-$3

  # Make sure we have not already done the work.
  if [ -f ${IFCFG_BRIDGE} ] ; then
    echo "# Bridge file '${IFCFG_BRIDGE}' already exists. Assuming that we don't have to modify it."
    return
  fi

  # Make sure that the physical device has a static address.
  grep -e "^IPADDR" ${IFCFG_DEVICE} > /dev/null
  if [ $? -ne 0 ] ; then
    echo "# The config file for device $1 has not been setup to be static."
    echo "!!!!! Unable to continue with bridge setup."
    exit 1
  fi

  touch ${IFCFG_BRIDGE}
  echo "DEVICE=${3}" >> ${IFCFG_BRIDGE}
  echo "TYPE=Bridge" >> ${IFCFG_BRIDGE}
  echo "ONBOOT=no" >> ${IFCFG_BRIDGE}
  echo "BOOTPROTO=none" >> ${IFCFG_BRIDGE}
  # get IPADDR, NETMASK and GATEWAY from the physical device config file.
  EXT_IPADDR=$(grep -e "IPADDR[ ]*=" ${IFCFG_DEVICE} | sed -e "s/^.*=[ ]*//")
  echo "IPADDR=${EXT_IPADDR}" >> ${IFCFG_BRIDGE}
  EXT_NETMASK=$(grep -e "NETMASK[ ]*=" ${IFCFG_DEVICE} | sed -e "s/^.*=[ ]*//")
  echo "NETMASK=${EXT_NETMASK}" >> ${IFCFG_BRIDGE}
  EXT_GATEWAY=$(grep -e "^[ ]*GATEWAY[ ]*=" ${IFCFG_DEVICE} | sed -e "s/^.*=[ ]*//" )
  if [ "${EXT_GATEWAY}" != "" ] ; then
    echo "GATEWAY=${EXT_GATEWAY}" >> ${IFCFG_BRIDGE}
  fi

  echo "# Modifying file '$1'."
  # comment out the static IP parameters
  sed -i -e "/^[ ]*IPADDR[ ]*=/s/^/#/" ${IFCFG_DEVICE}
  sed -i -e "/^[ ]*NETMASK[ ]*=/s/^/#/" ${IFCFG_DEVICE}
  sed -i -e "/^[ ]*GATEWAY[ ]*=/s/^/#/" ${IFCFG_DEVICE}
  sed -i -e "/^[ ]*BOOTPROTO[ ]*=/s/=.*/=none/" ${IFCFG_DEVICE}
  grep -e "PROMISC[ ]*=" ${IFCFG_DEVICE} > /dev/null
  if [ "$?" == "0" ] ; then
    # just change the value and uncomment.
    sed -i -e "/PROMISC/s/^.*\$/PROMISC=$2/" ${IFCFG_DEVICE}
  else
    # add the value
    sed -i -e "\$ a\
PROMISC=$2" ${IFCFG_DEVICE}
  fi

}


# ------------------------

# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd

# We have to be the root user.
check_root_user

# Check for some prereq's
# check_mysql_running
check_openstack_utils

# We need to create the neutron user before installing the package(s).
create_neutron_unix_user

EMAIL_ADDRESS=get_email_address

echo "# Load the Neutron packages."
yum -y install openstack-neutron || exit 1

# Configure some unix networking parameters.
cp -f ./neutron_sysctl.conf /etc/sysctl.d || exit 1
#echo "Restart network deamon so new unix network parameters will take effect."
#systemctl restart network.service

#echo "# Reread the network system config files."
sysctl -p /etc/sysctl.d/neutron_sysctl.conf

echo "# Setup the neutron configuration file."
set_service_conffile_auth /etc/neutron/neutron.conf keystone_authtoken ${CLOUD_EXT_CONTROL_HOSTNAME} neutron ${ID_NET_PW}
openstack-config --set /etc/neutron/neutron.conf keystone_authtoken auth_port 35357
openstack-config --set /etc/neutron/neutron.conf keystone_authtoken auth_protocol http

echo "# Set neutron config for reaching mysql database."
openstack-config --set /etc/neutron/neutron.conf database connection mysql://neutron:${NETDB_PW}@${CLOUD_EXT_CONTROL_HOSTNAME}/neutron

set_service_pastefile_auth /etc/neutron/api-paste.ini ${CLOUD_EXT_CONTROL_HOSTNAME} neutron ${ID_NET_PW}
grep -e "^auth_uri=" /etc/neutron/api-paste.ini  > /dev/null
if [ "$?" != "0" ] ; then
  sed -i -e "/^\[filter:authtoken\]/ a\
auth_uri=http://${CLOUD_EXT_CONTROL_HOSTNAME}:5000" /etc/neutron/api-paste.ini
fi


#-------------------
# Now we need to add a neutron plugin.
# We will use >>> openvswitch <<<.

yum -y install openstack-neutron-openvswitch

# Start up the service
systemctl restart openvswitch

# We need an integration bridge and an external bridge  no matter what.
ovs-vsctl add-br br-int
ovs-vsctl add-br br-ex

# Add a port (connection) from the EXTERNAL_INTERVACE to br-ex interface.
ovs-vsctl add-port br-ex ${CLOUD_EXT_NETWORKING_DEVICE}

# 
move_ip_from_phy_to_bridge ${CLOUD_EXT_NETWORKING_DEVICE} yes br-ex

echo "# Edit the /etc/neutron/l3_agent.ini file."
openstack-config --set /etc/neutron/l3_agent.ini DEFAULT interface_driver neutron.agent.linux.interface.OVSInterfaceDriver
openstack-config --set /etc/neutron/l3_agent.ini DEFAULT use_namespaces True

echo "# Edit the /etc/neutron/dhcp_agent.ini file."
openstack-config --set /etc/neutron/dhcp_agent.ini DEFAULT interface_driver neutron.agent.linux.interface.OVSInterfaceDriver
openstack-config --set /etc/neutron/dhcp_agent.ini DEFAULT use_namespaces True

echo "# Enable veth"
openstack-config --set /etc/neutron/l3_agent.ini DEFAULT ovs_use_veth True
openstack-config --set /etc/neutron/dhcp_agent.ini DEFAULT ovs_use_veth True

openstack-config --set /etc/neutron/neutron.conf DEFAULT core_plugin neutron.plugins.openvswitch.ovs_neutron_plugin.OVSNeutronPluginV2

# ---------------------------------------------------------------------
# Setup plugin for VLANs

echo "# configure /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini."
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini ovs tenant_network_type vlan
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini ovs network_vlan_ranges  physnet1:1:4094
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini ovs bridge_mappings  physnet1:br-${CLOUD_DATA_NETWORKING_DEVICE}

# If we have problems with network namespaces we can turn them off.
#openstack-config --set /etc/neutron/l3_agent.ini DEFAULT use_namespaces false
#openstack-config --set /etc/neutron/dhcp_agent.ini DEFAULT use_namespaces false
#openstack-config --set /etc/neutron/neutron.conf DEFAULT allow_overlapping_ips false

ovs-vsctl add-br br-${CLOUD_DATA_NETWORKING_DEVICE}
ovs-vsctl add-port br-${CLOUD_DATA_NETWORKING_DEVICE} ${CLOUD_DATA_NETWORKING_DEVICE}

move_ip_from_phy_to_bridge ${CLOUD_DATA_NETWORKING_DEVICE} no br-${CLOUD_DATA_NETWORKING_DEVICE}

# --------------------------------------------------------

# Setup the vos firewall to use.
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini securitygroup firewall_driver neutron.agent.firewall.NoopFirewall

#------------------

#systemctl enable openvswitch

systemctl restart neutron-openvswitch-agent
systemctl enable neutron-openvswitch-agent


# -----------------------
openstack-config --set /etc/neutron/dhcp_agent.ini DEFAULT dhcp_driver neutron.agent.linux.dhcp.Dnsmasq

systemctl restart neutron-dhcp-agent
systemctl restart neutron-l3-agent
systemctl enable neutron-dhcp-agent
systemctl enable neutron-l3-agent


