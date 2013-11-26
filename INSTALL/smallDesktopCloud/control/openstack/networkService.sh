#!/bin/bash

# Set up the openstack network service 'neutron'.

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# -------------------------------------


# ------------------------

# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd

# We have to be the root user.
check_root_user

# Check for some prereq's
check_mysql_running
check_message_queue_running
check_openstack_utils

NEUTRON_HOSTNAME=${CLOUD_EXT_CONTROL_HOSTNAME}

# -------------------------------------------------------

echo "# Setup the neutron MySQL database."
# The openstack-db utility does not know about the 'neutron' service yet.
# Do the setup the hardway.
#openstack-db --init --service neutron --password ${NETDB_PW} --rootpw ${MYSQL_ROOT_PW}
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="CREATE DATABASE neutron;"
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="CREATE USER 'neutron'@'localhost' IDENTIFIED BY '${NETDB_PW}';"
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="CREATE USER 'neutron'@'%' IDENTIFIED BY '${NETDB_PW}';"
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="GRANT ALL PRIVILEGES ON neutron.* TO 'neutron'@'localhost';"
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="GRANT ALL PRIVILEGES ON neutron.* TO 'neutron'@'%' ;"
mysql --user=root --password=${MYSQL_ROOT_PW} --execute="flush privileges;"

#---------------------------------------------------------

check_keystone_access

EMAIL_ADDRESS=get_email_address


keystone user-get neutron > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create keystone user neutron"
  keystone user-create --name=neutron --pass=${ID_NET_PW} --email=${EMAIL_ADDRESS}
else
  echo "# Set user keystone neutron password."
  keystone  user-password-update  --pass=${ID_NET_PW} neutron
fi

keystone user-role-list --user neutron --tenant service | grep admin > /dev/null
if [ "$?" != "0" ] ; then
  # The neutron user does not have any roles yet.
  echo "# Add role admin to id neutron"
  keystone user-role-add --user=neutron --tenant=service --role=admin
fi

keystone service-get neutron > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Register the networking service the identity service."
  keystone service-create --name=neutron --type=network --description="Openstack Networking Service" > /dev/null
fi

SERVICE_ID=$(keystone service-get neutron | grep -e "[[:space:]]id[[:space:]]" | sed -e "s/id//" -e "s/[ |]//g")

echo "# Neutron service ID=${SERVICE_ID}"

# Look for an endpoint for the service.
keystone endpoint-list | grep -e "${SERVICE_ID}" > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Creating the Neutron endpoint in the id registry"
  keystone endpoint-create --service-id=${SERVICE_ID} --publicurl=http://${NEUTRON_HOSTNAME}:9696 --internalurl=http://${NEUTRON_HOSTNAME}:9696 --adminurl=http://${NEUTRON_HOSTNAME}:9696 > /dev/null
fi

# --------------------------


create_neutron_unix_user

echo "# Load the Neutron packages."
yum -y install openstack-neutron python-neutron python-neutronclient
if [ "$?" != "0" ] ; then
  echo "There was a problem installing the neutron package."
  exit 1;
fi


set_service_conffile_auth /etc/neutron/neutron.conf keystone_authtoken ${CLOUD_EXT_CONTROL_HOSTNAME} neutron ${ID_NET_PW}
openstack-config --set /etc/neutron/neutron.conf keystone_authtoken auth_url http://${CLOUD_EXT_CONTROL_HOSTNAME}:35357/v2.0

set_qpid_config /etc/neutron/neutron.conf ${CLOUD_EXT_CONTROL_HOSTNAME}


#echo "Reread the network system config files."
#sysctl -p

openstack-config --set /etc/neutron/neutron.conf database connection mysql://neutron:${NETDB_PW}@${CLOUD_EXT_CONTROL_HOSTNAME}/neutron


set_service_pastefile_auth /etc/neutron/api-paste.ini ${CLOUD_EXT_CONTROL_HOSTNAME} neutron ${ID_NET_PW}

#grep -e "^auth_uri=" /etc/neutron/api-paste.ini  > /dev/null
#if [ "$?" != "0" ] ; then
#  sed -i -e "/^\[filter:authtoken\]/ a\
#auth_uri=http://${CLOUD_PUBLIC_HOSTNAME}:5000" /etc/neutron/api-paste.ini
#fi

# -----------------------------------------------------
# Install the open vSwitch plugin.

echo "# We are using open vSwitch for our core neutron plugin."
yum -y install openstack-neutron-openvswitch || exit 1

openstack-config --set /etc/neutron/neutron.conf DEFAULT core_plugin neutron.plugins.openvswitch.ovs_neutron_plugin.OVSNeutronPluginV2 || exit 1

echo "# We are choosing to use VLANs with open vSwitch."
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini ovs tenant_network_type vlan
openstack-config --set /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini ovs network_vlan_ranges physnet1:1:4094

# ------------------------------------------------------
echo "# Set up the nova config file for using neutron networking."
# Tell nova about neutron.
openstack-config --set /etc/nova/nova.conf DEFAULT network_api_class nova.network.neutronv2.api.API
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_url http://${CLOUD_EXT_NETWORKING_HOSTNAME}:9696
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_auth_strategy keystone
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_admin_tenant_name service
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_admin_username neutron
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_admin_password ${ID_NET_PW}
openstack-config --set /etc/nova/nova.conf DEFAULT neutron_admin_auth_url http://${CLOUD_EXT_CONTROL_HOSTNAME}:35357/v2.0
openstack-config --set /etc/nova/nova.conf DEFAULT firewall_driver nova.virt.firewall.NoopFirewallDriver



#------------------------------------------------------
# For some reason we need this symbolic link.
ln -s /etc/neutron/plugins/openvswitch/ovs_neutron_plugin.ini /etc/neutron/plugin.ini

systemctl restart neutron-server
systemctl enable neutron-server


