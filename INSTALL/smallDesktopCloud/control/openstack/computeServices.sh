#!/bin/bash

# Set up the openstack controller compute services (nova).

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# -------------------------------------
# -------------------------------------

# Load some networking parameters.
# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd

# We have to be the root user.
check_root_user

# Check for some prereq's
check_mysql_running
check_message_queue_running
check_openstack_utils

# Make the nova user.
create_nova_unix_user

# What host is the mysql database installed on. 
MYSQL_HOSTNAME=${CLOUD_EXT_CONTROL_HOSTNAME}
NOVA_HOSTNAME=${CLOUD_EXT_CONTROL_HOSTNAME}

echo "# Get the nova packages using yum."
yum -y install openstack-nova python-novaclient

# openstack-nova addes a file to /etc/sudoers.d that does not get
# included by ITTC's /etc/sudoers file. So include it.
grep "#includedir /etc/sudoers.d" /etc/sudoers
if [ "$?" != "0" ] ; then
  echo "# Adding '#includedir /etc/sudoers.d"
  sed -i -e "$ a\
#includedir /etc/sudoers.d" /etc/sudoers
fi

echo "# Setup the nova database."
openstack-config --set /etc/nova/nova.conf database connection mysql://nova:${COMPUTEDB_PW}@${NOVA_HOSTNAME}/nova
openstack-db --init --service nova --password ${COMPUTEDB_PW}  --rootpw ${MYSQL_ROOT_PW}

echo "# Setup the nova configuration file."
openstack-config --set /etc/nova/nova.conf DEFAULT my_ip ${CLOUD_DATA_CONTROL_IPADDR}
openstack-config --set /etc/nova/nova.conf DEFAULT vncserver_listen ${CLOUD_DATA_CONTROL_IPADDR}
openstack-config --set /etc/nova/nova.conf DEFAULT vncserver_proxyclient_address ${CLOUD_DATA_CONTROL_IPADDR}


# Get the keystone.rc source so we have the credentials to use keystone.
source_keystone_rc

EMAIL_ADDRESS=get_email_address

# Make sure that we can talk to keystone.
keystone user-list > /dev/null
if [ "$?" != "0" ] ; then
  echo "!!!!!! Failed to talk to keystone service."
  exit 1;
fi

keystone user-get nova > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create keystone user nova"
  keystone user-create --name=nova --pass=${ID_COMPUTE_PW} --email=${EMAIL_ADDRESS}
else
  echo "# Set user keystone glance password."
  keystone  user-password-update  --pass=${ID_COMPUTE_PW} nova
fi

keystone user-role-list --user nova --tenant service | grep admin > /dev/null
if [ "$?" != "0" ] ; then
  # The nova user does not have any roles yet.
  echo "# Add role admin to id nova"
  keystone user-role-add --user=nova --tenant=service --role=admin
fi

set_service_conffile_auth /etc/nova/nova.conf keystone_authtoken ${CLOUD_EXT_CONTROL_HOSTNAME} nova ${ID_COMPUTE_PW}

# Make sure we are using the paste file.
openstack-config --set /etc/nova/nova.conf DEFAULT api_paste_config /etc/nova/api-paste.ini

set_service_pastefile_auth /etc/nova/api-paste.ini ${CLOUD_EXT_CONTROL_HOSTNAME} nova ${ID_COMPUTE_PW} 5000
# For some reason this paste file is also given a 'auth_uri' property
# even though the 'glance' image service did not have it.
grep -e "^auth_uri=" /etc/nova/api-paste.ini > /dev/null
if [ "$?" != "0" ] ; then
  sed -i -e "/^\[filter:authtoken\]/ a\
auth_uri=http://${CLOUD_EXT_CONTROL_HOSTNAME}:5000/v2.0" /etc/nova/api-paste.ini
fi

keystone service-get nova > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Register the cpute service the identity service."
  keystone service-create --name=nova --type=compute --description="Nova Compute Service" > /dev/null
fi

SERVICE_ID=$(keystone service-get nova | grep -e "[[:space:]]id[[:space:]]" | sed -e "s/id//" -e "s/[ |]//g")

echo "# Nova service ID=${SERVICE_ID}"

# Look for an endpoint for the service.
keystone endpoint-list | grep -e "${SERVICE_ID}" > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Creating the Nova endpoint in the id registry"
  keystone endpoint-create --service-id=${SERVICE_ID} --publicurl=http://${NOVA_HOSTNAME}:8774/v2/%\(tenant_id\)s --internalurl=http://${NOVA_HOSTNAME}:8774/v2/%\(tenant_id\)s --adminurl=http://${NOVA_HOSTNAME}:8774/v2/%\(tenant_id\)s > /dev/null
fi

# Configure to use the Qpid message broker.
echo "# Configure to use qpid message service."
openstack-config --set /etc/nova/nova.conf  DEFAULT rpc_backend nova.openstack.common.rpc.impl_qpid
openstack-config --set /etc/nova/nova.conf DEFAULT qpid_hostname ${CLOUD_EXT_CONTROL_HOSTNAME}

echo "# Start the compute services. "
systemctl restart openstack-nova-api
systemctl restart openstack-nova-cert
systemctl restart openstack-nova-consoleauth
systemctl restart openstack-nova-scheduler
systemctl restart openstack-nova-conductor
systemctl restart openstack-nova-novncproxy 
systemctl enable openstack-nova-api
systemctl enable openstack-nova-cert
systemctl enable openstack-nova-consoleauth
systemctl enable openstack-nova-scheduler
systemctl enable openstack-nova-conductor
systemctl enable openstack-nova-novncproxy
