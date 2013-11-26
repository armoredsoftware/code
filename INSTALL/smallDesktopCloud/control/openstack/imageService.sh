#!/bin/bash

# Set up the openstack image service 'glance'.

# Load some common functions
# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }


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


echo "# Setting up the Image service Glance."

# The Glance service uses a unix group and user named 'glance' both with
# the ID of 161. If the group and user do not exist then it tries to create
# them. On our ITTC system we already have a user and group with ID=161 so
# the installation of the Glance does not work properly. To get around
# this is we create the Glance user and group with our own ID.
echo "# Setup the glance user and group."
getent group glance || groupadd -r --gid 9161 glance
getent passwd glance || useradd --uid 9161 -r -g glance -d /var/lib/glance -s /sbin/nologin -c "OpenStack Glance Daemons" glance


echo "# Install the Glance packages"
yum -y install openstack-glance || exit 1

# What host is the mysql database installed on. 
MYSQL_HOSTNAME=${CLOUD_EXT_CONTROL_HOSTNAME}
GLANCE_HOSTNAME=${CLOUD_EXT_CONTROL_HOSTNAME}

echo "# Setup glance configuration file for Mysql."
openstack-config --set /etc/glance/glance-api.conf DEFAULT sql_connection mysql://glance:${IMAGEDB_PW}@${MYSQL_HOSTNAME}/glance
openstack-config --set /etc/glance/glance-registry.conf DEFAULT sql_connection mysql://glance:${IMAGEDB_PW}@${MYSQL_HOSTNAME}/glance

echo "# Setup the glance database on the mysql server."
openstack-db --init --service glance --password ${IMAGEDB_PW} --rootpw ${MYSQL_ROOT_PW}

# Get the keystone.rc source so we have the credentials to use keystone.
source_keystone_rc

EMAIL_ADDRESS=get_email_address

# Make sure that we can talk to keystone.
keystone user-list > /dev/null
if [ "$?" != "0" ] ; then
  echo "!!!!!! Failed to talk to keystone service."
  exit 1;
fi

keystone user-get glance > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create keystone user glance"
  keystone user-create --name=glance --pass=${ID_IMAGE_PW} --email=${EMAIL_ADDRESS}
else
  echo "# Set user keystone glance password."
  keystone  user-password-update  --pass=${ID_IMAGE_PW} glance
fi

keystone user-role-list --user glance --tenant service | grep admin > /dev/null
if [ "$?" != "0" ] ; then
  # The glance user does not have any roles yet.
  echo "# Add role admin to id glance"
  keystone user-role-add --user=glance --tenant=service --role=admin
fi

echo "# Setup the glance configuration file(s) for authentication."
openstack-config --set /etc/glance/glance-api.conf keystone_authtoken auth_host ${CLOUD_PUBLIC_HOSTNAME}
openstack-config --set /etc/glance/glance-api.conf keystone_authtoken admin_user glance
openstack-config --set /etc/glance/glance-api.conf keystone_authtoken admin_tenant_name service
openstack-config --set /etc/glance/glance-api.conf keystone_authtoken admin_password ${ID_IMAGE_PW}
openstack-config --set /etc/glance/glance-registry.conf keystone_authtoken auth_host ${CLOUD_PUBLIC_HOSTNAME}
openstack-config --set /etc/glance/glance-registry.conf keystone_authtoken admin_user glance
openstack-config --set /etc/glance/glance-registry.conf keystone_authtoken admin_tenant_name service
openstack-config --set /etc/glance/glance-registry.conf keystone_authtoken admin_password ${ID_IMAGE_PW}

echo "# Setup the ini files for glance."
# Set up the ini file for the glance api
if [ ! -f /etc/glance/glance-api-paste.ini ] ; then
  cp /usr/share/glance/glance-api-dist-paste.ini /etc/glance/glance-api-paste.ini
fi

set_service_pastefile_auth /etc/glance/glance-api-paste.ini ${CLOUD_EXT_CONTROL_HOSTNAME} glance ${ID_IMAGE_PW}

# Get rid of the value that delays authorization decision
sed -i -e "/^delay_auth_decision[ ]*=[ ]*true/ d" /etc/glance/glance-api-paste.ini

# Set up the ini file for the glance registry
if [ ! -f /etc/glance/glance-registry-paste.ini ] ; then
  cp /usr/share/glance/glance-registry-dist-paste.ini /etc/glance/glance-registry-paste.ini
fi

set_service_pastefile_auth /etc/glance/glance-registry-paste.ini ${CLOUD_EXT_CONTROL_HOSTNAME} glance ${ID_IMAGE_PW}


keystone service-get glance > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Register the image service the identity service."
  keystone service-create --name=glance --type=image --description="Glance Image Service" > /dev/null
fi

SERVICE_ID=$(keystone service-get glance | grep -e "[[:space:]]id[[:space:]]" | sed -e "s/id//" -e "s/[ |]//g")

echo "# Glance service ID=${SERVICE_ID}"

# Look for an endpoint for the service.
keystone endpoint-list | grep -e "${SERVICE_ID}" > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Creating the Glance endpoint in the id registry"
  keystone endpoint-create --service-id=${SERVICE_ID} --publicurl=http://${GLANCE_HOSTNAME}:9292 --internalurl=http://${GLANCE_HOSTNAME}:9292 --adminurl=http://${GLANCE_HOSTNAME}:9292 > /dev/null
fi

echo "# Start the glance service. "
systemctl restart openstack-glance-api
systemctl restart openstack-glance-registry
systemctl enable openstack-glance-api
systemctl enable openstack-glance-registry



