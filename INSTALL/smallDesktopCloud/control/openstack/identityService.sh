#!/bin/bash

# Setup the openstack identiy service.

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
{ . ${UTIL_DIR}/fns ; } || { echo "!!! Failed to source file ${UTIL_DIR}/fns." ; exit 1; }

# Load some networking parameters.
source_file ${UTIL_DIR}/params-network
source_file ${UTIL_DIR}/passwd

# We have to be the root user.
check_root_user

# Check for some prereq's
check_mysql_running
check_message_queue_running
check_openstack_utils

echo "# Setting up the Identity service Keystone."

# The Keystone service uses a unix group and user named 'keystone' both with
# the ID of 163. If the group and user do not exist then it tries to create
# them. On our ITTC system we already have a user and group with ID=163 so
# the installation of the Keystone does not work properly. To get around
# this is we create the keystone user and group with our own ID.
echo "# Setup the keystone user and group."
getent group keystone || groupadd -r --gid 9163 keystone
getent passwd keystone || useradd --uid 9163 -r -g keystone -d /var/lib/keystone -s /sbin/nologin -c "OpenStack Keystone Daemons" keystone

# Install the keystone packages
echo "# Install Keystone packages."
yum -y install openstack-keystone python-keystoneclient || exit 1

# Initialize the keystone database.
echo "# Initialize the keystone database."
openstack-config --set /etc/keystone/keystone.conf sql connection mysql://keystone:${KEYSTONEDB_PW}@${CLOUD_EXT_CONTROL_HOSTNAME}/keystone
if [ "$?" != "0" ] ; then
  exit 1
fi
openstack-db --init --service keystone --password ${KEYSTONEDB_PW} --rootpw ${MYSQL_ROOT_PW}
echo "openstack-db return=$?"
#if [ "$?" != "0" ] ; then
#  exit 1
#fi

# Setup the keystone authentication token
grep -e "^admin_token" /etc/keystone/keystone.conf > /dev/null
if [ "$?" != "0" ] ; then
  # The admin token has not been set yet.
  echo "Setup the keystone authentication token."
  ADMIN_TOKEN=$(openssl rand -hex 10)
  openstack-config --set /etc/keystone/keystone.conf DEFAULT admin_token $ADMIN_TOKEN
else 
  # Get the admin token
  ADMIN_TOKEN=$(grep -e "^admin_token" /etc/keystone/keystone.conf | sed -e "s/^.*= *//")
fi
export ADMIN_TOKEN
echo "# Keystone admin_token is ${ADMIN_TOKEN}"

echo "# Setup the PKI tokens"
keystone-manage pki_setup --keystone-user keystone --keystone-group keystone
chown -R keystone:keystone /etc/keystone/* /var/log/keystone/keystone.log

# Start the keytone identify service
echo "# Start up the keystone identity service."
systemctl restart openstack-keystone.service
systemctl enable openstack-keystone.service

# Set up some environment variables so we don't have to give the admin token
# and end point all of the time.
export OS_SERVICE_TOKEN=${ADMIN_TOKEN}
export OS_SERVICE_ENDPOINT=http://${CLOUD_EXT_CONTROL_HOSTNAME}:35357/v2.0

echo "# Create tenants for admin and service."
keystone tenant-get admin > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create tenant admin."
  keystone tenant-create --name=admin --description="Admin Tenant"
fi
keystone tenant-get service > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create tenant service."
  keystone tenant-create --name=service --description="Service Tenant"
fi

EMAIL_ADDRESS=$(get_email_address)
echo "# Create admin user, email address=${EMAIL_ADDRESS}"
keystone user-get admin > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Create user admin"
  keystone user-create --name=admin --pass=${ID_ADMIN_PW} --email=${EMAIL_ADDRESS}
else
  echo "# Set user admin password."
  keystone  user-password-update  --pass=${ID_ADMIN_PW} admin
fi

# Add the admin role if it has not already been created.
keystone role-get admin > /dev/null
if [ "$?" != 0 ] ; then
  echo "Create admin role".
  keystone role-create --name=admin
fi

# Add the admin role to the user if it has not already been added.
keystone user-role-list --user admin --tenant admin | grep admin > /dev/null
if [ "$?" != 0 ] ; then
  # the admin user does not have any roles yet.
  echo "# Admin admin role to admin user for admin tenant."
  keystone user-role-add --user=admin --tenant=admin --role=admin
fi

keystone service-get keystone > /dev/null
if [ "$?" != "0" ] ; then
  echo "# Add Key service to the service directory."
  keystone service-create --name=keystone --type=identity --description="Keystone Identity Service" > /dev/null
fi
SERVICE_ID=$(keystone service-get keystone | grep -e "[[:space:]]id[[:space:]]" | sed -e "s/id//" -e "s/[ |]//g")

echo "# Keystone Service ID=${SERVICE_ID}"

#keystone endpoint-get 
#keystone endpoint-list

AUTHURL=http://${CLOUD_EXT_CONTROL_HOSTNAME}:35357/v2.0

# Look for an endpoint for the service.
keystone endpoint-list | grep -e "${SERVICE_ID}"
if [ "$?" != "0" ] ; then
  echo "# Adding endpoint for Keystone Service"
  keystone endpoint-create   --service-id=${SERVICE_ID}   --publicurl=http://${CLOUD_EXT_CONTROL_HOSTNAME}:5000/v2.0   --internalurl=http://${CLOUD_EXT_CONTROL_HOSTNAME}:5000/v2.0   --adminurl=${AUTHURL} 
fi

# Unset the ENV variables for bypassing authorization. This is needed
# to get a token.
OS_SERVICE_TOKEN=
OS_SERVICE_ENDPOINT=

echo "Test getting a token from keystone."
keystone --os-username=admin --os-password=${ID_ADMIN_PW} --os-auth-url=${AUTHURL} token-get > /dev/null
if [ "$?" != "0" ] ; then
  echo "!!!!!! Failed to get a token from keystone."
  echo "EXITING!!!!"
  exit 1
fi

# Now create an RC file with the keystone credentials setup so that user
# can 'source' the file then be able to use keystone.
if [ "${SUDO_USER}" == "" ] ; then
  # We are running as root
  SUDO_OPTS=
else
  SUDO_OPTS="sudo -u ${SUDO_USER}"
fi
echo "# Working on the keystone.rc file at ${TOP_LEVEL}/.keystone.rc."
if [ -f ${TOP_LEVEL}/.keystone.rc ] ; then
  # get rid of the old file.
  echo "# Removing old keystone.rc file."
  ${SUDO_OPTS} rm -f ${TOP_LEVEL}/.keystone.rc
  if [ "$?" != "0" ] ; then
    echo "!!!! We need permissions to remove file ${TOP_LEVEL}/.keystone.rc."
    echo "EXITING!!!!"
    exit 1
  fi
fi
echo "# Creating new keystone.rc file"
${SUDO_OPTS} echo "# ENV variables for keystone cmd." > ${TOP_LEVEL}/.keystone.rc
if [ "$?" != "0" ] ; then
    echo "We need permissions to create file ${TOP_LEVEL}/.keystone.rc."
    echo "EXITING!!!!"
    exit 1
fi
${SUDO_OPTS} sed -i -e "$ a\
export OS_USERNAME=admin" ${TOP_LEVEL}/.keystone.rc
${SUDO_OPTS} sed -i -e "$ a\
export OS_PASSWORD=${ID_ADMIN_PW}" ${TOP_LEVEL}/.keystone.rc
${SUDO_OPTS} sed -i -e "$ a\
export OS_TENANT_NAME=admin" ${TOP_LEVEL}/.keystone.rc
${SUDO_OPTS} sed -i -e "$ a\
export OS_AUTH_URL=http://${CLOUD_EXT_CONTROL_HOSTNAME}:35357/v2.0"  ${TOP_LEVEL}/.keystone.rc

# Now make it so armored group users can read and remove it.
${SUDO_OPTS} chgrp armoredsoftware ${TOP_LEVEL}/.keystone.rc
${SUDO_OPTS} chmod g+w ${TOP_LEVEL}/.keystone.rc
