#!/bin/bash
# Install the mysql database needed by several of the openstack services.

# Get the generic functions.
TOP_LEVEL=${TOP_LEVEL:-../..}
UTIL_DIR=${UTIL_DIR:-${TOP_LEVEL}/util}
. ${UTIL_DIR}/fns

# Make sure we are the 'root' user.
check_root_user

source_file ${UTIL_DIR}/network-fns
source_file ${UTIL_DIR}/params-network

# load the passwords file.
source_file ${UTIL_DIR}/passwd


#
# At ITTC We have our 'mysql' user in LDAP. This causes a problem since at
# boot time the mysql configuration file wants to create the /var/run/mysql
# directory with owner:group being mysql:mysql
# So we need to put mysql in the local group and password files.
MYSQL_GROUP_ENTRY=$(getent group mysql)
if [ "$?" == "0" ] ; then
  echo "# We have a mysql group from LDCAP. We need to have it local."
  # put the mysql group in the loca group file if it is not already there.
  grep -e "^mysql:" /etc/group > /dev/null
  if [ "$?" != "0" ] ; then
    echo "# Put mysql into group file."
    echo ${MYSQL_GROUP_ENTRY} >> /etc/group
  else
    echo "# It is already local. Do nothing." 
  fi
fi
MYSQL_USER_ENTRY=$(getent passwd mysql)
if [ "$?" == "0" ] ; then
  echo "# We have a mysql user from LDCAP. We need to have it local."
  # put the mysql user in the local passwd file if it is not already there.
  grep -e "^mysql:" /etc/passwd > /dev/null
  if [ "$?" != "0" ] ; then
    echo "# Put mysql into passwd file."
    echo ${MYSQL_USER_ENTRY} >> /etc/passwd
  else
    echo "# It is already local. Do nothing." 
  fi
fi


# Install the MySQL package.
echo 
yum -y install mysql-server MySQL-python || exit 1
if [ "$?" != "0" ] ; then
  echo "There was a problem installing the MySQL package."
  exit 1;
fi

# We use the 'expect' package to help with the 'mysql_secure_installation' script.
yum -y install expect
if [ "$?" != "0" ] ; then
  echo "There was a problem installing the expect package."
  exit 1;
fi

# Now add the binding address for our openstack hostname
grep -e "bind-address=" /etc/my.cnf.d/server.cnf > /dev/null
if [ "$?" == "0" ] ; then
  # we already have a binding address, lets change it to the current parameters.
  echo "Modifying MySQL bind-address config param to ${CLOUD_EXT_CONTROL_HOSTNAME}.${CLOUD_EXT_DOMAIN}"
  sed -i -e "/bind-address=/s/=.*$/=${CLOUD_EXT_CONTROL_HOSTNAME}.${CLOUD_EXT_DOMAIN}/" /etc/my.cnf.d/server.cnf
else
  echo "Adding the MySQL bind-address config param with ${CLOUD_EXT_CONTROL_HOSTNAME}.${CLOUD_EXT_DOMAIN}."
  sed -i -e "/^\[mysqld\]/a\
bind-address=${CLOUD_EXT_CONTROL_HOSTNAME}.${CLOUD_EXT_DOMAIN}" /etc/my.cnf.d/server.cnf
fi

# Start the mysql server and make it start on boot.
echo "Startin MySQL server."
systemctl start mysqld.service
systemctl enable mysqld.service


# See if mysql is setup without a password
# The following command should return success if no root password is set yet.
mysql --user=root --password= --execute= >/dev/null
if [ "$?" == "0" ] ; then
  # MySQL has not been setup yet so do it.
  echo "Make the MySQL installation secure."
  SECURE_MYSQL=$(expect -c "
    set timeout 10
    spawn mysql_secure_installation

    expect \"Enter current password for root (enter for none):\"
    send \"\r\"

    expect \"Set root password?\"
    send \"y\r\"

    expect \"New password:\"
    send \"${MYSQL_ROOT_PW}\r\"

    expect \"Re-enter new password:\"
    send \"${MYSQL_ROOT_PW}\r\"

    expect \"Remove anonymous users?\"
    send \"y\r\"

    expect \"Disallow root login remotely?\"
    send \"y\r\"

    expect \"Remove test database and access to it?\"
    send \"y\r\"

    expect \"Reload privilege tables now?\"
    send \"y\r\"

    expect eof
    ")
  SECURE_STATUS=$?
  echo ${SECURE_MYSQL}
  if [ "${SECURE_STATUS}" != 0 ] ; then 
    echo "Setting up Mysql Secure installation failed."
    exit 1
  fi
else
  echo "The MySQL installation already had a root password set so this script is hosed."
  echo "You can start over by executing the following"
  echo "sudo yum autoremove mysql-server"
  echo "sudo rm -rf /etc/my.cnf"
  echo "sudo rm -rf /etc/my.cnf.d"
  echo "sudo rm -rf /var/lib/mysql"
  exit 1
fi


