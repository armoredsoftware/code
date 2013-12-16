#!/bin/bash
# Setup the networking for our router host.
# Assume that we are starting with a fresh ITTC Fedorda 19 installation
# We assume that there are 2 NICs on the control host. See the
# file 'params-network.sh' and set up the parameters appropriately.
# General setup:
# - Use parameters from the file './params-network'.
# - Set up dnsmasq to be DNS and DHCP for a router.

# Load some common functions
TOP_LEVEL=${TOP_LEVEL:-..}
UTIL_DIR=${TOP_LEVEL}/util
. ${UTIL_DIR}/fns

source_file ${UTIL_DIR}/network-fns

# Are we the root user?
check_root_user

# See if our network parameters file is present
PARAMS_NETWORK=${UTIL_DIR}/params-network
if [ ! -f ${PARAMS_NETWORK} ] ; then
echo "!!! The '${PARAMS_NETWORK}' file for defining the network parameters is missing."
exit 1
fi
source_file ${PARAMS_NETWORK}

# Make sure that the network devices specified in the param-network.sh
# really exist.
network_device_must_exist ITTC_NET_DEVICE
network_device_must_exist CLOUD_EXT_ROUTER_DEVICE

# We do not want the ITTC DHCP client to over ride /etc/resolv.conf
# since we are going to add our own DNS server to /etc/resolv.conf so
# tell the ITTC_NET_DEVICE config file.
ITTC_NET_DEV_CONFIG=/etc/sysconfig/network-scripts/ifcfg-$ITTC_NET_DEVICE
echo "# Modifying '${ITTC_NET_DEVICE}' configuration file."
sed -i -e "/PEERDNS/s/=[ ]*yes/=no/" -e "/PEERDNS/s/#//g" ${ITTC_NET_DEV_CONFIG}
#sed -i -e "/IPV6_PEERDNS/s/-[ ]*yes/=no/" -e "/IPV6_PEERDNS/s/#//g" ${ITTC_NET_DEV_CONFIG}

# Setup the cloud external network device.
echo "# Modifying '$CLOUD_EXT_ROUTER_DEVICE' configuration file."
NET_CFG_FILE=/etc/sysconfig/network-scripts/ifcfg-${CLOUD_EXT_ROUTER_DEVICE}
sed -i -e "/BOOTPROTO/s/=.*/=static/"  $NET_CFG_FILE
# Set IP Address
grep -e "IPADDR" $NET_CFG_FILE > /dev/null
if [ "$?" != "0" ] ; then
  # The IPADDR is not in the file yet so add it.
  sed -i -e "/BOOTPROTO/a\
IPADDR=${CLOUD_EXT_ROUTER_IPADDR}" $NET_CFG_FILE
else
  # It already had IPADDR specified so replace it.
  sed -i -e "/IPADDR/s/=.*$/=${CLOUD_EXT_ROUTER_IPADDR}/" -e "/IPADDR/s/#//g" $NET_CFG_FILE
fi 
# Set Netmask
grep NETMASK $NET_CFG_FILE > /dev/null
if [ "$?" != "0" ] ; then
  # The NETMASK is not in the file yet so add it.
  sed -i -e "/NETMASK/a\
NETMASK=${CLOUD_EXT_NETMASK}" $NET_CFG_FILE
else
  # It already had NETMASK specified so replace it.
  sed -i -e "/NETMASK/s/=.*/=${CLOUD_EXT_NETMASK}/" -e "/NETMASK/s/#//g" $NET_CFG_FILE
fi 

# This is not our default route.
sed -i -e /DEFROUTE/s/yes/no/ $NET_CFG_FILE
sed -i -e /IPV6_DEFROUTE/s/yes/no/ /$NET_CFG_FILE
sed -i -e /PEERDNS/s/=.*$/=no/ $NET_CFG_FILE
sed -i -e /IPV6_PEERDNS/s/=.*$/=no/ $NET_CFG_FILE
sed -i -e /PEERROUTES/s/=.*$/=no/ $NET_CFG_FILE
sed -i -e /IPV6_PEERDNS/s/=.*$/=no/ $NET_CFG_FILE

# Set the device name
# Set Netmask
grep 'DEVICE=' $NET_CFG_FILE > /dev/null
if [ "$?" != "0" ] ; then
  # The DEVICE is not in the file yet so add it.
  sed -i -e "/NAME/a\
DEVICE=${CLOUD_EXT_DEVICE}" $NET_CFG_FILE
else
  # It already had DEVICE specified so replace it.
  sed -i -e "/DEVICE/s/=.*$/=${CLOUD_EXT_ROUTER_DEVICE}/" -e "/DEVICE/s/#//g" $NET_CFG_FILE
fi 

# We want this device to start on boot.
sed -i "/ONBOOT/s/=.*$/=yes/" $NET_CFG_FILE

# Now start up the cloud external interface
echo "# Starting up network interface ${CLOUD_EXT_ROUTER_DEVICE}."
ifup ${CLOUD_EXT_ROUTER_DEVICE}
if [ "$?" != "0" ] ; then
  echo "!!! Starting up network interface '${CLOUD_EXT_ROUTER_DEVICE}' failed."
  exit 1
fi

# We want forwarding of IPv4 packets for out router NAT
echo "# Setup IPv4 forwarding."
cp -f armored_network.conf /etc/sysctl.d
sysctl -p /etc/sysctl.d/armored_network.conf > /dev/null

# Enable NAT masquerating to the ITTC network device.
echo "# Setup NAT for ${CLOUD_EXT_ROUTER_DEVICE} to ${ITTC_NET_DEVICE}."
systemctl start iptables.service
iptables -t nat -A POSTROUTING -o ${ITTC_NET_DEVICE} -j MASQUERADE
iptables --append FORWARD --in-interface ${CLOUD_EXT_ROUTER_DEVICE} -j ACCEPT
service iptables save
systemctl enable iptables.service > /dev/null

# Put the dnsmasq configuration for the router host into the dnsmasq config
# directory
echo "# Create file /etc/dnsmasq.d/armored_dnsmasq.d"
sed -e /CLOUD_EXT_ROUTER_DEVICE/s/CLOUD_EXT_ROUTER_DEVICE/${CLOUD_EXT_ROUTER_DEVICE}/g -e /CLOUD_EXT_ROUTER_IPADDR/s/CLOUD_EXT_ROUTER_IPADDR/${CLOUD_EXT_ROUTER_IPADDR}/g -e /CLOUD_EXT_DOMAIN/s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/g ./armored_dnsmasq.d > /etc/dnsmasq.d/armored_dnsmasq.d

# Make sure the the 'control' ip and the 'networking' ipaddresses are different.
if [ "${CLOUD_EXT_CONTROL_IPADDR}" == "${CLOUD_EXT_NETWORKING_IPADDR}" ] ; then 
  echo "The CONTROL node and the NETWORKING node are not allowed to have the same IP address (${CLOUD_EXT_CONTROL_IPADDR})."
  exit 1
fi

# Create the hosts file used by dnsmask DNS.
echo "# Create file /etc/hosts-armored"
rm -f /etc/hosts-armored
# If the ip addres for router and control are the same....
# Add the controller and  router separately.
echo "${CLOUD_EXT_ROUTER_IPADDR}     ${CLOUD_EXT_ROUTER_HOSTNAME}.${CLOUD_EXT_DOMAIN}     ${CLOUD_EXT_ROUTER_HOSTNAME}" >> /etc/hosts-armored 
echo "${CLOUD_EXT_CONTROL_IPADDR}     ${CLOUD_EXT_CONTROL_HOSTNAME}.${CLOUD_EXT_DOMAIN}     ${CLOUD_EXT_CONTROL_HOSTNAME}" >> /etc/hosts-armored 
# Add networking ext address.
echo "${CLOUD_EXT_NETWORKING_IPADDR}     ${CLOUD_EXT_NETWORKING_HOSTNAME}.${CLOUD_EXT_DOMAIN}     ${CLOUD_EXT_NETWORKING_HOSTNAME}" >> /etc/hosts-armored
# Add addresses for the compute hosts.
for (( i = 1; i <= ${CLOUD_NUM_COMPUTE_NODES} ;  i++ )) ; do
  COMPUTE_IPADDR=${CLOUD_DATA_COMPUTE_IPADDR_PREFIX}.$i
  echo "${COMPUTE_IPADDR}     ${CLOUD_EXT_COMPUTE_HOSTNAME_PREFIX}${i}.${CLOUD_EXT_DOMAIN}     ${CLOUD_EXT_COMPUTE_HOSTNAME_PREFIX}${i}" >> /etc/hosts-armored
done


#sed -e s/CLOUD_EXT_ROUTER_IPADDR/${CLOUD_EXT_ROUTER_IPADDR}/g -e s/CLOUD_EXT_ROUTER_HOSTNAME/${CLOUD_EXT_ROUTER_HOSTNAME}/g -e s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/g -e s/CLOUD_EXT_NETWORKING_IPADDR/${CLOUD_EXT_NETWORKING_IPADDR}/g -e s/CLOUD_EXT_NETWORKING_HOSTNAME/${CLOUD_EXT_NETWORKING_HOSTNAME}/g ./hosts-armored >/etc/hosts-armored

# Create the hosts file used by dnsmask for DHCP.
#echo "# Create file /etc/dnsmasq.d/armored-dhcp-hosts.d"
#sed -e s/CLOUD_EXT_ROUTER_IPADDR/${CLOUD_EXT_ROUTER_IPADDR}/g -e s/CLOUD_EXT_ROUTER_HOSTNAME/${CLOUD_EXT_ROUTER_HOSTNAME}/g -e s/CLOUD_EXT_DOMAIN/${CLOUD_EXT_DOMAIN}/ ./armored-dhcp-hosts.d >/etc/dnsmasq.d/armored-dhcp-hosts.d

# We need to disable puppet agent so that it will not overright some of
# our files like /etc/resolv.conf and /etc/sudoers
systemctl stop puppetagent.service
systemctl disable puppetagent.service

# We want to have our own name server added to the resolv.conf file.
# See if it is already there.
grep ${CLOUD_EXT_ROUTER_IPADDR} /etc/resolv.conf > /dev/null
if [ "$?" != "0" ] ; then
  # The server address is not in the file yet so add it.
  sed -i "1i\
nameserver       ${CLOUD_EXT_ROUTER_IPADDR}" /etc/resolv.conf
fi

# Have the dnsmasq startup now and on boot.
systemctl restart dnsmasq.service
systemctl enable dnsmasq.service
