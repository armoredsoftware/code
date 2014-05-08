Test the Xen VChan library using dom0 and two CentOS6.5 Virtual Machines. One
VM is for a vchan server and one for a vchan client.

- dom0 is used to launch the VMs and then tell the VMs about each other.
- server VM is a CentOS VM that originates messages.
- client VM is a CentOS VM that sends response messages back to the server
  VM after it receives a messasge from the server VM.
- All communications, dom0-server dom0-client and server-client, use
  xen libvchan library.

- An application is built for dom0 that is named mgrExp1.
- An application is built for the server VM that is named serverExp1.
- An application is built for the client VM that is named clientExp1.

# Preparation #

Prepare the guest Virtual Machians and test applications.

For the purposes of this description the compute
node shall be named 'computeX'. In actual use the 'computeX' would be the
name of an actual compute node.

* Log into a compute node. This is done from a router node.
  * NOTE: When you login into computeX are in dom0.
* Compile the experiement applications.
  * change directory to this directory (directory with this README).
  * > make
* Create two VMs. One for the server and one for the client.
  * On your compute node:
    * > cd code/INSTALL/smallDesktopCloud/compute/xen
    * > sudo buildlargecentos65pvm.sh
    * > cd /xenImages
    * > sudo cp centos65x86_64_run.cfg centos65_client.cfg
    * > sudo mv centos65x86_64_run.cfg centos65_server.cfg
    * > sudo cp CentOS_65_server_PVH.img CentOS_65_client_PVH.img 
    * edit centos65_client.cfg and replace the 'name=' value with "CentOS65_client"
    * edit centos65_server.cfg and replace the 'name=' value with "CentOS65_server"
      * also in the value of the 'disk=' attribute, replace the text 'server' with 'client'.
* Start both VMs
  * > cd /xenImages
  * > sudo xl create centos65_server.cfg -c
  * > sudo xl create centos65_client.cfg -c

* yum repositories must be setup for each VM.
  * determine the IP address of each VM
    * use the console of the VM to login 
    * use 'ifconfig' command to dermine the IP address of the VM.
* On dom0 execute the shell command vmYumSetup.sh for both the server and client VM. 
  * Note: for the following command you'll have to type in the root user password for each repository description copied to the VM.
  * > ./vmYumSetup.sh <VM IP address>
* Get the required rpms for the VMs
  * For both server and client VMs
    * login into VM console
      * > sudo xl console <domId>
    * Execute the following command:
    * > cd ~
    * > rpm -i epel-release-6-8.noarch.rpm
    * > yum -y install xen-devel-4.2.2
    * > yum -y install xen-runtime-4.2.2
    * > reboot
      * Note: this reboot will change the domID and network IP address of the VM.
* Some kernel modules need to be running on the VMs for libvchan to work.
  * On each VM do the following:
    * > modprobe xen_gntalloc
    * > modprobe xen_evtchn # really only needed if doing just vchan server side
    * > modprobe xen_gntdev # really only neede if doing just vchan client side.
* cd to the ./bin directory (dom0)
  * For the server: copy the serverExp1 to the server VM.
    * > scp serverExp1 root@<server VM IP address>:
  * For the clien: copy the clientExp1 to the client VM.
    * > scp clientExp1 root@<client VM IP address>:
* On serverExp1 VM console:
  * > ./serverExp1
* On clientExp1 VM console:
  * > ./clientExp1
* On dom0:
  * > sudo ./mgrExp1 <server domId> <client domId>
* Watch the serverExp1 console to see message ids sent to client and those recieved
back from client as responses.
* Watch the clientExp1 console to see messages ids received from server and responses
sent back to server.
