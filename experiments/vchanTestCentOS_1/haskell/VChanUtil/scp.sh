#!/bin/bash
ip1=10.100.0.218
ip2=10.100.0.236

scp JVChanUtil.class libJVChanUtil.so root@$ip1: ;
scp JVChanUtil.class libJVChanUtil.so root@$ip2: ;

if [ "$1" == "modprobe" ]; then
  ssh root@$ip1 'modprobe xen_gntalloc;modprobe xen_evtchn;modprobe xen_gntdev;'
  ssh root@$ip2 'modprobe xen_gntalloc;modprobe xen_evtchn;modprobe xen_gntdev;'
fi
