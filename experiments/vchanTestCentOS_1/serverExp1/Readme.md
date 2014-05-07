The serverExp1 application that is a vchan server generating messages over a xen vchan for a vchan clientExp1 to respond to.

The serverExp1 is told the domainID that clientExp1 is running in by the application mgrExp1 that is run on dom0.

Usage: serverExp1 does not require any arguments. It may be run as any user.

Start serverExp1 on a VM before starting mgrExp1.

Once mgrExp1 has run, serverExp1 will print out the clientExp1 domain ID and then begin sending messages to clientExp1. The messages will just be incrementing integers starting at 1. For each messages the serverExp1 will block waiting for a response from the
clientExp1. When the response is received serverExp1 will print out the response, sleep for 3 seconds and then send subExp1 another message.
