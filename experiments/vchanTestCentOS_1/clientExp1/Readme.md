The masterExp1 application that is a vchan server generating messages over a xen vchan for a vchan client (subExp1) to respond to.

The masterExp1 is told the domainID that subExp1 is running in by the application mgrExp1 that is run on dom0.

Usage: masterExp1 does not require any arguments. It may be run as any user.

Start masterExp1 on a VM before starting subExp1.

Once mgrExp1 has run masterExp1 will print out the subExp1 domain ID and then begin sending messages to subExp1. The messages will just be incrementing integers starting at 1. For each messages the masterExp1 will block waiting for a response from the subExp1. When the response is received masterExp1 will print out the response, sleep for 3 seconds and then send subExp1 another message.
