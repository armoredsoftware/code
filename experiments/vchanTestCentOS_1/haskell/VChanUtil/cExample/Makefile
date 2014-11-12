#
# Compile and link the serverExp1 application.
#


##########################################################

APP_OBJS = main.o

CFLAGS = -I../ -I.

LDFLAGS = -L/usr/lib64

LDLIBS = -lxenvchan -lxenctrl

CC = gcc
APP = clientExp1

###########################################################

.PHONY: all

$(APP): $(APP_OBJS) ../common.o
	$(CC) ../common.c $(LDFLAGS) -o $@ $(APP_OBJS) $(CFLAGS) $(LDLIBS)

##########################################################


.PHONY: clean
clean:
	-rm *~
	-rm $(APP_OBJS)

.PHONY: clobber
clobber: clean
	-rm $(APP)
