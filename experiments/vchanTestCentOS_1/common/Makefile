#
# Compile and link the serverExp1 application.
#

EXP_ROOT = $(CURDIR)/..
include $(EXP_ROOT)/Rules.mk

##########################################################

APP_OBJS = common.o

CFLAGS += -I../include -I.

LDFLAGS = -L/usr/lib64

LDLIBS = -lxenvchan -lxenctrl

$(APP_OBJS) : CFLAGS += $(CFLAGS_libxenctrl)

APP = common

###########################################################

.PHONY: all
all: $(EXP_ROOT)/bin/$(APP)


$(APP): $(APP_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(APP_OBJS) $(LDLIBS) $(APPEND_LDFLAGS)

$(EXP_ROOT)/bin/$(APP): $(APP)
	cp $< $@

##########################################################


.PHONY: clean
clean:
	-rm *~
	-rm $(APP_OBJS)

.PHONY: clobber
clobber: clean
	-rm $(APP)
