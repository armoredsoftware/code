#
# Compile and link the serverExp1 application.
#

EXP_ROOT = $(CURDIR)/..
include $(EXP_ROOT)/Rules.mk

##########################################################

APP_OBJS = main.o

CFLAGS += -I../include -I.

LDFLAGS = -L/usr/lib64

LDLIBS = -lxenvchan -lxenctrl

$(APP_OBJS) : CFLAGS += $(CFLAGS_libxenctrl)

APP = clientExp1

###########################################################

.PHONY: all
all: $(EXP_ROOT)/bin/$(APP)


$(APP): $(APP_OBJS) $(EXP_ROOT)/common/common.o
	$(CC) $(EXP_ROOT)/common/common.c $(LDFLAGS) -o $@ $(APP_OBJS) $(LDLIBS) $(APPEND_LDFLAGS)

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
