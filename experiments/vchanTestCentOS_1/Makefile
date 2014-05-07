#
#
#
EXP_ROOT = $(CURDIR)
include Rules.mk

SUBDIRS-y :=
SUBDIRS-y += mgrExp1 serverExp1 clientExp1

.PHONY: all
all: subdirs-all

clean: subdirs-clean

clobber: clean
	-rm bin/*