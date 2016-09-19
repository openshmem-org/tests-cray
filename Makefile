### A minimal top level Makefile
#
# Copyright (c) 2015 Cray Inc.
#
# The contents of this file is proprietary information of Cray Inc.
# and may not be disclosed without prior written consent.
#

.PHONY: all sma1 sma2 smaf

all: sma1 sma2 smaf

sma1:
	$(MAKE) -C sma1 all

sma2:
	$(MAKE) -C sma2 all

smaf:
	$(MAKE) -C smaf all

clean:
	rm -rf testbin/
