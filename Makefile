### A minimal top level Makefile

all:
	$(MAKE) -C sma1 all
	$(MAKE) -C sma2 all
	$(MAKE) -C smaf all

clean:
	$(MAKE) -C sma1 clean
	$(MAKE) -C sma2 clean
	$(MAKE) -C smaf clean
