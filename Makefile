all:
	(cd src;$(MAKE))

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'BeepBeep'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)