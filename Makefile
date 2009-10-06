all:
	(cd src;$(MAKE) generate)
	(cd src;$(MAKE))
	(cd test;$(MAKE))

tests: all
	@for f in $(patsubst ebin/%.beam,%,$(wildcard ebin/*_test.beam)) ; do echo "++++++ Running $$f ++++++" ; erl -noshell -pa ebin -s $$f test -s init stop ; done

test: all
   ifdef m
	@echo "++++++ Running $(m)_test ++++++"
	@erl -noshell -pa ebin -s $(m)_test test -s init stop
   else
	@echo "Please specify module for test (m=xxx)"
   endif

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'BeepBeep'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)