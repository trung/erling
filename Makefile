all:
	(cd src;$(MAKE))
	(cd test;$(MAKE))

test: all
	for f in $(patsubst ebin/%.beam,%,$(wildcard ebin/*_test.beam)) ; do echo "Running $$f" ; erl -noshell -pa ebin -s $$f test -s init stop ; done

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'BeepBeep'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)