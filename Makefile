all:
	(cd src;$(MAKE))
	(cd test;$(MAKE))

test: all
	$(foreach var, $(patsubst ebin/%.beam,%,$(wildcard ebin/*_test.beam)), erl -noshell -pa ebin -s $(var) test -s init stop)

docs:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application "'BeepBeep'" '"."' '[no_packages]'

clean:
	(cd src;$(MAKE) clean)