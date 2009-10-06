include ../support/include.mk

all: $(EBIN_FILES_NO_DOCS)

generate: $(EBIN_GENERATE)
	erl -noshell -pa ../ebin -s record_helper make ../include/messages.hrl ../include/types.hrl -s init stop
debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES_NO_DOCS)
