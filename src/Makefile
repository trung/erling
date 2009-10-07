include ../support/include.mk

ERL_GENERATED_SOURCES := ../include/messages.hrl ../include/types.hrl

all: $(EBIN_FILES_NO_DOCS)

generate: record_utils.erl

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES_NO_DOCS)

record_utils.erl: $(EBIN_DIR)/record_helper.$(EMULATOR) $(ERL_GENERATED_SOURCES)
	$(ERL) -noshell -pa $(EBIN_DIR) -s record_helper make $(ERL_GENERATED_SOURCES) -s init stop

$(ERL_GENERATED_SOURCES):
	@echo "Nothing"