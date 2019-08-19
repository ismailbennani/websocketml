include ../config

define find_rec
	$(shell find * -name "$(1)")
endef

INCLUDES += unix.cma str.cma -I ../lib websocketml.cma

BYTES = test_base64.byte test_sha1.byte

all: $(BYTES)

%.byte: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) $<

run: all
	@$(eval TESTS := $(call find_rec,*.byte))
	@$(eval ntests := $(words $(TESTS)))
	@\
	COUNTER=1; \
	echo; \
	echo Number of tests : $(ntests); \
	echo; \
	for test in $(TESTS); do \
		echo ---------- Test $$COUNTER/$(ntests) : $$test ; \
		./$$test ; \
		if [ $$? -eq 0 ] ; then echo --- OK ; else echo NOT OK ; echo exiting ... ; break ; fi ; \
		echo ; \
		COUNTER=$$(($$COUNTER+1)) ; \
	done
	@echo Passed all tests

clean:
	@rm -f $(call find_rec,*.annot) $(call find_rec,*.cm[iox])
	@rm -f $(call find_rec,*.byte)
	@rm -f .depend

cleanall realclean mrproper: clean

.depend:
	@$(OCAMLDEP) $(INCLUDES) $(call find_rec,*.ml) $(call find_rec,*.mli) \
		> .depend

-include .depend
