include ../config

define find_rec
	$(shell find * -name "$(1)")
endef

OBJ = logger.cmo \
utils.cmo \
base64.cmo \
sha1.cmo \
ws.cmo \
http.cmo

all: websocketml.cma

websocketml.cma: $(OBJ) websocketml.cmi
	ocamlc -a -o $@ $(OBJ) websocketml.ml

clean:
	@rm -f $(call find_rec,*.annot) $(call find_rec,*.cm[iox])
	@rm -f .depend

cleanall realclean mrproper: clean

.depend:
	@$(OCAMLDEP) $(INCLUDES) $(call find_rec,*.ml) $(call find_rec,*.mli) \
		> .depend

-include .depend
