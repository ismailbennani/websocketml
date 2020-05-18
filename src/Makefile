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

all: websocketml.cma websocketml.cmxa

websocketml.cma: $(OBJ) websocketml.cmi
	ocamlc -a -o $@ $(OBJ) websocketml.ml

websocketml.cmxa: $(OBJ:.cmo=.cmx) websocketml.cmi
	ocamlopt -a -o $@ $(OBJ:.cmo=.cmx) websocketml.ml

clean:
	@rm -f $(call find_rec,*.annot) $(call find_rec,*.cm[iox])
	@rm -f .depend

cleanall realclean mrproper: clean
	rm -f websocketml.cma

.depend:
	@$(OCAMLDEP) $(INCLUDES) $(call find_rec,*.ml) $(call find_rec,*.mli) \
		> .depend

-include .depend
