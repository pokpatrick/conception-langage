CC = ocamlc
FLAG = -g -c
YACC = ocamlyacc
LEX = ocamllex
TARGS = logo parser

all: $(TARGS)

parser : ast.ml lexer.mll parser.mly
	$(CC) $(FLAG) ast.ml
	${LEX} lexer.mll
	${YACC} -v parser.mly
	${CC} ${FLAG} parser.mli
	${CC} ${FLAG} lexer.ml
	$(CC) ${FLAG} parser.ml 

SEMANTIC = ast.cmo memory.cmo continuation.cmo environnement.cmo parser.cmo lexer.cmo semantic.cmo

logo: parser ${SEMANTIC} logo.ml
	ocamlc -o logo ${SEMANTIC} graphics.cma logo.ml

clean :
	rm -f lexer.ml parser.mli parser.ml
	rm -f *~ *.cmi *.cmo *.output

.SUFFIXES : .mll .mly .mli .cmi .ml .cmo

.mly.ml : 
	$(YACC) $<

.mly.mli : 
	$(YACC) $<

.mll.ml : 
	$(LEX) $<

.ml.cmi : 
	$(CC) $(FLAG) $<

.mli.cmi : 
	$(CC) $(FLAG) $<

.ml.cmo : 
	$(CC) $(FLAG) $<
