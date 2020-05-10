.PHONY : all clean

# Some constants
GRAMA_FOLDER = grama_fld
SRC_FOLDER = src
PARSER_FILES = $(GRAMA_FOLDER)/ErrM.hs $(GRAMA_FOLDER)/LexGramatyka.hs $(GRAMA_FOLDER)/ParGramatyka.hs $(GRAMA_FOLDER)/PrintGramatyka.hs
# Default goal.
all : interpret

# Rules for building the parser.
%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

generate_grama:
	bnfc -m gramatyka -o $(GRAMA_FOLDER)

# Rules for interpreter
interpret : generate_grama $(PARSER_FILES)
	ghc -i$(SRC_FOLDER):$(GRAMA_FOLDER) --make $(SRC_FOLDER)/interpret.hs $(SRC_FOLDER)/programTypes.hs $(SRC_FOLDER)/static_typing.hs $(SRC_FOLDER)/semantics.hs $(PARSER_FILES) -o $@

# Rules for cleaning generated files.
clean :
	-rm -rf $(GRAMA_FOLDER)
	-rm -f $(SRC_FOLDER)/*.hi $(SRC_FOLDER)/*.o $(SRC_FOLDER)/*.log $(SRC_FOLDER)/*.aux $(SRC_FOLDER)/*.dvi
	-rm -f interpret
