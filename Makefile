HS_FILES = $(wildcard *.hs)
DOC_DIR = doc

.PHONY: all clean doc docdir test

all: $(HS_FILES)
	ghc $(HS_FILES)

doc: $(HS_FILES) docdir
	haddock -o $(DOC_DIR) --html $(HS_FILES)

docdir:
	mkdir -p $(DOC_DIR)

test: clean
	for f in $(HS_FILES); do \
		echo "Testing $$f" ; \
		doctest $$f || exit 1 ; \
	done

clean:
	rm -f *.o *.hi
	rm -rf doc
