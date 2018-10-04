GHC=ghc
GHCFLAGS=-Wall

all: pegi

pegi: Main.hs SParser.hs FirstGrammar.hs FGParser.hs
	$(GHC) $(GHCFLAGS) -o $@ $^

example1: pegi example1.peg
	./pegi example1.peg

hlint:
	hlint *.hs

clean: 
	rm -f pegi *.hi *.o Makefile.bak 
