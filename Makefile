HC=ghc
SOURCES=lab2/Main.hs
GEN_SOURCES=lab2/Lex.x lab2/Synt.y
GENERATED=lab2/Lex.hs lab2/Synt.hs
PACKAGE=lab.zip

.PHONY: pack all run clean

all: solution

run: solution
	./solution

clean:
	rm -rf lab2/*.o lab2/*.hi
	rm -rf $(GENERATED)
	rm -f solution

solution: $(GENERATED) $(SOURCES)
	$(HC) -O2 -XBangPatterns -i./lab2 -tmpdir . ./lab2/Main.hs -o solution

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex lab2/Lex.x -o lab2/Lex.hs
	happy lab2/Synt.y -o lab2/Synt.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile lab2