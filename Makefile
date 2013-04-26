SOURCES = $(wildcard *.hs)
MAIN = Main.hs
BINARY = m

GHCFLAGS = --make -Wall -O2

.PHONY: all clean

all: $(BINARY)

$(BINARY): $(SOURCES)
	ghc $(GHCFLAGS) $(MAIN) -o $@

clean:
	rm -f *.hi *.o $(BINARY)
