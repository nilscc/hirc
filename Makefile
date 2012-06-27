.PHONY: all install run clean

all: install

install:
	cabal install --bindir=.

run: install
	./hirc

clean:
	cabal clean
	-rm hirc
