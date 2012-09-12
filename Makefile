.PHONY: all install run clean

all: install

install:
	git rebase master production
	cabal install --bindir=.
	git checkout master

run: install
	./hirc

clean:
	cabal clean
	-rm hirc
