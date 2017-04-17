.PHONY: all install run clean

#all: install
all: fast

install:
	git rebase master production
	cabal install --bindir=.
	git checkout master

run: install
	./hirc

clean:
	cabal clean
	-rm hirc

.PHONY: config build fast

config:
	cabal config

build:
	cabal build

fast: build
	./dist/build/hirc/hirc
