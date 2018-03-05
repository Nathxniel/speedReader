.PHONY: all init build clean

all: build

init:
	mkdir ~/.speedReader

build:
	cabal install

clean:
	cabal clean
