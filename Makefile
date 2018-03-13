.PHONY: all init install clean

all: install

init:
	mkdir -p ~/.speedReader

install:
	cabal install
	sudo cp src/zipwav /bin/

clean:
	cabal clean

uninstall: clean
	rm -rf ~/.speedReader
