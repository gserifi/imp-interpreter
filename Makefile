all: build

build: ./src/main.hs
	ghc -o pimp ./src/main.hs
	rm -f **/*.hi **/*.o

clean:
	rm -f **/*.hi **/*.o
	rm -f main

install:
	mv pimp /usr/local/bin

uninstall:
	rm -f /usr/local/bin/pimp