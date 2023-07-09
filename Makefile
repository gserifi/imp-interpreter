all: build

build: ./src/main.hs
	ghc -o imp ./src/main.hs
	rm -f **/*.hi **/*.o

clean:
	rm -f **/*.hi **/*.o
	rm -f imp

install:
	mv imp /usr/local/bin

uninstall:
	rm -f /usr/local/bin/imp