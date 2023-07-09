all: build

build:
	ghc -o imp --make ./Src/Main.hs
	rm -f **/*.hi **/*.o

clean:
	rm -f **/*.hi **/*.o
	rm -f imp

install:
	mv imp /usr/local/bin

uninstall:
	rm -f /usr/local/bin/imp