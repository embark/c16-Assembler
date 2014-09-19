all: build

build:
	mkdir -p bin; ghc --make assembler.hs -outputdir tmp/ -o bin/assembler

run:
	bin/assembler

warn:
	mkdir -p bin; ghc --make -Wall assembler.hs -outputdir tmp/ -o bin/assembler
