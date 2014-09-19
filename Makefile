all: build

build:
	mkdir -p bin; ghc --make -XViewPatterns assembler.hs -outputdir tmp/ -o bin/assembler

run:
	bin/assembler
