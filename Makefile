
GG = ghc
BIN = bin
TMP = tmp

GFLAGS += --make
GFLAGS += -outputdir $(TMP)/

SRC += Assembler.hs
SRC += Main.hs


all: build

run:
	$(BIN)/assembler

build: $(SRC)
	mkdir -p bin
	$(GG) $(GFLAGS) $^ -o $(BIN)/assembler

warn: $(SRC)
	mkdir -p bin
	$(GG) -Wall $(GFLAGS) $^ -o $(BIN)/assembler

clean:
	-rm -r $(BIN)/*
	-rm -r $(TMP)/*

