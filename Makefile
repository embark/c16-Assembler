
GG = ghc
BIN = bin
TMP = tmp

GFLAGS += --make
GFLAGS += -XViewPatterns
GFLAGS += -outputdir $(TMP)/

SRC += Assembler.hs
SRC += Main.hs


all: build

run:
	$(BIN)/assembler

build: $(SRC)
	$(GG) $(GFLAGS) $^ -o $(BIN)/assembler

warn: $(SRC)
	$(GG) -Wall $(GFLAGS) $^ -o $(BIN)/assembler

clean:
	-rm -r $(BIN)/*
	-rm -r $(TMP)/*

