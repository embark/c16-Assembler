An assembler for https://github.com/CS350C-Fall-2014/instruction-set/tree/c16

Requires ghc to make

To build:
make

To run with command-line inputs:
```
make run
```
or
```
bin/assembler
```
(exit with ctl-d)

To run with a file:
```
bin/assembler example.asm
```

For more info:
```
bin/assembler --help
```

Supports labels, empty lines, and comments.
For example:
```
    // Program
    add r0, r0, 2
    add z, z, 2
    add r0, r0, z
label:
    add r1, r1, 4 // comment
    add r2, r0, r1   
brz r4, label
```
