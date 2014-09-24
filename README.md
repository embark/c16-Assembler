An assembler for [c16](https://github.com/CS350C-Fall-2014/instruction-set/tree/c16)

Supports slt, ld, st, add, brz, call, lea, shl

Supports labels, empty lines, and comments (can use ";" or "//"). Follows the syntax outlined on the
c16 github branch.
For example:
```
    ; Program
    add r0, r0, 2
    lea r1, r7+5
    lea r1, r7-5

    add z, z, 2
    add r0, r0, z
label:
    add r1, r1, 4 ; comment
    add r2, r0, r1
    brz r4, label
```

Requires ghc to make (already on CS lab machines)

To build:
```
make
```

To run with a file:
```
bin/assembler example.asm
```

To run with command-line inputs:
```
bin/assembler
```
(exit with ctl-d)


For more info:
```
bin/assembler --help
```


