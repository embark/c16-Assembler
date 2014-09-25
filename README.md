An assembler for [c16](https://github.com/CS350C-Fall-2014/instruction-set/tree/c16)

Supports slt, ld, st, add, brz, call, lea, shl

# Features
Supports labels, empty lines, and comments (can use ";" or "//"). 
Follows the syntax outlined on the c16 github branch.
For example:
```
    ; Program
    add r0, r0, 2
    lea r1, r7, 5
    lea r1, r7, -5

    add z, z, 2
    add r0, r0, z
label:
    add r1, r1, 4 ; comment
    add r2, r0, r1
    brz r4, label
```

#Immediate format
Immediates can be expressed in binary, hex, octal, or decimal:
```
add z, z, -1
add z, z, -0x1
add z, z, 0o3
add z, z, 0b01110
add z, z, 0b001_11
```

#Syntax
There are many delmiters you can use for paramters (any of the following: '+' '-' ',' _space_). The below instruction can be expressed any of the following ways, and other combinations:
```
add r0,r0,2
add r0 r0 2
add r0, r0+2
add r0, r0, +2
```

#Usage
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


