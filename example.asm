//Program    
    add r0, r0, 2
    lea r1, r7+5
    lea r1, r7-5

    add z, z, 2
    add r0, r0, z
hlt:
    brz z, hlt
label:
    add r1, r1, 4 // comment
    add r2, r0, r1   
    brz r4, label
