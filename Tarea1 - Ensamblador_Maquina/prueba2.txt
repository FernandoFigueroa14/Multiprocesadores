// Example 8.4
// This program counts the number of words not equal
// to 0 in data memory locations 4 and 5, storing
// the final count in R0

MOV R0, #0; // initialize result to 0
MOV R1, #1; // constant 1 for incrementing result
MOV R2, 4; // get data memory location 4
JMPZ R2, lab1; // if zero, skip next instruction
ADD R0, R0, R1; // not zero, so increment result
lab1: MOV R2, 5; // get data memory location 5
JMPZ R2, lab2; // if zero, skip next instruction
ADD R0, R0, R1; //not zero, so increment result
lab2: MOV 9, R0; // store result in data memory location 9