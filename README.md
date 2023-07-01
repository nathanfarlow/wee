# wee-lang

wee-lang is a minimal instruction set designed to be as easily portable to any system/esolang imaginable.

## Description
wee-lang has a memory array and 2 registers `A` and `B`. There are 11 instructions. An instruction either has no arguments or exactly one integer argument.

| Instruction | Description |
| ----------- | ----------- |
| `mov n` | `A = n` |
| `swap` | `swap(A, B)` |
| `add` | `A += B` |
| `sub` | `A -= B` |
| `load` | `A = memory[A]` |
| `store` | `memory[A] = B` |
| `setlt` | `A = (A < B)` |
| `jmpz n` | `if A == 0, go to nth instruction` |
| `getc` | `A = getchar()` |
| `putc` | `putchar(A)` |
| `exit` | `stop the program` |

Some key takeaways: There are no labels or data declarations. There are no redundant comparison instructions. There is no jump register instruction.