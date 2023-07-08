# wee

wee is a truly minimal instruction set designed to be as easy to port to a system/esolang as possible. This repository facilitates compiling C to wee. See [usage](#usage). The C is first compiled to elvm IR using the [elvm C compiler](https://github.com/shinh/elvm). The IR is then compiled to wee.

Try the [web demo](https://wee.farlow.dev)!

## Description
wee programs have a memory array and 2 registers `A` and `B`. There are 11 instructions. An instruction either has no arguments or exactly one signed integer argument.

| Instruction   | Description |
| ------------- | ----------- |
| `mov n`       | `A = n` |
| `swap`        | `swap values of A and B` |
| `add`         | `A += B` |
| `sub`         | `A -= B` |
| `load`        | `A = memory[A]` |
| `store`       | `memory[A] = B` |
| `setlt`       | `if A < B, then set A = 1, else A = 0` |
| `jmpz n`      | `if A == 0, go to nth instruction` |
| `getc`        | `A = getchar()` |
| `putc`        | `putchar(A)` |
| `exit`        | `stop the program` |

Some key takeaways: There are no redundant comparison instructions. There is no jump register instruction. There are no labels nor data declarations. 

## Usage
To compile a C program to wee, clone the repo and run the compile script. The compile script requires docker. The first run will take a while to build the docker image.
```bash
git clone https://github.com/nathanfarlow/wee.git
cd wee
./compile.sh <file.c> <memory size>
```
Here, the memory size argument is the size of the memory array (number of words) your backend will support.

Now you can interpret the wee file or compile it to a new target! An example python interpreter can be found [here](https://github.com/nathanfarlow/wee/tree/main/interpreter.py). See the [example directory](https://github.com/nathanfarlow/wee/tree/main/example) for a example C program that demonstrates I/O and stdlib things and the commands to compile and run it.

## FAQ
**Q:** Why does a simple C program produce so many instructions?

**A:** The elvm to wee compilation is efficient given the nature of the transformation. It incurs about a 6.5x instruction overhead (compared to elvm to brainfuck's ~1335x). The C to elvm compiler is very inefficient, though. They've done great work making things function, and an elvm optimizer could take things to the next level.

**Q:** How do I perform side effects beyond input/output of characters, like drawing to a screen?

**A:** I recommend in this case you repurpose getc/putc to be a communication channel between your program and your interpreter/native side. For example, maybe your wee program wants to draw to pixel (10, 20). Perhaps it calls putc 3 times: once with an integer denoting the "start draw" command, then 10, then 20.

**Q:** What about bitwise operations? Floating point math?

**A:** It won't be efficient, but you can do bitwise operations. You can write slow divide, mod, and multiply functions. This will give you all you need to deconstruct a number into its bits and construct a number from bits. One idea to replace floating point math is to use [fixed point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic). An alternative to both of these ideas is to again use your repurposed putc/getc channel to natively compute values interpreter/native side.
