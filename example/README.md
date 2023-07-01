Clone the repository and `cd`:
```bash
git clone https://github.com/nathanfarlow/wee.git
cd wee
```
Install docker to use the convenient compile script. Because our interpreter arbitrarily uses a memory array of 100000 elements, compile with:
```bash
./compile.sh example/example.c 100000 > program.wee
```
To run `program.wee`, you can use the example python interpreter. You'll need a python version >= 3.10, since it uses the `match` statement.
```bash
python3.10 interpreter.py program.wee
```
You should see something like
```
Hello! What's your name? Nathan
Nice to meet you, Nathan!
```
I encourage you to look at [example.c](https://github.com/nathanfarlow/wee/blob/main/example/example.c) to become familiar with how to interract with I/O and the stdlib.