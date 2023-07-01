Clone the repository and `cd`:
```bash
git clone https://github.com/nathanfarlow/wee.git
cd wee
```
Install Docker to use the convenient compile script. Because our interpreter arbitrarily uses a memory array of 100000 elements, compile with:
```bash
./compile.sh example/example.c 100000 > program.wee
```
To run `program.wee`, you can use the example python interpreter:
```bash
python3 interpreter.py program.wee
```
You should see something like
```
Hello! What's your name? Nathan
Nice to meet you, Nathan!
```
I encourage you to look at [example.c]() to become familiar with how to interract with I/O and the stdlib.