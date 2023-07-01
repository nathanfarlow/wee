// files within the same directory and files supported
// by elvm's stdlib can be included
#include "example.h"

#include <stdio.h>

// these get directly compiled into putc and getc instructions
extern void putchar(int);
extern int getchar();

void gets(char* s) {
    int c;
    while ((c = getchar()) != '\n') {
        *s++ = c;
    }
    *s = '\0';
}

int main() {
    // malloc and free "work", but you're better off
    // looking at the elvm stdlib as an example and then
    // writing your own. (it doesn't actually free things)
    char* buf = malloc(100);

    // using elvm's stdlib produces a huge amount of instructions.
    // if your target needs a small binary, it's best to avoid it.
    printf("Hello! What's your name? ");
    gets(buf);
    printf("Nice to meet you, %s!\n", buf);
    free(buf);
}
