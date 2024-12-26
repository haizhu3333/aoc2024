#include <stdio.h>

int main() {
    int a = 64196994;
    int b = 0;
    int c = 0;
    do {
        b = a & 7;
        b = b ^ 1;
        c = a >> b;
        b = b ^ 5;
        b = b ^ c;
        a = a >> 3;
        printf("%d ", b & 7);
    } while (a != 0);
    return 0;
}