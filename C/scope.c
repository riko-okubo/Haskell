#include <stdio.h>

int foo = 100;

int f(int x) {
 return x + foo;
} 
int main(void){
    int foo = 10000;
    int r = f(1);
    printf("r = %d\n", r);
    return 0;
 } 

//  局所変数のfooは、関数fの引数xと同じスコープを持つので、この場合、関数fの引数xが優先される。