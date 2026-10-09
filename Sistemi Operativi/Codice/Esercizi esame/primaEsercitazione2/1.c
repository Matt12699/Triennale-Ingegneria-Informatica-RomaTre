/*Write a program that calls fork(). Before calling fork(), have the main process access a variable (e.g., x) and set its value to
  something (e.g., 100). What value is the variable in the child process? What happens to the variable when both the child and parent
  change the value of x?*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

// La variabile ha valore 100 (lo stesso del padre)
// Dato che una volta lanciata la fork i processi diventano due processi distinti ciascuno con una propria memoria, l'incremento avviene
// soltanto all'interno di essi

int main(){

    int x = 100;

    int rc = fork();

    if(rc < 0){

        printf("Fork fallita!\n");
        exit(1);
    }else if(rc > 0){

        printf("[Parent] x ha valore: %d\n", x);
        x++;
        printf("[Parent] x ha valore: %d\n", x);


    }else if(rc == 0){

        printf("[Child] x ha valore: %d\n", x);
        x++;
        printf("[Child] x ha valore: %d\n", x);


    }

    return 0;
}