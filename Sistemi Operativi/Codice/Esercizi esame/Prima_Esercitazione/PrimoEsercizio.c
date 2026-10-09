/*Write a program that calls fork(). Before calling fork(), have the main process access a variable (e.g., x) and set its value to
  something (e.g., 100). What value is the variable in the child process? What happens to the variable when both the child and parent
  change the value of x?*/

// 1. La variabile ha lo stesso valore del processo padre ( ne eredita il valore )
// 2. Dato che una volta creato il processo figlio, questo diventa un processo a se x in entrambe le esecuzioni viene incrementata 
//    una volta sola, le modifiche non si riflettono nell altro processo.

#include <stdio.h>
#include <unistd.h> // Per la fork

int main(){

    int x = 100;

    int rc = fork();

    if(rc < 0){

        printf("Fork fallita! \n");

    }else if(rc == 0){

        x= x + 1;

        printf("Sono il processo figlio con PID: %d, x ha valore: %d\n", getpid(), x);

    }else if(rc > 0){

        x= x + 1;

        printf("Sono il processo padre con PID: %d, x ha valore: %d\n", getpid(), x);
    }
}