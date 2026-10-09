/* Write a program that creates a child process, and then in the child closes standard output. What happens if the child calls printf()
   to print some output after closing the descriptor?*/

   // 1. Non funziona la print del figlio dopo aver chiuso lo standard output. Questo succede perchè non vi è più un canale valido
   //    dove mandare i dati per la stampa su terminale

#include <stdio.h>
#include <unistd.h>

int main(){

    int rc = fork();

    if (rc < 0){

        printf("Fork fallita! \n");

    }else if (rc == 0){

        printf("Sono il processo figlio! \n");
        close(STDOUT_FILENO);
        printf("Ho chiuso lo standard output\n");

    }else if (rc > 0){

        printf("Sono il processo padre! \n");
    }

}