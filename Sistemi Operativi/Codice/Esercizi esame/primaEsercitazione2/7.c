/* Write a program that creates a child process, and then in the child closes standard output. What happens if the child calls printf()
   to print some output after closing the descriptor?*/

// Ricordando che una volta generato il processo figlio i due sono processi a se con propria memoria ecc... Il processo padre riesce
// a stampare, il processo figlio, chiudendo lo stream per l'output non riesce a stampare a schermo

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>


int main(){

    int rc = fork();

    if(rc < 0){

        printf("Fork fallita!\n");
        exit(1);

    }else if(rc > 0){

        printf("[Parent]\n");

    }else if(rc == 0){

        close(STDOUT_FILENO);
        printf("[Child]\n");


    }
}