/*Scrivi un programma che utilizza fork() per creare un processo figlio.
Il processo padre è un produttore che genera numeri interi casuali e li invia al figlio attraverso una pipe.
 Il figlio è il consumatore, che legge i numeri dalla pipe e li stampa.

Suggerimenti:

Usa pipe() per creare la pipe.
Il padre usa write() per scrivere nella pipe.
Il figlio usa read() per leggere dalla pipe. */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

int main(){

    int rc = fork();

    int pipefd[2];

    if(pipe(pipefd) == -1){
        perror("pipe fallita \n");
        exit(1);
    }

    if(rc < 0){

        printf("Fork fallita! \n");
        exit(1);

    }else if(rc > 0){

       // close(pipefd[0]);

        int numElementi = random() % 10;

        int wr = write(pipefd[1], &numElementi, sizeof(int));

        if(wr == -1){
            perror("write fallita");
            exit(1);
        }

        for(int i = 0; i<numElementi; i++){
            int v = random() % 100;
            int w = write(pipefd[1], &v, sizeof(int));
            if(w == -1){
                perror("write fallita");
                exit(1);
            }
            printf("[Padre] scritto n.%d\n", v);
        }

       // close(pipefd[1]);

    }else if(rc == 0){

        //close(pipefd[1]);

        int elementi;
        int rd = read(pipefd[0], &elementi, sizeof(int));

        if(rd == -1){
            perror("read fallita");
            exit(1);
        }

        for(int i = 0; i<elementi; i++){
            int v;
            int r = read(pipefd[0], &v, sizeof(int));
            if(r == -1){
                perror("read fallita");
                exit(1);
            }
            printf("[Figlio] ho letto il n.%d\n", v);
        }

        //close(pipefd[0]);

    }

    return 0;

}