/*Scrivi un programma in cui il processo padre crea un file, scrive all'interno una serie 
di numeri interi casuali (ad esempio 10 numeri casuali) usando la funzione write(), e poi crea un processo figlio con fork().
Il processo figlio deve leggere i numeri dal file usando read() e stamparli a schermo.

Suggerimenti:

Usa open() per aprire il file.
Dopo la fork(), il processo figlio deve rileggere dal file.
Usa lseek() per spostare il cursore all'inizio del file prima che il figlio inizi a leggere.*/

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    if(fd == -1){
        perror("Open fallita");
        exit(1);
    }

    int numElementi = random() % 10;

    printf("Gli elementi sono: %d\n", numElementi);

    for(int i = 0; i<numElementi; i++){
        int v = random() % 10;
        int w = write(fd, &v, sizeof(int));

        if(w == -1){
            perror("Scrittura non andata a buon fine");
            exit(1);
        }
        printf("Sono il padre ho scritto il n.%d\n", v);
    }

    int rc = fork();

    if(rc < 0){

        printf("Fork fallita! \n");
        exit(1);

    }else if(rc == 0){

        lseek(fd, 0, SEEK_SET);

        for(int i = 0; i<numElementi; i++){

            int v;

            int r = read(fd, &v, sizeof(int));

            if(r == -1){
                perror("Lettura non andata a buon fine");
                exit(1);
            }

            printf("Sono il figlio ho letto il n.%d\n", v);
        }
    }

    close(fd);

}