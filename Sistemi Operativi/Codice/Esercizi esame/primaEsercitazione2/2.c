/* Write a program that opens a file ( with the open() system call) and then calls fork() to create a new process. Can both the child 
   and parent access the file descriptor returned by open()? What happens when they are writing to the file concurrently, at the same
   time?*/

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

// Entrambi possono accedere all fd
// Padre e figlio condividono lo stesso offset del file, quindi le scritture possono interferire tra loro.
// L'ordine e il risultato dipendono dalla pianificazione dei processi e non è deterministico.

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    if(fd == -1){

        perror("Apertura del file non andata a buon fine\n");
        exit(1);
    }

    int rc = fork();

    if(rc < 0){

        printf("Fork fallita!\n");
        exit(1);

    }else if(rc > 0){

        printf("[Parent] il file descriptor ha valore: %d\n", fd);
        int v = 4;
        int w = write(fd, &v, sizeof(int));
        if(w == -1){
            perror("[Parent] errore nella scrittura\n");
            exit(1);
        }
        printf("[Parent] scritto il valore: %d\n", v);

    }else if(rc == 0){

        printf("[Child] il file descriptor ha valore: %d\n", fd);
        int v = 4;
        int w = write(fd, &v, sizeof(int));
        if(w == -1){
            perror("[Child] errore nella scrittura\n");
            exit(1);
        }
        printf("[Child] scritto il valore: %d\n", v);


    }

    return 0;
}