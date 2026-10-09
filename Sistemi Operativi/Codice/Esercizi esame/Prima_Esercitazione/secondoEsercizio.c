/* Write a program that opens a file ( with the open() system call) and then calls fork() to create a new process. Can both the child 
   and parent access the file descriptor returned by open()? What happens when they are writing to the file concurrently, at the same
   time?*/

   // 1. SI
   // 2.  Padre e figlio condividono lo stesso offset del file, quindi le scritture possono interferire tra loro.
   //     L'ordine e il risultato dipendono dalla pianificazione dei processi e non è deterministico.

#include <stdio.h>
#include <unistd.h> // Per la fork
#include <fcntl.h> // Per la open

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_WRONLY, S_IRWXU);

    int rc = fork();

    if (rc < 0){

        printf("Fork fallita! \n");

    }else if( rc == 0){

        write(fd, "Sono il figlio\n", 16);
        printf("Sono il processo figlio e fd e': %d\n", fd);

    }else if(rc > 0){

        write(fd, "Sono il padre\n", 15);
        printf("Sono il processo padre e fd e': %d\n", fd);
    }

}