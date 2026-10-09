/* Now write a program that uses wait() to wait for the child process to finish in the parent. What does wait() return?
   What happens if you use wait() in the child?*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

// La wait ritorna il pid del processo che ha cambiato stato: il figlio
// La wait nel figlio ritorna -1 dato che non c'è nessun processo da aspettare

int main(){

    
    int rc = fork();

    if(rc < 0){

        printf("Fork fallita!\n");
        exit(1);

    }else if(rc > 0){

        // int w = wait(NULL);
        printf("[Parent]\n");

    }else if(rc == 0){

        int w = wait(NULL);
        printf("[Child] La wait ritorna: %d\n", w);


    }
}