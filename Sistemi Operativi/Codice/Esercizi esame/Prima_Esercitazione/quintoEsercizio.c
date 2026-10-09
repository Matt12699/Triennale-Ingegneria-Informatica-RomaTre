/* Now write a program that uses wait() to wait for the child process to finish in the parent. What does wait() return?
   What happens if you use wait() in the child?*/

   //1. La wait ritorna il PID del processo figlio
   //2. Il processo padre termina per primo la sua esecuzione, la wait restituisce -1 dato che il processo figlio non ha figli e quindi
   //   non attende

   
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int main(){

    int rc = fork();

    if( rc < 0){

        printf("Fork fallita!\n");

    }else if(rc == 0){

        int w = wait(NULL);
        printf("Sono il figlio! con PID: %d La wait ha valore: %d\n", getpid(), w);

    }else if(rc > 0){

        printf("Sono il padre! con PID: %d \n", getpid());
    }
}