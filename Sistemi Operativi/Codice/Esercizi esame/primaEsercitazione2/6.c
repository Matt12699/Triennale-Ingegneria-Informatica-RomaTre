/* Write a slight modification of the previous program, this time using waitpid() instead of wait(). When would waitpid()
   be useful?*/

// WaitPid può essere utile quando il processo padre ha più figli e vuole aspettarne uno in particolare.

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

int main(){

    
    int rc = fork();

    if(rc < 0){

        printf("Fork fallita!\n");
        exit(1);

    }else if(rc > 0){

        int w = waitpid(rc, NULL, 0);
        printf("[Parent] La wait ha valore: %d\n", w);

    }else if(rc == 0){

        printf("[Child] Ho pid: %d\n", getpid());


    }
}