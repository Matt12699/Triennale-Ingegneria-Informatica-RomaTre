/* Write another program using fork(). The child process should print "hello"; The parent should print "goodbye". You should try
   to ensure that the child process always prints first; can you do this without calling wait() in the parent? */

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

        //wait(NULL);
        usleep(100000);
        printf("[Parent] Goodbye\n");

    }else if(rc == 0){

        printf("[Child] Hello\n");


    }
}