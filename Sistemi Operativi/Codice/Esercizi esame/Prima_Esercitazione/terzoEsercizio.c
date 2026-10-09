/* Write another program using fork(). The child process should print "hello"; The parent should print "goodbye". You should try
   to ensure that the child process always prints first; can you do this without calling wait() in the parent? */

#include <stdio.h>
#include <unistd.h>

int main(){

    int rc = fork();

    if (rc < 0){

        printf("Fork fallita!\n");
    }else if(rc == 0){

        printf("Hello\n");
    }else if(rc > 0){

        sleep(2);
        printf("Goodbye\n");
    }
}