/* Write a slight modification of the previous program, this time using waitpid() instead of wait(). When would waitpid()
   be useful?*/
 //2. Può essere utile quando il padre ha più figli in modo tale da specificare per quale figlio vuole attendere
   
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int main(){

    int rc = fork();

    if( rc < 0){

        printf("Fork fallita!\n");

    }else if(rc == 0){

        printf("Sono il figlio! con PID: %d\n", getpid());

    }else if(rc > 0){

        int w = waitpid(rc, NULL, 0);
        printf("Sono il padre! con PID: %d La wait ha valore: %d \n", getpid(), w);
    }
}