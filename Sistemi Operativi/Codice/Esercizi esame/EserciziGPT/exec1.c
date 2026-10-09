/*Scrivi un programma che crea un processo figlio tramite fork(). Il figlio esegue il comando ls (utilizzando exec()), 
mentre il padre aspetta che il figlio termini.

Suggerimento:
Usa execlp() per eseguire ls (questo cerca automaticamente il programma nel percorso definito dalla variabile d'ambiente PATH).
Usa wait() per far sì che il padre aspetti che il figlio termini prima di uscire.
*/

#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <string.h>

int main(){

    int rc = fork();

    if(rc < 0){
        printf("Fork fallita! \n");
        exit(1);
    }else if(rc == 0){

        char* myarg[3];

        myarg[0] = strdup("cat");
        myarg[1] = strdup("pippo.txt");
        myarg[2] = NULL;

        execvp(myarg[0], myarg);

    }else if(rc > 0){
        wait(NULL);
    }
}