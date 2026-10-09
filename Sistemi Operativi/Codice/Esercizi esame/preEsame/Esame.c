/*Sviluppa un'applicazione chiamata mycp che implementi le seguenti funzionalità:

- deve ricevere come primo argomento un path ad un file esistente (relativo o assoluto)
- deve ricevere come secondo argomento un path ad un file (può esistere o non esistere, il path può essere relativo o assoluto)
- se la dimensione del file specificato come primo argomento è maggiore di 1000 byte, il programma termina stampando a video il messaggio: "File troppo grande"
- se la dimensione del file specificato come primo argomento è minore o uguale a 1000 byte, 
il programma deve eseguire una copia del file eseguendo il comando cp in modo tale da creare (o sovrascrivere) il file specificato come secondo argomento.

Suggerimento: il percorso assoluto del comando cp è /usr/bin/cp

Cosa inviare su Moodle:

I file .c e .h contenenti l'implementazione di mycp. Il codice dovrebbe essere adeguatamente commentato laddove necessario.*/

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>

int dimensioneFile(char* file){

	int fd = open(file, O_RDWR, S_IRWXU);

	int inizio = lseek(fd, 0, SEEK_SET);
	int fine = lseek(fd, 0, SEEK_END);

	return fine - inizio;
}

void myCpy(char* path1, char* path2){

	int fd1 = open(path1, O_RDWR, S_IRWXU);
	printf("Aperto il primo file\n");
	int fd2 = open(path2, O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);
	printf("Aperto il secondo file\n");

	if(fd1 == -1){
		perror("Errore nell'apertura del primo file\n");
		exit(1);
	}

	if(fd2 == -1){
		perror("Errore nell'apertura del secondo file\n");
		exit(1);
	}

	if(dimensioneFile(path1) > 1000){
		printf("File troppo grande\n");
		exit(1);
	}

	close(fd1);
	close(fd2);

	int rc = fork();

	if(rc < 0){
		printf("Fork fallita!\n");
		exit(1);
	}else if(rc == 0){

		char* myargs[4];

		myargs[0] = "/usr/bin/cp";
		myargs[1] = strdup(path1);
		myargs[2] = strdup(path2);
		myargs[3] = NULL;

		execvp(myargs[0], myargs);

	}else if(rc > 0){

		wait(NULL);
		printf("Copia effettuata!\n");

	}

}


int main(int argc, char* argv[]){

	char* path1 = (char*) malloc(1000);
	char* path2 = (char*) malloc(1000);

	path1 = argv[1];
	path2 = argv[2];

	myCpy(path1, path2);

	return 0;


}