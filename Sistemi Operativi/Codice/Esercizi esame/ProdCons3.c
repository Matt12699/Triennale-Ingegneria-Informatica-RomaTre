#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

#define SIZE 10

void init(int fd){

    //Scrivo sul file il numero di occupati
    int occupati = 0;
    int w = write(fd, &occupati, sizeof(int));

    // Se la write non va a buon fine
    if(w == -1){
        perror("Errore nell'apertura iniziale del file\n");
        exit(1);
    }

    //Riporto il cursore all'inizio
    lseek(fd, 0, SEEK_SET);

}

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* produttore(void* arg){

        while(1){

            usleep(random()% (int) 1e6);

            pthread_mutex_lock(&lock);
            int fd = open("pippo.txt", O_RDWR, S_IRWXU);

            if(fd == -1){
                perror("Errore nell'apertura del file nella funzione produttore\n");
                exit(1);
            }
            //Riporto il cursore all'inizio
            lseek(fd, 0, SEEK_SET);
            //Leggo il numero di record
            int occupati;
            int r = read(fd, &occupati, sizeof(int));
            //Se la read non è andata a buon fine...
            if(r == -1){
                perror("Errore nella lettura degli occupati [Produttore]\n");
                exit(1);
            }
            // Se c'è spazio
            if(occupati < SIZE){

                int elementi = random() % (SIZE - occupati);
                // Devo scrivere a partire dall'ultimo elemento + 1
                int posizione = occupati * sizeof(int) + sizeof(int);
                lseek(fd, posizione, SEEK_SET);
                for(int i=0; i<elementi; i++){

                    int v = random() % 100;
                    // Non devo spostare il cursore perchè lo sposta la write
                    int w = write(fd, &v, sizeof(int));
                    if(w == -1){
                        perror("Errore nella scrittura di un elemento [Produttore]\n");
                        exit(1);
                    }

                    printf("[Produttore] inserito: %d\n", v);
                }

                // Vado ad aggiornare il numero di occupati
                occupati = occupati + elementi;
                // Riporto il cursore all'inizio
                lseek(fd, 0, SEEK_SET);
                int wr = write(fd, &occupati, sizeof(int));
                if( wr == -1){
                    perror("Errore nella scrittura degli occupati aggiornati [Produttore]\n");
                }
            }

            close(fd);
            pthread_mutex_unlock(&lock);
        }
}


void* consumatore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if( fd == -1){
            perror("Errore nell'apertura del file nella funzione consumatore\n");
            exit(1);
        }

        //Riporto il cursore all'inizio
        lseek(fd, 0, SEEK_SET);

        int occupati;
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("Errore nella lettura degli occupati [Consumatore]\n");
            exit(1);
        }

        if(occupati > 0){

            int elementi  = random() % occupati;
            for(int i = 0; i<elementi ; i++){
                // setto la posizione dell'ultimo elemento da leggere
                // il calcolo che faccio è: occupati, l ultimo elemento da leggere, e lo faccio scalare indietro di elementi
                // va moltiplicato * sizeof(int) dato che sul file gli interi sono da 4byte
                // -i in modo tale che ogni volta il cursore scala
                int posizione = (occupati-i) * sizeof(int);
                
                //Posiziono il cursore nel punto giusto
                lseek(fd, posizione, SEEK_SET);

                //Leggo un elemento
                int v;
                int r  = read(fd, &v, sizeof(int));
                if( r == -1){
                    perror("Errore nella lettura degli elementi [Consumatore]\n");
                    exit(1);
                }
                lseek(fd, posizione, SEEK_SET);
                //Lo rimpiazzo...
                int zero = 0;
                int w = write(fd, &zero, sizeof(int));
                if(w == -1){
                    perror("Errore nel rimpiazzamento [Consumatore]\n");
                    exit(1);
                }
                printf("[Consumatore] letto: %d\n", v);
            }

            // Aggiorno il numero di record
            occupati = occupati - elementi;

            //Sposto il cursore all'inizio
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));
            if( wr == -1){
                perror("Errore nell'aggiornamento degli occupati [Consumatore]\n");
                exit(1);
            }
        }

        close(fd);
        pthread_mutex_unlock(&lock);

    }
}

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    // Se la open non va a buon fine
    if(fd == -1){

        perror("Errore nell apertura che comprende creazione e troncamento\n");
        exit(1);

    }else{

        init(fd);

        pthread_t prod, cons;

        pthread_create(&prod, NULL, produttore, NULL);
        pthread_create(&cons, NULL, consumatore, NULL);

        pthread_join(prod, NULL);
        pthread_join(cons, NULL);
}

}