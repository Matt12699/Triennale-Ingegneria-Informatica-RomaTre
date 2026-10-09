#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <errno.h>

#define SIZE 10

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(int fd){

    // Scrivo il numero di record iniziale
    lseek(fd, 0, SEEK_SET);
    int occupati = 0;
    int w = write(fd, &occupati, sizeof(int));

    if(w == -1){
        perror("Errore nella scrittura iniziale degli occupati\n");
        exit(1);
    }

    
}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        // Mi ricavo il file descriptor
        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell apertura iniziale del file\n");
            exit(1);
        }

        //Mi ricavo il numero di record presenti
        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Produttore] errore nella lettura dei record\n");
            exit(1);
        }

        // Se c'è spazio libero
        if(occupati < SIZE){

            // Estraggo un numero casuale di elementi da inserire
            int elementi = random() % (SIZE - occupati);

            // Mi sposto sull'ultima posizione disponibile 
            int posizione = occupati * sizeof(int) + sizeof(int);
            lseek(fd, posizione, SEEK_SET);

            for(int i = 0; i<elementi ; i++){

                // Estraggo un numero casuale e lo inserisco nel file
                int v = random() % 100;
                int w = write(fd, &v, sizeof(int));

                if(w == -1){
                    perror("[Produttore] errore nell'inserimento di un elemento\n");
                    exit(1);
                }

                printf("[Produttore] inserito: %d\n", v);

            }

            // Aggiorno il numero di record
            occupati = occupati + elementi;
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));

            if(wr == -1){
                perror("[Produttore] Errore nell'aggiornamento degli occupati\n");
                exit(1);
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

        // Mi ricavo il file descriptor
        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell apertura iniziale del file\n");
            exit(1);
        }

        //Mi ricavo il numero di record presenti
        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Consumatore] errore nella lettura dei record\n");
            exit(1);
        }

        if(occupati > 0){

            int elementi = random() % occupati;

            for(int i = 0; i<elementi ; i++){

                // Sposto a ogni iterazione la posizione per leggere l 'elemento giusto
                int posizione = (occupati - i) * sizeof(int);
                lseek(fd, posizione, SEEK_SET);
                int v;
                int rd = read(fd, &v, sizeof(int));

                if(rd == -1){
                    perror("[Consumatore] errore nella lettura di un elemento\n");
                    exit(1);
                }

                printf("[Consumatore] estratto: %d\n", v);
                
                // Sostituisco con zero
                int zero = 0;
                lseek(fd, posizione, SEEK_SET);
                int w = write(fd, &zero, sizeof(int));
                
                if(w == -1){
                    perror("[Consumatore] errore nel rimpiazzamento\n");
                    exit(1);
                }
            
            }

            // Aggiorno il numero di occupati
            occupati = occupati - elementi;
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));

            if(wr == -1){
                perror("[Consumatore] errore nell'aggiornamento degli occupati\n");
                exit(1);
            }
        }

        close(fd);
        pthread_mutex_unlock(&lock);

    }

}

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    if(fd == -1){
        perror("Errore nell'apertura iniziale del file!\n");
        exit(1);
    }

    init(fd);

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);
}