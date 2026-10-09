#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <pthread.h>
#include <unistd.h>

#define SIZE 10

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(int fd){

    int occupati = 0;
    lseek(fd, 0, SEEK_SET);
    int w = write(fd, &occupati, sizeof(int));
    if( w == -1){
        perror("Errore nell'inserimento iniziale degli occupati\n");
        exit(1);
    }

}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        // ottengo l fd e ottengo il numero di record (Occupati)
        int fd = open("beatrice.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell'apertura del file\n");
            exit(1);
        }

        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));
        if( r == -1){
            perror("[Produttore] errore nella lettura iniziale degli occupati\n");
            exit(1);
        }

        if(occupati < SIZE){

            int elementi = random() % (SIZE - occupati);

            // Sposto il cursore all'ultima posizione disponibile
            int posizione = occupati * sizeof(int) + sizeof(int);
            lseek(fd, posizione, SEEK_SET);

            for(int i = 0; i<elementi ; i++){

                int v = random() % 500;
                int w = write(fd, &v, sizeof(int));

                if(w == -1){
                    perror("[Produttore] Errore nella scrittura di un elemento\n");
                    exit(1);
                }

                printf("[Produttore] inserito: %d\n", v);

            }

            //Aggiorno il numero di occupati
            occupati = occupati + elementi;
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));

            if(wr == -1){
                perror("[Produttore] errore nell'aggiornamento degli occupati\n");
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

        // ottengo l fd e ottengo il numero di record (Occupati)
        int fd = open("beatrice.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Consumatore] errore nell'apertura del file\n");
            exit(1);
        }

        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));
        if( r == -1){
            perror("[Consumatore] errore nella lettura iniziale degli occupati\n");
            exit(1);
        }

        if(occupati > 0){

            int elementi = random() % occupati;

            for(int i = 0; i<elementi; i++){

                int posizione = (occupati-i)*sizeof(int);

                lseek(fd, posizione, SEEK_SET);

                int v;
                int rd = read(fd, &v, sizeof(int));

                if(rd == -1){
                    perror("[Consumatore] errore nella lettura di un elemento\n");
                    exit(1);
                }
                printf("[Consumatore] estratto: %d\n", v); 
                lseek(fd, posizione, SEEK_SET);
                int zero = 0;
                int w = write(fd, &zero, sizeof(int));
                if(w == -1){
                    perror("[Consumatore] errore nel rimpiazzamento \n");
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

    // Apro il file 
    int fd = open("beatrice.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    if( fd == -1){
        perror("Errore nella creazione iniziale del file \n");
        exit(1);
    }

    init(fd);

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);


}