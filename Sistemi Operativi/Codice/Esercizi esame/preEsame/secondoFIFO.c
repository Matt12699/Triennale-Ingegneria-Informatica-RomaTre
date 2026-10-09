#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>
#include <errno.h>

#define SIZE 10

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(int fd){

    int occupati = 0;
    lseek(fd, 0, SEEK_SET);
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

        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell'apertura del file\n");
            exit(1);
        }

        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Produttore] errore nella lettura iniziale degli occupati\n");
            exit(1);
        }

        if(occupati < SIZE){

            int elementi = random() % (SIZE - occupati);
            int posizione = occupati * sizeof(int) + sizeof(int);
            lseek(fd, posizione, SEEK_SET);

            for(int i = 0; i<elementi; i++){

                int v = random() % 100;
                int w = write(fd, &v, sizeof(int));

                if(w == -1){
                    perror("[Produttore] errore nell'inserimento di un elemento\n");
                    exit(1);
                }
                printf("[Produttore] inserito in coda: %d\n", v);
            }

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

        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Consumatore] errore nell'apertura del file\n");
            exit(1);
        }

        lseek(fd, 0, SEEK_SET);
        int occupati;
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Consumatore] errore nella lettura iniziale degli occupati\n");
            exit(1);
        }

        if(occupati > 0){

            int elementi = random() % occupati;

            for(int i = 0; i<elementi; i++){

                int posizione= sizeof(int)*2;
                lseek(fd, posizione, SEEK_SET);

                int v;
                int rd = read(fd, &v, sizeof(int));
                if(rd == -1){
                    perror("[Consumatore] errore nella lettura di un elemento\n");
                    exit(1);
                }

                printf("[Consumatore] estratto dalla testa: %d\n", v);


            // Ora devo spostare tutti gli elementi successivi in avanti
                for (int j = 0; j < occupati - 1; j++) {
                    int src_pos = sizeof(int) + (j + 1) * sizeof(int);
                    int dst_pos = sizeof(int) + j * sizeof(int);

                    lseek(fd, src_pos, SEEK_SET);
                    int tmp;
                    int rd = read(fd, &tmp, sizeof(int));

                    if (rd == -1) {
                        perror("[Consumatore] errore nello spostamento\n");
                        exit(1);
                    }

                    lseek(fd, dst_pos, SEEK_SET);
                    int w = write(fd, &tmp, sizeof(int));

                    if (w == -1) {
                        perror("[Consumatore] errore nello scrivere il valore spostato\n");
                        exit(1);
                    }
                }

            }

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
        perror("Errore nell'apertura iniziale del file\n");
        exit(1);
    }

    init(fd);

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);

}