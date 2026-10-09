/* Due thread uno produce numeri su un file, un'altro dice quanti numeri sono maggiori di una certa soglia 
    alla fine fork execvp echo la scimmia nuda balla*/

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

#define SIZE 10

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
int elementiSoglia = 0;
int soglia;
int elementiMassimi;
int elementiEntrati = 0;

void init(int fd){

    int occupati = 0;
    int w = write(fd, &occupati, sizeof(int));

    if(w == -1){
        perror("Scrittura degli occupati fallita\n");
        exit(1);
    }

    lseek(fd, 0, SEEK_SET);

}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        int fd = open("doc.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell'apertura del file\n");
            exit(1);
        }

        //Leggo quanti sono gli elementi occupati
        int occupati;
        lseek(fd, 0, SEEK_SET);
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Produttore] errore nella lettura degli occupati\n");
            exit(1);
        }

        if(elementiEntrati > elementiMassimi){
            pthread_mutex_unlock(&lock);
            break;
        }

        // Posso inserire degli elementi
        if( occupati < SIZE){

            int elementi = random() % (SIZE - occupati);

            int posizioneUltimo = occupati * sizeof(int)+sizeof(int);

            lseek(fd, posizioneUltimo, SEEK_SET);

            for(int i = 0; i<elementi; i++){

                int v = random() % 15;
                int wr = write(fd, &v, sizeof(int));
                if(wr == -1){
                    perror("[Produttore] errore nella scrittura di un elemento\n");
                    exit(1);
                }
                printf("[Produttore] inserito il n.%d\n", v);
                elementiEntrati++;

            }

            //Aggiorno gli occupati
            lseek(fd, 0, SEEK_SET);
            occupati = occupati + elementi;
            int w = write(fd, &occupati, sizeof(int));
            if(w == -1){
                perror("[Prduttore] errore nell'aggiornamento degli occupati\n");
                exit(1);
            }
        }

        close(fd);
        pthread_mutex_unlock(&lock);


    }

    return NULL;

}

void* consumatore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        int fd = open("doc.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Consumatore] errore nell'apertura del file\n");
            exit(1);
        }

        //Leggo quanti sono gli elementi occupati
        int occupati;
        lseek(fd, 0, SEEK_SET);
        int r = read(fd, &occupati, sizeof(int));

        if(r == -1){
            perror("[Consumatore] errore nella lettura degli occupati\n");
            exit(1);
        }

        if(elementiEntrati > elementiMassimi){
            pthread_mutex_unlock(&lock);
            break;
        }

        if(occupati > 0){

            int elementi = random() % occupati;

            for(int i = 0; i<elementi; i++){

                int posizione = (occupati - i) * sizeof(int);
                lseek(fd, posizione, SEEK_SET);
                int v;
                int rd = read(fd, &v, sizeof(int));
                if(rd == -1){
                    perror("[Consumatore] errore nella lettura di un elemento\n");
                    exit(1);
                }
                printf("[Consumatore] letto il n.%d\n", v);
                if(v > soglia){
                    elementiSoglia++;
                }
            }

            lseek(fd, 0, SEEK_SET);
            occupati = occupati - elementi;
            int w = write(fd, &occupati, sizeof(int));
            if(w == -1){
                perror("[Consumatore] errore nell'aggiornamento degli occupati\n");
                exit(1);
            }


        }
        close(fd);
        pthread_mutex_unlock(&lock);
    }

    return NULL;

}

int main(int argc, char* argv[]){

    int fd = open("doc.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

    if(fd == -1){
        perror("Apertura iniziale del file fallita!\n");
        exit(1);
    }

    init(fd);

    elementiMassimi = atoi(argv[1]);
    soglia = atoi(argv[2]);

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);

    printf("Il numero di elementi letti che superano la soglia e': %d\n", elementiSoglia);

    int rc = fork();

    if(rc < 0){
        printf("Fork fallita! \n");
        exit(1);
    }else if(rc > 0){
        wait(NULL);
    }else if(rc == 0){
        char* myarg[3];

        myarg[0] = strdup("echo");
        myarg[1] = strdup("la scimmia nuda balla");
        myarg[2] = NULL;

        execvp(myarg[0], myarg);
    }



}