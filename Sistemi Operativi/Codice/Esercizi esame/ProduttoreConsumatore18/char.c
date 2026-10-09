#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>

#define SIZE 10

int durata;
int numeroCaratteriEntrati = 0;
char carattere;
int caratteriUguali = 0;
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
int numeratoreCaratteri = 0;

void init(int fd){

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

        if( numeroCaratteriEntrati >= durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        int fd = open("char.txt", O_RDWR, S_IRWXU);

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

        if( occupati < SIZE ){
            int elementi = random() % (SIZE - occupati);
            while(!((elementi + numeroCaratteriEntrati) <= durata)){
                elementi = random() % (SIZE - occupati);
            }
            int posizione = (occupati) * sizeof(char) + sizeof(int);
            lseek(fd, posizione, SEEK_SET);

            for(int i = 0; i<elementi ; i++){
                char c = random() % 122;

                while(!(c>=97 && c<=122)){
                    c = random() % 122;
                }
                int w = write(fd, &c, sizeof(char));
                
                if(w == -1){
                    perror("[Produttore] errore nell'inserimento di un carattere");
                    exit(1);
                }

                printf("[Produttore] inserito in posizione: %d: %c\n",numeratoreCaratteri, c);
                numeroCaratteriEntrati++;
                numeratoreCaratteri++;

                if( c == carattere){
                    caratteriUguali++;
                }
            }

            occupati = occupati + elementi;
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));
            if ( wr == -1){
                perror("[Produttore] errore nell'aggiornamento degli occupati\n");
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

        if( numeroCaratteriEntrati >= durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        int fd = open("char.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell'apertura del file\n");
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

            int elementi = random() % (occupati);
            
            for(int i = 0; i<elementi; i++){

                int posizione = (occupati-i-1) * sizeof(char) + sizeof(int);
                lseek(fd, posizione, SEEK_SET);
                char c;
                int rd = read(fd, &c, sizeof(char));
                if(rd == -1){
                    perror("[Consumatore] errore nella lettura di un carattere\n");
                    exit(1);
                }
                printf("[Consumatore] letto: %c\n", c);
                char x = 'X';
                lseek(fd, posizione, SEEK_SET);
                int w = write(fd, &x, sizeof(char));

                if(w == -1){
                    perror("[Consumatore] errore nel rimpiazzamento\n");
                    exit(1);
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

    return NULL;


}

int main(int argc, char* argv[]){

    durata = atoi(argv[1]);
    carattere = *argv[2];

    int fd = open("char.txt", O_CREAT | O_TRUNC | O_RDWR, S_IRWXU);

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

    printf("Ci sono %d caratteri uguali a %c\n", caratteriUguali, carattere);

    return 0;

}