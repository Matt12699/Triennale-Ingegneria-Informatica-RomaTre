/*Un file su disco ha il seguente formato:
<numero_record><record 1><record 2>…

dove:

-        <numero_record> è un intero rappresentante il numero di record attualmente presenti all’interno del file

-        <record1><record2>, …, sono ognuno un numero intero.

Il file è acceduto da due thread: un produttore ed un consumatore, ed è gestito come se fosse una pila: i nuovi elementi vengono accodati al termine del file, e la lettura (con contestuale rimozione) degli elementi avviene dall’ultimo elemento del file. Il file non deve contenere più di 10 record oltre all’indicatore iniziale del numero di record presenti.

I due thread, produttore e consumatore, hanno il seguente comportamento:

1)     Il produttore, in un ciclo infinito:

Deve attendere una quantità di tempo casuale inferiore al secondo
Una volta scaduta l’attesa, se la pila contenuta nel file è piena, deve attendere che qualche elemento venga rimosso dal consumatore
Quando si libera dello spazio nella pila, deve inserire un numero casuale di elementi (senza andare in overflow rispetto alle dimensioni della pila) ed aggiornare il contatore all’inizio del file
2)     Il consumatore, in un ciclo infinito:

Deve attendere una quantità di tempo casuale inferiore al secondo
Una volta scaduta l’attesa, se la pila è vuota, deve attendere che qualche elemento venga inserito dal produttore
Quando la pila non è vuota, deve leggere un numero casuale di elementi (inferiore o uguale al numero di elementi presenti nello stack), sostituirne il valore con il numero 0 ed aggiornare il valore all’inizio del file.
 

Suggerimento

Quando si esegue una read() o una write() su un file, viene spostato un cursore in avanti del numero di byte letti o scritti sul file. Ad esempio, se un file contenesse la stringa “ciaopino” e venisse effettuata una read() di 4 byte, questa leggerebbe “ciao”. Un’eventuale seconda read() di 4 byte leggerebbe invece “pino”. Allo stesso modo, se si eseguisse una prima lettura di 4 byte e successivamente una scrittura di 4 byte della stringa “anno”, il file conterrebbe la stringa “ciaoanno” al termine dell’esecuzione delle read() e delle write().
E’ possibile spostare il cursore anche senza necessariamente effettuare una read() o una write(), utilizzando la funzione lseek() – usa il manuale per scoprire come usare lseek().*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>

#define SIZE 10

void init(){

    int fd = open("pippo.txt", O_WRONLY, S_IRWXU);

    if(fd == -1){
        perror("Errore nell'apertura del file nella funzione d'inizializzazione\n");
        exit(1);
    }

    //Sposto il cursore all'inizio
    lseek(fd, 0, SEEK_SET);
    int occupati = 0;

    //Scrivo il numero di record
    int w = write(fd, &occupati, sizeof(int));
    if( w == -1){
        perror("Errore nella scrittura iniziale degli occupati\n");
        exit(1);
    }

}

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        //Ricavo l'fd e il numero di occupati
        int fd = open("pippo.txt", O_RDWR, S_IRWXU);

        if(fd == -1){
            perror("[Produttore] errore nell'apertura del file\n");
            exit(1);
        }

        int occupati;
        lseek(fd, 0, SEEK_SET);
        int r = read(fd, &occupati, sizeof(int));
        if(r == -1){
            perror("[Produttore] errore nella lettura degli occupati\n");
            exit(1);
        }

        //Se la pila non è piena
        if(occupati < SIZE){

            int elementi = random() % (occupati - SIZE);

            //Sposto il cursore sulla prima posizione disponibile
            int posizione = occupati * sizeof(int) + sizeof(int);

            lseek(fd, posizione, SEEK_SET);

            for(int i = 0; i<elementi; i++){

                int v = random() % 100;
                int w = write(fd, &v, sizeof(int));
                if(w == -1){
                    perror("[Produttore] errore nell'inserimento di un elemento\n");
                    exit(1);
                }
                printf("[Produttore] inserito: %d\n", v);
            }

            // Aggiorno il numero di occupati
            occupati = occupati + elementi;
            lseek(fd, 0, SEEK_SET);
            int wr = write(fd, &occupati, sizeof(int));

            if( wr == -1){

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

        if( fd == -1){
            perror("[Consumatore] errore nell'apertura del file\n");
            exit(1);
        }

        int occupati;
        lseek(fd, 0, SEEK_SET);
        int r = read(fd, &occupati, sizeof(int));
        if(r == -1){
            perror("[Consumatore] errore nella lettura degli occupati\n");
            exit(1);
        }

        if(occupati > 0){

            int elementi = random() % occupati;


            for(int i = 0; i<elementi; i++){

                // Calcolo la posizione dell'ultimo elemento
                int posizione = (occupati-i) * sizeof(int);

                lseek(fd, posizione, SEEK_SET);

                int v;
                int r = read(fd, &v, sizeof(int));
                if(r == -1){
                    perror("[Consumatore] errore nella lettura di un elemento\n");
                    exit(1);
                }
                printf("[Consumatore] letto: %d\n", v);

                //Rimpiazzo con zero
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

    if( fd == -1){
        perror("Errore nell'apertura iniziale del filen\n");
        exit(1);
    }

    init();

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);

    return 0;


}