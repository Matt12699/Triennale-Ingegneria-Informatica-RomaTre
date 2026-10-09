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
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <fcntl.h>

#define SIZE 10

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(int fd){

    //Scrivo il numero di record occupati
    int occupati = 0;
    write(fd, &occupati, sizeof(int));
    //Riimposto il cursore all inizio
    lseek(fd, 0, SEEK_SET); 

}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);
        pthread_mutex_lock(&lock);
        //Ottengo l fd
        int fd = open("pippo.txt", O_RDWR , S_IRWXU);
        int occupati;
        // Riporto il cursore all'inizio
        lseek(fd, 0, SEEK_SET);
        //Leggo il numero di occupati
        read(fd, &occupati, sizeof(int));
        //Se ci sono dei posti liberi e quindi occupati < SIZE
        if(occupati < SIZE){

            // Il numero di elementi immetibili è dato dalla differenza tra la dimensione massima e il numero di occupati
            int elementi = random() % (SIZE - occupati);
            for(int i = 0; i<elementi; i++){

                int v = random() % 100;
                // Scrivo il numero nel file
                write(fd, &v, sizeof(int));
                printf("[Produttore] inserito: %d\n", v);
                // Aggiorno il numero di occupati
                occupati++;
            }
            // Riporto il cursore all'inizio
            lseek(fd, 0, SEEK_SET);
            write(fd, &occupati, sizeof(int));
        }
        close(fd);
        pthread_mutex_unlock(&lock);

    }


}

/*
void* consumatore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);
        pthread_mutex_lock(&lock);
        int fd = open("pippo.txt", O_RDWR, S_IRWXU);
        int occupati;
        // Riporto il cursore all'inizio
        lseek(fd, 0, SEEK_SET);
        read(fd, &occupati, sizeof(int));
        if(occupati > 0){
            int elementi = random() % (occupati);

            for(int i=0; i<elementi; i++){
                // Sposto il cursore sull'ultimo elemento da estrarre che sarebbe (occupati-1)*sizeof(int) spostato di 4 byte dato
                // che il primo record è il numero di occupati
                int posizione = (occupati-1)*sizeof(int) + sizeof(int);
                lseek(fd, posizione, SEEK_SET);
                // Leggo dal file
                int v;
                read(fd, &v, sizeof(int));
                printf("[Consumatore] letto: %d\n", v);
                //Sovrascrivo il valore con 0
                int zero = 0;
                lseek(fd, posizione, SEEK_SET);
                write(fd, &zero, sizeof(int));
                //Aggiorno il numero di occupati
                occupati--;
            }

            lseek(fd, 0, SEEK_SET);
            write(fd, &occupati, sizeof(int));
        }
        close(fd);
        pthread_mutex_unlock(&lock);

    }

}*/

void* consumatore(void* arg) {
    while(1) {
        usleep(random() % (int) 1e6);
        pthread_mutex_lock(&lock);
        int fd = open("pippo.txt", O_RDWR, S_IRWXU);
        int occupati;
        lseek(fd, 0, SEEK_SET);
        read(fd, &occupati, sizeof(int));

        if(occupati > 0) {
            int elementi = random() % (occupati + 1);
            for(int i = 0; i < elementi; i++) {
                
                lseek(fd, (occupati-i)*4, SEEK_SET);
                int tolto;
                
                read(fd, &tolto, sizeof(int));
                printf("[Consumatore] letto: %d\n", tolto);
                lseek(fd, (occupati-i)*4, SEEK_SET);
                int zero=0;
                write(fd, &zero, sizeof(int));
                
            }

            int occupatiNuovo = occupati - elementi;
            lseek(fd, 0, SEEK_SET);
            write(fd, &occupatiNuovo, sizeof(int));
        }
        close(fd);
        pthread_mutex_unlock(&lock);
    }
}

int main(){

    int fd = open("pippo.txt", O_CREAT | O_TRUNC | O_RDWR , S_IRWXU);

    init(fd);
    
    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);


}