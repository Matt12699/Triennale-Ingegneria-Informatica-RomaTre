/*Una pila di 10 elementi interi è condivisa tra due thread: un produttore ed un consumatore

1)     Il produttore deve essere implementato secondo la seguente logica. In un ciclo infinito:

Deve attendere una quantità di tempo casuale inferiore al secondo
Una volta scaduta l’attesa, se la pila è piena, deve attendere che qualche elemento venga rimosso dal consumatore
Quando si libera dello spazio nello stack, deve inserire un numero casuale di elementi (senza andare in overflow)
2)     Il consumatore deve essere implementato secondo la seguente logica. In un ciclo infinito:

Deve attendere una quantità di tempo casuale inferiore al secondo
Una volta scaduta l’attesa, se lo stack è vuoto, deve attendere che qualche elemento venga inserito dal produttore
Quando lo stack non è vuoto, deve leggere un numero casuale di elementi (inferiore o uguale al numero di elementi presenti nello stack) 
Suggerimenti:

-        Lo stack può essere implementato con un array di interi, un contatore di elementi già inseriti, e con due funzioni: push() e pop()

-        Alcune funzioni utili: random() e usleep()*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define SIZE 10

typedef struct Stack{

    int* array;
    int occupati;
    int capacita;

}Stack;

Stack pila;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(){

    pila.array = (int*) calloc(SIZE, sizeof(int));
    pila.occupati = 0;
    pila.capacita = SIZE;
}

// Per l'inserimento di un elemento
void push(int v){

    if(pila.occupati < pila.capacita){

        pila.array[pila.occupati] = v;
        pila.occupati++;
    }
}

// Per l'estrazione di un elemento
int pop(){

    int ret=-1;
    if( pila.occupati > 0){

        ret = pila.array[pila.occupati - 1];
        pila.occupati--;
    }

    return ret;
}

// Funzione produttore
void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);
        pthread_mutex_lock(&lock);
        if(pila.occupati < pila.capacita){

            int elementi = random() % (pila.capacita - pila.occupati);
            for(int i=0; i<elementi;i++){

                int v = random();
                push(v);
                printf("[Produttore] inserito: %d\n", v);
            }
        }
        pthread_mutex_unlock(&lock);

    }


}

// Funzione consumatore
void* consumatore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);
        if(pila.occupati > 0){
            int elementi = random() % pila.occupati;
            for(int i=0; i<elementi;i++){

                int v = pop();
                printf("[Consumatore] letto: %d\n", v);
            }
        }
        pthread_mutex_unlock(&lock);

    }


}

int main(){

    init();

    pthread_t prod, cons;

    pthread_create(&prod, NULL, produttore, NULL);
    pthread_create(&cons, NULL, consumatore, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);

}
