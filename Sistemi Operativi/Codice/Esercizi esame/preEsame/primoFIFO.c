#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define SIZE 10

typedef struct Queue{

    int* array;
    int occupati;
    int capacita;

}Queue;

Queue coda;
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(){

    coda.array = (int*) malloc(SIZE * sizeof(int));
    coda.occupati = 0;
    coda.capacita = SIZE;
}

void enqueue(int v){
    
    if(coda.occupati < coda.capacita){
        coda.array[coda.occupati] = v;
        coda.occupati++;
    }
}

int dequeue(){

    int ret = -1;

    if(coda.occupati > 0){
        ret = coda.array[0];
        
        for(int i = 0; i<coda.occupati; i++){
            coda.array[i] = coda.array[i+1];
        }

        coda.occupati--;
    }

    return ret;
}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        if(coda.occupati < coda.capacita){

            int elementi = random() % (coda.capacita - coda.occupati);
            for(int i = 0; i<elementi; i++){

                int v = random() % 100;
                enqueue(v);
                printf("[Produttore] inserito in coda: %d\n", v);
            }
        }

        pthread_mutex_unlock(&lock);

    }

}

void* consumatore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        if(coda.occupati > 0){

            int elementi = random() % coda.occupati;
            for(int i = 0; i<elementi; i++){
                int v = dequeue();
                printf("[Consumatore] estratto dalla testa: %d\n", v);
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