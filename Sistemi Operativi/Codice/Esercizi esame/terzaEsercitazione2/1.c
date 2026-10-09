#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>

#define SIZE 10

typedef struct Stack{

    int* array;
    int occupati;
    int capacita;

}Stack;

Stack pila;
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void init(){

    // Inizializzo la pila
    pila.array = (int*) malloc(SIZE*sizeof(int));
    pila.occupati = 0;
    pila.capacita = SIZE;

}

void push(int v){

    if(pila.occupati < pila.capacita){
        pila.array[pila.occupati] = v;
        pila.occupati++;
    }
}

int pop(){

    int ret =-1;
    if(pila.occupati > 0){
        ret = pila.array[pila.occupati-1];
        pila.occupati--;
    }

    return ret;
}

void* produttore(void* arg){

    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);
        if(pila.occupati < pila.capacita){

            // Decido quanti elementi inserire
            int elementi = random() % (pila.capacita - pila.occupati);

            // Inserisco "elementi" casuali
            for(int i = 0; i<elementi; i++){

                int v = random() % 500;
                push(v);
                printf("[Produttore] inserito: %d\n", v);
            }

        }

        pthread_mutex_unlock(&lock);
    }

}

void* consumatore(void* arg){
    
    while(1){

        usleep(random() % (int) 1e6);

        pthread_mutex_lock(&lock);

        if(pila.occupati > 0){

            int elementi = random() % (pila.occupati);

            for(int i = 0; i<elementi; i++){

                int v = pop();
                printf("[Consumatore] estratto: %d\n", v);
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