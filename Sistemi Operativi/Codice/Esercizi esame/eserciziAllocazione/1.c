/*Esercizio 1: Allocazione di un array di interi
Scrivi un programma che alloca dinamicamente un array di 10 interi, 
riempie l'array con numeri casuali e li stampa. Poi libera la memoria.

Obiettivi:

Usare malloc() per allocare la memoria.
Usare free() per rilasciare la memoria.*/

#include <stdio.h>
#include <stdlib.h>

int main(){


    int* array = (int*) malloc(10*sizeof(int));

    if(array == NULL){
        printf("Errore nell'allocazione della memoria!");
        exit(1);
    }

    for(int i = 0;i <10; i++){

        array[i] = random() % 100;
    }

    for(int i = 0; i<10; i++){

        printf("L'elemento con posizione %d ha valore: %d\n", i+1, array[i]);
    }

    free(array);

    return 0;
}