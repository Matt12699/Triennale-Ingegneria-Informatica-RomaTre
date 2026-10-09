/*Esercizio 2: Uso di calloc() per allocare un array
Scrivi un programma che usa calloc() per allocare dinamicamente un array di 5 interi e verifica che sia inizializzato a zero.

Obiettivi:

Usare calloc() invece di malloc().
Controllare che i valori siano inizializzati a zero.
Liberare la memoria con free().*/

#include <stdio.h>
#include <stdlib.h>

int main(){

    int* array = (int*) calloc(5, sizeof(int));

    if(array == NULL){
        printf("Errore nell'allocazione della memoria!");
        exit(1);
    }

    for(int i = 0; i<5; i++){

        printf("%d\n", array[i]);
    }

    free(array);

    return 0;
}