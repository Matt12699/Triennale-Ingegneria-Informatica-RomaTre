/* Scrivi un programma che crea un array dinamico di numeri interi di dimensione specificata dall'utente, riempie l'array con numeri 
   casuali e stampa l'array. Assicurati di liberare la memoria allocata alla fine del programma*/

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]){

    int N = atoi(argv[1]);

    int* array = (int*) malloc(N*sizeof(int));

    for(int i = 0; i<N ; i++){

        array[i] = random() % 100;
    }

    for(int i = 0; i<N ; i++){

        printf("Il valore in posizione:%d ha valore:%d\n", i, array[i]);
    }

    free(array);

    return 0;


}