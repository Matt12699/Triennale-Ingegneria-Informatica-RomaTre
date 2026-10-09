/* Scrivi un programma che crea un array dinamico di numeri interi di dimensione specificata dall'utente, riempie l'array con numeri 
   casuali e stampa l'array. Assicurati di liberare la memoria allocata alla fine del programma*/


#include <stdio.h>
#include <stdlib.h> // Per la malloc

int main(){

    int size;
    printf("Inserisci la dimensione dell'array: ");
    scanf("%d", &size);

    int* array= (int*) malloc(size*sizeof(int));

    for(int i=0; i<size; i++){

        array[i]=random() % 100;

    }

    for(int i=0;i<size;i++){

        printf("\nL'elemento n. %d ha valore: %d", i+1, array[i]);

    }

    printf("\n");

    free(array);
}