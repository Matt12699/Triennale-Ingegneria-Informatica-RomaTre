/* Scrivi un programma che alloca memoria per un array di interi, usa memset per impostare tutti gli elementi a zero, quindi usa 
   memcpy per copiare il contenuto di un altro array nella memoria appena allocata*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(){

    int size;
    printf("Inserisci la dimensione dell'array: \n");
    scanf("%d", &size);

    int* array = (int*) malloc(size*sizeof(int));

    for(int i=0; i<size;i++){

        printf("Inserisci un elemento: \n");
        scanf("%d", &array[i]);

    }

    printf("\n\n\n");

    for(int i=0; i<size;i++){

        printf("Elemento n%d ha valore: %d \n", i+1, array[i]);

    }

    printf("\n\n\n");

    printf("Ora utilizzo memset! \n");

    printf("\n\n\n");

    memset(array, 0, size*sizeof(int));

    for(int i=0; i<size;i++){

        printf("Elemento n%d ha valore: %d \n", i, array[i]);

    }

    int* array2 = (int*) malloc(size*sizeof(int));

    printf("\n\n\n");
    printf("Ora utilizzo memcpy, ho appena allocato della memoria\n");
    printf("\n\n\n");

    memcpy(array2, array, size*sizeof(int));

    for(int i=0; i<size;i++){

        printf("Elemento n%d del nuovo array ha valore: %d \n", i, array2[i]);

    }

    

}