/* Scrivi un programma che alloca memoria per un array di interi, usa memset per impostare tutti gli elementi a zero, quindi usa 
   memcpy per copiare il contenuto di un altro array nella memoria appena allocata*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(){

    int* array = (int*) malloc(5*sizeof(int));

    for(int i = 0; i<5; i++){
        array[i] = random() % 100;
    }

    printf("Array prima di usare memset: ");
    for(int i = 0; i<5; i++){
        printf(" %d", array[i]);
    }
    printf("\n");

    int* array2 = (int*) malloc(5*sizeof(int));

    memcpy(array2, array, 5*sizeof(int));

    printf("Array2 prima di usare memset: ");
    for(int i = 0; i<5; i++){
        printf(" %d", array2[i]);
    }
    printf("\n");

    memset(array, 0, 5*sizeof(int));

    printf("Array dopo aver usato memset: ");
    for(int i = 0; i<5; i++){
        printf(" %d", array[i]);
    }
    printf("\n");
}