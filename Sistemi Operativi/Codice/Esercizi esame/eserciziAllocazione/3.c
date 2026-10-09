#include <stdio.h>
#include <stdlib.h>

int main() {
    // Allocazione iniziale di un array di 5 interi
    int* array = (int*) malloc(5 * sizeof(int));

    if (array == NULL) {
        perror("Errore nell'allocazione della memoria");
        exit(1);
    }

    // Riempimento dell'array con numeri casuali
    for (int i = 0; i < 5; i++) {
        array[i] = rand() % 100;  // Uso di rand() al posto di random()
    }

    printf("Array iniziale:\n");
    for (int i = 0; i < 5; i++) {
        printf("array[%d] = %d\n", i, array[i]);
    }

    // Tentativo di espandere l'array a 10 elementi con realloc()
    int* temp = (int*) realloc(array, 10 * sizeof(int));

    if (temp == NULL) {
        perror("Errore nella riallocazione della memoria");
        free(array); // Libera la memoria allocata precedentemente
        exit(1);
    }

    array = temp;  // Assegna il nuovo puntatore solo dopo il controllo

    // Riempimento degli elementi aggiunti
    for (int i = 5; i < 10; i++) {
        array[i] = rand() % 100;
    }

    // Stampa dell'array espanso
    printf("\nArray dopo realloc():\n");
    for (int i = 0; i < 10; i++) {
        printf("array[%d] = %d\n", i, array[i]);
    }

    // Libera la memoria allocata
    free(array);

    return 0;
}
