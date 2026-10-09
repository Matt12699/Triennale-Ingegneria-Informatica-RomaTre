/*
Esercizio 4: Matrice dinamica con malloc()
Scrivi un programma che alloca dinamicamente una matrice 3x3, la riempie con numeri casuali e la stampa a schermo.

Obiettivi:

Allocare memoria per una matrice dinamica.
Usare malloc() per ogni riga.
Liberare la memoria correttamente.
*/

#include <stdio.h>
#include <stdlib.h>

int righe;
int colonne;

int main(int argc, char* argv[]){

    righe = atoi(argv[1]);
    colonne = atoi(argv[2]);

    int** matrice;

    matrice = (int**) malloc(righe*sizeof(int*));

    for(int i = 0; i<righe; i++){
        matrice[i] = (int*) malloc(colonne*sizeof(int));
    }

    printf("La matrice composta da %d righe e %d colonne: \n", righe, colonne);

    for(int i = 0; i<righe ; i++){

        for(int j = 0; j<colonne ; j++){
            matrice[i][j] = (random() % 89) + 10;
        }
    }

    for(int i = 0; i<righe ; i++){

        for(int j = 0; j<colonne ; j++){
            printf(" %d ", matrice[i][j]);
        }

        printf("\n");
    }

    for(int i = 0; i<righe; i++){
        free(matrice[i]);
    }

    free(matrice);
    return 0;

}