/*Esercizio 2: Array di strutture
Espandi l'esercizio precedente per gestire un array di studenti (es. massimo 5 studenti).

Usa malloc() per allocare dinamicamente lo spazio per l’array.
Chiedi all'utente di inserire i dati di ogni studente.
Stampa i dati di tutti gli studenti.
Libera la memoria con free().*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Studente{

    char* nome;
    int eta;
    float mediaVoti;
}Studente;

int main(){

    Studente* stud[5];

    for(int i = 0; i<5; i++){
        stud[i] = (Studente*) malloc(sizeof(Studente));
    } 

    for(int i = 0; i<5; i++){

        stud[i]->nome = (char*) malloc(100*sizeof(char));

        printf("Inserisci il nome dello studente: ");
        fgets(stud[i]->nome, sizeof(stud[i]->nome), stdin);
        stud[i]->nome[strcspn(stud[i]->nome, "\n")] = '\0';
        printf("\n");

        printf("Inserisci l'eta: ");
        scanf("%d", &stud[i]->eta);
        printf("\n");

        printf("Inserisci la media: ");
        scanf("%f", &stud[i]->mediaVoti);
        printf("\n");

        getchar();

    }

    for(int i = 0; i<5; i++){
        printf("%s ha %d anni e ha una media pari a: %.2f\n", stud[i]->nome, stud[i]->eta, stud[i]->mediaVoti);
    }

    for(int i = 0; i<5; i++){
        free(stud[i]->nome);
    }

    return 0;


}