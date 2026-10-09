/*Esercizio 1: Struttura per rappresentare uno studente
Crea una struttura Studente che contiene:

Nome (array di caratteri)
Età (intero)
Media voti (float)
Scrivi un programma che:

Chiede all'utente di inserire i dati di uno studente.
Li memorizza in una variabile di tipo Studente.
Stampa i dati a schermo.*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Studente{

    char* nome;
    int eta;
    float mediaVoti;
}Studente;

int main(){

    Studente* stud = (Studente*) malloc(sizeof(Studente)); 
    stud->nome = (char*) malloc(100*sizeof(char));

    printf("Inserisci il nome dello studente: ");
    fgets(stud->nome, sizeof(stud->nome), stdin);
    stud->nome[strlen(stud->nome)-1] = '\0';
    printf("\n");

    printf("Inserisci l'eta: ");
    scanf("%d", &stud->eta);
    printf("\n");

    printf("Inserisci la media: ");
    scanf("%f", &stud->mediaVoti);
    printf("\n");

    printf("%s ha %d anni e ha una media pari a: %.2f\n", stud->nome, stud->eta, stud->mediaVoti);

    free(stud->nome);

    return 0;


}