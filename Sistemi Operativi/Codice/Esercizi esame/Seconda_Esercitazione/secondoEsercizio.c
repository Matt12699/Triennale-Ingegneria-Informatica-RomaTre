/* Implementa una struttura dati per un vettore dinamico di interi. Includi funzioni per aggiungere un elemento, rimuovere un elemento
   e stampare tutti gli elementi del vettore. Assicurati che il vettore aumenti la sua capacità quando necessario*/

#include <stdio.h>
#include <stdlib.h>

typedef struct Vett{

    int* array;
    int occupati;
    int size;

}Vett;

Vett vettore;

void aggiungi(int v){

    if(vettore.occupati >= vettore.size){
        aumentaCap();
    }


    vettore.array[vettore.occupati] = v;
    vettore.occupati++;

    printf("Elemento aggiunto !\n");

}

void rimuovi(){

    if(vettore.occupati == 0){

        printf("Il vettore e' vuoto \n");

    }else{

        vettore.occupati--;
        printf("Rimozione effettuata \n");


    }

}

void stampa(){

    if(vettore.occupati == 0){

        printf("Il vettore e' vuoto \n");

    }else{

        for(int i=0; i<vettore.occupati;i++){

            printf("L'elemento n.%d ha valore %d\n", i, vettore.array[i]);
        }

    }

}


void aumentaCap(){

    vettore.size = vettore.size*2;
    vettore.array= realloc(vettore.array, vettore.size*sizeof(int));
    printf("La size e' stata raddoppiata! \n");

}

void init(){

    printf("Inserisci la dimensione dell'array: ");
    scanf("%d", &vettore.size);

    vettore.array= malloc(vettore.size*sizeof(int));

    vettore.occupati= 0;

}

int main(){

    init();

    int scelta=1;

    while(scelta > 0){

        printf("-------------------\n");
        printf("Inserisci 1 per aggiungere un elemento \n");
        printf("Inserisci 2 per rimuovere un elemento \n");
        printf("Inserisci 3 per stampare gli elementi \n");
        printf("Inserisci -1 per uscire \n");
        printf("-------------------\n");
        scanf("%d", &scelta);

        if(scelta == 1){
            int v;
            printf("Inserisci il numero che vuoi aggiungere: ");
            scanf("%d", &v);
            aggiungi(v);

        }else if(scelta == 2){

            rimuovi();

        }else if(scelta == 3){

            stampa();
        }
    }

    free(vettore.array);


}