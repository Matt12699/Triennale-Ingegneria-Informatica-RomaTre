/* Implementa una struttura dati per un vettore dinamico di interi. Includi funzioni per aggiungere un elemento, rimuovere un elemento
   e stampare tutti gli elementi del vettore. Assicurati che il vettore aumenti la sua capacità quando necessario*/

#include <stdio.h>
#include <stdlib.h>

typedef struct vett{

    int* array;
    int occupati;
    int capacita;

}vett;

int sizeIniziale;
vett vettore;

void init(){

    vettore.array = (int*) malloc(sizeIniziale*sizeof(int));
    vettore.occupati = 0;
    vettore.capacita = sizeIniziale;

}

void aggiungiElemento(int v){

    if(vettore.occupati == vettore.capacita){
        vettore.capacita = vettore.capacita*2;
        vettore.array = (int*) realloc(vettore.array, vettore.capacita*sizeof(int));
        printf("Memoria riallocata!\n");
    }

    vettore.array[vettore.occupati] = v;
    vettore.occupati++;
    printf("Elemento aggiunto!\n");
}

void rimuoviElemento(){

    if(vettore.occupati == 0){
        printf("Struttura dati vuota!\n");
    }else{

        vettore.array[vettore.occupati-1] = 0;
        vettore.occupati--;
        printf("Elemento rimosso!\n");
    }

}

void stampa(){

    if(vettore.occupati == 0){
        printf("Struttura dati vuota\n");
    }else{

        printf("Elementi della struttura dati: ");
        for(int i = 0; i<vettore.occupati; i++){
            printf(" %d", vettore.array[i]);
        }
        printf("\n");
    }

}

int main(int argc, char* argv[]){

    sizeIniziale = atoi(argv[1]);
    init();

    int scelta = 1;

    while(scelta > 0){

        printf("----------------\n");
        printf("(1) Aggiungi un elemento\n");
        printf("(2) Rimuovi un elemento\n");
        printf("(3) Stampa gli elementi\n");
        printf("(-1) Esci\n");
        printf("----------------\n");
        scanf("%d", &scelta);

        if(scelta == 1){

            int v;
            printf("Scegli un numero: \n");
            scanf("%d", &v);
            aggiungiElemento(v);

        }else if(scelta == 2){

            rimuoviElemento();

        }else if(scelta == 3){

            stampa();

        }else if(scelta != -1){
            printf("[Attenzione] %d non e' nel menu\n", scelta);
            scelta = 1; // Se ha sbagliato a digitare numero
        }
    }

    free(vettore.array);


}