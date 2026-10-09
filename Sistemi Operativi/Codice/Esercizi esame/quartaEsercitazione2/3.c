/*Esercizio 3: Lista concatenata
Crea una lista concatenata per memorizzare numeri interi.
La lista deve supportare le seguenti operazioni:

Aggiunta in testa di un nuovo elemento.
Stampa della lista.
Cancellazione di un elemento specifico.*/

#include <stdio.h>
#include <stdlib.h>

typedef struct Node{

    int data;
    struct Node* next;

}Node;

typedef struct List{

    Node* head;

}List;

List lista;

void init(List* lista){

    lista = (List*) malloc(sizeof(List));
    lista->head = NULL;

}

void inserimentoInTesta(List* list, Node* new){

    if(list->head == NULL){
        list->head = new;
        new->next = NULL;
    }else{
        new->next = list->head;
        list->head = new;
    }

    printf("Elemento aggiunto!\n");
}

void stampaLista(List* list){

    if(list->head == NULL){
        printf("Lista vuota \n");
    }else{

        printf("Elementi:");
        Node* temp = list->head;
        while(temp!=NULL){

            printf(" %d", temp->data);
            temp = temp->next;
        }
        printf("\n");
    }
}

void cancellazioneElemento(List* list, int v){

    Node* temp = list->head;
    int trovato = 0;

    if(temp == NULL){
        printf("La lista e' vuota\n");
    }else if(temp->data == v){

        trovato = 1;
        list->head = list->head->next;
        free(temp);
        printf("Elemento rimosso\n");
    }else{

        while(temp->next!=NULL && trovato == 0){
            if(temp->next->data == v){
                trovato = 1;
            }else{

                temp = temp->next;

            }
        }

        if(trovato == 0){
        printf("Elemento non presente \n");
        }else{

        Node* temp2 = temp->next;
        temp->next = temp->next->next;
        temp2->next = NULL;
        free(temp2);
        printf("Elemento rimosso\n");
    }
    }
}

void liberaLista(List* lista) {
    Node* temp = lista->head;
    while (temp != NULL) {
        Node* temp2 = temp;  // Salviamo il nodo corrente
        temp = temp->next;   // Passiamo al prossimo nodo
        free(temp2);         // Libera il nodo corrente
    }
    lista->head = NULL;  // La lista ora è vuota
}

int main(){

    init(&lista);

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
            Node* new = (Node*) malloc(sizeof(Node));
            new->data = v;
            new->next = NULL;
            inserimentoInTesta(&lista, new);

        }else if(scelta == 2){

            int b;
            printf("Quale elemento vuoi cancellare? ");
            scanf("%d", &b);
            printf("\n");
            cancellazioneElemento(&lista, b);

        }else if(scelta == 3){

            stampaLista(&lista);

        }else if(scelta != -1){
            printf("[Attenzione] %d non e' nel menu\n", scelta);
            scelta = 1; // Se ha sbagliato a digitare numero
        }
    }

    liberaLista(&lista);

}