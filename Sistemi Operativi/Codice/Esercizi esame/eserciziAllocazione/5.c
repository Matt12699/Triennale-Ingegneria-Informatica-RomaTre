/*Esercizio 5: Lista concatenata con malloc()
Scrivi un programma che crea una lista concatenata di 5 nodi, assegnando a ogni nodo un valore casuale, e poi la stampa.

Obiettivi:

Usare malloc() per creare nodi dinamicamente.
Scorrere la lista e stamparla.
Usare free() per rilasciare la memoria.*/

#include <stdio.h>
#include <stdlib.h>

typedef struct Node{

    int data;
    struct Node* next;
}Node;

typedef struct Lista{

    Node* head;
    int size;

}Lista;

Node* creaNodo(int data){

    Node* new = (Node*) malloc(sizeof(Node));

    new->data = data;
    new->next = NULL;

    return new;

}

void aggiungiInLista(Lista* list, Node* new){

    if(list->head == NULL){
        list->head = new;
    }else{
        new->next = list->head;
        list->head = new;
    }

    list->size++;
}

Node* rimuoviDallaLista(Lista* list){

    if(list->head == NULL){

        return NULL;

    }else{
        Node* temp = list->head;
        list->head = list->head->next;
        return temp;
    }
}
// Funzione per liberare la memoria della lista
void liberaLista(Lista* lista) {
    Node* temp;
    while (lista->head != NULL) {
        temp = lista->head;
        lista->head = lista->head->next;
        free(temp);
    }
    free(lista);  // Libera la struct Lista stessa
}


int main(){

    Lista* list = (Lista*) malloc(sizeof(Lista));

    for(int i = 0; i<5; i++){

        Node* new = creaNodo(random()%100);
        aggiungiInLista(list, new);
    }

    Node* temp = list->head;

    for(int i = 0; i<list->size; i++){

        printf("->%d ", temp->data);
        temp = temp->next;

    }

    printf("\n");

    liberaLista(list);

}