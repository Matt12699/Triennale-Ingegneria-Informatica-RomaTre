/*Scrivi un programma che implementi le funzioni my_malloc() e my_free(). Queste funzioni devono comportarsi rispettivamente come le 
  funzioni malloc() e free(), cioè: la prima deve ritornare un puntatore void ad un area di memoria utilizzabile della dimensione 
  specificata come parametro; la seconda deve liberare l'area di memoria alla quale fa riferimento il puntatore passato come parametro.
  L'intero programma deve essere realizzato senza utilizzare le funzioni malloc() e free(). E' possibile utilizzare una solo volta
  mmap() e munmap(), per le quali sono disponibili le pagine di man.*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

#define MEMORY_POOL_SIZE (1024*1024*1024) //1GB
#define MIN_BLOCK_SIZE sizeof(Block)

typedef struct Block{

    size_t size;
    int free;
    struct Block* next;

}Block;

static Block* memory_pool = NULL;
static Block* free_list = NULL; //Lista che mi dice nodo per nodo se quel nodo è occupato o meno

//metto il pezzettino di meta dato all'inizio che mi dice che la memoria è utilizzabile
void init_memory_pool(){

    //Se il memory_pool è nullo
    if(memory_pool == NULL){

     memory_pool = (Block*) mmap(NULL, MEMORY_POOL_SIZE, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

    //Controllo che l'inizializzazione sia andata a buon fine
    if(memory_pool == MAP_FAILED){
        perror("mmap");
        exit(1);
    }

    memory_pool->size = MEMORY_POOL_SIZE - sizeof(Block);
    memory_pool->free = 1;
    memory_pool->next = NULL;
    // Inizializzo la free list la lista è tutta vuota all'inizio
    free_list = memory_pool;

}
}

//Per richieste di allocazione che permettono successive allocazioni
void split_block(Block* block, size_t size){

    // Creo un puntatore di base al nuovo blocco
    Block* new_block = (Block*)((char*)block + sizeof(Block) + size); 
    // Blocco corrente che voglio splittare + dimensione della struttura dati del blocco + dimensione dei dati allocati per quel blocco
    // Indirizzo di partenza del nuovo blocco

    // Dimensione corrente che ho a disposizione - dimensione dei dati che sto andando a utilizzare - dimensione del blocco che ho creato
    new_block->size = block->size - size - sizeof(Block);
    new_block->free = 1;
    new_block->next = block->next;

    block->size = size;
    block->free = 0;
    block->next = new_block;
}

//la richiamo quando faccio un allocazione o una free
void print_blocks(Block* block){

    int i = 0;
    while( block != NULL){

        printf("Blocco %d: \n", i++);
        printf("\tSize: %lu\n", block->size);
        printf("\tFree: %d\n", block->free);
        block = block->next;
    }

    printf("---------------------------\n");

}


void* my_malloc(size_t size){

    // Richiesta di allocazione sbagliata
    if(size <= 0){
        return NULL;
    }

    init_memory_pool();

    // Parto dall inizio della lista, scorro blocco per blocco finchè non c'è un blocco di dimensione sufficiente
    // a coprire la richiesta di allocazione che mi è stata fatta
    Block* current = free_list;


    while ( current != NULL){

        // Il nodo corrente è libero? Ha una dimensione sufficiente?
        if(current->free && current->size >= size){

            // Ho due casi, il caso in cui la richiesta non è enorme e mi permette di creare un nuovo blocco oppure il caso in cui
            // la richiesta è troppo grande da occupare l'intero blocco e non lasciarmi sufficiente spazio per dei metadati
            if(current->size > size + MIN_BLOCK_SIZE){
                split_block(current, size);
            }else{

                // secondo caso
                current->free = 0;
            }
            printf("Memoria allocata, nuovo layout: \n");
            print_blocks(free_list);
            return (void*)((char*)current + sizeof(Block));
        }

        current = current->next;
    }

    return NULL; // Non ho trovato nessun blocco, ho finito la memoria.

}

// Quando trovo due nodi liberi adiacenti li unisco
void merge_blocks(Block* block){

    while( block-> next != NULL && block->next->free){

        block->size += sizeof(Block) + block->next->size;
        block->next = block->next->next;
    }
}

void my_free(void* ptr){

    if( ptr == NULL){
        return;
    }

    Block* block = (Block*)((char*)ptr - sizeof(Block)); //Sottraggo la struttura dei metadati
    block->free = 1;
    merge_blocks(block);
    printf("Memoria liberata, nuovo layout:\n");
    print_blocks(free_list);

}

void cleanup_memory_pool(){

    if(memory_pool != NULL){

        if(munmap(memory_pool, MEMORY_POOL_SIZE) == -1){
            perror("munmap");
        }

        memory_pool = NULL;
        free_list = NULL;
    }
}

int main(){

    char* ptr1 = (char*) my_malloc(100);
    int* array = my_malloc(1000 * sizeof(int));

    my_free(array);
    my_free(ptr1);

    cleanup_memory_pool();

    return 0;
}