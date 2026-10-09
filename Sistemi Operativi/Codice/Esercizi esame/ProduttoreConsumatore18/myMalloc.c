/*Scrivi un programma che implementi le funzioni my_malloc() e my_free(). Queste funzioni devono comportarsi rispettivamente come le 
  funzioni malloc() e free(), cioè: la prima deve ritornare un puntatore void ad un area di memoria utilizzabile della dimensione 
  specificata come parametro; la seconda deve liberare l'area di memoria alla quale fa riferimento il puntatore passato come parametro.
  L'intero programma deve essere realizzato senza utilizzare le funzioni malloc() e free(). E' possibile utilizzare una solo volta
  mmap() e munmap(), per le quali sono disponibili le pagine di man.*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <stddef.h>

#define MEMORY_POOL_SIZE 1024*1024*1024
#define SIZE_MIN sizeof(Block)

typedef struct Block{

    size_t size;
    int free;
    struct Block* next;

}Block;

static Block* memory_pool = NULL;
static Block* free_list = NULL;

void init_memory_pool(){

    if(memory_pool == NULL){

        memory_pool = (Block*) mmap(NULL, MEMORY_POOL_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

        memory_pool->size = MEMORY_POOL_SIZE - sizeof(Block);
        memory_pool->free = 1;
        memory_pool->next = NULL;

        free_list = memory_pool;
    }
}

void print_blocks(Block* block){

    int i = 0;

    while(block!=NULL){
         printf("Blocco: %d\n", i++);
         printf("\tSize: %d\n", block->size);
         printf("\tFree: %d\n", block->free);
         block = block->next;
    }

    printf("---------------------\n");
}

void split_block(Block* block, size_t size){

    Block* new_block = (Block*) ((char*)block + sizeof(Block) + size);

    new_block->size = block->size - sizeof(Block) - size;
    new_block->free = 1;
    new_block->next = block->next;

    block->size = size;
    block->free = 0;
    block->next = new_block;
}

void* my_malloc(size_t size){

    if(size <= 0){
        return NULL;
    }

    init_memory_pool();

    Block* current = free_list;

    while(current!=NULL){

        if(current->free && current->size >= size){
            
            if(current->size + sizeof(Block)>=size){
                split_block(current, size);
            }else{
                current->free = 0;
            }

            printf("Memoria allocata nuovo layout: \n");
            print_blocks(free_list);
            return (void*) ((char*)current + sizeof(Block));
        }

        current = current->next;

    }

    return NULL;

}

void merge_blocks(Block* block){

    while(block->next!=NULL && block->next->free){
        block->size = block->size + block->next->size + sizeof(Block);
        block->next= block->next->next;
    }
}

void my_free(void* ptr){

    if(ptr == NULL)
    return;

    Block* block = (Block*)((char*)ptr - sizeof(Block));
    block->free = 1;
    merge_blocks(block);
    printf("Memoria liberata, nuovo layout: \n");
    print_blocks(block);

}

void cleanup_memory_pool(){

    if(memory_pool != NULL){
        
        if(munmap(memory_pool, MEMORY_POOL_SIZE) == -1){
            perror("munmap");
            exit(1);
        }
        memory_pool = NULL;
        free_list = NULL;
    }
}

int main(){

    char* c = (char*) my_malloc(100);
    int* array = (int*) my_malloc(1000*sizeof(int));

    my_free(array);
    my_free(c);

    cleanup_memory_pool();

    return 0;


}