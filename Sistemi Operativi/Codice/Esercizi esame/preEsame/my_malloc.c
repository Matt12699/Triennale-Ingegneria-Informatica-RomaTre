#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>
#include <stddef.h>

#define MEMORY_POOL_SIZE 1024*1024*1024
#define MIN_SIZE sizeof(Block)

typedef struct Block{

    size_t size;
    int free;
    struct Block* next;

}Block;

Block* memory_pool = NULL;
Block* free_list = NULL;

void init_memory_pool(){

    if(memory_pool==NULL){

        memory_pool = mmap(NULL, MEMORY_POOL_SIZE, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

        if(memory_pool == -1){
            perror("mmap");
            exit(1);
        }

        memory_pool->size = MEMORY_POOL_SIZE - sizeof(Block);
        memory_pool->free = 1;
        memory_pool->next = NULL;

        free_list = memory_pool;
    }
}

void print_blocks(Block* block){

    int i = 0;
    while(block!=NULL){
        printf("Blocco n.%d\n", i++);
        printf("\tSize: %d\n", block->size);
        printf("\tFree: %d\n", block->free);

        block = block->next;
    }

    printf("---------------------------\n");
}

void split_blocks(Block* block, size_t size){

    Block* new_block = (Block*) ((char*) block + size + sizeof(Block));
    new_block->size = block->size - size - sizeof(Block);
    new_block->free = 1;
    new_block->next = block->next;

    block->size = size;
    block->free = 0;
    block->next = new_block;
}

void* my_malloc(size_t size){

    if(size<=0){
        return NULL;
    }

    init_memory_pool();

    Block* current = free_list;

    while(current!=NULL){

        if(current->free && current->size>=size){
            if(current->size + sizeof(Block) >=size){
                split_blocks(current, size);
            }else{
                current->free = 0;
            }

            printf("Memoria allocata, nuovo layout: \n");
            print_blocks(free_list);
            return (void*) ((char*) current + sizeof(Block));
        }

        current = current->next;
    }

    return NULL;
}

void merge_blocks(Block* block){

    while(block->next!=NULL && block->next->free){
        block->size = block->size + block->next->size + sizeof(Block);
        block->next = block->next->next;
    }
}

void my_free(void* ptr){

    if(ptr == NULL)
    return;

    Block* block = (Block*) ((char*) ptr + sizeof(Block));
    block->free = 1;

    merge_blocks(block);
    printf("Memoria liberata, nuovo layout: \n");
    print_blocks(block);
}

void cleanup_memory_pool(){

    if(memory_pool!=NULL){

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