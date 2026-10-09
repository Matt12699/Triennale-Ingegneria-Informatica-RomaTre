#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>

#define SIZE 1000

typedef struct Process {
	long id;
	long exec_time;
	struct timeval arrival;
	struct timeval start;
	struct timeval end;
} Process;

typedef struct Node {
    struct Process* data;
    struct Node* next;
    struct Node* prev;
} Node;

typedef struct {
    Node* front;
    Node* rear;
} Queue;

Queue coda;
long durata;
long sommaTurnaroundTime = 0;
long id = 0;
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void initializeQueue(Queue* queue) {
    queue->front = NULL;
    queue->rear = NULL;
}

int isQueueEmpty(Queue* queue) {
    return (queue->front == NULL);
}

void enqueue(Queue* queue, Process* data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (newNode == NULL) {
        fprintf(stderr, "Memory allocation error\n");
        exit(EXIT_FAILURE);
    }

    newNode->data=data;
    newNode->next = NULL;

    if (isQueueEmpty(queue)) {
        queue->front = newNode;
        queue->rear = newNode;
    } else {
        newNode->prev = queue->rear;
        queue->rear->next = newNode;
        queue->rear = newNode;
    }
}

Process* dequeue(Queue* queue) {
    if (isQueueEmpty(queue)) {
        fprintf(stderr, "Queue is empty. Cannot dequeue.\n");
        exit(EXIT_FAILURE);
    }

    Process* element = queue->front->data;
    Node* temp = queue->front;

    if (queue->front == queue->rear) {
        // Last element in the queue
        queue->front = NULL;
        queue->rear = NULL;
    } else {
        queue->front = queue->front->next;
        queue->front->prev = NULL;
    }

    free(temp);
    return element;
}

//Funzione che conta gli elementi
int contaElementi(Queue* coda){

    int elementi = 0;

    Node* temp = coda->front;

    if(temp == NULL){

        return elementi;

    }else{

        while(temp!=NULL){
            elementi++;
            temp = temp->next;
        }

        return elementi;
    }
}

Process* creaProcesso(){

    Process* new = (Process*) malloc(sizeof(Process));

    new->id = id++;
    new->exec_time = random() % (long) 1e6;
    gettimeofday(&new->arrival, NULL);

    new->start.tv_sec = 0;
    new->start.tv_usec = 0;
    new->end.tv_sec = 0;
    new->end.tv_usec = 0;

    return new;
}

void* produttore(void* arg){

    while(1){

        usleep(100000);

        pthread_mutex_lock(&lock);

        if( id > durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        if(contaElementi(&coda) < SIZE){

            Process* new = creaProcesso();
            enqueue(&coda, new);
            printf("[Produttore] aggiunto in coda processo con id: %ld\n", new->id);
        }

        pthread_mutex_unlock(&lock);


    }

    return NULL;
}

long turnaroundTime(Process* estratto){

    int sec = estratto->end.tv_sec - estratto->arrival.tv_sec;
    int usec = estratto->end.tv_usec - estratto->arrival.tv_usec;

    return (sec*1000000) + usec;
}

void* consumatore(void* arg){

    while(1){

        pthread_mutex_lock(&lock);

        if( id > durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        if(contaElementi(&coda) > 0){

            Process* estratto = dequeue(&coda);
            printf("[Consumatore] estratto processo con id: %ld\n", estratto->id);
            pthread_mutex_unlock(&lock);
            gettimeofday(&estratto->start, NULL);
            usleep(estratto->exec_time);
            gettimeofday(&estratto->end, NULL);
            pthread_mutex_lock(&lock);
            printf("[Consumatore] eseguito processo con id: %ld\n", estratto->id);

            // Calcolo il turnaroundTime
            sommaTurnaroundTime = sommaTurnaroundTime + turnaroundTime(estratto);

        }

        pthread_mutex_unlock(&lock);


    }


    return NULL;
}

int main(int argc, char* argv[]){

    durata = atoi(argv[1]);
    int cpu = atoi(argv[2]);

    initializeQueue(&coda);

    pthread_t prod;
    pthread_t cons[cpu];

    pthread_create(&prod, NULL, produttore, NULL);

    for(int i = 0; i<cpu; i++){

        pthread_create(&cons[i], NULL, consumatore, NULL);

    }

    pthread_join(prod, NULL);

    for(int i = 0; i<cpu; i++){
        pthread_join(cons[i], NULL);
    }

    printf("Il turnaround time medio con %d cpu e %ld job e' di: %ldusec\n", cpu, durata, sommaTurnaroundTime/id);

    return 0;

}