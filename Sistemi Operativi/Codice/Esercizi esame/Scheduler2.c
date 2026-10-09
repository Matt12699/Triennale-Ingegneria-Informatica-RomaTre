#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>

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
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
long id = 0;
long durata;
long sommaTurnaround = 0;

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
int elementiCoda(Queue* coda){

    int contatore = 0;

    Node* temp = coda->front;
    while(temp!=NULL){

        contatore++;
        temp = temp->next;
    }

    return contatore;
}

//Funzione che crea un processo
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

//Funzione per calcolare il turnaround time
long turnaroundTime(Process* processo){

    long sec = processo->end.tv_sec - processo->arrival.tv_sec;
    long usec = processo->end.tv_usec - processo->arrival.tv_usec;

    return (sec*1000000) + usec;
}

void* produttore(void* arg){

    while(1){

        usleep(100000);

        pthread_mutex_lock(&lock);

        if(id > durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        int elementi  = elementiCoda(&coda);

        if(elementi < SIZE){

            Process* new = creaProcesso();
            enqueue(&coda, new);
            printf("[Produttore] immesso processo con id: %ld\n", new->id);
        }

        pthread_mutex_unlock(&lock);

    }

    return NULL;


}

void* consumatore(void* arg){

    while(1){

        pthread_mutex_lock(&lock);

        if(id > durata){
            pthread_mutex_unlock(&lock);
            break;
        }

        //Estraggo il numero di elementi
        int elementi = elementiCoda(&coda);

        //Se ci sono elementi
        if(elementi > 0){

            //estraggo un processo e lo eseguo
            Process* estratto = dequeue(&coda);
            printf("[Consumatore] estratto il processo con id: %ld\n", estratto->id);

            pthread_mutex_unlock(&lock);
            //Eseguo il processo estratto
            gettimeofday(&estratto->start, NULL);
            usleep(estratto->exec_time);
            gettimeofday(&estratto->end, NULL);

            pthread_mutex_lock(&lock);
            //Calcolo il turnaround time
            sommaTurnaround = sommaTurnaround + turnaroundTime(estratto);

            printf("[Consumatore] eseguito il processo con id: %ld\n", estratto->id);

        }

        pthread_mutex_unlock(&lock);


    }

    return NULL;

}

int main(int argv, char* argc[]){

    initializeQueue(&coda);

    int cpu = atoi(argc[1]);
    durata = atoi(argc[2]); 

    pthread_t prod;

    pthread_t cons[cpu];

    pthread_create(&prod, NULL, produttore, NULL);

    for(int i=0; i<cpu; i++){

        pthread_create(&cons[i], NULL, consumatore, NULL);

    }

    pthread_join(prod, NULL);

    for(int i=0; i<cpu; i++){

        pthread_join(cons[i], NULL);

    }

    printf("Il turnaround time medio e': %ldusec\n", sommaTurnaround / id);
}