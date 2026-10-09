#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>

#define SIZE 1000

//Struttura per implementare un processo
typedef struct Process {
	long id;
	long exec_time;
	struct timeval arrival;
	struct timeval start;
	struct timeval end;
} Process;

//Struttura per un nodo della coda
typedef struct Node {
    struct Process* data;
    struct Node* next;
    struct Node* prev;
} Node;

//Struttura per la coda
typedef struct {
    Node* front;
    Node* rear;
} Queue;

Queue coda;
pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
int simulation_length = 0;
double sommaTurnaroundTime = 0;
long id=0;

//Funzione per inizializzare la coda
void initializeQueue(Queue* queue) {
    queue->front = NULL;
    queue->rear = NULL;
}

//Funzione per capire se la coda è vuota
int isQueueEmpty(Queue* queue) {
    return (queue->front == NULL);
}

//Funzione per aggiungere in coda
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

//Funzione per rimuovere dalla coda
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

//Conta gli elementi presenti nella coda
int elementiCoda(Queue* coda){

    int contatore=0;
    
    Node* temp = coda->front;

    while(temp!=NULL){

        contatore++;
        temp = temp->next;

    }

    return contatore;
}


// Funzione che mi serve a creare un processo
Process* creaProcesso(){

    Process* new = (Process*) malloc(sizeof(Process));

    new->id = id++;
    new->exec_time = usleep(random() % (long) 1e6);
    gettimeofday(&new->arrival, NULL);

    new->start.tv_sec = 0;
    new->start.tv_usec = 0;
    new->end.tv_sec = 0;
    new->end.tv_usec = 0;

    return new;

}

//Genera processi a una certa velocità
void* produttore(void* arg){

    while(1){

        usleep(100000); //10 processi al secondo

        pthread_mutex_lock(&lock);

        int elementi = elementiCoda(&coda);
        printf("Ci sono %d elementi\n", elementi);

        if(elementi < SIZE){

            Process* data= creaProcesso();

            enqueue(&coda, data);

            if(data->id == simulation_length){
                pthread_mutex_unlock(&lock);
                break;
            }

            printf("[Produttore] immesso processo con id: %ld\n", data->id);

        }

        pthread_mutex_unlock(&lock);


    }

    pthread_mutex_unlock(&lock);

    return NULL;

}

// Funzione che calcola il turnaround time di un processo
struct timeval turnaroundTime(Process* processo){

    struct timeval turnaroundTime;

    turnaroundTime.tv_sec = processo->end.tv_sec - processo->arrival.tv_sec;
    turnaroundTime.tv_usec = processo->end.tv_usec - processo->arrival.tv_usec;

    return turnaroundTime;

}

//Estrae un processo dalla coda e lo esegue
void* consumatore(void* arg){

    while(1){

        pthread_mutex_lock(&lock);

        int elementi = elementiCoda(&coda);

        if(elementi > 0){

            Process* processo = dequeue(&coda);
            if(processo->id == simulation_length){
                pthread_mutex_unlock(&lock);
                break;
            }
            printf("[Consumatore] estratto il processo con id: %ld\n", processo->id);
            // Aggiorno il tempo di inizio del processo
            gettimeofday(&processo->start, NULL);
            // Simulo l'esecuzione
            usleep(processo->exec_time);
            // Aggiorno il tempo di fine del processo\q
            gettimeofday(&processo->end, NULL);
            struct timeval time = turnaroundTime(processo);
            double turnaroundTime = (time.tv_sec*1000000) + time.tv_usec;
            sommaTurnaroundTime = sommaTurnaroundTime + turnaroundTime;

        }

        pthread_mutex_unlock(&lock);

    }

    pthread_mutex_unlock(&lock);

    return NULL;

}

int main(int argc, char** argv){

	int cpus = atoi(argv[1]);
	simulation_length = (atoi(argv[2]));

	initializeQueue(&coda);
    
	pthread_t producer_t;
	pthread_t consumer_t[cpus];
	pthread_create(&producer_t, NULL, produttore, NULL);	
	for (int i=0; i<cpus; i++) {
		pthread_create(&consumer_t[i], NULL, consumatore, NULL);
	}
	pthread_join(producer_t, NULL);
	for (int i=0; i<cpus; i++) {
		pthread_join(consumer_t[i], NULL);
	}

	printf("Average turnaround time: %f\n", sommaTurnaroundTime/id);
	
	return 0;
}

