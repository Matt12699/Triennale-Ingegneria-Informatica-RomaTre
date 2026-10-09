#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include "queue.h"
#define SIZE 1000
#define EXEC_MAX 1000000
#define INTERARRIVAL 100000

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
pthread_mutex_t cumulative_mutex = PTHREAD_MUTEX_INITIALIZER;
Queue myQueue;
int count = 0;
int id = 0;
long cumulative_turnaround=0;
int executed = 0;
int simulation_length = 0;


void* producer(void* arg) {
	while(1) {
		usleep(INTERARRIVAL);
		pthread_mutex_lock(&mutex);
		if (id >= simulation_length) {
			pthread_mutex_unlock(&mutex);
			break;
		}
		while (count == SIZE) {
			pthread_mutex_unlock(&mutex);
		}
		Process *p = malloc(sizeof(Process));
		p->id=id++;
		p->exec_time = random() % EXEC_MAX;
		gettimeofday(&(p->arrival), NULL);
		enqueue(&myQueue, p);
		long timestamp = 1000*1000*p->arrival.tv_sec + p->arrival.tv_usec;
		count++;
		printf("[%ld]Arrivato nuovo job. \tID: %ld \t Tempo di esecuzione:%ld\n", timestamp, p->id, p->exec_time);
		pthread_mutex_unlock(&mutex);
	}
	return NULL;
}

void* consumer(void* arg) {
	while (1) {
		pthread_mutex_lock(&mutex);

		struct timespec wait_time;
		wait_time.tv_sec=1;
		while (count == 0 && executed<simulation_length) {
			pthread_mutex_unlock(&mutex);
		}
		
		if (executed >= simulation_length) {
			pthread_mutex_unlock(&mutex);
			break;
		}
		
		
		Process *p = dequeue(&myQueue);
		count--;
		executed++;
		gettimeofday(&(p->start), NULL);
		long start_timestamp = 1000*1000*p->start.tv_sec + p->start.tv_usec;
		printf("[%ld]Avvio esecuzione job.\tID: %ld\n", start_timestamp, p->id);
		pthread_mutex_unlock(&mutex);
		usleep(p->exec_time);
		gettimeofday(&(p->end), NULL);
		long end_timestamp = 1000*1000*p->end.tv_sec + p->end.tv_usec;
		printf("[%ld]Terminata esecuzione job.\tID: %ld\n", end_timestamp, p->id);
		pthread_mutex_lock(&cumulative_mutex);
		cumulative_turnaround += end_timestamp - (1000*1000*p->arrival.tv_sec + p->arrival.tv_usec);
		pthread_mutex_unlock(&cumulative_mutex);
		free(p);
	}
	return NULL;
}

int main(int argc, char** argv) {

	int cpus = atoi(argv[1]);
	simulation_length = (atoi(argv[2]));

	initializeQueue(&myQueue);
    
	pthread_t producer_t;
	pthread_t consumer_t[cpus];
	pthread_create(&producer_t, NULL, producer, NULL);	
	for (int i=0; i<cpus; i++) {
		pthread_create(&consumer_t[i], NULL, consumer, NULL);
	}
	pthread_join(producer_t, NULL);
	for (int i=0; i<cpus; i++) {
		pthread_join(consumer_t[i], NULL);
	}

	printf("Average turnaround time: %ld\n", cumulative_turnaround/id);
	
	return 0;
}