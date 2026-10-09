/*Implementa un programma che riceva per argomento quattro numeri interi e che avvii quattro thread, 
ognuno dei quali effettuerà un test di primalità su ciascuno dei numeri ricevuti per argomento.
 Ognuno di questi thread, se il numero in input è stato verificato essere primo primo, incrementerà una variabile globale condivisa in modo tale che quest'ultima contenga,
  al termine dell'esecuzione dei quattro thread, un valore rappresentante quanti dei quattro argomenti passati in input si sono rivelati essere numeri primi. 
  Il thread principale, dopo aver atteso la terminazione dei quattro thread, stamperà a video il valore della variabile globale precedentemente introdotta.

Suggerimenti:
- Utilizzare un algoritmo semplice per effettuare il test di primalità. Ad esempio, è sufficiente verificare la divisibilità intera per tutti gli interi 
a partire da 2 alla radice quadrata del numero da testare. Se il numero da testare risulta divisibile per uno di questi numeri, allora non è primo
- E' possibile convertire a interi i numeri in formato stringa passati come argomento utilizzando la funzione atoi() - man atoi per maggiori informazioni.

Cosa inviare su Moodle

I file .c e .h che contengono il programma sviluppato. Il codice dovrebbe essere adeguatamente commentato laddove necessario.*/

#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <stdlib.h>

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

int primalita = 0;

void* primalitaFunzione(void* arg){

	int intero = *(int*) arg;

	int primo = 1;

	for(int i = 2; i<intero-1; i++){
		if(intero%i==0){
			primo = 0;
		}
	}

	if(primo){
		pthread_mutex_lock(&lock);
		primalita++;
		pthread_mutex_unlock(&lock);
	}

	return NULL;

}

int main(int argc, char* argv[]){

	int intero1 = atoi(argv[1]);
	int intero2 = atoi(argv[2]);
	int intero3 = atoi(argv[3]);
	int intero4 = atoi(argv[4]);

	pthread_t primo, secondo, terzo, quarto;

	pthread_create(&primo, NULL, primalitaFunzione, &intero1);
	pthread_create(&secondo, NULL, primalitaFunzione, &intero2);
	pthread_create(&terzo, NULL, primalitaFunzione, &intero3);
	pthread_create(&quarto, NULL, primalitaFunzione, &intero4);

	pthread_join(primo, NULL);
	pthread_join(secondo, NULL);
	pthread_join(terzo, NULL);
	pthread_join(quarto, NULL);

	printf("In totale i numeri primi sono: %d\n", primalita);

	return 0;


}
