/*Scrivi un programma che legge una stringa dall’utente, la copia in un altro array e la stampa in maiuscolo.*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(){

    char* stringa = (char*) malloc(100*sizeof(char));

    printf("Inserisci una stringa: ");
    fgets(stringa, 100, stdin);

    printf("Hai inserito: %s\n", stringa);

    char* stringa2 = (char*) malloc(100*sizeof(char));

    strcpy(stringa2, stringa);

    for(int i = 0; i<strlen(stringa2); i++){
        if(stringa2[i] >=97 && stringa2[i]<=122){
            stringa2[i] = stringa2[i] - 32;
        }
    }

    printf("La stringa in maiuscolo e': %s\n", stringa2);

    free(stringa);
    free(stringa2);

    return 0;
}