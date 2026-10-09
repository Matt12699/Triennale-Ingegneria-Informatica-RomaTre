#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

int main(){

    int fd = open("pippo.txt", O_RDONLY, S_IRWXU);

    int occupati;
    read(fd, &occupati, sizeof(int));

    printf("Il numero di occupati e': %d\n", occupati);

    for(int i = 0; i<occupati; i++){

        int posizione = (occupati - i)* sizeof(int);

        lseek(fd, posizione, SEEK_SET);

        int v;
        read(fd, &v, sizeof(int));

        printf("Elemento n.%d ha valore: %d\n", (posizione/4), v);
    }
}