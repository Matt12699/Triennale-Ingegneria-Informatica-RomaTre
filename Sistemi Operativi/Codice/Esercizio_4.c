/*mmap serve a creare una mappa con nome, ovvero una mappa di un file che si trova sul file system, quindi ci consente
 di semplificare l'interazione di un file con un file system, infatti riserva un area di memoria a un file (file mappato in memoria)
 che si trova nel file system senza dover fare ne read ne write. Non sempre è conveniente utilizzarla. Potremmo avere anche delle mappe
 anonime che non hanno file mappati, sono esattamente quello che corrisponde all utilizzo di una malloc*/
 /*munmap ci serve a togliere l'area di memoria per un determinato file dal file system*/
 /*Risoluzione Esercizio 4: Alloco un area di memoria grande con mmap e sarà l unica area di memoria che il nostro programma potrà utilizzare
   Successivamente creo un metadato con all interno la size del blocco di memoria che lo succede, ci dice che il blocco che lo succede 
   è libero e ci dice che non ha successori. Ora supponiamo di voler fare una my_malloc(1000), questa richiesta è fattibile ma non 
   rimarrebbe spazio per contenere metadati. La my malloc ritorna l'inizio dell'indirizzo.
   Torno un passo indietro e voglio fare una my malloc(100). Quindi quello che facciamo è uno split anche qui ritorno l'indirizzo
   di memoria di base più il metadato, qui vediamo come cambia il metadato iniziale 
   Il caso è una situazione in cui abbiamo un blocco di memoria libero, uno ocupato e uno libero, per utilizzare tutta la memoria disponibile
   */