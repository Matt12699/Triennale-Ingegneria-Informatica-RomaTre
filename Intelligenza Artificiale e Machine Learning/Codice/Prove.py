# print() per stampare

# type() per conoscere il tipo dell'argomento

# Non serve specificare il tipo della variabile

# Per avere solo la parte intera della divisione //

# Per inserire dati da input basta chiamare la funzione input()
# E possibile gestire le eccezioni in questo modo
# try:
# prompt= 'Dammi un dato! '
# dato=input(prompt)
# print(dato)
# int(dato) per effettuare una conversione
# except:
# print('Please enter a number')

# Gli if hanno questa sintassi if x>0 : 
# print('x è positivo')
# else :
# print('x è negativo')

# Esistono funzioni built-in come max() e min()

# Esiste il modulo math 'import math' per eseguire la maggior parte delle funzioni matematiche es. math.sin(x), le funzioni trigonometriche accettano un argomento in radianti
# Quindi per convertire gradi/360 * 2pi

# Il modulo random permette di avere funzioni che generano numeri pseudo-casuali

# è possibile definire funzioni con def funzione(): istruzioni

# In python esiste una modalità interattiva, siamo in modalità interattiva se prima ci sono . . .

# Non si possono creare variabili con lo stesso nome di una funzione dato che in python la funzione è un oggetto funzione

# Se si tenta di assegnare a una variabile il valore di una fruitful function si otterrà un valore speciale chiamato "None"

# Qui non si usano parentesi graffe, infatti per capire il blocco di un istruzione condizionale, una funzione o un ciclo si identa semplicemente
# Il codice, quando sono finite le istruzioni si toglie l'identamento

# Si può fermare il ciclo quando si vuole con l'istruzione break
# Contrariamente si può usare l'istruzione continue per saltare alla successiva iterazione senza terminare il corpo del ciclo per l'iterazione corrente

# Esiste il for each-> 
# lista = [1, 2, 3]
# for numero in lista:
#  print(numero)
# print('Done')

# Per trovare il valore più grande in una lista:
# largest = None
# for itervar in [3, 41, 12, 9, 74, 15]
#  if largest is None or itervar > largest:
     # largest = itervar
# print(largest)

# Ovviamente il codice per il più piccolo è pressochè identico

# Funzione len per la lunghezza di una stringa

# è possibile ottenere delle sottostringhe tramite i : 
# es:
# s = 'Monty Python'
# print(s[0:5]) nota: l'ultimo numero (in questo caso 5) è escluso
# Monty 
# Se si omette il primo indice, la sottostringa parte dall'inizio della stringa. 
# Se si omette il secondo indice, la sottostringa arriva alla fine della stringa 
# Le stringhe non sono modificabili ma le possiamo modificare creandone una nuova
# Esiste l'operatore in che ci dice se la prima stringa è una sottostringa della seconda 
# es : 'a' in 'banana' true
# In questo linguaggio possono essere utilizzati gli operatori di confronto tra stringhe

# Esiste una funzione dir che ci dice i metodi assegnati a quell'oggetto
# find è una funzione che ci indica la posizione di un carattere o di una sottostringa all'interno di un altra, 
# come secondo argomento può essere specificato l'indice da cui far partire la ricerca
# Il metodo strip consente di togliere lo spazio all'inizio e alla fine di una stringa
# Il metodo startswith ci consente di capire se la nostra stringa incomincia con il parametro che gli abbiamo passato
# Il metodo capitalize ci consente di impostare a maiuscolo la lettera all'inizio della stringa
# Il metodo upper imposta a maiuscolo tutti i caratteri
# L'operatore format % ci consente di costruire stringhe, sostituendo parti di stringhe con dati memorizzati in variabili

# Gli elementi di una lista non devono essere necessariamente tutti dello stesso tipo, si possono addirittura annidare delle liste
# è possibile assegnare liste a variabili
# Le liste sono modificabili
# Se l'indice ha valore negativo si conta all'indietro ad esempio [-1] corrisponde all'ultimo elemento della lista
# L'operatore + concatena le liste l'operatore * replica una lista un certo numero di volte [0]*4 [0,0,0,0]
# L'operatore slice funziona anche per le liste
# Il metodo append aggiunge un nuovo elemento alla fine della lista
# Il metodo extend prende una lista come argomento e fa l'append di tutta la lista 
# Il metodo sort ordina gli elementi di una lista dal minore al maggiore è un metodo void non restituisce nulla 
# Se si conosce l'indice di un elemento di una lista è possibile eliminarlo con il metodo pop es. t.pop(1) il metodo
# Restituisce l'elemento rimosso
# Se non abbiamo bisogno dell'elemento rimosso possiamo utilizzare l'operatore del es. del t[1]
# Si può utilizzare del anche con l'operatore slice del t[1:5]
# Se conosciamo l'elemento da rimuovere ma non conosciamo il suo indice, possiamo usare remove
# è possibile convertire una stringa in una lista usando la funzione list

# Se invece vogliamo dividere la stringa in parole singole possiamo utilizzare il metodo split
# è possibile anche passare come parametro il delimitatore al metodo split, potrebbe essere uno spazio, un trattino ecc...
# Il metodo join fa l'inverso di split, prende una lista e concatena gli elementi, join è un metodo per le stringhe quindi occorre 
# richiamarlo per mezzo del delimitatore e passare la lista come argomento

# In Python c'è uno speciale costrutto che consente di accedere via via a tutti gli elementi di una lista, effettuare la stessa 
# operazione su ciascuno di essi, e memorizzare i nuovi elementi ottenuti in un'altra lista. es: lista2 = [e*2 for e in lista1]
# //moltiplica per 2 tutti gli elementi di lista 1 e li memorizza in lista 2

# Accade di frequente che due o più liste debbano essere attraversate simultaneamente: for a,b in zip(lista1, lista2) 
# l.append(a+b) 
# Ovviamente si ferma quando si esaurisce la lista più piccola

# I dizionari (mappe in java) sono un mapping tra un insieme di indici (chiavi) e un insieme di valori, ogni chiave individua un valore
# Per creare un dizionario senza item funzione dict()
# Per aggiungere un elemento al dizionario: eng2sp['one']='uno'
# Il seguente è sia un formato di output che un formato di input: 
# eng2sp = { 'one' : 'uno' , 'two' : 'dos' } //nota l'ordine degli item in un dizionario non è prevedibile, in questo caso si sarebbero
# potute scambiare
# In un dizionario si accede ai valori mediante la chiave print(eng2sp['one']) output: uno
# Per il calcolo degli item funzione len()
# L'operatore in funziona anche per i dizionari e ci dice se una chiave si trova in un dizionario, ma non ci dice se un valore
# si trova in un dizionario
# Il metodo .values() ci restituisce una lista con i valori del dizionario
# Il metodo .get('nomeChiave', 0) ci restituisce il valore associato alla chiave
# I dizionari supportano un metodo chiamato .items() che restituisce un elenco di tuple in cui ogni tupla è una coppia chiave valore

# Una tupla è una sequenza di valori, la differenza con una lista è che è immutabile, sintatticamente è una lista di valori separati 
# da virgole
# Per creare una tupla con un singolo elemento ocorre mettere una virgola alla fine t1=('a',)
# Per costruire una tupla funzione tuple() all'interno si può passare un argomento che se sarà una sequenza ci darà come risultato
# una tupla composta dagli elementi della sequenza
# Ovviamente come detto prima una tupla è immutabile ma si può fare il trucchetto delle stringhe e l'operatore slice
# Gli operatori di confronto funzionano per le tuple ma hanno una particolare funzione: Python comincia confrontando il primo elemento 
# di ciascuna sequenza. Se sono uguali passa a confrontare l'elemento successivo, e cosi via finche non ne trova due diversi. Gli 
# elementi successivi non vengono presi in considerazione

# Molto importante lo schema DSU :
# Decorate: "Decorare" una sequenza costruendo un elenco di tuple con una o più chiavi di ordinamento che precedono gli elementi della 
# sequenza
# Sort: Ordinare la lista delle tuple usando il sort di Python
# Undecorate: Eliminare la "Decorazione" estraendo gli elementi ordinati dalla sequenza

# Importante ricordare che in Python le assegnazioni possono essere multiple e invertite di lato

# I set sono collezioni non ordinate di altri oggetti, in cui ogni elemento compare una sola volta
# La funzione set() crea un set a partire da una lista
# Sui set si possono fare le operazioni insiemistiche come: unione, intersezione ecc...