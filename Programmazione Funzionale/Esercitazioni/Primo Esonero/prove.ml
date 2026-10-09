let var = 57;;
(*Definire una funzione ultime_cifre: int -> int * int che riporti il
valore intero delle due ultime cifre di un int. Ad esempio:
ultime_cifre 245 = (4,5)
ultime_cifre 5 = (0,5).
Se il numero è negativo, il segno va ignorato. Ad esempio
ultime_cifre (-245) = (4,5)
ultime_cifre (-5) = (0,5).
La funzione non deve mai sollevare eccezioni, ma riportare sempre una
coppia di interi.
Si cerchi di fornire una soluzione semplice, che operi direttamente sul
numero, anziché passare per la sua rappresentazione come stringa. Si
ricordi che n/m e’ il risultato della divisione intera, n mod m il modulo, e
abs n il valore assoluto.*)

let ultime_cifre x = ((abs(x mod 100)/10), abs(x mod 10));;

(*let (a, b) = ultime_cifre var in
Printf.printf "(%d, %d)\n" a b;;*)

(*Una cifra è bella se è 0, 3, 7; un numero è bello se la sua ultima cifra è bella
e la penultima (se esiste) non lo è. Quindi in particolare le cifre belle sono
numeri belli. Definire un predicato bello: int -> bool che determini
se un numero è bello. La funzione non deve mai sollevare eccezioni, ma
riportare sempre un bool.
*)


let bello x = let (a, b) = ultime_cifre x in match b with
                | 0 | 3 | 7 ->(match a with
                            3|7 ->false
                            | _ ->true)
                | _ -> false;;

(*let x = bello var in
Printf.printf "(%b)\n" x;;*)

(*(Esercizio 8 pag 45 del libro di testo) Scrivere una funzione data: int *
string -> bool, che, applicata a una coppia (d,m), dove d è un intero e
m una stringa, determini se la coppia rappresenta una data corretta, assumendo che l’anno non sia bisestile. Si assume che i mesi siano rappresentati
da stringhe con caratteri minuscoli ("gennaio", "febbraio",. . . ).
La funzione non deve mai sollevare eccezioni, ma riportare sempre un
bool.*)

let data (d, m) = match m with
            | "febbraio" -> d>0 && d<=28
            | "aprile" | "giugno" | "settembre" | "novembre" -> d>0 && d<=30
            | "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" | "dicembre" -> d>0 && d<=31
            | _ -> false;;

(*let x = data (31, "gennaio") in
Printf.printf "(%b)\n" x;;*)

(*Rappresentiamo le ore della giornata mediante coppie (h,m): int * int,
dove h è compreso tra 0 e 23, inclusi (le ore) e m è compreso tra 0 e 59,
inclusi (i minuti). Scrivere un programma con una funzione
somma_ore: (int * int) -> (int * int) -> int * int,
che calcoli la somma di due ore così rappresentate.
Ad esempio:
somma_ore (3,15) (4,20) = (7,35)
somma_ore (3,45) (4,20) = (8,5)
somma_ore (23,45)(0,20) = (0,5)
Se uno dei due argomenti non è la rappresentazione corretta di un’ora, la
funzione solleverà un’eccezione.
Ricordarsi, anche in questo esercizio, le funzioni predefinite di OCaml per
calcolare il modulo e la divisione intera.*)
exception OreMinutiErrati
let somma_ore (h1, m1) (h2, m2) = 
    let ore_giuste h = h>=0 && h<=23
in let minuti_giusti m = m>=0 && m<=59
in if (ore_giuste h1) && (ore_giuste h2) && (minuti_giusti m1) && (minuti_giusti m2) then
    (((h1 + h2) + ((m1+m2)/60) ) mod 24, (m1+m2) mod 60)
else
    raise OreMinutiErrati;;

(*read_max: unit -> int) Leggere da tastiera una sequenza di numeri interi (anche negativi), separati da Enter e terminata dalla stringa vuota (o da una qualsiasi stringa che non rappresenti un intero),
e calcolarne il massimo. Se non viene immesso alcun numero, la
funzione solleverà un’eccezione.
(Tenere presente la funzione predefinita max: ’a -> ’a -> ’a).
*)
exception NoInput
let read_max () =
  try let m = read_int()
    in let rec aux m = 
        try let n = read_int()
            in aux (max n m)
        with _ -> m
      in aux m
    with _ -> raise NoInput

let read_max2() = 
    try let n = read_int() in
        let rec aux n = 
            try let m = read_int() in
            aux (max n m)
        with _ -> n
    in aux n
with _ -> raise NoInput
    
(*(read_max_min: unit -> int * int) Leggere da tastiera una sequenza di numeri interi (anche negativi), separati da Enter e terminata dalla stringa vuota (o da una qualsiasi stringa che non rappresenti un intero), e calcolarne il massimo e il minimo. Se non viene
immesso alcun numero, la funzione solleverà un’eccezione.
(Tenere presente le funzione predefinite max: ’a -> ’a -> ’a
e min: ’a -> ’a -> ’a).*)

let read_max_min() = 
    try let massimo = read_int() in
        let minimo = massimo in
        let rec aux (massimo, minimo)   = 
         try let numeroPescato = read_int() 
        in aux((max numeroPescato massimo), (min numeroPescato minimo))
    with _ -> (massimo, minimo)
in aux (massimo, minimo)
with _ -> raise NoInput

(*(tutti_minori: int -> bool) Dato un intero n, leggere da tastiera
una sequenza di interi (anche negativi), separati da Enter e terminata
dalla stringa vuota (o da una qualsiasi stringa che non rappresenti
un intero), e determinare (riportando true o false) se i numeri letti
sono tutti minori di n. La funzione non deve mai sollevare eccezioni
e la sua esecuzione non deve terminare finché non viene immessa la
stringa vuota (o non numerica).
*)

let tutti_minori soglia = 
     let rec aux risultatoProv =  
      try let n = read_int() in
        aux (risultatoProv && (n<soglia))
    with _ -> risultatoProv
    in aux true;;

(* (occorre: int -> bool) Dato un intero n, leggere da tastiera una
sequenza di interi, separati da Enter e terminata dalla stringa vuota
(o da una qualsiasi stringa non numerica), e determinare (riportando
true o false) se n occorre nella sequenza. La funzione non deve mai
sollevare eccezioni e la sua esecuzione non deve terminare finché non
viene immessa la stringa vuota (o non numerica).*)

let occorre n = 
    let rec aux risultatoProv = 
        try let numeroEstratto = read_int() in
        aux (risultatoProv || n=numeroEstratto) 
        with _ -> risultatoProv
    in aux false;;

(*(num_di_stringhe: unit -> int) Leggere da tastiera una sequenza di stringhe non vuote, separate da Enter e terminata dalla stringa
vuota, e riportare la lunghezza della sequenza (il numero di stringhe immesse, esclusa la stringa vuota che termina la sequenza). La
funzione non deve mai sollevare eccezioni.*)

let num_di_stringhe() =
    let rec aux risultatoProv = 
        let stringa = read_line() in
        if stringa = "" then
        risultatoProv
        else
        aux 1+risultatoProv
    in aux 0;;

(*(stringa_max: unit -> string) Leggere da tastiera una sequenza
di stringhe non vuote, separate da Enter e terminata dalla stringa
vuota, e riportare la stringa di lunghezza massima. Se non viene
immessa nessuna stringa non vuota, la funzione riporterà la stringa
vuota.*)

let stringa_max() = 
    let rec aux (stringaMax, lunghezzaMax) =
        let stringaTemp = read_line() in
        let lunghezza = String.length stringaTemp in
        if stringaTemp = "" then
            stringaMax
        else
            if lunghezzaMax<lunghezza then
                aux(stringaTemp, lunghezza)
            else
                aux(stringaMax, lunghezzaMax)
    in aux ("", 0);;

(*sumbetween: int -> int -> int, tale che sumbetween n m = somma degli interi compresi tra n e m (estremi inclusi).*)

let sumbetween n m =
    if n=m then
        n+m
    else
         let rec aux n m risultatoProv =
                if n > m then
                    risultatoProv
                else
                    aux n (m-1) (risultatoProv + m)
                in aux n m 0;;

(*sumto: int -> int, tale che sumto n = somma degli interi compresi
tra 0 e n (incluso), assumendo n ≥ 0.*)

let sumto n =
    let rec aux n risultatoProv = 
        if n<0 then
            risultatoProv
        else
            aux (n-1) risultatoProv+n
    in aux n 0;;

(*power: int -> int -> int, tale che power n k = k-esima potenza
di n (assumendo n, k ≥ 0).*)

let power n k = 
    let rec aux n k risultatoProv = 
    if k = 0 then
        risultatoProv
    else
        aux n (k-1) (risultatoProv*n)
    in aux n k 1;;



(*fib: int -> int, tale che fib n = n-esimo numero di Fibonacci
(assumendo n ≥ 0).
La sequenza dei numeri di Fibonacci è così definita:
fib 0 = 0,
fib 1 = 1,
fib n = fib (n-1) + fib(n-2)
*)

let rec fib n =
    if n = 0 then
        0
    else
        if n=1 then
            1
        else fib (n-1) + fib(n-2);;

(* maxstring: string -> char, tale che maxstring s = massimo carattere in s (secondo il codice ASCII).
Ad esempio: maxstring "antonio"= 't'. Se l’argomento è la stringa vuota, la funzione solleverà un’eccezione.
*)
exception StringaVuota
let maxstring s =
    if s = "" then raise StringaVuota else
    let rec aux c contatore = 
            if contatore = String.length s then
                c
            else
                let carattereProv = s.[contatore] in
                if carattereProv > c then
                    aux carattereProv (contatore+1) 
                else
                    aux c (contatore+1) 
in aux s.[0] 0 ;;

let x = maxstring "xyz";;
Printf.printf "%c\n" x;;

