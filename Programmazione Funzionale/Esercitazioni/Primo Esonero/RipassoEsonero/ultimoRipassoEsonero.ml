

(* inits restituisce una lista con tutti i segmenti iniziali di lst *)
let rec inits list = 
  match list with
  [] -> [[]]
  | [x] -> [[x]]
  | x::rest -> [x] :: List.map (List.cons x) (inits rest) ;;

(* -------------------------------------------- *)
(* Funzione di supporto per stampare liste di liste di interi *)
let print_list_of_lists l =
  let rec print_list l =
    match l with
    | [] -> ()
    | x::rest -> Printf.printf "%d " x; print_list rest
  in
  let rec aux l =
    match l with
    | [] -> ()
    | x::rest ->
        Printf.printf "[ ";
        print_list x;
        Printf.printf "]\n";
        aux rest
  in
  aux l
;;

(* -------------------------------------------- *)
(* Funzione di prova per testare la funzione inits *)
let prova_inits () =
  Printf.printf "\n--- Test funzione inits (senza vuoto) ---\n";
  let lst = [1;2;3] in
  Printf.printf "Lista di partenza: [1; 2; 3]\n";
  let risultato = inits lst in
  Printf.printf "Segmenti iniziali:\n";
  print_list_of_lists risultato
;;

(* -------------------------------------------- *)
(* Eseguiamo la prova *)
prova_inits ();;

(* -------------------------------------------- *)
(* Codice per la creazione della funzione morse *)
let from_morse string =
  match string with
  ".-" -> 'a'
  | "-..." -> 'b'
  | "-.-." -> 'c'
  | "-.." -> 'd'
  | "." -> 'e'
  | "..-." -> 'f'
  | "--." -> 'g'
  | "...." -> 'h'
  | ".." -> 'i'
  | ".---" -> 'j'
  | "-.-" -> 'k'
  | ".-.." -> 'l'
  | "--" -> 'm'
  | "-." -> 'n'
  | "---" -> 'o'
  | ".--." -> 'p'
  | "--.-" -> 'q'
  | ".-." -> 'r'
  | "..." -> 's'
  | "-" -> 't'
  | "..-" -> 'u'
  | "...-" -> 'v'
  | "-..-" -> 'x'
  | "-.--" -> 'y'
  | "--.." -> 'z'
  | ".----" -> '1'
  | "..---" -> '2'
  | "...--" -> '3'
  | "....-" -> '4'
  | "....." -> '5'
  | "-...." -> '6'
  | "--..." -> '7'
  | "---.." -> '8'
  | "----." -> '9'
  | "-----" -> '0'
  | _ -> failwith "Not in the Morse alphabet"
;;



let decode_morse stringList = List.map from_morse stringList;;

let to_morse = function
    'a' -> ".-"
  | 'b' -> "-..."
  | 'c' -> "-.-."
  | 'd' -> "-.."
  | 'e' -> "."
  | 'f' -> "..-."
  | 'g' -> "--."
  | 'h' -> "...."
  | 'i' -> ".."
  | 'j' -> ".---"
  | 'k' -> "-.-"
  | 'l' -> ".-.."
  | 'm' -> "--"
  | 'n' -> "-."
  | 'o' -> "---"
  | 'p' -> ".--."
  | 'q' -> "--.-"
  | 'r' -> ".-."
  | 's' -> "..."
  | 't' -> "-"
  | 'u' -> "..-"
  | 'v' -> "...-"
  | 'x' -> "-..-"
  | 'y' -> "-.--"
  | 'z' -> "--.."
  | '1' -> ".----"
  | '2' -> "..---"
  | '3' -> "...--"
  | '4' -> "....-"
  | '5' -> "....."
  | '6' -> "-...."
  | '7' -> "--..."
  | '8' -> "---.."
  | '9' -> "----."
  | '0' -> "-----"
  | _ -> failwith "Not in the Morse alphabet"
;;

let encode_morse charList = List.map to_morse charList;;

let rec implode charList = 
  match charList with
  [] -> ""
  | x::rest -> (String.make 1 x)^(implode rest);;

(* Versione iterativa *)
let implode charList =
  let rec aux risultato charList = 
    match charList with
    [] -> risultato
    | x::rest -> aux (risultato^(String.make 1 x)) rest
  in aux "" charList;;

let explode string = 
  let rec aux risultato i string =
  if i = (String.length string) then risultato
  else 
    aux (risultato@[string.[i]]) (i+1) string
  in aux [] 0 string;;

  (* ----------------------------------------- *)
(* Prova della funzione explode *)

let test_explode s =
  Printf.printf "\n------------------------------\n";
  Printf.printf "Test explode:\n";
  Printf.printf "Stringa di partenza: \"%s\"\n" s;
  let risultato = explode s in
  Printf.printf "Lista dei caratteri: [";
  List.iter (fun c -> Printf.printf "'%c'; " c) risultato;
  Printf.printf "]\n";;

(* Esempi di test *)
test_explode "ciao";;
test_explode "esame";;
test_explode "";;

let decode_morse_to_string list = implode (decode_morse list);;
let encode_morse_from_string list = encode_morse (explode list);;

(* Applicata a una lista list rappresentante un insieme S, riporta una lista con tutti i sottoinsiemi di S *)

let rec powerset list = 
  match list with
  [] -> [[]]
  | x::rest -> let prest = powerset rest 
in prest @ (List.map (List.cons x) prest);;

(* Applicata a due insiemi (liste) setA setB, riporta la lista di tutte le coppie (x,y), con x appartenente a setA e y a setB *)

let rec cartprod setA setB =
    match setA with
    [] -> []
    | x::rest -> (List.map (function y-> (x,y)) setB) @ (cartprod rest setB);;

let cartprod setA setB =
  let rec aux risultato setA = 
    match setA with
    [] -> risultato
    | x::rest -> aux (risultato@(List.map (function y-> (x,y)) setB)) rest
  in aux [] setA;;

    (* ----------------------------------------- *)
(* Prova della funzione cartprod *)

let test_cartprod xs ys =
  Printf.printf "\n------------------------------\n";
  Printf.printf "Test cartprod:\n";
  Printf.printf "Primo insieme: [";
  List.iter (fun x -> Printf.printf "%d; " x) xs;
  Printf.printf "]\n";
  Printf.printf "Secondo insieme: [";
  List.iter (fun y -> Printf.printf "%d; " y) ys;
  Printf.printf "]\n";
  let risultato = cartprod xs ys in
  Printf.printf "Prodotto cartesiano:\n";
  List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) risultato;
  Printf.printf "\n";;

(* Esempi di test *)
test_cartprod [1;2] [3;4];;
test_cartprod [5] [6;7;8];;
test_cartprod [] [1;2];;
test_cartprod [1;2] [];;

let rec upTo n m =
  if n>m then 0
  else
    n + upTo (n+1) m;;

let x = upTo 1 3;;
Printf.printf "%d" x;;

(* interleave: ’a-> ’a list-> ’a list list, tale che interleave
 x lst riporti una lista con tutte le liste che si ottengono inserendo x in
 qualsiasi posizione in lst. Utilizzare la funzione List.map.
 Ad esempio, interleave 10 [0;1;2] = [[10; 0; 1; 2]; [0; 10; 1;
 2]; [0; 1; 10; 2]; [0; 1; 2; 10]]. *)

 let rec interleave x lst = match lst with
    [] -> [[x]]
    | y::rest -> (x::lst) :: List.map( function z-> y::z) (interleave x rest);;

(*permut: ’a list-> ’a list list, tale che permut lst riporti una
 lista con tutte le permutazioni di lst (in qualsiasi ordine).*)
 let rec permut list = 
  match list with
  [] -> [[]]
  | x::rest -> List.flatten(List.map (interleave x) (permut rest));;

(* Applicata a una lista list rappresentante un insieme S, riporta una lista con tutti i sottoinsiemi di S *)

let rec powerset list = 
  match list with
  [] -> [[]]
  | x::rest -> let prest = powerset rest 
in prest @ (List.map (List.cons x) prest);;

(* Merge sort*)
let rec merge l1 l2 =
  match (l1,l2) with
    ([],l) | (l,[]) -> l
  | (x::xs,y::ys) -> if x<=y then x::(merge xs (y::ys))
                     else y::(merge (x::xs) ys)
;;

let split l = 
  let rec split_aux l1 l2 = function
      [] -> (l1,l2)
    | [x] -> (x::l1,l2)
    | x::y::xs -> split_aux (x::l1) (y::l2) xs
  in split_aux [] [] l
;;

let rec mergesort = function
    [] -> []
  | [x] -> [x]
  | l -> let (xs,ys) = split l
         in merge (mergesort xs) (mergesort ys)
;;