open Encodage
open Chaines

(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)

(****Exercice 1****)
(*
  trouve_indice : (a -> bool) -> a list -> int option
  Fonction auxiliaire qui cherche l'indice d'un élément dans une liste en fonction d'une condition
  Paramètre f : fonction qui prend un élément et retourne un booléen
  Paramètre l : liste d'éléments
  Résultat : indice si l'élément est trouvé, None sinon
*)

let trouve_indice f liste =
  let rec indice i = function
    | [] -> None  (* Si la liste est vide, on retourne None *)
    | t :: q -> if f t then Some i else indice (i + 1) q   (* On incrémente i si l'élément ne correspond pas à la condition *)
  in
  indice 0 liste (* On commence l'indexation à 0 *)


(*  
  encoder_lettre : encodage −> char −> ( int * int )
  Fonction qui indique la touche et le nombre de fois qu’il faut appuyer dessus pour saisir la lettre passée en paramètre
  Paramètre encodage :  (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre c : char, la lettre à encoder
  Résultat : (int * int), le couple (n, k) où n est le numéro du bouton et k le nombre de fois qu'il faut appuyer sur ce bouton pour obtenir la lettre c
*)

let rec encoder_lettre encodage c =
  match encodage with
  | [] -> failwith "La liste d'encodage est vide"
  | (entier, liste_char)::reste -> 
      (* On vérifie si le caractère c est dans la liste des caractères associés à la touche entier *)
      (* Si oui, on trouve l'indice de c dans cette liste *)
      (* Si non, on continue à chercher dans le reste de la liste *)
      if List.mem c liste_char then
        match trouve_indice (fun x -> x = c) liste_char with
        | Some indice -> (entier, indice + 1)  (* +1 car l'indice commence à 0 mais le nombre d'appuis commence à 1 *)
        | None -> failwith "Caractère non trouvé dans la liste"
      else
        encoder_lettre reste c  (* On continue à chercher dans le reste de la liste *)

let%test _ = encoder_lettre Encodage.t9_map 'a' = (2, 1)
let%test _ = encoder_lettre Encodage.t9_map 'f' = (3, 3)
let%test _ = encoder_lettre Encodage.t9_map 'z' = (9, 4)


(* Fonction auxiliaire : dupliquer avec pause *)
(*Signature:
Paramètre:
    - booléen : renvoie True si la touche courante est la même que la touche précédente
    - int*int : contient la touche et le nombre d'appuie
Résultat: int list, c'est une liste contenant la touche et de longueur nombre d'appuie*)

let dupliquer pause (a, b) =
  let sep = if pause then [0] else [] in
  sep @ List.init b (fun _ -> a) (*List.init crée une liste de longueur b avec la valeur a*)

let%test _ = dupliquer true (2, 3) = [0; 2; 2; 2]
let%test _ = dupliquer false (3, 2) = [3; 3]


(*Fonction : encoder_mot*)
(*Signature:
Paramètre: 
    - (int*char list)list : type d'encodage
    - string : mot à encoder
Résultat: int list, c'est une liste contenant les touches à appuyer successivement pour générer le mot d'entrer*)

let encoder_mot encodage mot =
  let lettres = Chaines.decompose_chaine mot in
  let _, res =
    List.fold_left (fun (_, acc) c -> 
      (* Pour chaque lettre, on encode la lettre et on ajoute le code à la liste *)
      (* On utilise la fonction encoder_lettre pour obtenir la touche et le nombre d'appuis *)
      (* On utilise dupliquer pour créer la liste des touches avec les pauses nécessaires *)
      let touche, nb = encoder_lettre encodage c in
      let code = dupliquer false (touche, nb) @ [0] in
      (touche, acc @ code)
    ) (-1, []) lettres
  in
  res

(* Tests pour la fonction encoder_mot *)
let%test _ = encoder_mot Encodage.t9_map "abc" = [2;0;2;2;0;2;2;2;0]
let%test _ = encoder_mot Encodage.t9_map "bonjour" = [2;2;0;6;6;6;0;6;6;0;5;0;6;6;6;0;8;8;0;7;7;7;0]
let%test _ = encoder_mot Encodage.t9_map "tendre" = [8;0;3;3;0;6;6;0;3;0;7;7;7;0;3;3;0]


(****Exercice 2****)

(*  
  decoder_lettre : encodage -> (int * int) -> char
  Fonction qui décode un couple (touche, nombre d'appuis) en une lettre
  Paramètre encodage : (int * char list) list, la liste associant les touches du clavier numérique à des lettres
  Paramètre a : int, le numéro de la touche
  Paramètre b : int, le nombre d'appuis sur cette touche
  Résultat : char, la lettre correspondante
*)

let rec decoder_lettre encodage (a,b) =
  match encodage with
  |  [] -> failwith("La liste d'encodage est vide")
  |  t::q -> let (touche, lettres) = t 
            in 
            (* Si la touche correspondante est trouvée, on récupère la lettre *)
            if a = touche then  List.nth lettres (b-1) (* -1 car l'indexation commence à 0 *) 
            (* Sinon, on continue à chercher dans le reste de la liste *)
            else decoder_lettre q (a,b)

let%test _= decoder_lettre t9_map (2,2) = 'b'
let%test _= decoder_lettre t9_map (3,2) = 'e'
let%test _= decoder_lettre t9_map (9,4) = 'z'


(*  
  decoder_mot : encodage -> int list -> string
  Fonction qui décode une liste de touches en un mot
  Paramètre encodage : (int * char list) list, la liste associant les touches du clavier numérique à des lettres
  Paramètre code : int list, la liste des touches à décoder
  Résultat : string, le mot décodé
*)

let decoder_mot encodage liste =
  let rec couple liste_chiffre =    (*fonction qui renvoie un chiffre et le nombre d'occurence consécutif du chiffre *)
    match liste_chiffre with
    | [] -> []
    | 0 :: q -> couple q  (*0 est ignoré*)
    | t :: q ->
        let rec compter nb reste = (*fonction qui compte le nombre d'occurence successif*)
          match reste with
          | u :: reste_bis when u = t -> compter (nb + 1) reste_bis (*+1 si le chiffre suivant est le même que celui précédent*)
          | _ -> (nb, reste) (*retourne le nombre d'occurence et le reste de la liste*)
        in
        let (n, suite) = compter 1 q in (*compte le nombre d'occurence du chiffre t dans la liste*)
        (*On ajoute le couple (t, n) à la liste des couples*)
        (t, n) :: couple suite
  in
  let couples = couple liste in (*On applique la fonction couple à la liste des chiffres pour obtenir une liste de couples (touche, nombre d'appuis)*)
  (*On utilise la fonction decoder_lettre pour décoder chaque couple en une lettre*)
  (*On utilise List.map pour appliquer decoder_lettre à chaque couple de la liste*)
  (*On utilise Chaines.recompose_chaine pour recomposer la chaîne de caractères à partir de la liste de lettres*)
  let lettres = List.map (fun couple -> decoder_lettre encodage couple) couples in 
  Chaines.recompose_chaine lettres
  
(* Tests pour la fonction decoder_mot *)
let%test _ = decoder_mot Encodage.t9_map [2;0;2;2;0;2;2;2;0] = "abc"
let%test _ = decoder_mot Encodage.t9_map [2;2;0;6;6;6;0;6;6;0;5;0;6;6;6;0;8;8;0;7;7;7;0] = "bonjour"
let%test _ = decoder_mot Encodage.t9_map [8;0;3;3;0;6;6;0;3;0;7;7;7;0;3;3;0] = "tendre"








