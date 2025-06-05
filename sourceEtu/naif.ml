open Encodage
open Chaines

(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)


(*  
  encoder_lettre : encodage −> char −> ( int * int )
  Fonction qui indique la touche et le nombre de fois qu’il faut appuyer dessus pour saisir la lettre passée en paramètre
  Paramètre encodage :  (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre c : char, la lettre à encoder
  Résultat : (int * int), le couple (n, k) où n est le numéro du bouton et k le nombre de fois qu'il faut appuyer sur ce bouton pour obtenir la lettre c
*)


let find_index f l =
  let rec ind i = function
    | [] -> None
    | x :: xs -> if f x then Some i else ind (i + 1) xs
  in
  ind 0 l


let rec encoder_lettre encodage c =
  match encodage with
  | [] -> failwith "La liste d'encodage est vide"
  | (entier, liste_char)::reste ->
      if List.mem c liste_char then
        match find_index (fun x -> x = c) liste_char with
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
let dupliquer (pause) (a, b) =
  let sep = if pause then [0] else [] in
  sep @ (let rec aux (x, n) =
              if n <= 0 then []
              else x :: aux (x, n - 1)
            in aux (a, b))

(*Fonction : encoder_mot*)
(*Signature:
Paramètre: 
    - (int*char list)list : type d'encodage
    - string : mot à encoder
Résultat: int list, c'est une liste contenant les touches à appuyer successivement pour générer le mot d'entrer*)
let encoder_mot encodage mot =
  let lettres = Chaines.decompose_chaine mot in (*conversion du mot en liste de caractères*)
  let _, res =
    List.fold_left (fun (touche_avant, acc) c -> (*List.fold_left pour parcourir la liste et accumuler les résultat de chaque étape*)
      let (touche, nb) = encoder_lettre encodage c in (*appel à la fct précédente pour encoder c*)
      let code = dupliquer (touche = touche_avant) (touche,nb) in
      (touche, acc @ code) (*mise à jour*)
    ) (-1, []) lettres (*initialisation de la touche_avant à -1 et acc à liste vide*)
  in
  res 

let%test _ = encoder_mot Encodage.t9_map "cab" = [2;2;2;0;2;0;2;2]
let%test _ = encoder_mot Encodage.t9_map "hello" = [4;4;3;3;5;5;5;0;5;5;5;6;6;6]
let%test _ = encoder_mot Encodage.t9_map "world" = [9;6;6;6;7;7;7;5;5;5;3]


(****Exercice 2****)
let rec decoder_lettre encodage (a,b) =
  match encodage with
  |  [] -> failwith("aucune lettre associée")
  |  t::q -> let (touche, lettres) = t 
            in 
            if a = touche then  List.nth lettres (b-1)
            else decoder_lettre q (a,b)

let%test _= decoder_lettre t9_map (2,2) = 'b'
let%test _= decoder_lettre t9_map (3,2) = 'e'





