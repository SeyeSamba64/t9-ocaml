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

(*List.mem : mem a set est vrai si et seulement si est égal à un élément de .aset
List.ond : *)

(* Fonction auxiliaire à ajouter en haut *)
let find_index f l =
  let rec loop i = function
    | [] -> None
    | x :: xs -> if f x then Some i else loop (i + 1) xs
  in
  loop 0 l


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


(*
  encoder_mot : encodage −> string −> int list )
  Fonction qui calcule la suite de touche à presser pour saisir un mot passé en paramètre
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre mot : string, le mot à encoder
  Résultat : int list, la liste des touches à presser pour saisir le mot
*)

let rec encoder_mot encodage mot =
  match mot with
  | "" -> []  (* Si le mot est vide, on retourne une liste vide *)
  | _ ->
      let c = String.get mot 0 in  (* On prend le premier caractère du mot *)
      let (touche, appuis) = encoder_lettre encodage c in  (* On encode la lettre *)
      let reste = String.sub mot 1 (String.length mot - 1) in  (* On prend le reste du mot *)
      if reste = "" then [touche] else touche :: (List.init appuis (fun _ -> touche)) @ encoder_mot encodage reste

let%test _ = encoder_mot Encodage.t9_map "abc" = [2; 0; 2; 0; 2]
let%test _ = encoder_mot Encodage.t9_map "hello" = [4; 3; 3; 5; 6]
let%test _ = encoder_mot Encodage.t9_map "world" = [9; 6; 7; 5; 3]



(*****version 2*****)


(*Fonction nbr_appuie pour nous aider à trouver le nombre de fois qu'il faut appuyer sur une touche*)
(*Signature:
Paramètre:
- c : int , touche à appuyer
- liste : liste de char contenant les lettres associée à la touche c
Résultat: int , nombre de fois à appuyer sur la touche
Précondition: liste non vide et c entre 2 et 9 *)
let rec nbr_appuie c liste =
  match liste with
    |[]   -> failwith("aucune touche associée")
    |t::q ->  if t = c then 1 
              else 1 + nbr_appuie c q


(*Fonction: encoder_lettre*)
(*Signature: 
Paramètre: 
    - encodage : type dencodage utilisé
    - char : la lettre qu'on cherche à encoder
Résultat: int pour la touche, int pour le nombre de fois qu'il aut appuyer
Précondition: char doit être entre 'a' et 'z'*)


let rec encoder_lettre encodage c =
  match encodage with
    |[]   ->  failwith("aucune touche associée")
    |t::q ->  let (touche,list_lettre) = t 
              in
              if List.mem c list_lettre then (touche, (nbr_appuie c list_lettre)) (*List.mem pour vérifier si la lettre est dans la liste des lettres courante*)
              else encoder_lettre q c
    


let%test _ = encoder_lettre t9_map 'a' = (2, 1)


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
  let lettres = List.init (String.length mot) (String.get mot) in (*convertir le mot en une liste de caractère*)
  let _, res =
    List.fold_left (fun (touche_avant, acc) c -> (*List.fold_left pour parcourir la liste et accumuler les résultat de chaque étape*)
      let (touche, nb) = encoder_lettre encodage c in (*appel à la fct précédente pour encoder c*)
      let code = dupliquer (touche = touche_avant) (touche,nb) in
      (touche, acc @ code) (*mise à jour*)
    ) (-1, []) lettres (*initialisation de la touche_avant à -1 et acc à liste vide*)
  in
  res 

let%test _ = encoder_mot t9_map "cab" = [2;2;2;0;2;0;2;2]


