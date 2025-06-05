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

