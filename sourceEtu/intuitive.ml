open Encodage
open Chaines

(* Saisie des mots en mode T9 *)

(* 
  encoder_lettre : encodage −> char −> int 
  Fonction qui indique la touche qu’il faut appuyer pour saisir la lettre passée en paramètre
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre c : char, la lettre à encoder
*)

let rec encoder_lettre encodage c =
  match encodage with
  | [] -> failwith "La liste d'encodage est vide"
  | (entier, liste_char)::reste ->
      if List.mem c liste_char then 
        entier  (* Si la lettre est dans la liste, on retourne le numéro de la touche *)
      else
        encoder_lettre reste c  (* On continue à chercher dans le reste de la liste *)

let%test _ = encoder_lettre Encodage.t9_map 'a' = 2
let%test _ = encoder_lettre Encodage.t9_map 'f' = 3
let%test _ = encoder_lettre Encodage.t9_map 'z' = 9


(* 
  encoder_mot : encodage -> string -> int list
  Fonction qui encode un mot en une liste d'entiers représentant les touches à appuyer
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre mot : string, le mot à encoder
  Résultat : int list, la liste des touches à appuyer pour saisir le mot
*)

let rec encoder_mot encodage mot =
  let lettres = Chaines.decompose_chaine mot in
  match lettres with
  | [] -> [] (* Si la liste de lettres est vide, on retourne une liste vide *)
  | t::q -> 
      (* On encode la première lettre et on concatène avec l'encodage du reste du mot *)
      (* encoder_lettre retourne le numéro de la touche à appuyer pour la lettre t *)
      let touche = encoder_lettre encodage t in
      touche :: encoder_mot encodage (Chaines.recompose_chaine q)  (* On concatène le reste des lettres pour continuer l'encodage *)

let%test _ = encoder_mot Encodage.t9_map "abc" = [2; 2; 2]
let%test _ = encoder_mot Encodage.t9_map "hello" = [4; 3; 5; 5; 6]    
let%test _ = encoder_mot Encodage.t9_map "world" = [9; 6; 7; 5; 3]
