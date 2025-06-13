open Encodage
open Chaines

(* Saisie des mots en mode T9 *)

(****Exercice 4****)

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


(*-----------------------------------------------------------------------------------------------------------*)

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

(*-----------------------------------------------------------------------------------------------------------*)

(*
  empty : dico
  Constante qui crée un dictionnaire vide
  Résultat : dico, un dictionnaire vide
*)
type dico = Noeud of ( string list * ( int * dico ) list )
let empty = Noeud ([], [])

(*-----------------------------------------------------------------------------------------------------------*)

(*
  recherche : int -> (int * dico) list -> dico option
  Fonction qui recherche si une branche numérotée existe dans une liste de branches
  Paramètre c : int, le numéro de la branche à rechercher
  Paramètre lb : (int * dico) list, la liste des branches du dictionnaire
  Résultat : dico option, Some dico si la branche existe, None sinon
*)

let rec recherche c lb =
  match lb with
  | [] -> None
  | (tc, ta) :: q ->
      if c < tc then None
      else if c = tc then Some ta
      else recherche c q

let%test _ = recherche 2 [(2, empty); (3, empty)] = Some empty
let%test _ = recherche 3 [(2, empty); (3, empty)] = Some empty

(*
  maj : int -> dico -> (int * dico) list -> (int * dico) list
  Fonction qui met à jour la branche numérotée c dans une liste de branches :
  Paramètre c : int, le numéro de la branche à mettre à jour
  Paramètre nouvelle_branche : dico, le nouveau dictionnaire à associer à la branche c
  Paramètre lb : (int * dico) list, la liste des branches du dictionnaire
  Résultat : (int * dico) list, la liste des branches mise à jour
  
*)

let rec maj c nouvelle_branche lb =
  match lb with
  | [] -> [c, nouvelle_branche]
  | (tc, ta) :: q ->
      if c < tc then (c, nouvelle_branche) :: lb
      else if c = tc then (c, nouvelle_branche) :: q
      else (tc, ta) :: maj c nouvelle_branche q

let%test _ = maj 2 empty [(2, empty); (3, empty)] = [(2, empty); (3, empty)]
let%test _ = maj 4 empty [(2, empty); (3, empty)] = [(2, empty); (3, empty); (4, empty)]
(*
  ajouter : encodage −> dico −> string −> dico
  Fonction qui ajoute un mot dans le dictionnaire
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre dico : dico, le dictionnaire dans lequel on ajoute le mot
  Paramètre mot : sttring, le mot à ajouter
  Résultat : dico, le dictionnaire mis à jour avec le mot ajouté
*)

let ajouter encodage dico mot =
  let lc = encoder_mot encodage mot in
  let rec ajout lc (Noeud (mots, lb)) =
    match lc with
    | [] -> 
        if List.mem mot mots then Noeud (mots, lb)
        else Noeud (mot :: mots, lb)
    | c :: qlc ->
        let sous_arbre =
          match recherche c lb with
          | None -> Noeud ([], [])
          | Some n -> n
        in
        Noeud (mots, maj c (ajout qlc sous_arbre) lb)
  in  ajout lc dico

let%test _ = ajouter Encodage.t9_map empty "bon" = 
Noeud ([], [(2, Noeud ([], [(6, Noeud ([], [(6, Noeud (["bon"], []))]))]))])

let%test _ = ajouter Encodage.t9_map empty "bonjour" =
Noeud
 ([],
  [(2,
    Noeud
     ([],
      [(6,
        Noeud
         ([],
          [(6,
            Noeud
             ([],
              [(5,
                Noeud
                 ([],
                  [(6,
                    Noeud
                     ([], [(8, Noeud ([], [(7, Noeud (["bonjour"], []))]))]))]))]))]))]))])

(*-----------------------------------------------------------------------------------------------------------*)

(*
  creer_dico : encodage −> string −> dico
  Fonction qui construit un dictionnaire à partir d’un encodage et d’un fichier 
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre fichier : string, le chemin d'accès au fichier contenant les mots à ajouter au dictionnaire
  Résultat : dico, le dictionnaire construit à partir des mots du fichier
*)

let creer_dico encodage fichier =
  let rec lire_fichier texte dico =
    try
      let ligne = input_line texte in  (* On lit une ligne du fichier *)
      lire_fichier texte (ajouter encodage dico ligne)  (* On ajoute le mot et on continue à lire *)
    with End_of_file -> dico  (* Quand on atteint la fin du fichier, on retourne le dictionnaire *)
  in
  let texte = open_in fichier in  (* On ouvre le fichier en lecture *)
  let dico = lire_fichier texte empty in  (* On lit le fichier et construit le dictionnaire *)
  close_in texte;  (* On ferme le fichier *)
  dico

let dico_test = creer_dico Encodage.t9_map "dico_fr.txt"

(*
  supprimer : encodage −> dico −> string −> dico
  Fonction qui supprime un mot à un dictionnaire
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre dico : dico, le dictionnaire dans lequel on supprime le mot
  Paramètre mot : string, le mot à supprimer
  Résultat : dico, le dictionnaire mis à jour sans le mot supprimé
  précondition : le mot doit exister dans le dictionnaire
  postcondition : Cette fonction devra élaguer les branches éventuellement devenues inutiles du
dictionnaire
*)

(*-----------------------------------------------------------------------------------------------------------*)

(*let supprimer encodage dico mot =
  let lc = encoder_mot encodage mot in
  let rec *)



(*
  appartient : encodage −> dico −> string −> bool
  Fonction qui vérifie si un mot appartient au dictionnaire
  Paramètre encodage : (int * char list), la liste associant les touches du clavier numérique à des lettres
  Paramètre dico : dico, le dictionnaire dans lequel on cherche le mot
  Paramètre mot : string, le mot à vérifier
  Résultat : bool, true si le mot appartient au dictionnaire, false sinon
*)

let appartient encodage dico mot =
  let lc = encoder_mot encodage mot in  
  let rec trouver lc (Noeud (mots, lb)) =
    match lc with
    | [] -> List.mem mot mots  
    | c :: qlc ->
        match recherche c lb with
        | None -> false        
        | Some sous_branche -> trouver qlc sous_branche
  in
  trouver lc dico

let%test _ = appartient Encodage.t9_map dico_test "samba" = true
let%test _ = appartient Encodage.t9_map dico_test "ikram" = false

(*-----------------------------------------------------------------------------------------------------------*)



