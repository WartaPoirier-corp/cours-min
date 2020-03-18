let malist=1::2::3::[];;
let malist2=[1;2;3];;
malist=malist2;;
let maliste3=[1,2,3];;
0::malist;;

let rec ajoutenqueue (l1: int list) (e2:int)=
  match l1 with
  |[]->[e2]
  |e1::ll->e1::(ajoutenqueue ll e2);;
malist;;
ajoutenqueue malist 12;;

let rec appartient (x: 'a)(l : 'a list) : bool =
  match l with
    [] -> false
   |e::s -> e=x || (appartient x s);;

appartient 1 malist;;
appartient 1.2 malist;;
appartient 100 malist;;
appartient true [true;false;true];;
appartient 3.4 [2.1;3.4];;

let rec appartient2 x l =;(*les types sont inférés par l'interpréteur*)
  match l with
  |[] -> false
  |e::s -> e=x || (appartient x s);

let rec appartientbuggy x l =
  match l with
  |[] -> false
  |x::s->true (*ici x est defini localement et est n'importe quoi, c'est comme let x in ici, ca remplace localement le parametre x de la fonction*)
  |_::s ->(appartientbuggy x s);;

appartientbuggy 4 [1;2;3];;

List.tl malist;;
List.tl [];;
List.hd [];;
List.hd [2;3;4];;
[1;2;4]@[2;5];;
List.nth [1;2;3] 2;;
List.nth [1;2;3] 0;;
List.nth [1;2;3] (-1);;
List.nth [1;2;3] 5;;

let rec inserer e l=
(* l est croissante, le resultat est une liste croissante *)
  match l with
  | [] -> [e]
  | x::s -> if e<x then e::l else x::(inserer e s);;

inserer 2 [1;3;4];;

let rec triparinsertion l=match l with
  |[]->[]
  |x::ll->inserer x (triparinsertion ll);;

triparinsertion [2;3;1;8;7;9;10;4;6;5];;

type prenom=Tata|Titi|Toto;;(*les elements d'un type enumere sont dans l'ordre dans lesquels on les defini*)
inserer Toto [Tata;Titi];;
