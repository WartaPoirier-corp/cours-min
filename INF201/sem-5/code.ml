type list_int = Nil | Cons of int*list_int;;

let l1= Cons(1,Nil);;
let l2= Cons(2,l1);;
Cons(2,Cons(1,Nil))=l2;;
Cons(1,Cons(2,Nil))=l2;;
[1;2];;(*prochain cours listes natives*)

Cons(true,Cons(4,Nil));;(*erreur de type*)

type int_or_bool = Int of int| Bool of bool;;
[Int 4;Bool true];;

type list_int_or_bool=Nil|ConsInt of int * list_int_or_bool| ConsBool of bool*list_int_or_bool;;
ConsInt(3,ConsBool(true,Nil));;

type list_non_vide = Int of int | Cons2 of int*list_non_vide;;
Cons2(1,Int (2));;

type list_impair= Int of int | ConsImpair of int*int*list_impair;;

type autant_a_et_b = Nil | Cons3 of char*autant_a_et_b*char*autant_a_et_b;;
(*le premier char et le second sont different a valeur dans a,b*)

let singleton (x:int): list_int = Cons(x,Nil);;

let tete (l:list_int) :int=
  match l with
  |Nil-> failwith"Liste vide"
  |Cons(x,_)->x;;

let queue (l:list_int):list_int=
   match l with
  |Nil-> failwith"Liste vide"
  |Cons(_,q)->q;;
l2;;
tete l2;;
tete Nil;;
queue l2;;
let la_tete_vaut_0 (l:list_int): bool=
  match l with
  |Cons(0,_)->true
  |_->false;;
let second (l:list_int):int=
  match l with
  |Cons(_,Cons(e2,_))->e2;;

second l2;;
second Nil;;
let secondbis (l:list_int):int=
  match l with
  |Cons(_,Cons(e2,_))->e2
  |_->failwith "la liste ne contient pas deux elements";;
secondbis Nil;;

let rec appartient (x:int) (l:list_int) : bool=
  match l with
  |Nil->false
  |Cons(e,q)->if (x=e) then true else appartient x q;;

let rec appartient (x:int) (l:list_int) : bool=
  match l with
  |Nil->false
  |Cons(e,q)->(x=e) || (appartient x q);;

let rec appartient_buggy (x:int) (l:list_int) : bool=
  match l with
  |Nil->false
  |Cons(x,q)->true (*attention ca ne marchera pas, le x ici est redefini comme dans un let x in*)
  |Cons(_,q)-> appartient x q;;

l2;;
appartient_buggy 3 l2;;

let rec dernier (l:list_int): int=
  match l with
  |Nil->failwith "la liste Vide n'a pas de dernier element"
  |Cons(x,Nil)->x
  |Cons(_,q)->dernier q;;

dernier l2;;
