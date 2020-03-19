(*Fonction Recursive*)
let fact_sans_rec (n:int):int=
  if n=0 then 1 else n*fact_sans_rec (n-1);; (*On a oublié le mot clef rec*)

let rec fact (n:int):int=
  if n=0 then 1 else n*fact (n-1);;

fact 6;;

fact (-2);;

let rec fib n =
  match n with
  |0|1 ->1
  |_->fib (n-1) +fib (n-2);;

fib 10;;
fib 20;;
fib 30;;

(*Type recursif*)
type t = C of char| S of int*t;;

C('a');;

S(1,S(2,C('b')));;

type t2 = Noeud of t2*t2;; (*type pas bien fondé*)
type tree =Noeud of tree*tree | I of int;;

Noeud (Noeud (I 2, I 3),I 4);;

type int_liste = Vide | Cons of int * int_liste;; (*liste d'entier*)
Cons(1,Cons(2,Cons(3,Vide)));;
