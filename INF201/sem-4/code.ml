let rec
    pair (n:int):bool=if (n=0) then true else impair (n-1)
  and
    impair (n:int):bool= if (n=0) then false else pair (n-1);;

pair 100;;
pair 101;;
impair 100;;

type coef= int;;
type degre= int;;
type monome = coef*degre;;
type polynome = Mn of monome| Plus of monome*polynome;;
(*buggy*) let p=Plus (Mn (3,2), Plus (Mn (2,1), Mn(5,0)));;  (*p=3x^2+2x+5*)
let p=Plus ((3,2), Plus ((2,1), Mn(5,0)));;  (*p=3x^2+2x+5*)

let derive_monome ((a,d):monome):monome=
  match d with
    0->(0,0) (*derive une constante*)
   |_->(d*a,d-1);;(*ax^d se derive en dax^(d-1) pour d>0*)

let rec derive (p:polynome): polynome=
  match p with
  |Mn m-> Mn (derive_monome m)
  |Plus (m, q)-> Plus ((derive_monome m), derive q);;

p;;
derive p;;
derive derive p;;
derive (derive p);;

type ens= Vide |Ins of int*ens;;

let rec nb_elem (e:ens):int=
  match e with
  |Vide->0
  |Ins (x,ee)->1+nb_elem ee;;

let ens1= Ins (2 , Vide);;
let ens2= Ins (3 , ens1);;
nb_elem ens2;;
let rec somme (e:ens):int=
  match e with
  |Vide->0
  |Ins (x,ee)->x+somme ee;;
somme ens2;;

