let l1=2::[];;
let l2=3::l1;;
l2=[3;2];;
3::2::[]=[3;2];;

(*attention au virgule*)
let l3=[3,2];; (*pareil que [(3,2)], c'est une int*int list a un element*)
l3=[3;2];;

(*attention a l'ordre*)
[3;2]=[2;3];;

[true;false;true];;

[true;2];;

(*liste de liste*)
[[1];[2;3]];;

let rec pairs (l: int list):int list=
  match l with
  |[]->[]
  |e::q->let qpairs= pairs q in if e mod 2=0 then e::qpairs else qpairs;;

pairs [2;3;5;6;8;2;4;6;1];;

let rec rang_pairs  (l: int list):int list=
  match l with
  |[]->[]
  |[e]->[]
  |_::e::q->e::(rang_pairs q);;
istl
rang_pairs [1;2;5;7;8;3];;
rang_pairs [1;2;3];;

let rec supprime (x:int) (l:int list):int list=
  match l with
  |[]->[]
  |e::q->let q2=supprime x q in if x=e then q2 else e::q2;;

let rec concatene (l1:int list) (l2:int list): int list=
  match l1 with
  |[]->l2
  |e::q->e::(concatene q l2);;

concatene [1;2;3] [4;5];;
concatene [true] [false;true];;


let rec concatene2 (l1:'a list) (l2:'a list): 'a list=
  match l1 with
  |[]->l2
  |e::q->e::(concatene2 q l2);;

concatene2 [1;2] [3;4;5];;
concatene2 [true] [false;true];;

let rec concatene3 l1 l2= (*pas besoin de donner le type*)
  match l1 with
  |[]->l2
  |e::q->e::(concatene2 q l2);;

(*concatenation @ different de ::*)
[true]@[true];;
true::[true];;
[true]::[true];;
true@[true];;
