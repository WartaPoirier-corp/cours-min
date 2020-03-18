(*type synonyme*)
type euros=float;;
let x:euros=5.;;
let y:float=3.;;
x+.y;;
let ajoute_un_euro (somme:euros):euros = somme+.1.;;
ajoute_un_euro x;;
ajoute_un_euro y;;

(*type enumere*)
type enseigne = Trefle| Coeur| Carreau |Pique;;
type couleur = Rouge | Noir;;
let x:couleur=Rouge;;

let couleur_enseigne (e:enseigne) : couleur =
  if e=Trefle||e=Pique then Noir else Rouge;;

Rouge=Noir;;
Rouge=Trefle;;
Trefle<Coeur;;

couleur_enseigne Trefle;;

let couleur_enseigne (e:enseigne) : couleur =
  match e with
  |Trefle->Noir
  |Pique->Noir
  |_->Rouge;;(*e=Coeur ou Carreau*)

couleur_enseigne Coeur;;
x;;

(*type produit*)

type hauteur=As|Deux|Roi;;
type carte=hauteur*enseigne;;
let as_de_coeur=(As,Coeur);;
let couleur_carte (c:carte):couleur=
  let (h,e)=c in couleur_enseigne e;;
couleur_carte as_de_coeur;;

let couleur_carte (c:carte):couleur=
  let (_,e)=c in couleur_enseigne e;;

let couleur_carte (_,e:carte):couleur=
  couleur_enseigne e;; 

let couleur_carte (c:carte):couleur=
  match c with
  |(_,e)->couleur_enseigne e;;

fst(as_de_coeur);;


(*type union*)
type figure=Valet|Cavalier|Dame|Roi;;
type carte_tarot =
 Basse of int*enseigne (*entre 1 et 10 *)
|Figure of figure*enseigne
|Atout of int (*1 et 21*)
|Excuse;;

let valeur (c:carte_tarot) : int =
  match c with
   |Figure(Roi,_)|Atout(1)|Atout(21)|Excuse->5
   |Figure(Dame,_)->4
   |Figure(Cavalier,_)->3
   |Figure(Valet,_)->2
   |_->1;;
