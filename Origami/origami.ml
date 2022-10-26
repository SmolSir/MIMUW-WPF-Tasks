(***********************************)
(* MIMUW/WPF/origami               *)
(* autor: Bartosz Smolarczyk       *)
(* code review: Jagoda Bracha      *)
(***********************************)

(* Typy reprezentujące punkt i kartkę.
*)

type point = float * float;;
type kartka = point -> int;;

(* eps - ustalony epsilon wykorzystywany przy
*  równościach, w celu uniknięcia niedokładności
*  związanych z liczbami zmiennoprzecinkowymi.
*)
let eps = 1.e-9;;
let (=.) a b = (a -. eps <= b) && (b <= a +. eps);;

(* prostokat - jak w pliku .mli, z dodatkowym 
*  uwzględnieniem eps.
*)
let prostokat (x1, y1) (x2, y2) =
    fun (xk, yk) ->
        if(x1 -. eps <= xk && xk <= x2 +. eps && y1 -. eps <= yk && yk <= y2 +. eps) then 1 else 0;;

(* kolko - jak w pliku .mli, z dodatkowym
*  uwzględnieniem eps.
*)
let kolko (x1, y1) r =
    fun (xk, yk) -> if(r +. eps < Float.hypot (xk -. x1) (yk -. y1)) then 0 else 1;;

(* znak_il_wek - oblicza iloczyn wektorowy
*  i zwraca -1, 0, 1, gdy jest odpowiednio
*  mniejszy od zera, równy zeru z dokładnością
*  do eps, większy od zera.
*)
let znak_il_wek (x2, y2) (xk, yk) =
    let il_wek = (x2 *. yk) -. (xk *. y2) in
    if(il_wek =. 0.) then 0
    else if(il_wek < 0.) then -1
    else 1;;

(* symetria - zwraca odbicie punktu (x2, y2)
*  względem prostej przechodzącej przez punkty
*  (0., 0.) (x1, y2). Otrzymany punkt przesuwa
*  o wektor (xwek, ywek), ponieważ dane na
*  wejściu wektory zaczepione w (0., 0.) są
*  oryginalnie dowolne.
*  Oznaczenia:
*   - a_l = współczynnik kierunkowy prostej 
*        l : a_l * x przechodzącej przez 
*        (0, 0) (x2, y2),
*   - a_k = współczynnik kierunkowy prostej 
*        k prostopadłej do powyższej,
*   - b_k = współczynnik w k : a_k * x + b 
*        taki, że prosta k przechodzi przez 
*        (x2, y2),
*   - x_lk = współrzędna x przecięcia prostych k, l,
*   - y_lk = współrzędna y przecięcia prostych k, l,
*   - wynik = 2 *. S -. P przesunięte o wektor 
*        [xwek, ywek], gdzie S - punkt (x_lk, y_lk),
*        P - punkt (x2, y2). 
*)
let symetria (x1, y1) (x2, y2) (xwek, ywek) =
    if(x1 = 0.) then 
        (-. x2 +. xwek, y2 +. ywek)
    else if (y1 = 0.) then 
        (x2 +. xwek, -. y2 +. ywek)
    else
        let a_l = y1 /. x1 in
        let a_k = (-1.) /. a_l in
        let b_k = y2 -. (a_k *. x2) in
        let x_lk = b_k /. (a_l -. a_k) in
        let y_lk = a_l *. x_lk in
        (2. *. x_lk -. x2 +. xwek, 2. *. y_lk -. y2 +. ywek);;

(* zloz - składa kartkę jak opisano w 
*  pliku .mli, wykorzystując do tego znak_il_wek.
*)
let zloz (x1, y1) (x2, y2) k =
    fun (xk, yk) ->
        let po_lewo = znak_il_wek (x2 -. x1, y2 -. y1) (xk -. x1, yk -. y1) in
        if(po_lewo = -1) then 
            0
        else if(po_lewo = 0) then
            k (xk, yk)
        else
            k (xk, yk) + k (symetria (x1 -. x2, y1 -. y2) (xk -. x2, yk -. y2) (x2, y2));;

(* skladaj - jak w pliku .mli, dodatkowo 
*  wykorzystuje procedury wyższych rzędów, czego
*  wymagano w treści zadania.
*)
let rec skladaj lst k =
    List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k lst;;


(* TESTY *)
(*
(* KÓŁKO *)
let l = [((5., -10.), (5., 100.)); ((5., 0.), (5., 0.01));
	 ((1., 0.), (1., -1.)); ((5., 10.), (1., 0.));
	 ((1., 0.), (5., 10.))];;

let a = kolko (3., 3.) 7.;;

let a = skladaj l a;;

assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;

(* PROSTOKĄT *) 
let l = [((5., 0.), (5., 377.)); ((5., 0.), (5., 1.));
	 ((-6., -6.), (-6.1, -6.1)); ((9., 5.), (4., 2.))];;

let a = prostokat (0., 0.) (10., 10.);;

let a = skladaj l a;;

assert(a (5., 1.) = 7);;
assert(a (5., 2.) = 7);;
assert(a (5., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (9., 5.) = 1);;
assert(a (4., 0.) = 4);;
assert(a (3., 0.) = 4);;
assert(a (2., 0.) = 8);;
assert(a (1., 0.) = 8);;
assert(a (0., 0.) = 0);;
assert(a (0.8, -0.2) = 4);;
assert(a (10., 3.) = 2);;
assert(a (4., 1.) = 8);;
*)