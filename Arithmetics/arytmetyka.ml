(***********************************)
(* MIMUW/WPF/ARYTMETYKA            *)
(* autor: Bartosz Smolarczyk       *)
(* code review: Kacper Lewandowski *)
(***********************************)
(*
Podczas działania programu aktualne wartości, na których wykonywane są operacje,
przechowywane będą na liście par (float, float), gdzie pary reprezentować będą
aktualne przedziały możliwych wartości. W szczególności, pusta lista oznaczać
będzie zbiór pusty.
*)

type wartosc = (float * float) list;;


(*
sortuj_przedzialy - funkcja wykorzystywana jako komparator podczas sortowania
listy par.
*)
let sortuj_przedzialy (x, y) (x', y') =
    let compare_fst = compare x x' in
    if (compare_fst <> 0) then compare_fst
    else compare y y';;

(*
analiza_zer - funkcja pomocnicza sprawdzająca, czy któryś z przedziałów
na liście zawiera zero. Dla zbioru pustego zwraca zbiór pusty.
Jeżeli sprawdzany przedział nie zawiera zera, zero jest jednym z jego
krańców, lub obydwoma z jego krańców (wartość dokładna 0) to pozostaje
bez zmian. W przeciwnym wypadku zostaje podzielony na przedziały od
krańca lewego do -0., i od 0. do krańca prawego. Ostatni przypadek match
jest uzupełniem, aby rozważone były wszystkie przypadki. Nie zajdzie on
jednak nigdy, ponieważ pary przechowywane na liście są uporządkowane
i zawsze a <= b, co rozpatrują wcześniejsze przypadki. 
*)
let analiza_zer x =
    if (x = []) then [] else
    let rec f lst acc =
        match lst with
        | [] -> List.sort sortuj_przedzialy acc
        | h::t ->
            match h with
            | (a, b) when (a < 0. && b < 0.) -> f t ((a, b) :: acc)
            | (a, b) when (a > 0. && b > 0.) -> f t ((a, b) :: acc)
            | (a, b) when (a < 0. && b > 0.) -> f t ((a, -0.) :: (0., b) :: acc)
            | (a, b) when (a < 0. && b = 0.) -> f t ((a, -0.) :: acc)
            | (a, b) when (a = 0. && b > 0.) -> f t ((0., b) :: acc)
            | (a, b) when (a = 0. && b = 0.) -> f t ((0., 0.) :: acc)
            | _ -> [(neg_infinity, neg_infinity)]
    in f x [];;

(*
suma_przedzialow - funkcja przyjmuje listę przedziałów, które mogą na
siebie nachodzić i zwraca listę będącą sumą tych przedziałów taką, że
na siebie nie nachodzą.
*)
let suma_przedzialow x = 
    if(x = []) then [] else
    let rec f lst acc beg end_prev =
        match lst with
        | [] -> analiza_zer (List.sort sortuj_przedzialy ((beg, end_prev) :: acc))
        | h::t ->
            if (fst h > end_prev) then f t ((beg, end_prev) :: acc) (fst h) (snd h)
            else f t acc beg (max (end_prev) (snd h))
    in f x [] (fst (List.hd x)) (snd (List.hd x));;


(* wartosc_dokladnosc x p = x +/- p% *)
let wartosc_dokladnosc x p =
    let p = 0.01 *. p in
    let val1 = x -. (p *. x) in
    let val2 = x +. (p *. x) in
    suma_przedzialow [(min val1 val2, max val1 val2)];;

(* wartosc_od_do x y = [x;y] lub [x; -0.] U [0.;y] gdy x < 0, y > 0*)
let wartosc_od_do x y =
    suma_przedzialow [(x, y)];;

(* wartosc_dokladna x = [x;x], dla x = -0. zwraca [0., 0.]*)
let wartosc_dokladna x =
    suma_przedzialow [(x, x)];;

(* in_wartosc w x = x \in w *)
let in_wartosc x y = 
    let rec czy_nalezy lst a =
    match lst with
    | [] -> false
    | h::t -> if (fst h <= a && snd h >= a) then true else czy_nalezy t a
    in czy_nalezy x y;;

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc x =
    match x with
    | [] -> nan (*infinity*)
    | h::t -> fst h;;

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
let max_wartosc x =
    let x = List.rev x in
    match x with
    | [] -> nan (*neg_infinity*)
    | h::t -> snd h;;

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
let sr_wartosc x = 
    if x = [] then nan else (min_wartosc x +. max_wartosc x) /. 2.;;


(* dodawanie *)
let plus a b =
    let rec dodaj_do_przedzialu przedzial dod acc =
        match dod with
        | [] -> acc
        | h::t -> dodaj_do_przedzialu przedzial t (((fst przedzial +. fst h), (snd przedzial +. snd h)) :: acc)
    in
    let rec dodawanie_przedzialami lst dod acc =
        match lst with
        | [] -> suma_przedzialow (List.sort sortuj_przedzialy acc)
        | h::t -> dodawanie_przedzialami t dod ((dodaj_do_przedzialu h dod []) @ acc)
    in dodawanie_przedzialami a b [];;

(* odejmowanie *)
let minus a b =
    let rec odejmij_od_przedzialu przedzial od acc =
        match od with
        | [] -> acc
        | h::t -> odejmij_od_przedzialu przedzial t (((fst przedzial -. snd h), (snd przedzial -. fst h)) :: acc)
    in
    let rec odejmowanie_przedzialami lst od acc = 
        match lst with
        | [] -> suma_przedzialow (List.sort sortuj_przedzialy acc)
        | h::t -> odejmowanie_przedzialami t od ((odejmij_od_przedzialu h od []) @ acc)
    in odejmowanie_przedzialami a b [];;

(* mnożenie, przyjmujemy, że infinity * 0 = 0 oraz neg_infinity * 0 = 0 *)
let razy a b = 
    let rec mnoz_przedzial przedzial czynniki acc =
        match czynniki with
        | [] -> acc
        | h::t -> 
            let val1 = if(fst przedzial = 0. || fst h = 0.) then 0. else fst przedzial *. fst h in
            let val2 = if(fst przedzial = 0. || snd h = 0.) then 0. else fst przedzial *. snd h in
            let val3 = if(snd przedzial = 0. || fst h = 0.) then 0. else snd przedzial *. fst h in
            let val4 = if(snd przedzial = 0. || snd h = 0.) then 0. else snd przedzial *. snd h in
            mnoz_przedzial przedzial t (((min val1 (min val2 (min val3 val4))),(max val1 (max val2 (max val3 val4)))) :: acc)
        in
    let rec mnozenie_przedzialami lst czynniki acc = 
        match lst with
        | [] -> suma_przedzialow (List.sort sortuj_przedzialy acc)
        | h::t -> mnozenie_przedzialami t czynniki ((mnoz_przedzial h czynniki []) @ acc)
    in mnozenie_przedzialami a b [];;

(* dzielenie, zbiór pusty będzie wynikiem tylko przy dzieleniu przez dokładnie 0, 
w przeciwnym razie zwracamy granicę z dzielenia przez okolice 0. Dodatkowo 
infinity / infinity = infinity, neg_infinity / neg_infinity = infinity *)
let podzielic a b =
    let rec dziel_przedzial przedzial dzielniki acc =
        match dzielniki with
        | [] -> acc
        | h::t ->
            if h = (0., 0.) then [] else
            let val1 = if(fst przedzial = 0. && fst h = 0.) then 0. 
                else if (fst przedzial = neg_infinity && fst h = neg_infinity) then infinity
                else fst przedzial /. fst h in
            let val2 = if(fst przedzial = 0. && snd h = 0.) then 0. else fst przedzial /. snd h in
            let val3 = if(snd przedzial = 0. && fst h = 0.) then 0. else snd przedzial /. fst h in
            let val4 = if(snd przedzial = 0. && snd h = 0.) then 0. else
                if(snd przedzial = infinity && snd h = infinity) then infinity
                else snd przedzial /. snd h in
            dziel_przedzial przedzial t (((min val1 (min val2 (min val3 val4))),(max val1 (max val2 (max val3 val4)))) :: acc)
        in
    let rec dzielenie_przedzialami lst dzielniki acc =
        match lst with
        | [] -> suma_przedzialow (List.sort sortuj_przedzialy acc)
        | h::t -> dzielenie_przedzialami t dzielniki ((dziel_przedzial h dzielniki []) @ acc)
    in dzielenie_przedzialami (a) (b) [];;




(* kilka testów poprawnościowych *)
(*
    assert((min_wartosc (wartosc_dokladna 0.) ) = (max_wartosc (wartosc_dokladna (-0.))));;

    let a = wartosc_od_do 2. 6.;;
    let b = wartosc_od_do (-2.) 4.;;

    assert((min_wartosc (razy (b) (podzielic (podzielic (a) (b) ) (b) ) ) ) = (neg_infinity));;
    assert((max_wartosc (razy (b) (podzielic (podzielic (a) (b) ) (b) ) ) ) = (infinity));;

    let c = podzielic a b;; (* = [neg_infinity; -1.] U [0.5; infinity] *)
    let d = podzielic c b;; (* = [neg_infinity; -0.25] U [0.125; infinity] *)

    assert(max_wartosc d = infinity);;
    assert(min_wartosc d = neg_infinity);;
    assert(in_wartosc d (-0.25) = true);;
    assert(in_wartosc d 0.125 = true);;
    assert(in_wartosc d 0. = false);;

    let e = podzielic (wartosc_dokladna 1.) (wartosc_od_do (-1.) (-0.));; (* [neg_infinity; -1.] *)
    let f = podzielic e e;; (* [0.; infinity] *)

    assert(min_wartosc f = 0.);;
    assert(max_wartosc f = infinity);;
    assert(in_wartosc f (-1.) = false);;

    let g = razy f (wartosc_dokladna 0.);;
    assert(sr_wartosc g = 0.);;

*)