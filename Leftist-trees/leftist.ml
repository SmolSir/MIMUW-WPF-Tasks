(***********************************)
(* MIMUW/WPF/DRZEWA LEWICOWE       *)
(* autor: Bartosz Smolarczyk       *)
(* code review: Jakub Piotrowicz   *)
(***********************************)
(*
Kolejka priorytetowa funkcjonowała będzie przy użyciu
drzew lewicowych. W każdym węźle trzymane są kolejno
lewe poddrzewo, wartość w węźle, długość najkrótszej
ścieżki od węzła do liścia (w szczególności odległość
ta dla liścia równa jest 0), prawe poddrzewo.
*)
type 'a queue =
  Node of ('a queue) * ('a) * (int) * ('a queue) 
| Null;;


(* Wyjątek podnoszony przez delete_min, gdy kolejka jest pusta. *)
exception Empty;;


(* Tworzenie pustej kolejki priorytetowej. *)
let empty = Null;;


(*
Sprawdza, czy kolejka priorytetowa jest pusta (true).
Jeśli nie jest (false).
*)
let is_empty q = 
    match q with
    | Null -> true
    | _ -> false;;


(*
Funkcje pomocnicze zwracające poszczególne elementy dla danego
węzła drzewa lewicowego. Nigdy nie zostaną wywołane dla pustego
drzewa.
*)
let left_subtree q = 
    match q with
    | Null -> assert false
    |Node (a, b, c, d) -> a;;
let value q = 
    match q with
    | Null -> assert false
    |Node (a, b, c, d) -> b;;
let length q = 
    match q with
    | Null -> assert false
    |Node (a, b, c, d) -> c;;
let right_subtree q = 
    match q with
    | Null -> assert false
    |Node (a, b, c, d) -> d;;


(* Zwraca złączenie kolejek [qL] i [qR]. *)
let rec join qL qR =
    (* wstępne sprawdzenie kolejek - jeśli któraś jest pusta, wynikiem funkcji jest druga.
    Jeśli obie są puste, wynikiem jest pusta kolejka. *)
    if(qL = Null || qR = Null) then
        match (qL, qR) with
            | (Null, Null) -> Null
            | (Null, _) -> qR
            | (_, Null) -> qL
            | (_,_) -> assert false
    else
    (* Sprawdzam, czy element w korzeniu qL jest większy niż w korzeniu qR.
    Jeśli tak - zamieniam je ze sobą, jeśli nie - pozostawiam bez zmian. *)
    let qL, qR = if (value qL > value qR) then qR, qL else qL, qR
    in
    (* Wprowadzam pomocniczo wartości poszczególnych elementów w korzeniu qL. *)
    let lq_subtree_L = left_subtree qL in
    let lq_val = value qL in
    let lq_length = length qL in
    let lq_subtree_R = right_subtree qL in

    (* Rozbudowuję prawe poddrzewo korzenia qL o qR. *)
    let lq_subtree_R = join (lq_subtree_R) qR
    in
    (* Jeżeli aktualny korzeń qL nie ma lewego poddrzewa, to zwracam
    przetworzone drzewo, przy czym lewe poddrzewo zastępuję dotychczasowym
    prawym, a prawe lewym, by zachowany był warunek lewicowości. *)
    if (lq_subtree_L = Null) then Node (lq_subtree_R, lq_val, 0, lq_subtree_L)
    else
    (* W przeciwnym razie sprawdzam, które z poddrzew ma krótszą ścieżkę do swojego
    liścia, i zwracam to poddrzewo jako prawe, a drugie poddrzewo jako lewe. *)
    let (lq_subtree_L), (lq_subtree_R) = if (length (lq_subtree_R) > length (lq_subtree_L))
        then (lq_subtree_R), (lq_subtree_L)
        else (lq_subtree_L), (lq_subtree_R)
    in
    Node (lq_subtree_L, lq_val, (lq_length) + 1, lq_subtree_R);;


(* Zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q]. *)
let add e q =
    join (Node (Null, e, 0, Null)) (q);;


(* 
Dla niepustej kolejki [q] delete_min zwraca parę (e, q'), gdzie [e] to
minimalny element kolejki [q], a [q'] to kolejka [q] po usunięciu
elementu [e].
Jeśli kolejka [q] jest pusta, to podnosi wyjątek [Empty].
*)
let delete_min q =
    match q with
    | Null -> raise Empty
    | _ -> ((value q), join (left_subtree q) (right_subtree q));;


(*
(* TESTY *)
(* budowa kolejki używając add, sprawdzenie poprawnej kolejności *)
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 3);;
assert(is_empty b = true);;

(* budowa kolejki używając add, następnie join i sprawdzenie poprawnej kolejności *)
let b = add 2 empty;;
let b = add 4 b;;
let b = add (0) b;;
let b = add 3 b;;
let b = add 2 b;;

let c = add 9 empty;;
let c = add (-4) c;;
let c = add 2 c;;
let c = add 5 c;;
let c = add 1 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-4));;
let (a,b) = delete_min b;;
assert (a = (0));;
let (a,b) = delete_min b;;
assert (a = 1);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 2);;
let (a,b) = delete_min b;;
assert (a = 3);;
let (a,b) = delete_min b;;
assert (a = 4);;
let (a,b) = delete_min b;;
assert (a = 5);;
let (a,b) = delete_min b;;
assert (a = 9);;
assert (try let _=delete_min b in false with Empty -> true);;
*)