(***********************************)
(* MIMUW/WPF/iSet                  *)
(* autor: Bartosz Smolarczyk       *)
(* code review: Mieszko Grodzicki  *)
(***********************************)

(*
Złożoność - logarytmiczna amortyzowana.
*)

(*
t - typ reprezentujący drzewo. Dany wierzchołek może być
pusty (Null) lub składać się z kolejno lewego syna (left), 
wartości początkowej reprezentowanego przedziału (i_begin),
wartości końcowej reprezentowanego przedziału (i_end), prawego
syna (right), wysokości (height) i liczby (size) określającej
ilość elementów zawartych w jego lewym poddrzewie i w nim samym.
*)
type t =
Node of (t) * (int) * (int) * (t) * (int) * (int)
| Null;;

(*
empty - tworzy puste drzewo.
*)
let empty = Null;;

(*
is_empty - sprawdza, czy dane drzewo jest puste.
*)
let is_empty tree =
    match tree with
    | Null -> true
    | _ -> false;;

(*
Funkcje zwracające poszczególne elementy węzła w drzewie.
*)
let left tree =
    match tree with
    | Node (l, _, _, _, _, _) -> l
    | Null -> Null;;

let i_begin tree =
    match tree with
    | Node (_, x, _, _, _, _) -> x
    | Null -> min_int;;

let i_end tree =
    match tree with
    | Node (_, _, x, _, _, _) -> x
    | Null -> max_int;;

let right tree =
    match tree with
    | Node (_, _, _, r, _, _) -> r
    | Null -> Null;;

let height tree =
    match tree with
    | Node (_, _, _, _, h, _) -> h
    | Null -> 0;;

let size tree =
    match tree with
    | Node (_, _, _, _, _, s) -> s
    | Null -> 0;;

(*
make - łączy dwa poddrzewa i przedział w wierzchołek o powiększonej
o jeden wysokości tego z poddrzew, które ma większą wysokość.
Size tego wierzchołka to size lewego poddrzewa + size prawego
poddrzewa, powiększony o ilość elementów obieranego przedziału.
 *)
let make (left, i_begin, i_end, right) =
    Node (left, i_begin, i_end, right, max (height left) (height right) + 1, size left + size right + i_end - i_begin + 1);;

(*
bal - Wyrównuje wysokość drzewa. Jeśli różnica wysokości lewego
i prawego poddrzewa korzenia jest większa niż dwa, to następuje
odpowiednia rotacja drzewa w celu zmniejszenia tej różnicy.
*)
let bal l i_beg i_end r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, l_i_beg, l_i_end, lr, _, _) ->
            if height ll >= height lr then make (ll, l_i_beg, l_i_end, (make (lr, i_beg, i_end, r)))
            else
                (match lr with
                | Node (lrl, lr_i_beg, lr_i_end, lrr, _, _) ->
                    make ((make (ll, l_i_beg, l_i_end, lrl)), lr_i_beg, lr_i_end, (make (lrr, i_beg, i_end, r)))
                | Null -> assert false)
        | Null -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, r_i_beg, r_i_end, rr, _, _) ->
            if height rr >= height rl then make ((make (l, i_beg, i_end, rl)), r_i_beg, r_i_end, rr)
            else
                (match rl with
                | Node (rll, rl_i_beg, rl_i_end, rlr, _, _) ->
                    make ((make (l, i_beg, i_end, rll)), rl_i_beg, rl_i_end, (make (rlr, r_i_beg, r_i_end, rr)))
                | Null -> assert false)
        | Null -> assert false
    else make (l, i_beg, i_end, r);;

(*
balance - jeśli tree nie jest puste, to wywołuje funkcję bal dla drzewa tree
aż będzie ono wyrównane.
*)
let rec balance tree =
    match tree with
    | Null -> Null
    | _ -> 
        if(abs (height(left tree) - height(right tree)) > 2) then 
            balance (bal (left tree) (i_begin tree) (i_end tree) (right tree))
        else tree;;

(*
min_interval, remove_min_interval - funkcje pomocnicze dla funkcji merge,
zwracają odpowiednio najmniejszy przedział w tree oraz tree po usunięciu
najmniejszego przedziału.
*)
let rec min_interval tree =
    match tree with
    | Node (Null, i_begin, i_end, _, _, _) -> (i_begin, i_end)
    | Node (left, _, _, _, _, _) -> min_interval left
    | Null -> raise Not_found;;

let rec remove_min_interval tree =
    match tree with
    | Null -> invalid_arg "iSet.remove_min_interval"
    | Node (Null, _, _, right, _, _) -> right
    | Node (left, i_begin, i_end, right, _, _) -> bal (remove_min_interval left) i_begin i_end right;;

(*
merge - łączy dwa drzewa w jedno, po czym je wyrównuje. Wszystkie elementy
tree1 są zawsze mniejsze od elementów tree2.
*)
let merge tree1 tree2 =
    match tree1, tree2 with
    | Null, _ -> tree2
    | _, Null -> tree1
    | _, _ -> 
        let k = min_interval tree2 in
        balance (make (tree1, (fst k), (snd k), (remove_min_interval tree2)));;


(*
remove - usuwa z drzewa elementy należące do zadanego przedziału
i zwraca wyrównane drzewo niezawierające tych elementów.
*)
let remove interval tree =
    let rem_begin = fst interval in
    let rem_end = snd interval in
    let rec func tree =
        match tree with
        | Null -> Null
        | Node (left, i_begin, i_end, right, h, s) ->
            if(rem_end < i_begin) then 
                let l = balance (func left) in
                balance(make(l, i_begin, i_end, right))
                else
            if(i_end < rem_begin) then
                let r = balance (func right) in
                balance (make(left, i_begin, i_end, r)) else
            if(rem_begin <= i_begin && i_end <= rem_end) then
                let l = balance (func left) in
                let r = balance (func right) in
                balance (merge l r) else
            if(rem_begin <= i_begin && rem_end < i_end) then 
                let l = balance (func left) in
                balance (make(l, rem_end + 1, i_end, right)) else
            if(i_begin < rem_begin && i_end <= rem_end) then
                let r = balance (func right) in
                balance (make(left, i_begin, rem_begin - 1, r))
            else
                balance (make(left, i_begin, rem_begin - 1, make(Null, rem_end + 1, i_end, right)))
    in func tree;;

(*
add - dodaje do drzewa elementy z zadanego przedziału
i zwraca wyrównane drzewo. Funkcje extend poszerzają
(o ile to możliwe) zadany przedział tak, żeby jego krańce
były krańcami pewnych już zawartych w drzewie przedziałów
lub by nie należały do żadnego. Następnie z użyciem tak
powiększonego przedziału wykonywana jest operacja remove.
Do otrzymanego wyrównanego drzewa wstawiamy wyznaczony
przedział i wyrównujemy je.
*)
let add interval tree =
    let rec extend_beg x tree =
        match tree with
        | Null -> x
        | Node(left, i_begin, i_end, right, _, _) ->
            if(i_begin <= x && (i_end = max_int || x <= i_end + 1)) then 
                i_begin else
            if(x < i_begin) then
                extend_beg x left
            else
                extend_beg x right
    in 
    let add_begin = extend_beg (fst interval) tree in
    let rec extend_end x tree =
        match tree with
        | Null -> x
        | Node(left, i_begin, i_end, right, _, _) ->
            if(i_begin <= x + 1 && x <= i_end) then
                i_end else
            if(x + 1 < i_begin) then
                extend_end x left
            else
                extend_end x right
    in
    let add_end = extend_end (snd interval) tree in
    let new_tree = remove(add_begin, add_end) tree in
    let rec insert tree =
        match tree with
        | Null -> make(Null, add_begin, add_end, Null)
        | Node(l, int_begin, int_end, r, h, s) -> 
            if(int_end < add_begin) then
                let new_r = balance (insert r) in
                balance (make(l, int_begin, int_end, new_r)) else
            if(add_end < int_begin) then
                let new_l = balance (insert l) in
                balance (make(new_l, int_begin, int_end, r)) else
            if(add_end < int_begin) then
                balance (make(Null, add_begin, add_end, make(l, int_begin, int_end, r)))
            else
                balance (make(make(l, int_begin, int_end, r), add_begin, add_end, Null))
    in insert new_tree;;

(*
mem - sprawdza, czy dana liczba jest zawarta w drzewie.
*)
let rec mem x tree =
    match tree with 
    | Null -> false
    | Node (left, i_begin, i_end, right, _, _) ->
        if(i_begin <= x && x <= i_end) then true
        else
        if(i_begin > x) then mem x left
        else
        mem x right;;

(*
iter - wykonuje pewną funkcję f na wierzchołkach drzewa.
*)
let iter f tree =
    let rec func tree = 
        match tree with   
        | Null -> ()
        | Node (left, i_begin, i_end, right, _, _) -> func left; f (i_begin, i_end); func right
    in func tree;;

(*
fold - wykonuje rekurencyjnie pewną funkcję f na wierzchołkach
drzewa i zwraca wynik
*)
let fold f tree acc =
    let rec func acc tree = 
        match tree with
        | Null -> acc
        | Node (left, i_begin, i_end, right, _, _) ->
            func (f (i_begin, i_end) (func acc left)) right 
    in func acc tree;;

(*
elements - tworzy i zwraca listę par reprezentujących przedziały
obecne w drzewie.
*)
let elements tree = 
    let rec func acc tree =
        match tree with
        | Null -> acc
        | Node (left, i_begin, i_end, right, _, _) -> func ((i_begin, i_end) :: func acc right) left
    in
    func [] tree;;
(*
below - zwraca liczbę elementów w drzewie mniejszych bądź równych
danemu n.
*)
let below n tree =
    let rec func n tree acc =
        match tree with
        | Null -> if acc < 0 then max_int else acc
        | Node (l, int_begin, int_end, r, _, _) ->
            if(acc < 0) then max_int else
            if(n < int_begin) then func n l acc else
            if(int_end < n) then func n r (acc + size l + int_end - int_begin + 1) 
            else
                if(n - int_begin < 0) then max_int
                else
                func n Null (acc + size l + n - int_begin + 1)
    in func n tree 0;;

(*
zwraca trójkę (t, bool, t), gdzie pierwsze drzewo zawiera tylko
elementy mniejsze od danego x, prawe drzewo zawiera tylko elementy
większe od danego x, a wartość bool informuje, czy dokładna wartość
x znajduje się w drzewie.
*)
let split x tree =
    (remove(x, max_int) tree, mem x tree, remove(min_int, x) tree);;

(*
TESTY
remove - podstawowa operacja w remove, jak i w add, split - poprawne dzielenie na wyrównane drzewa.
(*
assert(remove (2,3) (Node(Null, 1, 5, Null, 1, 5)) = Node (Null, 1, 1, Node (Null, 4, 5, Null, 1, 2), 2, 1));;
assert(remove (1, 2) (Node(Node(Null, 1, 1, Null, 1, 1), 2, 2, Node(Null, 3, 3, Null, 1, 1), 2, 2)) = Node (Null, 3, 3, Null, 1, 1));;
assert(remove (0, 2) (Node(Null, 1, 1, Null, 1, 1)) = Null);;
assert(remove (2, 4) (Node(Node(Null, 1, 2, Null, 1, 2), 4, 5, Null, 4, 2)) = Node (Node (Null, 1, 1, Null, 1, 1), 5, 5, Null, 2, 2));;
assert(remove (2, 4) (Node(Null, 1, 2, Node(Null, 4, 5, Null, 1, 2), 2, 2)) = Node (Null, 1, 1, Node (Null, 5, 5, Null, 1, 1), 2, 1));;

let t = add (10, 10) empty;;
let t = add (1, 5) t;;
let t = add (7, 8) t;;
let t = add (15, 20) t;;
assert(split 13 t = (Node (Node (Null, 1, 5, Null, 1, 5), 7, 8, Node (Null, 10, 10, Null, 1, 1), 2, 7), false, Node (Null, 15, 20, Null, 1, 6)));;
assert(split 17 t = (Node (Node (Null, 1, 5, Null, 1, 5), 7, 8, Node (Node (Null, 10, 10, Null, 1, 1), 15, 16, Null, 2, 3), 3, 7), true, Node (Null, 18, 20, Null, 1, 3)));;
*)*)