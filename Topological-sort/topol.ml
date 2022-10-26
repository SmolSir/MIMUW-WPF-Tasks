(***********************************)
(* MIMUW/WPF/topol                 *)
(* autor: Bartosz Smolarczyk       *)
(* review: Arkadiusz Czarkowski    *)
(***********************************)

(* W rozwiązaniu wykorzystuję procedury dostępne w    *
 * module PMap. Złożoność czasowa O((m+n) * log(m+n)),*
 * gdzie m - liczba krawędzi w grafie. Złożoność      *
 * pamięciowa O(n + m), gdzie n - liczba              *
 * wierzchołków, m - liczba krawędzi w grafie.        *)
open PMap

(* Wyjątek podnoszony, gdy dany graf jest cykliczny   *)
exception Cykliczne;;

(* Funkcja budująca graf, korzystająca z modułu PMap. *
 * W reprezentacji grafu każdy wierzchołek wskazuje   *
 * na listę zawierającą wierzchołki, do których       *
 * wychodzą z niego krawędzie. W szczególności gdy    *
 * nie wychodzi żadna krawędź, to wskazuje na pustą   *
 * listę.                                             *)
let func acc (vert, edges) =
    let acc = List.fold_left (fun acc2 vert2 ->
        if(mem vert2 acc2) then 
            acc2 
        else 
            add vert2 [] acc2) 
        acc edges
    in
    if(mem vert acc) then
        add vert (edges @ (find vert acc)) acc
    else
        add vert edges acc;;

(* Funkcja budująca visited, korzystająca z modułu    *
 * PMap. Jeśli dany wierzchołek nie był jeszcze       *
 * przetworzony, to wskazuje na 0. Jeśli jest w       *
 * trakcie przetwarzania, to wskazuje na 1. Jeśli     *
 * jest przetworzony, to wskazuje na 2. Początkowo    *
 * wszystkie wierzchołki wskazują na 0, docelowo mają *
 * wskazywać na 2.                                    *)
let func2 acc (vert, edges) =
        let acc = List.fold_left 
            (fun acc2 vert2 -> add vert2 0 acc2) acc edges
        in
        add vert 0 acc;;

(* Funkcja wykonująca sortowanie topologiczne - dfs   *
 * post-order. Wchodząc do wierzchołka sprawdzamy, na *
 * co wskazuje jego visited:                          *
 * 0 - zmieniamy jego visited na 1, wywołujemy dfs    *
 * 1 - znaleźliśmy cykl, podnosimy wyjątek Cyklicznie *
 * 2 - wierzchołek przetworzony, kontynuujemy dfs.    *
 * Wychodząc z wierzchołka, zmieniamy jego visited    *
 * na 2, a wierzchołek dołączamy na przód listy       *
 * acc zbierającej wynik dfsa.                        *)
let toposort graf visit =
    let visited = ref visit in
    let acc = ref [] in
    let rec dfs vert edges =
        let status = find vert !visited in
        if (status = 1) then 
            raise Cykliczne
        else if (status = 0) then
            begin
            visited := add vert 1 !visited;
            List.iter (fun x -> dfs x (find x graf)) edges;
            acc := (vert :: !acc);
            visited := add vert 2 !visited
            end
    in iter dfs graf;
    !acc;;

(* Funkcja topol tworzy graf oraz visited, po czym    *
 * wywołuje na nich funkcję toposort, która zwraca    *
 * wynikową listę.                                    *)
let topol lst =
    let graf = List.fold_left func empty lst in 
    let visited = List.fold_left func2 empty lst in
    toposort graf visited;;

(* TESTY *)
(*
let g = [(1, [2]); (2, [5]); (1, [3]); (1, [4]); (2, [4]); (2, [6])];;
assert(topol g = [1; 2; 5; 6; 3; 4]);;
let g = [("a", ["c"]); ("e", ["g"]); ("f", ["a";"e"]); ("g", ["c";"a"])];;
assert(topol g = ["f"; "e"; "g"; "a"; "c"]);;
let g = [([1], [[2; 2]]); ([2; 2], [[3; 3; 3]]); ([3; 3; 3], [[2]; [5; 5]; [3; 3]])];;
assert(topol g = [[1]; [2; 2]; [3; 3; 3]; [3; 3]; [5; 5]; [2]]);;
let g = [(10.001, [10.002]); (10.002, [10.003])];;
assert(topol g = [10.001; 10.002; 10.003]);;
*)