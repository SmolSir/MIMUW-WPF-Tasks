(***********************************)
(* MIMUW/WPF/przelewanka           *)
(* autor: Bartosz Smolarczyk       *)
(* review: Marcin Mordecki         *)
(***********************************)

(* złożoność czasowa i pamięciowa: *)
(* O(n^2^k), gdzie                 *)
(* n - liczba szklanek,            *)
(* k - końcowy wynik programu,     *)
(* jeśli możliwe jest otrzymanie   *)
(* pożądanej konfiguracji.         *)

(* graph - zbiór sprawdzonych      *)
(*   dotychczas stanów wody        *)
(*   w szklankach                  *)
let graph = Hashtbl.create 64

(* q - kolejka wykorzystywana przy *)
(* przeglądaniu wszerz możliwych   *)
(* stanów wody w szklankach        *)
let q = Queue.create ()

(* ans - wynik                     *)
let ans = ref (-1);;

(* __gcd = NWD(a, b)               *)
let rec __gcd a b = 
    if(a = 0) then b
    else __gcd (b mod a) a;;

(* necessary - warunek konieczny,  *)
(* przynajmniej jedna szklanka     *)
(* docelowo ma być pusta lub pełna *)
let necessary (glass, target) =
    target = 0 || target = glass;;

(* expand - rozszerza kolejkę q    *)
(* o wszystkie stany wody, jakie   *)
(* możemy uzyskać z aktualnego     *)
(* stanu wody w szklankach         *)
let expand glass target water time n =
    if(Hashtbl.mem graph water == true || !ans <> (-1)) then ()
    else 
    begin
        Hashtbl.add graph water (Hashtbl.length graph);

        let check = ref true in
        for i = 0 to n do
            if(water.(i) <> target.(i)) then
                check := false
        done;
        if(!check) then
            ans := time
        else
        begin
            let time = time + 1
            and temp = Array.copy water in
            for i = 0 to n do        
                if(water.(i) <> glass.(i)) then
                (* dolanie do szklanki (i) wody do pełna *)
                begin
                    water.(i) <- glass.(i);
                    Queue.add (Array.copy water, time) q;
                    water.(i) <- temp.(i)
                end;

                if(water.(i) <> 0) then
                begin
                    (* wylanie wody ze szklanki (i) *)
                    water.(i) <- 0;
                    Queue.add (Array.copy water, time) q;
                    water.(i) <- temp.(i);

                    for j = 0 to n do
                        if(i <> j && water.(j) <> glass.(j)) then
                        (* przelanie części lub całości wody
                            ze szklanki (i) do szklanki (j) *)
                        begin
                            water.(j) <- min glass.(j) (water.(i) + water.(j));
                            water.(i) <- water.(i) - (water.(j) - temp.(j))
                        end; 
                        Queue.add (Array.copy water, time) q;
                        water.(i) <- temp.(i);
                        water.(j) <- temp.(j);
                    done
                end;
            done;
        end;
    end;;

(* przelewanka - procedura         *)
(* rozwiązująca zadanie, zgodna    *)
(* ze specyfikacją z pliku .mli    *)
let przelewanka tab = 
    match tab with
    | [||] -> 0
    | _ ->
    ans := (-1);
    let n = Array.length tab - 1
    and glass = Array.map fst tab
    and target = Array.map snd tab in

(* sprawdzenie, czy jest szansa    *)
(* napełnić szklanki do            *)
(* oczekiwanych wartości           *)
    let gcd = Array.fold_left __gcd 0 glass in
    if (gcd <> 0) then 
        for i = 0 to n do
            if(target.(i) mod gcd <> 0) then
                ans := (-42)
        done
    else ans := 0;

(* jeśli wszystkie warunki są      *)
(* spełnione, wywołany jest bfs    *)
    if (Array.exists necessary tab) then
    begin
        Queue.add (Array.make (n + 1) 0, 0) q;
        while (not (Queue.is_empty q) && !ans = (-1)) do
            let (water, time) = Queue.take q in
            expand glass target water time n
        done;
    end;
    Queue.clear q;
    Hashtbl.clear graph;
    max !ans (-1);;


(* TESTY
(*Nie ma rozwiązania*)
let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
assert ( przelewanka c = -1 );;
let c = [|(3,2);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = -1);;

(*Jest rozwiązanie*)
let c = [|(3,2);(3,3);(1,0);(12,1)|];;
assert ( przelewanka c = 4 );;
let c = [|(1,1);(100,99)|];;
assert ( przelewanka c = 2 );;
let c = [|(3,3);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = 6);;
let c = [|(100,3);(2,1);(1,0);(6,1)|];;
assert (przelewanka c = 7);;
let c = [|(3,3);(5,5);(5,5);(6,6)|];;
assert (przelewanka c = 4);;
*)