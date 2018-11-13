(*  Autor kodu: Przemysław Podleśny *)
(*  Code reviewer: Jakub Nowak      *)
(*  Korzeń kolejki priorytetowej - albo drzewo puste,
    albo rekord reprezentujący węzeł drzewa;
    left - lewy syn, priority - priorytet, right - prawy syn,
    height - prawa wysokość drzewa                                    *)

type 'a queue =
    | EmptyQueue
    | Node of {
        priority: 'a;
        height: int;
        left: 'a queue;
        right: 'a queue;
    }

(*  Zwraca prawą wysokość wierzchołka "q"
    (0 dla liścia, -1 dla pustego).         *)
let height = function
    | EmptyQueue -> -1
    | Node {height} -> height

(*  Tworzy drzewo o lewym synu w l,
    priorytecie priority i prawym synu w r. *)
let make_node l priority r =
    Node {
        left = l;
        priority = priority;
        right = r;
        height = height r + 1;
    }

(*  Porządkuje kolejki q1, q2 nierosnąco
    pod względem prawej wysokości ich korzeni.  *)
let order_heights q1 q2 =
    if height q1 >= height q2 then q1, q2
    else q2, q1

(*  Pusta kolejka.  *)
let empty = EmptyQueue

(*  Sprawdzenie, czy kolejka jest pusta.    *)
let is_empty = function
    | EmptyQueue -> true
    | Node _ -> false

(*  Łączenie kolejek q1 i q2.   *)
let rec join q1 q2 =
    match q1, q2 with (* Gdy jedna jest pusta, nie ma co łączyć.    *)
    | EmptyQueue, _ -> q2
    | _, EmptyQueue -> q1
    | Node {left; priority = prio1; right}, Node {priority = prio2} ->
        if prio1 > prio2 then join q2 q1 (* Niech prio1 < prio2. *)
        else (* d1 = q1, d2 = q2, zwracane jest d4. *)
            let d3 = join right q2
            in let l, r = order_heights left d3
                in make_node l prio1 r

(*  Wyjątek zwracany przez delete_min, gdy kolejka jest pusta       *)
exception Empty

(*  Zwraca parę (priorytet_korzenia, kolejka_po_usunięciu_korzenia)
    ...lub wyjątek, gdy kolejka jest pusta.     *)
let delete_min = function
    | EmptyQueue -> raise Empty
    | Node {left; priority; right} -> priority, join left right

(*  Dodaje do kolejki "q" element o priorytecie "x".    *)
let add x q =
    let single_x = make_node EmptyQueue x EmptyQueue
    in join single_x q

(*
(*                       TESTY                          *)

(*  Test poprawności zwracanego wyniku                  *)
open List

(*  Funkcja wypełnia początkowo pusty kontener
    base_case potęgami param1 modulo param2 od 0 do n-1 *)
let fill param1 param2 n add_fun base_case =
    let give_next numb = (numb * param1) mod param2
    in let rec fill_helper numb n =
        if n == 0 then base_case
        else add_fun numb (fill_helper (give_next numb) (n-1))
    in fill_helper 1 n

(*  Funkcja wypełniająca drzewo                         *)
let fill_tree param1 param2 n =
    fill param1 param2 n add empty

(*  Funkcja wypełniająca listę                          *)
let fill_list param1 param2 n =
    let join h l = h::l in
    fill param1 param2 n join []

(*  Parę testów dla sprawdzenia, że fill_list działa poprawnie  *)
let a = fill_list 1 10 5
let b = fill_list 2 7 7
let c = fill_list 7 9 5
;;

assert (a = [1; 1; 1; 1; 1]);;
assert (b = [1; 2; 4; 1; 2; 4; 1]);;
assert (c = [1; 7; 4; 1; 7]);;

exception StillNotEmpty

(*  Porównuje, czy kolejne elementy z kolejki q odpowiadają
    co do wartości kolejnym elementom posortowanej listy l  *)
let rec compare_queue_list q l =
    match q, l with
    | EmptyQueue, [] -> true
    | Node _, hd::tl ->
        let minimum, (rest:'a queue) = delete_min q in
            if minimum != hd then false
            else compare_queue_list rest tl
    | EmptyQueue, _ -> raise Empty
    | Node _, [] -> raise StillNotEmpty

let a_q = fill_tree 1 10 5
let b_q = fill_tree 2 7 7
let c_q = fill_tree 7 9 5
;;

(*  Sprawdzenie działania fill_tree i compare_queue_list    *)
assert( compare_queue_list a_q (sort compare a) == true );;
assert( compare_queue_list b_q (sort compare b) == true );;
assert( compare_queue_list c_q (sort compare c) == true );;
assert( compare_queue_list a_q (sort compare c) == false);;
assert( compare_queue_list c_q (sort compare a) == false);;

let d = sort compare (fill_list 555 1007 100)
let e = sort compare (fill_list 2482 17207 100)

let d_q = fill_tree 555 1007 100
let e_q = fill_tree 2482 17207 100

let h_d = height d_q
let h_e = height e_q
;;

assert( compare_queue_list d_q d == true );;
assert( compare_queue_list e_q e == true );;
assert( compare_queue_list d_q e == false);;
assert( compare_queue_list e_q d == false);;

(*  Funkcja wypełniająca drzewo i listę w ten sam sposób,
    sortująca listę i sprawdzająca, czy kolejka będzie
    zgodnie z założeniem zwracała elementy w kolejności rosnącej    *)
let check param1 param2 n =
    let q = fill_tree param1 param2 n
    and l = sort compare (fill_list param1 param2 n)
    in compare_queue_list q l

(*  Generuje testy na podstawie dwóch ziaren seed_1, seed_2
    będących liczbami naturalnymi i sprawdza, czy każde
    z odpowiednich drzew jest równe odpowiedniej liście     *)
let rec test_with_check seed_1 seed_2 dlugosci n =
    if n = 0 then true
    else if check seed_1 seed_2 dlugosci == false then false
    else let big_prime = 1000 * 1000 * 1000 + 9 in
        let new_1 = (seed_1 * seed_1) mod big_prime
        and new_2 = (seed_2 * seed_2) mod big_prime
        in test_with_check new_1 new_2 dlugosci (n-1)
;;

assert( test_with_check 500 1007 100 100 );;
assert( test_with_check 14211 437247 1000 1000 );;
assert( test_with_check 2324817 213213729 100000 100 );;
assert( test_with_check 22192 124823 10 10000000 );;

(*  Test wydajnościowy przez testy mniej losowe, a bardziej złośliwe,
    których rozwiązanie źle zaimplementowane nie powinno przejść
    w przyzwoicie małym czasie (tj. rzędu nie większego niż minuta)     *)

(*  Kolejka wypełniana coraz większymi elementami od 1 do n             *)
let rosnace_drzewo n =
    let rec rosnacy_helper acc m =
        if m = n then acc
        else rosnacy_helper (add m acc) (m+1)
    in rosnacy_helper empty 1

(*  Usuwa kolejne elementy z kolejki wypełnionej
    wszystkimi liczbami od 1 do n dla pewnego n *)
let oproznij q =
    let rec opr_helper n q =
        if is_empty q then true
        else let minimalny, reszta = delete_min q in
            if minimalny != n then false
            else opr_helper (n+1) reszta
    in opr_helper 1 q
;;

(*  Można zauważyć, że czasy rosną szybko, ale f wciąż da się opróżnić
    w mniej niż minutę, co sygnalizuje poprawną złożoność czasową      *)
assert(rosnace_drzewo 100 |> oproznij);;
assert(rosnace_drzewo 1000 |> oproznij);;
assert(rosnace_drzewo 10000 |> oproznij);;
assert(rosnace_drzewo 100000 |> oproznij);;
assert(rosnace_drzewo 1000000 |> oproznij);;
assert(rosnace_drzewo 10000000 |> oproznij);;

(*  Podobnie jak wyżej, ale elementy dodajemy w kolejności malejącej od n do 1  *)
let malejace_drzewo n =
    let rec malejacy_helper acc n =
        if n = 0 then acc
        else malejacy_helper (add n acc) (n-1)
    in malejacy_helper empty n
;;

(*  Tutaj drzewo działa zauważalnie szybciej; w tym przypadku
    czasy powinny być liniowe ze względu na jego wielkość       *)
assert(malejace_drzewo 100 |> oproznij);;
assert(malejace_drzewo 1000 |> oproznij);;
assert(malejace_drzewo 10000 |> oproznij);;
assert(malejace_drzewo 100000 |> oproznij);;
assert(malejace_drzewo 1000000 |> oproznij);;
assert(malejace_drzewo 10000000 |> oproznij);;
*)
