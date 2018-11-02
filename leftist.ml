(*  Autor kodu: Przemysław Podleśny *)
(*  Code reviewer: Jakub Nowak      *)

(*  Korzeń kolejki priorytetowej - albo drzewo puste, albo
    krotka postaci (lewy_syn, priorytet, wysokość, prawy_syn).  *)
type 'a queue =
    Empty_q | Node of 'a queue * 'a * int * 'a queue

(*  Zwraca prawą wysokość wierzchołka
    (0 dla liścia, -1 dla pustego).         *)
let height = function
    | Empty_q -> -1
    | Node (_, _, h, _) -> h

(*  Tworzy drzewo o lewym synu w l,
    priorytecie priority i prawym synu w r. *)
let make_node l priority r =
    Node(l, priority, height r + 1, r)

(*  Porządkuje kolejki q1, q2 nierosnąco
    pod względem prawej wysokości ich korzeni.  *)
let order_heights q1 q2 =
	if height q1 >= height q2 then (q1, q2)
	else (q2, q1)

(*  Pusta kolejka.  *)
let empty = Empty_q

(*  Sprawdzenie, czy kolejka jest pusta.    *)
let is_empty = function
    | Empty_q -> true
    | Node _ -> false

(*  Łączenie kolejek q1 i q2.   *)
let rec join q1 q2 =
	match q1, q2 with (* Gdy jedna jest pusta, nie ma co łączyć.    *)
	| Empty_q, _ -> q2
	| _, Empty_q -> q1
	| Node(l1, prio1, h1, r1), Node(l2, prio2, h2, r2) ->
        if prio1 > prio2 then join q2 q1 (* Niech prio1 < prio2.    *)
        else (* d1 = q1, d2 = q2, zwracane jest d4. *)
            let d3 = join r1 q2
            in let (l, r) = order_heights l1 d3
                in make_node l prio1 r

(*  Kolejka jest pusta! *)
exception Empty

(*  Zwraca parę (priorytet_korzenia, kolejka_po_usunięciu_korzenia) *)
(*  ...lub wyjątek, gdy kolejka jest pusta.     *)
let delete_min = function
	| Empty_q -> raise Empty
	| Node (l, priority, _, r) -> (priority, join l r)

(*  Dodaje do kolejki "q" element o priorytecie "x".    *)
let add x q =
	let single_x = make_node Empty_q x Empty_q
	in join single_x q
