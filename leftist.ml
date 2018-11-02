(*  Autor kodu: Przemysław Podleśny *)
(*  Code reviewer: Jakub Nowak      *)

(*  Korzeń kolejki priorytetowej - albo drzewo puste,
    albo rekord reprezentujący węzeł drzewa             *)
type 'a queue =
    EmptyQueue |
    Node of {
        left : 'a queue;
        priority : 'a;
        right : 'a queue;
        height : int;
    }

(*  Zwraca prawą wysokość wierzchołka "q"
    (0 dla liścia, -1 dla pustego).         *)
let height = function
    | EmptyQueue -> -1
    | Node {height} -> height

(*  Tworzy drzewo o lewym synu w l,
    priorytecie priority i prawym synu w r. *)
let make_node l priority r =
    Node{
        left = l;
        priority = priority;
        right = r;
        height = height r + 1;
    }

(*  Porządkuje kolejki q1, q2 nierosnąco
    pod względem prawej wysokości ich korzeni.  *)
let order_heights q1 q2 =
    if height q1 >= height q2 then (q1, q2)
    else (q2, q1)

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
            in let (l, r) = order_heights left d3
                in make_node l prio1 r

(*  Wyjątek zwracany przez delete_min, gdy kolejka jest pusta       *)
exception Empty

(*  Zwraca parę (priorytet_korzenia, kolejka_po_usunięciu_korzenia) *)
(*  ...lub wyjątek, gdy kolejka jest pusta.     *)
let delete_min = function
    | EmptyQueue -> raise Empty
    | Node {left; priority; right} -> (priority, join left right)

(*  Dodaje do kolejki "q" element o priorytecie "x".    *)
let add x q =
    let single_x = make_node EmptyQueue x EmptyQueue
    in join single_x q
