datatype 'a list = Empty
                 | Cons of 'a * 'a list;

datatype box = Bacon
             | Ix of int;

exception No_bacon of int;

fun where_is Empty             = raise No_bacon(0)
  | where_is (Cons (Bacon, t)) = 1
  | where_is (Cons (_, t))     = 1 + (where_is t);

val x = where_is (Cons (Ix 5, Cons (Ix 13, Cons (Bacon, Empty))));
(* val x = where_is (Cons (Ix 13, Cons (Ix 5, Empty))); exception raised *)

(* My non-shitty-exception version, which would be improved
   with better types: ie Maybe, Either, etc. *)
fun where_is' (l:box list) : int =
    let
        fun go n Empty             = 0
          | go n (Cons (Bacon, t)) = 1 + n
          | go n (Cons (_, t))     = go (n+1) t
    in
        go 0 l
    end;

val x = where_is' (Cons (Ix 5, Cons (Ix 13, Cons (Bacon, Empty))));
val x = where_is' (Cons (Ix 13, Cons (Ix 5, Empty)));

val x = (where_is (Cons (Ix 13, Cons (Ix 5, Empty)))) handle No_bacon(n) => n;
val x = (where_is (Cons (Ix 13, Cons (Bacon, Empty)))) handle No_bacon(n) => n;

exception Out_of_range

fun list_item _ Empty        = raise Out_of_range
  | list_item 1 (Cons (h,t)) = h
  | list_item n (Cons (h,t)) = list_item (n-1) t;


(* Skipping the rest of the chapter code
   It's not the sort of code you'd ever want to write! *)
