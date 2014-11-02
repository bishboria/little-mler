datatype meza    = Shrimp | Calamari | Escargots | Hummus;
datatype main    = Steak | Ravioli | Chicken | Eggplant;
datatype salad   = Green | Cucumber | Greek;
datatype dessert = Sundae | Mousse | Torte;

val x : meza * main * salad * dessert   = (Hummus, Steak, Green, Torte);
val x : dessert * meza * main * dessert = (Torte, Hummus, Steak, Sundae);

fun add_a_steak x = (x, Steak);
(add_a_steak : dessert -> dessert * main); (* doesn't change the actual definition *)

val x = add_a_steak 5;

fun eq_main (Steak, Steak)       = true
  | eq_main (Ravioli, Ravioli)   = true
  | eq_main (Chicken, Chicken)   = true
  | eq_main (Eggplant, Eggplant) = true
  | eq_main (_, _)               = false;

fun has_steak (_, Steak, _) = true
  | has_steak (_, _, _)     = false;
(has_steak : 'a * main * 'b -> bool);

val x = has_steak (5, Steak, true);

fun has_steak (_:meza, Steak, _:dessert) : bool = true
  | has_steak (_, _, _)                         = false;

(* val x = has_steak (5, Steak, true); No longer type correct *)

fun add_a_steak (x:meza) : meza * main = (x, Steak);

(* val x = add_a_steak 5; No longer type correct *)
val x = add_a_steak Hummus;
