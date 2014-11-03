datatype 'a list = Empty
                 | Cons of 'a * 'a list;

datatype orapl = Orange | Apple;

fun eq_orapl Orange Orange = true
  | eq_orapl Apple  Apple  = true
  | eq_orapl _      _      = false;
eq_orapl : orapl -> orapl -> bool;

fun eq_int (n:int) (m:int) : bool = n = m;
fun less_than (n:int) (m:int) : bool = n < m;

fun subst_int _ _ Empty        = Empty
  | subst_int n a (Cons (e,t)) = if eq_int a e then
                                     Cons(n, subst_int n a t)
                                 else
                                     Cons(e, subst_int n a t);

fun sub_orapl _ _ Empty        = Empty
  | sub_orapl n a (Cons (e,t)) = if eq_orapl a e then
                                     Cons(n, sub_orapl n a t)
                                 else
                                     Cons(e, sub_orapl n a t);

fun subst _   _ _ Empty        = Empty
  | subst rel n a (Cons (e,t)) = if rel a e then
                                     Cons(n, subst rel n a t)
                                 else
                                     Cons(e, subst rel n a t);

val subst_int = subst eq_int;
val x = subst_int 11 15 (Cons (15, Cons (6, Cons (15, Cons (17, Cons (15, Cons (8, Empty)))))));

val x = subst less_than 11 15 (Cons (17, Cons (6, Cons (15, Cons (17, Cons (15, Cons (8, Empty)))))));

fun in_range small large x = if less_than small x then
                                 less_than x large
                             else
                                 false;

fun subst_pred _    _ Empty        = Empty
  | subst_pred pred n (Cons (e,t)) = if pred e then
                                         Cons (n, subst_pred pred n t)
                                     else
                                         Cons (e, subst_pred pred n t);

fun is_15 n = eq_int n 15;

val x = subst_pred is_15 11 (Cons (15, Cons (6, Empty)));

fun less_than_15 n = less_than n 15;

fun in_range_11_16 x = if less_than 11 x then
                           less_than x 16
                       else
                           false;

fun in_range_c small large x = if less_than small x then
                                   less_than x large
                               else
                                   false;

val x = subst_pred (in_range_c 3 16) 22 (Cons (15, Cons (6, Empty)));

fun subst_c pred n Empty        = Empty
  | subst_c pred n (Cons (e,t)) = if pred e then
                                      Cons (n, subst_c pred n t)
                                  else
                                      Cons (e, subst_c pred n t);

(* aka append *)
fun combine Empty         l2 = l2
  | combine (Cons (a,l1)) l2 = Cons (a, combine l1 l2);

(* Or... *)
fun base (l2: 'a list) = l2;

fun combine_s Empty          = base
  | combine_s (Cons (a, l1)) = make_cons a (combine_s l1)

and make_cons a f l2 = Cons (a, f l2);

val x = make_cons 3 base;
