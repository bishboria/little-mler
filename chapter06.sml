datatype fruit = Peach | Apple | Pear | Lemon | Fig;

datatype tree = Bud
              | Flat of fruit * tree
              | Split of tree * tree;

fun flat_only Bud            = true
  | flat_only (Flat (f, t))  = flat_only t
  | flat_only (Split _)      = false;

fun split_only Bud            = true
  | split_only (Flat (f, t))  = false
  | split_only (Split (l, r)) = split_only l andalso split_only r;

val x = split_only (Split (Split (Bud,
                                  Split (Bud,
                                         Bud)),
                           Split (Bud,
                                  Split (Bud,
                                         Bud))));

fun contains_fruit Bud            = false
  | contains_fruit (Flat _)       = true
  | contains_fruit (Split (l, r)) = contains_fruit l orelse contains_fruit r;

fun contains_fruit x = not (split_only x);

fun height Bud            = 0
  | height (Flat (_, t))  = 1 + height t
  | height (Split (l, r)) = 1 + Int.max(height l, height r);

val x = height (Split (Split (Bud,
                              Flat (Lemon,
                                    Bud)),
                       Flat (Fig,
                             Split (Bud,
                                    Bud))));
val x = height (Split (Bud,
                       Flat (Lemon,
                             Bud)));

val x = height (Flat (Lemon, Bud));
val x = height Bud;
val x = height (Flat (Fig,
                      Flat (Lemon,
                            Flat (Apple,
                                  Bud))));
val x = height (Split (Split (Bud,
                              Bud),
                       Flat (Fig,
                             Flat (Lemon,
                                   Flat (Apple,
                                         Bud)))));

fun less_than (n:int, m:int) : bool = n < m;
fun larger_of (n:int, m:int) : int = if less_than(n, m) then
                                         m
                                     else
                                         n;

fun subst_in_tree f1 f2 Bud = Bud
  | subst_in_tree f1 f2 (Flat (f, t)) = if f = f2 then
                                            Flat (f1, subst_in_tree f1 f2 t)
                                        else
                                            Flat (f, subst_in_tree f1 f2 t)
  | subst_in_tree f1 f2 (Split (l, r)) = Split (subst_in_tree f1 f2 l,
                                                subst_in_tree f1 f2 r);

val x = subst_in_tree Apple Fig (Split (Split (Flat (Fig,
                                                     Bud),
                                               Flat (Fig,
                                                     Bud)),
                                        Flat (Fig,
                                              Flat (Lemon,
                                                    Flat (Apple,
                                                          Bud)))));

fun occurs _ Bud            = 0
  | occurs a (Flat (f, t))  = if a = f then
                                  1 + occurs a t
                              else
                                  occurs a t
  | occurs a (Split (l, r)) = occurs a l + occurs a r;


val x = occurs Fig (Split (Split (Flat (Fig,
                                        Bud),
                                  Flat (Fig,
                                        Bud)),
                           Flat (Fig,
                                 Flat (Lemon,
                                       Flat (Apple,
                                             Bud)))));

datatype 'a slist = Empty
                  | Scons of 'a sexp * 'a slist

     and 'a sexp  = AnAtom of 'a
                  | Aslist of 'a slist;

fun occurs_in_slist _ Empty          = 0
  | occurs_in_slist f (Scons (h, t)) = occurs_in_sexp f h + occurs_in_slist f t

and occurs_in_sexp f (AnAtom a) = if f = a then 1 else 0
  | occurs_in_sexp f (Aslist s) = occurs_in_slist f s;

val x = occurs_in_slist Fig (Scons (AnAtom(Fig),
                                    Scons (AnAtom(Fig),
                                           Scons (AnAtom(Lemon),
                                                  Empty))));
val x = occurs_in_slist Fig
                        (Scons (Aslist( Scons (AnAtom Fig,
                                               Scons (AnAtom Peach,
                                                      Empty))),
                                Scons (AnAtom Fig,
                                       Scons (AnAtom Lemon,
                                              Empty))));

val x = occurs_in_sexp Fig
                       (Aslist (Scons (AnAtom Fig,
                                       Scons (AnAtom Peach,
                                              Empty))));

fun subst_in_slist _ _ Empty    = Empty
  | subst_in_slist n a (Scons (s, y)) = Scons (subst_in_sexp n a s,
                                               subst_in_slist n a y)
and subst_in_sexp n a (AnAtom f) = if f = a then
                                       AnAtom n
                                   else
                                       AnAtom f
  | subst_in_sexp n a (Aslist y) = Aslist (subst_in_slist n a y);


fun eq_fruit_in_atom a (AnAtom s) = a = s
  | eq_fruit_in_atom a (Aslist y) = false

fun rem_from_slist _ Empty         = Empty
  | rem_from_slist a (Scons (s,y)) = if eq_fruit_in_atom a s then
                                         rem_from_slist a y
                                     else
                                         Scons (rem_from_sexp a s,
                                                rem_from_slist a y)
and rem_from_sexp a (AnAtom b) = AnAtom b
  | rem_from_sexp a (Aslist y) = Aslist (rem_from_slist a y);
