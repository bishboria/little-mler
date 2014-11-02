datatype 'a pizza = Bottom
                  | Topping of 'a * ('a pizza);

datatype fish = Anchovy | Lox | Tuna;

val x = Topping (Anchovy, Topping (Tuna, Topping (Anchovy, Bottom)));

fun rem_anchovy Bottom                 = Bottom
  | rem_anchovy (Topping (Anchovy, p)) = rem_anchovy p
  | rem_anchovy (Topping (t, p))       = Topping (t, rem_anchovy p);

val x = rem_anchovy (Topping (Lox, Topping (Anchovy, Topping (Tuna, Topping (Anchovy, Bottom)))));

val x = rem_anchovy (Topping (Lox, Topping (Tuna, Bottom)));

fun rem_tuna Bottom              = Bottom
  | rem_tuna (Topping (Tuna, p)) = rem_tuna p
  | rem_tuna (Topping (t, p))    = Topping (t, rem_tuna p);

fun rem_fish _ Bottom           = Bottom
  | rem_fish f (Topping (t, p)) = if f = t then
                                      rem_fish f p
                                  else
                                      Topping (t, rem_fish f p);

fun eq_fish Anchovy Anchovy = true
  | eq_fish Lox     Lox     = true
  | eq_fish Tuna    Tuna    = true
  | eq_fish _       _       = false;
    eq_fish : fish -> fish -> bool;


fun rem_fish _ Bottom           = Bottom
  | rem_fish f (Topping (t, p)) = if eq_fish f t then
                                      rem_fish f p
                                  else
                                      Topping (t, rem_fish f p);

fun eq_int (n:int, m:int) : bool = n = m;

fun rem_int _ Bottom           = Bottom
  | rem_int f (Topping (t, p)) = if eq_int(f, t) then
                                     rem_int f p
                                 else
                                     Topping (t, rem_int f p);

fun subst_fish _  _  Bottom           = Bottom
  | subst_fish f1 f2 (Topping (t, p)) = if t = f2 then
                                            Topping (f1, subst_fish f1 f2 p)
                                        else
                                            Topping (t, subst_fish f1 f2 p);

datatype num = Zero
             | OneMoreThan of num;

fun eq_num Zero            Zero            = true
  | eq_num (OneMoreThan x) (OneMoreThan y) = eq_num x y
  | eq_num _               _               = false;
