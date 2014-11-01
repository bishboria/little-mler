datatype shish_kebab = Skewer
                     | Onion of shish_kebab
                     | Lamb of shish_kebab
                     | Tomato of shish_kebab

val x = Skewer;
val x = Onion Skewer;
val x = Onion (Lamb (Onion Skewer));

fun only_onions Skewer    = true
  | only_onions (Onion x) = only_onions x
  | only_onions (Lamb _)  = false
  | only_onions (Tomato _) = false;
(only_onions : shish_kebab -> bool);

val x = only_onions (Onion (Onion Skewer));

fun is_vegetartian Skewer     = true
  | is_vegetartian (Onion x)  = is_vegetartian x
  | is_vegetartian (Lamb _)   = false
  | is_vegetartian (Tomato x) = is_vegetartian x;
(is_vegetartian : shish_kebab -> bool);

datatype 'a shish = Bottom of 'a
                  | Onion of 'a shish
                  | Lamb of 'a shish
                  | Tomato of 'a shish

datatype rod = Dagger | Fork | Sword

datatype plate = GoldPlate | SilverPlate | BrassPlate

val x = Onion (Tomato (Bottom Dagger));
val x = Onion (Tomato (Bottom GoldPlate));

fun is_veggie (Bottom _) = true
  | is_veggie (Onion x)  = is_veggie x
  | is_veggie (Lamb _)   = false
  | is_veggie (Tomato x) = is_veggie x;
(is_veggie : 'a shish -> bool);

fun what_bottom (Bottom x) = x
  | what_bottom (Onion x)  = what_bottom x
  | what_bottom (Lamb x)   = what_bottom x
  | what_bottom (Tomato x) = what_bottom x;
(what_bottom : 'a shish -> 'a);

val x = what_bottom (Onion (Tomato (Bottom GoldPlate)));
