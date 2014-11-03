fun identity x = x;
identity : 'a -> 'a;

fun true_maker x = true;
true_maker : 'a -> bool;

datatype bool_or_int = Hot of bool
                     | Cold of int;

val x = Hot true;
val x = Cold 10;

fun hot_maker x = Hot;

fun help f = Hot (true_maker (if true_maker 5 then f else true_maker));
help : ('a -> bool) -> bool_or_int;

datatype chain = Link of int * (int -> chain); (* not positive *)

fun ints n = Link (n + 1, ints);

val x = ints 3;

fun skips n = Link (n + 2, skips);

val x = skips 3;

fun eq_int (n:int, m:int) : bool = n = m;

fun divides_evenly (n, c) = eq_int (n mod c, 0);
divides_evenly : int * int -> bool;

fun is_mod_5_or_7 n = if divides_evenly(n, 5) then
                          true
                      else
                          divides_evenly(n, 7);
is_mod_5_or_7 : int -> bool;

fun some_ints n = if is_mod_5_or_7 (n+1) then
                      Link (n+1, some_ints)
                  else
                      some_ints (n+1);
some_ints : int -> chain;

val x = some_ints 1;
val x = some_ints 5;
val x = some_ints 7;
val x = some_ints 116;

fun chain_item(n:int, Link(i,f):chain) =
    if eq_int(n, 1) then
        i
    else
        chain_item(n-1, f i);

val x = chain_item(1, some_ints 0);
val x = chain_item(6, some_ints 0);
val x = chain_item(37, some_ints 0);

fun is_prime        n      = has_no_divisors(n, n-1)
and has_no_divisors (n, c) = if eq_int(c, 1) then
                                 true
                             else if divides_evenly(n, c) then
                                 false
                             else
                                 has_no_divisors(n, c-1);

(* Could also have had *)
local
    fun has_no_divisors' (n, c) = true (* blah *);
in
    fun is_prime' n = has_no_divisors'(n, n-1);
end;

(* Or *)
fun is_prime'' n =
    let
        fun has_no_divisors'' (n, c) = true
    in
        has_no_divisors''(n, n-1)
    end;

val x = is_prime 7;

fun primes n = if is_prime (n+1) then
                   Link (n+1, primes)
               else
                   primes (n+1);

val x = chain_item(12, primes 1);

fun fibs (n:int) (m:int) : chain = Link(n + m, fibs m);

fun fibs_1 x = fibs 1 x;
fun fibs_2 x = fibs 2 x;

val x = fibs_1 1;
val x = fibs_1 2;
