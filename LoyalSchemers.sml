(* Found on the last page of The Little MLer *)

signature Ysig = sig
    val Y : (('a -> 'a) -> ('a -> 'a)) -> 'a -> 'a
end;

functor Yfunc() :> Ysig = struct
    datatype 'a T = Into of 'a T -> 'a
    fun Y f = H f $ Into (H f)
    and H f a = f $ G a
    and G (Into a) x = a (Into a) x
end;

structure Ystruct = Yfunc();

fun mk_fact f n = if n = 0 then
                      1
                  else
                      n * f (n-1);

val x = Ystruct.Y mk_fact 10;


(* Or a much simpler way of defining Y *)
functor Yeasier() :> Ysig = struct
    fun Y f x = f (Y f) x
end;

structure Yy = Yeasier();

val x = Yy.Y mk_fact 10;
