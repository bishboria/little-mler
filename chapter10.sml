fun eq_int n m = n = m;

fun is_zero n = eq_int n 0;

exception Too_small;

fun pred n = if eq_int n 0 then
                 raise Too_small
             else
                 n - 1;

fun succ n = n + 1;

fun plus n m = if is_zero n then
                   m
               else
                   succ (plus (pred n) m);

datatype num = Zero
             | OneMoreThan of num;

fun is_zero Zero = true
  | is_zero _    = false;

fun pred Zero            = raise Too_small
  | pred (OneMoreThan n) = n;

fun succ n = OneMoreThan n;

fun plus n m = if is_zero n then
                   m
               else
                   succ (plus (pred n) m);

val x = plus (OneMoreThan (OneMoreThan (OneMoreThan Zero)))
             (OneMoreThan (OneMoreThan Zero));

signature N =                    (* Define the components that make up N *)
sig
    type number                  (* refer to the type locally as number *)
    exception Too_small
    val succ : number -> number  (* define the operations that can be performed *)
    val pred : number -> number
    val is_zero : number -> bool
end;

(* Allows multiple definitions of the same moudle/interface/
   collection of components *)

(* functor in sml gives produces structures from signatures *)
(* 'X :> Y' result of using this functor is a structure with signature Y *)
(* Empty parens, this structure has no dependencies *)
functor NumberAsNum() :> N =
  struct
    datatype num = Zero
                 | OneMoreThan of num
    type number = num (* associating the signature type with a concrete one *)
    exception Too_small
    fun succ n = OneMoreThan n
    fun pred Zero            = raise Too_small
      | pred (OneMoreThan n) = n
    fun is_zero Zero = true
      | is_zero _    = false
  end;

functor NumberAsInt() :> N =
  struct
    type number = int
    exception Too_small
    fun succ n = n+1
    fun pred n = if eq_int n 0 then
                     raise Too_small
                 else
                     n-1
    fun is_zero n = eq_int n 0
  end;

structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();

signature P =
  sig
    type number
    val plus : number -> number -> number
  end;

functor PON(structure a_N : N) :> P =
  struct
    type number = a_N.number
    fun plus n m = if a_N.is_zero n then
                       m
                   else
                       a_N.succ (plus (a_N.pred n) m);
  end;

(* signature: define an abstract interface *)
(* functor:   define a concrete implementation of that signature *)
(* structure: create an instance of that functor *)

structure IntArith = PON(structure a_N = IntStruct);
structure NumArith = PON(structure a_N = NumStruct);

(* val x = IntArith.plus 1 2; We need values of type IntArith.number, not *int* *)
(* val x = NumArith.plus Zero (OneMoreThan Zero); Same here *)

signature NCR =
  sig
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
  end;

functor NumberAsInt() :> NCR =
  struct
    type number = int
    exception Too_small
    fun conceal n = n
    fun succ n = n+1
    fun pred n = if eq_int n 0 then
                     raise Too_small
                 else
                     n-1
    fun is_zero n = eq_int n 0
    fun reveal n = n
  end;

functor NumberAsNum() :> NCR =
  struct
    datatype num = Zero | OneMoreThan of num
    type number = num
    exception Too_small
    fun conceal n = if eq_int n 0 then
                        Zero
                    else
                        OneMoreThan (conceal (n-1))
    fun succ n = OneMoreThan n
    fun pred Zero = raise Too_small
      | pred (OneMoreThan n) = n
    fun is_zero Zero = true
      | is_zero _    = false
    fun reveal n = if is_zero n then
                       0
                   else
                       1 + reveal (pred n)
  end;

structure IntStruct = NumberAsInt()
structure IntArith  = PON(structure a_N = IntStruct)

structure NumStruct = NumberAsNum()
structure NumArith  = PON(structure a_N = NumStruct)

infixr $
fun f $ x = f x;

val x = NumStruct.reveal $ NumStruct.succ $ NumStruct.conceal 0;
(* val x = NumStruct.reveal $ NumArith.plus (NumStruct.conceal 1)
                                         (NumStruct.conceal 2);
*)

(* where clause refines what a signature stands for *)
functor PON(structure a_N : N) :> P where type number = a_N.number =
  struct
    type number = a_N.number
    fun plus n m = if a_N.is_zero n then
                       m
                   else
                       a_N.succ (plus (a_N.pred n) m)
  end;

structure NumArith = PON(structure a_N = NumStruct);
structure IntArith = PON(structure a_N = IntStruct);

val x = NumStruct.reveal $ NumArith.plus (NumStruct.conceal 1)
                                         (NumStruct.conceal 2);

val x = IntStruct.reveal $ IntArith.plus (IntStruct.conceal 1)
                                         (IntStruct.conceal 2);

functor NumberAsInt2() :> N where type number = int =
  struct
    type number = int
    exception Too_small
    fun succ n = n+1
    fun pred n = if eq_int n 0 then
                     raise Too_small
                 else
                     n-1
    fun is_zero n = eq_int n 0
  end;

structure IntStruct2 = NumberAsInt2();
structure IntArith2  = PON(structure a_N = IntStruct2);

(* Can't do this with the old definition *)
val x = IntArith2.plus 1 2;


(* num in where clause refers to datatype defined *outside* this
   functor. doesn't work

functor NumberAsNum2() :> N where type number = num =
  struct
    datatype num = Zero | OneMoreThan of num
    type number = num
    exception Too_small
    fun succ n = OneMoreThan n
    fun pred Zero             = raise Too_small
      | pred (OneMoreThan n) = n
    fun is_zero Zero = true
      | is_zero _    = false
  end;
*)

signature S = sig
    type number1
    type number2
    val similar : number1 -> number2 -> bool
end;

functor Same(structure a_N : N; structure b_N : N) :>
        S where type number1 = a_N.number
          where type number2 = b_N.number = struct
    type number1 = a_N.number
    type number2 = b_N.number
    fun sim n m = if a_N.is_zero n then
                      b_N.is_zero m
                  else
                      sim (a_N.pred n) (b_N.pred m)
    fun similar n m = (sim n m handle a_N.Too_small => false)
                      handle b_N.Too_small => false
end;

structure SimIntNum = Same(structure a_N = IntStruct; structure b_N = NumStruct);
structure SimNumInt = Same(structure a_N = NumStruct; structure b_N = IntStruct);

val x = SimNumInt.similar (NumStruct.conceal 0)
                          (IntStruct.conceal 0);
val x = SimIntNum.similar (IntStruct.conceal 0)
                          (NumStruct.conceal 1);

structure SimNumNum = Same(structure a_N = NumStruct; structure b_N = NumStruct);

val x = SimNumNum.similar (NumStruct.conceal 3)
                          (NumStruct.conceal 3);

fun new_plus x y = NumStruct.reveal $ NumArith.plus (NumStruct.conceal x)
                                                    (NumStruct.conceal y);

fun new_plus x y = IntStruct.reveal $ IntArith.plus (IntStruct.conceal x)
                                                    (IntStruct.conceal y);


signature J = sig
    val new_plus : int -> int -> int
end;

(* *sharing* clause ensures that a_N.number and a_P.number are the same type! *)
functor NP( structure a_N : NCR
          ; structure a_P : P
          ; sharing type a_N.number = a_P.number)
        :> J = struct
    fun new_plus x y = a_N.reveal $ a_P.plus (a_N.conceal x)
                                             (a_N.conceal y);
end;

structure NPStruct = NP(structure a_N = NumStruct; structure a_P = NumArith);

structure NPStruct = NP(structure a_N = NumberAsNum()
                       ;structure a_P = PON(structure a_N = a_N)
                       );

signature T = sig
    type number
    val times : number -> number -> number
end;

functor TON( structure a_N : N
           ; structure a_P : P
           ; sharing type a_N.number = a_P.number
           )
        :> T where type number = a_N.number =
struct
    type number = a_N.number
    fun times n m = if a_N.is_zero n then
                        m
                    else
                        a_P.plus n (times (a_N.pred n) m)
end;
