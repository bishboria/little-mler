val x = 5; (* : int *)
7;
~5 + 5; (* ~ = -ve *)
5.32;   (* : real *)

true;  (* : bool *)
false; (* : bool  *)

datatype seasoning = Salt | Pepper;

Salt;

datatype num = Zero | OneMoreThan of num;

Zero; (* : num *)
0;    (* : int *)

OneMoreThan Zero;
OneMoreThan (OneMoreThan Zero);


datatype 'a openFacedSandwich = Bread of 'a
                              | Slice of 'a openFacedSandwich;

Bread 0;    (* : int openFacedSandwich *)
Bread true; (* : bool openFacedSandwich *)

Bread (OneMoreThan Zero); (* : num openFacedSandwich *)

Bread (Bread 0); (* : (int openFacedSandwich) openFacedSandwich *)
Bread (Bread (OneMoreThan Zero)); (* : (num openFacedSandwich) openFacedSandwich *)
