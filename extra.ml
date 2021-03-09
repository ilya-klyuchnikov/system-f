open Syntax
open Check

let check_equal_t =
  Testing.make_check_equal ~test_module:"Check" ~to_string:Syntax.string_of_type
    ()

let () =
  (*
      (\. \. (0 -> 1)) (\. 1 -> 0)
   => (\. (0 -> 1))   [0  :=   (\. 1 -> 0)]
   => \. (0 -> (\. 2 -> 0))
   *)
  check_equal_t ~name:"type_subst test 1"
    (fun () ->
      type_subst
        (AllT (1, ArrT ([ VarT 0 ], VarT 1)))
        0
        (AllT (1, ArrT ([ VarT 1 ], VarT 0))))
    (AllT (1, ArrT ([ VarT 0 ], AllT (1, ArrT ([ VarT 2 ], VarT 0)))))

let () =
  (*
      (\. \. (2 -> 1)) (\. 1 -> 0)
   => (\. (2 -> 1))   [0  :=   (\. 1 -> 0)]
   => \. (1 -> (\. 2 -> 0))
   *)
  check_equal_t ~name:"type_subst test 1"
    (fun () ->
      type_subst
        (AllT (1, ArrT ([ VarT 2 ], VarT 1)))
        0
        (AllT (1, ArrT ([ VarT 1 ], VarT 0))))
    (AllT (1, ArrT ([ VarT 1 ], AllT (1, ArrT ([ VarT 2 ], VarT 0)))))

let () =
  check_equal_t ~name:"tc_check propagates type information into lambdas"
    (fun () ->
      let t = ArrT ([ IntT ], IntT) in
      tc_check (0, Env.empty) (LamE ([ ("x", HoleT (ref None)) ], VarE "x")) t;
      t)
    (ArrT ([ IntT ], IntT))

let () =
  check_equal_t ~name:"tc_check propagates type information into Lambdas"
    (fun () ->
      let t = AllT (1, ArrT ([ VarT 0 ], VarT 0)) in
      tc_check (0, Env.empty)
        (LAME (1, LamE ([ ("y", HoleT (ref None)) ], VarE "y")))
        t;
      normalize_complete_type t)
    (AllT (1, ArrT ([ VarT 0 ], VarT 0)))

let () =
  check_equal_t
    ~name:"tc_check propagates type information back from lambda body"
    (fun () ->
      let t = ArrT ([ IntT ], HoleT (ref None)) in
      tc_check (0, Env.empty) (LamE ([ ("x", HoleT (ref None)) ], VarE "x")) t;
      normalize_complete_type t)
    (ArrT ([ IntT ], IntT))

let () =
  check_equal_t ~name:"tc_infer correctly opens a term inside big Lambda"
    (fun () ->
      tc_infer (0, Env.empty)
        (LAME
           ( 1,
             LamE
               ([ ("x", VarT 0) ], LAME (3, LamE ([ ("y", VarT 1) ], VarE "x")))
           )))
    (* This is the representation of type ∀ α. α → ∀ β γ δ. β → α,
       the 'delayed' versino of ∀ α β γ δ. α → β → α *)
    (AllT (1, ArrT ([ VarT 0 ], AllT (3, ArrT ([ VarT 1 ], VarT 3)))))

let () =
  (* The tc_check version of the tc_infer test above. *)
  check_equal_t ~name:"tc_check correctly opens a term inside big Lambda"
    (fun () ->
      let t =
        AllT
          (1, ArrT ([ VarT 0 ], AllT (3, ArrT ([ VarT 1 ], HoleT (ref None)))))
      in
      tc_check (0, Env.empty)
        (LAME
           ( 1,
             LamE
               ( [ ("x", HoleT (ref None)) ],
                 LAME (3, LamE ([ ("y", HoleT (ref None)) ], VarE "x")) ) ))
        t;
      normalize_complete_type t)
    (AllT (1, ArrT ([ VarT 0 ], AllT (3, ArrT ([ VarT 1 ], VarT 3)))))

let result = ()
