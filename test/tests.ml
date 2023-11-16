(* line 1 of getting_started.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) cf_idempotent (h : int) = (cf (cf h) = cf h) (*hint: \
        axiom *)")
  = [
      Oprovl.Ast.Lemma
        ( "cf_idempotent",
          Some [ Oprovl.Ast.TypeAnotation ("h", "int") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "cf",
                  Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h") ),
              Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h") ),
          Some Oprovl.Ast.Axiom );
    ]

(* line 2 of getting_started.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) inv_involution (h : int) = (inv (inv h) = h) (*hint: \
        axiom *)")
  = [
      Oprovl.Ast.Lemma
        ( "inv_involution",
          Some [ Oprovl.Ast.TypeAnotation ("h", "int") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "inv",
                  Oprovl.Ast.Function (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h")
                ),
              Oprovl.Ast.Id "h" ),
          Some Oprovl.Ast.Axiom );
    ]

(* line 3 of getting_started.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) cf_inv_commute (h : int) = (cf (inv h) = inv (cf h)) \
        (*hint: axiom *)")
  = [
      Oprovl.Ast.Lemma
        ( "cf_inv_commute",
          Some [ Oprovl.Ast.TypeAnotation ("h", "int") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "cf",
                  Oprovl.Ast.Function (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h")
                ),
              Oprovl.Ast.Function
                ( Oprovl.Ast.Id "inv",
                  Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h") )
            ),
          Some Oprovl.Ast.Axiom );
    ]

(* line 4 of getting_started.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) cf_inv_property (h : int) = (cf (inv (cf (inv h))) = cf \
        h)")
  = [
      Oprovl.Ast.Lemma
        ( "cf_inv_property",
          Some [ Oprovl.Ast.TypeAnotation ("h", "int") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "cf",
                  Oprovl.Ast.Function
                    ( Oprovl.Ast.Id "inv",
                      Oprovl.Ast.Function
                        ( Oprovl.Ast.Id "cf",
                          Oprovl.Ast.Function
                            (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h") ) ) ),
              Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h") ),
          None );
    ]

(* basic tst of type declaration *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "type list = | Nil | Cons")
  = [
      Oprovl.Ast.Type
        ( "list",
          [
            Oprovl.Ast.Constructor ("Nil", None);
            Oprovl.Ast.Constructor ("Cons", None);
          ] );
    ]

(* first line of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "type list = | Nil | Cons of (int * list)")
  = [
      Oprovl.Ast.Type
        ( "list",
          [
            Oprovl.Ast.Constructor ("Nil", None);
            Oprovl.Ast.Constructor
              ("Cons", Some [ Oprovl.Ast.Id "int"; Oprovl.Ast.Id "list" ]);
          ] );
    ]

(* line 1 of sample.ml without parentheses *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "type list = | Nil | Cons of int * list")
  = [
      Oprovl.Ast.Type
        ( "list",
          [
            Oprovl.Ast.Constructor ("Nil", None);
            Oprovl.Ast.Constructor
              ("Cons", Some [ Oprovl.Ast.Id "int"; Oprovl.Ast.Id "list" ]);
          ] );
    ]

(* basic let rec function test with function return type *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "let rec f1 (l1 : list) (l2 : list) : list = l1")
  = [
      Oprovl.Ast.RecFunction
        ( Oprovl.Ast.FunctionHeader
            ( "f1",
              Some "list",
              [
                Oprovl.Ast.TypeAnotation ("l1", "list");
                Oprovl.Ast.TypeAnotation ("l2", "list");
              ] ),
          Oprovl.Ast.Id "l1" );
    ]

(* basic let rec function test without function return type *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "let rec f1 (l1 : list) (l2 : list) = l1")
  = [
      Oprovl.Ast.RecFunction
        ( Oprovl.Ast.FunctionHeader
            ( "f1",
              None,
              [
                Oprovl.Ast.TypeAnotation ("l1", "list");
                Oprovl.Ast.TypeAnotation ("l2", "list");
              ] ),
          Oprovl.Ast.Id "l1" );
    ]

(* line 1 of sample.ml*)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string "type list = | Nil | Cons of (int * list)")
  = [
      Oprovl.Ast.Type
        ( "list",
          [
            Oprovl.Ast.Constructor ("Nil", None);
            Oprovl.Ast.Constructor
              ("Cons", Some [ Oprovl.Ast.Id "int"; Oprovl.Ast.Id "list" ]);
          ] );
    ]

(* line 2 of sample.ml*)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let rec append (l1 : list) (l2 : list) : list = match l1 with | Nil -> \
        l2 | Cons ((h : int), (t : list)) -> Cons (h, append t l2)")
  = [
      Oprovl.Ast.RecFunction
        ( Oprovl.Ast.FunctionHeader
            ( "append",
              Some "list",
              [
                Oprovl.Ast.TypeAnotation ("l1", "list");
                Oprovl.Ast.TypeAnotation ("l2", "list");
              ] ),
          Oprovl.Ast.Match
            ( Oprovl.Ast.Id "l1",
              [
                Oprovl.Ast.Pattern
                  (Oprovl.Ast.Constructor ("Nil", None), Oprovl.Ast.Id "l2");
                Oprovl.Ast.Pattern
                  ( Oprovl.Ast.Constructor
                      ( "Cons",
                        Some
                          [
                            Oprovl.Ast.TypeAnotation ("h", "int");
                            Oprovl.Ast.TypeAnotation ("t", "list");
                          ] ),
                    Oprovl.Ast.Constructor
                      ( "Cons",
                        Some
                          [
                            Oprovl.Ast.Id "h";
                            Oprovl.Ast.Function
                              ( Oprovl.Ast.Function
                                  (Oprovl.Ast.Id "append", Oprovl.Ast.Id "t"),
                                Oprovl.Ast.Id "l2" );
                          ] ) );
              ] ) );
    ]

(* line 3 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) append_nilnil = (append Nil (Nil) = (Nil))")
  = [
      Oprovl.Ast.Lemma
        ( "append_nilnil",
          None,
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Function
                    ( Oprovl.Ast.Id "append",
                      Oprovl.Ast.Constructor ("Nil", None) ),
                  Oprovl.Ast.Constructor ("Nil", None) ),
              Oprovl.Ast.Constructor ("Nil", None) ),
          None );
    ]

(* line 4 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) append_cons (h : int) (t : list) (l : list) = (append \
        (Cons (h, t)) l = Cons (h, append t l))")
  = [
      Oprovl.Ast.Lemma
        ( "append_cons",
          Some
            [
              Oprovl.Ast.TypeAnotation ("h", "int");
              Oprovl.Ast.TypeAnotation ("t", "list");
              Oprovl.Ast.TypeAnotation ("l", "list");
            ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Function
                    ( Oprovl.Ast.Id "append",
                      Oprovl.Ast.Constructor
                        ("Cons", Some [ Oprovl.Ast.Id "h"; Oprovl.Ast.Id "t" ])
                    ),
                  Oprovl.Ast.Id "l" ),
              Oprovl.Ast.Constructor
                ( "Cons",
                  Some
                    [
                      Oprovl.Ast.Id "h";
                      Oprovl.Ast.Function
                        ( Oprovl.Ast.Function
                            (Oprovl.Ast.Id "append", Oprovl.Ast.Id "t"),
                          Oprovl.Ast.Id "l" );
                    ] ) ),
          None );
    ]

(* line 5 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) append_nil (x : list) = (append x Nil = x) (*hint: \
        induction x *)")
  = [
      Oprovl.Ast.Lemma
        ( "append_nil",
          Some [ Oprovl.Ast.TypeAnotation ("x", "list") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Function (Oprovl.Ast.Id "append", Oprovl.Ast.Id "x"),
                  Oprovl.Ast.Constructor ("Nil", None) ),
              Oprovl.Ast.Id "x" ),
          Some (Oprovl.Ast.Induction "x") );
    ]

(* line 6 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let rec reverse (l : list) : list = match l with | Nil -> Nil | Cons \
        ((h : int), (t : list)) -> append (reverse t) (Cons (h, Nil))")
  = [
      Oprovl.Ast.RecFunction
        ( Oprovl.Ast.FunctionHeader
            ("reverse", Some "list", [ Oprovl.Ast.TypeAnotation ("l", "list") ]),
          Oprovl.Ast.Match
            ( Oprovl.Ast.Id "l",
              [
                Oprovl.Ast.Pattern
                  ( Oprovl.Ast.Constructor ("Nil", None),
                    Oprovl.Ast.Constructor ("Nil", None) );
                Oprovl.Ast.Pattern
                  ( Oprovl.Ast.Constructor
                      ( "Cons",
                        Some
                          [
                            Oprovl.Ast.TypeAnotation ("h", "int");
                            Oprovl.Ast.TypeAnotation ("t", "list");
                          ] ),
                    Oprovl.Ast.Function
                      ( Oprovl.Ast.Function
                          ( Oprovl.Ast.Id "append",
                            Oprovl.Ast.Function
                              (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "t") ),
                        Oprovl.Ast.Constructor
                          ( "Cons",
                            Some
                              [
                                Oprovl.Ast.Id "h";
                                Oprovl.Ast.Constructor ("Nil", None);
                              ] ) ) );
              ] ) );
    ]

(* line 7 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) rev_rev (x : list) = (reverse (reverse x) = x) (*hint: \
        axiom *)")
  = [
      Oprovl.Ast.Lemma
        ( "rev_rev",
          Some [ Oprovl.Ast.TypeAnotation ("x", "list") ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "reverse",
                  Oprovl.Ast.Function
                    (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "x") ),
              Oprovl.Ast.Id "x" ),
          Some Oprovl.Ast.Axiom );
    ]

(* line 8 of sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) append_assoc (l1 : list) (l2 : list) (l3 : list) = \
        (append (append l1 l2) l3 = append l1 (append l2 l3)) (*hint: \
        induction l1 *)")
  = [
      Oprovl.Ast.Lemma
        ( "append_assoc",
          Some
            [
              Oprovl.Ast.TypeAnotation ("l1", "list");
              Oprovl.Ast.TypeAnotation ("l2", "list");
              Oprovl.Ast.TypeAnotation ("l3", "list");
            ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Function
                    ( Oprovl.Ast.Id "append",
                      Oprovl.Ast.Function
                        ( Oprovl.Ast.Function
                            (Oprovl.Ast.Id "append", Oprovl.Ast.Id "l1"),
                          Oprovl.Ast.Id "l2" ) ),
                  Oprovl.Ast.Id "l3" ),
              Oprovl.Ast.Function
                ( Oprovl.Ast.Function
                    (Oprovl.Ast.Id "append", Oprovl.Ast.Id "l1"),
                  Oprovl.Ast.Function
                    ( Oprovl.Ast.Function
                        (Oprovl.Ast.Id "append", Oprovl.Ast.Id "l2"),
                      Oprovl.Ast.Id "l3" ) ) ),
          Some (Oprovl.Ast.Induction "l1") );
    ]

(* line 9 sample.ml *)
let%test _ =
  Oprovl.Parser.prog Oprovl.Lexer.read
    (Lexing.from_string
       "let (*prove*) rev_append (l1 : list) (l2 : list) = (reverse (append l1 \
        l2) = append (reverse l2) (reverse l1)) (*hint: induction l1 *)")
  = [
      Oprovl.Ast.Lemma
        ( "rev_append",
          Some
            [
              Oprovl.Ast.TypeAnotation ("l1", "list");
              Oprovl.Ast.TypeAnotation ("l2", "list");
            ],
          Oprovl.Ast.Equal
            ( Oprovl.Ast.Function
                ( Oprovl.Ast.Id "reverse",
                  Oprovl.Ast.Function
                    ( Oprovl.Ast.Function
                        (Oprovl.Ast.Id "append", Oprovl.Ast.Id "l1"),
                      Oprovl.Ast.Id "l2" ) ),
              Oprovl.Ast.Function
                ( Oprovl.Ast.Function
                    ( Oprovl.Ast.Id "append",
                      Oprovl.Ast.Function
                        (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "l2") ),
                  Oprovl.Ast.Function
                    (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "l1") ) ),
          Some (Induction "l1") );
    ]