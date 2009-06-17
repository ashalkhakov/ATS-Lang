(*
** Course: Concepts of Programming Languages (BU CAS CS 320)
** Semester: Summer I, 2009
** Instructor: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: June, 2009
//

(* ****** ****** *)

staload "PARCOMB/posloc.sats"

(* ****** ****** *)

staload "fixity.sats"

(* ****** ****** *)

implement fixopr_loc_get (opr) = case+ opr of
  | Prefix (loc, _, _) => loc
  | Infix (loc, _, _, _) => loc
  | Postfix (loc, _, _) => loc
// end of [fixopr_assoc_get]

(* ****** ****** *)

fn fixopr_assoc_get {a:type}
  (opr: fixopr a): assoc = case+ opr of
  | Infix (_, _, assoc, _) => assoc | _ => NonAssoc
// end of [fixopr_assoc_get]

fn fixopr_prec_get {a:type}
  (opr: fixopr a): int(*prec*) = case+ opr of
  | Prefix (_, p, _) => p
  | Infix (_, p, _, _) => p
  | Postfix (_, p, _) => p
// end of [fixopr_prec_get]

(* ****** ****** *)

#define APP_precedence 70
#define APP_assoc LeftAssoc

implement fixitm_make_app (f) =
  FIXITMopr (Infix (location_none, APP_precedence, APP_assoc, f))
// end of [fixitm_make_app]

(* ****** ****** *)

implement fixity_resolve {a} (app, ys) = let
  #define nil list_nil; #define :: list_cons
  #define ATM FIXITMatm; #define OPR FIXITMopr
  typedef I = fixitm a and IS = List (fixitm a)
  fun resolve (xs: IS, m: I, ys: IS):<cloref1> Option_vt a =
    case+ (xs, m, ys) of
    | (_, ATM _, _) => begin case+ xs of
      | ATM _ :: _ => resolve_app (xs, m, ys)
      | _ => next (m :: xs, ys)
      end // end of [begin]
    | (_, OPR (Prefix _), _) => next (m :: xs, ys)
    | (x :: nil (), OPR (Infix _), _) => next (m :: x :: nil (), ys)

    | (x :: (m1 as OPR f1) :: _, OPR (f as Infix _), _) => let
        val p = fixopr_prec_get f and p1 = fixopr_prec_get f1
      in
        case+ 0 of
        | _ when p > p1 => next (m :: xs, ys)
        | _ when p < p1 => reduce (xs, m :: ys)
        | _ (* p = p1 *) => let
            val assoc = fixopr_assoc_get f
            and assoc1 = fixopr_assoc_get f1
          in
            case+ (assoc, assoc1) of
            | (LeftAssoc (), LeftAssoc ()) => reduce (xs, m :: ys)
            | (RightAssoc (), RightAssoc ()) => next (m :: xs, ys)
            | (_, _) => None_vt ()
          end // end of [_]
      end (* end of [...] *)
    | (x :: nil (), OPR (Postfix _), _) => reduce (m :: x :: nil (), ys)
    | (x :: (m1 as OPR f1) :: _, OPR (f as Postfix _), _) => let
        val p = fixopr_prec_get f and p1 = fixopr_prec_get f1
      in
        case+ 0 of
        | _ when p > p1 => reduce (m :: xs, ys)
        | _ when p < p1 => reduce (xs, m :: ys)
        | _ (* p = p1 *) => None_vt ()
      end // end of [...]
    | (_, _, _) => None_vt ()
  // end of [resolve]

  and resolve_app
    (xs: IS, m: I, ys: IS):<cloref1> Option_vt a = case+ xs of
    | _ :: OPR opr1 :: _ => let
        val p1 = fixopr_prec_get opr1
        val sgn = compare (APP_precedence, p1): Sgn
      in
        case+ sgn of
        | 1 => next (m :: app :: xs, ys) | ~1 => reduce (xs, m :: ys)
        | _ (*0*) => let
            val assoc1 = fixopr_assoc_get opr1 in case+ assoc1 of
              | LeftAssoc () => reduce (xs, m :: ys) | _ => None_vt ()
          end // end of [_]
      end // end of [_ :: OPR :: _]
    | _ :: nil () => next (m :: app :: xs, ys)
    | _ => None_vt ()
  // end of [resolve_app]
              
  and reduce (xs: IS, ys: IS):<cloref1> Option_vt a = case+ xs of
    | (ATM a :: OPR (Prefix (_, _, f_pre)) :: xs1) =>
        next (ATM (f_pre a) :: xs1, ys)
    | (ATM a2 :: OPR (Infix (_, _, _, f_inf)) :: ATM a1 :: xs1) =>
        next (ATM (f_inf (a1, a2)) :: xs1, ys)
    | (OPR (Postfix (_, _, f_pos)) :: ATM a :: xs1) =>
        next (ATM (f_pos a) :: xs1, ys)
    | _ => None_vt ()
  // end of [reduce]

  and next (xs: IS, ys: IS):<cloref1> Option_vt a = case+ (xs, ys) of
    | (ATM a :: nil (), nil ()) => Some_vt (a)
    | (_, nil ()) => reduce (xs, nil)
    | (_, y :: ys1) => resolve (xs, y, ys1)
  // end of [next]`
in
  next (nil (), ys)
end // end of [fixity_resolve]

(* ****** ****** *)

(* end of [fixity.dats] *)
