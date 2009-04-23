(*
**
** a Tiger compiler written in ATS
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Spring, 2009
**
*)

(* ****** ****** *)

staload "templab.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

local

assume temp_t = int64

#define zro (int64_of_int 0)
#define one (int64_of_int 1)

val the_temp_base = int64_of_int (100)
val the_temp_count = ref_make_elt<int64> (the_temp_base)

in // in of [local]

implement temp_bogus = int64_of_int (~1)

implement temp_is_bogus (tmp) =
  if tmp < zro then true else false
// end of [temp_is_bogus]

implement temp_isnot_bogus (tmp) =
  if tmp >= zro then true else false
// end of [temp_isnot_bogus]

(* ****** ****** *)

implement temp_make_new () = let
  val n = !the_temp_count in !the_temp_count := n + one; n
end // end of [temp_make_new]

implement temp_make_fixed (n) = int64_of_int n

implement eq_temp_temp
  (tmp1, tmp2) = eq_int64_int64 (tmp1, tmp2)
// end of [eq_temp_temp]

implement compare_temp_temp
  (tmp1, tmp2) = compare_int64_int64 (tmp1, tmp2)
// end of [compare_temp_temp]

implement fprint_temp (out, tmp) = begin
  fprint_string (out, "tmp"); fprint_int64 (out, tmp)
end

implement temp_is_fixed (tmp) =
  if tmp < the_temp_base then true else false
// end of [temp_is_special]

end // end of [local]

implement print_temp tmp = fprint_temp (stdout_ref, tmp)
implement prerr_temp tmp = fprint_temp (stderr_ref, tmp)

implement fprint_templst (out, tmps) = let
  fun loop
    (out: FILEref, tmps: templst, i: int): void =
    case+ tmps of
    | list_cons (tmp, tmps) => begin
        if i > 0 then fprint_string (out, ", ");
        fprint_temp (out, tmp); loop (out, tmps, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
in
  loop (out, tmps, 0)
end // end of [fprint_templst]

implement print_templst tmps = fprint_templst (stdout_ref, tmps)
implement prerr_templst tmps = fprint_templst (stderr_ref, tmps)

(* ****** ****** *)

#define LABEL_PREFIX "_TIGERATS_LAB"

local

datatype label = LABint of int64 | LABname of string

assume label_t = label

#define zro (int64_of_int 0)
#define one (int64_of_int 1)

val the_label_count = ref_make_elt<int64> (zro)

in

implement label_make_new () = let
  val n = !the_label_count in
  !the_label_count := n + one; LABint n
end // end of [temp_make_new]

implement label_make_name (name) = LABname (name)

implement label_name_get (lab) = case+ lab of
  | LABint ind => LABEL_PREFIX + tostring_int64 (ind)
  | LABname name => name
// end of [label_name_get]

(* ****** ****** *)

implement eq_label_label (lab1, lab2) =
  case+ (lab1, lab2) of
  | (LABint i1, LABint i2) => eq_int64_int64 (i1, i2)
  | (LABname s1, LABname s2) => eq_string_string (s1, s2)
  | (_, _) => false
// end of [eq_label_label]

implement compare_label_label (lab1, lab2) =
  case+ (lab1, lab2) of
  | (LABint i1, LABint i2) => compare_int64_int64 (i1, i2)
  | (LABname s1, LABname s2) => compare_string_string (s1, s2)
  | (LABint _, LABname _) => ~1
  | (LABname _, LABint _) =>  1
// end of [compare_label_label]

(* ****** ****** *)

implement fprint_label (out, lab) = case+ lab of
  | LABint ind => begin
      fprint_string (out, LABEL_PREFIX); fprint_int64 (out, ind)
    end // end of [LABint]
  | LABname name => fprint_string (out, name)
// end of [fprint_label]

end // end of [local]

implement print_label lab = fprint_label (stdout_ref, lab)
implement prerr_label lab = fprint_label (stderr_ref, lab)

implement fprint_lablst (out, labs) = let
  fun loop
    (out: FILEref, labs: lablst, i: int): void =
    case+ labs of
    | list_cons (lab, labs) => begin
        if i > 0 then fprint_string (out, ", ");
        fprint_label (out, lab); loop (out, labs, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
in
  loop (out, labs, 0)
end // end of [fprint_lablst]

implement print_lablst labs = fprint_lablst (stdout_ref, labs)
implement prerr_lablst labs = fprint_lablst (stderr_ref, labs)

(* ****** ****** *)

implement tigerats_chr_lab = label_make_name ("tigerats_chr_lab")
implement tigerats_flush_lab = label_make_name ("tigerats_flush_lab")
implement tigerats_getchar_lab = label_make_name ("tigerats_getchar_lab")
implement tigerats_ord_lab = label_make_name ("tigerats_ord_lab")
implement tigerats_print_lab = label_make_name ("tigerats_print_lab")
implement tigerats_print_int_lab = label_make_name ("tigerats_print_int_lab")
implement tigerats_size_lab = label_make_name ("tigerats_size_lab")
implement tigerats_substring_lab = label_make_name ("tigerats_substring_lab")
implement tigerats_concat_lab = label_make_name ("tigerats_concat_lab")
implement tigerats_not_lab = label_make_name ("tigerats_not_lab")
implement tigerats_exit_lab = label_make_name ("tigerats_exit_lab")

implement tigerats_main_lab = label_make_name ("tigerats_main")

implement tigerats_array_alloc_lab = label_make_name ("tigerats_array_alloc")
implement tigerats_array_make_elt_lab = label_make_name ("tigerats_array_make_elt")

implement tigerats_eq_string_string_lab = label_make_name ("tigerats_eq_string_string")
implement tigerats_neq_string_string_lab = label_make_name ("tigerats_neq_string_string")

(* ****** ****** *)

(* end of [templab.dats] *)
