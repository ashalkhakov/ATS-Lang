//
// K&R, 2nd edition, pages 181 - 182
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "libc/SATS/dirent.sats"
staload "libc/sys/SATS/stat.sats"
staload "libc/sys/SATS/types.sats"

(* ****** ****** *)

%{^

#define MAXPATHLEN 1024

%}

#define MAXPATHLEN 1024


(* ****** ****** *)

extern fun fsize (name: string): void
extern fun dirwalk (dir: string, f: (string) -> void): void

implement main (argc, argv) = let
  fun loop {n,i:nat | i <= n}
    (A: &(@[string][n]), n: int n, i: int i): void =
    if i < n then (fsize A.[i]; loop (A, n, i+1)) else ()
  // end of [loop]
in
  if argc = 1 then fsize "." else loop (argv, argc, 1)
end // end of [main]

(* ****** ****** *)

implement fsize (name) = let
  var stbuf: stat with pf0 // uninitialized
  val (pf1 | err) = stat_err (pf0 | name, &stbuf)
in
  if :(pf0: stat? @ stbuf) => err >= 0 then let
    prval stat_v_succ (pf2) = pf1
    val mode = stat_st_mode_get (stbuf)
  in
    if S_ISDIR (mode) then let
      prval () = pf0 := pf2 in dirwalk (name, fsize)
    end else let
      val sz_off = stat_st_size_get (stbuf)
      val sz_lint = lint_of_off (sz_off)
      prval () = pf0 := pf2
    in
      printf ("%8ld %s\n", @(sz_lint, name))
    end // end of [if]
  end else let
    prval stat_v_fail (pf2) =  pf1; prval () = pf0 := pf2
  in
    // empty
  end // end of [if]
end // end of [fsize]

(* ****** ****** *)

extern fun dirent_is_self (dp: &dirent):<> bool
  = "atslib_dirent_is_self"

extern fun dirent_is_parent (dp: &dirent):<> bool
  = "atslib_dirent_is_parent"

%{^

static inline
ats_bool_type
atslib_dirent_is_self (ats_ref_type dp) {
  int cmp = strcmp (((ats_dirent_type*)dp)->d_name, ".") ;
  if (cmp == 0) return ats_true_bool ;
  return ats_false_bool ;
} /* end of [atslib_dirent_is_self] */

static inline
ats_bool_type
atslib_dirent_is_parent (ats_ref_type dp) {
  int cmp = strcmp (((ats_dirent_type*)dp)->d_name, "..") ;
  if (cmp == 0) return ats_true_bool ;
  return ats_false_bool ;
} /* end of [atslib_dirent_is_self] */

%}

(* ****** ****** *)

implement dirwalk (dirname, f) = let
(*
  val () = begin
    prerr "dirwalk: dirname = "; prerr dirname; prerr_newline ()
  end
*)
  #define M MAXPATHLEN
  typedef buf_t = bytes M
  var !p_name with pf_name = @[byte][M]()
  val () = pf_name := bytes_v_of_b0ytes_v (pf_name)
  val (pfopt_dir | p_dir) = opendir_err (dirname)
in
  if (p_dir <> null) then let
    prval Some_v pf_dir = pfopt_dir
    fun loop (buf: &buf_t, dir: &DIR):<cloref1> void = let
      val (pfboxopt | p_ent) = readdir_err (dir)
    in
      if (p_ent <> null) then let
        prval Some_v pfbox = pfboxopt
        prval vbox pf = pfbox
      in
        case+ 0 of
        | _ when dirent_is_self (!p_ent) =>
            $effmask_ref (loop (buf, dir))
        | _ when dirent_is_parent (!p_ent) =>
            $effmask_ref (loop (buf, dir))
        | _ => let
            val direntnameopt =
              direntnameopt_make (buf, dirname, !p_ent) where {
              extern fun direntnameopt_make (
                buf: &buf_t, dir: string, ent: &dirent
              ) :<> Stropt = "direntnameopt_make" 
            }
            val () = begin
              if stropt_is_some (direntnameopt) then begin
                $effmask_ref (f (stropt_unsome direntnameopt))
              end // end of [if]
            end // end of [val]
          in
            $effmask_ref (loop (buf, dir))
          end // end of [_]
      end else let
        prval None_v () = pfboxopt
      in
        // loop exists
      end // end of [if]
    end // end of [loop]
    val () = loop (!p_name, !p_dir)
  in
    closedir_exn (pf_dir | p_dir)
  end else let
    prval None_v () = pfopt_dir
  in
    prerrf ("dirwalk: can't open [%s]\n", @(dirname))
  end // end of [if]
end // end of [dirwalk]

(* ****** ****** *)

%{^

ats_ptr_type direntnameopt_make (
  ats_ref_type buf, ats_ptr_type dir, ats_ref_type ent
) {
  int cnt ;
  cnt = snprintf
    ((char*)buf, MAXPATHLEN, "%s/%s", dir, ((ats_dirent_type*)ent)->d_name) ;
  if (cnt < MAXPATHLEN) return buf ;
  return (ats_ptr_type)0 ;
}

%}


(* ****** ****** *)

(* end of [fsize.dats] *)
