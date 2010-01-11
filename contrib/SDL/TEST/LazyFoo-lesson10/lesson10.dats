//
// LazyFoo-lesson10 _translated_ into ATS
// See http://lazyfoo.net/SDL_tutorials/lesson10
//

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: January, 2010
//

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"
staload "contrib/SDL/SATS/SDL_ttf.sats"

(* ****** ****** *)

#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define SCREEN_BPP 32

(* ****** ****** *)

symintr int
overload int with int_of_Sint16
overload int with int_of_Uint16
overload int with int_of_SDL_EventType

(* ****** ****** *)

//
// this one is in the SDL_image extension of SDL
//
extern
fun IMG_Load (filename: string): SDL_Surface_ref0 
  = "IMG_Load"

extern
fun load_image (filename: string): SDL_Surface_ref0
implement load_image (filename) = let
  val loadedImage = IMG_Load (filename)
in
  if ref_is_null loadedImage then loadedImage else let
    val optimizedImage = SDL_DisplayFormat (loadedImage)
    val () = SDL_FreeSurface (loadedImage)
  in
    if ref_is_null (optimizedImage) then optimizedImage else let
      // Map the color key
      val (pf_format | p_format) = SDL_Surface_format (optimizedImage)
      val colorkey = SDL_MapRGB (!p_format, (Uint8)0, (Uint8)0xFF, (Uint8)0xFF)
      //Set all pixels of color R 0, G 0xFF, B 0xFF to be transparent
      prval () = minus_addback (pf_format | optimizedImage)
      val _err = SDL_SetColorKey (optimizedImage, SDL_SRCCOLORKEY, colorkey)
    in
      optimizedImage
    end // end of [if]
  end // end of [if]
end // end of [load_image]

(* ****** ****** *)

extern
fun apply_surface {l1,l2:anz} (
    x: int, y: int, src: !SDL_Surface_ref l1, dst: !SDL_Surface_ref l2
  ) : void

implement apply_surface
  (x, y, src, dst) = () where {
  var offset: SDL_Rect // unintialized
  val () = SDL_Rect_init (offset, (Sint16)x, (Sint16)y, (Uint16)0, (Uint16)0)
  val _err = SDL_UpperBlit_ptr (src, null, dst, &offset)
} // end of [apply_surface]

(* ****** ****** *)

implement main () = () where {
  val _err = SDL_Init (SDL_INIT_EVERYTHING)
  val () = assert_errmsg (_err = 0, #LOCATION)
  val [l1:addr] screen = SDL_SetVideoMode (
    SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, SDL_SWSURFACE
  ) // end of [val]
  val () = assert_errmsg (ref_isnot_null screen, #LOCATION)
  val _err = TTF_Init ()
  val () = assert_errmsg (_err = 0, #LOCATION)
//
  val () = SDL_WM_SetCaption (
    stropt_some "Press an Arrow Key", stropt_none
  ) // end of [val]
//
  val [l2:addr] background = load_image ("LazyFoo-lesson10/background.png")
  val () = assert_errmsg (ref_isnot_null background, #LOCATION)
//
  // Open the font
  val font = TTF_OpenFont ("LazyFoo-lesson10/lazy.ttf", 72)
  val () = assert_errmsg (TTF_Font_ref_isnot_null font, #LOCATION)
//
  //The color of the font
  var textColor : SDL_Color
  val () = SDL_Color_init (textColor, (Uint8)0, (Uint8)0, (Uint8)0)
  // Render the text
  val [_l:addr] up = TTF_RenderText_Solid (font, "Up", textColor)
  val () = assert_errmsg (ref_isnot_null up, #LOCATION)
  val [_l:addr] down = TTF_RenderText_Solid (font, "Down", textColor)
  val () = assert_errmsg (ref_isnot_null down, #LOCATION)
  val [_l:addr] left = TTF_RenderText_Solid (font, "Left", textColor)
  val () = assert_errmsg (ref_isnot_null left, #LOCATION)
  val [_l:addr] right = TTF_RenderText_Solid (font, "Right", textColor)
  val () = assert_errmsg (ref_isnot_null right, #LOCATION)
//
  var quit: bool = false
  var event: SDL_Event?
  val () = while (~quit) let
//
    val () = while (true) begin
      if SDL_PollEvent (event) > 0 then let
        prval () = opt_unsome (event)
        val _type = SDL_Event_type event
(*
        val () = printf ("event.type = %i\n", @((int)_type))
*)
        val () = if _type = SDL_QUIT then quit := true
      in
        continue
      end else let
        prval () = opt_unnone (event) in break
      end // end of [if]
    end // end of [val]
//
    val () = apply_surface (0, 0, background, screen)
//
    val keystates = SDL_GetKeyState_null ()
//
    val () = if (keystates[SDLK_UP] > 0) then let
      val w = SDL_Surface_w up and h = SDL_Surface_h up
    in
      apply_surface((SCREEN_WIDTH - w) / 2, (SCREEN_HEIGHT / 2 - h) / 2, up, screen)
    end // end of [val]
//
    val () = if (keystates[SDLK_DOWN] > 0) then let
      val w = SDL_Surface_w down and h = SDL_Surface_h down
    in
      apply_surface((SCREEN_WIDTH - w) / 2, (SCREEN_HEIGHT / 2 - h) / 2 + SCREEN_HEIGHT / 2, down, screen)
    end // end of [val]
//
    val () = if (keystates[SDLK_LEFT] > 0) then let
      val w = SDL_Surface_w left and h = SDL_Surface_h left
    in
      apply_surface((SCREEN_WIDTH / 2 - w) / 2, (SCREEN_HEIGHT - h) / 2, left, screen)
    end // end of [val]
//
    val () = if (keystates[SDLK_RIGHT] > 0) then let
      val w = SDL_Surface_w right and h = SDL_Surface_h right
    in
      apply_surface((SCREEN_WIDTH / 2 - w) / 2 + SCREEN_WIDTH / 2, (SCREEN_HEIGHT - h) / 2, right, screen)
    end // end of [val]
//
    val _err = SDL_Flip (screen)
    val () = assert_errmsg (_err = 0, #LOCATION)
  in
    // nothing
  end // end of [val]
//
  val () = TTF_CloseFont (font)
  val () = SDL_FreeSurface (background)
  val () = SDL_FreeSurface (up)
  val () = SDL_FreeSurface (down)
  val () = SDL_FreeSurface (left)
  val () = SDL_FreeSurface (right)
  val () = TTF_Quit ()
  val _ptr = SDL_Quit_screen (screen)
  val () = SDL_Quit ()
} // end of [main]

(* ****** ****** *)

(* end of [LazyFoo-lesson10.dats] *)
