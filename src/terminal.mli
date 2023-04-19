type style

val no_color : bool ref

val disable_progress_bars : bool ref

val print : style list -> string -> unit

val println : style list -> string -> unit

val task : ?style:style list -> string -> (unit -> 'a) -> 'a

val party_emoji : unit -> string

type progress_bar

val progress_bar : nsteps:int -> progress_bar

val open_progress_bar : step:int -> info:(int -> string) -> progress_bar

val with_progress_bar :
  string -> progress_bar -> (progress:(int -> unit) -> 'a) -> 'a

val red : style

val green : style

val yellow : style

val blue : style

val cyan : style

val magenta : style

val bold : style
