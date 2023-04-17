type style

val no_color : bool ref

val print : style list -> string -> unit

val println : style list -> string -> unit

val task : ?style:style list -> string -> (unit -> 'a) -> 'a

val party_emoji : unit -> string

val red : style

val green : style

val yellow : style

val blue : style

val cyan : style

val magenta : style

val bold : style
