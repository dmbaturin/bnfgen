type grammar

val load_from_string : string -> (grammar, string) result

val load_from_channel : in_channel -> (grammar, string) result

val load_from_file : string -> (grammar, string) result

val dump_rules : grammar -> string

val generate : ?debug:(string -> unit) -> ?max_depth:int option -> ?start_symbol:string -> ?separator:string -> grammar -> (string, string) result

