type expr =
    Variable of data_type
  | Assign of string * expr
  | Noexpr

type stmt =
    Block of stmt list
  | Master of expr * expr * stmt
  | Print of expr

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type data_type =
    Map of string
  | Array of string

type program = data_type list * func_decl list
