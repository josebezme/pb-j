type expr =
    Id of string
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

type program = string list * func_decl list
