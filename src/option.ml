type 'a t = 'a option

let value ~default = function
  | None -> default
  | Some value -> value
