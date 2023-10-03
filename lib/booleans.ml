open Core

type 'a boolean = 'a -> 'a -> 'a

let true' : 'a boolean = fun t _ -> t
let false' : 'a boolean = fun _ f -> f

let ( && ) (x : 'a boolean) (y : 'a boolean) : 'a boolean =
 fun t f -> x (y t f) f

let ( || ) (x : 'a boolean) (y : 'a boolean) : 'a boolean =
 fun t f -> x t (y t f)

let ( ! ) (x : 'a boolean) : 'a boolean = fun t f -> x f t

type op = AND | OR | NOT

let string_of_booleans (bs : 'a boolean list) =
  List.map ~f:(fun b -> b "true" "false") bs |> String.concat ~sep:", "

let test_op ~op =
  match op with
  | AND ->
      List.map
        ~f:(fun (x, y) ->
          let res = x && y in
          Fmt.str "(%s) -> %s"
            (string_of_booleans [ x; y ])
            (string_of_booleans [ res ]))
        [ (false', false'); (false', true'); (true', false'); (true', true') ]
      |> String.concat ~sep:"\n" |> Fmt.str "AND Results:\n%s"
  | OR ->
      List.map
        ~f:(fun (x, y) ->
          let res = x || y in
          Fmt.str "(%s) -> %s"
            (string_of_booleans [ x; y ])
            (string_of_booleans [ res ]))
        [ (false', false'); (false', true'); (true', false'); (true', true') ]
      |> String.concat ~sep:"\n" |> Fmt.str "OR Results:\n%s"
  | NOT ->
      List.map
        ~f:(fun x ->
          let res = !x in
          Fmt.str "%s -> %s" (string_of_booleans [ x ])
            (string_of_booleans [ res ]))
        [ false'; true' ]
      |> String.concat ~sep:"\n" |> Fmt.str "NOT Results:\n%s"
