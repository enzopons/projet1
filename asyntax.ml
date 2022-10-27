type exp = Int of int | Float of float | Par of exp | Moins of exp | 
Plus of exp | IN of exp | FLO of exp | Plusi of exp*exp
| Multi of exp*exp | Moinsi of exp*exp | Divi of exp * exp| Mod of 
exp*exp | Plusf of exp*exp | Multf of exp*exp | Moinsf of exp*exp ;;

let rec bien_type expr = 
  let rec aux = function (*mettre le type stp La deuxieme composante vaut 1 si int et 0 si float *)
    | Int _ -> (true, 1)
    | Float _ -> (true, 0)
    | Par e -> aux e
    | Moins e -> aux e 
    | Plus e -> aux e 
    | IN e ->
        let a, b = aux e in
        (a && b = 0, 1)
    | FLO e ->
        let a, b = aux e in
        (a && b = 1, 0)
    | Plusi(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Plusf(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Multi(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Multf(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Moinsi(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Moinsf(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Mod(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Divi(e1,e2) ->
        let a1, b1 = aux e1 in
        let a2, b2 = aux e2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
  in
  let a, b = aux expr in
  a ;; 


