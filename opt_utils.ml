let get_float strs name =
  let pat = name in
  let n = String.length pat in
  let r = Str.regexp pat in
  let rec find = function
    | [] -> raise Not_found
    | str :: t ->
      let i = try Str.search_forward r str 0 with Not_found -> -1 in
      if i == 0 then
	float_of_string (Str.string_after str n)
      else
	find t in
  find strs;;
