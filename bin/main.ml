open Simply_lambda

let () =
  let and_results = Booleans.test_op ~op:AND in
  let or_results = Booleans.test_op ~op:OR in
  let not_results = Booleans.test_op ~op:NOT in
  Printf.printf "%s\n\n%s\n\n%s\n" and_results or_results not_results
