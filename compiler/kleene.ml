(*
 * Solving the inductive kleene step requires a basis, k, and range (i,j) to
 * produce a new basis that can be used to solve subsequent inductive steps.
 *)
let inductive_step basis k max_i max_j =
  let next_basis = Hashtbl.create 100 in
  let inductive_inc_step i j =
    Re.Union
      ( Hashtbl.find basis (i, j),
        Re.Concatenation
          ( Re.Concatenation
              (Hashtbl.find basis (i, k), Re.Closure (Hashtbl.find basis (k, k))),
            Hashtbl.find basis (k, j) ) )
  in
  for i = 1 to max_i do
    for j = 1 to max_j do
      Hashtbl.add next_basis (i, j) (inductive_inc_step i j)
    done
  done;

  next_basis
