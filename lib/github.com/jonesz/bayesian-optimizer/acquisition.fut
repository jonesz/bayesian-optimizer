import "../../diku-dk/linalg/linalg"

module UCB (R: real) = {
	type t = R.t
	module L = mk_ordered_linalg R

	-- u(x) + kappa * sigma(x)
	def acq [m][n][d] (pd: [m][d]t -> [m]t -> [n][d]t -> ([n]t, [n][n]t)) kappa (D_x: [m][d]t) (D_f: [m]t) (x_next: [n][d]t) =
		let (u, var) = pd D_x D_f x_next
		let var = L.fromdiag var
		in map2 (\u_i var_i -> R.sqrt var_i |> (R.*) kappa |> (R.+) u_i) u var
}
