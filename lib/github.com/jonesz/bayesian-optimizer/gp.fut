import "../../diku-dk/linalg/linalg"

def cov kern x_0 x_1 = map (\a -> map (\b -> kern a b) x_1) x_0

-- NOTE: We pass in `C_inv` because of autodiff computations; if we compute the derivative
-- with `vjp`, etc., then autodiff will include whatever routine computes the inverse, which
-- busts it.

module gp_linalg(R: real) = {
	type t = R.t
	module L = mk_ordered_linalg R

	def compute_C_inv [m] [d] (kern: [d]t -> [d]t -> t) (X: [m][d]t) =
		cov (kern) X X |> L.inv
		
	-- u(x) + K(x, X) * inv(K(X, X)) * (Y - u(X)
	def u_D [m] [d] [n] (mean: [d]t -> t) (kern: [d]t -> [d]t -> t) (C_inv: [m][m]t) (X: [m][d]t) (Y: [m]t) (x: [n][d]t) : [n]t =
		let u_s = map (mean) x
		let K_s = cov kern x X
		let m   = map (mean) X |> map2 (R.-) Y
		in L.matmul K_s C_inv |> flip L.matvecmul_row m |> map2 (R.+) u_s

	-- K(x, x) - K(x, X) * inv(K(X, X)) * K(X, x)
	def K_D [m] [d] [n] (kern: [d]t -> [d]t -> t) (C_inv: [m][m]t) (X: [m][d]t) (x: [n][d]t) : [n][n]t =
		let K_ss   = cov kern x x
		let K_s_l  = cov kern x X
		let K_s_r  = transpose K_s_l
		in L.matmul K_s_l C_inv |> flip (L.matmul) K_s_r |> L.matsub K_ss

	def pd mean kern C_inv X Y x = (u_D mean kern C_inv X Y x, K_D kern C_inv X x)
}
