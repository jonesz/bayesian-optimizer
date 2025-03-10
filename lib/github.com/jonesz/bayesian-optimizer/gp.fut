import "../../diku-dk/linalg/linalg"

def cov 'a 't (kern: a -> a -> t) x_0 x_1 =
	map (\a -> map (\b -> kern a b) x_1) x_0

-- NOTE: We pass in `C_inv` because of autodiff computations; if we compute the derivative
-- with `vjp`, etc., then autodiff will include whatever routine computes the inverse, which
-- busts it.
	
-- u(x) + K(x, X) * inv(K(X, X)) * (Y - u(X)
def func_u_D 'a [z] [l] mean kern add sub matmul matvecmul_row C_inv (X: [z]a) Y (x: [l]a) : [l]a =
	let u_s = map (mean) x
	let K_s = cov kern x X
	let m   = map (mean) X |> map2 (sub) Y
	in matmul K_s C_inv |> flip matvecmul_row m |> map2 (add) u_s

-- K(x, x) - K(x, X) * inv(K(X, X)) * K(X, x)
def func_K_D 'a [z] [l] kern matsub matmul C_inv (X: [z]a) (x: [l]a) =
	let K_ss  = cov kern x x
	let K_s_l = cov kern x X
	let K_s_r = transpose K_s_l
	in matmul K_s_l C_inv |> flip (matmul) K_s_r |> matsub K_ss

module gp_linalg(R: real) = {
	module L = mk_ordered_linalg R

	def compute_C_inv kern X =
		cov kern X X |> L.inv
		
	def u_D mean kern C_inv X Y x =
		func_u_D mean kern (R.+) (R.-) L.matmul L.matvecmul_row C_inv X Y x

	def K_D kern C_inv X x =
		func_K_D kern L.matsub L.matmul C_inv X x
}
