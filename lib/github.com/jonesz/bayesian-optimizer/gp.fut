import "../../diku-dk/linalg/linalg"

def before_deriv kern inv X =
	map (\i -> map (\j -> kern i j) X) X |> inv
	
-- u_s + K_s * K^{-1} * (y - u)
def u_D 'a [z] mean kern add sub mul_matrow dp C_inv (X: [z]a) Y (x: a) =
	let K_s = map (kern x) X
	let u_s = mean x
	let u   = map (mean) X |> map2 (sub) Y
	in mul_matrow C_inv u |> dp K_s |> add u_s

-- K_ss - K_s * K^{-1} * K_s
def K_D 'a [z] kern sub mul_matrow dp C_inv (X: [z]a) (x: a) =
	let K_ss = kern x x 
	let K_s  = map (kern x) X
	in mul_matrow C_inv K_s |> dp K_s |> sub K_ss

module gp_linalg(R: real) = {
	module L = mk_ordered_linalg R

	def compute_C_inv kern X =
		before_deriv kern L.inv X 
		
	def u_D mean kern C_inv X Y x =
		u_D mean kern (R.+) (R.-) L.matvecmul_row L.dotprod C_inv X Y x

	def K_D kern C_inv X x =
		K_D kern (R.-) L.matvecmul_row L.dotprod C_inv X x
}
