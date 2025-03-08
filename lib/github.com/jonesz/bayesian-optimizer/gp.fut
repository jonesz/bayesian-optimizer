import "../../diku-dk/linalg/linalg"

-- u_s + K_s * K^{-1} * (y - u)
def u_D 'a [z] mean kern add sub mul_matrow dp inv (X: [z]a) Y (x: a) =
	let C   = map (\i -> map (\j -> kern i j) X) X |> inv
	let K_s = map (kern x) X
	let u_s = mean x
	let u   = map (mean) X |> map2 (sub) Y
	in mul_matrow C u |> dp K_s |> add u_s

-- K_ss - K_s * K^{-1} * K_s
def K_D 'a [z] kern sub mul_matrow inv dp (X: [z]a) (x: a) =
	let K_ss = kern x x 
	let K_s  = map (kern x) X
	let C    = map (\i -> map (\j -> kern i j) X) X |> inv
	in mul_matrow C K_s |> dp K_s |> sub K_ss

module gp_linalg(R: real) = {
	module L = mk_ordered_linalg R

	def u_D mean kern X Y x =
		u_D mean kern (R.+) (R.-) L.matvecmul_row L.dotprod L.inv X Y x

	def K_D kern X x =
		K_D kern (R.-) L.matvecmul_row L.inv L.dotprod X x
}
