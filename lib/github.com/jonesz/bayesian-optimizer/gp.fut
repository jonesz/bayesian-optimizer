import "../../diku-dk/linalg/linalg"

-- u_s + K_s * K^{-1} * (y - u)
def u_D 'a [z] mean kern add sub matmul dp inv (X: [z]a) Y (x: a) =
	let C   = map (\i -> map (\j -> kern i j) X) X |> inv
	let K_s = map (kern x) X
	let u_s = mean x
	let u   = map (mean) X |> map2 (sub) Y
	in matmul C u |> dp K_s |> add u_s

-- K_ss - K_s * K^{-1} * K_s
def K_D 'a [z] kern sub matmul inv dp (X: [z]a) (x: a) =
	let K_ss = kern x x 
	let K_s  = map (kern x) X
	let C    = map (\i -> map (\j -> kern i j) X) X |> inv
	in matmul C K_s |> dp K_s |> sub K_ss
