def cov kern x_0 x_1 =
	map (\i -> map (\j -> kern i j) x_0) x_1

-- u_s + K_s * K^{-1} * (y - u)
def u_D mean kern add sub mul X Y x =
	let C   = cov kern X X
	let K_s = cov kern X x         
	let u_s = map (mean) x
	let u   = map (mean) X |> map2 (sub) Y
	in mul C u |> mul K_s |> add u_s

-- K_ss - K_s * K^{-1} * K_s
def K_D kern sub mul X x =
	let K_ss = cov kern x x
	let K_s  = cov kern X x
	let C    = cov kern X X
	in mul C K_s |> mul K_s |> sub K_ss
