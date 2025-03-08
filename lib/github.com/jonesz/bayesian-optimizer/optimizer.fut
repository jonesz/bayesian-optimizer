def opt k f p x_0 =
	loop (D_x, D_f) = ([x_0], [f x_0]) for _i < k do
		let x_next = p D_x D_f
		in (D_x ++ [x_next], D_f ++ [f x_next])
