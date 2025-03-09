def opt k obj next x_0 =
	loop (D_x, D_f) = ([x_0], [obj x_0]) for _i < k do
		let x_next = next D_x D_f
		in (D_x ++ [x_next], D_f ++ [obj x_next])
