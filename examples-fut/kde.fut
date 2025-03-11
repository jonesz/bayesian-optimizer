def kernel_epanechnikov u =
	(f32.**) u 2f32 |> (f32.-) 1f32 |> (f32.*) 0.75f32

def kde [n] kern h (xs: [n]f32) x =
	let f x_i = x - x_i |> flip (/) h |> kern
	in map (f) xs |> reduce (+) 0f32 |> flip (/) (f32.i64 n |> (*) h)
