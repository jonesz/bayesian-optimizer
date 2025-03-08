-- u(x) + lambda * sigma(x).
def ucb pd sqrt mul add kappa D_x D_f x_next =
	let (u, sigma) = pd D_x D_f x_next
	in sqrt sigma |> mul kappa |> add u
