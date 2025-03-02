-- l2 loss
def l2 sub sqr add ne sqrt x_0 x_1 =
	map2 sub x_0 x_1 |> map sqr |> reduce add ne |> sqrt
	
module type kernel = {
	type v
	type s

	val kernel : v -> v -> s
}
