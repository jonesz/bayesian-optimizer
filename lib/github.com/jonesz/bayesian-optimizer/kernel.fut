module type kernel = {
	type v
	type s

	val kernel : v -> v -> s
}

module mk_kernel_se (R: real) (V: vector) : kernel = {
	def kernel x_0 x_1 = ???
}
