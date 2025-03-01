module type mean = {
	type v
	type s

	val u : v -> s
}

module mk_mean_constant (R: real) (P: {val constant : R.t}) : mean with s = R.t = {
	type s = R.t

	def u _ = P.constant
}
