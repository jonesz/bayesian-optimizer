import "../lib/github.com/diku-dk/cpprandom/random"
import "gmm"
import "kde"

-- NUMBER OF MIXTURES
let N = 10i64
let M = 1000i64

module dist = uniform_real_distribution f32 minstd_rand

let mu = 
	let rng = minstd_rand.rng_from_seed [123]
	let (_, mu) =
		let rngs = minstd_rand.split_rng N rng
		let (rngs, mu) = map (dist.rand (1, 6)) rngs |> unzip
		let rng = minstd_rand.join_rng rngs
		in (rng, mu)
	in mu

let sigma = 
	let rng = minstd_rand.rng_from_seed [1234]
	let (_, sigma) =
		let rngs = minstd_rand.split_rng N rng
		let (rngs, sigma) = map (dist.rand (1, 6)) rngs |> unzip
		let rng = minstd_rand.join_rng rngs
		in (rng, sigma)
	in sigma

let w = replicate N (1f32) |> map (flip (/) (f32.i64 N))

let xs =
	let rng = minstd_rand.rng_from_seed [12345]
	let rngs = minstd_rand.split_rng M rng
	let (_, X) = map (\rng_i -> GMM_sample rng_i w mu sigma) rngs |> unzip
	in X

entry main x =
	(kde kernel_epanechnikov 1.0f32 xs x, GMM_density w mu sigma x)
