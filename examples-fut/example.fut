import "../lib/github.com/diku-dk/cpprandom/random"
import "gmm"
import "kde"
import "../lib/github.com/jonesz/bayesian-optimizer/kernel"
import "../lib/github.com/jonesz/bayesian-optimizer/gp"
import "../lib/github.com/jonesz/bayesian-optimizer/acquisition"
import "../lib/github.com/jonesz/bayesian-optimizer/optimizer"
import "../lib/github.com/jonesz/bayesian-optimizer/mean"

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

let obj h =
	let x = (iota 100) |> map (f32.i64) |> map (flip (/) 25f32)
	let kde_x = map (kde kernel_gaussian h xs) x
	let den_x = map (GMM_density w mu sigma) x
	in map2 (-) kde_x den_x |> map (flip (**) 2f32) |> reduce (+) 0f32


module K = kSE_ARD f32
module GP = gp_linalg f32
module U = UCB f32

let kern (x_0: [1]f32) (x_1: [1]f32) = K.kernel [1.0f32] 1.0f32 x_0 x_1
let mean _v = 0f32
let pd C_inv X Y x = GP.pd mean kern C_inv X Y x

let opt k x_0 =
	loop (D_x, D_f) = ([[x_0]], [obj x_0]) for _i < k do
		let C_inv = GP.compute_C_inv kern D_x
		let acq x_next =
			U.acq (GP.pd mean kern C_inv) 1.0f32 D_x D_f x_next

		let acq_opt x_next =
			acq x_next |> reduce (f32.+) 0f32 |> f32.neg

		let x_next = gd_f32 1.0f32 acq_opt (tail D_x)
		in (D_x ++ x_next, D_f ++ [map (obj) x_next])

entry main x = opt 1000 x
