import "../lib/github.com/diku-dk/cpprandom/random"

module norm_dist = normal_distribution f32 minstd_rand

def GMM_sample [n] rng (w: [n]f32) mu sigma =
	let rngs = minstd_rand.split_rng n rng 
	let (rngs, samples) =
		map3 (\rng_i mu_i sigma_i -> norm_dist.rand {mean=mu_i, stddev=sigma_i} rng_i) rngs mu sigma |> unzip
	let sample = map2 (*) w samples |> reduce (+) 0f32
	let rng = minstd_rand.join_rng rngs
	in (rng, sample)

def GMM_density w mu sigma x =
	let f w_i mu_i sigma_i = 
		x - mu_i |> flip (**) 2f32 |> flip (/) (2 * sigma_i) |> f32.neg
		|> f32.exp |> flip (/) (2f32 * f32.pi * sigma_i |> f32.sqrt) |> (*) w_i
	in map3 (f) w mu sigma |> reduce (+) 0f32
