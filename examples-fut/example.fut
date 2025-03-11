import "../lib/github.com/diku-dk/cpprandom/random"
import "gmm"
import "kde"

module dist = uniform_real_distribution f32 minstd_rand
let rng = minstd_rand.rng_from_seed [123]
