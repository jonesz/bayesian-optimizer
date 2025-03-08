import "kernel"
import "mean"
import "gp"
import "policy"
import "lbfgs"

module GP = gp_linalg f32
let kern = kSE_f32 1.0f32 1.0f32
let mean = mk_mu_constant 0.0f32

let pd X Y x =
	let C_inv = GP.compute_C_inv kern X
	in (GP.u_D mean kern C_inv X Y x, GP.K_D kern C_inv X x)

let kappa = 1.0f32

let pol X Y x_next = ucb pd f32.sqrt (f32.*) (f32.+) kappa X Y x_next

-- TODO: At this point, we need a better avenue for BFGS.

let next D_x D_f =
	let f x_next = pol D_x D_f x_next
	in lbfgs f (last D_x)

-- ==
-- entry: test_opt
-- input { }
-- output { }
-- entry test_opt = ???
