import "gp"
import "../../diku-dk/linalg/linalg"

module L = mk_ordered_linalg f32
module GP = gp_linalg f32

-- TODO: Use an actual kernel.
let kern a b = (f32.*) a b

-- The diagonal of the covariance of multiple points should equal
-- the variance of a single point.
-- ==
-- entry: gp_single_multiple_equivalence
-- random input { [10]f32 [10]f32 [5]f32 }
-- output { true }
entry gp_single_multiple_equivalence [a] [b] (X: [a]f32) (Y: [a]f32) (x: [b]f32) =
	let C_inv = GP.compute_C_inv kern X
	let m_Var = GP.K_D kern C_inv X x
	let s_Var = map (\x_i -> GP.K_D kern C_inv X [x_i]) x
	in (length s_Var) == 5
