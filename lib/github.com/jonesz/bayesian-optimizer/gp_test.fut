import "gp"
import "kernel"
import "../../diku-dk/linalg/linalg"

module L = mk_ordered_linalg f32
module GP = gp_linalg f32
let kern x_0 x_1 = kSE_f32 1.0f32 0.5f32 x_0 x_1

-- Each independent points' variance should be equal to the respective
-- entry of the multiple points' diagonal.
-- ==
-- entry: gp_single_multiple_equivalence
-- random input { [10][1]f32 [5][1]f32 }
-- output { true }
entry gp_single_multiple_equivalence [a] [b] (X: [a][1]f32) (x: [b][1]f32) =
	let C_inv = GP.compute_C_inv kern X
	let m_Var = GP.K_D kern C_inv X x |> L.fromdiag

	let s_Var = map (\x_i -> GP.K_D kern C_inv X [x_i]) x
		|> transpose |> flatten |> transpose |> flatten -- [5][1][1]f32 to [5]f32

	let s_Var = s_Var :> [b]f32
	in zip m_Var s_Var |> all (\(a, b) -> a == b)
