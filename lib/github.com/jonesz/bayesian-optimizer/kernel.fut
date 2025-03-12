-- | Kernel functions.
-- Referenced from: https://www.cs.toronto.edu/~duvenaud/thesis.pdf

module kSE_ARD(R: real) = {
  type t = R.t

  -- | The kSE_ARD kernel.
  def kernel [d] (lengthscale: [d]t) (sigma_sqr: t) (x_0: [d]t) (x_1: [d]t) : t =
    let f l_d x_0_d x_1_d =
      (R.-) x_0_d x_1_d
      |> flip (R.**) (R.i32 2)
      |> flip (R./) ((R.**) l_d (R.i32 2))         -- (x_0_d - x_1_d)^2 / (l_d)^2
   
    in map3 (f) lengthscale x_0 x_1                -- (2.4)
      |> reduce (R.+) (R.i32 0)
      |> flip (R./) (R.i32 2i32 |> R.neg)
      |> R.exp
      |> (R.*) sigma_sqr
}

-- `binOp k_a(x_0, x_1) k_b(x_0, x_1)`
def kBinOp binOp ka kb x_0 x_1 = binOp (ka x_0 x_1) (kb x_0 x_1)
