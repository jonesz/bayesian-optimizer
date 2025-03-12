-- | Kernel functions.
-- Referenced from: https://www.cs.toronto.edu/~duvenaud/thesis.pdf

-- | The kSE_ARD kernel.
module kSE_ARD (R: real) = {
  type t = R.t

  -- | The kSE_ARD kernel.
  def kernel [d] (lengthscale: [d]t) (sigma_sqr: t) (x_0: [d]t) (x_1: [d]t) : t =
    let f l_d x_0_d x_1_d =                               -- (x_0_d - x_1_d)^2 / (l_d)^2
      (R.-) x_0_d x_1_d
      |> flip (R.**) (R.i32 2)
      |> flip (R./) ((R.**) l_d (R.i32 2))

    in map3 (f) lengthscale x_0 x_1                       -- (2.4)
       |> reduce (R.+) (R.i32 0)
       |> flip (R./) (R.i32 2i32 |> R.neg)
       |> R.exp
       |> (R.*) sigma_sqr
}

-- | The kPER_ARD kernel.
module kPER_ARD (R: real) = {
  type t = R.t

  -- | The kPER_ARD kernel.
  def kernel [d] (lengthscale: [d]t) (sigma_sqr: t) (p: [d]t) (x_0: [d]t) (x_1: [d]t) : t =
    let f l_d p_d x_0_d x_1_d =                           -- See "2.2 A few basic kernels."
      (R.-) x_0_d x_1_d
      |> (R.*) R.pi
      |> flip (R./) p_d
      |> R.sin
      |> flip (R.**) (R.i32 2)
      |> flip (R./) ((R.**) l_d (R.i32 2))

    in map4 (f) lengthscale p x_0 x_1
       |> reduce (R.+) (R.i32 0)
       |> (R.*) (R.i32 2 |> R.neg)
       |> R.exp
       |> (R.*) sigma_sqr
}

-- `binOp k_a(x_0, x_1) k_b(x_0, x_1)`
def kBinOp binOp ka kb x_0 x_1 = binOp (ka x_0 x_1) (kb x_0 x_1)
