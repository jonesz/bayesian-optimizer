-- | Kernel functions; computes "similarity" between two values `x_0`, `x_1`.
-- https://www.cs.toronto.edu/~duvenaud/thesis.pdf

-- The L2 loss.
def l2 sub sqr add ne sqrt x_0 x_1 =
  map2 sub x_0 x_1 |> map sqr |> reduce add ne |> sqrt

-- An isotropic squared exponential kernel.
def kSE sub sqr add ne sqrt mul div neg_two exp lengthscale sigma x_0 x_1 =
  let top = l2 sub sqr add ne sqrt x_0 x_1
  let bot = sqr lengthscale |> mul neg_two
  in div top bot |> exp |> mul (sqr sigma)

def kSE_f32 lengthscale sigma x_0 x_1 =
  kSE (f32.-) (flip (f32.**) 2f32) (f32.+) 0f32 (f32.sqrt) (f32.*) (f32./) (2f32 |> f32.neg) (f32.exp) lengthscale sigma x_0 x_1

def kAdd_f32 k0 k1 x_0 x_1 = (f32.+) (k0 x_0 x_1) (k1 x_0 x_1)
def kMul_f32 k0 k1 x_0 x_1 = (f32.*) (k0 x_0 x_1) (k1 x_0 x_1)

module kSE_mod(R: real) = {
  type t = R.t

  def kernel lengthscale sigma x_0 x_1 =
    let top = map2 (R.-) x_0 x_1 |> map (flip (R.**) (R.i64 2)) |> reduce (R.+) (R.i64 0) |> R.sqrt
    let bot = (R.**) lengthscale (R.i64 2) |> (R.*) (R.i64 2 |> R.neg)
    in (R./) top bot |> R.exp |> (R.*) ((R.**) sigma (R.i64 2))
}

-- ==
-- entry: test_func_mod_equivalence
-- random input { [1000]f32 [1000]f32 [1000][10]f32 [1000][10]f32 }
-- output { true }
module M = kSE_mod f32
entry test_func_mod_equivalence l s x_0 x_1 =
  let z_0 = map4 (kSE_f32)  l s x_0 x_1
  let z_1 = map4 (M.kernel) l s x_0 x_1
  in map2 (==) z_0 z_1 |> and

-- ==
-- entry: test_func_kAdd
-- random input { [1000][10]f32 [1000][10]f32 }
entry test_func_kAdd x_0 x_1 =
  let k = kAdd_f32 (kSE_f32 1.0f32 0.5f32) (kSE_f32 0.33f32 0.5f32)
  in map2 (k) x_0 x_1

-- ==
-- entry: test_func_kMul
-- random input { [1000][10]f32 [1000][10]f32 }
entry test_func_kMul x_0 x_1 =
  let k = kMul_f32 (kSE_f32 1.0f32 0.5f32) (kSE_f32 0.33f32 0.5f32)
  in map2 (k) x_0 x_1
