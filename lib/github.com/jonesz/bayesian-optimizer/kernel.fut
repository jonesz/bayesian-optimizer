-- The L2 loss.
def l2 sub sqr add ne sqrt x_0 x_1 =
  map2 sub x_0 x_1 |> map sqr |> reduce add ne |> sqrt

-- An isotropic squared exponential kernel.
def kSE sub sqr add ne sqrt mul div neg_two exp lengthscale sigma x_0 x_1 =
  let top = l2 sub sqr add ne sqrt x_0 x_1
  let bot = sqr lengthscale |> mul neg_two
  in div top bot |> exp |> mul (sqr sigma)

def kOp op k1 k2 x_0 x_1 =
  op (k1 x_0 x_1) (k2 x_0 x_1)

def kSE_f32 lengthscale sigma x_0 x_1 =
  kSE (f32.-) (flip (f32.**) 2f32) (f32.+) 0f32 (f32.sqrt) (f32.*) (f32./) (2f32 |> f32.neg) (f32.exp) lengthscale sigma x_0 x_1

def kAdd_f32 k0 k1 x_0 x_1 = kOp (f32.+) (k0 x_0 x_1) (k1 x_0 x_1)
def kMul_f32 k0 k1 x_0 x_1 = kOp (f32.*) (k0 x_0 x_1) (k1 x_0 x_1)
