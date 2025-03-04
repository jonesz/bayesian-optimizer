import "kernel"

-- ==
-- entry: bench_kernel_fstyle
-- compiled random input { [1000]f32 [1000]f32 [1000][10]f32  [1000][10]f32 }
-- compiled random input { [1000]f32 [1000]f32 [1000][100]f32 [1000][100]f32 }
entry bench_kernel_fstyle l s x_0 x_1 =
	map4 (kSE_f32) l s x_0 x_1

-- ==
-- entry: bench_kernel_module
-- compiled random input { [1000]f32 [1000]f32 [1000][10]f32  [1000][10]f32 }
-- compiled random input { [1000]f32 [1000]f32 [1000][100]f32 [1000][100]f32 }
module M = kSE_mod f32
entry bench_kernel_module l s x_0 x_1 =
	map4 (M.kernel) l s x_0 x_1
