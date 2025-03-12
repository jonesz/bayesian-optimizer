import "kernel"
module M = kSE_ARD f32

-- ==
-- entry: bench_kernel_kSE_ARD
-- compiled random input { [1000][10]f32  [1000]f32 [1000][10]f32  [1000][10]f32  }
-- compiled random input { [1000][100]f32 [1000]f32 [1000][100]f32 [1000][100]f32 }
entry bench_kernel_kSE_ARD l s x_0 x_1 =
	map4 (M.kernel) l s x_0 x_1
