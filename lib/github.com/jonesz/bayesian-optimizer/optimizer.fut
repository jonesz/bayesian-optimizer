def opt k obj next x_0 =
	loop (D_x, D_f) = ([x_0], [obj x_0]) for _i < k do
		let x_next = next D_x D_f
		in (D_x ++ [x_next], D_f ++ [obj x_next])

-- "Convex Optimization and Parallel Computing for Portfolio Optimization"
-- https://futhark-lang.org/student-projects/kasper-msc-thesis.pdf

def grad_vjp one f x = vjp f x one

-- ==
-- entry: test_grad_vjp
-- input  { [1f32, 2f32, 3f32] }
-- output { [2f32, 4f32, 6f32] }
entry test_grad_vjp x =
	let f x = reduce (+) 0.0f32 (map (**2.0) x)
	in grad_vjp 1f32 f x 
