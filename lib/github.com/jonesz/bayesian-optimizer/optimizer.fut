-- "Convex Optimization and Parallel Computing for Portfolio Optimization"
-- https://futhark-lang.org/student-projects/kasper-msc-thesis.pdf

def grad_vjp one f x = vjp f x one

-- TODO: Utilize an actual step rate, learning rate, etc.
def gd_f32 n f x_0 =
	let (x, _) = loop (x, n) = (x_0, n) for i < 100 do
		let G = grad_vjp 1f32 f x
		let x_next = map (f32.* n) G |> map2 (f32.-) x
		let n = n * 0.95f32
		in (x_next, n)
	in x

-- ==
-- entry: test_grad_vjp
-- input  { [1f32, 2f32, 3f32] }
-- output { [2f32, 4f32, 6f32] }
entry test_grad_vjp x =
	let f x = reduce (+) 0.0f32 (map (**2.0) x)
	in grad_vjp 1f32 f x 

-- ==
-- entry: test_gd_f32
-- input  { 1f32 }
-- output { [-1.0f32, -1.0f32, -1.0f32] }
entry test_gd_f32 n =
	let f x = reduce (+) 0.0f32 (map (\a -> a + 1f32 |> flip (**) 2.0f32) x)
	in gd_f32 n f [1f32, 200f32, -95f32]
