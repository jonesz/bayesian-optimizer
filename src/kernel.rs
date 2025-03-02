use std::ops::{Add, Mul, Sub};

mod loss {
    use super::*;
    pub fn l2<T, const N: usize, U: Fn(T) -> T>(x_0: &[T; N], x_1: &[T; N], sqrt: U) -> T
    where
        for<'a> &'a T: Sub<Output = T>,
        T: Add<Output = T> + Mul<Output = T>,
    {
        sqrt(
            x_0.iter()
                .zip(x_1)
                .map(|(a, b)| (a - b) * (a - b))
                .reduce(|a, b| a + b)
                .unwrap(),
        )
    }
}

pub trait Kernel<T> {
    fn kernel<const N: usize>(&self, x_0: &[T; N], x_1: &[T; N]) -> T;
}

/// An isotropic Squared Exponential kernel.
pub struct KernelSE<T> {
    l: T,
    s: T,
}

impl Kernel<f32> for KernelSE<f32> {
    fn kernel<const N: usize>(&self, x_0: &[f32; N], x_1: &[f32; N]) -> f32 {
        let top = loss::l2(x_0, x_1, f32::sqrt);
        // TODO: We could store `l_sqr` and `s_sqr`; `bot` is effectively constant...
        // Wonder what the compiler will give me here?
        let bot = self.l * self.l * -2f32;
        self.s * self.s * f32::exp(top / bot)
    }
}
