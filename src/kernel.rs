use faer::mat::MatRef;
use std::marker::PhantomData;

pub trait Kernel<S> {
    fn kernel(&self, x_0: &MatRef<S>, x_1: &MatRef<S>) -> S;
}

/// An isotropic Squared Exponential kernel.
pub struct SquaredExponential<S> {
    lengthscale: S,
    amplitude: S,
}

impl<S> Kernel<S> for SquaredExponential<S> {
    fn kernel(&self, x_0: &MatRef<S>, x_1: &MatRef<S>) -> S {
        todo!();
    }
}
