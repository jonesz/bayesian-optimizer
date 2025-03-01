use std::marker::PhantomData;

pub trait Kernel<V, S> {
    fn kernel(&self, x_0: &V, x_1: &V) -> S;
}

pub struct SquaredExponential<V, S>(S, PhantomData<V>);

impl<V, S> Kernel<V, S> for SquaredExponential<V, S> {
    fn kernel(&self, x_0: &V, x_1: &V) -> S {
        todo!();
    }
}
