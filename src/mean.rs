use faer::mat::MatRef;
use std::marker::PhantomData;

pub trait Mean<S> {
    fn u(&self, x_0: &MatRef<S>) -> S;
}

pub struct Constant<S>(S);

impl<S> Mean<S> for Constant<S>
where
    S: Copy,
{
    fn u(&self, _: &MatRef<S>) -> S {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
