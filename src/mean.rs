use std::marker::PhantomData;

pub trait Mean<V, S> {
    fn u(&self, x_0: &V) -> S;
}

pub struct Constant<V, S>(S, PhantomData<V>);

impl<V, S> Mean<V, S> for Constant<V, S>
where
    S: Copy,
{
    fn u(&self, _: &V) -> S {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
