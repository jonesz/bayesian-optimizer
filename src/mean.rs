pub trait Mean<T> {
    fn u<const N: usize>(&self, x_0: &[T; N]) -> T;
}

pub struct Constant<T>(T);

impl<T> Mean<T> for Constant<T>
where
    T: Copy,
{
    fn u<const N: usize>(&self, _: &[T; N]) -> T {
        self.0
    }
}

#[cfg(test)]
mod tests {}
