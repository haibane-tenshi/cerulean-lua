pub trait Userdata {
    type RuntimeView;
    type Error;

    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: Self::RuntimeView,
    ) -> Option<Result<(), Self::Error>>;
}
