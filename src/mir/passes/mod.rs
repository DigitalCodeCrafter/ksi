mod copy_prop;
mod const_prop;
mod dead_local_elim;

pub use copy_prop::CopyPropagation;
pub use const_prop::ConstPropagation;
pub use dead_local_elim::DeadLocalElim;

pub trait Pass {
    fn name(&self) -> &'static str;
    fn run(&mut self, body: &mut super::Body);
}

pub fn run_passes(body: &mut super::Body, passes: &mut [Box<dyn Pass>]) {
    for pass in passes {
        super::verifier::verify_body(body).unwrap();

        pass.run(body);
    }

    super::verifier::verify_body(body).unwrap();
}
