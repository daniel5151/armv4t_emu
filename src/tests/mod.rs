use std::sync::{Once, ONCE_INIT};

pub mod ram;

static INIT: Once = ONCE_INIT;

pub fn setup() {
    INIT.call_once(|| {
        env_logger::init();
    });
}
