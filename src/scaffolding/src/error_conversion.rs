use failure::{Fail, Error, Context, ResultExt};
use winapi::HRESULT;
use std::fmt::{self, Display, Debug, Formatter};
use utils;

pub struct HRFail(pub HRESULT);

impl Display for HRFail {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", utils::hr2msg(self.0))
    }
}

impl Debug for HRFail {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", utils::hr2msg(self.0))
    }
}

impl From<HRESULT> for HRFail {
    fn from(v: HRESULT) -> Self {
        HRFail(v)
    }
}

impl Fail for HRFail {}

pub trait ResultExtHr<T> {
    fn into_error(self) -> Result<T, Error>;
    fn into_error_with_context<F, D>(self, f: F) -> Result<T, Context<D>> where
        F: FnOnce(HRESULT) -> D,
        D: Display + Send + Sync + 'static;
    fn into_error_context<D>(self, d: D) -> Result<T, Context<D>> where
        D: Display + Send + Sync + 'static;
}

impl<T> ResultExtHr<T> for Result<T, HRESULT> {
    fn into_error(self) -> Result<T, Error> {
        self.map_err(|hr| HRFail(hr).into())
    }

    fn into_error_with_context<F, D>(self, f: F) -> Result<T, Context<D>> where
    F: FnOnce(HRESULT) -> D,
    D: Display + Send + Sync + 'static 
    {
        match self {
            Ok(v) => Ok(v),
            Err(hr) => {
                self.into_error().with_context(|_| f(hr))
            }
        }
    }

    fn into_error_context<D>(self, d: D) -> Result<T, Context<D>> where
    D: Display + Send + Sync + 'static
    {
        self.into_error().context(d)
    }
}

pub trait ErrorExt {
    fn downcast_ref_causes<T: Fail>(&self) -> Option<&T>;
}

impl ErrorExt for Error {
    fn downcast_ref_causes<T: Fail>(&self) -> Option<&T> {
        self.causes().filter_map(|c| c.downcast_ref()).next()
    }
}
