#[macro_use]

macro_rules! write_delimited {
    ($writer:ident($($parms:tt)*), $list:expr, $delim:expr) => {{
        let mut first_item=true;
        for v in $list {
            if !first_item {
                try!{$writer!($($parms)*,$delim)};
            }
            try!{$writer!($($parms)*,v)};
            first_item=false;
        }
        Ok(())
    }}
}

macro_rules! offset_of {
  ($type_:ident, $field:ident) => {{
    let tmp: $type_ = unsafe{ ::std::mem::uninitialized::<_>()};
    (&tmp.$field as *const _ as usize) - (&tmp as *const _ as usize)
  }}
}
