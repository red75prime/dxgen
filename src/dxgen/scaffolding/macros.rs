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
