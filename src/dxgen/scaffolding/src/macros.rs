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
    let offset = (&tmp.$field as *const _ as usize) - (&tmp as *const _ as usize);
    ::std::mem::forget(tmp);
    offset
  }}
}

// Based on gfx-rs gfx-vertex! https://github.com/gfx-rs/gfx/blob/master/src/core/src/macros/mod.rs
macro_rules! dx_vertex {
    ($name:ident {
        $(($semantics:ident, $sem_idx:expr, $format:ident) $field:ident: $ty:ty,)*
    }) => {
        use dxsems::VertexFormat;
        #[derive(Clone, Copy, Debug)]
        pub struct $name {
            $(pub $field: $ty,)*
        }
        impl VertexFormat for $name {
            fn generate(input_slot: u32) -> Vec<D3D12_INPUT_ELEMENT_DESC> {
                use dxsems::*;
                let mut e_descs = Vec::new();
                $(
                  e_descs.push(D3D12_INPUT_ELEMENT_DESC {
                    SemanticName: $semantics.as_ptr() as ::winapi::LPCSTR,
                    SemanticIndex: $sem_idx,
                    Format: $format, 
                    InputSlot: input_slot,
                    AlignedByteOffset: offset_of!($name, $field) as ::winapi::UINT, 
                    InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
                    InstanceDataStepRate: 0,
                  });
                )*
                e_descs
            }
        }
    }
}

macro_rules! alias {
    ($i:ident, $e:expr) => {
        macro_rules! $i (() => ($e));
    };
}
