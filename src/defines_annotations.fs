module defines_annotations 
open annotations

let d3d12Encode4CM = "\
#[inline]
pub fn D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING(Src0: UINT, Src1: UINT, Src2: UINT, Src3: UINT) -> UINT {
    (Src0 & D3D12_SHADER_COMPONENT_MAPPING_MASK) |
    ((Src1 & D3D12_SHADER_COMPONENT_MAPPING_MASK)<< D3D12_SHADER_COMPONENT_MAPPING_SHIFT)    |
    ((Src2 & D3D12_SHADER_COMPONENT_MAPPING_MASK)<<(D3D12_SHADER_COMPONENT_MAPPING_SHIFT*2)) |
    ((Src3 & D3D12_SHADER_COMPONENT_MAPPING_MASK)<<(D3D12_SHADER_COMPONENT_MAPPING_SHIFT*3)) | 
    D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES
}"

let d3d12Decode4CM = "\
#[inline]
pub fn D3D12_DECODE_SHADER_4_COMPONENT_MAPPING(ComponentToExtract: UINT, Mapping: UINT) -> UINT {
    (Mapping >> (D3D12_SHADER_COMPONENT_MAPPING_SHIFT*ComponentToExtract)) & D3D12_SHADER_COMPONENT_MAPPING_MASK
}"

let makeConst def ty vl =
    (def, UseCustom(ty, "pub const "+def+": "+ty+" = "+vl+";"))

let d3d12_defines = [
        makeConst "D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES" "UINT" "1<<(D3D12_SHADER_COMPONENT_MAPPING_SHIFT*4)";
        ("D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING", UseCustom("UINT", d3d12Encode4CM));
        makeConst "D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING" "UINT" "0x00001688";
        ("D3D12_DECODE_SHADER_4_COMPONENT_MAPPING", UseCustom("UINT", d3d12Decode4CM));
    ]

let defines: Map<string, Map<string, DefineAnnotation>> =
  [
    ("d3d12", d3d12_defines |> Map.ofList );
    ("d3d11on12", [] |> Map.ofList );
    ("d3d12sdklayers", [] |> Map.ofList );
    ("d3dcommon", [] |> Map.ofList );
    ("dxgi", [] |> Map.ofList );
    ("dxgiformat", [] |> Map.ofList );
    ("dxgitype", [] |> Map.ofList );
    ("dxgi1_2", [] |> Map.ofList );
    ("dxgi1_3", [] |> Map.ofList );
    ("dxgi1_4", [] |> Map.ofList );
    ("dwrite", [] |> Map.ofList );
    ("d2d1", [] |> Map.ofList );
    ("d2d1_1", [] |> Map.ofList );
    ("d2d1_2", [] |> Map.ofList );
    ("d2d1_3", [] |> Map.ofList );
    ("d3d12shader", [] |> Map.ofList );
    ("wincodec", [] |> Map.ofList );
    ("objidl", [] |> Map.ofList );
    ("objidlbase", [] |> Map.ofList );
    ("ocidl", [] |> Map.ofList );
    ("urlmon", [] |> Map.ofList );
    ("DocumentTarget", [] |> Map.ofList );
    ("d2d1effectauthor", [] |> Map.ofList );
    ("d3d11", [] |> Map.ofList );
  ] |> Map.ofList
