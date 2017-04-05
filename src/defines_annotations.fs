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

let d3d12EncodeBasicFilter = "\
#[inline]
pub fn D3D12_ENCODE_BASIC_FILTER(
    min: D3D12_FILTER_TYPE, 
    mag: D3D12_FILTER_TYPE, 
    mip: D3D12_FILTER_TYPE,
    reduction: D3D12_FILTER_REDUCTION_TYPE,
    ) -> D3D12_FILTER {
     (((min as UINT & D3D12_FILTER_TYPE_MASK) << D3D12_MIN_FILTER_SHIFT) 
    | ((mag as UINT & D3D12_FILTER_TYPE_MASK) << D3D12_MAG_FILTER_SHIFT) 
    | ((mip as UINT & D3D12_FILTER_TYPE_MASK) << D3D12_MIP_FILTER_SHIFT) 
    | ((reduction as UINT & D3D12_FILTER_REDUCTION_TYPE_MASK) << D3D12_FILTER_REDUCTION_TYPE_SHIFT)) as D3D12_FILTER
}"

let d3d12EncodeAniso = "\
#[inline]
pub fn D3D12_ENCODE_ANISOTROPIC_FILTER(
    reduction: D3D12_FILTER_REDUCTION_TYPE,
    ) -> D3D12_FILTER {
     ( D3D12_ANISOTROPIC_FILTERING_BIT 
     | D3D12_ENCODE_BASIC_FILTER(D3D12_FILTER_TYPE_LINEAR, 
                                 D3D12_FILTER_TYPE_LINEAR, 
                                 D3D12_FILTER_TYPE_LINEAR, 
                                 reduction) as UINT) as D3D12_FILTER
}"

let d3d12DecodeMin = "\
#[inline]
pub fn D3D12_DECODE_MIN_FILTER(
    filter: D3D12_FILTER,
    ) -> D3D12_FILTER_TYPE {
    ((filter as UINT >> D3D12_MIN_FILTER_SHIFT) & D3D12_FILTER_TYPE_MASK) as D3D12_FILTER_TYPE
}"

let d3d12DecodeMag = "\
#[inline]
pub fn D3D12_DECODE_MAG_FILTER(
    filter: D3D12_FILTER,
    ) -> D3D12_FILTER_TYPE {
    ((filter as UINT >> D3D12_MAG_FILTER_SHIFT) & D3D12_FILTER_TYPE_MASK) as D3D12_FILTER_TYPE
}"

let d3d12DecodeMip = "\
#[inline]
pub fn D3D12_DECODE_MIP_FILTER(
    filter: D3D12_FILTER,
    ) -> D3D12_FILTER_TYPE {
    ((filter as UINT >> D3D12_MIP_FILTER_SHIFT) & D3D12_FILTER_TYPE_MASK) as D3D12_FILTER_TYPE
}"

let d3d12DecodeReduction = "\
#[inline]
pub fn D3D12_DECODE_FILTER_REDUCTION(
    filter: D3D12_FILTER,
    ) -> D3D12_FILTER_REDUCTION_TYPE {
    ((filter as UINT >> D3D12_FILTER_REDUCTION_TYPE_SHIFT) 
        & D3D12_FILTER_REDUCTION_TYPE_MASK) as D3D12_FILTER_REDUCTION_TYPE
}"

let d3d12DecodeIsComparison = "\
#[inline]
pub fn D3D12_DECODE_IS_COMPARISON_FILTER(
    filter: D3D12_FILTER,
    ) -> bool {
    D3D12_DECODE_FILTER_REDUCTION(filter) == D3D12_FILTER_REDUCTION_TYPE_COMPARISON
}"

let d3d12DecodeIsAnyso = "\
#[inline]
pub fn D3D12_DECODE_IS_ANISOTROPIC_FILTER(
    filter: D3D12_FILTER,
    ) -> bool {
    (filter as UINT & D3D12_ANISOTROPIC_FILTERING_BIT != 0) 
    && D3D12_DECODE_MIN_FILTER(filter) == D3D12_FILTER_TYPE_LINEAR
    && D3D12_DECODE_MAG_FILTER(filter) == D3D12_FILTER_TYPE_LINEAR
    && D3D12_DECODE_MIP_FILTER(filter) == D3D12_FILTER_TYPE_LINEAR
}"

// TODO: Implement macro. makeCustom "D3D12_DECODE_IS_ANISOTROPIC_FILTER" 0x960e2fe1 "type" "";
// D3D12_DECODE_IS_ANISOTROPIC_FILTER ( D3D12Filter ) ( ( ( D3D12Filter ) &
// D3D12_ANISOTROPIC_FILTERING_BIT ) && ( D3D12_FILTER_TYPE_LINEAR ==
// D3D12_DECODE_MIN_FILTER ( D3D12Filter ) ) && ( D3D12_FILTER_TYPE_LINEAR ==
// D3D12_DECODE_MAG_FILTER ( D3D12Filter ) ) && ( D3D12_FILTER_TYPE_LINEAR ==
// D3D12_DECODE_MIP_FILTER ( D3D12Filter ) ) ) typedef

let makeConst def hash ty vl =
    ((def, hash), UseCustom(ty, "pub const "+def+": "+ty+" = "+vl+";"))

let makeCustom def hash ty vl =
    ((def, hash), UseCustom(ty, vl))

let d3d12_defines = [
        makeConst  "D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES" 0xf63eb4c8 "UINT" "1<<(D3D12_SHADER_COMPONENT_MAPPING_SHIFT*4)";
        makeCustom "D3D12_ENCODE_SHADER_4_COMPONENT_MAPPING" 0xc3cbded0 "UINT" d3d12Encode4CM;
        makeConst  "D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING" 0x894b055f "UINT" "0x00001688";
        makeCustom "D3D12_DECODE_SHADER_4_COMPONENT_MAPPING" 0x911e0205 "UINT" d3d12Decode4CM;
        makeCustom "D3D12_ENCODE_BASIC_FILTER" 0x12c1eed1 "" d3d12EncodeBasicFilter;
        makeCustom "D3D12_ENCODE_ANISOTROPIC_FILTER" 0xd74d5b3b "" d3d12EncodeAniso;
        makeCustom "D3D12_DECODE_MIN_FILTER" 0x49830f04 "" d3d12DecodeMin;
        makeCustom "D3D12_DECODE_MAG_FILTER" 0x2cd87f36 "" d3d12DecodeMag;
        makeCustom "D3D12_DECODE_MIP_FILTER" 0xd061e0a "" d3d12DecodeMip;
        makeCustom "D3D12_DECODE_FILTER_REDUCTION" 0xfbb2cf55 "" d3d12DecodeReduction;
        makeCustom "D3D12_DECODE_IS_COMPARISON_FILTER" 0xc4790483 "" d3d12DecodeIsComparison;
        makeCustom "D3D12_DECODE_IS_ANISOTROPIC_FILTER" 0x960e2fe1 "" d3d12DecodeIsAnyso;
    ]

let defines: Map<string, Map<string*int, DefineAnnotation>> =
  [
    ("d3d12", d3d12_defines |> Map.ofList );
    ("d3d11on12", [] |> Map.ofList );
    ("d3d12sdklayers", [] |> Map.ofList );
    ("d3dcommon", [] |> Map.ofList );
    ("dxgi", [] |> Map.ofList );
    ("dxgiformat", [(("DXGI_FORMAT_DEFINED", 0), DefineAnnotation.Exclude)] |> Map.ofList );
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
