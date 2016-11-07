module defines_annotations 
open annotations

let d3d12ScmAlwaysBit = "\
pub const D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES: UINT = 1<<(D3D12_SHADER_COMPONENT_MAPPING_SHIFT*4);"

let d3d12_defines = [
        ("D3D12_SHADER_COMPONENT_MAPPING_ALWAYS_SET_BIT_AVOIDING_ZEROMEM_MISTAKES",UseCustom("UINT", d3d12ScmAlwaysBit))
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
