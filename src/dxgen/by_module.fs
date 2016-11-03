module by_module

open annotations
open annotations_autogen

let annotations_by_module = 
  [
    ("d3d12",
      {interfaces = d3d12annotations;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
      defines = Map.empty;
    });
    ("d3d11on12",
      {interfaces = d3d11on12;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d3d12sdklayers",
      {interfaces = d3d12sdklayers;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
      defines = Map.empty;
    });
    ("d3dcommon",
      {interfaces = d3dcommon;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
      defines = Map.empty;
    });
    ("dxgi",
      {interfaces = dxgi;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
      defines = Map.empty;
    });
    ("dxgiformat",
      {interfaces = [];
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("dxgitype",
      {interfaces = [];
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("dxgi1_2",
      {interfaces = dxgi1_2;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi"];
      defines = Map.empty;
    });
    ("dxgi1_3",
      {interfaces = dxgi1_3;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2"];
      defines = Map.empty;
    });
    ("dxgi1_4",
      {interfaces = dxgi1_4;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2";"dxgi1_3"];
      defines = Map.empty;
    });
    ("dwrite",
      {interfaces = dwrite;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d2d1",
      {interfaces = d2d1;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d2d1_1",
      {interfaces = d2d1_1;
      enums = [("D2D1_PROPERTY", EAEnumHex); ("D2D1_SUBPROPERTY", EAEnumHex);] |> Map.ofList;
      structs = Map.empty;
      dependencies = ["d2d1"];
      defines = Map.empty;
    });
    ("d2d1_2",
      {interfaces = annotations_d2d1.d2d1_2;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d2d1_3",
      {interfaces = annotations_d2d1.d2d1_3;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d3d12shader",
      {interfaces = d3d12shader;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("wincodec",
      {interfaces = wincodec;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("objidl",
      {interfaces = annotations_objidl.objidl;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("objidlbase",
      {interfaces = annotations_objidlbase.objidlbase;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("ocidl",
      {interfaces = annotations_ocidl.ocidl;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("urlmon",
      {interfaces = annotations_urlmon.urlmon;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("DocumentTarget",
      {interfaces = annotations_DocumentTarget.documentTarget;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d2d1effectauthor",
      {interfaces = a_d2d1effectauthor.annot;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
    ("d3d11",
      {interfaces = annotations_d3d11.d3d11;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
      defines = Map.empty;
    });
  ] |> Map.ofList
