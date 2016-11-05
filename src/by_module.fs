module by_module

open annotations
open annotations_autogen

let annotations_by_module = 
  [
    ("d3d12",
      {interfaces = d3d12annotations;
      structs = d3d12structs;
      dependencies = [];
    });
    ("d3d11on12",
      {interfaces = d3d11on12;
      structs = Map.empty;
      dependencies = [];
    });
    ("d3d12sdklayers",
      {interfaces = d3d12sdklayers;
      structs = d3d12structs;
      dependencies = [];
    });
    ("d3dcommon",
      {interfaces = d3dcommon;
      structs = d3d12structs;
      dependencies = [];
    });
    ("dxgi",
      {interfaces = dxgi;
      structs = d3d12structs;
      dependencies = [];
    });
    ("dxgiformat",
      {interfaces = [];
      structs = Map.empty;
      dependencies = [];
    });
    ("dxgitype",
      {interfaces = [];
      structs = Map.empty;
      dependencies = [];
    });
    ("dxgi1_2",
      {interfaces = dxgi1_2;
      structs = d3d12structs;
      dependencies = ["dxgi"];
    });
    ("dxgi1_3",
      {interfaces = dxgi1_3;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2"];
    });
    ("dxgi1_4",
      {interfaces = dxgi1_4;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2";"dxgi1_3"];
    });
    ("dwrite",
      {interfaces = dwrite;
      structs = Map.empty;
      dependencies = [];
    });
    ("d2d1",
      {interfaces = d2d1;
      structs = Map.empty;
      dependencies = [];
    });
    ("d2d1_1",
      {interfaces = d2d1_1;
      structs = Map.empty;
      dependencies = ["d2d1"];
    });
    ("d2d1_2",
      {interfaces = annotations_d2d1.d2d1_2;
      structs = Map.empty;
      dependencies = [];
    });
    ("d2d1_3",
      {interfaces = annotations_d2d1.d2d1_3;
      structs = Map.empty;
      dependencies = [];
    });
    ("d3d12shader",
      {interfaces = d3d12shader;
      structs = Map.empty;
      dependencies = [];
    });
    ("wincodec",
      {interfaces = wincodec;
      structs = Map.empty;
      dependencies = [];
    });
    ("objidl",
      {interfaces = annotations_objidl.objidl;
      structs = Map.empty;
      dependencies = [];
    });
    ("objidlbase",
      {interfaces = annotations_objidlbase.objidlbase;
      structs = Map.empty;
      dependencies = [];
    });
    ("ocidl",
      {interfaces = annotations_ocidl.ocidl;
      structs = Map.empty;
      dependencies = [];
    });
    ("urlmon",
      {interfaces = annotations_urlmon.urlmon;
      structs = Map.empty;
      dependencies = [];
    });
    ("DocumentTarget",
      {interfaces = annotations_DocumentTarget.documentTarget;
      structs = Map.empty;
      dependencies = [];
    });
    ("d2d1effectauthor",
      {interfaces = a_d2d1effectauthor.annot;
      structs = Map.empty;
      dependencies = [];
    });
    ("d3d11",
      {interfaces = annotations_d3d11.d3d11;
      structs = Map.empty;
      dependencies = [];
    });
  ] |> Map.ofList
