module annotations_d3d11on12

open annotations
open custom_impls

let d3d11on12=[
  ("ID3D11On12DeviceVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("CreateWrappedResource",[
      ("This",AThis);
      ("pResource12",InComPtr);
      ("pFlags11",ANone);
      ("InState",ANone);
      ("OutState",ANone);
      ("riid",ANone);
      ("ppResource11",OutReturnInterface "riid");
    ],MANone);
    ("ReleaseWrappedResources",[
      ("This",AThis);
      ("ppResources",InComPtrArrayOfSize "NumResources");
      ("NumResources",ANone);
    ],MANone);
    ("AcquireWrappedResources",[
      ("This",AThis);
      ("ppResources",InComPtrArrayOfSize "NumResources");
      ("NumResources",ANone);
    ],MANone);
  ]);
  ]
