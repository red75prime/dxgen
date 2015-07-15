﻿module annotations_autogen
open annotations

let getPrivateData= // Low level method
  ("GetPrivateData",[
      ("This",AThis);
      ("guid",ANone);
      ("pDataSize",InOutReturn);
      ("pData",OutOfSize "pDataSize");
    ],MAUnsafe) 

let setPrivateData=
  ("SetPrivateData",[
      ("This",AThis);
      ("guid",ANone);
      ("DataSize",ANone);
      ("pData", InOfSize "DataSize");
    ],MANone)

let setPrivateDataInterface=
  ("SetPrivateDataInterface",[
      ("This",AThis);
      ("guid",ANone);
      ("pData",InIUnknown);
    ],MANone)

let setName=
  ("SetName",[
      ("This",AThis);
      ("Name",ANone);
    ],MANone)

let getDevice=
  ("GetDevice",[
    ("This",AThis);
    ("riid",ANone);
    ("ppvDevice",OutReturnInterface "riid");
  ],MANone)

let d3d12annotations=[
  ("ID3D10BlobVtbl",IAManual,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("GetBufferPointer",[
      ("This",AThis);
    ],MANone);
    ("GetBufferSize",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12CommandAllocatorVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("Reset",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12CommandListVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetType",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12CommandQueueVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("UpdateTileMappings",[
      ("This",AThis);
      ("pResource",ANone);
      ("NumResourceRegions",ANone);
      ("pResourceRegionStartCoordinates",InOptionalArrayOfSize "NumResourceRegions");
      ("pResourceRegionSizes",InOptionalArrayOfSize "NumResourceRegions");
      ("pHeap",InOptional);
      ("NumRanges",ANone);
      ("pRangeFlags",InOptionalArrayOfSize "NumRanges");
      ("pHeapRangeStartOffsets",InOptionalArrayOfSize "NumRanges");
      ("pRangeTileCounts",InOptionalArrayOfSize "NumRanges");
      ("Flags",ANone);
    ],MANone);
    ("CopyTileMappings",[
      ("This",AThis);
      ("pDstResource",ANone);
      ("pDstRegionStartCoordinate",ANone);
      ("pSrcResource",ANone);
      ("pSrcRegionStartCoordinate",ANone);
      ("pRegionSize",ANone);
      ("Flags",ANone);
    ],MANone);
    ("ExecuteCommandLists",[
      ("This",AThis);
      ("NumCommandLists",ANone);
      ("ppCommandLists",InArrayOfSize "NumCommandLists");
    ],MANone);
    ("SetMarker",[
      ("This",AThis);
      ("Metadata",ANone);
      ("pData",ANone);
      ("Size",ANone);
    ],MADontImplement);
    ("BeginEvent",[
      ("This",AThis);
      ("Metadata",ANone);
      ("pData",ANone);
      ("Size",ANone);
    ],MADontImplement);
    ("EndEvent",[
      ("This",AThis);
    ],MADontImplement);
    ("Signal",[
      ("This",AThis);
      ("pFence",ANone);
      ("Value",ANone);
    ],MANone);
    ("Wait",[
      ("This",AThis);
      ("pFence",ANone);
      ("Value",ANone);
    ],MANone);
    ("GetTimestampFrequency",[
      ("This",AThis);
      ("pFrequency",OutReturn);
    ],MANone);
    ("GetClockCalibration",[
      ("This",AThis);
      ("pGpuTimestamp",OutReturnCombine("GPUCPUTimestamp","gpu_timestamp"));
      ("pCpuTimestamp",OutReturnCombine("GPUCPUTimestamp","cpu_timestamp"));
    ],MANone);
    ("GetDesc",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12CommandSignatureVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
  ]);
  ("ID3D12DebugCommandListVtbl",IAManual,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("AssertResourceState",[
      ("This",AThis);
      ("pResource",ANone);
      ("Subresource",ANone);
      ("State",ANone);
    ],MANone);
    ("SetFeatureMask",[
      ("This",AThis);
      ("Mask",ANone);
    ],MANone);
    ("GetFeatureMask",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12DebugCommandQueueVtbl",IAManual,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("AssertResourceState",[
      ("This",AThis);
      ("pResource",ANone);
      ("Subresource",ANone);
      ("State",ANone);
    ],MANone);
  ]);
  ("ID3D12DebugDeviceVtbl",IAManual,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("SetFeatureMask",[
      ("This",AThis);
      ("Mask",ANone);
    ],MANone);
    ("GetFeatureMask",[
      ("This",AThis);
    ],MANone);
    ("ReportLiveDeviceObjects",[
      ("This",AThis);
      ("Flags",ANone);
    ],MANone);
  ]);
  ("ID3D12DebugVtbl",IAManual,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("EnableDebugLayer",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12DescriptorHeapVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetDesc",[
      ("This",AThis);
    ],MANone);
    ("GetCPUDescriptorHandleForHeapStart",[
      ("This",AThis);
      ("__ret_val",OutReturn);
    ],MANone);
    ("GetGPUDescriptorHandleForHeapStart",[
      ("This",AThis);
      ("__ret_val",OutReturn);
    ],MANone);
  ]);
  ("ID3D12DeviceChildVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
  ]);
  ("ID3D12DeviceVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    ("GetNodeCount",[
      ("This",AThis);
    ],MANone);
    ("CreateCommandQueue",[
      ("This",AThis);
      ("pDesc",ANone);
      ("riid",ANone);
      ("ppCommandQueue",OutReturnKnownInterface("riid","D3D12CommandQueue"));
    ],MANone);
    ("CreateCommandAllocator",[
      ("This",AThis);
      ("type",ANone);
      ("riid",ANone);
      ("ppCommandAllocator",OutReturnKnownInterface("riid","D3D12CommandAllocator"));
    ],MANone);
    ("CreateGraphicsPipelineState",[
      ("This",AThis);
      ("pDesc",ANone);
      ("riid",ANone);
      ("ppPipelineState",OutReturnKnownInterface("riid","D3D12PipelineState"));
    ],MANone);
    ("CreateComputePipelineState",[
      ("This",AThis);
      ("pDesc",ANone);
      ("riid",ANone);
      ("ppPipelineState",OutReturnKnownInterface("riid","D3D12PipelineState"));
    ],MANone);
    ("CreateCommandList",[
      ("This",AThis);
      ("nodeMask",ANone);
      ("type",ANone);
      ("pCommandAllocator",ANone);
      ("pInitialState",InOptional);
      ("riid",ANone);
      ("ppCommandList",OutReturnInterface "riid");
    ],MANone);
    ("CheckFeatureSupport",[
      ("This",AThis);
      ("Feature",TypeSelector("pFeatureSupportData",[("Options","D3D12_FEATURE_D3D12_OPTIONS","D3D12_FEATURE_DATA_D3D12_OPTIONS");
                                                     ("Arch","D3D12_FEATURE_ARCHITECTURE","D3D12_FEATURE_DATA_ARCHITECTURE");
                                                     ("FeatureLevels","D3D12_FEATURE_FEATURE_LEVELS","D3D12_FEATURE_DATA_FEATURE_LEVELS");
                                                     ("FormatSupport","D3D12_FEATURE_FORMAT_SUPPORT","D3D12_FEATURE_DATA_FORMAT_SUPPORT");
                                                     ("MultisampleQualityLevels","D3D12_FEATURE_MULTISAMPLE_QUALITY_LEVELS","D3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS");
                                                     ("FormatInfo","D3D12_FEATURE_FORMAT_INFO","D3D12_FEATURE_DATA_FORMAT_INFO");
                                                     ]));
      ("pFeatureSupportData",InOutOfSize "FeatureSupportDataSize");
      ("FeatureSupportDataSize",ANone);
    ],MANone);
    ("CreateDescriptorHeap",[
      ("This",AThis);
      ("pDescriptorHeapDesc",ANone);
      ("riid",ANone);
      ("ppvHeap",OutReturnKnownInterface("riid","D3D12DescriptorHeap"));
    ],MANone);
    ("GetDescriptorHandleIncrementSize",[
      ("This",AThis);
      ("DescriptorHeapType",ANone);
    ],MANone);
    ("CreateRootSignature",[
      ("This",AThis);
      ("nodeMask",ANone);
      ("pBlobWithRootSignature",InByteArrayOfSize "blobLengthInBytes");
      ("blobLengthInBytes",ANone);
      ("riid",ANone);
      ("ppvRootSignature",OutReturnKnownInterface("riid","D3D12RootSignature"));
    ],MANone);
    ("CreateConstantBufferView",[
      ("This",AThis);
      ("pDesc",InOptional);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CreateShaderResourceView",[
      ("This",AThis);
      ("pResource",InOptional);
      ("pDesc",InOptional);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CreateUnorderedAccessView",[
      ("This",AThis);
      ("pResource",InOptional);
      ("pCounterResource",InOptional);
      ("pDesc",InOptional);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CreateRenderTargetView",[
      ("This",AThis);
      ("pResource",InOptional);
      ("pDesc",InOptional);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CreateDepthStencilView",[
      ("This",AThis);
      ("pResource",InOptional);
      ("pDesc",InOptional);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CreateSampler",[
      ("This",AThis);
      ("pDesc",ANone);
      ("DestDescriptor",ANone);
    ],MANone);
    ("CopyDescriptors",[
      ("This",AThis);
      ("NumDestDescriptorRanges",ANone);
      ("pDestDescriptorRangeStarts",InArrayOfSize "NumDestDescriptorRanges");
      ("pDestDescriptorRangeSizes",InOptionalArrayOfSize "NumDestDescriptorRanges");
      ("NumSrcDescriptorRanges",ANone);
      ("pSrcDescriptorRangeStarts",InArrayOfSize "NumSrcDescriptorRanges");
      ("pSrcDescriptorRangeSizes",InOptionalArrayOfSize "NumSrcDescriptorRanges");
      ("DescriptorHeapsType",ANone);
    ],MANone);
    ("CopyDescriptorsSimple",[
      ("This",AThis);
      ("NumDescriptors",ANone);
      ("DestDescriptorRangeStart",ANone);
      ("SrcDescriptorRangeStart",ANone);
      ("DescriptorHeapsType",ANone);
    ],MANone);
    ("GetResourceAllocationInfo",[
      ("This",AThis);
      ("visibleMask",ANone);
      ("numResourceDescs",ANone);
      ("pResourceDescs",InArrayOfSize "numResourceDescs");
    ],MANone);
    ("GetCustomHeapProperties",[
      ("This",AThis);
      ("nodeMask",ANone);
      ("heapType",ANone);
    ],MANone);

//// ------------------------ Continue here -----------------------------------------------------
    ("CreateCommittedResource",[
      ("This",AThis);
      ("pHeapProperties",ANone);
      ("HeapFlags",ANone);
      ("pResourceDesc",ANone);
      ("InitialResourceState",ANone);
      ("pOptimizedClearValue",ANone);
      ("riidResource",ANone);
      ("ppvResource",OutReturnKnownInterface("riidResource","D3D12Resource"));
    ],MANone);
    ("CreateHeap",[
      ("This",AThis);
      ("pDesc",ANone);
      ("riid",ANone);
      ("ppvHeap",OutReturnKnownInterface("riid","D3D12Heap"));
    ],MANone);
    ("CreatePlacedResource",[
      ("This",AThis);
      ("pHeap",ANone);
      ("HeapOffset",ANone);
      ("pDesc",ANone);
      ("InitialState",ANone);
      ("pOptimizedClearValue",ANone);
      ("riid",ANone);
      ("ppvResource",OutReturnKnownInterface("riid","D3D12Resource"));
    ],MANone);
    ("CreateReservedResource",[
      ("This",AThis);
      ("pDesc",ANone);
      ("InitialState",ANone);
      ("pOptimizedClearValue",ANone);
      ("riid",ANone);
      ("ppvResource",OutReturnKnownInterface("riid","D3D12Resource"));
    ],MANone);
    ("CreateSharedHandle",[
      ("This",AThis);
      ("pObject",ANone);
      ("pAttributes",ANone);
      ("Access",ANone);
      ("Name",ANone);
      ("pHandle",ANone);
    ],MANone);
    ("OpenSharedHandle",[
      ("This",AThis);
      ("NTHandle",ANone);
      ("riid",ANone);
      ("ppvObj",OutReturnInterface("riid"));
    ],MANone);
    ("OpenSharedHandleByName",[
      ("This",AThis);
      ("Name",ANone);
      ("Access",ANone);
      ("pNTHandle",ANone);
    ],MANone);
    ("MakeResident",[
      ("This",AThis);
      ("NumObjects",ANone);
      ("ppObjects",ANone);
    ],MANone);
    ("Evict",[
      ("This",AThis);
      ("NumObjects",ANone);
      ("ppObjects",ANone);
    ],MANone);
    ("CreateFence",[
      ("This",AThis);
      ("InitialValue",ANone);
      ("Flags",ANone);
      ("riid",ANone);
      ("ppFence",OutReturnKnownInterface("riid","D3D12Fence"));
    ],MANone);
    ("GetDeviceRemovedReason",[
      ("This",AThis);
    ],MANone);
    ("GetCopyableFootprints",[
      ("This",AThis);
      ("pResourceDesc",ANone);
      ("FirstSubresource",ANone);
      ("NumSubresources",ANone);
      ("BaseOffset",ANone);
      ("pLayouts",ANone);
      ("pNumRows",ANone);
      ("pRowSizeInBytes",ANone);
      ("pTotalBytes",ANone);
    ],MANone);
    ("CreateQueryHeap",[
      ("This",AThis);
      ("pDesc",ANone);
      ("riid",ANone);
      ("ppvHeap",OutReturnKnownInterface("riid","D3D12Heap")); // maybe wrong. TODO: Check docs.
    ],MANone);
    ("SetStablePowerState",[
      ("This",AThis);
      ("Enable",ANone);
    ],MANone);
    ("CreateCommandSignature",[
      ("This",AThis);
      ("pDesc",ANone);
      ("pRootSignature",ANone);
      ("riid",ANone);
      ("ppvCommandSignature",OutReturnKnownInterface("riid","D3D12CommandSignature"));
    ],MANone);
    ("GetResourceTiling",[
      ("This",AThis);
      ("pTiledResource",ANone);
      ("pNumTilesForEntireResource",ANone);
      ("pPackedMipDesc",ANone);
      ("pStandardTileShapeForNonPackedMips",ANone);
      ("pNumSubresourceTilings",ANone);
      ("FirstSubresourceTilingToGet",ANone);
      ("pSubresourceTilingsForNonPackedMips",ANone);
    ],MANone);
    ("GetAdapterLuid",[
      ("This",AThis);
      ("__ret_val",OutReturn);
    ],MANone);
  ]);
  ("ID3D12FenceVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetCompletedValue",[
      ("This",AThis);
    ],MANone);
    ("SetEventOnCompletion",[
      ("This",AThis);
      ("Value",ANone);
      ("hEvent",ANone);
    ],MANone);
    ("Signal",[
      ("This",AThis);
      ("Value",ANone);
    ],MANone);
  ]);
  ("ID3D12GraphicsCommandListVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetType",[
      ("This",AThis);
    ],MANone);
    ("Close",[
      ("This",AThis);
    ],MANone);
    ("Reset",[
      ("This",AThis);
      ("pAllocator",ANone);
      ("pInitialState",ANone);
    ],MANone);
    ("ClearState",[
      ("This",AThis);
      ("pPipelineState",ANone);
    ],MANone);
    ("DrawInstanced",[
      ("This",AThis);
      ("VertexCountPerInstance",ANone);
      ("InstanceCount",ANone);
      ("StartVertexLocation",ANone);
      ("StartInstanceLocation",ANone);
    ],MANone);
    ("DrawIndexedInstanced",[
      ("This",AThis);
      ("IndexCountPerInstance",ANone);
      ("InstanceCount",ANone);
      ("StartIndexLocation",ANone);
      ("BaseVertexLocation",ANone);
      ("StartInstanceLocation",ANone);
    ],MANone);
    ("Dispatch",[
      ("This",AThis);
      ("ThreadGroupCountX",ANone);
      ("ThreadGroupCountY",ANone);
      ("ThreadGroupCountZ",ANone);
    ],MANone);
    ("CopyBufferRegion",[
      ("This",AThis);
      ("pDstBuffer",ANone);
      ("DstOffset",ANone);
      ("pSrcBuffer",ANone);
      ("SrcOffset",ANone);
      ("NumBytes",ANone);
    ],MANone);
    ("CopyTextureRegion",[
      ("This",AThis);
      ("pDst",ANone);
      ("DstX",ANone);
      ("DstY",ANone);
      ("DstZ",ANone);
      ("pSrc",ANone);
      ("pSrcBox",ANone);
    ],MANone);
    ("CopyResource",[
      ("This",AThis);
      ("pDstResource",ANone);
      ("pSrcResource",ANone);
    ],MANone);
    ("CopyTiles",[
      ("This",AThis);
      ("pTiledResource",ANone);
      ("pTileRegionStartCoordinate",ANone);
      ("pTileRegionSize",ANone);
      ("pBuffer",ANone);
      ("BufferStartOffsetInBytes",ANone);
      ("Flags",ANone);
    ],MANone);
    ("ResolveSubresource",[
      ("This",AThis);
      ("pDstResource",ANone);
      ("DstSubresource",ANone);
      ("pSrcResource",ANone);
      ("SrcSubresource",ANone);
      ("Format",ANone);
    ],MANone);
    ("IASetPrimitiveTopology",[
      ("This",AThis);
      ("PrimitiveTopology",ANone);
    ],MANone);
    ("RSSetViewports",[
      ("This",AThis);
      ("NumViewports",ANone);
      ("pViewports",ANone);
    ],MANone);
    ("RSSetScissorRects",[
      ("This",AThis);
      ("NumRects",ANone);
      ("pRects",ANone);
    ],MANone);
    ("OMSetBlendFactor",[
      ("This",AThis);
      ("BlendFactor",ANone);
    ],MANone);
    ("OMSetStencilRef",[
      ("This",AThis);
      ("StencilRef",ANone);
    ],MANone);
    ("SetPipelineState",[
      ("This",AThis);
      ("pPipelineState",ANone);
    ],MANone);
    ("ResourceBarrier",[
      ("This",AThis);
      ("NumBarriers",ANone);
      ("pBarriers",ANone);
    ],MANone);
    ("ExecuteBundle",[
      ("This",AThis);
      ("pCommandList",ANone);
    ],MANone);
    ("SetDescriptorHeaps",[
      ("This",AThis);
      ("NumDescriptorHeaps",ANone);
      ("ppDescriptorHeaps",ANone);
    ],MANone);
    ("SetComputeRootSignature",[
      ("This",AThis);
      ("pRootSignature",ANone);
    ],MANone);
    ("SetGraphicsRootSignature",[
      ("This",AThis);
      ("pRootSignature",ANone);
    ],MANone);
    ("SetComputeRootDescriptorTable",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BaseDescriptor",ANone);
    ],MANone);
    ("SetGraphicsRootDescriptorTable",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BaseDescriptor",ANone);
    ],MANone);
    ("SetComputeRoot32BitConstant",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("SrcData",ANone);
      ("DestOffsetIn32BitValues",ANone);
    ],MANone);
    ("SetGraphicsRoot32BitConstant",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("SrcData",ANone);
      ("DestOffsetIn32BitValues",ANone);
    ],MANone);
    ("SetComputeRoot32BitConstants",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("Num32BitValuesToSet",ANone);
      ("pSrcData",ANone);
      ("DestOffsetIn32BitValues",ANone);
    ],MANone);
    ("SetGraphicsRoot32BitConstants",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("Num32BitValuesToSet",ANone);
      ("pSrcData",ANone);
      ("DestOffsetIn32BitValues",ANone);
    ],MANone);
    ("SetComputeRootConstantBufferView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("SetGraphicsRootConstantBufferView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("SetComputeRootShaderResourceView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("SetGraphicsRootShaderResourceView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("SetComputeRootUnorderedAccessView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("SetGraphicsRootUnorderedAccessView",[
      ("This",AThis);
      ("RootParameterIndex",ANone);
      ("BufferLocation",ANone);
    ],MANone);
    ("IASetIndexBuffer",[
      ("This",AThis);
      ("pView",ANone);
    ],MANone);
    ("IASetVertexBuffers",[
      ("This",AThis);
      ("StartSlot",ANone);
      ("NumViews",ANone);
      ("pViews",ANone);
    ],MANone);
    ("SOSetTargets",[
      ("This",AThis);
      ("StartSlot",ANone);
      ("NumViews",ANone);
      ("pViews",ANone);
    ],MANone);
    ("OMSetRenderTargets",[
      ("This",AThis);
      ("NumRenderTargetDescriptors",ANone);
      ("pRenderTargetDescriptors",ANone);
      ("RTsSingleHandleToDescriptorRange",ANone);
      ("pDepthStencilDescriptor",ANone);
    ],MANone);
    ("ClearDepthStencilView",[
      ("This",AThis);
      ("DepthStencilView",ANone);
      ("ClearFlags",ANone);
      ("Depth",ANone);
      ("Stencil",ANone);
      ("NumRects",ANone);
      ("pRects",ANone);
    ],MANone);
    ("ClearRenderTargetView",[
      ("This",AThis);
      ("RenderTargetView",ANone);
      ("ColorRGBA",ANone);
      ("NumRects",ANone);
      ("pRects",ANone);
    ],MANone);
    ("ClearUnorderedAccessViewUint",[
      ("This",AThis);
      ("ViewGPUHandleInCurrentHeap",ANone);
      ("ViewCPUHandle",ANone);
      ("pResource",ANone);
      ("Values",ANone);
      ("NumRects",ANone);
      ("pRects",ANone);
    ],MANone);
    ("ClearUnorderedAccessViewFloat",[
      ("This",AThis);
      ("ViewGPUHandleInCurrentHeap",ANone);
      ("ViewCPUHandle",ANone);
      ("pResource",ANone);
      ("Values",ANone);
      ("NumRects",ANone);
      ("pRects",ANone);
    ],MANone);
    ("DiscardResource",[
      ("This",AThis);
      ("pResource",ANone);
      ("pRegion",ANone);
    ],MANone);
    ("BeginQuery",[
      ("This",AThis);
      ("pQueryHeap",ANone);
      ("Type",ANone);
      ("Index",ANone);
    ],MANone);
    ("EndQuery",[
      ("This",AThis);
      ("pQueryHeap",ANone);
      ("Type",ANone);
      ("Index",ANone);
    ],MANone);
    ("ResolveQueryData",[
      ("This",AThis);
      ("pQueryHeap",ANone);
      ("Type",ANone);
      ("StartIndex",ANone);
      ("NumQueries",ANone);
      ("pDestinationBuffer",ANone);
      ("AlignedDestinationBufferOffset",ANone);
    ],MANone);
    ("SetPredication",[
      ("This",AThis);
      ("pBuffer",ANone);
      ("AlignedBufferOffset",ANone);
      ("Operation",ANone);
    ],MANone);
    ("SetMarker",[
      ("This",AThis);
      ("Metadata",ANone);
      ("pData",ANone);
      ("Size",ANone);
    ],MANone);
    ("BeginEvent",[
      ("This",AThis);
      ("Metadata",ANone);
      ("pData",ANone);
      ("Size",ANone);
    ],MANone);
    ("EndEvent",[
      ("This",AThis);
    ],MANone);
    ("ExecuteIndirect",[
      ("This",AThis);
      ("pCommandSignature",ANone);
      ("MaxCommandCount",ANone);
      ("pArgumentBuffer",ANone);
      ("ArgumentBufferOffset",ANone);
      ("pCountBuffer",ANone);
      ("CountBufferOffset",ANone);
    ],MANone);
  ]);
  ("ID3D12HeapVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetDesc",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12InfoQueueVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("SetMessageCountLimit",[
      ("This",AThis);
      ("MessageCountLimit",ANone);
    ],MANone);
    ("ClearStoredMessages",[
      ("This",AThis);
    ],MANone);
    ("GetMessage",[
      ("This",AThis);
      ("MessageIndex",ANone);
      ("pMessage",ANone);
      ("pMessageByteLength",ANone);
    ],MANone);
    ("GetNumMessagesAllowedByStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("GetNumMessagesDeniedByStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("GetNumStoredMessages",[
      ("This",AThis);
    ],MANone);
    ("GetNumStoredMessagesAllowedByRetrievalFilter",[
      ("This",AThis);
    ],MANone);
    ("GetNumMessagesDiscardedByMessageCountLimit",[
      ("This",AThis);
    ],MANone);
    ("GetMessageCountLimit",[
      ("This",AThis);
    ],MANone);
    ("AddStorageFilterEntries",[
      ("This",AThis);
      ("pFilter",ANone);
    ],MANone);
    ("GetStorageFilter",[
      ("This",AThis);
      ("pFilter",ANone);
      ("pFilterByteLength",ANone);
    ],MANone);
    ("ClearStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("PushEmptyStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("PushCopyOfStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("PushStorageFilter",[
      ("This",AThis);
      ("pFilter",ANone);
    ],MANone);
    ("PopStorageFilter",[
      ("This",AThis);
    ],MANone);
    ("GetStorageFilterStackSize",[
      ("This",AThis);
    ],MANone);
    ("AddRetrievalFilterEntries",[
      ("This",AThis);
      ("pFilter",ANone);
    ],MANone);
    ("GetRetrievalFilter",[
      ("This",AThis);
      ("pFilter",ANone);
      ("pFilterByteLength",ANone);
    ],MANone);
    ("ClearRetrievalFilter",[
      ("This",AThis);
    ],MANone);
    ("PushEmptyRetrievalFilter",[
      ("This",AThis);
    ],MANone);
    ("PushCopyOfRetrievalFilter",[
      ("This",AThis);
    ],MANone);
    ("PushRetrievalFilter",[
      ("This",AThis);
      ("pFilter",ANone);
    ],MANone);
    ("PopRetrievalFilter",[
      ("This",AThis);
    ],MANone);
    ("GetRetrievalFilterStackSize",[
      ("This",AThis);
    ],MANone);
    ("AddMessage",[
      ("This",AThis);
      ("Category",ANone);
      ("Severity",ANone);
      ("ID",ANone);
      ("pDescription",ANone);
    ],MANone);
    ("AddApplicationMessage",[
      ("This",AThis);
      ("Severity",ANone);
      ("pDescription",ANone);
    ],MANone);
    ("SetBreakOnCategory",[
      ("This",AThis);
      ("Category",ANone);
      ("bEnable",ANone);
    ],MANone);
    ("SetBreakOnSeverity",[
      ("This",AThis);
      ("Severity",ANone);
      ("bEnable",ANone);
    ],MANone);
    ("SetBreakOnID",[
      ("This",AThis);
      ("ID",ANone);
      ("bEnable",ANone);
    ],MANone);
    ("GetBreakOnCategory",[
      ("This",AThis);
      ("Category",ANone);
    ],MANone);
    ("GetBreakOnSeverity",[
      ("This",AThis);
      ("Severity",ANone);
    ],MANone);
    ("GetBreakOnID",[
      ("This",AThis);
      ("ID",ANone);
    ],MANone);
    ("SetMuteDebugOutput",[
      ("This",AThis);
      ("bMute",ANone);
    ],MANone);
    ("GetMuteDebugOutput",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12ObjectVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
  ]);
  ("ID3D12PageableVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
  ]);
  ("ID3D12PipelineStateVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("GetCachedBlob",[
      ("This",AThis);
      ("ppBlob",ANone);
    ],MANone);
  ]);
  ("ID3D12QueryHeapVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
  ]);
  ("ID3D12ResourceVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
    ("Map",[
      ("This",AThis);
      ("Subresource",ANone);
      ("pReadRange",ANone);
      ("ppData",ANone);
    ],MANone);
    ("Unmap",[
      ("This",AThis);
      ("Subresource",ANone);
      ("pWrittenRange",ANone);
    ],MANone);
    ("GetDesc",[
      ("This",AThis);
      ("__ret_val",OutReturn);
    ],MANone);
    ("GetGPUVirtualAddress",[
      ("This",AThis);
    ],MANone);
    ("WriteToSubresource",[
      ("This",AThis);
      ("DstSubresource",ANone);
      ("pDstBox",ANone);
      ("pSrcData",ANone);
      ("SrcRowPitch",ANone);
      ("SrcDepthPitch",ANone);
    ],MANone);
    ("ReadFromSubresource",[
      ("This",AThis);
      ("pDstData",ANone);
      ("DstRowPitch",ANone);
      ("DstDepthPitch",ANone);
      ("SrcSubresource",ANone);
      ("pSrcBox",ANone);
    ],MANone);
    ("GetHeapProperties",[
      ("This",AThis);
      ("pHeapProperties",ANone);
      ("pHeapFlags",ANone);
    ],MANone);
  ]);
  ("ID3D12RootSignatureDeserializerVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    ("GetRootSignatureDesc",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID3D12RootSignatureVtbl",IAAutogen,[
    ("QueryInterface",[],MAIUnknown);
    ("AddRef",[],MAIUnknown);
    ("Release",[],MAIUnknown);
    getPrivateData;
    setPrivateData;
    setPrivateDataInterface;
    setName;
    getDevice;
  ]);
  ("ID3DIncludeVtbl",IAAutogen,[
    ("Open",[
      ("This",AThis);
      ("IncludeType",ANone);
      ("pFileName",ANone);
      ("pParentData",ANone);
      ("ppData",ANone);
      ("pBytes",ANone);
    ],MANone);
    ("Close",[
      ("This",AThis);
      ("pData",ANone);
    ],MANone);
  ]);
  ("IUnknown",IAManual,[
    ]);
  ("SECURITY_ATTRIBUTES",IAManual,[
    ]);
  ]
