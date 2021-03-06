﻿module a_d2d1effectauthor

open annotations

let annot=[
  ("ID2D1AnalysisTransformVtbl",IAManual, "IUnknownVtbl", [
    ("ProcessAnalysisResults",[
      ("This",AThis);
      ("analysisData",ANone);
      ("analysisDataCount",ANone);
    ],MANone);
  ]);
  ("ID2D1BlendTransformVtbl",IAManual, "ID2D1ConcreteTransformVtbl", [
    ("SetDescription",[
      ("This",AThis);
      ("description",ANone);
    ],MANone);
    ("GetDescription",[
      ("This",AThis);
      ("description",ANone);
    ],MANone);
  ]);
  ("ID2D1BorderTransformVtbl",IAManual, "ID2D1ConcreteTransformVtbl", [
    ("SetExtendModeX",[
      ("This",AThis);
      ("extendMode",ANone);
    ],MANone);
    ("SetExtendModeY",[
      ("This",AThis);
      ("extendMode",ANone);
    ],MANone);
    ("GetExtendModeX",[
      ("This",AThis);
    ],MANone);
    ("GetExtendModeY",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID2D1BoundsAdjustmentTransformVtbl",IAManual, "ID2D1TransformNodeVtbl", [
    ("SetOutputBounds",[
      ("This",AThis);
      ("outputBounds",ANone);
    ],MANone);
    ("GetOutputBounds",[
      ("This",AThis);
      ("outputBounds",ANone);
    ],MANone);
  ]);
  ("ID2D1ComputeInfoVtbl",IAManual, "ID2D1RenderInfoVtbl", [
    ("SetComputeShaderConstantBuffer",[
      ("This",AThis);
      ("buffer",ANone);
      ("bufferCount",ANone);
    ],MANone);
    ("SetComputeShader",[
      ("This",AThis);
      ("shaderId",ANone);
    ],MANone);
    ("SetResourceTexture",[
      ("This",AThis);
      ("textureIndex",ANone);
      ("resourceTexture",ANone);
    ],MANone);
  ]);
  ("ID2D1ComputeTransformVtbl",IAManual, "ID2D1TransformVtbl", [
    ("SetComputeInfo",[
      ("This",AThis);
      ("computeInfo",ANone);
    ],MANone);
    ("CalculateThreadgroups",[
      ("This",AThis);
      ("outputRect",ANone);
      ("dimensionX",ANone);
      ("dimensionY",ANone);
      ("dimensionZ",ANone);
    ],MANone);
  ]);
  ("ID2D1ConcreteTransformVtbl",IAManual, "ID2D1TransformNodeVtbl", [
    ("SetOutputBuffer",[
      ("This",AThis);
      ("bufferPrecision",ANone);
      ("channelDepth",ANone);
    ],MANone);
    ("SetCached",[
      ("This",AThis);
      ("isCached",ANone);
    ],MANone);
  ]);
  ("ID2D1DrawInfoVtbl",IAManual, "ID2D1RenderInfoVtbl", [
    ("SetPixelShaderConstantBuffer",[
      ("This",AThis);
      ("buffer",ANone);
      ("bufferCount",ANone);
    ],MANone);
    ("SetResourceTexture",[
      ("This",AThis);
      ("textureIndex",ANone);
      ("resourceTexture",ANone);
    ],MANone);
    ("SetVertexShaderConstantBuffer",[
      ("This",AThis);
      ("buffer",ANone);
      ("bufferCount",ANone);
    ],MANone);
    ("SetPixelShader",[
      ("This",AThis);
      ("shaderId",ANone);
      ("pixelOptions",ANone);
    ],MANone);
    ("SetVertexProcessing",[
      ("This",AThis);
      ("vertexBuffer",ANone);
      ("vertexOptions",ANone);
      ("blendDescription",ANone);
      ("vertexRange",ANone);
      ("vertexShader",ANone);
    ],MANone);
  ]);
  ("ID2D1DrawTransformVtbl",IAManual, "ID2D1TransformVtbl", [
    ("SetDrawInfo",[
      ("This",AThis);
      ("drawInfo",ANone);
    ],MANone);
  ]);
  ("ID2D1EffectContextVtbl",IAManual, "IUnknownVtbl", [
    ("GetDpi",[
      ("This",AThis);
      ("dpiX",ANone);
      ("dpiY",ANone);
    ],MANone);
    ("CreateEffect",[
      ("This",AThis);
      ("effectId",ANone);
      ("effect",ANone);
    ],MANone);
    ("GetMaximumSupportedFeatureLevel",[
      ("This",AThis);
      ("featureLevels",ANone);
      ("featureLevelsCount",ANone);
      ("maximumSupportedFeatureLevel",ANone);
    ],MANone);
    ("CreateTransformNodeFromEffect",[
      ("This",AThis);
      ("effect",ANone);
      ("transformNode",ANone);
    ],MANone);
    ("CreateBlendTransform",[
      ("This",AThis);
      ("numInputs",ANone);
      ("blendDescription",ANone);
      ("transform",ANone);
    ],MANone);
    ("CreateBorderTransform",[
      ("This",AThis);
      ("extendModeX",ANone);
      ("extendModeY",ANone);
      ("transform",ANone);
    ],MANone);
    ("CreateOffsetTransform",[
      ("This",AThis);
      ("offset",ANone);
      ("transform",ANone);
    ],MANone);
    ("CreateBoundsAdjustmentTransform",[
      ("This",AThis);
      ("outputRectangle",ANone);
      ("transform",ANone);
    ],MANone);
    ("LoadPixelShader",[
      ("This",AThis);
      ("shaderId",ANone);
      ("shaderBuffer",ANone);
      ("shaderBufferCount",ANone);
    ],MANone);
    ("LoadVertexShader",[
      ("This",AThis);
      ("resourceId",ANone);
      ("shaderBuffer",ANone);
      ("shaderBufferCount",ANone);
    ],MANone);
    ("LoadComputeShader",[
      ("This",AThis);
      ("resourceId",ANone);
      ("shaderBuffer",ANone);
      ("shaderBufferCount",ANone);
    ],MANone);
    ("IsShaderLoaded",[
      ("This",AThis);
      ("shaderId",ANone);
    ],MANone);
    ("CreateResourceTexture",[
      ("This",AThis);
      ("resourceId",ANone);
      ("resourceTextureProperties",ANone);
      ("data",ANone);
      ("strides",ANone);
      ("dataSize",ANone);
      ("resourceTexture",ANone);
    ],MANone);
    ("FindResourceTexture",[
      ("This",AThis);
      ("resourceId",ANone);
      ("resourceTexture",ANone);
    ],MANone);
    ("CreateVertexBuffer",[
      ("This",AThis);
      ("vertexBufferProperties",ANone);
      ("resourceId",ANone);
      ("customVertexBufferProperties",ANone);
      ("buffer",ANone);
    ],MANone);
    ("FindVertexBuffer",[
      ("This",AThis);
      ("resourceId",ANone);
      ("buffer",ANone);
    ],MANone);
    ("CreateColorContext",[
      ("This",AThis);
      ("space",ANone);
      ("profile",ANone);
      ("profileSize",ANone);
      ("colorContext",ANone);
    ],MANone);
    ("CreateColorContextFromFilename",[
      ("This",AThis);
      ("filename",ANone);
      ("colorContext",ANone);
    ],MANone);
    ("CreateColorContextFromWicColorContext",[
      ("This",AThis);
      ("wicColorContext",ANone);
      ("colorContext",ANone);
    ],MANone);
    ("CheckFeatureSupport",[
      ("This",AThis);
      ("feature",ANone);
      ("featureSupportData",ANone);
      ("featureSupportDataSize",ANone);
    ],MANone);
    ("IsBufferPrecisionSupported",[
      ("This",AThis);
      ("bufferPrecision",ANone);
    ],MANone);
  ]);
  ("ID2D1EffectImplVtbl",IAManual, "IUnknownVtbl", [
    ("Initialize",[
      ("This",AThis);
      ("effectContext",ANone);
      ("transformGraph",ANone);
    ],MANone);
    ("PrepareForRender",[
      ("This",AThis);
      ("changeType",ANone);
    ],MANone);
    ("SetGraph",[
      ("This",AThis);
      ("transformGraph",ANone);
    ],MANone);
  ]);
  ("ID2D1OffsetTransformVtbl",IAManual, "ID2D1TransformNodeVtbl", [
    ("SetOffset",[
      ("This",AThis);
      ("offset",ANone);
    ],MANone);
    ("GetOffset",[
      ("This",AThis);
      ("__ret_val",ANone);
    ],MANone);
  ]);
  ("ID2D1RenderInfoVtbl",IAManual, "IUnknownVtbl", [
    ("SetInputDescription",[
      ("This",AThis);
      ("inputIndex",ANone);
      ("inputDescription",ANone);
    ],MANone);
    ("SetOutputBuffer",[
      ("This",AThis);
      ("bufferPrecision",ANone);
      ("channelDepth",ANone);
    ],MANone);
    ("SetCached",[
      ("This",AThis);
      ("isCached",ANone);
    ],MANone);
    ("SetInstructionCountHint",[
      ("This",AThis);
      ("instructionCount",ANone);
    ],MANone);
  ]);
  ("ID2D1ResourceTextureVtbl",IAManual, "IUnknownVtbl", [
    ("Update",[
      ("This",AThis);
      ("minimumExtents",ANone);
      ("maximimumExtents",ANone);
      ("strides",ANone);
      ("dimensions",ANone);
      ("data",ANone);
      ("dataCount",ANone);
    ],MANone);
  ]);
  ("ID2D1SourceTransformVtbl",IAManual, "ID2D1TransformVtbl", [
    ("SetRenderInfo",[
      ("This",AThis);
      ("renderInfo",ANone);
    ],MANone);
    ("Draw",[
      ("This",AThis);
      ("target",ANone);
      ("drawRect",ANone);
      ("targetOrigin",ANone);
    ],MANone);
  ]);
  ("ID2D1TransformGraphVtbl",IAManual, "IUnknownVtbl", [
    ("GetInputCount",[
      ("This",AThis);
    ],MANone);
    ("SetSingleTransformNode",[
      ("This",AThis);
      ("node",ANone);
    ],MANone);
    ("AddNode",[
      ("This",AThis);
      ("node",ANone);
    ],MANone);
    ("RemoveNode",[
      ("This",AThis);
      ("node",ANone);
    ],MANone);
    ("SetOutputNode",[
      ("This",AThis);
      ("node",ANone);
    ],MANone);
    ("ConnectNode",[
      ("This",AThis);
      ("fromNode",ANone);
      ("toNode",ANone);
      ("toNodeInputIndex",ANone);
    ],MANone);
    ("ConnectToEffectInput",[
      ("This",AThis);
      ("toEffectInputIndex",ANone);
      ("node",ANone);
      ("toNodeInputIndex",ANone);
    ],MANone);
    ("Clear",[
      ("This",AThis);
    ],MANone);
    ("SetPassthroughGraph",[
      ("This",AThis);
      ("effectInputIndex",ANone);
    ],MANone);
  ]);
  ("ID2D1TransformNodeVtbl",IAManual, "IUnknownVtbl", [
    ("GetInputCount",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ID2D1TransformVtbl",IAManual, "ID2D1TransformNodeVtbl", [
    ("MapOutputRectToInputRects",[
      ("This",AThis);
      ("outputRect",ANone);
      ("inputRects",ANone);
      ("inputRectsCount",ANone);
    ],MANone);
    ("MapInputRectsToOutputRect",[
      ("This",AThis);
      ("inputRects",ANone);
      ("inputOpaqueSubRects",ANone);
      ("inputRectCount",ANone);
      ("outputRect",ANone);
      ("outputOpaqueSubRect",ANone);
    ],MANone);
    ("MapInvalidRect",[
      ("This",AThis);
      ("inputIndex",ANone);
      ("invalidInputRect",ANone);
      ("invalidOutputRect",ANone);
    ],MANone);
  ]);
  ("ID2D1VertexBufferVtbl",IAManual, "IUnknownVtbl", [
    ("Map",[
      ("This",AThis);
      ("data",ANone);
      ("bufferSize",ANone);
    ],MANone);
    ("Unmap",[
      ("This",AThis);
    ],MANone);
  ]);
  ]
