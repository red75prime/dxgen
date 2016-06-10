﻿module annotations_objidlbase

open annotations

let objidlbase=[
  ("AsyncIMultiQIVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Begin_QueryMultipleInterfaces",[
      ("This",AThis);
      ("cMQIs",ANone);
      ("pMQIs",ANone);
    ],MANone);
    ("Finish_QueryMultipleInterfaces",[
      ("This",AThis);
      ("pMQIs",ANone);
    ],MANone);
  ]);
  ("AsyncIPipeByteVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Begin_Pull",[
      ("This",AThis);
      ("cRequest",ANone);
    ],MANone);
    ("Finish_Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Begin_Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
    ("Finish_Push",[
      ("This",AThis);
    ],MANone);
  ]);
  ("AsyncIPipeDoubleVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Begin_Pull",[
      ("This",AThis);
      ("cRequest",ANone);
    ],MANone);
    ("Finish_Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Begin_Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
    ("Finish_Push",[
      ("This",AThis);
    ],MANone);
  ]);
  ("AsyncIPipeLongVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Begin_Pull",[
      ("This",AThis);
      ("cRequest",ANone);
    ],MANone);
    ("Finish_Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Begin_Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
    ("Finish_Push",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IActivationFilterVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("HandleActivation",[
      ("This",AThis);
      ("dwActivationType",ANone);
      ("rclsid",ANone);
      ("pReplacementClsId",ANone);
    ],MANone);
  ]);
  ("IAddrExclusionControlVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetCurrentAddrExclusionList",[
      ("This",AThis);
      ("riid",ANone);
      ("ppEnumerator",ANone);
    ],MANone);
    ("UpdateAddrExclusionList",[
      ("This",AThis);
      ("pEnumerator",ANone);
    ],MANone);
  ]);
  ("IAddrTrackingControlVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("EnableCOMDynamicAddrTracking",[
      ("This",AThis);
    ],MANone);
    ("DisableCOMDynamicAddrTracking",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IAgileObjectVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
  ]);
  ("IAgileReferenceVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Resolve",[
      ("This",AThis);
      ("riid",ANone);
      ("ppvObjectReference",ANone);
    ],MANone);
  ]);
  ("IAsyncManagerVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("CompleteCall",[
      ("This",AThis);
      ("Result",ANone);
    ],MANone);
    ("GetCallContext",[
      ("This",AThis);
      ("riid",ANone);
      ("pInterface",ANone);
    ],MANone);
    ("GetState",[
      ("This",AThis);
      ("pulStateFlags",ANone);
    ],MANone);
  ]);
  ("IAsyncRpcChannelBufferVtbl",IAAutogen(Set.ofList []), "IRpcChannelBuffer2Vtbl", [
    ("Send",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pSync",ANone);
      ("pulStatus",ANone);
    ],MANone);
    ("Receive",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pulStatus",ANone);
    ],MANone);
    ("GetDestCtxEx",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pdwDestContext",ANone);
      ("ppvDestContext",ANone);
    ],MANone);
  ]);
  ("ICallFactoryVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("CreateCall",[
      ("This",AThis);
      ("riid",ANone);
      ("pCtrlUnk",ANone);
      ("riid2",ANone);
      ("ppv",ANone);
    ],MANone);
  ]);
  ("ICancelMethodCallsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Cancel",[
      ("This",AThis);
      ("ulSeconds",ANone);
    ],MANone);
    ("TestCancel",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IChannelHookVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("ClientGetSize",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("pDataSize",ANone);
    ],MANone);
    ("ClientFillBuffer",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("pDataSize",ANone);
      ("pDataBuffer",ANone);
    ],MANone);
    ("ClientNotify",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("cbDataSize",ANone);
      ("pDataBuffer",ANone);
      ("lDataRep",ANone);
      ("hrFault",ANone);
    ],MANone);
    ("ServerNotify",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("cbDataSize",ANone);
      ("pDataBuffer",ANone);
      ("lDataRep",ANone);
    ],MANone);
    ("ServerGetSize",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("hrFault",ANone);
      ("pDataSize",ANone);
    ],MANone);
    ("ServerFillBuffer",[
      ("This",AThis);
      ("uExtent",ANone);
      ("riid",ANone);
      ("pDataSize",ANone);
      ("pDataBuffer",ANone);
      ("hrFault",ANone);
    ],MANone);
  ]);
  ("IClientSecurityVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("QueryBlanket",[
      ("This",AThis);
      ("pProxy",ANone);
      ("pAuthnSvc",ANone);
      ("pAuthzSvc",ANone);
      ("pServerPrincName",ANone);
      ("pAuthnLevel",ANone);
      ("pImpLevel",ANone);
      ("pAuthInfo",ANone);
      ("pCapabilites",ANone);
    ],MANone);
    ("SetBlanket",[
      ("This",AThis);
      ("pProxy",ANone);
      ("dwAuthnSvc",ANone);
      ("dwAuthzSvc",ANone);
      ("pServerPrincName",ANone);
      ("dwAuthnLevel",ANone);
      ("dwImpLevel",ANone);
      ("pAuthInfo",ANone);
      ("dwCapabilities",ANone);
    ],MANone);
    ("CopyProxy",[
      ("This",AThis);
      ("pProxy",ANone);
      ("ppCopy",ANone);
    ],MANone);
  ]);
  ("IComThreadingInfoVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetCurrentApartmentType",[
      ("This",AThis);
      ("pAptType",ANone);
    ],MANone);
    ("GetCurrentThreadType",[
      ("This",AThis);
      ("pThreadType",ANone);
    ],MANone);
    ("GetCurrentLogicalThreadId",[
      ("This",AThis);
      ("pguidLogicalThreadId",ANone);
    ],MANone);
    ("SetCurrentLogicalThreadId",[
      ("This",AThis);
      ("rguid",ANone);
    ],MANone);
  ]);
  ("IEnumStringVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Next",[
      ("This",AThis);
      ("celt",ANone);
      ("rgelt",ANone);
      ("pceltFetched",ANone);
    ],MANone);
    ("Skip",[
      ("This",AThis);
      ("celt",ANone);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppenum",ANone);
    ],MANone);
  ]);
  ("IEnumUnknownVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Next",[
      ("This",AThis);
      ("celt",ANone);
      ("rgelt",ANone);
      ("pceltFetched",ANone);
    ],MANone);
    ("Skip",[
      ("This",AThis);
      ("celt",ANone);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppenum",ANone);
    ],MANone);
  ]);
  ("IExternalConnectionVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("AddConnection",[
      ("This",AThis);
      ("extconn",ANone);
      ("reserved",ANone);
    ],MANone);
    ("ReleaseConnection",[
      ("This",AThis);
      ("extconn",ANone);
      ("reserved",ANone);
      ("fLastReleaseCloses",ANone);
    ],MANone);
  ]);
  ("IFastRundownVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
  ]);
  ("IGlobalInterfaceTableVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("RegisterInterfaceInGlobal",[
      ("This",AThis);
      ("pUnk",ANone);
      ("riid",ANone);
      ("pdwCookie",ANone);
    ],MANone);
    ("RevokeInterfaceFromGlobal",[
      ("This",AThis);
      ("dwCookie",ANone);
    ],MANone);
    ("GetInterfaceFromGlobal",[
      ("This",AThis);
      ("dwCookie",ANone);
      ("riid",ANone);
      ("ppv",ANone);
    ],MANone);
  ]);
  ("IGlobalOptionsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Set",[
      ("This",AThis);
      ("dwProperty",ANone);
      ("dwValue",ANone);
    ],MANone);
    ("Query",[
      ("This",AThis);
      ("dwProperty",ANone);
      ("pdwValue",ANone);
    ],MANone);
  ]);
  ("IInternalUnknownVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("QueryInternalInterface",[
      ("This",AThis);
      ("riid",ANone);
      ("ppv",ANone);
    ],MANone);
  ]);
  ("IMallocVtbl", IAManual, "IUnknownVtbl", [
    ("Alloc",[
      ("This",AThis);
      ("cb",ANone);
    ],MANone);
    ("Realloc",[
      ("This",AThis);
      ("pv",ANone);
      ("cb",ANone);
    ],MANone);
    ("Free",[
      ("This",AThis);
      ("pv",ANone);
    ],MANone);
    ("GetSize",[
      ("This",AThis);
      ("pv",ANone);
    ],MANone);
    ("DidAlloc",[
      ("This",AThis);
      ("pv",ANone);
    ],MANone);
    ("HeapMinimize",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IMarshal2Vtbl",IAAutogen(Set.ofList []), "IMarshalVtbl", [
  ]);
  ("IMarshalVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetUnmarshalClass",[
      ("This",AThis);
      ("riid",ANone);
      ("pv",ANone);
      ("dwDestContext",ANone);
      ("pvDestContext",ANone);
      ("mshlflags",ANone);
      ("pCid",ANone);
    ],MANone);
    ("GetMarshalSizeMax",[
      ("This",AThis);
      ("riid",ANone);
      ("pv",ANone);
      ("dwDestContext",ANone);
      ("pvDestContext",ANone);
      ("mshlflags",ANone);
      ("pSize",ANone);
    ],MANone);
    ("MarshalInterface",[
      ("This",AThis);
      ("pStm",ANone);
      ("riid",ANone);
      ("pv",ANone);
      ("dwDestContext",ANone);
      ("pvDestContext",ANone);
      ("mshlflags",ANone);
    ],MANone);
    ("UnmarshalInterface",[
      ("This",AThis);
      ("pStm",ANone);
      ("riid",ANone);
      ("ppv",ANone);
    ],MANone);
    ("ReleaseMarshalData",[
      ("This",AThis);
      ("pStm",ANone);
    ],MANone);
    ("DisconnectObject",[
      ("This",AThis);
      ("dwReserved",ANone);
    ],MANone);
  ]);
  ("IMarshalingStreamVtbl",IAAutogen(Set.ofList []), "IStreamVtbl", [
    ("GetMarshalingContextAttribute",[
      ("This",AThis);
      ("attribute",ANone);
      ("pAttributeValue",ANone);
    ],MANone);
  ]);
  ("IMultiQIVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("QueryMultipleInterfaces",[
      ("This",AThis);
      ("cMQIs",ANone);
      ("pMQIs",ANone);
    ],MANone);
  ]);
  ("INoMarshalVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
  ]);
  ("IPSFactoryBufferVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("CreateProxy",[
      ("This",AThis);
      ("pUnkOuter",ANone);
      ("riid",ANone);
      ("ppProxy",ANone);
      ("ppv",ANone);
    ],MANone);
    ("CreateStub",[
      ("This",AThis);
      ("riid",ANone);
      ("pUnkServer",ANone);
      ("ppStub",ANone);
    ],MANone);
  ]);
  ("IPipeByteVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("cRequest",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
  ]);
  ("IPipeDoubleVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("cRequest",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
  ]);
  ("IPipeLongVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Pull",[
      ("This",AThis);
      ("buf",ANone);
      ("cRequest",ANone);
      ("pcReturned",ANone);
    ],MANone);
    ("Push",[
      ("This",AThis);
      ("buf",ANone);
      ("cSent",ANone);
    ],MANone);
  ]);
  ("IProcessInitControlVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("ResetInitializerTimeout",[
      ("This",AThis);
      ("dwSecondsRemaining",ANone);
    ],MANone);
  ]);
  ("IReleaseMarshalBuffersVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("ReleaseMarshalBuffer",[
      ("This",AThis);
      ("pMsg",ANone);
      ("dwFlags",ANone);
      ("pChnl",ANone);
    ],MANone);
  ]);
  ("IRpcChannelBuffer2Vtbl",IAAutogen(Set.ofList []), "IRpcChannelBufferVtbl", [
    ("GetProtocolVersion",[
      ("This",AThis);
      ("pdwVersion",ANone);
    ],MANone);
  ]);
  ("IRpcChannelBuffer3Vtbl",IAAutogen(Set.ofList []), "IRpcChannelBuffer2Vtbl", [
    ("Send",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pulStatus",ANone);
    ],MANone);
    ("Receive",[
      ("This",AThis);
      ("pMsg",ANone);
      ("ulSize",ANone);
      ("pulStatus",ANone);
    ],MANone);
    ("Cancel",[
      ("This",AThis);
      ("pMsg",ANone);
    ],MANone);
    ("GetCallContext",[
      ("This",AThis);
      ("pMsg",ANone);
      ("riid",ANone);
      ("pInterface",ANone);
    ],MANone);
    ("GetDestCtxEx",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pdwDestContext",ANone);
      ("ppvDestContext",ANone);
    ],MANone);
    ("GetState",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pState",ANone);
    ],MANone);
    ("RegisterAsync",[
      ("This",AThis);
      ("pMsg",ANone);
      ("pAsyncMgr",ANone);
    ],MANone);
  ]);
  ("IRpcChannelBufferVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetBuffer",[
      ("This",AThis);
      ("pMessage",ANone);
      ("riid",ANone);
    ],MANone);
    ("SendReceive",[
      ("This",AThis);
      ("pMessage",ANone);
      ("pStatus",ANone);
    ],MANone);
    ("FreeBuffer",[
      ("This",AThis);
      ("pMessage",ANone);
    ],MANone);
    ("GetDestCtx",[
      ("This",AThis);
      ("pdwDestContext",ANone);
      ("ppvDestContext",ANone);
    ],MANone);
    ("IsConnected",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IRpcHelperVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetDCOMProtocolVersion",[
      ("This",AThis);
      ("pComVersion",ANone);
    ],MANone);
    ("GetIIDFromOBJREF",[
      ("This",AThis);
      ("pObjRef",ANone);
      ("piid",ANone);
    ],MANone);
  ]);
  ("IRpcOptionsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Set",[
      ("This",AThis);
      ("pPrx",ANone);
      ("dwProperty",ANone);
      ("dwValue",ANone);
    ],MANone);
    ("Query",[
      ("This",AThis);
      ("pPrx",ANone);
      ("dwProperty",ANone);
      ("pdwValue",ANone);
    ],MANone);
  ]);
  ("IRpcProxyBufferVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Connect",[
      ("This",AThis);
      ("pRpcChannelBuffer",ANone);
    ],MANone);
    ("Disconnect",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IRpcStubBufferVtbl",IAManual, "IUnknownVtbl", [
    ("Connect",[
      ("This",AThis);
      ("pUnkServer",ANone);
    ],MANone);
    ("Disconnect",[
      ("This",AThis);
    ],MANone);
    ("Invoke",[
      ("This",AThis);
      ("_prpcmsg",ANone);
      ("_pRpcChannelBuffer",ANone);
    ],MANone);
    ("IsIIDSupported",[
      ("This",AThis);
      ("riid",ANone);
    ],MANone);
    ("CountRefs",[
      ("This",AThis);
    ],MANone);
    ("DebugServerQueryInterface",[
      ("This",AThis);
      ("ppv",ANone);
    ],MANone);
    ("DebugServerRelease",[
      ("This",AThis);
      ("pv",ANone);
    ],MANone);
  ]);
  ("IRpcSyntaxNegotiateVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("NegotiateSyntax",[
      ("This",AThis);
      ("pMsg",ANone);
    ],MANone);
  ]);
  ("ISequentialStreamVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Read",[
      ("This",AThis);
      ("pv",ANone);
      ("cb",ANone);
      ("pcbRead",ANone);
    ],MANone);
    ("Write",[
      ("This",AThis);
      ("pv",ANone);
      ("cb",ANone);
      ("pcbWritten",ANone);
    ],MANone);
  ]);
  ("IServerSecurityVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("QueryBlanket",[
      ("This",AThis);
      ("pAuthnSvc",ANone);
      ("pAuthzSvc",ANone);
      ("pServerPrincName",ANone);
      ("pAuthnLevel",ANone);
      ("pImpLevel",ANone);
      ("pPrivs",ANone);
      ("pCapabilities",ANone);
    ],MANone);
    ("ImpersonateClient",[
      ("This",AThis);
    ],MANone);
    ("RevertToSelf",[
      ("This",AThis);
    ],MANone);
    ("IsImpersonating",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IStdMarshalInfoVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetClassForHandler",[
      ("This",AThis);
      ("dwDestContext",ANone);
      ("pvDestContext",ANone);
      ("pClsid",ANone);
    ],MANone);
  ]);
  ("IStreamVtbl",IAAutogen(Set.ofList []), "ISequentialStreamVtbl", [
    ("Seek",[
      ("This",AThis);
      ("dlibMove",ANone);
      ("dwOrigin",ANone);
      ("plibNewPosition",ANone);
    ],MANone);
    ("SetSize",[
      ("This",AThis);
      ("libNewSize",ANone);
    ],MANone);
    ("CopyTo",[
      ("This",AThis);
      ("pstm",ANone);
      ("cb",ANone);
      ("pcbRead",ANone);
      ("pcbWritten",ANone);
    ],MANone);
    ("Commit",[
      ("This",AThis);
      ("grfCommitFlags",ANone);
    ],MANone);
    ("Revert",[
      ("This",AThis);
    ],MANone);
    ("LockRegion",[
      ("This",AThis);
      ("libOffset",ANone);
      ("cb",ANone);
      ("dwLockType",ANone);
    ],MANone);
    ("UnlockRegion",[
      ("This",AThis);
      ("libOffset",ANone);
      ("cb",ANone);
      ("dwLockType",ANone);
    ],MANone);
    ("Stat",[
      ("This",AThis);
      ("pstatstg",ANone);
      ("grfStatFlag",ANone);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppstm",ANone);
    ],MANone);
  ]);
  ("ISurrogateVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("LoadDllServer",[
      ("This",AThis);
      ("Clsid",ANone);
    ],MANone);
    ("FreeSurrogate",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ISynchronizeContainerVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("AddSynchronize",[
      ("This",AThis);
      ("pSync",ANone);
    ],MANone);
    ("WaitMultiple",[
      ("This",AThis);
      ("dwFlags",ANone);
      ("dwTimeOut",ANone);
      ("ppSync",ANone);
    ],MANone);
  ]);
  ("ISynchronizeEventVtbl",IAAutogen(Set.ofList []), "ISynchronizeHandleVtbl", [
    ("SetEventHandle",[
      ("This",AThis);
      ("ph",ANone);
    ],MANone);
  ]);
  ("ISynchronizeHandleVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetHandle",[
      ("This",AThis);
      ("ph",ANone);
    ],MANone);
  ]);
  ("ISynchronizeMutexVtbl",IAAutogen(Set.ofList []), "ISynchronizeVtbl", [
    ("ReleaseMutex",[
      ("This",AThis);
    ],MANone);
  ]);
  ("ISynchronizeVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Wait",[
      ("This",AThis);
      ("dwFlags",ANone);
      ("dwMilliseconds",ANone);
    ],MANone);
    ("Signal",[
      ("This",AThis);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IWaitMultipleVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("WaitMultiple",[
      ("This",AThis);
      ("timeout",ANone);
      ("pSync",ANone);
    ],MANone);
    ("AddSynchronize",[
      ("This",AThis);
      ("pSync",ANone);
    ],MANone);
  ]);
  ]