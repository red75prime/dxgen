﻿module annotations_ocidl

open annotations

let ocidl=[
  ("IAdviseSinkExVtbl",IAManual, "IAdviseSinkVtbl", [
    ("OnViewStatusChange",[
      ("This",AThis);
      ("dwViewStatus",ANone);
    ],MANone);
  ]);
  ("IClassFactory2Vtbl",IAManual, "IClassFactoryVtbl", [
    ("GetLicInfo",[
      ("This",AThis);
      ("pLicInfo",ANone);
    ],MANone);
    ("RequestLicKey",[
      ("This",AThis);
      ("dwReserved",ANone);
      ("pBstrKey",ANone);
    ],MANone);
    ("CreateInstanceLic",[
      ("This",AThis);
      ("pUnkOuter",ANone);
      ("pUnkReserved",ANone);
      ("riid",ANone);
      ("bstrKey",ANone);
      ("ppvObj",ANone);
    ],MANone);
  ]);
  ("IConnectionPointContainerVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("EnumConnectionPoints",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
    ("FindConnectionPoint",[
      ("This",AThis);
      ("riid",ANone);
      ("ppCP",ANone);
    ],MANone);
  ]);
  ("IConnectionPointVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetConnectionInterface",[
      ("This",AThis);
      ("pIID",ANone);
    ],MANone);
    ("GetConnectionPointContainer",[
      ("This",AThis);
      ("ppCPC",ANone);
    ],MANone);
    ("Advise",[
      ("This",AThis);
      ("pUnkSink",ANone);
      ("pdwCookie",ANone);
    ],MANone);
    ("Unadvise",[
      ("This",AThis);
      ("dwCookie",ANone);
    ],MANone);
    ("EnumConnections",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
  ]);
  ("IEnumConnectionPointsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Next",[
      ("This",AThis);
      ("cConnections",ANone);
      ("ppCP",ANone);
      ("pcFetched",ANone);
    ],MANone);
    ("Skip",[
      ("This",AThis);
      ("cConnections",ANone);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
  ]);
  ("IEnumConnectionsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Next",[
      ("This",AThis);
      ("cConnections",ANone);
      ("rgcd",ANone);
      ("pcFetched",ANone);
    ],MANone);
    ("Skip",[
      ("This",AThis);
      ("cConnections",ANone);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
  ]);
  ("IEnumOleUndoUnitsVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Next",[
      ("This",AThis);
      ("cElt",ANone);
      ("rgElt",ANone);
      ("pcEltFetched",ANone);
    ],MANone);
    ("Skip",[
      ("This",AThis);
      ("cElt",ANone);
    ],MANone);
    ("Reset",[
      ("This",AThis);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
  ]);
  ("IFontDispVtbl",IAManual, "IDispatchVtbl", [  ]);
  ("IFontEventsDispVtbl",IAManual, "IDispatchVtbl", [  ]);
  ("IFontVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("get_Name",[
      ("This",AThis);
      ("pName",ANone);
    ],MANone);
    ("put_Name",[
      ("This",AThis);
      ("name",ANone);
    ],MANone);
    ("get_Size",[
      ("This",AThis);
      ("pSize",ANone);
    ],MANone);
    ("put_Size",[
      ("This",AThis);
      ("size",ANone);
    ],MANone);
    ("get_Bold",[
      ("This",AThis);
      ("pBold",ANone);
    ],MANone);
    ("put_Bold",[
      ("This",AThis);
      ("bold",ANone);
    ],MANone);
    ("get_Italic",[
      ("This",AThis);
      ("pItalic",ANone);
    ],MANone);
    ("put_Italic",[
      ("This",AThis);
      ("italic",ANone);
    ],MANone);
    ("get_Underline",[
      ("This",AThis);
      ("pUnderline",ANone);
    ],MANone);
    ("put_Underline",[
      ("This",AThis);
      ("underline",ANone);
    ],MANone);
    ("get_Strikethrough",[
      ("This",AThis);
      ("pStrikethrough",ANone);
    ],MANone);
    ("put_Strikethrough",[
      ("This",AThis);
      ("strikethrough",ANone);
    ],MANone);
    ("get_Weight",[
      ("This",AThis);
      ("pWeight",ANone);
    ],MANone);
    ("put_Weight",[
      ("This",AThis);
      ("weight",ANone);
    ],MANone);
    ("get_Charset",[
      ("This",AThis);
      ("pCharset",ANone);
    ],MANone);
    ("put_Charset",[
      ("This",AThis);
      ("charset",ANone);
    ],MANone);
    ("get_hFont",[
      ("This",AThis);
      ("phFont",ANone);
    ],MANone);
    ("Clone",[
      ("This",AThis);
      ("ppFont",ANone);
    ],MANone);
    ("IsEqual",[
      ("This",AThis);
      ("pFontOther",ANone);
    ],MANone);
    ("SetRatio",[
      ("This",AThis);
      ("cyLogical",ANone);
      ("cyHimetric",ANone);
    ],MANone);
    ("QueryTextMetrics",[
      ("This",AThis);
      ("pTM",ANone);
    ],MANone);
    ("AddRefHfont",[
      ("This",AThis);
      ("hFont",ANone);
    ],MANone);
    ("ReleaseHfont",[
      ("This",AThis);
      ("hFont",ANone);
    ],MANone);
    ("SetHdc",[
      ("This",AThis);
      ("hDC",ANone);
    ],MANone);
  ]);
  ("IObjectWithSiteVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("SetSite",[
      ("This",AThis);
      ("pUnkSite",ANone);
    ],MANone);
    ("GetSite",[
      ("This",AThis);
      ("riid",ANone);
      ("ppvSite",ANone);
    ],MANone);
  ]);
  ("IOleControlSiteVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("OnControlInfoChanged",[
      ("This",AThis);
    ],MANone);
    ("LockInPlaceActive",[
      ("This",AThis);
      ("fLock",ANone);
    ],MANone);
    ("GetExtendedControl",[
      ("This",AThis);
      ("ppDisp",ANone);
    ],MANone);
    ("TransformCoords",[
      ("This",AThis);
      ("pPtlHimetric",ANone);
      ("pPtfContainer",ANone);
      ("dwFlags",ANone);
    ],MANone);
    ("TranslateAcceleratorA",[
      ("This",AThis);
      ("pMsg",ANone);
      ("grfModifiers",ANone);
    ],MANone);
    ("OnFocus",[
      ("This",AThis);
      ("fGotFocus",ANone);
    ],MANone);
    ("ShowPropertyFrame",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IOleControlVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetControlInfo",[
      ("This",AThis);
      ("pCI",ANone);
    ],MANone);
    ("OnMnemonic",[
      ("This",AThis);
      ("pMsg",ANone);
    ],MANone);
    ("OnAmbientPropertyChange",[
      ("This",AThis);
      ("dispID",ANone);
    ],MANone);
    ("FreezeEvents",[
      ("This",AThis);
      ("bFreeze",ANone);
    ],MANone);
  ]);
  ("IOleInPlaceObjectWindowlessVtbl",IAManual, "IOleInPlaceObjectVtbl", [
    ("OnWindowMessage",[
      ("This",AThis);
      ("msg",ANone);
      ("wParam",ANone);
      ("lParam",ANone);
      ("plResult",ANone);
    ],MANone);
    ("GetDropTarget",[
      ("This",AThis);
      ("ppDropTarget",ANone);
    ],MANone);
  ]);
  ("IOleInPlaceSiteExVtbl",IAManual, "IOleInPlaceSiteVtbl", [
    ("OnInPlaceActivateEx",[
      ("This",AThis);
      ("pfNoRedraw",ANone);
      ("dwFlags",ANone);
    ],MANone);
    ("OnInPlaceDeactivateEx",[
      ("This",AThis);
      ("fNoRedraw",ANone);
    ],MANone);
    ("RequestUIActivate",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IOleInPlaceSiteWindowlessVtbl",IAManual, "IOleInPlaceSiteExVtbl", [
    ("CanWindowlessActivate",[
      ("This",AThis);
    ],MANone);
    ("GetCapture",[
      ("This",AThis);
    ],MANone);
    ("SetCapture",[
      ("This",AThis);
      ("fCapture",ANone);
    ],MANone);
    ("GetFocus",[
      ("This",AThis);
    ],MANone);
    ("SetFocus",[
      ("This",AThis);
      ("fFocus",ANone);
    ],MANone);
    ("GetDC",[
      ("This",AThis);
      ("pRect",ANone);
      ("grfFlags",ANone);
      ("phDC",ANone);
    ],MANone);
    ("ReleaseDC",[
      ("This",AThis);
      ("hDC",ANone);
    ],MANone);
    ("InvalidateRect",[
      ("This",AThis);
      ("pRect",ANone);
      ("fErase",ANone);
    ],MANone);
    ("InvalidateRgn",[
      ("This",AThis);
      ("hRGN",ANone);
      ("fErase",ANone);
    ],MANone);
    ("ScrollRect",[
      ("This",AThis);
      ("dx",ANone);
      ("dy",ANone);
      ("pRectScroll",ANone);
      ("pRectClip",ANone);
    ],MANone);
    ("AdjustRect",[
      ("This",AThis);
      ("prc",ANone);
    ],MANone);
    ("OnDefWindowMessage",[
      ("This",AThis);
      ("msg",ANone);
      ("wParam",ANone);
      ("lParam",ANone);
      ("plResult",ANone);
    ],MANone);
  ]);
  ("IOleParentUndoUnitVtbl",IAManual, "IOleUndoUnitVtbl", [
    ("Open",[
      ("This",AThis);
      ("pPUU",ANone);
    ],MANone);
    ("Close",[
      ("This",AThis);
      ("pPUU",ANone);
      ("fCommit",ANone);
    ],MANone);
    ("Add",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("FindUnit",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("GetParentState",[
      ("This",AThis);
      ("pdwState",ANone);
    ],MANone);
  ]);
  ("IOleUndoManagerVtbl",IAManual, "IUnknownVtbl", [
    ("Open",[
      ("This",AThis);
      ("pPUU",ANone);
    ],MANone);
    ("Close",[
      ("This",AThis);
      ("pPUU",ANone);
      ("fCommit",ANone);
    ],MANone);
    ("Add",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("GetOpenParentState",[
      ("This",AThis);
      ("pdwState",ANone);
    ],MANone);
    ("DiscardFrom",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("UndoTo",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("RedoTo",[
      ("This",AThis);
      ("pUU",ANone);
    ],MANone);
    ("EnumUndoable",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
    ("EnumRedoable",[
      ("This",AThis);
      ("ppEnum",ANone);
    ],MANone);
    ("GetLastUndoDescription",[
      ("This",AThis);
      ("pBstr",ANone);
    ],MANone);
    ("GetLastRedoDescription",[
      ("This",AThis);
      ("pBstr",ANone);
    ],MANone);
    ("Enable",[
      ("This",AThis);
      ("fEnable",ANone);
    ],MANone);
  ]);
  ("IOleUndoUnitVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Do",[
      ("This",AThis);
      ("pUndoManager",ANone);
    ],MANone);
    ("GetDescription",[
      ("This",AThis);
      ("pBstr",ANone);
    ],MANone);
    ("GetUnitType",[
      ("This",AThis);
      ("pClsid",ANone);
      ("plID",ANone);
    ],MANone);
    ("OnNextAdd",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IPerPropertyBrowsingVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetDisplayString",[
      ("This",AThis);
      ("dispID",ANone);
      ("pBstr",ANone);
    ],MANone);
    ("MapPropertyToPage",[
      ("This",AThis);
      ("dispID",ANone);
      ("pClsid",ANone);
    ],MANone);
    ("GetPredefinedStrings",[
      ("This",AThis);
      ("dispID",ANone);
      ("pCaStringsOut",ANone);
      ("pCaCookiesOut",ANone);
    ],MANone);
    ("GetPredefinedValue",[
      ("This",AThis);
      ("dispID",ANone);
      ("dwCookie",ANone);
      ("pVarOut",ANone);
    ],MANone);
  ]);
  ("IPersistMemoryVtbl",IAManual, "IPersistVtbl", [
    ("IsDirty",[
      ("This",AThis);
    ],MANone);
    ("Load",[
      ("This",AThis);
      ("pMem",ANone);
      ("cbSize",ANone);
    ],MANone);
    ("Save",[
      ("This",AThis);
      ("pMem",ANone);
      ("fClearDirty",ANone);
      ("cbSize",ANone);
    ],MANone);
    ("GetSizeMax",[
      ("This",AThis);
      ("pCbSize",ANone);
    ],MANone);
    ("InitNew",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IPersistPropertyBag2Vtbl",IAManual, "IPersistVtbl", [
    ("InitNew",[
      ("This",AThis);
    ],MANone);
    ("Load",[
      ("This",AThis);
      ("pPropBag",ANone);
      ("pErrLog",ANone);
    ],MANone);
    ("Save",[
      ("This",AThis);
      ("pPropBag",ANone);
      ("fClearDirty",ANone);
      ("fSaveAllProperties",ANone);
    ],MANone);
    ("IsDirty",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IPersistPropertyBagVtbl",IAManual, "IPersistVtbl", [
    ("InitNew",[
      ("This",AThis);
    ],MANone);
    ("Load",[
      ("This",AThis);
      ("pPropBag",ANone);
      ("pErrorLog",ANone);
    ],MANone);
    ("Save",[
      ("This",AThis);
      ("pPropBag",ANone);
      ("fClearDirty",ANone);
      ("fSaveAllProperties",ANone);
    ],MANone);
  ]);
  ("IPersistStreamInitVtbl",IAManual, "IPersistVtbl", [
    ("IsDirty",[
      ("This",AThis);
    ],MANone);
    ("Load",[
      ("This",AThis);
      ("pStm",ANone);
    ],MANone);
    ("Save",[
      ("This",AThis);
      ("pStm",ANone);
      ("fClearDirty",ANone);
    ],MANone);
    ("GetSizeMax",[
      ("This",AThis);
      ("pCbSize",ANone);
    ],MANone);
    ("InitNew",[
      ("This",AThis);
    ],MANone);
  ]);
  ("IPicture2Vtbl",IAManual, "IUnknownVtbl", [
    ("get_Handle",[
      ("This",AThis);
      ("pHandle",ANone);
    ],MANone);
    ("get_hPal",[
      ("This",AThis);
      ("phPal",ANone);
    ],MANone);
    ("get_Type",[
      ("This",AThis);
      ("pType",ANone);
    ],MANone);
    ("get_Width",[
      ("This",AThis);
      ("pWidth",ANone);
    ],MANone);
    ("get_Height",[
      ("This",AThis);
      ("pHeight",ANone);
    ],MANone);
    ("Render",[
      ("This",AThis);
      ("hDC",ANone);
      ("x",ANone);
      ("y",ANone);
      ("cx",ANone);
      ("cy",ANone);
      ("xSrc",ANone);
      ("ySrc",ANone);
      ("cxSrc",ANone);
      ("cySrc",ANone);
      ("pRcWBounds",ANone);
    ],MANone);
    ("set_hPal",[
      ("This",AThis);
      ("hPal",ANone);
    ],MANone);
    ("get_CurDC",[
      ("This",AThis);
      ("phDC",ANone);
    ],MANone);
    ("SelectPicture",[
      ("This",AThis);
      ("hDCIn",ANone);
      ("phDCOut",ANone);
      ("phBmpOut",ANone);
    ],MANone);
    ("get_KeepOriginalFormat",[
      ("This",AThis);
      ("pKeep",ANone);
    ],MANone);
    ("put_KeepOriginalFormat",[
      ("This",AThis);
      ("keep",ANone);
    ],MANone);
    ("PictureChanged",[
      ("This",AThis);
    ],MANone);
    ("SaveAsFile",[
      ("This",AThis);
      ("pStream",ANone);
      ("fSaveMemCopy",ANone);
      ("pCbSize",ANone);
    ],MANone);
    ("get_Attributes",[
      ("This",AThis);
      ("pDwAttr",ANone);
    ],MANone);
  ]);
  ("IPictureDispVtbl",IAManual, "IDispatchVtbl", [  ]);
  ("IPictureVtbl",IAManual, "IUnknownVtbl", [
    ("get_Handle",[
      ("This",AThis);
      ("pHandle",ANone);
    ],MANone);
    ("get_hPal",[
      ("This",AThis);
      ("phPal",ANone);
    ],MANone);
    ("get_Type",[
      ("This",AThis);
      ("pType",ANone);
    ],MANone);
    ("get_Width",[
      ("This",AThis);
      ("pWidth",ANone);
    ],MANone);
    ("get_Height",[
      ("This",AThis);
      ("pHeight",ANone);
    ],MANone);
    ("Render",[
      ("This",AThis);
      ("hDC",ANone);
      ("x",ANone);
      ("y",ANone);
      ("cx",ANone);
      ("cy",ANone);
      ("xSrc",ANone);
      ("ySrc",ANone);
      ("cxSrc",ANone);
      ("cySrc",ANone);
      ("pRcWBounds",ANone);
    ],MANone);
    ("set_hPal",[
      ("This",AThis);
      ("hPal",ANone);
    ],MANone);
    ("get_CurDC",[
      ("This",AThis);
      ("phDC",ANone);
    ],MANone);
    ("SelectPicture",[
      ("This",AThis);
      ("hDCIn",ANone);
      ("phDCOut",ANone);
      ("phBmpOut",ANone);
    ],MANone);
    ("get_KeepOriginalFormat",[
      ("This",AThis);
      ("pKeep",ANone);
    ],MANone);
    ("put_KeepOriginalFormat",[
      ("This",AThis);
      ("keep",ANone);
    ],MANone);
    ("PictureChanged",[
      ("This",AThis);
    ],MANone);
    ("SaveAsFile",[
      ("This",AThis);
      ("pStream",ANone);
      ("fSaveMemCopy",ANone);
      ("pCbSize",ANone);
    ],MANone);
    ("get_Attributes",[
      ("This",AThis);
      ("pDwAttr",ANone);
    ],MANone);
  ]);
  ("IPointerInactiveVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("GetActivationPolicy",[
      ("This",AThis);
      ("pdwPolicy",ANone);
    ],MANone);
    ("OnInactiveMouseMove",[
      ("This",AThis);
      ("pRectBounds",ANone);
      ("x",ANone);
      ("y",ANone);
      ("grfKeyState",ANone);
    ],MANone);
    ("OnInactiveSetCursor",[
      ("This",AThis);
      ("pRectBounds",ANone);
      ("x",ANone);
      ("y",ANone);
      ("dwMouseMsg",ANone);
      ("fSetAlways",ANone);
    ],MANone);
  ]);
  ("IPropertyBag2Vtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("Read",[
      ("This",AThis);
      ("cProperties",ANone);
      ("pPropBag",ANone);
      ("pErrLog",ANone);
      ("pvarValue",ANone);
      ("phrError",ANone);
    ],MANone);
    ("Write",[
      ("This",AThis);
      ("cProperties",ANone);
      ("pPropBag",ANone);
      ("pvarValue",ANone);
    ],MANone);
    ("CountProperties",[
      ("This",AThis);
      ("pcProperties",ANone);
    ],MANone);
    ("GetPropertyInfo",[
      ("This",AThis);
      ("iProperty",ANone);
      ("cProperties",ANone);
      ("pPropBag",ANone);
      ("pcProperties",ANone);
    ],MANone);
    ("LoadObject",[
      ("This",AThis);
      ("pstrName",ANone);
      ("dwHint",ANone);
      ("pUnkObject",ANone);
      ("pErrLog",ANone);
    ],MANone);
  ]);
  ("IPropertyNotifySinkVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("OnChanged",[
      ("This",AThis);
      ("dispID",ANone);
    ],MANone);
    ("OnRequestEdit",[
      ("This",AThis);
      ("dispID",ANone);
    ],MANone);
  ]);
  ("IPropertyPage2Vtbl",IAManual, "IPropertyPageVtbl", [
    ("EditProperty",[
      ("This",AThis);
      ("dispID",ANone);
    ],MANone);
  ]);
  ("IPropertyPageSiteVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("OnStatusChange",[
      ("This",AThis);
      ("dwFlags",ANone);
    ],MANone);
    ("GetLocaleID",[
      ("This",AThis);
      ("pLocaleID",ANone);
    ],MANone);
    ("GetPageContainer",[
      ("This",AThis);
      ("ppUnk",ANone);
    ],MANone);
    ("TranslateAcceleratorA",[
      ("This",AThis);
      ("pMsg",ANone);
    ],MANone);
  ]);
  ("IPropertyPageVtbl",IAAutogen(Set.ofList []), "IUnknownVtbl", [
    ("SetPageSite",[
      ("This",AThis);
      ("pPageSite",ANone);
    ],MANone);
    ("Activate",[
      ("This",AThis);
      ("hWndParent",ANone);
      ("pRect",ANone);
      ("bModal",ANone);
    ],MANone);
    ("Deactivate",[
      ("This",AThis);
    ],MANone);
    ("GetPageInfo",[
      ("This",AThis);
      ("pPageInfo",ANone);
    ],MANone);
    ("SetObjects",[
      ("This",AThis);
      ("cObjects",ANone);
      ("ppUnk",ANone);
    ],MANone);
    ("Show",[
      ("This",AThis);
      ("nCmdShow",ANone);
    ],MANone);
    ("Move",[
      ("This",AThis);
      ("pRect",ANone);
    ],MANone);
    ("IsPageDirty",[
      ("This",AThis);
    ],MANone);
    ("Apply",[
      ("This",AThis);
    ],MANone);
    ("Help",[
      ("This",AThis);
      ("pszHelpDir",ANone);
    ],MANone);
    ("TranslateAcceleratorA",[
      ("This",AThis);
      ("pMsg",ANone);
    ],MANone);
  ]);
  ("IProvideClassInfo2Vtbl",IAManual, "IProvideClassInfoVtbl", [
    ("GetGUID",[
      ("This",AThis);
      ("dwGuidKind",ANone);
      ("pGUID",ANone);
    ],MANone);
  ]);
  ("IProvideClassInfoVtbl",IAManual, "IUnknownVtbl", [
    ("GetClassInfoA",[
      ("This",AThis);
      ("ppTI",ANone);
    ],MANone);
  ]);
  ("IProvideMultipleClassInfoVtbl",IAManual, "IProvideClassInfo2Vtbl", [
    ("GetMultiTypeInfoCount",[
      ("This",AThis);
      ("pcti",ANone);
    ],MANone);
    ("GetInfoOfIndex",[
      ("This",AThis);
      ("iti",ANone);
      ("dwFlags",ANone);
      ("pptiCoClass",ANone);
      ("pdwTIFlags",ANone);
      ("pcdispidReserved",ANone);
      ("piidPrimary",ANone);
      ("piidSource",ANone);
    ],MANone);
  ]);
  ("IQuickActivateVtbl",IAManual, "IUnknownVtbl", [
    ("QuickActivate",[
      ("This",AThis);
      ("pQaContainer",ANone);
      ("pQaControl",ANone);
    ],MANone);
    ("SetContentExtent",[
      ("This",AThis);
      ("pSizel",ANone);
    ],MANone);
    ("GetContentExtent",[
      ("This",AThis);
      ("pSizel",ANone);
    ],MANone);
  ]);
  ("ISimpleFrameSiteVtbl",IAManual, "IUnknownVtbl", [
    ("PreMessageFilter",[
      ("This",AThis);
      ("hWnd",ANone);
      ("msg",ANone);
      ("wp",ANone);
      ("lp",ANone);
      ("plResult",ANone);
      ("pdwCookie",ANone);
    ],MANone);
    ("PostMessageFilter",[
      ("This",AThis);
      ("hWnd",ANone);
      ("msg",ANone);
      ("wp",ANone);
      ("lp",ANone);
      ("plResult",ANone);
      ("dwCookie",ANone);
    ],MANone);
  ]);
  ("ISpecifyPropertyPagesVtbl",IAManual, "IUnknownVtbl", [
    ("GetPages",[
      ("This",AThis);
      ("pPages",ANone);
    ],MANone);
  ]);
  ("IViewObjectExVtbl",IAManual, "IViewObject2Vtbl", [
    ("GetRect",[
      ("This",AThis);
      ("dwAspect",ANone);
      ("pRect",ANone);
    ],MANone);
    ("GetViewStatus",[
      ("This",AThis);
      ("pdwStatus",ANone);
    ],MANone);
    ("QueryHitPoint",[
      ("This",AThis);
      ("dwAspect",ANone);
      ("pRectBounds",ANone);
      ("ptlLoc",ANone);
      ("lCloseHint",ANone);
      ("pHitResult",ANone);
    ],MANone);
    ("QueryHitRect",[
      ("This",AThis);
      ("dwAspect",ANone);
      ("pRectBounds",ANone);
      ("pRectLoc",ANone);
      ("lCloseHint",ANone);
      ("pHitResult",ANone);
    ],MANone);
    ("GetNaturalExtent",[
      ("This",AThis);
      ("dwAspect",ANone);
      ("lindex",ANone);
      ("ptd",ANone);
      ("hicTargetDev",ANone);
      ("pExtentInfo",ANone);
      ("pSizel",ANone);
    ],MANone);
  ]);
  ]
