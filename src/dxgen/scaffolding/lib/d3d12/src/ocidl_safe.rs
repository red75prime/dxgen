// This file is autogenerated

use utils::*;

pub trait TConnectionPointContainer: TUnknown {
  //  Method EnumConnectionPoints
  //  Error: ppEnum parameter: ANone annotation cannot be used with double indirection
  //  Method FindConnectionPoint
  //  Error: ppCP parameter: ANone annotation cannot be used with double indirection
  
}

impl TUnknown for ConnectionPointContainer {
  fn new(ptr: *mut IUnknown) -> Self {
    ConnectionPointContainer(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for ConnectionPointContainer {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for ConnectionPointContainer {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TConnectionPointContainer for ConnectionPointContainer {}

pub struct ConnectionPointContainer(*mut IConnectionPointContainer);

impl HasIID for ConnectionPointContainer {
  fn iid() -> REFGUID { &IID_IConnectionPointContainer }
}

pub trait TConnectionPoint: TUnknown {
  //  Method GetConnectionInterface
  
  #[allow(non_snake_case)]
  fn get_connection_interface(&self, i_i_d: &mut IID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IConnectionPoint)).GetConnectionInterface(i_i_d) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetConnectionPointContainer
  //  Error: ppCPC parameter: ANone annotation cannot be used with double indirection
  //  Method Advise
  
  #[allow(non_snake_case)]
  fn advise(&self, unk_sink: &mut IUnknown, pdwCookie: &mut DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IConnectionPoint)).Advise(unk_sink, pdwCookie) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Unadvise
  
  #[allow(non_snake_case)]
  fn unadvise(&self, cookie: DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IConnectionPoint)).Unadvise(cookie) };
    hr2ret(_hr,_hr)
  }
  
  //  Method EnumConnections
  //  Error: ppEnum parameter: ANone annotation cannot be used with double indirection
  
}

impl TUnknown for ConnectionPoint {
  fn new(ptr: *mut IUnknown) -> Self {
    ConnectionPoint(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for ConnectionPoint {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for ConnectionPoint {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TConnectionPoint for ConnectionPoint {}

pub struct ConnectionPoint(*mut IConnectionPoint);

impl HasIID for ConnectionPoint {
  fn iid() -> REFGUID { &IID_IConnectionPoint }
}

pub trait TEnumConnectionPoints: TUnknown {
  //  Method Next
  
  #[allow(non_snake_case)]
  fn next(&self, cConnections: ULONG, c_p: &mut LPCONNECTIONPOINT, pcFetched: &mut ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnectionPoints)).Next(cConnections, c_p, pcFetched) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Skip
  
  #[allow(non_snake_case)]
  fn skip(&self, cConnections: ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnectionPoints)).Skip(cConnections) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Reset
  
  #[allow(non_snake_case)]
  fn reset(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnectionPoints)).Reset() };
    hr2ret(_hr,_hr)
  }
  
  //  Method Clone
  //  Error: ppEnum parameter: ANone annotation cannot be used with double indirection
  
}

impl TUnknown for EnumConnectionPoints {
  fn new(ptr: *mut IUnknown) -> Self {
    EnumConnectionPoints(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for EnumConnectionPoints {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for EnumConnectionPoints {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TEnumConnectionPoints for EnumConnectionPoints {}

pub struct EnumConnectionPoints(*mut IEnumConnectionPoints);

impl HasIID for EnumConnectionPoints {
  fn iid() -> REFGUID { &IID_IEnumConnectionPoints }
}

pub trait TEnumConnections: TUnknown {
  //  Method Next
  
  #[allow(non_snake_case)]
  fn next(&self, cConnections: ULONG, rgcd: LPCONNECTDATA, pcFetched: &mut ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnections)).Next(cConnections, rgcd, pcFetched) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Skip
  
  #[allow(non_snake_case)]
  fn skip(&self, cConnections: ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnections)).Skip(cConnections) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Reset
  
  #[allow(non_snake_case)]
  fn reset(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumConnections)).Reset() };
    hr2ret(_hr,_hr)
  }
  
  //  Method Clone
  //  Error: ppEnum parameter: ANone annotation cannot be used with double indirection
  
}

impl TUnknown for EnumConnections {
  fn new(ptr: *mut IUnknown) -> Self {
    EnumConnections(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for EnumConnections {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for EnumConnections {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TEnumConnections for EnumConnections {}

pub struct EnumConnections(*mut IEnumConnections);

impl HasIID for EnumConnections {
  fn iid() -> REFGUID { &IID_IEnumConnections }
}

pub trait TEnumOleUndoUnits: TUnknown {
  //  Method Next
  //  Error: rgElt parameter: ANone annotation cannot be used with double indirection
  //  Method Skip
  
  #[allow(non_snake_case)]
  fn skip(&self, cElt: ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumOleUndoUnits)).Skip(cElt) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Reset
  
  #[allow(non_snake_case)]
  fn reset(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IEnumOleUndoUnits)).Reset() };
    hr2ret(_hr,_hr)
  }
  
  //  Method Clone
  //  Error: ppEnum parameter: ANone annotation cannot be used with double indirection
  
}

impl TUnknown for EnumOleUndoUnits {
  fn new(ptr: *mut IUnknown) -> Self {
    EnumOleUndoUnits(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for EnumOleUndoUnits {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for EnumOleUndoUnits {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TEnumOleUndoUnits for EnumOleUndoUnits {}

pub struct EnumOleUndoUnits(*mut IEnumOleUndoUnits);

impl HasIID for EnumOleUndoUnits {
  fn iid() -> REFGUID { &IID_IEnumOleUndoUnits }
}

pub trait TFont: TUnknown {
  //  Method get_Name
  
  #[allow(non_snake_case)]
  fn get_Name(&self, name: &mut BSTR) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Name(name) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Name
  
  #[allow(non_snake_case)]
  fn put_Name(&self, name: BSTR) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Name(name) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Size
  
  #[allow(non_snake_case)]
  fn get_Size(&self, size: &mut CY) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Size(size) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Size
  
  #[allow(non_snake_case)]
  fn put_Size(&self, size: CY) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Size(size) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Bold
  
  #[allow(non_snake_case)]
  fn get_Bold(&self, bold: &mut BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Bold(bold) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Bold
  
  #[allow(non_snake_case)]
  fn put_Bold(&self, bold: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Bold(bold) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Italic
  
  #[allow(non_snake_case)]
  fn get_Italic(&self, italic: &mut BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Italic(italic) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Italic
  
  #[allow(non_snake_case)]
  fn put_Italic(&self, italic: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Italic(italic) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Underline
  
  #[allow(non_snake_case)]
  fn get_Underline(&self, underline: &mut BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Underline(underline) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Underline
  
  #[allow(non_snake_case)]
  fn put_Underline(&self, underline: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Underline(underline) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Strikethrough
  
  #[allow(non_snake_case)]
  fn get_Strikethrough(&self, strikethrough: &mut BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Strikethrough(strikethrough) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Strikethrough
  
  #[allow(non_snake_case)]
  fn put_Strikethrough(&self, strikethrough: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Strikethrough(strikethrough) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Weight
  
  #[allow(non_snake_case)]
  fn get_Weight(&self, weight: &mut SHORT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Weight(weight) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Weight
  
  #[allow(non_snake_case)]
  fn put_Weight(&self, weight: SHORT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Weight(weight) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_Charset
  
  #[allow(non_snake_case)]
  fn get_Charset(&self, charset: &mut SHORT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_Charset(charset) };
    hr2ret(_hr,_hr)
  }
  
  //  Method put_Charset
  
  #[allow(non_snake_case)]
  fn put_Charset(&self, charset: SHORT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).put_Charset(charset) };
    hr2ret(_hr,_hr)
  }
  
  //  Method get_hFont
  
  #[allow(non_snake_case)]
  fn get_hFont(&self, phFont: &mut HFONT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).get_hFont(phFont) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Clone
  //  Error: ppFont parameter: ANone annotation cannot be used with double indirection
  //  Method IsEqual
  
  #[allow(non_snake_case)]
  fn is_equal(&self, font_other: &mut IFont) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).IsEqual(font_other) };
    hr2ret(_hr,_hr)
  }
  
  //  Method SetRatio
  
  #[allow(non_snake_case)]
  fn set_ratio(&self, cyLogical: LONG, cyHimetric: LONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).SetRatio(cyLogical, cyHimetric) };
    hr2ret(_hr,_hr)
  }
  
  //  Method QueryTextMetrics
  
  #[allow(non_snake_case)]
  fn query_text_metrics(&self, t_m: &mut TEXTMETRICOLE) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).QueryTextMetrics(t_m) };
    hr2ret(_hr,_hr)
  }
  
  //  Method AddRefHfont
  
  #[allow(non_snake_case)]
  fn add_ref_hfont(&self, hFont: HFONT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).AddRefHfont(hFont) };
    hr2ret(_hr,_hr)
  }
  
  //  Method ReleaseHfont
  
  #[allow(non_snake_case)]
  fn release_hfont(&self, hFont: HFONT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).ReleaseHfont(hFont) };
    hr2ret(_hr,_hr)
  }
  
  //  Method SetHdc
  
  #[allow(non_snake_case)]
  fn set_hdc(&self, hDC: HDC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IFont)).SetHdc(hDC) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for Font {
  fn new(ptr: *mut IUnknown) -> Self {
    Font(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for Font {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for Font {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TFont for Font {}

pub struct Font(*mut IFont);

impl HasIID for Font {
  fn iid() -> REFGUID { &IID_IFont }
}

pub trait TObjectWithSite: TUnknown {
  //  Method SetSite
  
  #[allow(non_snake_case)]
  fn set_site(&self, unk_site: &mut IUnknown) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IObjectWithSite)).SetSite(unk_site) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetSite
  //  Error: ppvSite parameter: ANone annotation cannot be applied to void pointer
  
}

impl TUnknown for ObjectWithSite {
  fn new(ptr: *mut IUnknown) -> Self {
    ObjectWithSite(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for ObjectWithSite {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for ObjectWithSite {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TObjectWithSite for ObjectWithSite {}

pub struct ObjectWithSite(*mut IObjectWithSite);

impl HasIID for ObjectWithSite {
  fn iid() -> REFGUID { &IID_IObjectWithSite }
}

pub trait TOleControlSite: TUnknown {
  //  Method OnControlInfoChanged
  
  #[allow(non_snake_case)]
  fn on_control_info_changed(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).OnControlInfoChanged() };
    hr2ret(_hr,_hr)
  }
  
  //  Method LockInPlaceActive
  
  #[allow(non_snake_case)]
  fn lock_in_place_active(&self, fLock: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).LockInPlaceActive(fLock) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetExtendedControl
  //  Error: ppDisp parameter: ANone annotation cannot be used with double indirection
  //  Method TransformCoords
  
  #[allow(non_snake_case)]
  fn transform_coords(&self, ptl_himetric: &mut POINTL, ptf_container: &mut POINTF, flags: DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).TransformCoords(ptl_himetric, ptf_container, flags) };
    hr2ret(_hr,_hr)
  }
  
  //  Method TranslateAcceleratorA
  
  #[allow(non_snake_case)]
  fn translate_accelerator_a(&self, msg: &mut MSG, grfModifiers: DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).TranslateAcceleratorA(msg, grfModifiers) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnFocus
  
  #[allow(non_snake_case)]
  fn on_focus(&self, fGotFocus: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).OnFocus(fGotFocus) };
    hr2ret(_hr,_hr)
  }
  
  //  Method ShowPropertyFrame
  
  #[allow(non_snake_case)]
  fn show_property_frame(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControlSite)).ShowPropertyFrame() };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for OleControlSite {
  fn new(ptr: *mut IUnknown) -> Self {
    OleControlSite(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for OleControlSite {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for OleControlSite {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TOleControlSite for OleControlSite {}

pub struct OleControlSite(*mut IOleControlSite);

impl HasIID for OleControlSite {
  fn iid() -> REFGUID { &IID_IOleControlSite }
}

pub trait TOleControl: TUnknown {
  //  Method GetControlInfo
  
  #[allow(non_snake_case)]
  fn get_control_info(&self, c_i: &mut CONTROLINFO) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControl)).GetControlInfo(c_i) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnMnemonic
  
  #[allow(non_snake_case)]
  fn on_mnemonic(&self, msg: &mut MSG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControl)).OnMnemonic(msg) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnAmbientPropertyChange
  
  #[allow(non_snake_case)]
  fn on_ambient_property_change(&self, dispID: DISPID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControl)).OnAmbientPropertyChange(dispID) };
    hr2ret(_hr,_hr)
  }
  
  //  Method FreezeEvents
  
  #[allow(non_snake_case)]
  fn freeze_events(&self, bFreeze: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleControl)).FreezeEvents(bFreeze) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for OleControl {
  fn new(ptr: *mut IUnknown) -> Self {
    OleControl(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for OleControl {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for OleControl {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TOleControl for OleControl {}

pub struct OleControl(*mut IOleControl);

impl HasIID for OleControl {
  fn iid() -> REFGUID { &IID_IOleControl }
}

pub trait TOleUndoUnit: TUnknown {
  //  Method Do
  
  #[allow(non_snake_case)]
  fn do(&self, undo_manager: &mut IOleUndoManager) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleUndoUnit)).Do(undo_manager) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetDescription
  
  #[allow(non_snake_case)]
  fn get_description(&self, bstr: &mut BSTR) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleUndoUnit)).GetDescription(bstr) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetUnitType
  
  #[allow(non_snake_case)]
  fn get_unit_type(&self, clsid: &mut CLSID, plID: &mut LONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleUndoUnit)).GetUnitType(clsid, plID) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnNextAdd
  
  #[allow(non_snake_case)]
  fn on_next_add(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IOleUndoUnit)).OnNextAdd() };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for OleUndoUnit {
  fn new(ptr: *mut IUnknown) -> Self {
    OleUndoUnit(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for OleUndoUnit {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for OleUndoUnit {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TOleUndoUnit for OleUndoUnit {}

pub struct OleUndoUnit(*mut IOleUndoUnit);

impl HasIID for OleUndoUnit {
  fn iid() -> REFGUID { &IID_IOleUndoUnit }
}

pub trait TPerPropertyBrowsing: TUnknown {
  //  Method GetDisplayString
  
  #[allow(non_snake_case)]
  fn get_display_string(&self, dispID: DISPID, bstr: &mut BSTR) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPerPropertyBrowsing)).GetDisplayString(dispID, bstr) };
    hr2ret(_hr,_hr)
  }
  
  //  Method MapPropertyToPage
  
  #[allow(non_snake_case)]
  fn map_property_to_page(&self, dispID: DISPID, clsid: &mut CLSID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPerPropertyBrowsing)).MapPropertyToPage(dispID, clsid) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPredefinedStrings
  
  #[allow(non_snake_case)]
  fn get_predefined_strings(&self, dispID: DISPID, ca_strings_out: &mut CALPOLESTR, ca_cookies_out: &mut CADWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPerPropertyBrowsing)).GetPredefinedStrings(dispID, ca_strings_out, ca_cookies_out) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPredefinedValue
  
  #[allow(non_snake_case)]
  fn get_predefined_value(&self, dispID: DISPID, cookie: DWORD, var_out: &mut VARIANT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPerPropertyBrowsing)).GetPredefinedValue(dispID, cookie, var_out) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PerPropertyBrowsing {
  fn new(ptr: *mut IUnknown) -> Self {
    PerPropertyBrowsing(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PerPropertyBrowsing {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PerPropertyBrowsing {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPerPropertyBrowsing for PerPropertyBrowsing {}

pub struct PerPropertyBrowsing(*mut IPerPropertyBrowsing);

impl HasIID for PerPropertyBrowsing {
  fn iid() -> REFGUID { &IID_IPerPropertyBrowsing }
}

pub trait TPointerInactive: TUnknown {
  //  Method GetActivationPolicy
  
  #[allow(non_snake_case)]
  fn get_activation_policy(&self, pdwPolicy: &mut DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPointerInactive)).GetActivationPolicy(pdwPolicy) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnInactiveMouseMove
  
  #[allow(non_snake_case)]
  fn on_inactive_mouse_move(&self, rect_bounds: LPCRECT, x: LONG, y: LONG, grfKeyState: DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPointerInactive)).OnInactiveMouseMove(rect_bounds, x, y, grfKeyState) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnInactiveSetCursor
  
  #[allow(non_snake_case)]
  fn on_inactive_set_cursor(&self, rect_bounds: LPCRECT, x: LONG, y: LONG, mouse_msg: DWORD, fSetAlways: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPointerInactive)).OnInactiveSetCursor(rect_bounds, x, y, mouse_msg, fSetAlways) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PointerInactive {
  fn new(ptr: *mut IUnknown) -> Self {
    PointerInactive(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PointerInactive {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PointerInactive {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPointerInactive for PointerInactive {}

pub struct PointerInactive(*mut IPointerInactive);

impl HasIID for PointerInactive {
  fn iid() -> REFGUID { &IID_IPointerInactive }
}

pub trait TPropertyBag2: TUnknown {
  //  Method Read
  
  #[allow(non_snake_case)]
  fn read(&self, cProperties: ULONG, prop_bag: &mut PROPBAG2, err_log: &mut IErrorLog, pvarValue: &mut VARIANT, phrError: &mut HRESULT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyBag2)).Read(cProperties, prop_bag, err_log, pvarValue, phrError) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Write
  
  #[allow(non_snake_case)]
  fn write(&self, cProperties: ULONG, prop_bag: &mut PROPBAG2, pvarValue: &mut VARIANT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyBag2)).Write(cProperties, prop_bag, pvarValue) };
    hr2ret(_hr,_hr)
  }
  
  //  Method CountProperties
  
  #[allow(non_snake_case)]
  fn count_properties(&self, pcProperties: &mut ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyBag2)).CountProperties(pcProperties) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPropertyInfo
  
  #[allow(non_snake_case)]
  fn get_property_info(&self, iProperty: ULONG, cProperties: ULONG, prop_bag: &mut PROPBAG2, pcProperties: &mut ULONG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyBag2)).GetPropertyInfo(iProperty, cProperties, prop_bag, pcProperties) };
    hr2ret(_hr,_hr)
  }
  
  //  Method LoadObject
  
  #[allow(non_snake_case)]
  fn load_object(&self, pstrName: LPCOLESTR, hint: DWORD, unk_object: &mut IUnknown, err_log: &mut IErrorLog) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyBag2)).LoadObject(pstrName, hint, unk_object, err_log) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PropertyBag2 {
  fn new(ptr: *mut IUnknown) -> Self {
    PropertyBag2(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PropertyBag2 {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PropertyBag2 {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPropertyBag2 for PropertyBag2 {}

pub struct PropertyBag2(*mut IPropertyBag2);

impl HasIID for PropertyBag2 {
  fn iid() -> REFGUID { &IID_IPropertyBag2 }
}

pub trait TPropertyNotifySink: TUnknown {
  //  Method OnChanged
  
  #[allow(non_snake_case)]
  fn on_changed(&self, dispID: DISPID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyNotifySink)).OnChanged(dispID) };
    hr2ret(_hr,_hr)
  }
  
  //  Method OnRequestEdit
  
  #[allow(non_snake_case)]
  fn on_request_edit(&self, dispID: DISPID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyNotifySink)).OnRequestEdit(dispID) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PropertyNotifySink {
  fn new(ptr: *mut IUnknown) -> Self {
    PropertyNotifySink(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PropertyNotifySink {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PropertyNotifySink {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPropertyNotifySink for PropertyNotifySink {}

pub struct PropertyNotifySink(*mut IPropertyNotifySink);

impl HasIID for PropertyNotifySink {
  fn iid() -> REFGUID { &IID_IPropertyNotifySink }
}

pub trait TPropertyPageSite: TUnknown {
  //  Method OnStatusChange
  
  #[allow(non_snake_case)]
  fn on_status_change(&self, flags: DWORD) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPageSite)).OnStatusChange(flags) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetLocaleID
  
  #[allow(non_snake_case)]
  fn get_locale_i_d(&self, locale_i_d: &mut LCID) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPageSite)).GetLocaleID(locale_i_d) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPageContainer
  //  Error: ppUnk parameter: ANone annotation cannot be used with double indirection
  //  Method TranslateAcceleratorA
  
  #[allow(non_snake_case)]
  fn translate_accelerator_a(&self, msg: &mut MSG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPageSite)).TranslateAcceleratorA(msg) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PropertyPageSite {
  fn new(ptr: *mut IUnknown) -> Self {
    PropertyPageSite(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PropertyPageSite {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PropertyPageSite {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPropertyPageSite for PropertyPageSite {}

pub struct PropertyPageSite(*mut IPropertyPageSite);

impl HasIID for PropertyPageSite {
  fn iid() -> REFGUID { &IID_IPropertyPageSite }
}

pub trait TPropertyPage: TUnknown {
  //  Method SetPageSite
  
  #[allow(non_snake_case)]
  fn set_page_site(&self, page_site: &mut IPropertyPageSite) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).SetPageSite(page_site) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Activate
  
  #[allow(non_snake_case)]
  fn activate(&self, hWndParent: HWND, rect: LPCRECT, bModal: BOOL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Activate(hWndParent, rect, bModal) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Deactivate
  
  #[allow(non_snake_case)]
  fn deactivate(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Deactivate() };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPageInfo
  
  #[allow(non_snake_case)]
  fn get_page_info(&self, page_info: &mut PROPPAGEINFO) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).GetPageInfo(page_info) };
    hr2ret(_hr,_hr)
  }
  
  //  Method SetObjects
  //  Error: ppUnk parameter: ANone annotation cannot be used with double indirection
  //  Method Show
  
  #[allow(non_snake_case)]
  fn show(&self, nCmdShow: UINT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Show(nCmdShow) };
    hr2ret(_hr,_hr)
  }
  
  //  Method Move
  
  #[allow(non_snake_case)]
  fn move(&self, rect: LPCRECT) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Move(rect) };
    hr2ret(_hr,_hr)
  }
  
  //  Method IsPageDirty
  
  #[allow(non_snake_case)]
  fn is_page_dirty(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).IsPageDirty() };
    hr2ret(_hr,_hr)
  }
  
  //  Method Apply
  
  #[allow(non_snake_case)]
  fn apply(&self) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Apply() };
    hr2ret(_hr,_hr)
  }
  
  //  Method Help
  
  #[allow(non_snake_case)]
  fn help(&self, pszHelpDir: LPCOLESTR) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).Help(pszHelpDir) };
    hr2ret(_hr,_hr)
  }
  
  //  Method TranslateAcceleratorA
  
  #[allow(non_snake_case)]
  fn translate_accelerator_a(&self, msg: &mut MSG) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IPropertyPage)).TranslateAcceleratorA(msg) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for PropertyPage {
  fn new(ptr: *mut IUnknown) -> Self {
    PropertyPage(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for PropertyPage {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for PropertyPage {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TPropertyPage for PropertyPage {}

pub struct PropertyPage(*mut IPropertyPage);

impl HasIID for PropertyPage {
  fn iid() -> REFGUID { &IID_IPropertyPage }
}

