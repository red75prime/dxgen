use d3d12_safe::*;
// This file is autogenerated

use utils::*;


pub struct D3D12Debug(*mut ID3D12Debug);

impl HasIID for D3D12Debug {
  fn iid() -> REFGUID { &IID_ID3D12Debug }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12Debug(pp_vtbl as *mut _ as *mut ID3D12Debug) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for D3D12Debug {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for D3D12Debug {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl D3D12Debug {
  //  Method EnableDebugLayer
  
  pub fn enable_debug_layer(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12Debug)).EnableDebugLayer() };
    ()
  }
  
  
}

pub struct D3D12InfoQueue(*mut ID3D12InfoQueue);

impl HasIID for D3D12InfoQueue {
  fn iid() -> REFGUID { &IID_ID3D12InfoQueue }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12InfoQueue(pp_vtbl as *mut _ as *mut ID3D12InfoQueue) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for D3D12InfoQueue {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for D3D12InfoQueue {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl D3D12InfoQueue {
  //  Method SetMessageCountLimit
  
  pub fn set_message_count_limit(&self, message_count_limit: UINT64) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).SetMessageCountLimit(message_count_limit) };
    hr2ret(hr,hr)
  }
  
  //  Method ClearStoredMessages
  
  pub fn clear_stored_messages(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).ClearStoredMessages() };
    ()
  }
  
  //  Method GetMessage
  
  pub fn get_message(&self, message_index: UINT64, message: Option<&mut D3D12_MESSAGE>, message_byte_length: &mut SIZE_T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetMessage(message_index, opt_as_mut_ptr(&message), message_byte_length) };
    hr2ret(hr,hr)
  }
  
  //  Method GetNumMessagesAllowedByStorageFilter
  
  pub fn get_num_messages_allowed_by_storage_filter(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetNumMessagesAllowedByStorageFilter() };
    hr
  }
  
  //  Method GetNumMessagesDeniedByStorageFilter
  
  pub fn get_num_messages_denied_by_storage_filter(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetNumMessagesDeniedByStorageFilter() };
    hr
  }
  
  //  Method GetNumStoredMessages
  
  pub fn get_num_stored_messages(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetNumStoredMessages() };
    hr
  }
  
  //  Method GetNumStoredMessagesAllowedByRetrievalFilter
  
  pub fn get_num_stored_messages_allowed_by_retrieval_filter(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetNumStoredMessagesAllowedByRetrievalFilter() };
    hr
  }
  
  //  Method GetNumMessagesDiscardedByMessageCountLimit
  
  pub fn get_num_messages_discarded_by_message_count_limit(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetNumMessagesDiscardedByMessageCountLimit() };
    hr
  }
  
  //  Method GetMessageCountLimit
  
  pub fn get_message_count_limit(&self) -> UINT64 {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetMessageCountLimit() };
    hr
  }
  
  //  Method AddStorageFilterEntries
  
  pub fn add_storage_filter_entries(&self, filter: &mut D3D12_INFO_QUEUE_FILTER) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).AddStorageFilterEntries(filter) };
    hr2ret(hr,hr)
  }
  
  //  Method GetStorageFilter
  
  pub fn get_storage_filter(&self, filter: &mut D3D12_INFO_QUEUE_FILTER, filter_byte_length: &mut SIZE_T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetStorageFilter(filter, filter_byte_length) };
    hr2ret(hr,hr)
  }
  
  //  Method ClearStorageFilter
  
  pub fn clear_storage_filter(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).ClearStorageFilter() };
    ()
  }
  
  //  Method PushEmptyStorageFilter
  
  pub fn push_empty_storage_filter(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushEmptyStorageFilter() };
    hr2ret(hr,hr)
  }
  
  //  Method PushCopyOfStorageFilter
  
  pub fn push_copy_of_storage_filter(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushCopyOfStorageFilter() };
    hr2ret(hr,hr)
  }
  
  //  Method PushStorageFilter
  
  pub fn push_storage_filter(&self, filter: &mut D3D12_INFO_QUEUE_FILTER) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushStorageFilter(filter) };
    hr2ret(hr,hr)
  }
  
  //  Method PopStorageFilter
  
  pub fn pop_storage_filter(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PopStorageFilter() };
    ()
  }
  
  //  Method GetStorageFilterStackSize
  
  pub fn get_storage_filter_stack_size(&self) -> UINT {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetStorageFilterStackSize() };
    hr
  }
  
  //  Method AddRetrievalFilterEntries
  
  pub fn add_retrieval_filter_entries(&self, filter: &mut D3D12_INFO_QUEUE_FILTER) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).AddRetrievalFilterEntries(filter) };
    hr2ret(hr,hr)
  }
  
  //  Method GetRetrievalFilter
  
  pub fn get_retrieval_filter(&self, filter: &mut D3D12_INFO_QUEUE_FILTER, filter_byte_length: &mut SIZE_T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetRetrievalFilter(filter, filter_byte_length) };
    hr2ret(hr,hr)
  }
  
  //  Method ClearRetrievalFilter
  
  pub fn clear_retrieval_filter(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).ClearRetrievalFilter() };
    ()
  }
  
  //  Method PushEmptyRetrievalFilter
  
  pub fn push_empty_retrieval_filter(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushEmptyRetrievalFilter() };
    hr2ret(hr,hr)
  }
  
  //  Method PushCopyOfRetrievalFilter
  
  pub fn push_copy_of_retrieval_filter(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushCopyOfRetrievalFilter() };
    hr2ret(hr,hr)
  }
  
  //  Method PushRetrievalFilter
  
  pub fn push_retrieval_filter(&self, filter: &mut D3D12_INFO_QUEUE_FILTER) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PushRetrievalFilter(filter) };
    hr2ret(hr,hr)
  }
  
  //  Method PopRetrievalFilter
  
  pub fn pop_retrieval_filter(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).PopRetrievalFilter() };
    ()
  }
  
  //  Method GetRetrievalFilterStackSize
  
  pub fn get_retrieval_filter_stack_size(&self) -> UINT {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetRetrievalFilterStackSize() };
    hr
  }
  
  //  Method AddMessage
  
  pub fn add_message(&self, category: D3D12_MESSAGE_CATEGORY, severity: D3D12_MESSAGE_SEVERITY, i_d: D3D12_MESSAGE_ID, description: LPCSTR) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).AddMessage(category, severity, i_d, description) };
    hr2ret(hr,hr)
  }
  
  //  Method AddApplicationMessage
  
  pub fn add_application_message(&self, severity: D3D12_MESSAGE_SEVERITY, description: LPCSTR) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).AddApplicationMessage(severity, description) };
    hr2ret(hr,hr)
  }
  
  //  Method SetBreakOnCategory
  
  pub fn set_break_on_category(&self, category: D3D12_MESSAGE_CATEGORY, bEnable: BOOL) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).SetBreakOnCategory(category, bEnable) };
    hr2ret(hr,hr)
  }
  
  //  Method SetBreakOnSeverity
  
  pub fn set_break_on_severity(&self, severity: D3D12_MESSAGE_SEVERITY, bEnable: BOOL) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).SetBreakOnSeverity(severity, bEnable) };
    hr2ret(hr,hr)
  }
  
  //  Method SetBreakOnID
  
  pub fn set_break_on_id(&self, i_d: D3D12_MESSAGE_ID, bEnable: BOOL) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).SetBreakOnID(i_d, bEnable) };
    hr2ret(hr,hr)
  }
  
  //  Method GetBreakOnCategory
  
  pub fn get_break_on_category(&self, category: D3D12_MESSAGE_CATEGORY) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetBreakOnCategory(category) };
    hr
  }
  
  //  Method GetBreakOnSeverity
  
  pub fn get_break_on_severity(&self, severity: D3D12_MESSAGE_SEVERITY) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetBreakOnSeverity(severity) };
    hr
  }
  
  //  Method GetBreakOnID
  
  pub fn get_break_on_id(&self, i_d: D3D12_MESSAGE_ID) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetBreakOnID(i_d) };
    hr
  }
  
  //  Method SetMuteDebugOutput
  
  pub fn set_mute_debug_output(&self, bMute: BOOL) -> () {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).SetMuteDebugOutput(bMute) };
    ()
  }
  
  //  Method GetMuteDebugOutput
  
  pub fn get_mute_debug_output(&self) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut ID3D12InfoQueue)).GetMuteDebugOutput() };
    hr
  }
  
  
}

