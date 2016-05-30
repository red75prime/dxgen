// This file is autogenerated

use utils::*;

#[derive(Clone)]

pub struct D3D12FunctionParameterReflection(*mut ID3D12FunctionParameterReflection);

impl HasIID for D3D12FunctionParameterReflection {
  fn iid() -> REFGUID { &IID_ID3D12FunctionParameterReflection }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12FunctionParameterReflection(pp_vtbl as *mut _ as *mut ID3D12FunctionParameterReflection) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}


impl D3D12FunctionParameterReflection {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_PARAMETER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionParameterReflection)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  
}
#[derive(Clone)]

pub struct D3D12FunctionReflection(*mut ID3D12FunctionReflection);

impl HasIID for D3D12FunctionReflection {
  fn iid() -> REFGUID { &IID_ID3D12FunctionReflection }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12FunctionReflection(pp_vtbl as *mut _ as *mut ID3D12FunctionReflection) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}


impl D3D12FunctionReflection {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_FUNCTION_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetConstantBufferByIndex
  
  #[allow(non_snake_case)]
  pub fn get_constant_buffer_by_index(&self, buffer_index: UINT) -> D3D12ShaderReflectionConstantBuffer {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetConstantBufferByIndex(buffer_index) };
    D3D12ShaderReflectionConstantBuffer::new(_hr as *mut _)
  }
  
  //  Method GetConstantBufferByName
  
  #[allow(non_snake_case)]
  pub fn get_constant_buffer_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionConstantBuffer {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetConstantBufferByName(name) };
    D3D12ShaderReflectionConstantBuffer::new(_hr as *mut _)
  }
  
  //  Method GetResourceBindingDesc
  
  #[allow(non_snake_case)]
  pub fn get_resource_binding_desc(&self, resource_index: UINT, desc: &mut D3D12_SHADER_INPUT_BIND_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetResourceBindingDesc(resource_index, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetVariableByName
  
  #[allow(non_snake_case)]
  pub fn get_variable_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionVariable {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetVariableByName(name) };
    D3D12ShaderReflectionVariable::new(_hr as *mut _)
  }
  
  //  Method GetResourceBindingDescByName
  
  #[allow(non_snake_case)]
  pub fn get_resource_binding_desc_by_name(&self, name: LPCSTR, desc: &mut D3D12_SHADER_INPUT_BIND_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetResourceBindingDescByName(name, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetFunctionParameter
  
  #[allow(non_snake_case)]
  pub fn get_function_parameter(&self, parameter_index: INT) -> D3D12FunctionParameterReflection {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12FunctionReflection)).GetFunctionParameter(parameter_index) };
    D3D12FunctionParameterReflection::new(_hr as *mut _)
  }
  
  
}

pub struct D3D12LibraryReflection(*mut ID3D12LibraryReflection);

impl HasIID for D3D12LibraryReflection {
  fn iid() -> REFGUID { &IID_ID3D12LibraryReflection }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12LibraryReflection(pp_vtbl as *mut _ as *mut ID3D12LibraryReflection) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}
impl Drop for D3D12LibraryReflection {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for D3D12LibraryReflection {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}



impl D3D12LibraryReflection {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_LIBRARY_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12LibraryReflection)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetFunctionByIndex
  
  #[allow(non_snake_case)]
  pub fn get_function_by_index(&self, function_index: INT) -> D3D12FunctionReflection {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12LibraryReflection)).GetFunctionByIndex(function_index) };
    D3D12FunctionReflection::new(_hr as *mut _)
  }
  
  
}
#[derive(Clone)]

pub struct D3D12ShaderReflectionConstantBuffer(*mut ID3D12ShaderReflectionConstantBuffer);

impl HasIID for D3D12ShaderReflectionConstantBuffer {
  fn iid() -> REFGUID { &IID_ID3D12ShaderReflectionConstantBuffer }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12ShaderReflectionConstantBuffer(pp_vtbl as *mut _ as *mut ID3D12ShaderReflectionConstantBuffer) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}


impl D3D12ShaderReflectionConstantBuffer {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_SHADER_BUFFER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionConstantBuffer)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetVariableByIndex
  
  #[allow(non_snake_case)]
  pub fn get_variable_by_index(&self, index: UINT) -> D3D12ShaderReflectionVariable {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionConstantBuffer)).GetVariableByIndex(index) };
    D3D12ShaderReflectionVariable::new(_hr as *mut _)
  }
  
  //  Method GetVariableByName
  
  #[allow(non_snake_case)]
  pub fn get_variable_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionVariable {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionConstantBuffer)).GetVariableByName(name) };
    D3D12ShaderReflectionVariable::new(_hr as *mut _)
  }
  
  
}
#[derive(Clone)]

pub struct D3D12ShaderReflectionType(*mut ID3D12ShaderReflectionType);

impl HasIID for D3D12ShaderReflectionType {
  fn iid() -> REFGUID { &IID_ID3D12ShaderReflectionType }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12ShaderReflectionType(pp_vtbl as *mut _ as *mut ID3D12ShaderReflectionType) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}


impl D3D12ShaderReflectionType {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_SHADER_TYPE_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetMemberTypeByIndex
  
  #[allow(non_snake_case)]
  pub fn get_member_type_by_index(&self, index: UINT) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetMemberTypeByIndex(index) };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method GetMemberTypeByName
  
  #[allow(non_snake_case)]
  pub fn get_member_type_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetMemberTypeByName(name) };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method GetMemberTypeName
  
  #[allow(non_snake_case)]
  pub fn get_member_type_name(&self, index: UINT) -> LPCSTR {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetMemberTypeName(index) };
    _hr
  }
  
  //  Method IsEqual
  
  #[allow(non_snake_case)]
  pub fn is_equal(&self, type_: &mut ID3D12ShaderReflectionType) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).IsEqual(type_) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetSubType
  
  #[allow(non_snake_case)]
  pub fn get_sub_type(&self) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetSubType() };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method GetBaseClass
  
  #[allow(non_snake_case)]
  pub fn get_base_class(&self) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetBaseClass() };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method GetNumInterfaces
  
  #[allow(non_snake_case)]
  pub fn get_num_interfaces(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetNumInterfaces() };
    _hr
  }
  
  //  Method GetInterfaceByIndex
  
  #[allow(non_snake_case)]
  pub fn get_interface_by_index(&self, uIndex: UINT) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).GetInterfaceByIndex(uIndex) };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method IsOfType
  
  #[allow(non_snake_case)]
  pub fn is_of_type(&self, type_: &mut ID3D12ShaderReflectionType) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).IsOfType(type_) };
    hr2ret(_hr,_hr)
  }
  
  //  Method ImplementsInterface
  
  #[allow(non_snake_case)]
  pub fn implements_interface(&self, base: &mut ID3D12ShaderReflectionType) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionType)).ImplementsInterface(base) };
    hr2ret(_hr,_hr)
  }
  
  
}
#[derive(Clone)]

pub struct D3D12ShaderReflectionVariable(*mut ID3D12ShaderReflectionVariable);

impl HasIID for D3D12ShaderReflectionVariable {
  fn iid() -> REFGUID { &IID_ID3D12ShaderReflectionVariable }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12ShaderReflectionVariable(pp_vtbl as *mut _ as *mut ID3D12ShaderReflectionVariable) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}


impl D3D12ShaderReflectionVariable {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_SHADER_VARIABLE_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionVariable)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetType
  
  #[allow(non_snake_case)]
  pub fn get_type(&self) -> D3D12ShaderReflectionType {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionVariable)).GetType() };
    D3D12ShaderReflectionType::new(_hr as *mut _)
  }
  
  //  Method GetBuffer
  
  #[allow(non_snake_case)]
  pub fn get_buffer(&self) -> D3D12ShaderReflectionConstantBuffer {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionVariable)).GetBuffer() };
    D3D12ShaderReflectionConstantBuffer::new(_hr as *mut _)
  }
  
  //  Method GetInterfaceSlot
  
  #[allow(non_snake_case)]
  pub fn get_interface_slot(&self, uArrayIndex: UINT) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflectionVariable)).GetInterfaceSlot(uArrayIndex) };
    _hr
  }
  
  
}

pub struct D3D12ShaderReflection(*mut ID3D12ShaderReflection);

impl HasIID for D3D12ShaderReflection {
  fn iid() -> REFGUID { &IID_ID3D12ShaderReflection }
  fn new(pp_vtbl : *mut IUnknown) -> Self { D3D12ShaderReflection(pp_vtbl as *mut _ as *mut ID3D12ShaderReflection) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}
impl Drop for D3D12ShaderReflection {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for D3D12ShaderReflection {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}



impl D3D12ShaderReflection {
  //  Method GetDesc
  
  #[allow(non_snake_case)]
  pub fn get_desc(&self, desc: &mut D3D12_SHADER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetDesc(desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetConstantBufferByIndex
  
  #[allow(non_snake_case)]
  pub fn get_constant_buffer_by_index(&self, index: UINT) -> D3D12ShaderReflectionConstantBuffer {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetConstantBufferByIndex(index) };
    D3D12ShaderReflectionConstantBuffer::new(_hr as *mut _)
  }
  
  //  Method GetConstantBufferByName
  
  #[allow(non_snake_case)]
  pub fn get_constant_buffer_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionConstantBuffer {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetConstantBufferByName(name) };
    D3D12ShaderReflectionConstantBuffer::new(_hr as *mut _)
  }
  
  //  Method GetResourceBindingDesc
  
  #[allow(non_snake_case)]
  pub fn get_resource_binding_desc(&self, resource_index: UINT, desc: &mut D3D12_SHADER_INPUT_BIND_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetResourceBindingDesc(resource_index, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetInputParameterDesc
  
  #[allow(non_snake_case)]
  pub fn get_input_parameter_desc(&self, parameter_index: UINT, desc: &mut D3D12_SIGNATURE_PARAMETER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetInputParameterDesc(parameter_index, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetOutputParameterDesc
  
  #[allow(non_snake_case)]
  pub fn get_output_parameter_desc(&self, parameter_index: UINT, desc: &mut D3D12_SIGNATURE_PARAMETER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetOutputParameterDesc(parameter_index, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetPatchConstantParameterDesc
  
  #[allow(non_snake_case)]
  pub fn get_patch_constant_parameter_desc(&self, parameter_index: UINT, desc: &mut D3D12_SIGNATURE_PARAMETER_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetPatchConstantParameterDesc(parameter_index, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetVariableByName
  
  #[allow(non_snake_case)]
  pub fn get_variable_by_name(&self, name: LPCSTR) -> D3D12ShaderReflectionVariable {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetVariableByName(name) };
    D3D12ShaderReflectionVariable::new(_hr as *mut _)
  }
  
  //  Method GetResourceBindingDescByName
  
  #[allow(non_snake_case)]
  pub fn get_resource_binding_desc_by_name(&self, name: LPCSTR, desc: &mut D3D12_SHADER_INPUT_BIND_DESC) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetResourceBindingDescByName(name, desc) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetMovInstructionCount
  
  #[allow(non_snake_case)]
  pub fn get_mov_instruction_count(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetMovInstructionCount() };
    _hr
  }
  
  //  Method GetMovcInstructionCount
  
  #[allow(non_snake_case)]
  pub fn get_movc_instruction_count(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetMovcInstructionCount() };
    _hr
  }
  
  //  Method GetConversionInstructionCount
  
  #[allow(non_snake_case)]
  pub fn get_conversion_instruction_count(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetConversionInstructionCount() };
    _hr
  }
  
  //  Method GetBitwiseInstructionCount
  
  #[allow(non_snake_case)]
  pub fn get_bitwise_instruction_count(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetBitwiseInstructionCount() };
    _hr
  }
  
  //  Method GetGSInputPrimitive
  
  #[allow(non_snake_case)]
  pub fn get_g_s_input_primitive(&self) -> D3D_PRIMITIVE {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetGSInputPrimitive() };
    _hr
  }
  
  //  Method IsSampleFrequencyShader
  
  #[allow(non_snake_case)]
  pub fn is_sample_frequency_shader(&self) -> BOOL {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).IsSampleFrequencyShader() };
    _hr
  }
  
  //  Method GetNumInterfaceSlots
  
  #[allow(non_snake_case)]
  pub fn get_num_interface_slots(&self) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetNumInterfaceSlots() };
    _hr
  }
  
  //  Method GetMinFeatureLevel
  
  #[allow(non_snake_case)]
  pub fn get_min_feature_level(&self, level: &mut D3D_FEATURE_LEVEL) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetMinFeatureLevel(level) };
    hr2ret(_hr,_hr)
  }
  
  //  Method GetThreadGroupSize
  
  #[allow(non_snake_case)]
  pub fn get_thread_group_size(&self, size_x: &mut UINT, size_y: &mut UINT, size_z: &mut UINT) -> UINT {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetThreadGroupSize(size_x, size_y, size_z) };
    _hr
  }
  
  //  Method GetRequiresFlags
  
  #[allow(non_snake_case)]
  pub fn get_requires_flags(&self) -> UINT64 {
  
    let _hr=unsafe { (*(self.0 as *mut ID3D12ShaderReflection)).GetRequiresFlags() };
    _hr
  }
  
  
}
