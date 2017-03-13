module libclang

// Enumerations updated for clang 3.9.0 

open System
open System.Runtime.InteropServices

[<Flags>]
type TranslationUnitFlags =
    | None = 0x00
    | DetailedProcessingRecord = 0x01
    | Incomplete = 0x02
    | PrecompiledPreamble = 0x04
    | CacheCompletionResults = 0x08
    | ForSerialization = 0x10
    | CXXChainedPCH = 0x20
    | SkipFunctionBodies = 0x40
    | IncludeBriefCommentsInCodeCompletion = 0x80
    | CreatePreambleOnFirstParse = 0x100
    | KeepGoing = 0x200

type CursorKind =
    | UnexposedDecl = 1
    | StructDecl = 2
    | UnionDecl = 3
    | ClassDecl = 4
    | EnumDecl = 5
    | FieldDecl = 6
    | EnumConstantDecl = 7
    | FunctionDecl = 8
    | VarDecl = 9
    | ParmDecl = 10
    | ObjCInterfaceDecl = 11
    | ObjCCategoryDecl = 12
    | ObjCProtocolDecl = 13
    | ObjCPropertyDecl = 14
    | ObjCIvarDecl = 15
    | ObjCInstanceMethodDecl = 16
    | ObjCClassMethodDecl = 17
    | ObjCImplementationDecl = 18
    | ObjCCategoryImplDecl = 19
    | TypedefDecl = 20
    | CxxMethod = 21
    | Namespace = 22
    | LinkageSpec = 23
    | Constructor = 24
    | Destructor = 25
    | ConversionFunction = 26
    | TemplateTypeParameter = 27
    | NonTypeTemplateParameter = 28
    | TemplateTemplateParameter = 29
    | FunctionTemplate = 30
    | ClassTemplate = 31
    | ClassTemplatePartialSpecialization = 32
    | NamespaceAlias = 33
    | UsingDirective = 34
    | UsingDeclaration = 35
    | TypeAliasDecl = 36
    | ObjCSynthesizeDecl = 37
    | ObjCDynamicDecl = 38
    | CxxAccessSpecifier = 39
    | FirstDecl = 1
    | LastDecl = 39
    | FirstRef = 40
    | ObjCSuperClassRef = 40
    | ObjCProtocolRef = 41
    | ObjCClassRef = 42
    | TypeRef = 43
    | CxxBaseSpecifier = 44
    | TemplateRef = 45
    | NamespaceRef = 46
    | MemberRef = 47
    | LabelRef = 48
    | OverloadedDeclRef = 49
    | VariableRef = 50
    | LastRef = 50
    | FirstInvalid = 70
    | InvalidFile = 70
    | NoDeclFound = 71
    | NotImplemented = 72
    | InvalidCode = 73
    | LastInvalid = 73
    | FirstExpr = 100
    | UnexposedExpr = 100
    | DeclRefExpr = 101
    | MemberRefExpr = 102
    | CallExpr = 103
    | ObjCMessageExpr = 104
    | BlockExpr = 105
    | IntegerLiteral = 106
    | FloatingLiteral = 107
    | ImaginaryLiteral = 108
    | StringLiteral = 109
    | CharacterLiteral = 110
    | ParenExpr = 111
    | UnaryOperator = 112
    | ArraySubscriptExpr = 113
    | BinaryOperator = 114
    | CompoundAssignOperator = 115
    | ConditionalOperator = 116
    | CStyleCastExpr = 117
    | CompoundLiteralExpr = 118
    | InitListExpr = 119
    | AddrLabelExpr = 120
    | StmtExpr = 121
    | GenericSelectionExpr = 122
    | GnuNullExpr = 123
    | CxxStaticCastExpr = 124
    | CxxDynamicCastExpr = 125
    | CxxReinterpretCastExpr = 126
    | CxxConstCastExpr = 127
    | CxxFunctionalCastExpr = 128
    | CxxTypeidExpr = 129
    | CxxBoolLiteralExpr = 130
    | CxxNullPtrLiteralExpr = 131
    | CxxThisExpr = 132
    | CxxThrowExpr = 133
    | CxxNewExpr = 134
    | CxxDeleteExpr = 135
    | UnaryExpr = 136
    | ObjCStringLiteral = 137
    | ObjCEncodeExpr = 138
    | ObjCSelectorExpr = 139
    | ObjCProtocolExpr = 140
    | ObjCBridgedCastExpr = 141
    | PackExpansionExpr = 142
    | SizeOfPackExpr = 143
    | LambdaExpr = 144
    | ObjCBoolLiteralExpr = 145
    | ObjCSelfExpr = 146
    | OMPArraySectionExpr = 147
    | LastExpr = 147
    | FirstStmt = 200
    | UnexposedStmt = 200
    | LabelStmt = 201
    | CompoundStmt = 202
    | CaseStmt = 203
    | DefaultStmt = 204
    | IfStmt = 205
    | SwitchStmt = 206
    | WhileStmt = 207
    | DoStmt = 208
    | ForStmt = 209
    | GotoStmt = 210
    | IndirectGotoStmt = 211
    | ContinueStmt = 212
    | BreakStmt = 213
    | ReturnStmt = 214
    | AsmStmt = 215
    | ObjCAtTryStmt = 216
    | ObjCAtCatchStmt = 217
    | ObjCAtFinallyStmt = 218
    | ObjCAtThrowStmt = 219
    | ObjCAtSynchronizedStmt = 220
    | ObjCAutoreleasePoolStmt = 221
    | ObjCForCollectionStmt = 222
    | CxxCatchStmt = 223
    | CxxTryStmt = 224
    | CxxForRangeStmt = 225
    | SehTryStmt = 226
    | SehExceptStmt = 227
    | SehFinallyStmt = 228
    | MsAsmStmt = 229
    | NullStmt = 230
    | DeclStmt = 231
    | OMPParallelDirective = 232
    | OMPSimdDirective              = 233
    | OMPForDirective               = 234
    | OMPSectionsDirective          = 235
    | OMPSectionDirective           = 236
    | OMPSingleDirective            = 237
    | OMPParallelForDirective       = 238
    | OMPParallelSectionsDirective  = 239
    | OMPTaskDirective              = 240
    | OMPMasterDirective            = 241
    | OMPCriticalDirective          = 242
    | OMPTaskyieldDirective         = 243
    | OMPBarrierDirective           = 244
    | OMPTaskwaitDirective          = 245
    | OMPFlushDirective             = 246
    | SEHLeaveStmt                  = 247
    | OMPOrderedDirective           = 248
    | OMPAtomicDirective            = 249
    | OMPForSimdDirective           = 250
    | OMPParallelForSimdDirective   = 251
    | OMPTargetDirective            = 252
    | OMPTeamsDirective             = 253
    | OMPTaskgroupDirective         = 254
    | OMPCancellationPointDirective = 255
    | OMPCancelDirective            = 256
    | OMPTargetDataDirective        = 257
    | OMPTaskLoopDirective          = 258
    | OMPTaskLoopSimdDirective      = 259
    | OMPDistributeDirective        = 260
    | OMPTargetEnterDataDirective   = 261
    | OMPTargetExitDataDirective    = 262
    | OMPTargetParallelDirective    = 263
    | OMPTargetParallelForDirective = 264
    | LastStmt = 264
    | TranslationUnit = 300
    | UnexposedAttr = 400
    | IbActionAttr = 401
    | IbOutletAttr = 402
    | IbOutletCollectionAttr = 403
    | CxxFinalAttr = 404
    | CxxOverrideAttr = 405
    | AnnotateAttr = 406
    | AsmLabelAttr = 407
    | PackedAttr                    = 408
    | PureAttr                      = 409
    | ConstAttr                     = 410
    | NoDuplicateAttr               = 411
    | CUDAConstantAttr              = 412
    | CUDADeviceAttr                = 413
    | CUDAGlobalAttr                = 414
    | CUDAHostAttr                  = 415
    | CUDASharedAttr                = 416
    | VisibilityAttr                = 417
    | DLLExport                     = 418
    | DLLImport                     = 419
    | FirstAttr = 400
    | LastAttr = 419
    | PreprocessingDirective = 500
    | MacroDefinition = 501
    | MacroExpansion = 502
    | MacroInstantiation = 502
    | InclusionDirective = 503
    | FirstPreprocessing = 500
    | LastPreprocessing = 503
    | ModuleImportDecl              = 600
    | TypeAliasTemplateDecl         = 601
    | FirstExtraDecl                = 600
    | LastExtraDecl                 = 601
    | OverloadCandidate             = 700


type TypeKind =
    | Invalid = 0
    | Unexposed = 1
    | Void = 2
    | Bool = 3
    | Char_U = 4
    | UChar = 5
    | Char16 = 6
    | Char32 = 7
    | UShort = 8
    | UInt = 9
    | ULong = 10
    | ULongLong = 11
    | UInt128 = 12
    | Char_S = 13
    | SChar = 14
    | WChar = 15
    | Short = 16
    | Int = 17
    | Long = 18
    | LongLong = 19
    | Int128 = 20
    | Float = 21
    | Double = 22
    | LongDouble = 23
    | NullPtr = 24
    | Overload = 25
    | Dependent = 26
    | ObjCId = 27
    | ObjCClass = 28
    | ObjCSel = 29
    | FirstBuiltin = 2
    | LastBuiltin = 29
    | Complex = 100
    | Pointer = 101
    | BlockPointer = 102
    | LValueReference = 103
    | RValueReference = 104
    | Record = 105
    | Enum = 106
    | Typedef = 107
    | ObjCInterface = 108
    | ObjCObjectPointer = 109
    | FunctionNoProto = 110
    | FunctionProto = 111
    | ConstantArray = 112
    | Vector = 113
    | IncompleteArray = 114
    | VariableArray = 115
    | DependentSizedArray = 116
    | MemberPointer = 117
    | Auto = 118
    | Elaborated = 119

type ChildVisitResult =
    | Break = 0
    | Continue = 1
    | Recurse = 2

type CXErrorCode = 
    | Success = 0
    | Failure = 1
    | Crashed = 2
    | InvalidArguments = 3
    | ASTReadError = 4

[<Literal>]
let TypeLayoutError_Invalid = -1L
[<Literal>]
let TypeLayoutError_Incomplete = -2L
[<Literal>]
let TypeLayoutError_Dependent = -3L
[<Literal>]
let TypeLayoutError_NotConstantSize = -4L
[<Literal>]
let TypeLayoutError_InvalidFieldName = -5L

#nowarn "9"
[<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)>]
type UnsavedFile =
    struct
        val public fileName: string
        val public contents: string
        val public length: uint64
    end

[<StructLayout(LayoutKind.Sequential)>]
type Cursor =
    struct
        val public kind: CursorKind
        val public xdata: int
        val public data0: IntPtr
        val public data1: IntPtr
        val public data2: IntPtr
    end

[<StructLayout(LayoutKind.Sequential)>]
type Type =
    struct
        val public kind: TypeKind
        val public data0: IntPtr
        val public data1: IntPtr
    end

[<StructLayout(LayoutKind.Sequential)>]
type SourceRange =
    struct
        val public data0: IntPtr
        val public data1: IntPtr
        val public ptr_begin: uint32
        val public ptr_end: uint32
    end

[<StructLayout(LayoutKind.Sequential)>]
type SourceLocation =
    struct
        val public data0: IntPtr
        val public data1: IntPtr
        val public int_data: uint32
    end

[<StructLayout(LayoutKind.Sequential)>]
type Token =
    struct
        val public int_data0: uint32
        val public int_data1: uint32
        val public int_data2: uint32
        val public int_data3: uint32
        val public ptr_data: IntPtr
    end

[<StructLayout(LayoutKind.Sequential)>]
type CXString =
    struct
        val data: IntPtr
        val private_flags: uint32
    end

type Index = IntPtr
type TranslationUnit = IntPtr
type ClientData = IntPtr
type File = IntPtr

type CallingConv=
  |CXCallingConv_Default = 0
  |CXCallingConv_C = 1
  |X86StdCall = 2
  |X86FastCall = 3
  |X86ThisCall = 4
  |X86Pascal = 5
  |AAPCS = 6
  |AAPCS_VFP = 7
  // Value 8 was PnaclCall, but it was never used, so it could safely be re-used. 
  |IntelOclBicc = 9
  |X86_64Win64 = 10
  |X86_64SysV = 11
  |X86VectorCall = 12
  |Invalid = 100
  |Unexposed = 200

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type CursorVisitor = delegate of Cursor * Cursor * ClientData -> ChildVisitResult

[<DllImport("libclang", EntryPoint = "clang_disposeString", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeString(CXString str)

let toString (str: CXString) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    disposeString(str)
    if result=null then "" else result

[<DllImport("libclang", EntryPoint = "clang_createIndex", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Index createIndex(int excludeDeclarationsFromPch, int displayDiagnostics)

[<DllImport("libclang", EntryPoint = "clang_disposeIndex", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeIndex(Index translationUnit)

[<DllImport("libclang", EntryPoint = "clang_parseTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern TranslationUnit parseTranslationUnit(Index index, string fileName, string[] args, int numArgs, UnsavedFile[] unsavedFiles, uint32 numUnsavedFiles, TranslationUnitFlags flags)

let parseTranslationUnitFS (index: Index) (fileName: string) (args: string[]) (unsavedFiles: UnsavedFile[]) (flags: TranslationUnitFlags) =
    parseTranslationUnit(index, fileName, args, Array.length args, unsavedFiles, Array.length unsavedFiles |> uint32, flags)

[<DllImport("libclang", EntryPoint = "clang_disposeTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeTranslationUnit(TranslationUnit translationUnit)

[<DllImport("libclang", EntryPoint = "clang_saveTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void saveTranslationUnit(TranslationUnit translationUnit, string fileName, uint32 options)

[<DllImport("libclang", EntryPoint = "clang_defaultSaveOptions", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 defaultSaveOptions(TranslationUnit translationUnit)

[<DllImport("libclang", EntryPoint = "clang_getTranslationUnitCursor", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Cursor getTranslationUnitCursor(TranslationUnit translationUnit)

[<DllImport("libclang", EntryPoint = "clang_Cursor_getTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern TranslationUnit getTranslationUnit(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_visitChildren", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 visitChildren(Cursor cursor, CursorVisitor visitor, ClientData clientData)

[<DllImport("libclang", EntryPoint = "clang_getCursorKind", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CursorKind getCursorKind(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getCursorType(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorResultType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getCursorResultType(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getEnumDeclIntegerType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getEnumDeclIntegerType(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getTypedefDeclUnderlyingType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getTypedefDeclUnderlyingType(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorSpelling", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CXString getCursorSpelling(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorDisplayName", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CXString getCursorDisplayName(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorExtent", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern SourceRange getCursorExtent(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getRangeStart", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern SourceLocation getRangeStart(SourceRange range)

[<DllImport("libclang", EntryPoint = "clang_getRangeEnd", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern SourceLocation getRangeEnd(SourceRange range)

[<DllImport("libclang", EntryPoint = "clang_getExpansionLocation", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void getExpansionLocation(SourceLocation source, File& file, uint32& line, uint32& column, uint32& offset)

[<DllImport("libclang", EntryPoint = "clang_getFileName", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CXString getFileName(File file)

let getFileNameFS file=
  getFileName file |> toString

[<DllImport("libclang", EntryPoint = "clang_getEnumConstantDeclValue", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int64 getEnumConstantDeclValue(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getEnumConstantDeclUnsignedValue", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint64 getEnumConstantDeclUnsignedValue(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getTypeSpelling", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CXString getTypeSpelling(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getTypeDeclaration", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Cursor getTypeDeclaration(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getNumArgTypes", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int getNumArgTypes(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getArgType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getArgType(Type ty, uint32 i)

[<DllImport("libclang", EntryPoint = "clang_equalTypes", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 equalTypes(Type tya, Type tyb)

let equalTypesFs tya tyb =
    equalTypes(tya, tyb) <> 0u

[<DllImport("libclang", EntryPoint = "clang_getArrayElementType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getArrayElementType(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getArraySize", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int64 getArraySize(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getFunctionTypeCallingConv", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CallingConv getFunctionTypeCallingConv(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getResultType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getResultType(Type ty)

[<DllImport("libclang", EntryPoint = "clang_tokenize", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void tokenize(TranslationUnit translationUnit, SourceRange range, Token* & tokens, uint32& tokenCount)

[<DllImport("libclang", EntryPoint = "clang_disposeTokens", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeTokens(TranslationUnit translationUnit, Token* tokens, uint32 tokenCount)

[<DllImport("libclang", EntryPoint = "clang_getTokenSpelling", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CXString getTokenSpelling(TranslationUnit translationUnit, Token token)

[<DllImport("libclang", EntryPoint = "clang_Type_getSizeOf", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int64 getSizeOfType(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getPointeeType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getPointeeType(Type ty)

[<DllImport("libclang", EntryPoint = "clang_getCanonicalType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getCanonicalType(Type ty)

[<DllImport("libclang", EntryPoint = "clang_Cursor_getNumArguments", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int32 getNumArguments(Cursor ty)

[<DllImport("libclang", EntryPoint = "clang_isConstQualifiedType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 isConstQualifiedType(Type ty)

let isConstQualifiedTypeFS ty=
  isConstQualifiedType ty <> 0u

[<DllImport("libclang", EntryPoint = "clang_Cursor_isBitField", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 isBitField(Cursor c)

let isBitFieldFS c=
  isBitField c <> 0u

[<DllImport("libclang", EntryPoint = "clang_CXXMethod_isPureVirtual", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 isPureVirtual(Cursor c)

let isPureVirtualFS c=
  isPureVirtual c <> 0u

[<DllImport("libclang", EntryPoint = "clang_getFieldDeclBitWidth", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int32 getFieldDeclBitWidth(Cursor c)

[<DllImport("libclang", EntryPoint = "clang_Type_getOffsetOf", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern int64 getOffsetOfInBits(Type t, String s)

[<DllImport("libclang", EntryPoint = "clang_Type_getNamedType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getNamedType(Type t)


let tokenizeFS cursor=
  let translationUnit=getTranslationUnit cursor
  let range = cursor |> getCursorExtent
  let mutable tokens: nativeptr<Token> = Unchecked.defaultof<_>
  let mutable tokenCount = 0u

  tokenize(translationUnit, range, &tokens, &tokenCount)
  try
      [0.. int(tokenCount)-1] |> 
        List.map (fun idx -> 
            let tokenSpelling = getTokenSpelling(translationUnit, Microsoft.FSharp.NativeInterop.NativePtr.get tokens idx) |> toString
            tokenSpelling )
  finally
      disposeTokens(translationUnit, tokens, tokenCount)


let visitChildrenFS<'t> (cursor:Cursor) (visitor: Cursor -> Cursor -> 't -> ChildVisitResult) (data:'t) =
  let handle=GCHandle.Alloc (box data)
  try
    let vis1 = fun cursor parent param -> 
                let handle = GCHandle.FromIntPtr param
                visitor cursor parent (unbox(handle.Target))
    visitChildren(cursor,new CursorVisitor(vis1),GCHandle.ToIntPtr handle)
  finally
    handle.Free()

let getCursorDisplayNameFS cursor=
  getCursorDisplayName cursor |> toString

let getTypeSpellingFS cursor=
  getTypeSpelling cursor |> toString

let getCursorSpellingFS cursor=
  getCursorSpelling cursor |> toString
