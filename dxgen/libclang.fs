module libclang

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
    | LastExpr = 145
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
    | LastStmt = 231
    | TranslationUnit = 300
    | FirstAttr = 400
    | UnexposedAttr = 400
    | IbActionAttr = 401
    | IbOutletAttr = 402
    | IbOutletCollectionAttr = 403
    | CxxFinalAttr = 404
    | CxxOverrideAttr = 405
    | AnnotateAttr = 406
    | AsmLabelAttr = 407
    | LastAttr = 407
    | PreprocessingDirective = 500
    | MacroDefinition = 501
    | MacroExpansion = 502
    | MacroInstantiation = 502
    | InclusionDirective = 503
    | FirstPreprocessing = 500
    | LastPreprocessing = 503

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

type ChildVisitResult =
    | Break = 0
    | Continue = 1
    | Recurse = 2

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
type String =
    struct
        val data: IntPtr
        val private_flags: uint32
    end

type Index = IntPtr
type TranslationUnit = IntPtr
type ClientData = IntPtr

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type CursorVisitor = delegate of Cursor * Cursor * ClientData -> ChildVisitResult

[<DllImport("libclang", EntryPoint = "clang_createIndex", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Index createIndex(int excludeDeclarationsFromPch, int displayDiagnostics)

[<DllImport("libclang", EntryPoint = "clang_disposeIndex", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeIndex(Index translationUnit)

[<DllImport("libclang", EntryPoint = "clang_parseTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern TranslationUnit parseTranslationUnit(Index index, string fileName, string[] args, int numArgs, UnsavedFile[] unsavedFiles, uint32 numUnsavedFiles, TranslationUnitFlags flags)

[<DllImport("libclang", EntryPoint = "clang_disposeTranslationUnit", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeTranslationUnit(TranslationUnit translationUnit)

[<DllImport("libclang", EntryPoint = "clang_getTranslationUnitCursor", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Cursor getTranslationUnitCursor(TranslationUnit translationUnit)

[<DllImport("libclang", EntryPoint = "clang_visitChildren", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern uint32 visitChildren(Cursor cursor, CursorVisitor visitor, ClientData clientData)

[<DllImport("libclang", EntryPoint = "clang_getCursorKind", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern CursorKind getCursorKind(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorSpelling", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern String getCursorSpelling(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getCursorType", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern Type getCursorType(Cursor cursor)

[<DllImport("libclang", EntryPoint = "clang_getTypeSpelling", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern String getTypeSpelling(Type ty)

[<DllImport("libclang", EntryPoint = "clang_disposeString", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
extern void disposeString(String str)