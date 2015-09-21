dxgen
=====

Code generator for winapi-rs and safe(ish) wrappers around winapi bindings.

Reads the DirectX header files using libclang and generates the corresponding rust source code. Provides a system for mapping names to more idiomatic rust names.

The program requires that path to 'libclang.dll' to be in PATH environment variable.

![](http://i.imgur.com/zLHKwJx.png?1)
