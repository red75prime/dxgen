dxgen
=====

Code generator for winapi-rs and safe(ish) wrappers around winapi bindings.

Reads the D3D12 and DXGI header files using libclang and generates the corresponding rust source code. Provides a system for mapping names to more idiomatic rust names.

The program requires that path to 'libclang.dll' to be in PATH environment variable.

![](http://i.imgur.com/0dofwl6.png)
