dxgen
=====

Code generator for winapi-rs and safe(ish) wrappers around winapi bindings.

Reads the D3D12 and DXGI header files using libclang and generates the corresponding rust source code. Provides a system for mapping names to more idiomatic rust names.

The program requires that path to 'libclang.dll' to be in PATH environment variable.

![](http://i.imgur.com/0dofwl6.png)

You will need windows SDK and Rust-msvc 64-bit to build Rust project located at dxgen/src/dxgen/scaffolding. 

If linker can't find d3d12.lib, then add environment variable

    LIBRARY_PATH=C:\Program Files (x86)\Windows Kits\10\Lib\10.0.10240.0\um\x64
