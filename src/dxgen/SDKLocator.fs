module SDKLocator

let private findPathInRegistry () =
    use path = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(@"Software\Microsoft\Windows Kits\Installed Roots")
    path.GetValue("KitsRoot10") :?> string

let public findSDKRootDirectory () = findPathInRegistry()