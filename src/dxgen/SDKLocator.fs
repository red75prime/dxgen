module SDKLocator

let private findPathInRegistry () =
    use path = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(@"Software\Microsoft\Windows Kits\Installed Roots")
    path.GetValue("KitsRoot81") :?> string

let public findSDKRootDirectory () = System.IO.Path.Combine(findPathInRegistry (), @"Include\shared\")