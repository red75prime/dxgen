@echo off
cls
".\tools\nuget\NuGet.exe" "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"

".\tools\nuget\NuGet.exe" "Install" "NUnit.Runners" "-OutputDirectory" "packages" "-ExcludeVersion"

"packages\FAKE\tools\Fake.exe" build.fsx
