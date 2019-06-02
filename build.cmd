    
.paket\paket.bootstrapper.exe 5.209

if errorlevel 1 (
  exit /b %errorlevel%
)

SET TOOL_PATH=.fake

IF NOT EXIST "%TOOL_PATH%\fake.exe" (
  dotnet tool install fake-cli --tool-path ./%TOOL_PATH%
)

"%TOOL_PATH%/fake.exe" run build.fsx %*