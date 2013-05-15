@rem
@echo on
@setlocal
@set verbose=NO
@set quiet=NO

@set release_lib=rel\skyraid\lib\
@set release_erts_bin=%CD%\rel\skyraid\erts-5.10.1\bin
@set skyraid_src=%CD%\apps\skyraid
@set skyraid_webmachine_src=%CD%\apps\skyraid_webmachine
 
@if "%1"=="usage" @goto usage
@if "%1"=="compile" @goto compile
@if "%1"=="test" @goto test
@if "%1"=="deps" @goto deps
@if "%1"=="clean" @goto clean
@if "%1"=="rel" @goto rel
@if "%1"=="relclean" @goto relclean
@if "%1"=="stage" @goto stage
@if "%1"=="skyraid" @goto skyraid
@if "%1"=="observer" @goto observer
@echo Unknown command: "%1"

 
:usage
@echo Usage: %~n0 [compile^|test^|deps^|clean^|rel^|relclean^|stage^|skyraid^|observer^]
@goto :EOF

:compile
@rebar compile
@goto :EOF

:test
@rebar eunit skip_deps=true
@goto :EOF

:deps
@rebar get-deps
@goto :EOF

:clean
@rebar clean
@goto :EOF

:rel
@rebar generate
@goto :EOF

:relclean
%release_erts_bin%\epmd.exe -kill
@echo Are you sure you want to delete the release?
@rmdir /S rel\skyraid
@goto :EOF
 
:stage
@rmdir /S %release_lib%skyraid-1
@rmdir /S %release_lib%skyraid_webmachine-1
@mklink /D %release_lib%skyraid-1 %skyraid_src%
@mklink /D %release_lib%skyraid_webmachine-1 %skyraid_webmachine_src%
@goto :EOF

:skyraid
"rel/skyraid/bin/skyraid" %2
@goto :EOF

:observer
erl -sname observer -hidden -setcookie skyraid -run observer
@goto :EOF