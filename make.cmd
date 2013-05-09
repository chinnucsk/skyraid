@rem
@echo on
@setlocal
@set verbose=NO
@set quiet=NO

 
@set release_lib=rel\skyraid\lib\
@set release_erts_bin=%CD%\rel\skyraid\erts-*\bin
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
@echo Unknown command: "%1"

 
:usage
@echo Usage: %~n0 [compile^|test^|deps^|clean^|rel^|relclean^|stage^|skyraid^]
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
@rem %release_erts_bin%\epmd.exe -kill
@rmdir /S rel\skyraid
@goto :EOF
 
:stage
@rmdir /S %release_lib%skyraid-1
@rmdir /S %release_lib%skyraid_webmachine-1
@mklink %release_lib%skyraid-1 %skyraid_src%
@mklink %release_lib%skyraid_webmachine-1 %skyraid_webmachine_src%
@goto :EOF

 
:skyraid:
"rel/skyraid/bin/skyraid" %2
@goto :EOF