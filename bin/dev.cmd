@rem
@echo on
@setlocal
@setlocal enabledelayedexpansion
@set verbose=YES
@set quiet=NO
@set root=%CD%\..

@set libpath
@for /D %%a in (../apps/*) do set libpath=!libpath! -pa %root%\apps\%%a\ebin
@for /D %%a in (../deps/*) do set libpath=!libpath! -pa %root%\deps\%%a\ebin
@echo %libpath%

werl %libpath% -smp -s reloader -s skyraid
