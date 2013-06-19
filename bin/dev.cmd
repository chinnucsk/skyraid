@rem
@echo on
@setlocal
@setlocal enabledelayedexpansion
@set verbose=YES
@set quiet=NO
@set root=%CD%\..

:ctags
@set tagpath;
@for /D %%a in (../apps/*) do set tagpath=!tagpath! -pa %root%\apps\%%a\ebin
@for /D %%a in (../deps/*) do set tagpath=!tagpath! -pa %root%\deps\%%a\ebin
@echo %tagpath% 

werl %tagpath% -smp -s reloader -s skyraid


PAUSE