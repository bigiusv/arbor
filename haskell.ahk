#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

#Hotstring EndChars 
SetTitleMatchMode, 2

^w::
	WinActivate, GHCi
Return

^q::
    Send :load Server.AbacoServer{Enter}
Return
^a::
    Send main{Enter}
Return

^b::
	Send :load arbor{Enter}
Return

;^o::
^o:: 
	Send main{Enter}
Return
^d::
	Send :cd D:\Dati\Profili\M026980\Documents\programmi\nucleo\bi-mvc\arbor{Enter}
Return 

^n:: 
	WinActivate, Notepad
Return
^m:: 
	WinActivate, Mozilla
    Send {F5}
Return

^j:: 
	WinActivate, JBoss
Return
;^r:: Send :: Parser {Enter}
;	= do {{}{Enter}{Space}{Space}{Space}{Space}{Space};{Enter};{Enter};{Enter};{Enter}{}{Enter}
;Return