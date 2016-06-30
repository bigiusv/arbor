;-------------------------------------------------------------------------------------------

; Ahk Window Spy 1.3.3

; by Decarlo



; shows window info: active, under pointer, hidden, all



; hotkey toggles the following modes:

; press 1x - tooltip

; press 2x - snapshot window, press CapsLock to update

; press 3x - polling window, updates with pointer motion

; press 4x - off

; hold for at least 0.4 sec - show control list of active window, if not in another mode.

; press hotkey from control list mode - display Window Chart, including info on hidden windows.

; press/hold Esc to exit, or press hotkey for 0.4 sec



; other features:

; shows pointer coordinates relative:

;	to screen,

;	to active window, including right/bottom mirror coordinates,

;	to window beneath pointer

; shows various attributes corresponding to both window active & window under pointer.

; handles tooltip bugs related to one or more of the following:

;	hovering over the tooltip,

;	hovering over irretrievable/non-existent control,

;	AutoHotkey.ini string present in window title,

;	re-size cursors,

; shows win-drag coordinates indicating new window position after dragging

; temporary deactivation of tooltip/mode3 by holding LCtrl, to highlight/double-click.

; Alternatively, 3+ click or select/Enter to activate/Open during navigation.



; add: option to monitor user-specified controls



; history, recent topmost:

; fixed exit-related bugs due to conversion to function form

; converted to function form to reduce global variable list and avoid naming conflicts

; replaced some unnecessary variables with reusable temp var

; added automatic control-size scaling on window re-size

; added window chart mode, which includes info on hidden windows

; changed code for exiting and fixed exiting from tooltip mode using Esc

; added holding of RShift+RCtrl for 2 sec restores to default window size & position

; added remembering of window position

; changed code for toggling AlwaysOnTop and fixed related window activation bug

; changed default snapshot key to CapsLock

; simplified the code

; changed toggle of window AlwaysOnTop to RShift+RCtrl

; fixed bug of snapshot window disappearing on toggling AlwaysOnTop

; added option to show control list: press hotkey for 0.4 sec if not in another mode

; fixed tooltip reappearing in window mode when applying LShift

; added pressing RCtrl toggles AlwaysOnTop

; added pressing LShift refreshes Snapshot

; fixed bug of tooltip reappearing on Monitoring exit

; fixed Ctrl to properly deactivate monitoring to allow highlight/copy

; added right/bottom-border window coordinates relative to within screen

; added right/bottom pointer coordinates relative to within active window

; added Esc as alternative to exit without hotkey toggle

; fixed bug of tooltip reappearing on Snapshot exit.

; added win-drag coordinates

; added window modes

; added PID, process name, colour, & pointer fields

; added display of attributes of window beneath pointer, tooltip mode

; added various modifications to handle specific tooltip peculiarities

; basic active window info in tooltip





#!w::	

AhkWindowSpy()

return





AhkWindowSpy()

{



static flagWindowInfo				; remember last mode used

global ws_xGui, ws_yGui, ws_wGui, ws_hGui	; remember last window position & size



DetectHiddenWindows, off

SetTitleMatchMode, 2

CoordMode, Mouse

intervalPointerWindowInfo = 300



ifWinExist, Window Chart 1

{

	WinClose, Window Chart 1

	flagWindowInfo =

	KeyWait, w

	EXIT

}

ifWinExist, Control List 1

{

	winListIDsVisible =

	winListIDs =

	winListPIDs =

	winListTitles =

	winListClasses =

	winProcessList =

	



	WinClose, Control List 1,, 3

	flagWindowInfo = W

	WinGet, Win#, List



	Loop %Win#%

	{

		winListItem := win#%A_Index%

		winListIDsVisible = %winListIDsVisible%%winListItem%`r`n

	}



	DetectHiddenWindows, on

	WinGet, Win#, List



	Loop %Win#%

	{

		winListItem := win#%A_Index%

		winListIDs = %winListIDs%%winListItem%`r`n

	}



	Loop, parse, winListIDs, `n, `r

	{

		WinGet, winPID%A_Index%, PID, ahk_id %A_LoopField%

		winListItem := winPID%A_Index%

		winListPIDs = %winListPIDs%%winListItem%`r`n		; pid's with windows



		WinGetTitle, winTitle%A_Index%, ahk_id %A_LoopField%

		winListItem := winTitle%A_Index%

		winListTitles = %winListTitles%%winListItem%`r`n	; titles of windows



		WinGetClass, winClass%A_Index%, ahk_id %A_LoopField%

		winListItem := winClass%A_Index%

		winListClasses = %winListClasses%%winListItem%`r`n	; classes of windows



	}



	Loop, parse, winListPIDs, `n, `r

	{

		if A_LoopField =

			CONTINUE

		WinGet, processName%A_Index%, ProcessName, ahk_pid %A_LoopField%

		winListItem := processName%A_Index%

		winProcessList=%winProcessList%%winListItem%`r`n	; processes with windows

	}



	Sort, winListIDs

	Sort, winListPIDs, NU

	Sort, winListTitles, U

	Sort, winListClasses, U

	Sort, winProcessList, U



	sleep 400

	if GetKeyState("LWin","p")

	{

		KeyWait, w

		msgbox %Win#% windows found

		msgbox,, Windows Chart 1, window id's`r`n`r`n%winListIDs%

		msgbox,, Windows Chart 1, pid's owning windows`r`n`r`n%winListPIDs%

		msgbox,, Windows Chart 1, titles of windows`r`n`r`n%winListTitles%

		msgbox,, Windows Chart 1, classes of windows`r`n`r`n%winListClasses%

		msgbox,, Windows Chart 1, processes with windows`r`n`r`n%winProcessList%

	}



	; Create the ListView

	Gui, Add, ListView, AltCreate r33 w700 gWindowChart , Item|ID|PID|Class|Process|win type|Title

		

	Loop, %Win#%

	{

		LV_Add("", 1, win#%A_Index%, winPID%A_Index%, winClass%A_Index%, processName%A_Index%, "", winTitle%A_Index%) ; 1st param=options

		temp_LV_1 := win#%A_Index%

		IfNotInString, winListIDsVisible, %temp_LV_1%

			LV_Modify(A_Index, "Col6", "hidden")

	}

	

	LV_ModifyCol()  ; Auto-size each column

	LV_ModifyCol(1, "33 Integer Left")	; column heading 33 pixels; left-justification

	LV_ModifyCol(3, "Integer")	; For sorting purposes - PID is an integer

	LV_ModifyCol(5, "Sort")		; sort rows by Process

	LV_ModifyCol(6, 53)		; column heading 53 pixels



	Loop, %Win#%

		LV_Modify(A_Index, "", A_Index)		; number the sorted rows



	Gui, +Resize

	Gui, Show,, Ahk Window Spy - Window Chart 1

	return



	KeyWait, w

	EXIT

}

Loop 8

{

	sleep 50

	if not GetKeyState("w","p")

		BREAK			; continues with thread

	if A_Index=8			; display control list

	{

		if flagWindowInfo <>	; exit the current mode

		{

			GoSub ws_GuiClose

			KeyWait, w

			EXIT

		}

		WinGet, controlList, ControlList, A

		Gui Destroy

		Gui, Font, 0x183CE7

		Gui, Color, 0xD6D3D6

		Gui, +Resize w300

		Gui, Add, Edit, +AutoSize +VScroll +HScroll w165 h600 ReadOnly, %controlList%

		Gui, Show, x-2 y100 w175, Control List 1

		ControlSend, Edit1, {end}, Control List 1

		flagWindowInfo = C

		KeyWait, w

		EXIT

	}

}





If flagWindowInfo=3				; toggle to off

{

	GoSub ws_GuiClose

	EXIT

}

else If flagWindowInfo=2			; toggle to window Monitoring

{

	Gui, Show, NoActivate, Ahk Window Spy - auto-refresh	; similar effect as above

	ControlSetText, Edit1, %Info1Tips%%tooltip1Info%, Ahk Window Spy -

	flagWindowInfo=3

}

else If flagWindowInfo=1			; toggle to Snapshot

{

	flagWindowInfo=2

	Tooltip

	GuiTopmost=1

	Gui, +AlwaysOnTop +Resize +VScroll +HScroll 

	Gui, Font, 0x183CE7 ;s6, Courier	; blue

	Gui, Color, 0xD6D3D6  		; <light grey; 0xFFFFA5 yellow

	Gui, Add, Edit, +VScroll +HScroll w301 h400 ReadOnly, %Info1Tips%%tooltip1Info%

	if ws_xGui =

		Gui, Show, x-2 y301, Ahk Window Spy - snapshot

	else

		Gui, Show, x%ws_xGui% y%ws_yGui%, Ahk Window Spy - snapshot

	WinWaitActive, Ahk Window Spy - ,, 1		; this improves responsiveness

	ControlSend, Edit1, ^{home}, Ahk Window Spy - 

}

else

{

	flagWindowInfo=1			; activate Tooltip

	SetTimer, PointerWindowInfo, %intervalPointerWindowInfo%

}

KeyWait, w

return





PointerWindowInfo:



SetTitleMatchMode, 2



Info1Tips = LCtrl pauses for select/copy/double-clicking.`r`nCapsLock updates info in snapshot mode.`r`nRShift+RCtrl toggles AlwaysOnTop.`r`n`r`n

tooltipInfo = pointer x`,y = `t(%xC%`,%yC%)   to screen`r`npointer x`,y = `t(%xCA%`,%yCA%`,%xCAm%`,%yCAm%)   to win active`r`npointer x`,y = `t(%xCU%`,%yCU%)   to win under`r`ncolour, rgb`t%color%`r`nwin active x`,y =`t(%xWinActive%`,%yWinActive%`,%xWinActiveM%`,%yWinActiveM%)`r`nwin under x`,y =`t(%xWinUnder%`,%yWinUnder%)`r`nwin drag x`,y =`t(%xWinDrag%`,%yWinDrag%)`r`nwin active w`,h =`t%widthWinActive%`,%heightWinActive%`r`nwin under w`,h =`t%widthWinUnder%`,%heightWinUnder%`r`nTitle active`t%titleActive%`r`nTitle under`t%titleUnder%`r`nPID active =`t%PIDActive%`r`nPID under =`t%PIDUnder%`r`nprocess active =`t%procActive%`r`nprocess under =`t%procUnder%`r`nwin cnt hidden`ttoggle for info`r`nwin class active`t%classActive%`r`nwin class under`t%classUnder%`r`nwin id active`t%idActive%`r`nwin id under`t%id%`r`npointer`t`t%A_Cursor%`r`ncontrol active`t%controlActive%`r`ncontrol under`t%controlUnder%`r`ncontrol under, text:`t

tooltip1Info = %tooltipInfo%`r`n%controlText%

tooltip2Info = %tooltipInfo%-bypassed-



if ( GetKeyState("RShift","p") AND GetKeyState("RCtrl","p") )	; toggle window AlwaysOnTop

{

	timeRShiftRCtrl = %A_TickCount%

	WinGet, IDActive, ID, A



	if GuiTopmost

	{

		Gui, -AlwaysOnTop

		GuiTopmost=



		WinW("Ahk Window Spy - ", 3)		; max of 3 sec

		WinActivate, ahk_id %IDActive%

	}

	else

	{

		Gui, +AlwaysOnTop

		GuiTopmost=1

	}

	KeyWait, RCtrl

	if ( A_TickCount - timeRShiftRCtrl > 1200 )	; holding RShift+RCtrl restores default window position

	{

		ws_xGui =

		ws_yGui =



		ifWinExist, Ahk Window Spy - snapshot

			Gui, Show, x-2 y301 w329 h439 AutoSize NoActivate, Ahk Window Spy - snapshot

		else

			Gui, Show, x-2 y301 w329 h439 AutoSize NoActivate, Ahk Window Spy - auto-refresh

	}

}

if GetKeyState("CapsLock","p")		; refresh Snapshot

ifWinExist, Ahk Window Spy - snapshot

{

	ControlSetText, Edit1,, Ahk Window Spy -

	sleep 70

	ControlSetText, Edit1, %Info1Tips%%tooltip1Info%, Ahk Window Spy -

	ControlSend, Edit1, ^{home}, Ahk Window Spy - 

}

if GetKeyState("LCtrl","p")		; temporary deactivate

{

	Tooltip		; tooltip off

	Loop		; prevents updating of values

	{

		sleep 100

		if not GetKeyState("Ctrl","p")

			BREAK

	}

}

if GetKeyState("Esc","p")

{

	SetTimer, PointerWindowInfo, off

	{

		GoTo ws_GuiClose	; avoid GoSub since this timer is turned off ***

		; will EXIT

	}

}



MouseGetPos, xC, yC, id, controlUnder	; relative to screen

WinGet, idActive, ID, A

ControlGetFocus, controlActive, ahk_id %idActive%

WinGetTitle, titleUnder, ahk_id %id%

WinGetTitle, titleActive, A

WinGetClass, classUnder, ahk_id %id%

WinGetClass, classActive, A

WinGetPos, xWinUnder, yWinUnder, widthWinUnder, heightWinUnder, ahk_id %id%

WinGetPos, xWinActive, yWinActive, widthWinActive, heightWinActive, A

WinGet, PIDUnder, PID, ahk_id %id%

WinGet, PIDActive, PID, A

WinGet, ProcUnder, ProcessName, ahk_id %id%

WinGet, ProcActive, ProcessName, A

if flagWindowInfo=1

{

	ifNotInString, titleUnder, AutoHotkey.ini -

		ControlGetText, controlText, %controlUnder%, ahk_id %id%

}

else

	ControlGetText, controlText, %controlUnder%, ahk_id %id%

xCU := xC-xWinUnder	; relative to window under pointer

yCU := yC-yWinUnder	; relative to window under pointer

xCA := xC-xWinActive	; relative to window active

yCA := yC-yWinActive	; relative to window active

xCAm := xWinActive + widthWinActive - xC		; mirror coordinate

yCAm := yWinActive + heightWinActive - yC		; mirror coordinate

xWinActiveM := A_ScreenWidth - xWinActive - widthWinActive   ; mirror coordinate

yWinActiveM := A_ScreenHeight - yWinActive - heightWinActive   ; mirror coordinate

PixelGetColor, color, xC, yC , RGB

;---------------------------------------------

GetKeyState, ksLButton, LButton, p

GetKeyState, ksRButton, RButton, p

if (ksLButton="D" OR ksRButton="D")	;ok

{

	if xD=

	{

		xD = %xC%

		yD = %yC%

		xWin2 = %xWinUnder%

		yWin2 = %yWinUnder%

	}

	xWinDrag := xWin2 - xD + xC	; new window position x

	yWinDrag := yWin2 - yD + yC	; new window position y

}

else

{

	xD=

	yD=

	xWinDrag=

	yWinDrag=

}



;---------------------------	; Display window-info in window edit-control



if flagWindowInfo = 2			; skip tooltip in window mode

	RETURN



if flagWindowInfo = 3

{

	ControlSetText, Edit1, %Info1Tips%%tooltip1Info%, Ahk Window Spy -

	RETURN

}								

;----------------------------; Display window-info in tooltip

if classUnder=tooltips_class32

	Tooltip

else

IfWinActive AutoHotkey.ini -		; handle tooltip-related bugs

{

	typeTooltip=

	IfInString, titleUnder, AutoHotkey.ini -

		typeTooltip=2			; sets which tooltip format

	StringLeft, PointerType, A_Cursor, 4

	if PointerType=Size

		typeTooltip=2

	if controlUnder=

		typeTooltip=2

	if classUnder=Shell_TrayWnd

	{

		if controlUnder<>

			typeTooltip=

		else

			typeTooltip=2

	}

	if typeTooltip=2

		ToolTip, %tooltip2Info%

	else

		ToolTip, %tooltip1Info%

}

else

{

	IfInString, titleUnder, AutoHotkey.ini

	{

		IfWinNotActive, ini - AutoHotkey

			ToolTip, %tooltip2Info%

	}

	else

		ToolTip, %tooltip1Info%

}



Return



;---------------------------------



ws_GuiClose:		; cannot GoTo external subroutine from Function



if WinExist("Ahk Window Spy - ") AND NOT WinExist("Ahk Window Spy - Window Chart")

	WinGetPos, ws_xGui, ws_yGui, ws_wGui, ws_hGui, Ahk Window Spy -		; remember win position & size

SetTimer, PointerWindowInfo, off

sleep 300

Gui, Destroy

flagWindowInfo=

Tooltip

return





WindowChart:



if A_GuiEvent = RightClick

{

	LV_GetText(LV_winId, A_EventInfo, 2)

	msgbox %LV_winId%

	;WinHide, ahk_id %LV_winId%

}

else if A_GuiEvent = DoubleClick ;DoubleClick

{

	LV_GetText(LV_winId, A_EventInfo, 2)

	LV_GetText(isHidden, A_EventInfo, 6)

	if isHidden

		RETURN

	;msgbox %LV_winId%

	;WinShow, ahk_id %LV_winId%

	WinRestore, ahk_id %LV_winId%

	WinActivate, ahk_id %LV_winId%

}

return	



} ;----------------------- End Function: Ahk Window Spy





GuiSize:  ; Allows the control to grow or shrink in response to user's resizing of window.

if A_EventInfo = 1  ; The window has been minimized.  No action needed.

	return

ControlGetFocus, gui_control_ID, A

if gui_control_ID contains Edit,SysListView

	GuiControl, Move, %gui_control_ID%, % "W" . (A_GuiWidth - 17) . " H" . (A_GuiHeight - 8) ; xm=x at margin

return





WinW(windowTitleElement="", secToWait=30)	; WinWait,WinActivate,WinWaitActive

{

	SetTitleMatchMode, 2

	WinWait %windowTitleElement%,, %secToWait%

	WinActivate

	WinWaitActive,,, %secToWait%		; default 30 sec maximum

	ifWinNotActive %windowTitleElement%

		Msgbox, Target window did not activate within %secToWait% sec.`n`nThe originating thread may cause problems if it continues.

	return

}





GuiClose:  ; Indicate that the script should exit automatically when GUI window is closed.

GuiEscape:

; NEXT 2 LINES REPLACE THOSE FROM PREVIOUS VERSION

if WinExist("Ahk Window Spy - ")

	CloseGUI = 1		; let ws_GuiClose function subroutine handle this

Gui Destroy

Tooltip

return





/*

THIS ROUTINE FROM PREVIOUS VERSION MAY BE DELETED



PointerWindowInfoOff:



WinGetPos, ws_xGui, ws_yGui, ws_wGui, ws_hGui, Ahk Window Spy -

Gui, Destroy

sleep 300

flagWindowInfo=

Tooltip



return

*/



;----------------------------------------------------------------

