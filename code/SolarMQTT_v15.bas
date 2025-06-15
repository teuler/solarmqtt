' "SolarMQTT_v15.bas"
' The MIT Licence (MIT)
' Copyright (c) 2023-25 Thomas Euler
'
' Options:
'   OPTION COLOURCODE ON
'   OPTION DISPLAY 64, 80
'   OPTION WIFI "Elchland3", "xxx"
'
' For Adafruit Featherwing 320x240 TFT (ILI9341 TFT):
'   OPTION SYSTEM SPI GP18, GP19, GP16
'   OPTION LCDPANEL ILI9341, L, GP15, GP14, GP13, GP11
'
' For Waveshare 320x240 TFT (ST7789)
'   OPTION SYSTEM SPI GP10,GP11,GP12
'   OPTION SDCARD GP22
'   OPTION LCDPANEL ST7789_320, LANDSCAPE,GP8,GP15,GP9,GP13
'   OPTION TOUCH GP16,GP17
'   GUI CALIBRATE xxx
'
' 2023-04-30 - v0.01, Initial release
' 2023-08-12 - v1.08, Some small fixes
' 2023-09-02 - v1.10, Consider units in MQTT messages
' 2023-09-13 - v1.11, Some small bug fixes
' 2023-10-03 - v1.12, Trying to fix `solarweek.dat`
' 2023-10-03 - v1.13, `solarweek.dat` code fixed; file now uses `\r\n` to
'                     stay compatible with files saved on a Windows PC ...
' 2023-10-21 - v1.14, Clean up logging and debug messages
'                     Keep only one MQTT field entry for units
'                     Add outdoor temperature and humidity
' 2025-06-15 - v1.15, Adapted to MMBasic v6.00.02 (change in Format$)
'
' To grab the program, e.g.,
'   tftp 192.168.178.110 get "SolarMQTT_v14.bas"
' ----------------------------------------------------------------------------
Option Explicit
Option Base 0
Option Escape
Option Autorun 1
Option Colorcode On
Option Display 64, 80

Const DB_MQTT     = &H01
Const DB_BCHART   = &H02
Const DB_WLOG     = &H04
Const DB_DLOG     = &H08
Const DEBUG       = DB_MQTT

'Const LG_INFO     = 1
'Const LG_WARN     = 2
'Const LG_DEBUG    = 3

Const USE_WDOG    = 0
Const USE_FRBUF   = 0
Const USE_TH      = 1
Const TEST_FOD    = 0
Const RECORD_PICS = 0
Const PROG_NAME$  = "SolarMQTT"
Const PROG_VER    = 1.15

' MQTT brocker, topic etc.
Const MQTT_ADDR$  = "192.168.178.20"
Const MQTT_PORT   = 1883
Const MQTT_USER$  = "picomite"
Const MQTT_PW$    = "jumble5645rest"
Const MQTT_TOUT_S = 15.0
Const TOP_SOL_PAN = "solar/panels"
Const FLD_POW     = "pw"   ' power
Const FLD_POW_U   = "pw-u" ' power unit
Const FLD_ENRGY   = "en"   ' energy
Const FLD_ENRGY_U = "en-u" ' energy unit
Const FLD_OVPOW   = "opw"  ' over-power (Shelly plug)
Const FLD_OVHEAT  = "oht"  ' over-heat (Shelly plug)
Const FLD_TH      = "th"   ' temperature, humidity

' Log file-related
Const FNAME_DAY$  = "solarday.dat"
Const DAY_RECLEN  = 25     ' incl. "\n\r"
Const REC_END     = "\r\n"
Const REC_FILL_CH = "_"
Const DAYLOG_MIN  = 2
Const DAYHR1      = 5
Const DAYHR2      = 22
Const DAYLOG_N    = Int((DAYHR2 -DAYHR1)*60/DAYLOG_MIN) +10
Const DAY_REF$    = "2025-01-01"
Const DAY_EP0     = Epoch(Format$(DAYHR1, DAY_REF$ +" %02.0f:00:00"))
Const DAY_EP2     = Epoch(Format$(DAYHR2, DAY_REF$ +" %02.0f:00:00"))
Const DAY_EP2_0   = DAY_EP2- DAY_EP0
Const FNAME_WEEK$ = "solarweek.dat"
Const N_DAYS      = 5
Const MAX_POW_W   = 600
Const POW_CORR    = 1.12
Const MAX_E_KWH   = 4.5
Const ENRGY2_OFFS = 0 ' KWh (before last reset)
Const ENRGY1_OFFS = 0
Const FNAME_PIC$  = "B:/pic%05.0g.bmp"
Const SDCARD_OK   = MM.Info(SDCARD) = "Ready"

' Display-related constants
Const DISPL_UPD_S = 1.0

' Colors
Const C_TXT          = RGB(White)
Const C_TXT_LO       = RGB(0,128,0)
Const C_BKG          = RGB(Black)
Const C_HEART        = RGB(Green)
Const C_GOOD         = RGB(Green)
Const C_FAIR         = RGB(Yellow)
Const C_BAD          = RGB(Orange)
Const C_WORSE        = RGB(Red)

' MQTT message-relate constants
Const SEP_FIELD   = ";"
Const SEP_KEY     = ":"
Const SEP_DATA    = "["
Const SEP_VALS    = ","
Const MAX_FIELDS  = 8 '7
Const MAX_VALUES  = 2
Const FIELD_NUM   = 1
Const FIELD_STR   = 2

' Global variables
Dim integer round = 0, nMsg = 0, nErr = 0, nReconnect = 0, nWDog = 0
Dim float tLastMsg_s = 0, tLastDisplUpdate_s
Dim float tMinOfDay, tPrevMinOfDay = 0, en
Dim string msg$, key$, dat$, fn$, s0$, s1$
Dim integer t0_s, t1_s
Dim integer first_of_day = TEST_FOD
Dim integer iPic = 0, j

Dim float pow(1), pState(1), energy(1), energy_week = 0, energy_day = 0
Dim string pow_unit$, energy_unit$

Dim integer log.pow(DAYLOG_N -1,2), log.epo(DAYLOG_N), log.nDay = 0
Dim float log.energy(N_DAYS-1), log.first_energy = 0
Dim float log.T_C, log.H_percent
Dim string log.wdays$(N_DAYS-1) length 4
Dim string log.last_wlog_date$ length 10
Dim integer log.nWeek = 0

' Define structure to parse an MQTT message
Dim string  msg.topic$ = ""
Dim string  msg.keys$(MAX_FIELDS -1)
Dim string  msg.vals$(MAX_FIELDS -1, MAX_VALUES -1)
Dim integer msg.size(MAX_FIELDS -1)
Dim integer msg.type(MAX_FIELDS -1)
Dim integer msg.n = 0, msg.isProcessing = 0, msg.isNew = 0

' Check CPU speed
_log 1, "CPU running with " +Str$(Val(MM.Info(CPUSPEED)) /1e6) +" MHz"

' Check SDCARD
_log 1, "SD card is " +Choice(SDCARD_OK, "ready", "missing")

' Check WiFi
_log 1, "Checking WiFi ..."
_log 0, "-> " +Choice(isWiFiOk(1), "ok", "not connected")

' Get network time ...
_log 1, "Getting time ..."
On Error Skip 1
WEB NTP 2
_log 0, "-> " +Choice(MM.Errno = 0, "ok", "no success")

' Connect to MQTT server
StartMQTT

' Read logs, if any
readWeekLog
first_of_day = Not(log.last_wlog_date$ = Date$)
If first_of_day Then _log 1, "Is first reading of today ("+Date$+")"
readDayLog

' Getting ready
InitDisplay

' Main loop
Do
  If Not(Date$ = "01-01-2000") Then
    t0_s = Epoch(Date$ +" 00:00:00")
    t1_s = Epoch(Now)
    If (t1_s < (t0_s +DAYHR1 *3600)) Or (t1_s > (t0_s +DAYHR2 *3600)) Then
      ' Do nothing, sleep, it's night
      CLS
      Option Heartbeat Off
      Backlight 0
      WatchDog Off
      Pause 30000
      Print ".";
      first_of_day = 1
      Continue Do
    Else
      Backlight 100
      Option Heartbeat On
    EndIf
  EndIf

  nWDog = MM.Info(BOOT COUNT)
  If USE_WDOG Then WatchDog 20000

  If msg.isNew Then
    ' Parse message and indicate that a new one is ready
    _log 1, "Parse new message ..."
    msg.isProcessing = 1
    tLastMsg_s = Timer /1000
    ParseMsg MM.MESSAGE$

    If msg.n = 0 Then
      _log 0, "-> Message could not be parsed."
      Inc nErr, 1
    Else
      ' Message successfully parsed
      s0$ = "-> Message #" +Str$(nMsg) +" received: "
      s0$ = s0$ +"`" +msg.topic$ +"` "
      s0$ = s0$ +"with " +Str$(msg.n) +" field(s)"
      _log 0, s0$

      ' Extract data ...
      Select Case msg.topic$
        Case TOP_SOL_PAN
          ' Solar panels
          ' Current power reading ...
          pow(0) = getVal(FLD_POW, 0) *POW_CORR
          pow(1) = getVal(FLD_POW, 1) *POW_CORR
          s0$ = getValStr$(FLD_POW_U, 0)
        /*
          s1$ = getValStr$(FLD_POW_U, 1)
          If s0$ <> s1$ Then
            ' Units for current power differs; should not happen
            ' TODO: Handle this possibility
             Print "WARNING: Power units different (";s0$;" vs. ";s1$;")"
          EndIf
        */
          pow_unit$ = s0$

          ' Status of Shelly Plugs (overheating etc.)
          pState(0) = getVal(FLD_OVPOW, 0) Or (getVal(FLD_OVHEAT, 0) << 1)
          pState(1) = getVal(FLD_OVPOW, 1) Or (getVal(FLD_OVHEAT, 1) << 1)

          ' Energy collected so far, considering also the unit
          energy(0) = getVal(FLD_ENRGY, 0) *POW_CORR +ENRGY1_OFFS
          energy(1) = getVal(FLD_ENRGY, 1) *POW_CORR +ENRGY2_OFFS
        /*
          For j=0 To 1
            s0$ = UCase$(getValStr$(FLD_ENRGY_U, j))
            energy(j) = energy(j) * Choice(Left$(s0$, 1) = "M", 1000, 1)
          Next j
        */
          energy_unit$ = getValStr$(FLD_ENRGY_U, 0)

         ' If this is the first reading of the day, log energy collected
          ' so far (=level from the morning)
          If first_of_day Then
            _log 1, "First reading of this day"
            addToWeekLog energy()
            readWeekLog
            Kill FNAME_DAY$
            _log 0, "-> Yesterday's file deleted."
            first_of_day = 0
          EndIf

          ' Calculate energy
          en = Math(Sum energy())
          energy_day = en -log.energy(N_DAYS -1)
          energy_week = en -log.first_energy

          If USE_TH Then
            ' Get temperature and humidity
            log.T_C = getVal(FLD_TH, 0)
            log.H_percent = getVal(FLD_TH, 1)
          EndIf

      End Select
      msg.isNew = 0

      Inc nMsg, 1
    EndIf
    msg.isNew = 0
    msg.isProcessing = 0
  EndIf

  ' Update display, if time is up
  If Timer/1000 > tLastDisplUpdate_s +DISPL_UPD_S Then
    UpdateDisplay
  EndIf

  ' Log values
  tMinOfDay = Int((Epoch(Now) -Epoch(Date$ +" 00:00:01")) /60)
  If ((tMinOfDay Mod DAYLOG_MIN) = 0) And (tMinOfDay <> tPrevMinOfDay) Then
    ' Add to log file every `DAYLOG_MIN` minute ...
    _log 1, "Log now ... (" +DateTime$(Now) +")"
    addToDayLog pow()
    tPrevMinOfDay = tMinOfDay
    If RECORD_PICS And SDCARD_OK Then
      ' Copy screen into file
      iPic = Epoch(DAY_REF$ +" " +Time$) -Epoch(DAY_REF$ +" 00:00:01")
      fn$ = Format$(iPic, FNAME_PIC$)
      _log 0, "Saving image #"+Str$(iPic)+" ('"+fn$+"')"
      Save Image fn$
    EndIf
  EndIf

  ' Check if last message is too long ago ...
  If Timer/1000 > tLastMsg_s +MQTT_TOUT_S Then
    StartMQTT 1
    Inc nReconnect, 1
  Else
    Pause 200
    Inc round, 1
  EndIf

  ' Check for user abort
  key$ = Inkey$
  If Len(key$) > 0 And (Asc(LCase$(key$)) = 113 Or Asc(key$) = 27) Then
    If USE_FRBUF Then FRAMEBUFFER Close F
    WatchDog Off
    End
  EndIf
Loop
End

' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' File log-related routines
'
Sub addToWeekLog _energy(), drv$
  ' Add a record of the energy collected for yesterday
  ' "dd:mm:yyyy,xxxx.xxx_____"
  Local string dat$, lastDate$ = "", d$ = Choice(Len(drv$)=0, "A:", drv$), s$
  Local integer nRec
  Local float energy
  _log 1, "Check solar week log"
  Drive d$
  Open FNAME_WEEK$ For Random As #2
  nRec = Int(Lof(#2) /DAY_RECLEN)
  If nRec > 0 Then
    ' Read last entry to check if there is already one for today
    Seek #2, Lof(#2) -DAY_RECLEN '+1
    lastDate$ = Left$(Input$(DAY_RECLEN, #2), 10)
    _log 0, "Get date of last entry"
    _log 0, lastDate$ +Choice(lastDate$ = Date$, " = ",  "<> ") +Date$
  EndIf
  If Len(lastDate$) = 0 Or Not(lastDate$ = Date$) Then
    ' No entry so far of no entry for today
    _log 0, "Writing entry to week log ..."
    energy = Math(Sum _energy())
    dat$ = Date$ +"," +Str$(energy, 4,3)
    Seek #2, Lof(#2) +1
    Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +REC_END;
    _log 0, dat$ +"' to '" +d$ +"\\" +FNAME_WEEK$ +"' "
    _log 0, "->now " +Str$(Lof(#2)) +" bytes)"
  EndIf
  Close #2
End Sub

Sub readWeekLog drv$
  ' Read last `N_DAYS` solar week log file
  Local string d$ = Choice(Len(drv$) = 0, "A:", drv$), dat$, s$, ld$
  Local integer nRec, i, iRec, iFirst = 1
  Drive d$
  _log 1, "Reading solar week log file ..."
  Open FNAME_WEEK$ For Random As #2
  nRec = Int(Lof(#2) /DAY_RECLEN)
  log.last_wlog_date$ = ""
  If DEBUG And DB_WLOG Then _log 3, "nRec="+Str$(nRec)
  If nRec > 0 Then
    ' Read entries
    log.nWeek = 0
    For i=0 To N_DAYS -1
      iRec = nRec -(N_DAYS -i)
      If DEBUG And DB_WLOG Then Print "iRec=";iRec
      If iRec >= 0 Then
        If DEBUG And DB_WLOG Then Print "i=";i
        Seek #2, iRec *DAY_RECLEN +1 +iRec
        dat$ = Input$(DAY_RECLEN-1, #2)
        ld$ = Left$(dat$, 10)
        If DEBUG And DB_WLOG Then Print "dat$='"+dat$+"' iFirst=";iFirst
        If iFirst Then
          ' First entry read; use as baseline
          log.first_energy = Val(Field$(dat$, 2, ","))
          If DEBUG And DB_WLOG Then Print "log.first_energy=";log.first_energy
          iFirst = 0
        EndIf
        ' Process entries
        log.energy(i) = Val(Field$(dat$, 2, ","))
        log.wdays$(i) = Left$(Day$(ld$), 2)
        s$ = "#" +Str$(log.nWeek) +": " +Left$(dat$, 10) +" "
        s$ = s$ +Str$(log.energy(i),0,3) +" "
        s$ = s$ +energy_unit$ +" (" +log.wdays$(i) +")"
        _log 0, s$
        Inc log.nWeek, 1
      EndIf
    Next i
    log.last_wlog_date$ = ld$
  EndIf
  _log 0, "-> " +Str$(log.nWeek +1) +" record(s) read."
  Close #2
End Sub


Sub addToDayLog _pow(), drv$
  ' Add a record to the solar day log file
  ' "yy:yy:yy,xxx,xxx        "
  Local dat$, d$ = Choice(Len(drv$) = 0, "A:", drv$)
  Drive d$
  _log 1, "Check solar day log"
  If _pow(0) < 0.1 And _pow(1) < 0.1 Then
    _log 0, "Zero values not written to log."
    Exit Sub
  EndIf
  Open FNAME_DAY$ For Random As #1
  If Lof(#1) = 0 Then
    ' File is empty, start file with today's date
    _log 0, "Starting log file ..."
    dat$ = Date$
    Print #1, dat$ +Space$(DAY_RECLEN -1 -Len(dat$)) +REC_END;
  EndIf
  dat$ = Time$ +SEP_VALS +Str$(_pow(0),3,0) +SEP_VALS +Str$(_pow(1),3,0)
  _log 0, "Writing '" +dat$ +"' to '" +d$ +"\\" +FNAME_DAY$ +"'"
  Print #1, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +REC_END;
  log.pow(log.nDay,0) = _pow(0)
  log.pow(log.nDay,1) = _pow(1)
  log.pow(log.nDay,2) = Math(Sum _pow())
  log.epo(log.nDay) = Epoch(DAY_REF$ +" " +Time$) -DAY_EP0
  Inc log.nDay, 1
  _log 0, "-> now " +Str$(Lof(#1)) +" bytes)"
  Close #1
End Sub


Sub readDayLog drv$
  ' Read solar day log file
  Local string d$ = Choice(Len(drv$) = 0, "A:", drv$), dat$, s$
  Local integer n, nRec, i, ep1
  Drive d$
  _log 1, "Reading solar day log file ..."
  Open FNAME_DAY$ For Random As #1
  n = Lof(#1)
  log.nDay = 0
  nRec = Int(n /DAY_RECLEN)
  If nRec > 0 Then
    '' Check if the file is from today
    'Seek #1, 1
    'dat$ = Left$(Input$(Day_RECLEN, #1), 10)
    ''Print "'"+dat$+"'", "'"+Date$+"'", dat$ = Date$
    'If dat$ = Date$ Or Date$ = "01-01-2000" Then
    '  ' It is today's file (or the current date has not yet been determined);
    '  ' Read values ...
      For i=1 To nRec-1
        If DEBUG And DB_DLOG Then Print "i=";i, i*DAY_RECLEN +1
       'Seek #1, i *DAY_RECLEN +1
       'Seek #1, i *DAY_RECLEN +i ' 2023-09-28 ??
        Seek #1, i *DAY_RECLEN +1 +i
        dat$ = Input$(DAY_RECLEN -1, #1)
        If DEBUG And DB_DLOG Then Print " dat$='";dat$;"'", Len(dat$)
        log.epo(i-1) = 0
        s$ = DAY_REF$ +" " +Left$(dat$, 8)
        If DEBUG And DB_DLOG Then Print "s$='";s$;"'",Len(s$),Asc(s$)
        If Len(s$) > 18 And Asc(dat$) > 0 Then
          ' Try to deal with errors in file caused by firmware crashes ...
          log.epo(i-1) = Epoch(s$) -DAY_EP0
          log.pow(i-1,0) = Val(Field$(dat$, 2, ","))
          log.pow(i-1,1) = Val(Field$(dat$, 3, ","))
          log.pow(i-1,2) = log.pow(i-1,0) +log.pow(i-1,1)
        Else
          '_log 4, "Entry "+Str$(i)+" seems damaged"
          'Print Len(s$), Asc(dat$)
        EndIf
        Inc log.nDay, 1
      Next i
    'Else
    '  ' It's an old file; kill it such that `addToDayLog` creates a new one
    '  Close #1
    '  Kill FNAME_DAY$
    '  Print "Yesterday's file deleted."
    '  Exit Sub
    'EndIf
  EndIf
  _log 0, "-> " +Str$(log.nDay) +" record(s) read."
  Close #1
End Sub

' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' Display-related routines
'
Sub InitDisplay
  ' Initialize display
  SplashScreen 1000
  CLS
  If USE_FRBUF Then
    On Error Skip 1
    FRAMEBUFFER Close F
    FRAMEBUFFER Create
  EndIf
  UpdateDisplay
End Sub


Sub UpdateDisplay
  ' Update display
  Local integer x, y, dx, dx2, dy=16, gp=16, x1, x2, y1, fw
  Local integer col, cbk, col1, cbk1, c1, c2, dy2=dy*2+gp, dy3, i,j, pw
  Local integer xp(2), yp(2), nw
  Local string s$, v$
  Local float enrgy, dxf

  If USE_FRBUF Then FRAMEBUFFER Write F

  ' Solar power (as number)
  x = 2
  y = 2
  x1 = x +52
  dx = Int(MM.HRES /2)
  col = RGB(green)
  cbk = RGB(0,128,0)
  col1 = RGB(white)
  cbk1 = RGB(red)

  Box x-2,y-2, dx-2, dy*3+4, 1, C_BKG,cbk
  _text x,y, "Power",col,cbk,4
  _text x,y+dy, "[" +pow_unit$ +"]",col,cbk,4
  _text x,y+dy*2, "total",col,cbk,4
  s$ = Str$(pow(0) +pow(1), 3, 0)
  _text x1,y-1, s$,col,cbk,6

  ' Solar power state (overpower, overheating; Shelly!!)
  Inc x, dx+4
  x2 = x +102
  Box x-2,y-2, dx-2, dy*3+4, 1, C_BKG,cbk
  _text x,y, "Panels   OP OH", col,cbk,4
  For i=0 To 1
    s$ = "#" +Str$(i+1) +" " +Str$(pow(i), 3,1)
    _text x,y+dy, s$, col,cbk,4
    If pState(i) And 1 Then
      _text x2,y+dy, String$(3,158), col1,cbk1,4,1
    Else
      _text x2,y+dy, Chr$(251), col,cbk,4,1
    EndIf
    If pState(i) And 2 Then
      _text x2+32,y+dy, ">"+Chr$(96)+"C", col1,cbk1,4,1
    Else
      _text x2+32,y+dy, Chr$(251), col,cbk,4,1
    EndIf
    Inc y, dy
  Next i

  ' Solar power (as graph, day)
  ' 6 - 8,12,16,20 - 22
  x = 2
  y = dy*4 -8
  dx = MM.HRES/5*3 +1
  dy3 = MM.VRES -y -66
  dxf = dx /(DAYHR2 -DAYHR1)
  For i=DAYHR1 To DAYHR2
    x1 = Int(2 +(i-DAYHR1) *dxf)
    Line x1,y+dy3,x1,y+dy3+3,1,Choice(i Mod 4 = 0, col, cbk)
    If i Mod 4 = 0 Then
      _text x1,y+dy3+5, Str$(i)+"h",,,1,1
    EndIf
  Next i

  Box x,y,dx,dy3, 1,cbk,RGB(black)

  nw = Epoch(DAY_REF$ +" " +Time$) -DAY_EP0
  x1 = x +Int(dx /DAY_EP2_0 *nw)
  Line x1,y,x1,y+dy3,1,cbk

  y1 = Int(y+dy3 -dy3/3)
  pw = MAX_POW_W/3
  Line x,y1,x+dx,y1, 1,cbk
  Text x+dx-2,y1, Str$(pw,3,0), "RB",1
  y1 = Int(y+dy3 -dy3/3*2)
  pw = MAX_POW_W/3*2
  Line x,y1,x+dx,y1, 1,cbk
  Text x+dx-2,y1, Str$(pw,3,0), "RB",1

  Math Set x, xp()
  Math Set y +dy3 -1, yp()
  For i=0 To log.nDay-1
    If log.pow(i,2) > 0 Then
      For j=0 To 2
        x1 = x +Int(dx /DAY_EP2_0 *log.epo(i))
        y1 = y +dy3 -Int(dy3/MAX_POW_W *log.pow(i,j))
        Pixel x1,y1, Choice(j<2, col, col1)
        xp(j) = x1
        yp(j) = y1
      Next j
    EndIf
  Next i

  ' Collected energy as graph (week)
  x = dx +8
  dx = (MM.HRES -x -5*(N_DAYS -1)) /N_DAYS
  If DEBUG And DB_BCHART Then Print "log.nWeek=";log.nWeek
  For i=0 To N_DAYS -1
    x1 = x +i*(dx+4)
    Box x1,y,dx,dy3, 1,cbk,cbk
    If DEBUG And DB_BCHART Then Print "i=";i
    s$ = log.wdays$(i)
    If Len(s$) > 0 Then
      If i = N_DAYS -1 Then
        ' Today ...
        enrgy = energy_day
        If DEBUG And DB_BCHART Then Print "'";s$;"' (today), energy_day=";energy_day
      Else
        ' Earlier ...
        enrgy = log.energy(i+1) -log.energy(i)
        If DEBUG And DB_BCHART Then Print "'";s$;"' enrgy=";enrgy
      EndIf
      _text x1+Int(dx/2) +1,y+dy3+5, s$,,c_BKG,1,1
      dy = Int(enrgy /MAX_E_KWH *dy3)
      y1 = y +dy3 -dy
      Box x1,y1,dx,dy, 1,C_TXT,cbk
      Text x1+4,y+dy3-8, Str$(enrgy,1,2),"LTU",1,,col,-1
    EndIf
  Next i

  ' Status line(s)
  Font 1
  fw = MM.Info(FONTWIDTH)
  dx = Int(MM.HRES /6)
  dy = 15
  x = 2
  y = MM.VRES -dy

  s$ = getDateTime$()
  If USE_TH Then
    s$ = s$ +" T=" +Str$(log.T_C, -2, 0) +Chr$(96) +"C"
    s$ = s$ +" H=" +Str$(log.H_percent) +"%"
  EndIf
  _text MM.HRES/2,y, s$,,,1,1
  Inc y, -(dy*2 +2)

  For i=0 To 5
    s$ = ""
    x1 = x +dx*i
    x2 = x1 +dx/2 -1
    Box x1,y, dx-2,dy*2, 1, C_BKG,C_TXT_LO
    Select Case i
      Case 0: s$ = "error": v$ = Str$(nErr)
      Case 1: s$ = "cnnct": v$ = Str$(nReconnect)
      Case 2: s$ = "msgs" : v$ = Str$(nMsg)
      Case 3: s$ = "rboot": v$ = Str$(nWDog)
      Case 4: s$ = "recs" : v$ = Str$(log.nDay)
      Case 5: s$ = "kWh"  : v$ = Str$(energy_week,0,0)
    End Select
    If Len(s$) > 0 Then
      _text x2,y, s$,,C_TXT_LO,1,1
      _text x2,y+dy, v$,,C_TXT_LO,1,1
    EndIf
  Next i

  If USE_FRBUF Then FRAMEBUFFER Copy F,N
  tLastDisplUpdate_s = Timer /1000
End Sub


Sub _text x%,y%, s$, c,bc, f%, ctr
  Local string a$ = Choice(ctr, "CT", "LT")
  Local integer _x = x%, _y = y%, _f = Choice(f% = 0, 3, f%)
  Local integer _fc = Choice(c = 0, C_TXT, c)
  Local integer _bc = Choice(bc = 0, C_BKG, bc)
  Text _x,_y, s$, a$,_f,, _fc,_bc
End Sub

Function _color(v, v1,v2,v3)
  If v < v1 Then _color = C_GOOD: Exit Function
  If v < v2 Then _color = C_FAIR: Exit Function
  If v < v3 Then _color = C_BAD: Exit Function
  _color = C_WORSE
End Function

Function getDateTime$() As string
  getDateTime = Left$(Day$(Now), 3) +" " +Date$ +" " +Time$
End Function


Sub SplashScreen(t_ms)
  ' Splash screen
  Local integer x1,y1, fh
  Local string s$
  CLS
  Font #2, 1
  fh = MM.Info(FONTHEIGHT) +2
  x1 = Int(MM.HRES/2 -1)
  y1 = Int(MM.VRES/2 -1) -2*fh
  _text x1,y1, PROG_NAME$, ,,2,1
  s$ = "v" +Str$(PROG_VER, 1,2)
  _text x1,y1+fh, s$, ,,2,1
  _text x1,y1+fh*2, "MMBasic", ,,2,1
  _text x1,y1+fh*3, "v" +Str$(MM.VER, 1,4), ,,2,1
  Pause t_ms
End Sub

' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' MQTT-related routines
'
Sub StartMQTT restart
  ' (Re)connect to MQTT server
  _log 1, Choice(restart, "Restart", "Start") +" MQTT client ..."
  On Error Skip 1
  WEB MQTT Close
  On Error Skip 1
  WEB MQTT Connect MQTT_ADDR$, MQTT_PORT, MQTT_USER$, MQTT_PW$, cb_MQTT
  Local integer errno = MM.Errno
  If errno = 16 Then
    _log 4, "Not connected? ("+Str$(errno)+")"
    _doReboot
  EndIf
  WEB MQTT Subscribe TOP_SOL_PAN
  round = 0
  tLastMsg_s = Timer /1000
  _log 0, "-> ok"
End Sub


Sub cb_MQTT
  ' MQTT callback - called when a message to a subscribed topic arrived
  Local integer count = 0, flip = 0
  If DEBUG And DB_MQTT Then
    Print "MM.Topic$='";MM.TOPIC$
    Print "MM.Message$='";MM.MESSAGE$
  EndIf

  ' Wait until an earlier message is processed
  Do While msg.isProcessing
    If (count Mod 25) = 0 Then
      Print Choice(flip, "\&0d-", "\&0d|");
      flip = Not(flip)
    EndIf
    Inc count, 1
    Pause 20
  Loop

  ' Indicate that a new message for topic `msg.topic$` is ready to parse
  msg.topic$ = MM.TOPIC$
  msg.isNew = 1
End Sub


Function getValStr$(key$, index) As string
  ' Get `index` value of field named `key$` as string
  Local integer i
  Local string s$ = ""
  getValStr$ = ""
  If msg.n = 0 Then Exit Function
  For i=0 To msg.n
    If UCase$(msg.keys$(i)) = UCase$(key$) Then
      If index >= 0 And index < msg.size(i) Then
        s$ = msg.vals$(i,index)
        If s$ = "off" Or s$ = "on" Then
          getValStr$ = Choice(s$ = "on", "1", "0")
        Else
          getValStr$ = s$
        EndIf
      EndIf
      End Function
    EndIf
  Next i
End Function


Function getVal(key$, index) As Float
  ' Get `index` value of field named `key$` as float
  getVal = Val(getValStr$(key$, index))
End Function


Sub ParseMsg s$
  ' Parse a message `s$` into the `msg.xxx` structure
  Local string tmp$ = s$, rmd$, dta$
  Local integer i,j,k, n = 0, m
  msg.n = 0
  Do
    ' Find fields (delimited by SEP_FIELD)
    i = Instr(tmp$, SEP_FIELD)
    If i > 0 Then
      ' Field found ...
      rmd$ = Trim$(Mid$(tmp$, i+1))
      tmp$ = Trim$(Left$(tmp$, i-1))

      ' Parse field ...
      If DEBUG And DB_MQTT Then Print "fields:";n, tmp$
      j = Instr(tmp$, SEP_KEY)
      If j <= 0 Then
        ' Message corrupt ...
        msg.n = 0
        Exit Sub
      EndIf
      msg.keys$(n) = Trim$(Left$(tmp$, j-1))
      dta$ = Trim$(Mid$(tmp$, j+1))
      m = 0
      If Left$(dta$, 1) = SEP_DATA Then
        ' Multiple values for this field (key)
        dta$ = Trim$(Mid$(dta$, 2, Len(dta$)-2))
        Do
          k = Instr(dta$, SEP_VALS)
          If k > 0 Then
            msg.vals$(n,m) = Trim$(Left$(dta$, k-1))
            dta$ = Trim$(Mid$(dta$, k+1))
            Inc m, 1
          EndIf
        Loop Until k = 0
      EndIf
      msg.vals$(n,m) = dta$
      msg.size(n) = m +1

      ' Check type
      k = Val(msg.vals$(n,0))
      If k = 0 And Not(Str$(k) = msg.vals$(n,0)) Then
        msg.type(n) = FIELD_STR
      Else
        msg.type(n) = FIELD_NUM
      EndIf

      Inc n, 1
      tmp$ = rmd$
    EndIf
  Loop Until i = 0
  msg.n = n
End Sub

' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' WiFI-related routines
'
Function isWiFiOk(reboot)
  ' Check if WiFi connected and restart if `reboot` = 1
  Local integer state = MM.Info(WiFi status)
  Local string s$
  isWiFiOk = 0
  Select Case state
    Case 0  : s$ = "Link is down"
    Case 2  : s$ = "Connected w/o valid IP address"
    Case 1,3: s$ = "Connected to " +MM.Info(IP Address): isWiFiOk = 1
    Case -1 : s$ = "Connection failed"
    Case -2 : s$ = "SSID not found"
    Case -3 : s$ = "Authentification failed"
  End Select
  _log 0, "->" +s$
  If isWiFiOk Or Not(reboot) Then Exit Function

  ' WiFi is not ok and user wants to reboot; do so ...
  _doReboot
End Function

Sub _doReboot
  ' Reboot after a few seconds
  Local integer i = 3
  Do While i >= 0
    Print "\&0dRestarting in " +Str$(i) +" secs ...";
    Pause 1000
    Inc i, -1
  Loop
  Print
  Option Autorun 1
  CPU restart
End Function

' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
' Helper routines
'
Function Trim$(s$, ch$)
  ' Trims characters in `ch$` from string `s%`
  Trim$ = s$
  If Len(ch$) = 0 Then ch$ = " "
  Do While Instr(ch$, Right$(Trim$, 1))
    Trim$ = Mid$(Trim$, 1, Len(Trim$) -1)
  Loop
  Do While Instr(ch$, Left$(Trim$, 1))
    Trim$ = Mid$(Trim$, 2)
  Loop
End Function

Sub _log mode, msg$
  ' Prints a message string
  If mode = 1 Then
    Print "INFO|"+msg$
  ElseIf mode = 2 Then
    Print "WARN|"+msg$
  ElseIf mode = 3 Then
    Print "DBG |"+msg$
  ElseIf mode = 4 Then
    Print "ERR |"+msg$
  Else
    Print "     "+msg$
  EndIf
End Sub

' ----------------------------------------------------------------------------                        