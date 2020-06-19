
REM  FUNCTIONS
'--------------------------------------------------------------'
DEF FNBOX$(X%,Y%)
  LOCAL X1%, Y1%
  X1%=POS:Y1%=CSRLIN
  PRINT CHR$(218);
  FOR I% = 1 TO X%-1
    PRINT CHR$(196);
  NEXT I%
  PRINT CHR$(191)
  FOR I% = 1 TO Y%-1%
    LOCATE (Y1%+I%),(X1%):PRINT CHR$(179)
  NEXT I%
  FOR I% = 1 TO Y%-1%
    LOCATE (Y1%+I%),(X1%+X%):PRINT CHR$(179)
  NEXT I%
  LOCATE (Y1%+Y%),(X1%):PRINT CHR$(192);
  FOR I% = 1 TO X%-1
    PRINT CHR$(196);
  NEXT I%
  PRINT CHR$(217)
END DEF


SUB PROM(X$,N%,N1%)
      LOCAL X%, X1%, Y%, A$, I%, A1$
      I% = 0
      IF LEN(X$)>N% THEN N% = LEN(X$)
      X% = POS
      Y% = CSRLIN
      X$ = X$ + SPACE$(N%-LEN(X$))
      COLOR 14,1
      PRINT X$
      LOCATE Y%, X%
      X1%=X%
      IF N1%>1 THEN
        IF N1%>N% THEN N1%=N%
        X1%=X1%+N1%-1
        IF X1%>(X%+N%-1) THEN X1% = X1%-1
        LOCATE Y%, X1%
      END IF
      IN:
        A1$ = MID$(X$, (X1%-X%+1), 1)
        COLOR 15,0
        PRINT A1$
        LOCATE Y%, X1%
        COLOR 14,1
        A$ = INKEY$

        IF A$ = CHR$(0) THEN GOTO IN

        IF A$ = CHR$(13) THEN GOTO BOTTOM

        IF A$ = CHR$(0)+CHR$(77) THEN          'MOVE RIGHT
          PRINT A1$
          X1%=X1%+1
          IF X1%>(X%+N%-1) THEN X1% = X1%-1
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(75) THEN          'MOVE LEFT
          PRINT A1$
          X1%=X1%-1
          IF X1% < X% THEN X1% = X%
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(82) THEN          'INSERT
          A1$ = " "
          X$ = MID$(X$,1,(X1%-X%))+A1$+MID$(X$,(X1%-X%+1),(N%-(X1%-X%)-1))
          LOCATE Y%, X%
          PRINT X$
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(83) THEN          'DELETE CHARACTER
          X$ = MID$(X$,1,(X1%-X%))+MID$(X$,(X1%-X%+2),(N%-(X1%-X%)-1))+" "
          LOCATE Y%, X%
          PRINT X$
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(25) THEN                      'DELETE LINE
          X$ = SPACE$(N%)
          LOCATE Y%, X%
          PRINT X$
          X1%=X%
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(08) THEN                  'DESTRUCTIVE BACKSPACE
          A1$ = " "
          X1%=X1%-1
          IF X1% < X% THEN X1% = X%
          X$ = MID$(X$,1,(X1%-X%))+A1$+MID$(X$,(X1%-X%+2),(N%-(X1%-X%)-1))
          LOCATE Y%, X%:PRINT X$
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF LEN(A$) = 1 THEN                    'ENTER CHARACTER
          X$ = MID$(X$,1,(X1%-X%))+A$+MID$(X$,(X1%-X%+2),(N%-(X1%-X%)-1))
          LOCATE Y%, X%
          PRINT X$
          X1%=X1%+1
          IF X1%>(N%+X%-1) THEN X1% = X1%-1
          LOCATE Y%, X1%
          GOTO IN
        END IF

        GOTO IN
  BOTTOM:
  COLOR 7,0
  LOCATE Y%, X%
  PRINT X$
  X1% = 1
  Y1% = Y%+1
  LOCATE Y1%, X1%
END SUB



DEF FNVEL#(S#,N%)
  LOCAL VEL#
  IF S#<.5 THEN S#=.5
  IF S#>30 THEN S#=30
  RESTORE SLOPEDATA
  FOR K2%=1 TO 21
    READ S5#(K2%)
  IF S5#(K2%)<S# THEN NEXT K2%
  SLOPEDATA:
    DATA .5,.6,.7,.8,.9,1,1.5,2,2.5,3,3.5,4,5,6,7,8,9,10,15,20,30
  ON N% GOTO WD, FW, GR, BA, WT, PV
  WD:
    RESTORE WDDATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  WDDATA:
    DATA .175,.19,.208,.22,.235,.25,.31,.35,.4,.43,.47,.5,.56,.62,.67,.71,.75,.8,.98,1.13,1.4
    GOTO CALC1
  FW:
    RESTORE FWDATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  FWDATA:
    DATA .33,.37,.39,.42,.45,.47,.58,.66,.74,.82,.88,.95,1.05,1.15,1.25,1.32,1.4,1.5,1.8,2.1,2.6
    GOTO CALC1
  GR:
    RESTORE GRDATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  GRDATA:
    DATA .5,.55,.59,.63,.67,.71,.86,1,1.11,1.22,1.3,1.4,1.55,1.7,1.85,2,2.1,2.2,2.7,3.2,3.8
    GOTO CALC1
  BA:
    RESTORE BADATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  BADATA:
    DATA .7,.77,.83,.88,.91,1,1.2,1.4,1.6,1.7,1.85,2,2.2,2.4,2.6,2.8,3,3.2,3.8,4.45,5.5
    GOTO CALC1
  WT:
    RESTORE WTDATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  WTDATA:
    DATA 1.08,1.18,1.27,1.35,1.45,1.51,1.85,2.15,2.4,2.65,2.85,3.1,3.4,3.7,4.1,4.3,4.6,4.8,5.9,6.8,8.4
    GOTO CALC1
  PV:
    RESTORE PVDATA
    FOR K3%=1 TO K2%
      READ V5#(K3%)
    NEXT K3%
  PVDATA:
    DATA 1.4,1.55,1.65,1.8,1.9,2,2.4,2.8,3.2,3.45,3.75,4,4.5,4.8,5.1,5.6,6,6.3,7.75,9,11
  CALC1:
    D#=((S#-S5#(K2%-1))/(S5#(K2%)-S5#(K2%-1)))
    IF S#=.5 THEN VEL#=V5#(K2%)
    IF S#>.5 THEN VEL#=V5#(K2%-1)+D#*(V5#(K2%)-V5#(K2%-1))
    FNVEL# = VEL#
END DEF






