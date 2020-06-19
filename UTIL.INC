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
        IF A$ = CHR$(0)+CHR$(83) THEN          'DELETE
          X$ = MID$(X$,1,(X1%-X%))+MID$(X$,(X1%-X%+2),(N%-(X1%-X%)-1))+" "
          LOCATE Y%, X%
          PRINT X$
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

