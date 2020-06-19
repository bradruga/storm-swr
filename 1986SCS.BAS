DIM L$(300,4), S$(300,4), T1$(300,4), Z1$(300,4), B$(300,4),_
          DEP$(300,4), T#(300,4), U%(300,4)

TIMEC1$="JUNE 1986 SCS METHOD"

MAINPROGRAM:
  I4%=1:P2#=3.5
  GOSUB ENTER1986CONCENTRATIONTIME
END          	

HEADING:
  CLS
  SCREEN 0
  COLOR 15,0,0
  LOCATE 1,29
  PRINT"STORM SEWER ANALYSIS"
  COLOR 7,0,0
RETURN


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
      FOR I%=1 TO N%
        IF MID$(X$,N%+1-I%,1)=" " THEN EXIT FOR
      NEXT I%
      X% = POS
      Y% = CSRLIN
      X$ = MID$((X$ + SPACE$(I%-1)),1,N%)
      COLOR 14,1
      I% = 0
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
        LOCATE 21,78
        COLOR 0,0
        PRINT A1$
        LOCATE Y%, X1%
        COLOR 15,0
        PRINT A1$
        LOCATE Y%, X1%
        COLOR 14,1
        A$ = INKEY$

        IF A$ = CHR$(0) THEN GOTO IN

        IF A$ = CHR$(9) THEN GOTO IN

        IF A$ = CHR$(13) THEN GOTO BOTTOM

        IF A$ = CHR$(0)+CHR$(77) THEN          'MOVE RIGHT
          PRINT A1$
          X1%=X1%+1
          IF X1%>(X%+N%-1) THEN X1% = X%+N%-1
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

        IF A$ = CHR$(0)+CHR$(116) THEN         'MOVE RIGHT 5 SPACES
          PRINT A1$
          X1%=X1%+5
          IF X1%>(X%+N%-1) THEN X1% = X%+N%-1
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(115) THEN         'MOVE LEFT 5 SPACES
          PRINT A1$
          X1%=X1%-5
          IF X1% < X% THEN X1% = X%
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(71) THEN         'HOME
          PRINT A1$
          X1%=X%
          LOCATE Y%, X1%
          GOTO IN
        END IF

        IF A$ = CHR$(0)+CHR$(79) THEN          'END
          PRINT A1$
          FOR I%= N% TO 1 STEP -1
            IF MID$(X$,I%,1) <> " " THEN
              X1% = X%+I%
              IF X1%>(X%+N%-1) THEN X1%=X%+N%-1
              EXIT FOR
            END IF
          NEXT I%
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








ENTER1986CONCENTRATIONTIME:
  ON ERROR GOTO 0
  CLS:GOSUB HEADING:J%=1:N2%=6
  LOCATE 3,34:PRINT "SUBAREA ";A$(I2%)
  LOCATE 4,17:PRINT "TIME OF CONCENTRATION - JUNE 1986 SCS METHOD"
  LOCATE 5,1 :PRINT FNBOX$(79,15)
  START1986:
    GOSUB OVERLAND
    IF J%=5 THEN RETURN
    GOSUB CONCENTRATED
    IF J%=5 THEN RETURN
    GOSUB CHANNEL
    IF J%=5 THEN RETURN
    GOSUB OTHER
    IF J%=5 THEN RETURN
  LOCATE 22,1:PRINT "ENTER MORE DATA? ";:Y1$="N":CALL PROM(Y1$,1,1)
  IF Y1$="Y" THEN
    GOSUB CLEAR1
    GOTO START1986
  END IF
RETURN

OVERLAND:
  LOCATE 21,1:PRINT "SHEET FLOW"
  IF VAL(L$(I4%,J%))=0 THEN L$(I4%,J%)="300  "
  LOCATE 22,1:PRINT "LENGTH (FT) = ";:CALL PROM(L$(I4%,J%),5,1)
  L1#=VAL(L$(I4%,J%))
  IF L1#=0 THEN
    GOSUB CLEAR1
    RETURN
  END IF
  IF L1#<1 OR L1#>300 THEN
    LOCATE 19,13:COLOR 15:PRINT"LENGTH MUST BE BETWEEN 1 FT AND 300 FT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                          "
    IF L1#<1 THEN L$(I4%,J%)="1    "
    IF L1#>300 THEN L$(I4%,J%)="300  "
    GOTO OVERLAND
  END IF
  SLOPELABEL1:
  IF VAL(S$(I4%,J%))=0 THEN S$(I4%,J%)="0.5  "
  LOCATE 22,22:PRINT "SLOPE (%) = ";:CALL PROM(S$(I4%,J%),5,1)
  S1#=VAL(S$(I4%,J%))
  IF S1#<.1 THEN
    LOCATE 19,13:COLOR 15:PRINT"SLOPE MUST BE EQUAL TO OR GREATER THAN 0.1 PERCENT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                    "
    S$(I4%,J%)="0.1  "
    GOTO SLOPELABEL1
  END IF
  MANNLABEL1:
  COLOR 7,0
  ON KEY(1) GOSUB PRINTMANNHELP
  KEY(1) ON
  LOCATE 22,41:PRINT "ENTER MANNING'S N (F1 - HELP): ";
  IF VAL(T1$(I4%,J%))=0 THEN T1$(I4%,J%)="0.15      "
  CALL PROM(T1$(I4%,J%),5,3)
  IF PRINTMANNFLAG%=1 THEN GOSUB CLEARMANNHELP
  MANN#=VAL(T1$(I4%,J%))
  IF MANN#<.011 OR MANN#>.8 THEN
    LOCATE 19,13:COLOR 15:PRINT"MANNING'S N MUST BE BETWEEN 0.011 AND 0.8!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                "
    IF MANN#<.011 THEN T1$(I4%,J%)="0.011"
    IF MANN#>.8 THEN T1$(I4%,J%)="0.8  "
    GOTO MANNLABEL1
  END IF
  T#(I4%,J%)=((((MANN#*L1#)^.8)*.007)/((P2#^.5)*((S1#/100)^.4)))*60
  LOCATE N2%,6:PRINT"SHEET FLOW"
  LOCATE N2%+1,10:PRINT"LENGTH =";L1#; "FT,  ";
  PRINT "SLOPE ="; S1#; "%,  ";
  PRINT "N = "; T1$(I4%,J%); TAB(62); "Tc = ";
  PRINT USING "###.##"; T#(I4%,J%);: PRINT " MIN"
  U%(I4%,J%)=1
  N2%=N2%+3:J%=J%+1
  GOSUB CLEAR1
RETURN

CONCENTRATED:
  LOCATE 21,1:PRINT "SHALLOW CONCENTRATED FLOW"
  LOCATE 22,1:PRINT "LENGTH (FT) = ";:CALL PROM(L$(I4%,J%),5,1)
  L1#=VAL(L$(I4%,J%))
  IF L1#=0 THEN
    GOSUB CLEAR1
    RETURN
  END IF
  IF L1#<1 OR L1#>10000 THEN
    LOCATE 19,13:COLOR 15:PRINT"LENGTH MUST BE BETWEEN 1 FT AND 10000 FT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                            "
    IF L1#<1 THEN L$(I4%,J%)="1    "
    IF L1#>10000 THEN L$(I4%,J%)="10000"
    GOTO CONCENTRATED
  END IF
  SLOPELABEL2:
  LOCATE 22,24:PRINT "SLOPE (%) = ";:CALL PROM(S$(I4%,J%),5,1)
  S1#=VAL(S$(I4%,J%))
  IF S1#<.1 THEN
    LOCATE 19,13:COLOR 15:PRINT"SLOPE MUST BE EQUAL TO OR GREATER THAN 0.1 PERCENT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                    "
    S$(I4%,J%)="0.1  "
    GOTO SLOPELABEL2
  END IF
  SURFACELABEL1:
  LOCATE 22,45:PRINT "SURFACE (U-UNPAVED, P-PAVED): ";:T1$(I4%,J%)="U"
  CALL PROM(T1$(I4%,J%),1,1)
  IF T1$(I4%,J%)<>"U" AND T1$(I4%,J%)<>"P" THEN SURFACELABEL1:
  IF T1$(I4%,J%)="U" THEN T#(I4%,J%)=L1#/(60*(16.1345*(S1#/100)^.5))
  IF T1$(I4%,J%)="P" THEN T#(I4%,J%)=L1#/(60*(20.3282*(S1#/100)^.5))
  LOCATE N2%,6:PRINT"SHALLOW CONCENTRATED FLOW"
  LOCATE N2%+1,10:PRINT"LENGTH =";L1#; "FT,  ";
  PRINT "SLOPE ="; S1#; "%,  ";
  IF T1$(I4%,J%)="U" THEN PRINT "UNPAVED";
  IF T1$(I4%,J%)="P" THEN PRINT "PAVED";
  PRINT TAB(62); "Tc = ";
  PRINT USING "###.##"; T#(I4%,J%);: PRINT " MIN"
  U%(I4%,J%)=2
  N2%=N2%+3:J%=J%+1
  GOSUB CLEAR1
RETURN

CHANNEL:
  LOCATE 21,1:PRINT "CHANNEL FLOW"
  LOCATE 22,1:PRINT "LENGTH (FT) = ";:CALL PROM(L$(I4%,J%),5,1)
  L1#=VAL(L$(I4%,J%))
  IF L1#=0 THEN
    GOSUB CLEAR1
    RETURN
  END IF
  IF L1#<1 OR L1#>10000 THEN
    LOCATE 19,13:COLOR 15:PRINT"LENGTH MUST BE BETWEEN 1 FT AND 10000 FT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                            "
    IF L1#<1 THEN L$(I4%,J%)="1    "
    IF L1#>10000 THEN L$(I4%,J%)="10000"
    GOTO CHANNEL
  END IF
  SLOPELABEL3:
  LOCATE 22,24:PRINT "SLOPE (%) = ";:CALL PROM(S$(I4%,J%),5,1)
  S1#=VAL(S$(I4%,J%))
  IF S1#<.1 THEN
    LOCATE 19,13:COLOR 15:PRINT"SLOPE MUST BE EQUAL TO OR GREATER THAN 0.1 PERCENT!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                    "
    S$(I4%,J%)="0.1  "
    GOTO SLOPELABEL3
  END IF
  MANNLABEL2:
  LOCATE 22,45:PRINT "ENTER MANNING'S N: ";
  IF VAL(T1$(I4%,J%))=0 THEN T1$(I4%,J%)="0.03      "
  CALL PROM(T1$(I4%,J%),5,4)
  MANN#=VAL(T1$(I4%,J%))
  IF MANN#<.009 OR MANN#>1 THEN
    LOCATE 19,13:COLOR 15:PRINT"MANNING'S N MUST BE BETWEEN 0.009 AND 1.000!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                "
    IF MANN#<.1 THEN T1$(I4%,J%)="0.009"
    IF MANN#>1 THEN T1$(I4%,J%)="1.000"
    GOTO MANNLABEL2
  END IF
  LOCATE 22,1:PRINT "                                                                             "
  LOCATE 22,1:PRINT "ENTER SIDE SLOPE (HORIZ:1) = ";:Z$(I4%,J%)="3    "
  CALL PROM(Z$(I4%,J%),5,1)
  Z1#=VAL(Z$(I4%,J%))
  BOTTOMLABEL1:
  LOCATE 22,40:PRINT "ENTER BOTTOM WIDTH (FT) = ";
  IF B$(I4%,J%)=CHT$(0) THEN B$(I4%,J%)="0    "
  CALL PROM(B$(I4%,J%),5,1)
  B#=VAL(B$(I4%,J%))
  IF B#=0 AND Z1#=0 THEN
    LOCATE 19,5:COLOR 15:PRINT"THE BOTTOM WIDTH MUST BE GREATER THAN 0.00 SINCE THE SIDE SLOPE IS 0.00!"
    DELAY 2:COLOR 7:LOCATE 19,3:PRINT"                                                                           "
    B$(I4%,J%)="1    "
    GOTO BOTTOMLABEL1
  END IF
  LOCATE 22,1:PRINT "                                                                             "
  DEPTHLABEL1:
  LOCATE 22,1:PRINT "ENTER FLOW DEPTH (FT) = ";:DEP$(I4%,J%)="1    "
  CALL PROM(DEP$(I4%,J%),5,1)
  DEP#=VAL(DEP$(I4%,J%))
  IF DEP#=0 THEN
    LOCATE 19,13:COLOR 15:PRINT"THE DEPTH MUST BE GREATER THAN 0.00!"
    DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                      "
    DEP$(I4%,J%)="1    "
    GOTO DEPTHLABEL1
  END IF
  A#=(B#+Z1#*DEP#)*DEP#
  W#=B#+2*DEP#*((1+(Z1#^2))^.5)
  T#(I4%,J%)=L1#/(60*((1.486/MANN#)*((S1#/100)^.5)*((A#/W#)^(2/3))))
  LOCATE N2%,6:PRINT"CHANNEL FLOW   ----   B =";_
                     B#;"FT,  DEPTH =";DEP#;"FT,  Z =";Z1#
  LOCATE N2%+1,10:PRINT"LENGTH =";L1#; "FT,  ";
  PRINT "SLOPE ="; S1#; "%,  ";
  PRINT "N = "; T1$(I4%,J%); TAB(62); "Tc =";
  PRINT USING "####.##"; T#(I4%,J%);: PRINT " MIN"
  U%(I4%,J%)=3
  N2%=N2%+3:J%=J%+1
  GOSUB CLEAR1
RETURN

OTHER:
    LOCATE 21,1:PRINT "OTHER METHOD - ENTER DESCRIPTION: ";:CALL PROM(T1$(I4%,J%),25,1)
    IF T1$(I4%,J%)="                         " THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB LENGTH
    OTHERVELOCITYLABEL1:
    LOCATE 22,31:PRINT "VELOCITY (FPS) = ";:CALL PROM(S$(I4%,J%),5,1)
    VEL#=VAL(S$(I4%,J%))
    IF VEL#=0 THEN
      LOCATE 19,13:COLOR 15:PRINT"THE VELOCITY MUST BE EQUAL TO OR GREATER THAN 0.1!"
      DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                   "
      S$(I4%,J%)="0.1  "
      GOTO OTHERVELOCITYLABEL1
    END IF
    U%(I4%,J%)=4
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT T1$(I4%,J%)
    LOCATE N2%+1,10:PRINT "LENGTH = "; L$(I4%,J%); " FT,  ";_
      "VELOCITY ="; VEL#; "FPS"; TAB(62); "Tc =";
    PRINT USING "####.##"; T#(I4%,J%);: PRINT " MIN"
    N2%=N2%+3:J%=J%+1
    GOSUB CLEAR1
RETURN

LENGTH:
    LOCATE 22,1:PRINT "LENGTH (FT) = ";:L1$="0":CALL PROM(L$(I4%,J%),5,1)
    L1#=VAL(L$(I4%,J%))
    IF L1#=0 THEN RETURN
    IF L1#<1 OR L1#>10000 THEN
      LOCATE 19,13:COLOR 15:PRINT"LENGTH MUST BE BETWEEN 1 FT AND 10,000 FT!"
      DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                              "
      IF L1#<1 THEN L$(I4%,J%)="1    "
      IF L1#>10000 THEN L$(I4%,J%)="10000"
      GOTO LENGTH
    END IF
RETURN

CLEAR1:
    LOCATE 21,1:PRINT "                                                                            "
    LOCATE 22,1:PRINT "                                                                            "
RETURN

PRINTMANNHELP:
  PRINTMANNFLAG%=1
  COLOR 14,1
  LOCATE 6,15:PRINT CHR$(201);
  FOR J3%=1 TO 50
    PRINT CHR$(205);
  NEXT J3%
  PRINT CHR$(187)
  LOCATE 7,15:PRINT CHR$(186);"  Surface Description                     N       ";CHR$(186)
  LOCATE 8,15:PRINT CHR$(186);"  ";:FOR J3%=1 TO 46:PRINT CHR$(196);:NEXT J3%:PRINT "  ";CHR$(186)
  LOCATE 9,15:PRINT CHR$(186);"  Smooth surfaces .....................  0.011    ";CHR$(186)
  LOCATE 10,15:PRINT CHR$(186);"  Fallow (no residue) .................  0.05     ";CHR$(186)
  LOCATE 11,15:PRINT CHR$(186);"  Cultivated soils:                               ";CHR$(186)
  LOCATE 12,15:PRINT CHR$(186);"    Residue cover ";CHR$(243);"20% ................  0.06     ";CHR$(186)
  LOCATE 13,15:PRINT CHR$(186);"    Residue cover >20% ................  0.17     ";CHR$(186)
  LOCATE 14,15:PRINT CHR$(186);"  Grass:                                          ";CHR$(186)
  LOCATE 15,15:PRINT CHR$(186);"    Short grass prarie ................  0.15     ";CHR$(186)
  LOCATE 16,15:PRINT CHR$(186);"    Dense grasses .....................  0.24     ";CHR$(186)
  LOCATE 17,15:PRINT CHR$(186);"    Bermudagrass ......................  0.41     ";CHR$(186)
  LOCATE 18,15:PRINT CHR$(186);"               <<<<  F1-MORE  >>>>                ";CHR$(186)
  LOCATE 19,15:PRINT CHR$(200);
  FOR J3%=1 TO 50
  PRINT CHR$(205);
  NEXT J3%
  PRINT CHR$(188)
  COLOR 7,0
  ON KEY(1) GOSUB MOREMANNHELP
  LOCATE 21,51
RETURN

MOREMANNHELP:
  PRINTMANNFLAG%=1
  COLOR 14,1
  LOCATE 9,15:PRINT CHR$(186);"  Range (natural) .....................  0.13     ";CHR$(186)
  LOCATE 10,15:PRINT CHR$(186);"  Woods:                                          ";CHR$(186)
  LOCATE 11,15:PRINT CHR$(186);"    Light underbrush ..................  0.40     ";CHR$(186)
  LOCATE 12,15:PRINT CHR$(186);"    Dense underbrush ..................  0.80     ";CHR$(186)
  LOCATE 13,15:PRINT CHR$(186);"                                                  ";CHR$(186)
  LOCATE 14,15:PRINT CHR$(186);"                                                  ";CHR$(186)
  LOCATE 15,15:PRINT CHR$(186);"  Source:   Urban Hydrology for Small             ";CHR$(186)
  LOCATE 16,15:PRINT CHR$(186);"            Watersheds (TR-55)                    ";CHR$(186)
  LOCATE 17,15:PRINT CHR$(186);"                                                  ";CHR$(186)
  LOCATE 18,15:PRINT CHR$(186);"               <<<<  F1-LAST  >>>>                ";CHR$(186)
  COLOR 7,0
  ON KEY(1) GOSUB PRINTMANNHELP
  LOCATE 21,51
RETURN

CLEARMANNHELP:
  PRINTMANNFLAG%=0
  LOCATE 6,15:PRINT "                                                    "
  LOCATE 7,15:PRINT "                                                    "
  LOCATE 8,15:PRINT "                                                    "
  LOCATE 9,15:PRINT "                                                    "
  LOCATE 10,15:PRINT "                                                    "
  LOCATE 11,15:PRINT "                                                    "
  LOCATE 12,15:PRINT "                                                    "
  LOCATE 13,15:PRINT "                                                    "
  LOCATE 14,15:PRINT "                                                    "
  LOCATE 15,15:PRINT "                                                    "
  LOCATE 16,15:PRINT "                                                    "
  LOCATE 17,15:PRINT "                                                    "
  LOCATE 18,15:PRINT "                                                    "
  LOCATE 19,15:PRINT "                                                    "
  KEY(1) OFF
  IF J%=1 THEN RETURN
  FOR K%=1 TO J%-1
    IF U%(I4%,K%)=1 THEN
      LOCATE 6+3*(K%-1),6:PRINT"SHEET FLOW"
      LOCATE 7+3*(K%-1),10:PRINT"LENGTH =";VAL(L$(I4%,K%)); "FT,  ";
      PRINT "SLOPE =";VAL(S$(I4%,K%)); "%,  ";
      PRINT "N = "; T1$(I4%,K%); TAB(62); "Tc = ";
      PRINT USING "###.##"; T#(I4%,K%);: PRINT " MIN"
    END IF
    IF U%(I4%,K%)=2 THEN
      LOCATE 6+3*(K%-1),6:PRINT"SHALLOW CONCENTRATED FLOW"
      LOCATE 7+3*(K%-1),10:PRINT"LENGTH =";VAL(L$(I4%,K%)); "FT,  ";
      PRINT "SLOPE =";VAL(S$(I4%,K%)); "%,  ";
      IF T1$(I4%,K%)="U" THEN PRINT "UNPAVED";
      IF T1$(I4%,K%)="P" THEN PRINT "PAVED";
      PRINT TAB(62); "Tc = ";
      PRINT USING "###.##"; T#(I4%,K%);: PRINT " MIN"
    END IF
    IF U%(I4%,K%)=3 THEN
      LOCATE 6+3*(K%-1),6:PRINT"CHANNEL FLOW   ----   B =";_
                         B#;"FT,  DEPTH =";DEP#;"FT,  Z =";Z1#
      LOCATE 7+3*(K%-1),10:PRINT"LENGTH =";VAL(L$(I4%,K%)); "FT,  ";
      PRINT "SLOPE =";VAL(S$(I4%,K%)); "%,  ";
      PRINT "N = "; T1$(I4%,K%); TAB(62); "Tc = ";
      PRINT USING "###.##"; T#(I4%,K%);: PRINT " MIN"
    END IF
    IF U%(I4%,K%)=4 THEN
      LOCATE 6+3*(K%-1),6:PRINT T1$(I4%,K%)
      LOCATE 7+3*(K%-1),10:PRINT "LENGTH =";VAL(L$(I4%,K%)); "FT,  ";_
        "VELOCITY ="; VAL(S$(I4%,K%)); "FPS"; TAB(62); "Tc =";
      PRINT USING "####.##"; T#(I4%,K%);: PRINT " MIN"
    END IF
  NEXT K%
RETURN
