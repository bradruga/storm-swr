ENTERSUBAREAS:
  CLS
  GOSUB FILETEST
  IF I2%>299 THEN
    LOCATE 10,25:COLOR 15,0:PRINT STR$(MAXPIPES%);" SUBAREAS HAVE BEEN ENTERED"
    DELAY 3:COLOR 7,0
    GOTO ENDENTERSUBAREAS
  END IF
  C1=VAL(P4$(1)):C2=VAL(P4$(2)):C3=VAL(P4$(3)):C4=VAL(P4$(4))
  GOSUB HEADING
  GOSUB ENTERHEADING
  LOCATE 3,24
  IF I2%=0 THEN PRINT "NO SUBAREAS HAVE BEEN ENTERED"
  IF I2%=1 THEN PRINT " 1 SUBAREA HAS BEEN ENTERED"
  IF I2%>1 THEN PRINT I2%;"SUBAREAS HAVE BEEN ENTERED"
  IF I2%>0 THEN
    LOCATE 4,23: PRINT "THE LAST SUBAREA ENTERED WAS ";A$(I2%)
  END IF
  GOSUB ENTERCVALUE
  LOCATE 23,25:PRINT"ENTER 'N' TO EXIT ";:Z$="N":CALL PROM(Z$,1,1)
  IF Z$<>"N" THEN
    I2%=I2%-1
    GOTO ENTERSUBAREAS
  END IF
  IF TIMECONCTYPE$="1" THEN GOSUB ENTER1986CONCENTRATIONTIME
  IF TIMECONCTYPE$="2" THEN GOSUB ENTERUPLANDCONCENTRATIONTIME
  I4%=I2%
  GOSUB FLOWTOTALS
  LOCATE 22,1:PRINT "ENTER ANOTHER SUBAREA? ";:Y1$="Y":CALL PROM(Y1$,1,1)
  IF Y1$="Y" THEN GOTO ENTERSUBAREAS
ENDENTERSUBAREAS:
RETURN



FILETEST:
  IF F$=CHT$(0) THEN
    LOCATE 10,25:COLOR 15,0:PRINT "YOU MUST ENTER THE FILE NAME!"
    DELAY 3:COLOR 7,0
    GOTO START
  END IF

ENTERHEADING:
  LOCATE 2,28:PRINT "SUBAREA ENTRY ROUTINE"
RETURN

ENTERCVALUE:
  ON ERROR GOTO 0
  INCR I2%
  IF A$(I2%)=CHT$(0) THEN
    A$(I2%)="A1"
    IF I2%>1 THEN
      A9$=MID$(A$(I2%-1),2,4):A%=VAL(A9$):INCR A%
      A$(I2%)=MID$(A$(I2%-1),1,1)+MID$(STR$(A%),2,4)
    END IF
  END IF
  NUM%=2:IF I2%=1 THEN NUM%=1
  LOCATE 6,6:PRINT "ENTER SUBAREA DESIGNATION: ";:CALL PROM(A$(I2%),4,NUM%)
  LOCATE 6,6:PRINT "                                  "
  LOCATE 2,28:PRINT "                      "
  LOCATE 4,23:PRINT "                                              "
  I4%=I2%
  GOSUB ENTERCVALUE1
RETURN

ENTERCVALUE1:
  LOCATE 3,24:PRINT "        SUBAREA ";A$(I4%);"            "
  LOCATE 5,17:PRINT FNBOX$(42,10)
  LOCATE 6,20:PRINT "ENTER LAND USE AREAS:":PRINT
  A1#=0:A2#=0:A3#=0:A4#=0
  Y1%=CSRLIN:Y2%=CSRLIN
  WOODED:
    IF C1 = 0 THEN MEADOW
    LOCATE Y1%, 24 :INCR Y1%
    PRINT P3$(1);"AREA (ACRES): ";:X1%=POS:CALL PROM(A1$(I4%),7,1)
    A1# = VAL(A1$(I4%))
    IF A1#=0 THEN
      A1$(I4%)="0      ":LOCATE Y1%-1, X1%:PRINT A1$(I4%)
      GOTO MEADOW
    END IF
    IF A1#>999 OR A1#<.01 THEN
      LOCATE 21,13:COLOR 15:PRINT"AREA MUST BE WITHIN THE RANGE OF 0.01 TO 999 ACRES!"
      DELAY 2:COLOR 7:LOCATE 21,13:PRINT"                                                   "
      DECR Y1%
      IF A1#<.01 THEN A1$(I4%)=".01    "
      IF A1#>999 THEN A1$(I4%)="999    "
      GOTO WOODED
    END IF
  MEADOW:
    IF C2 = 0 THEN GRASS
    LOCATE Y1%, 24 :INCR Y1%
    PRINT P3$(2);"AREA (ACRES): ";:X1%=POS:CALL PROM(A2$(I4%),7,1)
    A2# = VAL(A2$(I4%))
    IF A2#=0 THEN
      A2$(I4%)="0      ":LOCATE Y1%-1, X1%:PRINT A2$(I4%)
      GOTO GRASS
    END IF
    IF A2#>999 OR A2#<.01 THEN
      LOCATE 21,13:COLOR 15:PRINT"AREA MUST BE WITHIN THE RANGE OF 0.01 TO 999 ACRES!"
      DELAY 2:COLOR 7:LOCATE 21,13:PRINT"                                                   "
      DECR Y1%
      IF A2#<.01 THEN A2$(I4%)=".01    "
      IF A2#>999 THEN A2$(I4%)="999    "
      GOTO MEADOW
    END IF
  GRASS:
    IF C3 = 0 THEN IMPERVIOUS
    LOCATE Y1%, 24 :INCR Y1%
    PRINT P3$(3);"AREA (ACRES): ";:X1%=POS:CALL PROM(A3$(I4%),7,1)
    A3# = VAL(A3$(I4%))
    IF A3#=0 THEN
      A3$(I4%)="0      ":LOCATE Y1%-1, X1%:PRINT A3$(I4%)
      GOTO IMPERVIOUS
    END IF
    IF A3#>999 OR A3#<.01 THEN
      LOCATE 21,13:COLOR 15:PRINT"AREA MUST BE WITHIN THE RANGE OF 0.01 TO 999 ACRES!"
      DELAY 2:COLOR 7:LOCATE 21,13:PRINT"                                                   "
      DECR Y1%
      IF A3#<.01 THEN A3$(I4%)=".01    "
      IF A3#>999 THEN A3$(I4%)="999    "
      GOTO GRASS
    END IF
  IMPERVIOUS:
    IF C4 = 0 THEN TOTALS
    LOCATE Y1%, 24
    PRINT P3$(4);"AREA (ACRES): ";:X1%=POS:CALL PROM(A4$(I4%),7,1)
    ON ERROR GOTO IMPERERR
    A4# = VAL(A4$(I4%))
    IF A4#=0 THEN
      A4$(I4%)="0      ":LOCATE Y1%, X1%:PRINT A4$(I4%)
      GOTO TOTALS
    END IF
    IF A4#>999 OR A4#<.01 THEN
      LOCATE 21,13:COLOR 15:PRINT"AREA MUST BE WITHIN THE RANGE OF 0.01 TO 999 ACRES!"
      DELAY 2:COLOR 7:LOCATE 21,13:PRINT"                                                   "
      IF A4#<.01 THEN A4$(I4%)=".01    "
      IF A4#>999 THEN A4$(I4%)="999    "
      GOTO IMPERVIOUS
    END IF
    GOTO TOTALS
      IMPERERR:
        RESUME IMPERVIOUS
  TOTALS:
    A5#(I4%) = A1#+A2#+A3#+A4#
    IF A5#(I4%)>999 THEN
      Y1%=Y2%
      LOCATE 21,13:COLOR 15:PRINT"AREA MUST BE WITHIN THE RANGE OF 0.01 TO 999 ACRES!"
      DELAY 2:COLOR 7:LOCATE 21,13:PRINT"                                                   "
      GOTO WOODED
    END IF
    C#(I4%) = (A1#*C1+A2#*C2+A3#*C3+A4#*C4)/A5#(I4%)
  INCR Y1%,2:LOCATE Y1%, 24
  PRINT "TOTAL AREA = ";:PRINT USING "###.##"; A5#(I4%);:PRINT " ACRES"
  INCR Y1%:LOCATE Y1%, 24
  PRINT "COMPOSITE C = ";:PRINT USING "#.##"; C#(I4%)
RETURN


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
    GOSUB OTHER1
    IF J%=5 THEN RETURN
  LOCATE 22,1:PRINT "ENTER MORE DATA? ";:Y1$="N":CALL PROM(Y1$,1,1)
  IF Y1$="Y" THEN
    GOSUB CLEAR1
    GOTO START1986
  END IF
RETURN

OVERLAND:
  LOCATE 21,1:PRINT "SHEET FLOW"
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
	LOCATE 22,45:PRINT "SURFACE (U-UNPAVED, P-PAVED): ";
	IF T1$(I4%,J%)="" THEN T1$(I4%,J%)="U"
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
	LOCATE 22,1:PRINT "ENTER SIDE SLOPE (HORIZ:1) = ";
	IF Z$(I4%,J%)="" THEN Z$(I4%,J%)="3    "
  CALL PROM(Z$(I4%,J%),5,1)
  Z1#=VAL(Z$(I4%,J%))
  BOTTOMLABEL1:
  LOCATE 22,40:PRINT "ENTER BOTTOM WIDTH (FT) = ";
	IF B$(I4%,J%)="" THEN B$(I4%,J%)="0    "
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
	LOCATE 22,1:PRINT "ENTER FLOW DEPTH (FT) = ";
	IF DEP$(I4%,J%)="" THEN DEP$(I4%,J%)="1    "
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

OTHER1:
    LOCATE 21,1:PRINT "OTHER METHOD - ENTER DESCRIPTION: ";:CALL PROM(T1$(I4%,J%),25,1)
    IF T1$(I4%,J%)="                         " THEN
      GOSUB CLEAR1
      L1#=0
      RETURN
    END IF
    GOSUB LENGTH1
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

LENGTH1:
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


ENTERUPLANDCONCENTRATIONTIME:
  ON ERROR GOTO 0
  CLS:GOSUB HEADING:J%=1:K%=1:N2%=6
  LOCATE 3,34:PRINT "SUBAREA ";A$(I2%)
  LOCATE 4,18:PRINT "TIME OF CONCENTRATION - SCS UPLAND METHOD"
  LOCATE 5,1 :PRINT FNBOX$(79,15)
  START2:
    GOSUB FOREST
    IF J%=5 THEN RETURN
    GOSUB FALLOW
    IF J%=5 THEN RETURN
    GOSUB GRASS1
    IF J%=5 THEN RETURN
    GOSUB BARE
    IF J%=5 THEN RETURN
    GOSUB WATERWAY
    IF J%=5 THEN RETURN
    GOSUB PAVED
    IF J%=5 THEN RETURN
    GOSUB OTHER
    IF J%=5 THEN RETURN
  LOCATE 22,1:PRINT "ENTER MORE DATA? ";:Y1$="N":CALL PROM(Y1$,1,1)
  IF Y1$="Y" THEN GOTO START2
RETURN

FOREST:
    LOCATE 21,1:PRINT TC1$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=1
		VEL#=FNVEL#(S1#,1)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC1$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

FALLOW:
    LOCATE 21,1:PRINT TC2$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=2
		VEL#=FNVEL#(S1#,2)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC2$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

GRASS1:
    LOCATE 21,1:PRINT TC3$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=3
		VEL#=FNVEL#(S1#,3)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC3$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

BARE:
    LOCATE 21,1:PRINT TC4$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=4
		VEL#=FNVEL#(S1#,4)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC4$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

WATERWAY:
    LOCATE 21,1:PRINT TC5$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=5
		VEL#=FNVEL#(S1#,5)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC5$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

PAVED:
    LOCATE 21,1:PRINT TC6$
    GOSUB LENGTH
    IF L1#=0 THEN
      GOSUB CLEAR1
      RETURN
    END IF
    GOSUB SLOPE
    U%(I4%,J%)=6
		VEL#=FNVEL#(S1#,6)
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT TC6$
    GOSUB INFOLINE
    GOSUB TCLINE
RETURN

OTHER:
    LOCATE 21,1:PRINT "OTHER METHOD - ENTER DESCRIPTION: ";:CALL PROM(T1$(I4%,J%),25,1)
    IF T1$(I4%,J%)="                         " THEN
      GOSUB CLEAR1
      L1#=0
      RETURN
    END IF
    INCR K%
    GOSUB LENGTH
    LOCATE 22,31:PRINT "VELOCITY (FPS) = ";:CALL PROM(S$(I4%,J%),5,1)
    VEL#=VAL(S$(I4%,J%))
    U%(I4%,J%)=7
    T#(I4%,J%)=L1#/(VEL#*60)
    LOCATE N2%,6:PRINT T1$(I4%,J%)
    LOCATE N2%+1,11:PRINT "LENGTH ="; L$(I4%,J%); "FEET"; TAB(34);_
      "VELOCITY ="; VEL#; "FPS"; TAB(56); "Tc = ";
    GOSUB TCLINE
RETURN

FLOWTOTALS:
  T8#(I4%)=0
  FOR K1%=1 TO J%-1
    T8#(I4%) = T8#(I4%)+T#(I4%,K1%)
  NEXT K1%
  LOCATE 19,6:PRINT "TOTAL Tc = ";:PRINT USING "###.##"; T8#(I4%);:PRINT " MIN"
  TIMECONC#=T8#(I4%):GOSUB INTENSITY
  I1#(I4%)=INTENS#
  LOCATE 19,31:PRINT "INTENSITY = ";:PRINT USING "#.##"; I1#(I4%);:PRINT " IN/HR"
  Q#=C#(I4%)*I1#(I4%)*A5#(I4%)
  LOCATE 19,61:PRINT "FLOW = ";:PRINT USING "###.##"; Q#;:PRINT " CFS"
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

SLOPE:
    LOCATE 22,31:PRINT "SLOPE (%) = ";:CALL PROM(S$(I4%,J%),5,1)
    S1#=VAL(S$(I4%,J%))
    IF S1#<.5 OR S1#>30 THEN
      LOCATE 19,13:COLOR 15:PRINT"SLOPE MUST BE WITHIN THE RANGE OF 0.5 TO 30 PERCENT!"
      DELAY 2:COLOR 7:LOCATE 19,13:PRINT"                                                     "
      IF S1#<.5 THEN S$(I4%,J%)=".5   "
      IF S1#>30 THEN S$(I4%,J%)="30   "
      GOTO SLOPE
    END IF
RETURN

INFOLINE:
    LOCATE N2%+1,11:PRINT "LENGTH ="; L1#; "FEET";_
      TAB(36); "SLOPE ="; S1#; "%"; TAB(56); "Tc = ";
RETURN

TCLINE:
    PRINT USING "###.##"; T#(I4%,J%);: PRINT " MIN"
    N2%=N2%+3:J%=J%+1
    GOSUB CLEAR1
RETURN

INTENSITY:
	IF TIMECONC#<VAL(MIN1$(1)) THEN
    INTENS# = VAL(INTEN1$(1))
    RETURN
  END IF
  FOR I%=1 TO INTENLEN%
		IF TIMECONC#<VAL(MIN1$(I%)) THEN CALCINTEN
  NEXT I%
  INTENS# = VAL(INTEN1$(INTENLEN%))
  RETURN
  CALCINTEN:
		INTENS# = ((TIMECONC#-VAL(MIN1$(I%-1)))/(VAL(MIN1$(I%))-VAL(MIN1$(I%-1))))_
               *(VAL(INTEN1$(I%))-VAL(INTEN1$(I%-1)))+VAL(INTEN1$(I%-1))
RETURN
