1PRINTSUBAREADATA:
  ON ERROR GOTO 0
  CLS
  GOSUB FILETEST
  IF I2%=0 THEN
    LOCATE 10,25:COLOR 15,0:PRINT "NO SUBAREAS HAVE BEEN ENTERED!"
    DELAY 3:COLOR 7,0
    GOTO START
  END IF
  GOSUB HEADING
  GOSUB PRINTSUBAREAMENU
  INCTOT%=0:Y1%=0
  START4:
    LOCATE 15,29
    Y1$="1"
    IF Y1%=1 THEN Y1$="3"
    CALL PROM(Y1$,1,1)
    IF Y1$="1" THEN GOSUB PRINTSUBAREADATA1
    IF Y1$="2" THEN GOSUB INCLUDETOTALS
    IF Y1$="3" THEN RETURN
    GOTO START4

PRINTSUBAREAMENU:
  LOCATE 8,18:PRINT FNBOX$(39,9)
  LOCATE 9,21:PRINT"ENTER NUMBER OF DESIRED FUNCTION:"
  LOCATE 11,23:PRINT"1) PRINT SUBAREA DATA"
  LOCATE 12,23:PRINT"2) INCLUDE SUBAREA TOTALS"
  LOCATE 13,23:PRINT"3) EXIT TO MAIN MENU"
  LOCATE 15,21:PRINT"NUMBER?"
RETURN

PRINTSUBAREADATA1:
  GOSUB GETDATE
  A$=A$(1):LOCATE 21,6:PRINT"ENTER BEGINNING SUBAREA: ";:CALL PROM(A$,4,2)
  A1$=A$
  A$=A$(I2%):LOCATE 21,40:PRINT"ENTER ENDING SUBAREA: ";:CALL PROM(A$,4,2)
  A2$=A$
  PRINTSUBAREALABEL1:
  FILEADDRESS$="LPT1"
  LOCATE 22,11:PRINT"SELECT OUTPUT FILE ADDRESS (LPT1=PRINTER): ";
  CALL PROM(FILEADDRESS$,12,1)
  IF FILEADDRESS$ = "LPT1        " THEN FILEADDRESS$ = "LPT1:"
  IF FILEADDRESS$ = "SCRN        "_
    OR FILEADDRESS$ = "SCRN:       "_
    OR FILEADDRESS$ = "CON         " THEN GOTO PRINTSUBAREALABEL1
  I%=0:N3%=0:N%=7:HEADINGJUSTPRINTED=0
  FOR J%=1 TO I2%
    IF A1$=A$(J%) THEN I%=J%
    IF A2$=A$(J%) THEN N3%=J%
  NEXT J%
  IF I%=0 OR N3%=0 THEN GOTO PRINTSUBAREADATA1
  B1$="#.##":B2$="###.##":B3$="##.##":B4$="####.##"
  OPEN FILEADDRESS$ FOR OUTPUT AS #3
    IF FILEADDRESS$="LPT1:" THEN
      WIDTH #3, 133
      PRINT #3, CHR$(15)
      ON ERROR GOTO 0   'PUT IN ERROR CONTROL FOR PAPER ERROR, DESELECTED, ETC.
    END IF
    PRINTSUBAREALABEL2:
      GOSUB PRINTSUBAREAHEADING
    PRINTSUBAREALABEL3:
      FLAG1=0
      GOSUB CHECKN
      IF FLAG1=1 THEN GOTO PRINTSUBAREALABEL2
      GOSUB PRINTAREAC
			IF TIMECONCTYPE$ = "1" THEN GOSUB PRINTTC1 ELSE GOSUB PRINTTC2
      GOSUB PRINTFLOW1
      I%=I%+1
      IF I%<N3% OR I%=N3% THEN GOTO PRINTSUBAREALABEL3
      IF INCTOT%=1 THEN
        N%=N%+8
        IF N%>52 OR N%=52 THEN
          IF FILEADDRESS$="LPT1:" THEN
            PRINT #3, CHR$(12)
          END IF
          GOSUB PRINTSUBAREAHEADING
        END IF
        IF N%>15 THEN
          PRINT #3, "          ---------------------------------------------------------------------------------"
          PRINT #3, ""
        END IF
        PRINT #3, "":PRINT #3, ""
        PRINT #3, "                                        * * * SUBAREA TOTALS * * *"
        PRINT #3, ""
        PRINT #3, "                                        TOTAL AREA = ";
        PRINT #3, USING B4$; A2#;:PRINT #3, " ACRES"
        PRINT #3, ""
        IF A6#>0 THEN
          PRINT #3, "                                       TOTAL ";P3$(1);"AREA = ";
          PRINT #3, USING B4$; A6#;:PRINT #3, " ACRES"
        END IF
        IF A7#>0 THEN
          PRINT #3, "                                       TOTAL ";P3$(2);"AREA = ";
          PRINT #3, USING B4$; A7#;:PRINT #3, " ACRES"
        END IF
        IF A8#>0 THEN
          PRINT #3, "                                       TOTAL ";P3$(3);"AREA = ";
          PRINT #3, USING B4$; A8#;:PRINT #3, " ACRES"
        END IF
        IF A9#>0 THEN
          PRINT #3, "                                       TOTAL ";P3$(4);"AREA = ";
          PRINT #3, USING B4$; A9#;:PRINT #3, " ACRES"
        END IF
      END IF
    IF FILEADDRESS$="LPT1:" THEN
      PRINT #3, CHR$(18)
      PRINT #3, CHR$(12)
    END IF
  CLOSE #3
  LOCATE 21,6:PRINT"                              "
  LOCATE 21,40:PRINT"                           "
  LOCATE 22,11:PRINT"                                                        "
  A2#=0:A6#=0:A7#=0:A8#=0:A9#=0
  Y1%=1
RETURN

PRINTSUBAREAHEADING:
    PRINT #3, "                                        * * * SUBAREA DATA * * *"
    PRINT #3, ""
		PRINT #3, ""
		IF TIMECONCTYPE$="1" THEN TCP$="JUNE 1986 SCS, P-2YR = 3.5 IN.  " ELSE TCP$="SCS VELOCITY CHART              "
		PRINT #3, "PROJECT: "; P1$; " TIME CONC. METHOD: ";TCP$;"CALC. BY: "; P5$
    IF N$="1" THEN G$=Z4$
    IF N$="2" THEN G$=Z5$
    PRINT #3, "INTENSITY DATA: "; G$; "   RATIONAL METHOD - "; I1$; " YEAR STORM"; "           "; DATE1$
    PRINT #3, "-----------------------------------------------------------------------------------------------------------------------------------"
    PRINT #3, ""
    HEADINGJUSTPRINTED=1
RETURN

CHECKN:
  N%=N%+11
  FOR J%=1 TO 4
    IF P4$(J%)<>"0" THEN
      N%=N%+1
    END IF
    IF U#(I%,J%)>0 THEN
      N%=N%+2
    END IF
  NEXT J%
  IF N%=52 OR N%>52 THEN
    N%=7
    IF FILEADDRESS$="LPT1:" THEN
      PRINT #3, CHR$(12)
    END IF
    FLAG1=1
    RETURN
  END IF
  IF N%>35 THEN
    PRINT #3, "          ---------------------------------------------------------------------------------"
    PRINT #3, ""
  END IF
RETURN

PRINTAREAC:
    PRINT #3, "AREA: "; A$(I%)
    A2#=A2#+A5#(I%)
    PRINT #3, "               TOTAL AREA = ";:PRINT #3, USING B2$; A5#(I%);:PRINT #3, " ACRES"
    IF VAL(P4$(1))<>0 THEN
      A1#=VAL(A1$(I%)):A6#=A6#+A1#
      PRINT #3, "                  ";P3$(1);"AREA     (C = ";:PRINT #3, USING B1$; VAL(P4$(1));:PRINT #3, ") = ";:PRINT #3, USING B2$; A1#;:PRINT #3, " ACRES"
    END IF
    IF VAL(P4$(2))<>0 THEN
      A1#=VAL(A2$(I%)):A7#=A7#+A1#
      PRINT #3, "                  ";P3$(2);"AREA     (C = ";:PRINT #3, USING B1$; VAL(P4$(2));:PRINT #3, ") = ";:PRINT #3, USING B2$; A1#;:PRINT #3, " ACRES"
    END IF
    IF VAL(P4$(3))<>0 THEN
      A1#=VAL(A3$(I%)):A8#=A8#+A1#
      PRINT #3, "                  ";P3$(3);"AREA     (C = ";:PRINT #3, USING B1$; VAL(P4$(3));:PRINT #3, ") = ";:PRINT #3, USING B2$; A1#;:PRINT #3, " ACRES"
    END IF
    IF VAL(P4$(4))<>0 THEN
      A1#=VAL(A4$(I%)):A9#=A9#+A1#
      PRINT #3, "                  ";P3$(4);"AREA     (C = ";:PRINT #3, USING B1$; VAL(P4$(4));:PRINT #3, ") = ";:PRINT #3, USING B2$; A1#;:PRINT #3, " ACRES"
    END IF
    PRINT #3, ""
    PRINT #3, "          COMPOSITE C = ";:PRINT #3, USING B1$; C#(I%)
RETURN

PRINTTC1:
  PRINT #3, ""
  PRINT #3, "          TIME OF CONCENTRATION, Tc = ";:PRINT #3, USING B2$; T8#(I%);:PRINT #3, " MINUTES"
  FOR J%=1 TO 4
    IF U%(I%,J%)=0 THEN RETURN
		IF L$(I%,J%)="0    " THEN EXIT FOR
    IF U%(I%,J%)=1 THEN
			PRINT #3, "                    SHEET FLOW"
			PRINT #3, "                      LENGTH = ";
			PRINT #3, USING "#####"; VAL(L$(I%,J%));: PRINT #3, " FT.     SLOPE = ";
			PRINT #3, USING B3$; VAL(S$(I%,J%));: PRINT #3, "%   N = ";
			PRINT #3, USING "#.###"; VAL(T1$(I%,J%));: PRINT #3, "   Tc = ";
			PRINT #3, USING "###.##"; T#(I%,J%);: PRINT #3, " MIN"
		END IF
    IF U%(I%,J%)=2 THEN
			PRINT #3, "                    SHALLOW CONCENTRATED FLOW"
			PRINT #3, "                      LENGTH = ";
			PRINT #3, USING "#####"; VAL(L$(I%,J%));: PRINT #3, " FT.     SLOPE = ";
			PRINT #3, USING B3$; VAL(S$(I%,J%));: PRINT #3, "%   ";
			IF T1$(I%,J%)="U" THEN PRINT #3, "UNPAVED     Tc = ";
			IF T1$(I%,J%)="P" THEN PRINT #3, "PAVED       Tc = ";
			PRINT #3, USING "###.##"; T#(I%,J%);: PRINT #3," MIN"
		END IF
    IF U%(I%,J%)=3 THEN
			PRINT #3, "                    CHANNEL FLOW   ----   B = ";
			PRINT #3, USING "##.###"; VAL(B$(I%,J%));
			PRINT #3, " FT,  DEPTH = ";
			PRINT #3, USING "##.###"; VAL(DEP$(I%,J%));
			PRINT #3, " FT,  Z =";
			PRINT #3, USING "##.##"; VAL(Z$(I%,J%))
			PRINT #3, "                      LENGTH =";
			PRINT #3, USING "#####"; VAL(L$(I%,J%));: PRINT #3, " FT.     SLOPE = ";
			PRINT #3, USING B3$; VAL(S$(I%,J%));: PRINT #3, "%   N = ";
			PRINT #3, USING "#.###"; VAL(T1$(I%,J%));: PRINT #3, "    Tc = ";
			PRINT #3, USING "###.##"; T#(I%,J%);: PRINT #3, " MIN"
		END IF
    IF U%(I%,J%)=4 THEN
			PRINT #3, "                    ";T1$(I%,J%)
			PRINT #3, "                      LENGTH = ";
			PRINT #3, USING "#####"; VAL(L$(I%,J%));: PRINT #3, " FT.     VELOCITY = ";
			PRINT #3, USING "##.##"; VAL(S$(I%,J%));: PRINT #3, " FPS         Tc = ";
			PRINT #3, USING "###.##"; T#(I%,J%);: PRINT #3, " MIN"
		END IF
  NEXT J%
RETURN

PRINTTC2:
  PRINT #3, ""
  PRINT #3, "          TIME OF CONCENTRATION, Tc = ";:PRINT #3, USING B2$; T8#(I%);:PRINT #3, " MINUTES"
  FOR J%=1 TO 4
    IF U%(I%,J%)=0 THEN RETURN
    IF L$(I%,J%)="0    " THEN EXIT FOR
    IF U%(I%,J%)=1 THEN
      PRINT #3, "                    FOREST WITH HEAVY GROUND LITTER AND MEADOW"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=2 THEN
      PRINT #3, "                    FALLOW OR MINIMUM TILLAGE CULTIVATION & WOODLAND"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=3 THEN
      PRINT #3, "                    SHORT GRASS PASTURE & LAWNS"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=4 THEN
      PRINT #3, "                    NEARLY BARE GROUND"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=5 THEN
      PRINT #3, "                    GRASSED WATERWAY"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=6 THEN
      PRINT #3, "                    PAVED AREA (SHEET FLOW) & SHALLOW GUTTER FLOW"
      GOSUB PRINTTCVALUES
    END IF
    IF U%(I%,J%)=7 THEN
      PRINT #3, "                    "; T1$(I%,J%)
      PRINT #3, "                      LENGTH = ";
        PRINT #3, USING "#####"; VAL(L$(I%,J%));:PRINT #3, " FT.     VELOCITY = ";
        PRINT #3, USING B3$; VAL(S$(I%,J%));:PRINT #3, " FPS   TRAVEL TIME = ";
        PRINT #3, USING B2$; T#(I%,J%);:PRINT #3, " MINUTES"
    END IF
  NEXT J%
RETURN

PRINTFLOW1:
  PRINT #3, ""
  PRINT #3, "          INTENSITY = ";:PRINT #3, USING B1$; I1#(I%);
    PRINT #3, " INCHES/HOUR"
  PRINT #3, " "
  PRINT #3, "          FLOW = (";:PRINT #3, USING B1$; C#(I%);:PRINT #3, ")(";
    PRINT #3, USING B1$; I1#(I%);:PRINT #3, ")(";
    IF A5#(I%)<10 THEN
      PRINT #3, USING B1$; A5#(I%);
    END IF
		IF A5#(I%)<100 AND A5#(I%)=>10 THEN
      PRINT #3, USING B3$; A5#(I%);
    END IF
    IF A5#(I%)=>100 THEN
      PRINT #3, USING B2$; A5#(I%);
		END IF
		PRINT #3, ") = ";:PRINT #3, USING B2$; (C#(I%)*I1#(I%)*A5#(I%));
    PRINT #3, " CFS"
RETURN

PRINTTCVALUES:
  PRINT #3, "                      LENGTH = ";
    PRINT #3, USING "#####"; VAL(L$(I%,J%));:PRINT #3, " FT.     SLOPE = ";
    PRINT #3, USING B3$; VAL(S$(I%,J%));:PRINT #3, " %   TRAVEL TIME = ";
    PRINT #3, USING B2$; T#(I%,J%);:PRINT #3, " MINUTES"
RETURN

INCLUDETOTALS:
  INCTOT%=1
RETURN

GETDATE:
  MONTH$=MID$(DATE$,1,2)
  DAY$=MID$(DATE$,4,2)
  YEAR$=MID$(DATE$,7,4)
  IF VAL(MONTH$)=1 THEN MONTH$="JANUARY"
  IF VAL(MONTH$)=2 THEN MONTH$="FEBRUARY"
  IF VAL(MONTH$)=3 THEN MONTH$="MARCH"
  IF VAL(MONTH$)=4 THEN MONTH$="APRIL"
  IF VAL(MONTH$)=5 THEN MONTH$="MAY"
  IF VAL(MONTH$)=6 THEN MONTH$="JUNE"
  IF VAL(MONTH$)=7 THEN MONTH$="JULY"
  IF VAL(MONTH$)=8 THEN MONTH$="AUGUST"
  IF VAL(MONTH$)=9 THEN MONTH$="SEPTEMBER"
  IF VAL(MONTH$)=10 THEN MONTH$="OCTOBER"
  IF VAL(MONTH$)=11 THEN MONTH$="NOVEMBER"
  IF VAL(MONTH$)=12 THEN MONTH$="DECEMBER"
  DATE1$=MONTH$+STR$(VAL(DAY$))+", "+YEAR$
RETURN
