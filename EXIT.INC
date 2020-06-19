EXITROUTINE:
ON ERROR GOTO 0
IF F$=CHT$(0) THEN END
CLS
GOSUB HEADING
LOCATE 10,5:Y1$="Y":PRINT "SAVE DATA? ";:CALL PROM(Y1$,1,1)
IF Y1$="N" OR Y1$="n" THEN GOTO EXITPROMPT
  OPEN F$ FOR OUTPUT AS #1
  WRITE #1, P1$,P3$(1),P3$(2),P3$(3),P3$(4),P4$(1),P4$(2),P4$(3),P4$(4),_
            I1$,N$,P5$,I2%,NUM1%,TIMECONCTYPE$,P2#
  IF I2%>0 THEN
    FOR I% = 1 TO I2%
      WRITE #1, A$(I%), A5#(I%), C#(I%), T8#(I%), A1$(I%), A2$(I%), A3$(I%), A4$(I%), I1#(I%)
      FOR J% = 1 TO 4
				WRITE #1, L$(I%,J%), S$(I%,J%), T#(I%,J%), T1$(I%,J%), U%(I%,J%),Z$(I%,J%),B$(I%,J%),DEP$(I%,J%)
      NEXT J%
    NEXT I%
  END IF
  IF NUM1%>0 THEN
    FOR I% = 1 TO NUM1%
      WRITE #1, A5$(I%), IN1$(I%), IN2$(I%), A6#(I%), A7#(I%), COMP1#(I%),_
                T1#(I%), T2#(I%), T3#(I%), INT1#(I%), Q#(I%), D$(I%),_
                SL#(I%), N#(I%), Q1#(I%), V#(I%), V1#(I%), L#(I%),_
                F#(I%), U1#(I%), L1#(I%), D#(I%)
    NEXT I%
  END IF
  CLOSE #1
EXITPROMPT:
  LOCATE 12,5:Y1$="N":PRINT "EXIT STORM SEWER PROGRAM? ";:CALL PROM(Y1$,1,1)
  IF Y1$="N" THEN GOTO START
END
