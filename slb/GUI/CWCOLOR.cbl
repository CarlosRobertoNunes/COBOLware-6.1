       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCOLOR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/08/2010.
       SECURITY.      *************************************************
                      *                                               *
                      *  Habilita e desabilita tabela de cores        *
                      *                                               *
                      *************************************************
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  TABCOR-REG PIC X(8)
           OCCURS 0 TO 255 DEPENDING ON MAX-COLOR.

       01  AREAS-DE-TRABALHO.
           05 CWMENU-CLOSE      PIC  X(2)   VALUE 'OF'.
           05 COLOR-STATUS      PIC  X      VALUE '0'.
              88 COLOR-ON                   VALUE '1'.
              88 COLOR-OFF                  VALUE '0' '3'.
              88 COLOR-CLOSED               VALUE '3'.
           05 MAX-COLOR  COMP-5 PIC S9(004) VALUE 0.
           05 B          COMP-X PIC  9(002) VALUE 0.
           05 F          COMP-X PIC  9(002) VALUE 0.

       COPY SP2.

       LINKAGE SECTION.

       01  FLAG PIC X(2).

       PROCEDURE DIVISION USING FLAG.

       000-INICIO.

           IF  FLAG = 'OF'
               PERFORM 020-COLOR-OFF THRU 020-99-FIM
           ELSE
               PERFORM 010-COLOR-ON THRU 010-99-FIM
           END-IF
           DISPLAY "CWCOLOR-STATUS" UPON ENVIRONMENT-NAME
           DISPLAY COLOR-STATUS UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       010-COLOR-ON.

           IF COLOR-OFF
              MOVE 0          TO SP2-CO-ID
              PERFORM VARYING B FROM 0 BY 1 UNTIL B > 15
                 PERFORM VARYING F FROM 0 BY 1 UNTIL F > 15
                   IF  (B NOT = 0) OR (F NOT = 0)
                        ADD  1        TO SP2-CO-ID
                                         MAX-COLOR
                        MOVE LOW-VALUES TO SP2-CO-FG-BG (1)
                                           SP2-CO-FG-BG (2)
                        IF NOT (SP2-CO-ID = 31 OR 112)
                           CALL SP2   USING SP2-GET-COLOR-DEF
                                            SP2-COLOR-DEF
                           IF (SP2-CO-FG-BG (1) NOT = LOW-VALUES)
                           OR (SP2-CO-FG-BG (2) NOT = LOW-VALUES)
                              MOVE SP2-CO-DATA (3:)
                                TO TABCOR-REG (MAX-COLOR)
                           END-IF
                        END-IF
                        MOVE "t"      TO SP2-CO-TYPE (1)
                                         SP2-CO-TYPE (2)
                        MOVE F (1: 1) TO SP2-CO-TEXT (1)
                        MOVE B (1: 1) TO SP2-CO-TEXT (2)
                        CALL SP2   USING SP2-SET-COLOR-DEF SP2-COLOR-DEF
                  END-IF
                 END-PERFORM
              END-PERFORM
              SET COLOR-ON TO TRUE
              CALL 'CWUSER' USING X'11'
      *       DISPLAY "CWCOLOR"  UPON ENVIRONMENT-VALUE
      *       DISPLAY "CWMENU"   UPON ENVIRONMENT-NAME
           END-IF.

       010-99-FIM. EXIT.

       020-COLOR-OFF.

           IF COLOR-ON
              MOVE LOW-VALUES TO SP2-CO-DATA
              MOVE 0          TO SP2-CO-ID
              PERFORM VARYING B FROM 0 BY 1 UNTIL B > 15
                 PERFORM VARYING F FROM 0 BY 1 UNTIL F > 15
                   IF  (B NOT = 0) OR (F NOT = 0)
                        ADD  1         TO SP2-CO-ID
                        IF NOT (SP2-CO-ID = 31 OR 112)
                           IF SP2-CO-ID NOT > MAX-COLOR
                              MOVE TABCOR-REG (SP2-CO-ID)
                                TO SP2-CO-DATA (3:)
                              CALL SP2   USING SP2-SET-COLOR-DEF
                                               SP2-COLOR-DEF
                           ELSE
                              CALL SP2   USING SP2-DELETE-COLOR
                                               SP2-COLOR-DEF
                           END-IF
                        END-IF
                  END-IF
                 END-PERFORM
              END-PERFORM
              MOVE 0 TO MAX-COLOR
              CALL "CWFONT" USING X'FF'
              PERFORM VARYING SP2-FO-ID FROM 1 BY 1 UNTIL SP2-FO-ID > 4
                      CALL SP2   USING SP2-DELETE-FONT SP2-FONT-DEF
              END-PERFORM
      *       CALL 'CWUSER' USING X'10'
joa114*       CANCEL "CWUSER"
grito *       CANCEL "CWSCRE"
              CANCEL "CWSPID"
              CANCEL "CWATTR"
              CANCEL "CWFONT"
              DISPLAY 'CWMENU-CLOSE' UPON ENVIRONMENT-NAME
              MOVE SPACES TO CWMENU-CLOSE
              ACCEPT CWMENU-CLOSE FROM ENVIRONMENT-VALUE
              IF CWMENU-CLOSE = 'OF' OR 'of' OR 'Of' OR 'oF'
                 CONTINUE
              ELSE
                 MOVE LOW-VALUES TO SP2-WD-DATA
                 CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
                 CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
              END-IF
      *       CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM
              SET COLOR-CLOSED TO TRUE
           END-IF.

       020-99-FIM. EXIT.

       END PROGRAM CWCOLOR.

