       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSTOP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Encerramento de sistemas                    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO-1.
           05 CWSAVE                   PIC  X(003) VALUE SPACES.
           05 FATOR-W           COMP-X PIC  9(002) VALUE 0.
           05 CWMENU.
              10 CWMENU-END            PIC  X(008) VALUE SPACES.
              10 CWMENU-AREA01 POINTER.
              10 CWMENU-AREA02 POINTER.
              10 CWMENU-AREA03 POINTER.
              10 CWMENU-AREA04 POINTER.
              10 CWMENU-AREA05 POINTER.
              10 CWMENU-AREA06 POINTER.
              10 CWMENU-AREA07 POINTER.
              10 CWMENU-AREA08 POINTER.
              10 CWMENU-AREA09 POINTER.
              10 CWMENU-AREA10 POINTER.
           05 FIM                      PIC  X(002) VALUE SPACES.
           05 I                        PIC  9(003) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 M                        PIC  9(003) VALUE 0.
           05 F                        PIC  9(003) VALUE 0.
           05 SISTEMA-ENCERRADO        PIC  X(036) VALUE
                "#Sistema encerrado".

       COPY CWTIME.
       COPY CWSEND.
       COPY CWGETL.
       COPY CWSPWS.
       COPY CONSOLE.

       LINKAGE SECTION.

       01  COMMAREA01           PIC  X(2000).
       01  COMMAREA02           PIC  X(2000).
       01  COMMAREA03           PIC  X(2000).
       01  COMMAREA04           PIC  X(2000).
       01  COMMAREA05           PIC  X(2000).
       01  COMMAREA06           PIC  X(2000).
       01  COMMAREA07           PIC  X(2000).
       01  COMMAREA08           PIC  X(2000).
       01  COMMAREA09           PIC  X(2000).
       01  COMMAREA10           PIC  X(2000).

       SCREEN SECTION.

       01  WINDOWS BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 HIGH.
           05 LINE 01 COLUMN 01 PIC X(2000) FROM SPACES.

       PROCEDURE DIVISION USING COMMAREA01
                                COMMAREA02
                                COMMAREA03
                                COMMAREA04
                                COMMAREA05
                                COMMAREA06
                                COMMAREA07
                                COMMAREA08
                                COMMAREA09
                                COMMAREA10.

       000-INICIO.

           CALL "CWLOCK" USING "U"
      *    EXEC COBOLware Close END-EXEC
           DISPLAY "CWSAVE" UPON ENVIRONMENT-NAME
           ACCEPT CWSAVE    FROM ENVIRONMENT-VALUE
           IF   CWSAVE = "ON"
                EXEC COBOLware SAVE (Close-All)
                END-EXEC
           END-IF
           COPY CWSPPD.

           CALL "CWLOGW" USING SISTEMA-ENCERRADO
           PERFORM 010-END THRU 010-99-FIM.

       000-99-FIM. STOP RUN.

       010-END.

           DISPLAY 'CWSTARTED' UPON ENVIRONMENT-NAME
           ACCEPT  CONSOLE-PROGRAM FROM ENVIRONMENT-VALUE
           IF CONSOLE-PROGRAM NOT = SPACES
              MOVE 'STOP RUN'    TO CONSOLE-MSG
              CALL "CONSOLE"  USING PARAMETROS-CONSOLE
           END-IF
           CALL 'CWSETS' USING X'02' CWMENU

           IF   CWMENU-END NOT = SPACES
                SET ADDRESS COMMAREA01 TO CWMENU-AREA01
                SET ADDRESS COMMAREA02 TO CWMENU-AREA02
                SET ADDRESS COMMAREA03 TO CWMENU-AREA03
                SET ADDRESS COMMAREA04 TO CWMENU-AREA04
                SET ADDRESS COMMAREA05 TO CWMENU-AREA05
                SET ADDRESS COMMAREA06 TO CWMENU-AREA06
                SET ADDRESS COMMAREA07 TO CWMENU-AREA07
                SET ADDRESS COMMAREA08 TO CWMENU-AREA08
                SET ADDRESS COMMAREA09 TO CWMENU-AREA09
                SET ADDRESS COMMAREA10 TO CWMENU-AREA10
                CALL CWMENU-END USING COMMAREA01
                                      COMMAREA02
                                      COMMAREA03
                                      COMMAREA04
                                      COMMAREA05
                                      COMMAREA06
                                      COMMAREA07
                                      COMMAREA08
                                      COMMAREA09
                                      COMMAREA10
                  ON OVERFLOW
                     MOVE SPACES TO CWSEND-MSG
                     STRING "Imposs¡vel executar o programa "
                             DELIMITED BY SIZE
                             CWMENU-END DELIMITED BY SPACE
                     INTO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
           END-CALL
      *    MOVE "CWMENU"   TO SP2-WD-NAME
           MOVE LOW-VALUES TO SP2-WD-NAME
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           CALL SP2   USING SP2-END-SESSION  SP2-NULL-PARM.

       010-99-FIM. EXIT.

       END PROGRAM CWSTOP.
