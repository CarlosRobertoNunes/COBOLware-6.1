       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWONOF.
       AUTHOR.        COBOLware Services Ltda..
       DATE-WRITTEN.  08/07/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Liga/Desliga opcao                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ONOFF              PIC X(001) VALUE SPACE.
           05 SAVE-ONOFF         PIC X(001) VALUE SPACE.
           05 POSIT-ONOFF        PIC X(006) VALUE "000009".
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  CWONOF-FLAG           PIC X(001).
           88 CWONOF-ON          VALUE "1".
           88 CWONOF-OFF         VALUE "2".

       01  CWONOF-POSITIONS.
           05 CWONOF-STATUS-LINE    PIC 9(002).
           05 CWONOF-STATUS-COLUMN  PIC 9(002).
           05 CWONOF-HELP-LINE      PIC 9(002).
           05 CWONOF-HELP-COLUMN    PIC 9(002).

       01  TECLA               PIC 9(002) VALUE 0. COPY CWKEYS.

       COPY CWMOUS.

       PROCEDURE DIVISION USING CWONOF-FLAG CWONOF-POSITIONS TECLA
                                PARAMETROS-CWMOUS.
       X91.
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER.

       000-INICIO.

           IF   CWONOF-FLAG = "1"
                DISPLAY (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN)
                        "Ligado   "
                      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           ELSE
                DISPLAY (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN)
                        "Desligado"
                      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 0
           END-IF

           IF   X91-PARAMETER < 3
                GOBACK
           END-IF
           MOVE 0                       TO TECLA
           MOVE CWONOF-POSITIONS (5: 4) TO POSIT-ONOFF (1: 4)
           IF   CWONOF-FLAG = "1"
                MOVE "L" TO ONOFF
           ELSE
                MOVE "D" TO ONOFF
           END-IF
           MOVE ONOFF  TO SAVE-ONOFF
           MOVE CWONOF-POSITIONS (1: 4) TO CWMOUS-CURSOR-POSITION
           IF   CWONOF-FLAG = "1"
                IF   POSIT-ONOFF NOT = "000009"
                     CALL "CWMSGW" USING POSIT-ONOFF "[D]esliga"
                     MOVE ALL X"02" TO CWMOUS-LINE(CWONOF-HELP-LINE)
                                            (CWONOF-HELP-COLUMN: 09)
                     CALL "CWMOUS" USING PARAMETROS-CWMOUS
                ELSE
                     MOVE 255 TO CWMOUS-KEY
                END-IF
                EVALUATE CWMOUS-KEY
                    WHEN 1     SET ESC  TO TRUE
                    WHEN 2     MOVE "D" TO ONOFF
                    WHEN OTHER
                         ACCEPT
                        (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN) ONOFF
                                  WITH UPDATE AUTO-SKIP
                                 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                        ACCEPT TECLA FROM ESCAPE KEY
                        IF   PAGE-UP OR PAGE-DOWN
                             MOVE SPACE TO ONOFF
                        END-IF
                END-EVALUATE
           ELSE
                IF   POSIT-ONOFF NOT = "000009"
                     CALL "CWMSGW" USING POSIT-ONOFF "[L]iga   "
                     MOVE ALL X"00" TO CWMOUS-LINE(CWONOF-HELP-LINE)
                                            (CWONOF-HELP-COLUMN: 09)
                     MOVE ALL X"02" TO CWMOUS-LINE(CWONOF-HELP-LINE)
                                            (CWONOF-HELP-COLUMN: 06)
                     CALL "CWMOUS" USING PARAMETROS-CWMOUS
                ELSE
                     MOVE 255 TO CWMOUS-KEY
                END-IF
                EVALUATE CWMOUS-KEY
                    WHEN 1     SET ESC TO TRUE
                    WHEN 2     MOVE "L" TO ONOFF
                    WHEN OTHER
                         ACCEPT
                        (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN) ONOFF
                                  WITH UPDATE AUTO-SKIP
                                 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                        ACCEPT TECLA FROM ESCAPE KEY
                        IF   PAGE-UP OR PAGE-DOWN
                             MOVE SPACE TO ONOFF
                        END-IF
                END-EVALUATE
           END-IF
           INSPECT ONOFF CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   ONOFF NOT = SAVE-ONOFF
                IF   CWONOF-FLAG = "1"
                     MOVE "2" TO CWONOF-FLAG
                ELSE
                     MOVE "1" TO CWONOF-FLAG
                END-IF
                GO TO 000-INICIO
           END-IF
           IF   CWONOF-FLAG = "1"
                DISPLAY (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN)
                        "Ligado   "
                      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
           ELSE
                DISPLAY (CWONOF-STATUS-LINE, CWONOF-STATUS-COLUMN)
                        "Desligado"
                      WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 0
           END-IF
           CALL "CWMSGW" USING POSIT-ONOFF "         ".

       000-99-FIM. GOBACK.

       END PROGRAM CWONOF.
