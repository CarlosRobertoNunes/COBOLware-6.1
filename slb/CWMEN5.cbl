       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN5.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Configura mensagens de erro (CWEMSG)        *
                      *                                               *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
           05 NOME                     PIC  X(030) VALUE "LOGON".
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 CWCHECK-ERASE            PIC  X(001) VALUE "0".
           05 CWCHECK-STOP             PIC  X(001) VALUE "0".
           05 CWCHECK-BEEP             PIC  X(001) VALUE "1".

       COPY CWCONF.

       SCREEN SECTION.

       01  CTAC-LIT-CWMEN5.
           05 LINE 08 COLUMN 05 VALUE "Atributos de mensage".
           05 LINE 08 COLUMN 25 VALUE "ns".
           05 LINE 10 COLUMN 08 VALUE "Sonora".
           05 LINE 12 COLUMN 08 VALUE "Aguardar at‚ interve".
           05 LINE 12 COLUMN 28 VALUE "n‡Æo".
           05 LINE 12 COLUMN 36 VALUE "segundos".
           05 LINE 14 COLUMN 08 VALUE "Apagar mensagem ap¢s".
           05 LINE 14 COLUMN 28 VALUE " espera".

       01  CTAC-VAR-CWMEN5.
           05 LINE 10 COLUMN 05 PIC X(001) USING CWCHECK-ERASE.
           05 LINE 12 COLUMN 05 PIC X(001) USING CWCHECK-STOP.
           05 LINE 12 COLUMN 33 PIC 9(002) USING CWCONF-TIMER.
           05 LINE 14 COLUMN 05 PIC X(001) USING CWCHECK-BEEP.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           CALL "CWGETU" USING NOME TASK PROGRAMA '?'
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF > "09"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "92" TO CWCONF-REG92
                   MOVE NOME TO CWCONF-ELEMENTO
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   SET CWSQLC-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
                   IF   FS-CWCONF = "23"
                        MOVE "S" TO CWCONF-BEEP
                        MOVE "S" TO CWCONF-STOP
                        MOVE 0   TO CWCONF-TIMER
                        MOVE "S" TO CWCONF-ERASE
                        SET CWSQLC-WRITE TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-EQUAL TO TRUE
                        SET CWSQLC-LOCK TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                   END-IF
           END-PERFORM

           IF   FS-CWCONF > "09"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 2 CAPTION " ~Aplicar " WIDTH 9
                KEY 777
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 11 CAPTION " ~Cancelar " WIDTH 10
                KEY ESC
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 59 WIDTH 13
                     CAPTION " [esc]-~Sa¡da " KEY ESC TAB-OFF
           END-EXEC

           IF   CWCONF-ERASE-ON
                MOVE "1" TO CWCHECK-ERASE
           ELSE
                MOVE "0" TO CWCHECK-ERASE
           END-IF

           IF   CWCONF-STOP-ON
                MOVE "1" TO CWCHECK-STOP
           ELSE
                MOVE "0" TO CWCHECK-STOP
           END-IF

           IF   CWCONF-BEEP-ON
                MOVE "1" TO CWCHECK-BEEP
           ELSE
                MOVE "0" TO CWCHECK-BEEP
           END-IF

           DISPLAy CTAC-LIT-CWMEN5
           PERFORM UNTIL ESC
                   ACCEPT CTAC-VAR-CWMEN5
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   CWCHECK-ERASE = "0"
                        MOVE "N" TO CWCONF-ERASE
                   ELSE
                        MOVE "S" TO CWCONF-ERASE
                   END-IF
                   IF   CWCHECK-STOP = "0"
                        MOVE "N" TO CWCONF-STOP
                   ELSE
                        MOVE "S" TO CWCONF-STOP
                   END-IF
                   IF   CWCHECK-BEEP = "0"
                        MOVE "N" TO CWCONF-BEEP
                   ELSE
                        MOVE "S" TO CWCONF-BEEP
                   END-IF
                   IF   TECLA = 777
                        SET CWSQLC-REWRITE TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                   END-IF
           END-PERFORM

           EXEC COBOLware OBJECT (DROP) END-EXEC

           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-99-FIM. EXIT PROGRAM.
       END PROGRAM CWMEN5.
