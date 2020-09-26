       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTASK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Avanca o contador de TASK ou de SPOOL        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TRAVADO             PIC X(002) VALUE "9D".
           05 LB-CWCONF           PIC X(255) VALUE "cwconf".

       COPY CWUNIX.
       COPY CWCONF.
       LINKAGE SECTION.

       01   OPCAO PIC 9(001).
            88 OPCAO-OK VALUE 1 THRU 9.
       01   TASK  PIC 9(006).

       PROCEDURE DIVISION USING OPCAO TASK.

       000-INICIO.

           IF   NOT OPCAO-OK
                GOBACK
           END-IF
           DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
           ACCEPT  LB-CWCONF FROM ENVIRONMENT-VALUE
           IF  OPCAO = 1
               CALL "CWUNIX" USING PARAMETROS-CWUNIX
               IF   CWUNIX-ON
                    DISPLAY "CWCONF"       UPON ENVIRONMENT-NAME
                    DISPLAY "$TEMP/cwtask" UPON ENVIRONMENT-VALUE
               END-IF
           END-IF

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF > "09"
           AND  CWUNIX-ON
           AND  OPCAO = 1
                SET CWSQLC-CREATE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   FS-CWCONF > "09"
                DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
                DISPLAY LB-CWCONF UPON ENVIRONMENT-VALUE
                DISPLAY "UNIX"    UPON ENVIRONMENT-NAME
                GOBACK
           ELSE
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                SET CWSQLC-UPDATE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "TK" TO CWCONF-CHAVE
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF = "23"
                     INITIALIZE CWCONF-REGTK
                     MOVE "TK" TO CWCONF-CHAVE
                     SET CWSQLC-WRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
                     DISPLAY LB-CWCONF UPON ENVIRONMENT-VALUE
                     DISPLAY "UNIX"    UPON ENVIRONMENT-NAME
                     GO TO 000-INICIO
                ELSE
                     ADD 1 TO CWCONF-TASK (OPCAO)
                     IF CWCONF-TASK (OPCAO)  = 0
                        MOVE 1 TO CWCONF-TASK (OPCAO)
                     END-IF
                     MOVE CWCONF-TASK (OPCAO) TO TASK
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
                     DISPLAY LB-CWCONF UPON ENVIRONMENT-VALUE
                     DISPLAY "UNIX"    UPON ENVIRONMENT-NAME
                END-IF
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CWTASK.
