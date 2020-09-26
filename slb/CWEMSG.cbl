       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEMSG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/05/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Emite mensagen de erro "X(030)"             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 MULTI-USER               PIC  9(001) VALUE 0.
           05 TMP                      PIC  X(003) VALUE SPACES.
           05 CONTA-LOOP               PIC  9(004) VALUE 0.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
           05 NOME                     PIC  X(030) VALUE "LOGON".
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 SALVA                    PIC  X(080) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE "?".
           05 MSG-D                    PIC  X(030) VALUE SPACES.
           05 HORA-1.
              10 HH-1                  PIC  9(002).
              10 MM-1                  PIC  9(002).
              10 SS-1                  PIC  9(002).
              10 CC-1                  PIC  9(002).
           05 HORA-2.
              10 HH-2                  PIC  9(002).
              10 MM-2                  PIC  9(002).
              10 SS-2                  PIC  9(002).
              10 CC-2                  PIC  9(002).
           05 SEGUNDOS-1               PIC  9(008) VALUE 0.
           05 SEGUNDOS-2               PIC  9(008) VALUE 0.
           05 SEGUNDOS                 PIC  9(002) VALUE 0.
           05 CX                       PIC  9(008) COMP-X VALUE 0.
           05 CX-3                     PIC  9(002) VALUE 0.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 TIMER                    PIC  9(002) VALUE ZERO.
           05 TESTE-ERASE              PIC  X(001) VALUE "N".
              88 ERASE-ON                          VALUE "S" "s"
                                                         "Y" "y".
           05 TESTE-STOP               PIC  X(001) VALUE "N".
              88 STOP-ON                           VALUE "S" "s"
                                                         "Y" "y".
           05 TESTE-BEEP               PIC  X(001) VALUE "S".
              88 BEEP-ON                           VALUE "S" "s"
                                                         "Y" "y".
       COPY CWSEND.
       COPY CWUNIX.
       COPY CWCONF.

       LINKAGE SECTION.

       01  MSG PIC X(030).

       PROCEDURE DIVISION USING MSG.

       000-INICIO.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           IF   ERASE-ON
                CALL "CBL_READ_SCR_CHARS" USING X"1602"
                                                SALVA
                                                X"0044"
           END-IF
           ON   1
                DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 2) = "ON"
                     MOVE 1 TO MULTI-USER
                END-IF
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "?"         TO SET-LOG
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE 1 TO MULTI-USER
                END-IF
                CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                        MOVE "92" TO CWCONF-REG92
                        MOVE NOME TO CWCONF-ELEMENTO
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-IGNORE-LOCK TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                  FS-CWCONF KCO PCO
                        IF   FS-CWCONF = "23"
                        AND  NOME NOT = SPACES
                             MOVE SPACES TO NOME
                             MOVE "9D"   TO FS-CWCONF
                        END-IF
                END-PERFORM
                IF   FS-CWCONF = "23"
                     MOVE "S"          TO TESTE-BEEP
                                          TESTE-STOP
                                          TESTE-ERASE
                END-IF
                IF   FS-CWCONF < "10"
                     MOVE CWCONF-BEEP  TO TESTE-BEEP
                     MOVE CWCONF-STOP  TO TESTE-STOP
                     MOVE CWCONF-ERASE TO TESTE-ERASE
                     MOVE CWCONF-TIMER TO TIMER
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG
                                    FS-CWCONF KCO PCO.

           CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                            ESPACOS
                                            X"0044"

           IF   TIMER < 3
           AND  ERASE-ON
           AND  NOT STOP-ON
                MOVE 3 TO TIMER
           END-IF

           IF   MSG NOT EQUAL SPACES
                MOVE SPACE TO NADA
                PERFORM VARYING CX FROM 30 BY -1
                        UNTIL NADA NOT = SPACE
                        MOVE MSG (CX: 1) TO NADA
                END-PERFORM
                MOVE MSG TO MSG-D
                ADD  2   TO CX
                IF  STOP-ON AND ERASE-ON
                    PERFORM 120-BUTTON THRU 120-99-FIM
                ELSE
                CALL "CWMSGW" USING "2303.." MSG-D CX
                IF   BEEP-ON
                     CALL X"E5"
                END-IF
                IF   TIMER NOT = 0
                     ACCEPT HORA-1 FROM TIME
                     MOVE   0        TO SEGUNDOS
                     MOVE   HORA-1   TO HORA-2
                     COMPUTE SEGUNDOS-1 =  HH-1 * 60
                     COMPUTE SEGUNDOS-1 = (SEGUNDOS-1 + MM-1) * 60
                     COMPUTE SEGUNDOS-1 =  SEGUNDOS-1 + SS-1
                     MOVE 0 TO CONTA-LOOP
                     PERFORM UNTIL (SEGUNDOS NOT < TIMER)
                                OR (HORA-1 > HORA-2)
                     ADD 1 TO CONTA-LOOP
                     IF   CONTA-LOOP = 1
                     OR   MULTI-USER = 0
                          ACCEPT HORA-2 FROM TIME
                          COMPUTE SEGUNDOS-2 =  HH-2 * 60
                          COMPUTE SEGUNDOS-2 = (SEGUNDOS-2 + MM-2) * 60
                          COMPUTE SEGUNDOS-2 =  SEGUNDOS-2 + SS-2
                          COMPUTE SEGUNDOS   =  SEGUNDOS-2 - SEGUNDOS-1
                          COMPUTE CX-3 = CX + 3
                          DISPLAY (23, CX-3)  SEGUNDOS "/" TIMER
                     END-IF
                     END-PERFORM
                END-IF
                IF   STOP-ON
                     PERFORM 110-PAUSA THRU 110-99-FIM
                END-IF
                IF   ERASE-ON
                     CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                     SALVA
                                                     X"0044"
                END-IF
           END-IF.

       000-FIM. EXIT PROGRAM.

       110-PAUSA.

          EXEC COBOLware LINE
               LINE FOOTLINE COLUMN 57 TYPE "R"
               LINE-END 23
               SCREEN(1) "<Enter>-Segue"
          END-EXEC.

       110-99-FIM. EXIT.

       120-BUTTON.

           IF   BEEP-ON
                CALL X"E5"
           END-IF
           MOVE MSG-D       TO CWSEND-MSG
           CALL "CWSEND" USING PARAMETROS-CWSEND.

       120-99-FIM. EXIT.

       END PROGRAM CWEMSG.

