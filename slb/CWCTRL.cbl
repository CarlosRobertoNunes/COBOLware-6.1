       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCTRL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  13/08/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de CONTROL (ACCEPT/DISPLAY)       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT CTRL   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CTRL-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CTRL
                  RESERVE NO ALTERNATE AREA.

      *    SELECT TXT   ASSIGN TO "E:\CTRL.TXT"
      *                 ORGANIZATION  IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CTRL
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CTRL.

       01  CTRL-REG.
           05 CTRL-CHAVE                   PIC  X(080).
           05 CTRL-MODE                    PIC  X(001).
           05 CTRL-FORE                    PIC  9(001).
           05 CTRL-BACK                    PIC  9(001).
           05 CTRL-SECURE                  PIC  X(001).
           05 CTRL-BLINK                   PIC  X(001).
           05 CTRL-BZERO                   PIC  X(001).
           05 CTRL-EMPTY                   PIC  X(001).
           05 CTRL-BEEP                    PIC  X(001).
           05 CTRL-REVERSE                 PIC  X(001).
           05 CTRL-AUTO                    PIC  X(001).
           05 CTRL-HIGH                    PIC  X(001).
           05 CTRL-UPPLOW                  PIC  X(001).
           05 CTRL-ADVANCE                 PIC  X(001).

      *FD  TXT.
      *01  TXT-REG PIC X(100).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 I           PIC  9(002) VALUE 0.
           05 P           PIC  9(002) VALUE 0.
           05 N           PIC  9(002) VALUE 0.
           05 NUMERO      PIC  9(018) VALUE 0.
           05 PALAVRA     PIC  X(080) VALUE SPACES.
           05 ER-CTRL.
              10 FS-CTRL  PIC  X(002) VALUE "00".
              10 LB-CTRL  PIC  X(255) VALUE "$TEMP/cwctrl3.".

       LINKAGE SECTION.

       01  LK-CTRL                       PIC  X(080).
       01  LK-ATTR.
           05 LK-MODE                    PIC  X(001).
           05 LK-FORE                    PIC  9(001).
           05 LK-BACK                    PIC  9(001).
           05 LK-SECURE                  PIC  X(001).
           05 LK-BLINK                   PIC  X(001).
           05 LK-BZERO                   PIC  X(001).
           05 LK-EMPTY                   PIC  X(001).
           05 LK-BEEP                    PIC  X(001).
           05 LK-REVERSE                 PIC  X(001).
           05 LK-AUTO                    PIC  X(001).
           05 LK-HIGH                    PIC  X(001).
           05 LK-UPPLOW                  PIC  X(001).
           05 LK-ADVANCE                 PIC  X(001).

       PROCEDURE DIVISION USING LK-CTRL LK-ATTR.

       INICIO.

           IF   LK-CTRL(1:50) = SPACES
                GOBACK
           END-IF

           ON 1
              MOVE ","  TO MINUSCULAS (50: 1)
              MOVE " "  TO MAIUSCULAS (50: 1)
              OPEN I-O CTRL.

           INSPECT LK-CTRL CONVERTING MINUSCULAS TO MAIUSCULAS.
           MOVE LOW-VALUES TO CTRL-REG
           MOVE LK-CTRL    TO CTRL-CHAVE
           READ CTRL
                INVALID KEY
                        PERFORM INTERPRETA THRU FIM-INTERPRETA
           END-READ

           IF   CTRL-FORE    NOT = LOW-VALUE
                MOVE CTRL-FORE    TO LK-FORE
           END-IF

           IF   CTRL-BACK    NOT = LOW-VALUE
                MOVE CTRL-BACK    TO LK-BACK
           END-IF

           IF   CTRL-SECURE  NOT = LOW-VALUE
                MOVE CTRL-SECURE  TO LK-SECURE
           END-IF

           IF   CTRL-BLINK   NOT = LOW-VALUE
                MOVE CTRL-BLINK   TO LK-BLINK
           END-IF

           IF   CTRL-BZERO   NOT = LOW-VALUE
                MOVE CTRL-BZERO   TO LK-BZERO
           END-IF

           IF   CTRL-EMPTY   NOT = LOW-VALUE
                MOVE CTRL-EMPTY   TO LK-EMPTY
           END-IF

           IF   CTRL-BEEP    NOT = LOW-VALUE
                MOVE CTRL-BEEP    TO LK-BEEP
           END-IF

           IF   CTRL-REVERSE NOT = LOW-VALUE
                MOVE CTRL-REVERSE TO LK-REVERSE
           END-IF

           IF   CTRL-AUTO    NOT = LOW-VALUE
                MOVE CTRL-AUTO    TO LK-AUTO
           END-IF

           IF   CTRL-HIGH    NOT = LOW-VALUE
                MOVE CTRL-HIGH    TO LK-HIGH
           END-IF

           IF   CTRL-UPPLOW  NOT = LOW-VALUE
                MOVE CTRL-UPPLOW  TO LK-UPPLOW
           END-IF

           IF   CTRL-ADVANCE NOT = LOW-VALUE
                MOVE CTRL-ADVANCE TO LK-ADVANCE
           END-IF.

       FIM. GOBACK.

       INTERPRETA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
                   PERFORM SKIP-WORD THRU FIM-SKIP-WORD
                   EVALUATE TRUE
                       WHEN PALAVRA = "IS"
                         OR PALAVRA = "ARE"
                            CONTINUE
                       WHEN PALAVRA = "FOREGROUND-COLOR"
                            PERFORM SKIP-WORD THRU FIM-SKIP-WORD
                            MOVE 0  TO NUMERO
                            MOVE 18 TO N
                            PERFORM UNTIL P = 0
                                    MOVE PALAVRA (P: 1) TO NUMERO (N: 1)
                                    SUBTRACT 1 FROM P N
                            END-PERFORM
                            PERFORM UNTIL NUMERO < 8
                                    SUBTRACT 7 FROM NUMERO
                            END-PERFORM
                            MOVE NUMERO TO CTRL-FORE
                       WHEN PALAVRA = "BACKGROUND-COLOR"
                            PERFORM SKIP-WORD THRU FIM-SKIP-WORD
                            MOVE 0  TO NUMERO
                            MOVE 18 TO N
                            PERFORM UNTIL P = 0
                                    MOVE PALAVRA (P: 1) TO NUMERO (N: 1)
                                    SUBTRACT 1 FROM P N
                            END-PERFORM
                            PERFORM UNTIL NUMERO < 8
                                    SUBTRACT 7 FROM NUMERO
                            END-PERFORM
                            MOVE NUMERO TO CTRL-BACK
                       WHEN PALAVRA = "HIGHLIGHT"
                         OR  PALAVRA = "HIGH"
                             MOVE "H" TO CTRL-HIGH
                   END-EVALUATE
           END-PERFORM.
      *    MOVE CTRL-REG TO TXT-REG
      *    INSPECT TXT-REG CONVERTING X"00" TO "."
      *    OPEN EXTEND TXT
      *    WRITE TXT-REG
      *    CLOSE TXT
      *    WRITE CTRL-REG.

       FIM-INTERPRETA. EXIT.

       SKIP-WORD.

           MOVE 0      TO P
           MOVE SPACES TO PALAVRA
           PERFORM VARYING I FROM I BY 1
                     UNTIL I > 80
                        OR (LK-CTRL (I: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           PERFORM VARYING I FROM I BY 1
                     UNTIL I > 80
                        OR (LK-CTRL (I: 1) = SPACE)
                   ADD  1              TO P
                   MOVE LK-CTRL (I: 1) TO PALAVRA (P: 1)
           END-PERFORM

           IF   PALAVRA = "IS" OR = "ARE"
                GO TO SKIP-WORD
           END-IF.

       FIM-SKIP-WORD. EXIT.

       END PROGRAM CWCTRL.
