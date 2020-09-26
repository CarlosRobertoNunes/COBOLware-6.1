       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL7.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  24/06/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 7 - ACCEPT de campo numerico de       *
                      *             formato variavel                  *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 TECLA-CWUSER         PIC  9(003) VALUE 0.
           05 TESTE-SINAL          PIC S9(018) VALUE 0.
           05 INT                  PIC  9(002) VALUE 0.
           05 A                    PIC  9(002) VALUE 0.
           05 B                    PIC  9(002) VALUE 0.
           05 I                    PIC  9(002) VALUE 0.
           05 P                    PIC  9(002) VALUE 0.
           05 P2                   PIC  9(002) VALUE 0.
           05 SINAL                PIC  X(001) VALUE SPACE.

       01  MATRIZ.
           05 P0S-R7.
              10 LIN-R7      PIC 9(002).
              10 COL-R7      PIC 9(002).
           05 LEN-R7         PIC 9(002).
           05 ATTR-R7.
              10 MODE-R7            PIC  X(001).
              10 FORE-R7            PIC  X(001).
              10 BACK-R7            PIC  X(001).
              10 SECURE-R7          PIC  X(001).
              10 BLINK-B            PIC  X(001).
              10 BZERO-R7           PIC  X(001).
              10 EMPTY-R7           PIC  X(001).
              10 BEEP-R7            PIC  X(001).
              10 REVERSE-R7         PIC  X(001).
              10 AUTO-R7            PIC  X(001).
              10 HIGH-R7            PIC  X(001).
              10 UPPLOW-R7          PIC  X(001).
              10 ADVANCE-R7         PIC  X(001).
              10 LENF-R7     PIC 9(002).
              10 FILLER      PIC X(003).
           05 FILLER         PIC X(013).
           05 ATTR-INIT-R7   PIC X(001).
           05 COM-SINAL-R7   PIC X(001).
           05 MODO-ACCEPT-R7 PIC X(001).
           05 PIC-R7         PIC X(080).
           05 DATANAME-R7    PIC X(030).
           05 DATA-R7        PIC X(080).
           05 CTRL-R7        PIC X(080).

       01  CBL-READ-WRITE-SCR-CHARS-ATTR.
           10 SCREEN-POSITION.
              15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
              15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
           10 ATTRIBUTE-BUFFER     PIC  X(080).
           10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.

       LINKAGE SECTION.

       01  OPCAO  PIC X(001).
       01  NUMERO PIC X(018).
       01  REDEFINES NUMERO.
           05 N OCCURS 18 PIC S9.
       01  TIPO   PIC X(001).
       01  S      PIC 9(003).
       01  DEC    PIC 9(002).
       01  L      PIC 9(002).
       01  C      PIC 9(002).
       01  TECLA  PIC 9(002).                         COPY CWKEYS.

       PROCEDURE DIVISION USING OPCAO NUMERO TIPO S DEC L C TECLA.

       000-INICIO.

           INITIALIZE MATRIZ
           COMPUTE P = 18 - S + 1
           INSPECT NUMERO CONVERTING SPACES TO ZEROS
           MOVE NUMERO (1: S)   TO TESTE-SINAL (P: S)

           IF   TESTE-SINAL NEGATIVE
                MOVE "-" TO SINAL
           ELSE
                MOVE "+" TO SINAL
           END-IF
           If DEC NOT = 0
              MOVE " fbsbZebrahua" TO ATTR-R7
           ELSE
              MOVE " fbsbzebrahua" TO ATTR-R7
           END-IF
           MOVE "$CWREL7"       TO DATANAME-R7
           MOVE L               TO LIN-R7
           MOVE C               TO COL-R7
           MOVE SPACES          TO DATA-R7
           MOVE "R"             TO REVERSE-R7
           MOVE "U"             TO MODE-R7

           IF   TIPO = "V" OR "v"
                COMPUTE INT = S - DEC
                COMPUTE I = INT / 3
                COMPUTE P = INT - ((I - 1) * 3)
                MOVE 0 TO A B
                PERFORM P TIMES
                        ADD 1 TO A B
                        MOVE NUMERO (A: 1) TO DATA-R7 (B: 1)
                END-PERFORM
                IF  P NOT = 0
                AND P < INT
                      ADD  1             TO B
                      MOVE "."           TO DATA-R7 (B: 1)
                      PERFORM I TIMES
                              PERFORM 3 TIMES
                                  ADD 1 TO A B
                                  MOVE NUMERO (A: 1) TO DATA-R7 (B: 1)
                              END-PERFORM
                              ADD  1             TO B
                              MOVE "."           TO DATA-R7 (B: 1)
                      END-PERFORM
                      SUBTRACT 1 FROM B
                END-IF
                IF  DEC NOT = 0
                    ADD 1 TO B
                    MOVE ","           TO DATA-R7 (B: 1)
                    PERFORM DEC TIMES
                            ADD 1 TO A B
                            MOVE NUMERO (A: 1) TO DATA-R7 (B: 1)
                    END-PERFORM
                END-IF
                ADD 1 TO B
                MOVE SINAL TO DATA-R7 (B: 1)
           ELSE
                IF   DEC = 0
                     MOVE NUMERO (1: S) TO DATA-R7
                ELSE
                     COMPUTE P = S - DEC
                     COMPUTE P2 = P + 1
                     IF  P NOT = 0
                          STRING NUMERO (1: P) ","
                                 NUMERO (P2: DEC) DELIMITED BY SIZE
                                 INTO DATA-R7
                     ELSE
                          STRING ","
                                 NUMERO (1: DEC) DELIMITED BY SIZE
                                 INTO DATA-R7
                     END-IF
                END-IF
           END-IF
           MOVE DATA-R7         TO PIC-R7
           INSPECT PIC-R7 CONVERTING "1234567890-" TO "ZZZZZZZZZZ+"
           IF   DEC NOT = 0
                PERFORM VARYING P FROM 30 BY -1
                        UNTIL P = 2
                           OR PIC-R7 (P:  1) = ","
                         CONTINUE
                END-PERFORM
                SUBTRACT 1 FROM P
                INSPECT PIC-R7 (P: ) CONVERTING "Z" TO "9"
           END-IF

           PERFORM VARYING P FROM 30 BY -1
                   UNTIL P = 1
                      OR PIC-R7 (P:  1) NOT = SPACE
                    CONTINUE
           END-PERFORM

           MOVE P  TO LEN-R7 LENF-R7 STRING-LENGTH
           COMPUTE ROW-NUMBER    = L - 1
           COMPUTE COLUMN-NUMBER = C - 1
           CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                           ATTRIBUTE-BUFFER
                                           STRING-LENGTH
           IF  OPCAO = "A" OR "a"
               CALL "CWUSER" USING "ACWREL7  ," MATRIZ
                                                LENGTH OF MATRIZ
                                                TECLA-CWUSER
               MOVE SPACES TO DATA-R7 (P + 1:)
               MOVE TECLA-CWUSER TO TECLA
               MOVE SPACES       TO NUMERO
               MOVE SPACE        TO SINAL
               MOVE S            TO I
               ADD  1            TO I
               PERFORM VARYING P FROM P BY -1 UNTIL P = 0
                       IF   DATA-R7 (P:1) NUMERIC
                            SUBTRACT 1 FROM I
                            MOVE DATA-R7 (P:1) TO NUMERO (I: 1)
                            IF  SINAL = "-"
                                COMPUTE N (P) = N (P) * -1
                            END-IF
                            IF  I = 1
                                MOVE 0 TO P
                            END-IF
                       ELSE
                            IF   DATA-R7 (P:1) = "-"
                                 MOVE "-" TO SINAL
                            END-IF
                       END-IF
               END-PERFORM
           ELSE
               CALL "CWUSER" USING "DCWREL7  ," MATRIZ
                                                LENGTH OF MATRIZ
           END-IF
           CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                            ATTRIBUTE-BUFFER
                                            STRING-LENGTH
           GOBACK.

       END PROGRAM CWREL7.
