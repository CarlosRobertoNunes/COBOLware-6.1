       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTRIM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/04/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Alinha … esquerda conte£do de vari vel       *
                      *  Eliminando espa‡os a direirta e devolvendo   *
                      *  O novo tamanho da string                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 WS-BUFFER           PIC X(32768).
           05 I            COMP-5 PIC S9(004).
           05 X            COMP-5 PIC S9(004).
           05 VIRGULA      COMP-5 PIC S9(004).
           05 PONTO        COMP-5 PIC S9(004).
           05 POS-VIRGULA  COMP-5 PIC S9(004).
           05 POS-PONTO    COMP-5 PIC S9(004).
           05 DIGITOS      COMP-5 PIC S9(004).
           05 NEGATIVO     COMP-5 PIC S9(004).
           05 POSITIVO     COMP-5 PIC S9(004).
           05 TOT          COMP-5 PIC S9(004).
           05 LB           COMP-5 PIC S9(004).

       01  X91.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  BUFFER-IN     PIC X.
       01  LENGTH-BUFFER PIC 9(8) COMP-X.
       01  BUFFER        PIC X.

       PROCEDURE DIVISION USING BUFFER-IN LENGTH-BUFFER BUFFER.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER < 3
                SET ADDRESS OF BUFFER TO ADDRESS OF BUFFER-IN
           ELSE
                MOVE BUFFER-IN (1:LENGTH-BUFFER)
                  TO BUFFER    (1:LENGTH-BUFFER)
           END-IF

           INSPECT BUFFER(1:LENGTH-BUFFER)
                   CONVERTING LOW-VALUES TO SPACE
           PERFORM 010-REBARBAS THRU 010-99-FIM

           INITIALIZE AREAS-DE-TRABALHO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH-BUFFER
                                            OR X > 0
                   EVALUATE TRUE
                       WHEN BUFFER(I:1) = ','   ADD  1 TO VIRGULA TOT
                                                MOVE I TO POS-VIRGULA
                       WHEN BUFFER(I:1) = '.'   ADD  1 TO PONTO TOT
                                                MOVE I TO POS-PONTO
                       WHEN BUFFER(I:1) NUMERIC ADD  1 TO DIGITOS  TOT
                       WHEN BUFFER(I:1) = '-'
                            AND (I = 1 OR LENGTH-BUFFER)
                                                ADD  1 TO NEGATIVO TOT
                       WHEN BUFFER(I:1) = '+'
                            AND (I = 1 OR LENGTH-BUFFER)
                                                ADD  1 TO POSITIVO TOT
                       WHEN OTHER               ADD  1 TO X
           END-PERFORM
           IF  POSITIVO > 0 AND NEGATIVO > 0
               CONTINUE
           ELSE
               IF  X = 0 AND TOT = LENGTH-BUFFER
               AND NEGATIVO < 2
               AND POSITIVO < 2
                   INSPECT BUFFER(1:LENGTH-BUFFER)
                           CONVERTING '+-' TO '  '
                   IF (PONTO = 1
                   AND POS-VIRGULA > POS-PONTO)
                   OR (POS-PONTO = 0
                   AND VIRGULA = 1)
                       INSPECT BUFFER(1:LENGTH-BUFFER)
                               CONVERTING '.,' TO ',.'
                       INSPECT BUFFER(1:LENGTH-BUFFER)
                               CONVERTING ',' TO ' '
                       MOVE SPACES TO WS-BUFFER
                       MOVE 0      TO X
                       PERFORM VARYING I FROM 1 BY 1
                                 UNTIL I > LENGTH-BUFFER
                               IF BUFFER(I:LENGTH-BUFFER) NOT = SPACE
                                  ADD 1 TO X
                                  MOVE BUFFER(I:LENGTH-BUFFER)
                                    TO WS-BUFFER(X:1)
                               END-IF
                       END-PERFORM
                       MOVE X TO LENGTH-BUFFER
                       MOVE WS-BUFFER(1:LENGTH-BUFFER)
                         TO    BUFFER(1:LENGTH-BUFFER)
                   ELSE
                       PERFORM 010-REBARBAS THRU 010-99-FIM
                   END-IF
                   MOVE SPACES TO WS-BUFFER
                   MOVE LENGTH-BUFFER TO LB
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH-BUFFER
                                       OR ((BUFFER(I:1) NOT = ZERO)
                                       AND (BUFFER(I:1) NOT = SPACE))
                              SUBTRACT 1 FROM LB
                   END-PERFORM
                   IF NEGATIVO = 1
                      MOVE '-' TO WS-BUFFER
                      MOVE 1   TO X
                   ELSE
                      MOVE 0   TO X
                   END-IF
                   IF BUFFER(I:1) = '.'
                   OR I > LENGTH-BUFFER
                      ADD 1    TO X
                      MOVE '0' TO WS-BUFFER(X:)
                   END-IF
                   ADD  1  TO X
                   MOVE BUFFER(I:LB)               TO WS-BUFFER(X:)
                   MOVE SPACES                     TO BUFFER
                                                      (1:LENGTH-BUFFER)
                   MOVE LB TO LENGTH-BUFFER
                   SUBTRACT 1                    FROM X
                   ADD  X                          TO LENGTH-BUFFER
                   MOVE WS-BUFFER(1:LENGTH-BUFFER) TO BUFFER
                                            (1:LENGTH-BUFFER)
               END-IF
           END-IF.

       000-99-FIM. GOBACK.

       010-REBARBAS.

           MOVE LENGTH-BUFFER TO LB
           IF   BUFFER(LB:1) = SPACE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I = LENGTH-BUFFER
                        OR BUFFER(I:LB) = SPACES
                           SUBTRACT 1 FROM LB
                END-PERFORM
                COMPUTE LENGTH-BUFFER = I - 1
           END-IF
           MOVE LENGTH-BUFFER TO LB
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = LENGTH-BUFFER
                   OR BUFFER(I:1) NOT = SPACE
                      SUBTRACT 1 FROM LB
           END-PERFORM
           MOVE LB                         TO LENGTH-BUFFER
           MOVE BUFFER(I:LENGTH-BUFFER)    TO WS-BUFFER
           MOVE SPACES                     TO BUFFER(I:LENGTH-BUFFER)
           MOVE WS-BUFFER(1:LENGTH-BUFFER) TO BUFFER(1:LENGTH-BUFFER).

       010-99-FIM. EXIT.

       END PROGRAM CWTRIM.
