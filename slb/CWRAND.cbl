       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRAND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/12/2010.
       SECURITY.      *************************************************
                      *                                               *
                      * Retorna numero randomico                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 USUARIO.
              10 U PIC 9(2) COMP-X OCCURS 30.
           05 TASK    PIC 9(6) VALUE 0.
           05 HOJE PIC 9(6) VALUE 0.
           05 P    PIC 9(2) VALUE 0.
           05 REDEFINES P.
              10   PIC X(1).
              10 X PIC 9(1).
           05 CC-0 PIC 9(2) VALUE 0.
           05 HORA PIC 9(8) VALUE 0.
           05 REDEFINES HORA.
              10 HH PIC 99.
              10 MM PIC 99.
              10 SS PIC 99.
              10 CC PIC 99.
           05 RANDOM-W PIC V9(18).
           05 RANDOM-A PIC 9(18) VALUE 0.

       LINKAGE SECTION.

       01  RANDOM-L PIC 9(18) VALUE 0.

       PROCEDURE DIVISION USING RANDOM-L.

       000-INICIO.

           ON 1
              EXEC COBOLware GetSystem
                   TASK TASK
                   USER USUARIO
              END-EXEC.
           ACCEPT HOJE FROM DATE
           MOVE RANDOM-L TO RANDOM-A
           MOVE 0        TO P

           PERFORM TEST AFTER UNTIL P < 19
                               AND (RANDOM-L NOT = RANDOM-A)
                               AND (RANDOM-L (1: 5)  NOT = '00000')
                               AND (RANDOM-L (15: 4) NOT = '0000')
               ACCEPT HORA FROM TIME
               ADD 1     TO CC-0
               IF CC = 0
                  MOVE CC-0 TO CC
               END-IF
               ADD    HOJE TO HORA
               PERFORM 2 TIMES
                  ADD TASK TO HORA
                  ADD CC-0 TO HH
                  COMPUTE RANDOM-W = HORA ** (1 / CC)
                  IF  P = 0
                  OR  RANDOM-L (1: 5) = '00000'
                      MOVE RANDOM-W (1: 6) TO RANDOM-L (1: 6)
                  END-IF
                  PERFORM VARYING P FROM 1 BY 1
                            UNTIL P > 18
                               OR (RANDOM-W (P:1) NOT = '0')
                          CONTINUE
                  END-PERFORM
                  IF  P < 19
                      MOVE RANDOM-W (P: 1) TO X
                      MOVE RANDOM-W (X: 8) TO HORA
                      IF P < 4
                         ADD U (P)         TO HORA
                      END-IF
                  END-IF
               END-PERFORM
               MOVE RANDOM-W (P: 12) TO RANDOM-L (7: 12)
               ACCEPT HORA FROM TIME
               IF CC = 0
                  COMPUTE CC = MM + CC-0
                  MOVE MM TO RANDOM-L (5:2)
               END-IF
               MOVE CC TO RANDOM-L (5:2)
           END-PERFORM.

       000-99-FIM. GOBACK.

       END PROGRAM CWRAND.
