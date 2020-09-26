       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWAIT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  12/12/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Aguarda N segundos                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TIME-A                  PIC  X(006) VALUE SPACES.
           05 TIME-B                  PIC  X(006) VALUE SPACES.
           05 ESPERA                  PIC  9(018) VALUE ZERO.

       LINKAGE SECTION.

       COPY CWWAIT.

       PROCEDURE DIVISION USING PARAMETROS-CWWAIT.

       000-INICIO.

           MOVE 0 TO ESPERA
           ACCEPT TIME-A FROM TIME
           PERFORM UNTIL ESPERA NOT < CWWAIT-SECONDS
                ACCEPT TIME-B FROM TIME
                IF TIME-B NOT = TIME-A
                   MOVE TIME-B TO TIME-A
                   ADD  1      TO ESPERA
                END-IF
           END-PERFORM

           MOVE ZERO TO CWWAIT-SECONDS.

       000-99-FIM. GOBACK.
       END PROGRAM CWWAIT.
