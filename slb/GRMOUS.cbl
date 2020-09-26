       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRMOUS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/05/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula escape key pelo mouse                 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                        PIC  9(002) VALUE 0.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       COPY CWMOUS.

       LINKAGE SECTION.

       01  PARAMETROS-GRMOUS.
           05 GRMOUS-CONTROL.
              10 GRMOUS-LINE OCCURS 25.
                 15 GRMOUS-POSIT OCCURS 80 PIC  9(002) COMP-X.
              10 GRMOUS-MODE               PIC  9(002) COMP-X.
              10 GRMOUS-KEY                PIC  9(002) COMP-X.
              10 GRMOUS-BUTTON             PIC  9(002) COMP-X.
           05 GRMOUS-CURSOR-POSITION.
              10 GRMOUS-CURSOR-LIN         PIC  9(002).
              10 GRMOUS-CURSOR-COL         PIC  9(002).

       01  PARAMETROS2-GRMOUS.
           05 GRMOUS-LIN                PIC  9(002) COMP-X.
           05 GRMOUS-COL                PIC  9(002) COMP-X.
           05 GRMOUS-LENGTH             PIC  9(002) COMP-X.
           05 GRMOUS-STRING             PIC  X(080) OCCURS 254.
           05 GRMOUS-COLOR              PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING PARAMETROS-GRMOUS PARAMETROS2-GRMOUS.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 2
                MOVE PARAMETROS2-GRMOUS TO CWMOUS-MENSAGES
           ELSE
                MOVE LOW-VALUES         TO CWMOUS-MENSAGES
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 25
                   MOVE GRMOUS-LINE (I) TO CWMOUS-LINE (I)
           END-PERFORM
           MOVE GRMOUS-MODE   TO CWMOUS-MODE
           MOVE GRMOUS-KEY    TO CWMOUS-KEY
           MOVE GRMOUS-BUTTON TO CWMOUS-BUTTON

           IF   GRMOUS-CURSOR-POSITION NOT NUMERIC
                MOVE 0 TO CWMOUS-CURSOR-LIN
                          CWMOUS-CURSOR-COL
           ELSE
                MOVE GRMOUS-CURSOR-POSITION TO CWMOUS-CURSOR-POSITION
           END-IF

           SET   CWMOUS-TIMEOUT-ENABLE
                 CWMOUS-TIMEOUT-OFF TO TRUE

           CALL "CWMOUS" USING PARAMETROS-CWMOUS
           MOVE CWMOUS-KEY    TO GRMOUS-KEY
           MOVE CWMOUS-BUTTON TO GRMOUS-BUTTON.


       000-99-FIM. GOBACK.
       END PROGRAM GRMOUS.
