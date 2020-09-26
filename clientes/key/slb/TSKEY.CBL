       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSKEY.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/04/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Teste da KEYBOXF                             *
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
       COPY CWBOXF.

       01  AREAS-DE-TRABALHO.
           05 Filename-DESCRICAO pic x(20) value spaces.

       PROCEDURE DIVISION.

       000-INICIO.
           MOVE "WSTU908" TO CWBOXF-PROGRAM
           MOVE 53 TO CWBOXF-HORIZONTAL-LENGTH
           MOVE "Cod.Produto Nome de Busca  " TO CWBOXF-TITLE
           MOVE 12 TO CWBOXF-STRING-1-LENGTH
           MOVE 40 TO CWBOXF-STRING-2-LENGTH
           MOVE 15 TO CWBOXF-LINE.
           MOVE 03 TO CWBOXF-COLUMN.
           MOVE 2  TO CWBOXF-VERTICAL-LENGTH.
           MOVE SPACES TO CWBOXF-OPTION.
      *    IF F2
      *       MOVE 1 TO CWBOXF-ORDER
      *    ELSE
              MOVE 2 TO CWBOXF-ORDER
      *    END-IF
                     CWBOXF-RETURN
           MOVE '01'          TO CWBOXF-WORK-AREA
          CALL "KEYBOXF" USING PARAMETROS-CWBOXF
          MOVE CWBOXF-OPTION TO Filename-DESCRICAO
          DISPLAY Filename-DESCRICAO at 0101
          if Filename-DESCRICAO = spaces
             stop run.
          go to 000-INICIO.

       000-99-FIM.

       END PROGRAM TSKEY.
