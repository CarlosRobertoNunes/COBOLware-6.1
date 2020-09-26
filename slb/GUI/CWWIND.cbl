       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWIND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/09/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de Janelas                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT WINDOW ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD  KEY   IS WINDOW-CHAVE
                  ACCESS MODE   IS RANDOM
                  FILE STATUS   IS FS-WINDOW.

       DATA DIVISION.
       FILE SECTION.

       FD  WINDOW
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINDOW.

       01  WINDOW-REG.
           05 WINDOW-CHAVE      COMP-X PIC  9(0004).
           05 WINDOW-CURPOS            PIC  X(0002).
           05 WINDOW-SCR-pre           PIC  X(0008).
           05 WINDOW-SCR-cur           PIC  X(0008).
           05 WINDOW-NAME              PIC  X(0008).

       WORKING-STORAGE SECTION.

       78  SP2    value "SP2".
       78  CWUSER value "CWUSER".
       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 CORRENTE-ID      COMP-5 PIC S9(004) VALUE 0.
           05 CWUSER-STATUS           PIC  X(001) VALUE '0'.
           05 AJUSTE                  PIC  9(004) VALUE 0.
           05 POP-CHAR                PIC  9(004) VALUE 0.
           05 CWLITS                  PIC  X(003) VALUE SPACES.
           05 CWACCENT                PIC  X(003) VALUE SPACES.
           05 TIT                     PIC  X(080) VALUE SPACES.
           05 LL                      PIC  9(002) VALUE 0.
           05 CC                      PIC  9(002) VALUE 0.
           05 COR              COMP-X PIC  9(002) VALUE 0.
           05 ER-WINDOW.
              10 FS-WINDOW            PIC  X(002) VALUE "00".
              10 LB-WINDOW            PIC  X(255) VALUE 'CWWIND$$$'.
           05 TAMANHO          COMP-X PIC  9(008) VALUE 0.
           05 TAMANHO-RETORNO  COMP-X PIC  9(008) VALUE 0.
           05 DIM.
              10 FILLER               PIC  X(001) VALUE 'W'.
              10 SCR.
                 15 SCR-AT.
                    16 SCR-LIN           PIC  9(002) VALUE 1.
                    16 SCR-COL           PIC  9(002) VALUE 1.
                 15 SCR-SIZE          PIC  9(002) VALUE 80.
                 15 SCR-LINES         PIC  9(002) VALUE 25.
              10 POP-WINDOW    COMP-X PIC  9(004) VALUE 7.

       COPY CWBOXW.
       COPY CWSPWS.

       LINKAGE SECTION.

       COPY CWWIND.

       PROCEDURE DIVISION USING PARAMETROS-CWWIND.

       000-INICIO.

           ON 1
              COPY CWSPPD.
              DISPLAY "CWLITS"        UPON ENVIRONMENT-NAME
              ACCEPT   CWLITS         FROM ENVIRONMENT-VALUE
              INSPECT  CWLITS   CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWACCENT"      UPON ENVIRONMENT-NAME
              ACCEPT   CWACCENT       FROM ENVIRONMENT-VALUE
              INSPECT  CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF  CWWIND-AT(1:1) = '$' OR '?'
               IF  CWWIND-AT(1:1) = '$'
                   MOVE CWWIND-LINE(2:1) TO CWUSER-STATUS
               ELSE
                   MOVE CWUSER-STATUS    TO CWWIND-LINE(2:1)
               END-IF
               GOBACK
           END-IF

           DISPLAY 'CWWIND-ACTIVE' UPON ENVIRONMENT-NAME
           DISPLAY 'ON'            UPON ENVIRONMENT-VALUE
           EVALUATE TRUE
               WHEN CWWIND-OPEN
                 OR CWWIND-BOX
                    PERFORM OPEN-WINDOW  thru FIM-OPEN-WINDOW
               WHEN CWWIND-CLOSE
                    PERFORM CLOSE-WINDOW thru FIM-CLOSE-WINDOW
               WHEN CWWIND-LIN
                    PERFORM DRAW-LINES   thru FIM-DRAW-LINES
           END-EVALUATE
           DISPLAY 'CWWIND-ACTIVE' UPON ENVIRONMENT-NAME
           DISPLAY 'OFF'           UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       OPEN-WINDOW.

           IF CWWIND-OPEN
              IF POP-WINDOW = 7
                 OPEN I-O WINDOW
              END-IF
              ADD 2 TO POP-WINDOW
              MOVE POP-WINDOW (1: 2)         TO WINDOW-CHAVE(1:2)
                                                CWWIND-REFERENCE
              MOVE  SCR                      TO WINDOW-SCR-pre
              CALL "CBL_GET_CSR_POS"      USING WINDOW-CURPOS
              MOVE  CWWIND-LINE              TO SCR-LIN
              MOVE  CWWIND-COLUMN            TO SCR-COL
              MOVE  CWWIND-HORIZONTAL-LENGTH TO SCR-SIZE
              MOVE  CWWIND-VERTICAL-LENGTH   TO SCR-LINES
              MOVE  SCR                      TO WINDOW-SCR-cur
           END-IF

           IF  CWWIND-BOX
               MOVE 'NOWIN'               TO CWBOXW-FUNCTION
           ELSE
               MOVE 'BOX'                 TO CWBOXW-FUNCTION
               IF  CWWIND-ERASE
                   MOVE 'ERASE'           TO CWBOXW-FUNCTION
               END-IF
           END-IF

           MOVE 0                         TO CWBOXW-COLOR-SHADE
           IF CWWIND-SHADOW
              MOVE 8                      TO CWBOXW-COLOR-SHADE
           END-IF
           MOVE CWWIND-LINE               TO CWBOXW-LINE
           MOVE CWWIND-COLUMN             TO CWBOXW-COLUMN
           MOVE CWWIND-TYPE               TO CWBOXW-TYPE
           MOVE CWWIND-VERTICAL-LENGTH    TO CWBOXW-VERTICAL-LENGTH
           MOVE CWWIND-HORIZONTAL-LENGTH  TO CWBOXW-HORIZONTAL-LENGTH
           IF  CWWIND-BOXED OR CWWIND-BOX
               SUBTRACT 1 FROM CWBOXW-LINE
                               CWBOXW-COLUMN
               IF NOT CWWIND-OPEN
                  ADD 1 TO SCR-LINES SCR-SIZE
               END-IF
           ELSE
               MOVE 9 TO CWBOXW-TYPE
           END-IF
           IF  CWWIND-BACKGROUND-COLOR   = 9
           AND CWWIND-FOREGROUND-COLOR   = 9
           AND CWWIND-COLOR              = 255
               MOVE CWWIND-COLOR-FRAME  to CWBOXW-COLOR-FRAME
               MOVE CWWIND-COLOR-BORDER to CWBOXW-COLOR-BORDER
               MOVE CWWIND-COLOR-SHADE  to CWBOXW-COLOR-SHADE
           ELSE
               IF (CWWIND-BACKGROUND-COLOR NOT = 9)
               OR (CWWIND-FOREGROUND-COLOR NOT = 9)
                  MOVE 0 TO COR
                  IF CWWIND-BACKGROUND-COLOR < 9
                     COMPUTE COR = CWWIND-BACKGROUND-COLOR * 16
                  END-IF
                  IF CWWIND-FOREGROUND-COLOR < 9
                     ADD CWWIND-FOREGROUND-COLOR TO COR
                  END-IF
                  MOVE COR TO CWBOXW-COLOR-FRAME
                              CWBOXW-COLOR-BORDER
               ELSE
                   IF  CWWIND-COLOR = 255
                       MOVE 7 TO CWBOXW-COLOR-FRAME
                                 CWBOXW-COLOR-BORDER
                   ELSE
                       MOVE CWWIND-COLOR TO CWBOXW-COLOR-FRAME
                                            CWBOXW-COLOR-BORDER
                   END-IF
               END-IF
               IF CWWIND-REVERSED
                  COMPUTE CWBOXW-COLOR-FRAME = CWBOXW-COLOR-FRAME * 16
                  MOVE CWBOXW-COLOR-FRAME TO CWBOXW-COLOR-BORDER
               END-IF
               IF CWWIND-HIGHLIGHT
                  ADD 8  TO CWBOXW-COLOR-FRAME
                            CWBOXW-COLOR-BORDER
               END-IF
               IF CWWIND-BLINK
                  ADD 128 TO CWBOXW-COLOR-FRAME
                             CWBOXW-COLOR-BORDER
               END-IF
           END-IF

           IF CWWIND-OPEN
              IF  CWUSER-STATUS = '0'
                  CALL  CWUSER  USING DIM
              END-IF
              CALL "CWBOXW" USING DIM
              CALL "CWOBJE" USING DIM
              MOVE LOW-VALUES      TO SP2-WD-DATA
              CALL SP2 USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
              IF CORRENTE-ID = 0
                 MOVE SP2-WD-WINDOW-ID TO CORRENTE-ID
              END-IF
              MOVE SP2-WD-NAME     TO WINDOW-NAME
              WRITE WINDOW-REG
              MOVE LOW-VALUES      TO SP2-WD-DATA
                                      SP2-PD-DATA
              MOVE 'CWMENU'        TO SP2-ND-NAME
              CALL SP2          USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
              MOVE SP2-ND-NAME     TO SP2-WD-NAME
                                      SP2-WD-PANEL-NAME SP2-PD-NAME
              CALL SP2 USING SP2-GET-WINDOW-DEF  SP2-WINDOW-DEF
              CALL SP2 USING SP2-GET-PANEL-DEF   SP2-PANEL-DEF
              MOVE SPACES          TO SP2-ND-NAME
              MOVE POP-WINDOW      TO POP-CHAR
              STRING 'CW' POP-CHAR DELIMITED BY SIZE INTO SP2-ND-NAME
              MOVE SP2-ND-NAME     TO SP2-WD-NAME
                                      SP2-WD-PANEL-NAME SP2-PD-NAME
pops          MOVE X'02'           TO SP2-WD-OPTIONS-3
pops          MOVE X'24'           TO SP2-WD-SYSTEM-MENU
pops          MOVE X'10'           TO SP2-WD-SYSTEM-MENU
pops          MOVE "d"             TO SP2-WD-BOR-TYPE
280316*       COMPUTE SP2-WD-ROW = (SCR-LIN - 2) * 10
280316        COMPUTE SP2-WD-ROW = (SCR-LIN - 2) * 5
              COMPUTE SP2-WD-COL = SCR-COL * 5
              MOVE SCR-SIZE        TO SP2-WD-WIDTH
frango*       COMPUTE AJUSTE = SCR-SIZE / 5,2
frango*       IF SCR-SIZE (2:1) = '0' OR '2' OR '4' OR '6' OR '8'
frango*          SUBTRACT 1 FROM AJUSTE
frango*       ELSE
frango*          ADD 1 TO AJUSTE
frango*       END-IF
frango*       SUBTRACT AJUSTE FROM SP2-WD-WIDTH
frango        move 0 to ajuste
              COMPUTE SP2-WD-COL = SCR-COL * 5 + (AJUSTE * 2,5)
              MOVE SP2-WD-WIDTH TO SP2-PD-WIDTH
                                   SP2-WD-TOT-WIDTH
                                   SP2-PD-TOT-WIDTH
              MOVE SCR-LINES    TO SP2-WD-HEIGHT      SP2-PD-HEIGHT
                                   SP2-WD-TOT-HEIGHT  SP2-PD-TOT-HEIGHT
              MOVE CWWIND-TITLE TO SP2-WD-TITLE
              IF   CWLITS = "LOW"
                   INSPECT SP2-WD-TITLE
                           CONVERTING MAIUSCULAS TO MINUSCULAS
              END-IF
              IF   CWLITS = "UPP"
                   INSPECT SP2-WD-TITLE
                           CONVERTING MINUSCULAS TO MAIUSCULAS
              END-IF
              IF   CWACCENT = "OFF"
                   INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                                TO ACENTOS-OFF
              ELSE
                   INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                                TO ACENTOS-WINDOWS
              END-IF
              MOVE CORRENTE-ID      TO SP2-WD-OWNR-ID
      *       MOVE 0                TO SP2-WD-OWNR-ID
*************
              MOVE X"10"            TO SP2-WD-MORE-OPTIONS
              MOVE  "m"             TO SP2-WD-bor-type
              MOVE  "o"             TO SP2-WD-bor-type
*************
              CALL SP2 USING SP2-OPEN-WINDOW     SP2-WINDOW-DEF
              CALL SP2 USING SP2-RESERVE-MEMORY  SP2-NULL-PARM
              CALL SP2 USING SP2-SET-PANEL-DEF   SP2-PANEL-DEF
              CALL SP2 USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
              CALL SP2 USING SP2-DISPLAY-WINDOW  SP2-NULL-PARM
              CALL "CBL_SET_CSR_POS"  USING X"0000"
           ELSE
              CALL "CWBOXW" USING PARAMETROS-CWBOXW
              IF (CWWIND-BOXED OR CWWIND-BOX)
              AND(CWWIND-TITLE NOT = SPACES)
                 MOVE CWBOXW-LINE   TO LL
                 COMPUTE TAMANHO = CWWIND-HORIZONTAL-LENGTH - 4
                 CALL "CWPACK" USING CWWIND-TITLE
                                     TAMANHO
                                     TAMANHO-RETORNO
                 MOVE SPACES TO TIT
                 STRING ' ' CWWIND-TITLE(1:TAMANHO-RETORNO) ' '
                        DELIMITED BY SIZE
                   INTO TIT
                 COMPUTE TAMANHO = TAMANHO-RETORNO + 2
                 EVALUATE TRUE
                     WHEN CWWIND-CENTERED
                          COMPUTE CC = (CWBOXW-COLUMN
                                     + (CWBOXW-HORIZONTAL-LENGTH) / 2)
                                     - (TAMANHO / 2) + 1
                     WHEN CWWIND-LEFT
                          COMPUTE CC = CWBOXW-COLUMN
                                     + CWBOXW-HORIZONTAL-LENGTH
                                     - TAMANHO
                     WHEN OTHER
                          COMPUTE CC = CWBOXW-COLUMN + 3
                 END-EVALUATE
                 IF CWWIND-BOTTOM
                    ADD CWBOXW-VERTICAL-LENGTH TO LL
                    ADD 1 TO LL
                 END-IF
                 DISPLAY TIT AT LINE LL COLUMN CC WITH SIZE TAMANHO
              END-IF
           END-IF.

       FIM-OPEN-WINDOW. EXIT.

       CLOSE-WINDOW.

           MOVE CWWIND-REFERENCE TO WINDOW-CHAVE(1:2)
           READ WINDOW

           IF FS-WINDOW = '00'
              MOVE SPACES          TO SP2-ND-NAME
              MOVE WINDOW-CHAVE    TO POP-CHAR
              STRING 'CW' POP-CHAR DELIMITED BY SIZE INTO SP2-ND-NAME
              CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
              CALL CWUSER USING "%"
Wilson        DISPLAY (1, 1) ERASE
              MOVE WINDOW-SCR-cur TO SCR
              CALL  CWUSER  USING DIM
              EXEC COBOLware Object Drop END-EXEC
              CALL SP2   USING SP2-CLOSE-WINDOW    SP2-WINDOW-DEF
              MOVE WINDOW-NAME TO SP2-ND-NAME
              CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
Frango        CALL SP2   USING SP2-DISPLAY-WINDOW  SP2-NULL-PARM
              MOVE WINDOW-SCR-pre TO SCR
              SUBTRACT 2 FROM POP-WINDOW
              CALL  CWUSER  USING DIM
              CALL "CWBOXW" USING DIM
              CALL "CWOBJE" USING DIM
              CALL "CBL_SET_CSR_POS"  USING WINDOW-CURPOS
              ADD      2   TO POP-WINDOW
              DELETE WINDOW RECORD
           END-IF

           IF CWWIND-REFERENCE(1:2) = POP-WINDOW (1:2)
              SUBTRACT 2 FROM POP-WINDOW
              IF POP-WINDOW = 7
                 CLOSE WINDOW
              END-IF
           END-IF.

       FIM-CLOSE-WINDOW. EXIT.

       DRAW-LINES.

       FIM-DRAW-LINES. EXIT.
       END PROGRAM CWWIND.
