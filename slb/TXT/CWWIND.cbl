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
           05 WINDOW-CHAVE             PIC  X(0002).
           05 WINDOW-DATA              PIC  X(2000).
           05 WINDOW-ATTRIB            PIC  X(2000).
           05 WINDOW-SCR               PIC  X(0008).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 CWUSER-STATUS           PIC  X(001) VALUE '0'.
           05 CBLS.
              10 CBL-READ-SCR-CHATTRS   PIC  X(020) VALUE
                 "CBL-READ-SCR-CHATTRS".
              10 CBL-WRITE-SCR-CHATTRS  PIC  X(021) VALUE
                 "CBL-WRITE-SCR-CHATTRS".
           05 CURPOS.
              10 CURPOS-ROW    COMP-X PIC  9(002).
              10 CURPOS-COL    COMP-X PIC  9(002).
           05 STRING-LENGTH           PIC  9(004) COMP-X VALUE 0.
           05 TIT                     PIC  X(080) VALUE SPACES.
           05 LL                      PIC  9(002) VALUE 0.
           05 CC                      PIC  9(002) VALUE 0.
           05 COR              COMP-X PIC  9(002) VALUE 0.
           05 ER-WINDOW.
              10 FS-WINDOW            PIC  X(002) VALUE "00".
              10 LB-WINDOW            PIC  X(255) VALUE 'CWWIND$$$'.
           05 TAMANHO          COMP-X PIC  9(008) VALUE 0.
           05 TAMANHO-RETORNO  COMP-X PIC  9(008) VALUE 0.
           05 WINPOS.
              10 WINPOS-LIN           PIC  9(002) VALUE 0.
              10 WINPOS-COL           PIC  9(002) VALUE 0.
           05 DIM.
              10 FILLER               PIC  X(001) VALUE 'W'.
              10 SCR.
                 15 SCR-LIN           PIC  9(002) VALUE 1.
                 15 SCR-COL           PIC  9(002) VALUE 1.
                 15 SCR-SIZE          PIC  9(002) VALUE 80.
                 15 SCR-LINES         PIC  9(002) VALUE 25.
              10 POP-WINDOW    COMP-X PIC  9(004) VALUE 7.

       COPY CWBOXW.

       LINKAGE SECTION.

       COPY CWWIND.

       PROCEDURE DIVISION USING PARAMETROS-CWWIND.

       000-INICIO.

           ON 1
              INSPECT CBLS CONVERTING '-' TO '_'.

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
              MOVE POP-WINDOW (1: 2)       TO WINDOW-CHAVE
                                              CWWIND-REFERENCE
              CALL CBL-READ-SCR-CHATTRS USING X"0000"
                                              WINDOW-DATA
                                              WINDOW-ATTRIB
                                              X"07D0"
              MOVE SCR                      TO WINDOW-SCR
              WRITE WINDOW-REG
              MOVE CWWIND-LINE              TO SCR-LIN
              MOVE CWWIND-COLUMN            TO SCR-COL
              MOVE CWWIND-HORIZONTAL-LENGTH TO SCR-SIZE
              MOVE CWWIND-VERTICAL-LENGTH   TO SCR-LINES
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
               ADD 1 TO SCR-LINES SCR-SIZE
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
              IF LL = 0
                 MOVE 1 TO LL
              END-IF
              COMPUTE CURPOS-ROW = LL - 1
              COMPUTE CURPOS-COL = CC - 1
              MOVE TAMANHO TO STRING-LENGTH
              CALL "CBL_WRITE_SCR_CHARS" USING CURPOS
                                               TIT
                                               STRING-LENGTH
           END-IF

           IF CWWIND-OPEN
              MOVE SCR-LIN TO WINPOS-LIN
              MOVE SCR-COL TO WINPOS-COL
      *       IF   NOT WINPOS NOT = '0000'
      *            SUBTRACT 1 FROM WINPOS-LIN
      *                            WINPOS-COL
      *       END-IF
              DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
              DISPLAY WINPOS UPON ENVIRONMENT-VALUE
              CALL "CWUSER" USING DIM
              CALL "CWBOXW" USING DIM
              CALL "CWOBJE" USING DIM
           END-IF.

       FIM-OPEN-WINDOW. EXIT.

       CLOSE-WINDOW.

           MOVE CWWIND-REFERENCE TO WINDOW-CHAVE
           READ WINDOW

           IF FS-WINDOW = '00'
              CALL CBL-WRITE-SCR-CHATTRS USING X"0000"
                                               WINDOW-DATA
                                               WINDOW-ATTRIB
                                               X"07D0"
              MOVE WINDOW-SCR TO SCR
              MOVE SCR-LIN TO WINPOS-LIN
              MOVE SCR-COL TO WINPOS-COL
      *       IF   NOT WINPOS NOT = '0000'
      *            SUBTRACT 1 FROM WINPOS-LIN
      *                            WINPOS-COL
      *       END-IF
              DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
              DISPLAY WINPOS UPON ENVIRONMENT-VALUE
              CALL "CWUSER" USING X"00"
              SUBTRACT 2 FROM POP-WINDOW
              CALL "CWBOXW" USING DIM
              CALL "CWUSER" USING DIM
              CALL "CWOBJE" USING DIM
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
