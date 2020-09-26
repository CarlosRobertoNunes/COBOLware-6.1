       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMOUS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/08/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula escape key pelo mouse                 *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 MULTI-USER              PIC  9(001) VALUE 0.
           05 TMP                     PIC  X(003) VALUE SPACES.
           05 TEXTO                   PIC  9(002) COMP-X VALUE 0.
           05 FUNDO                   PIC  9(002) COMP-X VALUE 0.
           05 REVERSO                 PIC  9(002) COMP-X VALUE 0.
           05 HORA-A                  PIC  X(006) VALUE SPACES.
           05 HORA-B                  PIC  X(006) VALUE SPACES.
           05 SEGUNDOS                PIC  9(004) VALUE 0.
           05 SALVO                   PIC  9(001) VALUE 0.
           05 I                       PIC  9(002) COMP-X VALUE 255.
           05 Y                       PIC  9(002) COMP-X VALUE 255.
           05 KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
           05 LX                      PIC  9(004) COMP-X.
           05 CX                      PIC  9(004) COMP-X.
           05 save-LX                 PIC  9(004) COMP-X.
           05 save-CX                 PIC  9(004) COMP-X.
           05 SAVE-STRING             PIC  X(080) VALUE SPACES.
           05 SAVE-STRING-ATTR        PIC  X(080) VALUE SPACES.
           05 SAVE-ATTRIBUTE-BUFFER   PIC  X(080) VALUE SPACES.
           05 START-STRING            PIC  9(002) COMP-X VALUE 0.
           05 FLAG                    PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-POSITION-A        PIC  X(008).
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE     PIC  9(004) COMP-X.
              10 MOUSE-EVENT-TIME     PIC  9(008) COMP-X.
              10 MOUSE-EVENT-ROW      PIC  9(004) COMP-X.
              10 MOUSE-EVENT-COL      PIC  9(004) COMP-X.
           05 MOUSE-READ-TYPE         PIC  9(002) COMP-X VALUE 0.
           05 CBL-READ-WRITE-SCR-ATTRS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION2.
                 15 ROW-NUMBER2       PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER2    PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE-BUFFER     PIC  X(080) VALUE SPACES.
              10 REDEFINES ATTRIBUTE-BUFFER.
                 15 AB OCCURS 80      PIC  9(002) COMP-X.
              10 ATTRIBUTE-BUFFER2    PIC  X(080) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.
              10 STRING-LENGTH2       PIC  9(004) COMP-X VALUE 0.
           05 CARACTER-BUFFER         PIC  X(080) VALUE SPACES.
           05 MOUSE-HANDLE            PIC  X(004) COMP-X VALUE 1.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 CURSOR-POSITION.
              10 CURSOR-LIN            PIC  9(002) COMP-X VALUE 00.
              10 CURSOR-COL            PIC  9(002) COMP-X VALUE 00.

       COPY CWGETL.
       COPY CWUNIX.

       LINKAGE SECTION.

       01  PARAMETROS-CWMOUS.
           05 OCCURS 50.
              10 CWMOUS-POSIT OCCURS 80 PIC 9(002) COMP-X.
           05 CWMOUS-MODE               PIC 9(002) COMP-X.
           05 CWMOUS-KEY                PIC 9(002) COMP-X.
           05 CWMOUS-BUTTON             PIC 9(002) COMP-X.
           05 CWMOUS-CURSOR-POSITION.
              10 CWMOUS-CURSOR-LIN      PIC  9(002).
              10 CWMOUS-CURSOR-COL      PIC  9(002).
           05 CWMOUS-TIMEOUT-STATUS     PIC  X(001).
              88 CWMOUS-TIMEOUT-ENABLE               VALUE "1" X"00".
              88 CWMOUS-TIMEOUT-DISABLE              VALUE "0".
           05 CWMOUS-TIMEOUT-RETURN     PIC  X(001).
              88 CWMOUS-TIMEOUT-ON                   VALUE "1".
              88 CWMOUS-TIMEOUT-OFF                  VALUE "0" X"00".
           05 CWMOUS-LINE               PIC  9(002) COMP-X.
           05 CWMOUS-COLUMN             PIC  9(002) COMP-X.
           05 CWMOUS-LENGTH             PIC  9(002) COMP-X.
           05 CWMOUS-STRING             PIC  X(080) OCCURS 254.
           05 CWMOUS-COLOR              PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING PARAMETROS-CWMOUS.

       000-INICIO.

           ON   1
                DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 2) = "ON"
                     MOVE 1 TO MULTI-USER
                END-IF
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE 1 TO MULTI-USER
                END-IF.
           IF   MULTI-USER = 1
                MOVE 255 TO CWMOUS-KEY
                CALL "CWATCH"
                GOBACK
           END-IF
           IF   CWMOUS-CURSOR-POSITION NOT NUMERIC
                MOVE ZEROS TO CWMOUS-CURSOR-POSITION
           END-IF

           IF   CWMOUS-CURSOR-POSITION NOT = ZEROS
                COMPUTE CURSOR-LIN = CWMOUS-CURSOR-LIN - 1
                COMPUTE CURSOR-COL = CWMOUS-CURSOR-COL - 1
                CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
           END-IF

           CALL "CWGETL" USING PARAMETROS-CWGETL
           IF   CWGETL-MOUSE = 1
                CALL "CBL_INIT_MOUSE"    USING MOUSE-HANDLE
                                               MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                    MOUSE-POSITION-A
                      ON   OVERFLOW
                           CONTINUE
                END-CALL
           ELSE
                MOVE 255 TO CWMOUS-KEY
                IF   CWMOUS-CURSOR-POSITION NOT = ZEROS
                     PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                                           OR CWMOUS-TIMEOUT-ON
                             CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                             PERFORM TEMPO-LIMITE THRU FIM-TEMPO-LIMITE
                     END-PERFORM
                END-IF
                GOBACK
           END-IF

           MOVE 0 TO CWMOUS-KEY
                     FLAG
                     MOUSE-EVENT-TYPE
                     CWMOUS-BUTTON
           SET CWMOUS-TIMEOUT-OFF TO TRUE

           IF  (CWGETL-TIMEOUT NOT = 0)
           AND  CWMOUS-TIMEOUT-ENABLE
                ACCEPT HORA-A FROM TIME
                MOVE 0 TO SEGUNDOS
           END-IF
           PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                               OR (CWMOUS-MODE      = 1 or 3)
                               OR (MOUSE-EVENT-TYPE > 1)
              CALL "CWATCH"
              CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
              IF   CWGETL-MOUSE = 1
                   CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                       MOUSE-POSITION
                         ON   OVERFLOW
                              CONTINUE
                   END-CALL
                   IF  ROW-MOUSE    > 25
                   OR  COLUMN-MOUSE > 80
                       INITIALIZE MOUSE-POSITION
                   END-IF
                   IF    MOUSE-POSITION-A NOT = MOUSE-POSITION
                         MOVE 0 TO SEGUNDOS
                   END-IF
                   MOVE MOUSE-POSITION TO MOUSE-POSITION-A
              END-IF
              IF   CWMOUS-LINE > 0
              AND  CWMOUS-COLUMN > 0
                   PERFORM EXIBE-TEXTO THRU FIM-EXIBE-TEXTO
              END-IF
              IF   CWGETL-MOUSE = 1
                   CALL "CBL_READ_MOUSE_EVENT" USING MOUSE-HANDLE
                                                     MOUSE-DATA
                                                     MOUSE-READ-TYPE
                    ON   OVERFLOW
                         CONTINUE
                    END-CALL
              END-IF
              IF  (KEY-STATUS = 0
              OR  CWMOUS-MODE = 1 or 3)
                 IF CWMOUS-POSIT(ROW-MOUSE + 1 COLUMN-MOUSE + 1) NOT = 0
                 OR CWMOUS-MODE = 1 or 3
                    COMPUTE LX             = ROW-MOUSE    + 1
                    COMPUTE CX             = COLUMN-MOUSE + 1
                    PERFORM SET-REVERSO THRU FIM-SET-REVERSO
                    IF MOUSE-EVENT-TYPE NOT = 0
                    OR CWMOUS-MODE = 1 or 3
                       MOVE CWMOUS-POSIT(ROW-MOUSE + 1 COLUMN-MOUSE + 1)
                         TO CWMOUS-KEY
                       MOVE MOUSE-EVENT-TYPE TO CWMOUS-BUTTON
                       IF   CWMOUS-MODE = 2 OR 3
                            COMPUTE CWMOUS-CURSOR-LIN = ROW-MOUSE    + 1
                            COMPUTE CWMOUS-CURSOR-COL = COLUMN-MOUSE + 1
                       END-IF
                    END-IF
                 ELSE
                    MOVE    0               TO MOUSE-EVENT-TYPE
                    PERFORM RESET-REVERSO THRU FIM-RESET-REVERSO
                 END-IF
              END-IF
              PERFORM TEMPO-LIMITE THRU FIM-TEMPO-LIMITE
           END-PERFORM.

           PERFORM RESET-REVERSO THRU FIM-RESET-REVERSO

           IF   KEY-STATUS = 1
                MOVE 255 TO CWMOUS-KEY
           END-IF

           IF   CWMOUS-LINE > 0
           AND  CWMOUS-COLUMN > 0
           AND  SALVO = 1
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                   SAVE-STRING
                                                   SAVE-STRING-ATTR
                                                   STRING-LENGTH2
                MOVE 0 TO SALVO Y
           END-IF

           IF   CWGETL-MOUSE = 1
                CALL "CBL_TERM_MOUSE"    USING MOUSE-HANDLE
                                               MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF
           EXIT PROGRAM.

       TEMPO-LIMITE.

           IF   CWGETL-TIMEOUT NOT = 0
           AND  CWMOUS-TIMEOUT-ENABLE
                ACCEPT HORA-B FROM TIME
                IF   HORA-B NOT = HORA-A
                     MOVE HORA-B TO HORA-A
                     ADD 1 TO SEGUNDOS
                     IF   SEGUNDOS NOT < CWGETL-TIMEOUT
                          MOVE 1 TO KEY-STATUS
                          SET CWMOUS-TIMEOUT-ON TO TRUE
                     END-IF
                END-IF
           END-IF.

       FIM-TEMPO-LIMITE. EXIT.

       SET-REVERSO.

           IF   FLAG = CWMOUS-POSIT (LX CX)
           OR   CWMOUS-MODE = 1 or 3
           or  (lx = save-lx)
           and (cx = save-cx)
                GO TO FIM-SET-REVERSO
           END-IF

           move lx to save-lx
           move cx to save-cx

           PERFORM RESET-REVERSO THRU FIM-RESET-REVERSO

           MOVE    CWMOUS-POSIT (LX CX) TO FLAG
           MOVE    ROW-MOUSE            TO ROW-NUMBER
           MOVE    COLUMN-MOUSE         TO COLUMN-NUMBER
           MOVE    0                    TO STRING-LENGTH

           PERFORM VARYING CX FROM CX BY -1
                   UNTIL CX < 2
                      OR CWMOUS-POSIT (LX CX) NOT = FLAG
                   CONTINUE
           END-PERFORM

           IF   CWMOUS-POSIT (LX CX) NOT = FLAG
                ADD 1 TO CX
           END-IF

           COMPUTE START-STRING = CX - 1

           PERFORM VARYING CX FROM CX BY 1
                   UNTIL CX > 79
                      OR CWMOUS-POSIT (LX CX) NOT = FLAG
                         ADD 1 TO STRING-LENGTH
           END-PERFORM

           MOVE START-STRING             TO COLUMN-NUMBER

           CALL "CBL_READ_SCR_ATTRS"  USING SCREEN-POSITION
                                            ATTRIBUTE-BUFFER
                                            STRING-LENGTH

           MOVE ATTRIBUTE-BUFFER         TO SAVE-ATTRIBUTE-BUFFER
===>  *    MOVE ALL X"70"                TO ATTRIBUTE-BUFFER
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH
                   COMPUTE FUNDO = AB (I) / 16
                   COMPUTE TEXTO = AB (I) - (FUNDO * 16)
                   COMPUTE REVERSO = FUNDO + (TEXTO * 16)
                   IF   REVERSO = AB (I)
                        MOVE 112 TO AB (I)
                   ELSE
                        MOVE REVERSO TO AB (I)
                   END-IF
           END-PERFORM

           CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                            ATTRIBUTE-BUFFER
                                            STRING-LENGTH.

       FIM-SET-REVERSO. EXIT.

       RESET-REVERSO.

           IF   FLAG = 0
           OR   CWMOUS-MODE = 1 or 3
                GO TO FIM-RESET-REVERSO
           END-IF

           MOVE 0                        TO FLAG
           MOVE SAVE-ATTRIBUTE-BUFFER    TO ATTRIBUTE-BUFFER
           CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                            ATTRIBUTE-BUFFER
                                            STRING-LENGTH.

       FIM-RESET-REVERSO. EXIT.

       EXIBE-TEXTO.

           IF   CWMOUS-POSIT (ROW-MOUSE + 1 COLUMN-MOUSE + 1) =  Y
      *    OR   ROW-MOUSE + 1 = CWMOUS-LINE
                GO TO FIM-EXIBE-TEXTO
           END-IF

           MOVE CWMOUS-POSIT (ROW-MOUSE + 1 COLUMN-MOUSE + 1) TO Y

           IF   Y = 0
                MOVE SPACES            TO CARACTER-BUFFER
           ELSE
                MOVE CWMOUS-STRING (Y) TO CARACTER-BUFFER
txt   *         CALL "CWTEXT" USING CARACTER-BUFFER
txt   *                   LENGTH OF CARACTER-BUFFER
           END-IF

           COMPUTE ROW-NUMBER2    = CWMOUS-LINE   - 1
           COMPUTE COLUMN-NUMBER2 = CWMOUS-COLUMN - 1
           MOVE CWMOUS-LENGTH    TO STRING-LENGTH2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH2
                   MOVE CWMOUS-COLOR (1: 1) TO ATTRIBUTE-BUFFER2 (I: 1)
           END-PERFORM

           IF   SALVO = 0
                CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION2
                                                  SAVE-STRING
                                                  SAVE-STRING-ATTR
                                                  STRING-LENGTH2
                MOVE 1 TO SALVO
           END-IF

           IF   CARACTER-BUFFER = SPACES
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                   SAVE-STRING
                                                   SAVE-STRING-ATTR
                                                   STRING-LENGTH2
                MOVE 0 TO SALVO
           ELSE
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER2
                                                   STRING-LENGTH2
           END-IF.

       FIM-EXIBE-TEXTO. EXIT.

       END PROGRAM CWMOUS.
