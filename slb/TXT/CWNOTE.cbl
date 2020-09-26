       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWNOTE INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/11/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Bloco de notas                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT WORKAREA ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-WORKAREA.

       DATA DIVISION.
       FILE SECTION.

       FD  WORKAREA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WORKAREA.

       01  WORKAREA-REG         PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 JANPOS.
              10 JANLIN               PIC  9(002) VALUE 0.
              10 JANCOL               PIC  9(002) VALUE 0.
           05 ERRO                    PIC  9(001) VALUE 0.
           05 LD                      PIC  9(002) VALUE 0.
           05 TEXTO                   PIC  X(080) VALUE SPACES.
           05 M                COMP-5 PIC S9(004) VALUE 0.
           05 B                COMP-5 PIC S9(004) VALUE 0.
           05 I                COMP-5 PIC S9(004) VALUE 0.
           05 L-SAVE           COMP-5 PIC S9(004) VALUE 0.
           05 C-SAVE           COMP-5 PIC S9(004) VALUE 0.
           05 MARGIN-SAVE      COMP-5 PIC S9(004) VALUE 0.
           05 LINHAS           COMP-5 PIC S9(004) VALUE 0.
           05 ER-WORKAREA.
              10 FS-WORKAREA          PIC  X(002) VALUE "00".
              10 LB-WORKAREA          PIC  X(255) VALUE "HELP".
           05 TECLA-X          COMP-X PIC  9(002).
           05 TELA.
              10 L             COMP-5 PIC S9(004) VALUE 0.
              10 C             COMP-5 PIC S9(004) VALUE 0.
              10 LM            COMP-5 PIC S9(004) VALUE 0.
              10 CM            COMP-5 PIC S9(004) VALUE 0.
              10 COLUNA        COMP-5 PIC S9(004) VALUE 0.
              10 TELA-LIN OCCURS 25   PIC  X(80).
              10          OCCURS 25.
                 15 MOUSE OCCURS 80   PIC S9(004) COMP-5.
           05 TELA-SAVE   OCCURS 25   PIC  X(80).
           05 MEMORIA                 PIC  X(32756).
           05 MEMORIA-PRE      COMP-5 PIC S9(004) VALUE 1.
           05 MEMORIA-CURSOR   COMP-5 PIC S9(004) VALUE 1.
           05 SAVE-CURSOR      COMP-5 PIC S9(004) VALUE 1.
           05 SAVE-POINTER     COMP-5 PIC S9(004) VALUE 1.
           05 MEMORIA-POINTER  COMP-5 PIC S9(004) VALUE 1.
           05 MEMORIA-MARGIN   COMP-5 PIC S9(004) VALUE 0.
           05 CURPOS.
              10 CURPOS-LIN            PIC  9(002) VALUE ZERO.
              10 CURPOS-COL            PIC  9(002) VALUE ZERO.
           05 COL-FIM                  PIC  9(002) VALUE ZERO.
           05 LIN-FIM                  PIC  9(002) VALUE ZERO.
           05 CARACTER                 PIC  X(001).
           05 TECLA-EDIT               PIC  9(003) VALUE 0. COPY CWEDIT.
           05 REPINS                   PIC  X(001) VALUE "?".
              88 INSERT-OFF                        VALUE "0".
              88 INSERT-ON                         VALUE "1".
           05 WORK                    PIC  X(32756).
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-HANDLE             PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS            PIC  9(002) COMP-X VALUE 3.
           05 CURSOR-POSITION.
              10 ROW-CURSOR            PIC  9(002) COMP-X.
              10 COLUMN-CURSOR         PIC  9(002) COMP-X.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME      PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW       PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL       PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE          PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-POSITION.
              10 ROW-MOUSE             PIC  9(004) COMP-X.
              10 COLUMN-MOUSE          PIC  9(004) COMP-X.
           05 SCREEN-POSITION.
              10 SCREEN-ROW              PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-COLUMN           PIC  9(002) COMP-X VALUE 0.
           05 STRING-LENGTH              PIC  9(004) COMP-X VALUE 0.

       COPY CWGETL.
       COPY CWSEND.
       COPY CWUNIX.

       LINKAGE SECTION.

       COPY CWNOTE.

       PROCEDURE DIVISION USING PARAMETROS-CWNOTE.

       000-INICIO.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF   CWNOTE-OLDFILE = SPACES
                MOVE CWNOTE-FILE    TO LB-WORKAREA
           ELSE
                MOVE CWNOTE-OLDFILE TO LB-WORKAREA
           END-IF
           MOVE SPACE       TO CWNOTE-ARROW
           MOVE SPACES      TO MEMORIA
           MOVE 0           TO LINHAS
           OPEN INPUT WORKAREA
           INITIALIZE TELA
           MOVE 1 TO L C
           IF LB-WORKAREA NOT = SPACES
              PERFORM UNTIL FS-WORKAREA > "09"
                         OR B = LENGTH OF MEMORIA
                      READ WORKAREA
                      IF  FS-WORKAREA < "10"
                      AND (WORKAREA-REG NOT = X"1A")
                          ADD 1 TO B
                          MOVE WORKAREA-REG TO MEMORIA (B: 1)
                      END-IF
                      IF  FS-WORKAREA = "10"
                      AND (WORKAREA-REG NOT = X"0A")
                      AND B < LENGTH OF MEMORIA
                          ADD 1 TO B
                          MOVE X"0D0A" TO MEMORIA (B: 1)
                      END-IF
              END-PERFORM
              IF  B = LENGTH OF MEMORIA
                  READ WORKAREA
                  IF  FS-WORKAREA < "10"
                      MOVE "Tamanho do texto excede a capacidade"
                           TO  CWSEND-MSG
                      CALL "CWSEND" USING PARAMETROS-CWSEND
                      CLOSE WORKAREA
                      GOBACK
                  END-IF
              END-IF
              CLOSE WORKAREA
           ELSE
              PERFORM VARYING L FROM 1 BY 1 UNTIL L > 25
                      PERFORM VARYING C FROM 80 BY -1
                              UNTIL C = 0
                                 OR CWNOTE-MESSAGE(L) (C:1) NOT = SPACE
                              CONTINUE
                      END-PERFORM
                      MOVE 0 TO M
                      PERFORM C TIMES
                          ADD 1 TO B M
                          MOVE CWNOTE-MESSAGE(L) (M:1)
                            TO MEMORIA  (B: 1)
                      END-PERFORM
                      ADD 1 TO B MOVE X'0D' TO MEMORIA(B: 1)
                      ADD 1 TO B MOVE X'0A' TO MEMORIA(B: 1)
                      MOVE CWNOTE-MESSAGE (L) TO TELA-LIN (L)
                      IF   CWNOTE-MESSAGE (L) NOT = SPACES
                           MOVE L TO LINHAS
                      END-IF
              END-PERFORM
           END-IF

           IF   CWNOTE-VIEW
                PERFORM 200-DISPLAY THRU 200-99-FIM
.               GOBACK
           END-IF

frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango     add JANLIN to CWNOTE-LINE
frango     add JANCOL to CWNOTE-COLUMN
frango     DISPLAY '0000'        UPON ENVIRONMENT-VALUE

           IF   CWNOTE-TITLE NOT = SPACES
                COMPUTE L = CWNOTE-LINE - 1
                COMPUTE C = CWNOTE-COLUMN - 1
                IF CWNOTE-VIEW-WINDOW
                   EXEC COBOLware BoxW OPEN
                        LINE              L
                        COLUMN            C
                        VERTICAL-LENGTH   CWNOTE-VERTICAL-LENGTH
                        HORIZONTAL-LENGTH CWNOTE-HORIZONTAL-LENGTH
                        COLOR-FRAME  CWNOTE-COLOR-FRAME
                        COLOR-BORDER CWNOTE-COLOR-BORDER
                        COLOR-SHADE  CWNOTE-COLOR-SHADE
                   END-EXEC
                ELSE
                   EXEC COBOLware BoxW OPEN
                        LINE              L
                        COLUMN            C
                        VERTICAL-LENGTH   CWNOTE-VERTICAL-LENGTH
                        HORIZONTAL-LENGTH CWNOTE-HORIZONTAL-LENGTH
                        COLOR-BORDER 78
                   END-EXEC
                END-IF
                ADD 2 TO C
                PERFORM VARYING LD FROM LENGTH OF CWNOTE-TITLE
                            BY -1 UNTIL LD = 1
                            OR CWNOTE-TITLE (LD: 1) NOT = SPACE
                            CONTINUE
                END-PERFORM
                ADD 2 TO LD
                MOVE CWNOTE-TITLE TO TEXTO (2: )
                DISPLAY TEXTO LINE L COLUMN C WITH SIZE LD HIGHLIGHT
           END-IF

           COMPUTE COL-FIM = CWNOTE-COLUMN
                           + CWNOTE-HORIZONTAL-LENGTH - 1
           COMPUTE LIN-FIM = CWNOTE-LINE
                           + CWNOTE-VERTICAL-LENGTH   - 1
           PERFORM 200-DISPLAY THRU 200-99-FIM

           CALL "CWGETL" USING PARAMETROS-CWGETL
           IF   CWGETL-MOUSE = 1
                MOVE 5 TO CWGETL-MOUSE
                CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF

           PERFORM UNTIL EDIT-ESC
              IF   ERRO NOT = 3
                   MOVE 0 TO ERRO
                   COMPUTE ROW-CURSOR    = CURPOS-LIN - 1
                   COMPUTE COLUMN-CURSOR = CURPOS-COL - 1
                   CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                   MOVE 0 TO KEY-STATUS
                   PERFORM UNTIL KEY-STATUS = 1
                              OR CWUNIX-ON
                              OR CWGETL-MOUSE = 0
                           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                           IF  KEY-STATUS = 0
                               CALL "CBL_READ_MOUSE_EVENT" USING
                                                           MOUSE-HANDLE
                                                           MOUSE-DATA
                                                        MOUSE-READ-TYPE
                                    ON   OVERFLOW
                                         CONTINUE
                               END-CALL
                               IF   MOUSE-EVENT-TYPE = 2 OR 3
                                    CALL "CBL_GET_MOUSE_POSITION"
                                          USING MOUSE-HANDLE
                                                MOUSE-POSITION
                                      ON   OVERFLOW
                                           CONTINUE
                                    END-CALL
                                    COMPUTE L = ROW-MOUSE    + 1
                                    COMPUTE C = COLUMN-MOUSE + 1
                                    MOVE MOUSE (L C) TO M
                                    IF  MOUSE (L C) = 0
                                        PERFORM VARYING C FROM C BY -1
                                                  UNTIL C = 1
                                                OR (MOUSE (L C) NOT = 0)
                                                CONTINUE
                                        END-PERFORM
                                    END-IF
                                    IF  MOUSE (L C) NOT = 0
                                        IF MOUSE (L C) = MEMORIA-CURSOR
                                           MOVE 2 TO ERRO
                                           SET EDIT-ENTER TO TRUE
                                           MOVE 1 TO KEY-STATUS
                                        END-IF
                                        MOVE MOUSE (L C)
                                          TO MEMORIA-CURSOR
                                        MOVE L TO CURPOS-LIN
                                        MOVE C TO CURPOS-COL
                                        COMPUTE ROW-CURSOR    = L - 1
                                        COMPUTE COLUMN-CURSOR = C - 1
                                        CALL "CBL_SET_CSR_POS"
                                             USING CURSOR-POSITION
                                    END-IF
                               END-IF
                           END-IF
                   END-PERFORM
                   IF  ERRO = 0
                       CALL "CWKBDC" USING CURPOS CARACTER TECLA-EDIT
                                       "000" "A" REPINS
                   END-IF
                   IF  ERRO = 2
                       MOVE 0 TO ERRO
                   END-IF
              END-IF
              COMPUTE MEMORIA-PRE = MEMORIA-CURSOR - 1
              EVALUATE TRUE
                  WHEN CWNOTE-VIEW-WINDOW
                       SET EDIT-ESC TO TRUE
                  WHEN EDIT-ENTER
                    OR EDIT-CONTROL-V
                       COMPUTE I = LENGTH OF MEMORIA - 1
                       IF  MEMORIA (I: 2) NOT = SPACES
                           MOVE 1 TO ERRO
                       ELSE
                           MOVE 0 TO MEMORIA-MARGIN
                           MOVE MEMORIA (MEMORIA-CURSOR: ) TO WORK
                           MOVE X"0D0A" TO MEMORIA (MEMORIA-CURSOR: 2)
                           ADD 2 TO MEMORIA-CURSOR
                           MOVE WORK TO MEMORIA (MEMORIA-CURSOR: )
                           IF   CURPOS-LIN = LIN-FIM
                                PERFORM VARYING MEMORIA-POINTER
                                           FROM MEMORIA-POINTER BY 1
                                          UNTIL MEMORIA-POINTER
                                                = LENGTH OF MEMORIA
                                        OR MEMORIA (MEMORIA-POINTER: 1)
                                          = X"0A"
                                          CONTINUE
                                END-PERFORM
                                IF   MEMORIA-POINTER < LENGTH OF MEMORIA
                                     ADD 1 TO MEMORIA-POINTER
                                END-IF
                           END-IF
                       END-IF
                  WHEN EDIT-TAB
                       MOVE ">" TO CWNOTE-ARROW
                       SET EDIT-ESC TO TRUE
                  WHEN EDIT-SHIFT-TAB
                       MOVE "<" TO CWNOTE-ARROW
                       SET EDIT-ESC TO TRUE
                  WHEN EDIT-ALT-Y
                       MOVE "=" TO CWNOTE-ARROW
                       SET EDIT-ESC TO TRUE
                  WHEN TECLA-EDIT = 0
                       IF   INSERT-ON
                       OR   MEMORIA (MEMORIA-CURSOR: 1) = X"0D"
                            IF (MEMORIA (LENGTH OF MEMORIA:1) NOT = " ")
                            OR  MEMORIA-CURSOR = LENGTH OF MEMORIA
                                MOVE 1 TO ERRO
                            ELSE
                                MOVE MEMORIA (MEMORIA-CURSOR:) TO WORK
                                MOVE CARACTER TO MEMORIA
                                                 (MEMORIA-CURSOR: 1)
                                ADD 1 TO MEMORIA-CURSOR
                                IF  CURPOS-COL = COL-FIM
                                    ADD 1 TO MEMORIA-MARGIN
                                END-IF
                                MOVE WORK TO MEMORIA (MEMORIA-CURSOR:)
                            END-IF
                       ELSE
                            MOVE CARACTER TO MEMORIA (MEMORIA-CURSOR: 1)
                            IF   MEMORIA-CURSOR = LENGTH OF MEMORIA
                                 MOVE 1 TO ERRO
                            ELSE
                                ADD 1 TO MEMORIA-CURSOR
                                IF   CURPOS-COL < COL-FIM
                                     ADD 1 TO CURPOS-COL
                                ELSE
                                     ADD 1 TO MEMORIA-MARGIN
                                END-IF
                            END-IF
                       END-IF
                  WHEN EDIT-CURSOR-RIGHT
                       IF  MEMORIA-CURSOR = LENGTH OF MEMORIA
                       OR  MEMORIA (MEMORIA-CURSOR: ) = SPACES
                       OR  MEMORIA (MEMORIA-CURSOR: ) = X"0A"
                           IF   ERRO = 3
                                MOVE 0 TO ERRO
                           ELSE
                                MOVE 1 TO ERRO
                           END-IF
                       ELSE
                           ADD 1 TO MEMORIA-CURSOR
                           IF  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                               ADD  1             TO MEMORIA-CURSOR
                               MOVE 0             TO MEMORIA-MARGIN
                                                     ERRO
                               IF   CURPOS-LIN NOT < LIN-FIM
                                    PERFORM UNTIL MEMORIA-POINTER
                                                  = LENGTH OF MEMORIA
                                         OR MEMORIA
                                            (MEMORIA-POINTER: 1) = X"0A"
                                            ADD 1 TO MEMORIA-POINTER
                                    END-PERFORM
                                    ADD 1 TO MEMORIA-POINTER
                               END-IF
                           ELSE
                               IF  CURPOS-COL NOT < COL-FIM
                                   ADD 1 TO MEMORIA-MARGIN
                               END-IF
                           END-IF
                           PERFORM 110-WORD THRU 110-99-FIM
                       END-IF
                  WHEN EDIT-CURSOR-LEFT
                    OR EDIT-BACKSPACE
                       EVALUATE TRUE
                           WHEN CURPOS-COL > CWNOTE-COLUMN
                                SUBTRACT 1 FROM MEMORIA-CURSOR
                                PERFORM 110-WORD THRU 110-99-FIM
                           WHEN MEMORIA-MARGIN > 0
                                SUBTRACT 1 FROM MEMORIA-MARGIN
                                IF   MEMORIA-CURSOR > 1
                                     SUBTRACT 1 FROM MEMORIA-CURSOR
                                END-IF
                                PERFORM 110-WORD THRU 110-99-FIM
                           WHEN MEMORIA-CURSOR > 1
                                SUBTRACT 1 FROM MEMORIA-CURSOR
                                IF   MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                                     SUBTRACT 1 FROM MEMORIA-CURSOR
                                     PERFORM VARYING I
                                                FROM MEMORIA-CURSOR
                                                  BY -1
                                                  UNTIL I < 2
                                               OR MEMORIA (I: 1) = X"0A"
                                             CONTINUE
                                     END-PERFORM
                                     IF  MEMORIA (I: 1) <> X"0A"
                                     AND MEMORIA-POINTER > 1
                                         SUBTRACT 1 FROM MEMORIA-POINTER
                                     END-IF
                                     MOVE 0 TO MEMORIA-MARGIN
                                     ADD  2 TO I
                                     MOVE CWNOTE-COLUMN TO CURPOS-COL
                                     PERFORM VARYING I FROM I BY 1
                                               UNTIL I > MEMORIA-CURSOR
                                             IF  CURPOS-COL < COL-FIM
                                                 ADD 1 TO CURPOS-COL
                                             ELSE
                                                 ADD 1 TO MEMORIA-MARGIN
                                             END-IF
                                     END-PERFORM
                                END-IF
                                IF   CURPOS-LIN = CWNOTE-LINE
                                     MOVE MEMORIA-CURSOR
                                       TO MEMORIA-POINTER
                                     PERFORM UNTIL MEMORIA-POINTER = 1
                                                OR MEMORIA
                                         (MEMORIA-POINTER: 1) = X"0A"
                                        SUBTRACT 1 FROM MEMORIA-POINTER
                                     END-PERFORM
                                     IF   MEMORIA-POINTER NOT = 1
                                          ADD 1 TO MEMORIA-POINTER
                                     END-IF
                                END-IF
                                PERFORM 110-WORD THRU 110-99-FIM
                           WHEN OTHER
                                IF   ERRO = 3
                                     MOVE 0 TO ERRO
                                ELSE
                                     MOVE 1 TO ERRO
                                END-IF
                       END-EVALUATE
                       IF  EDIT-BACKSPACE
                       AND ERRO = 0
                           IF   MEMORIA (MEMORIA-CURSOR: 1) = X"0D"
                                COMPUTE I = MEMORIA-CURSOR + 2
                           ELSE
                                COMPUTE I = MEMORIA-CURSOR + 1
                           END-IF
                           MOVE MEMORIA (I: ) TO WORK
                           MOVE WORK TO MEMORIA (MEMORIA-CURSOR: )
                       END-IF
                  WHEN EDIT-CURSOR-DOWN
                   AND (MEMORIA (MEMORIA-CURSOR: ) = SPACES
                    OR  MEMORIA (MEMORIA-CURSOR: ) = X"0A")
                        MOVE 1 TO ERRO
                  WHEN EDIT-CURSOR-UP
                   AND MEMORIA-CURSOR = 1
                       MOVE 1 TO ERRO
                  WHEN EDIT-CURSOR-DOWN
                       MOVE CURPOS-COL     TO C C-SAVE
                       MOVE 0              TO L
                       MOVE MEMORIA-CURSOR TO SAVE-CURSOR
                       PERFORM TEST AFTER UNTIL CURPOS-COL = C
                            OR  MEMORIA (MEMORIA-CURSOR: ) = X"0A"
                          ADD 1 TO MEMORIA-CURSOR CURPOS-COL
                          IF  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                          OR  MEMORIA-CURSOR = LENGTH OF MEMORIA
                              IF  L = 0
                              AND MEMORIA-CURSOR < LENGTH OF MEMORIA
                                  IF  MEMORIA(MEMORIA-CURSOR:) <> X"0A"
                                      MOVE CWNOTE-COLUMN TO CURPOS-COL
                                      PERFORM 100-SKIP-MARGIN
                                         THRU 100-99-FIM
                                              MEMORIA-MARGIN TIMES
                                      MOVE 1 TO L
                                  ELSE
                                      MOVE C TO CURPOS-COL
                                  END-IF
                              ELSE
                                  MOVE C TO CURPOS-COL
                                  SUBTRACT 2 FROM MEMORIA-CURSOR
                              END-IF
                          END-IF
                       END-PERFORM
                       IF   MEMORIA (MEMORIA-CURSOR: ) <> X"0A"
                            ADD 1 TO MEMORIA-CURSOR
                            IF   CURPOS-LIN = LIN-FIM
                            AND  MEMORIA-CURSOR <> SAVE-CURSOR
                                 PERFORM VARYING MEMORIA-POINTER
                                            FROM MEMORIA-POINTER
                                              BY 1
                                    UNTIL
                                     MEMORIA-POINTER = LENGTH OF MEMORIA
                                       OR
                                    MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                                         CONTINUE
                                 END-PERFORM
                                 IF MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                                    ADD 1 TO MEMORIA-POINTER
                                 END-IF
                            END-IF
                       ELSE
                            SUBTRACT 1 FROM MEMORIA-CURSOR
                            MOVE MEMORIA-MARGIN TO MARGIN-SAVE
                            PERFORM 200-DISPLAY
                            PERFORM UNTIL MEMORIA-CURSOR = 1
                                       OR (CURPOS-COL NOT > C-SAVE)
                                    SUBTRACT 1 FROM MEMORIA-CURSOR
                                                    CURPOS-COL
                            END-PERFORM
                            IF  MEMORIA-MARGIN > MARGIN-SAVE
                                COMPUTE C-SAVE = MEMORIA-MARGIN
                                               - MARGIN-SAVE
                                SUBTRACT C-SAVE FROM MEMORIA-CURSOR
                                MOVE MARGIN-SAVE TO MEMORIA-MARGIN
                            END-IF
                       END-IF
                       IF   MEMORIA-CURSOR = SAVE-CURSOR
                            MOVE 1 TO ERRO
                       ELSE
                            IF  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                                MOVE 0 TO MEMORIA-MARGIN
                            END-IF
                       END-IF
                  WHEN EDIT-CURSOR-UP
                       MOVE CURPOS-COL TO C
                       MOVE 0          TO CURPOS-COL L
                       MOVE MEMORIA-CURSOR TO SAVE-CURSOR
                       PERFORM UNTIL MEMORIA-CURSOR < 2
                                  OR MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                               SUBTRACT 1 FROM MEMORIA-CURSOR
                       END-PERFORM
                       IF  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                           SUBTRACT 1 FROM MEMORIA-CURSOR
                           PERFORM VARYING MEMORIA-CURSOR
                                      FROM MEMORIA-CURSOR BY -1
                                        UNTIL MEMORIA-CURSOR < 2
                                         OR MEMORIA
                                          (MEMORIA-CURSOR: 1) = X"0A"
                                   CONTINUE
                           END-PERFORM
                           IF  MEMORIA (MEMORIA-CURSOR: 1) <> X"0A"
                               SUBTRACT 1 FROM MEMORIA-CURSOR
                           END-IF
                       ELSE
                           MOVE 1 TO L
                                     MEMORIA-POINTER
                       END-IF
                       MOVE CWNOTE-COLUMN TO CURPOS-COL
                       PERFORM 100-SKIP-MARGIN THRU 100-99-FIM
                               MEMORIA-MARGIN TIMES
                       PERFORM UNTIL CURPOS-COL = C
                          ADD 1 TO MEMORIA-CURSOR CURPOS-COL
                          IF   MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                               MOVE C TO CURPOS-COL
                               SUBTRACT 2 FROM MEMORIA-CURSOR
                          END-IF
                       END-PERFORM
                       IF   L = 0
                            ADD 1 TO MEMORIA-CURSOR
                       END-IF
                       IF   MEMORIA-CURSOR = SAVE-CURSOR
                            IF   MEMORIA-MARGIN NOT = 0
                                 MOVE 0 TO MEMORIA-MARGIN
                            ELSE
                                 MOVE 1 TO ERRO
                            END-IF
                       END-IF
                  WHEN EDIT-DEL
                       IF   MEMORIA (MEMORIA-CURSOR: 1) = X"0D"
                            COMPUTE I = MEMORIA-CURSOR + 2
                       ELSE
                            COMPUTE I = MEMORIA-CURSOR + 1
                       END-IF
                       MOVE MEMORIA (I: ) TO WORK
                       MOVE WORK TO MEMORIA (MEMORIA-CURSOR: )
                  WHEN EDIT-END
                       IF  MEMORIA-CURSOR = LENGTH OF MEMORIA
                       OR  MEMORIA (MEMORIA-CURSOR: ) = SPACES
                       OR  MEMORIA (MEMORIA-CURSOR: ) = X"0D0A"
                           MOVE 1 TO ERRO
                       ELSE
                           PERFORM
                             UNTIL MEMORIA-CURSOR = LENGTH OF MEMORIA
                                OR MEMORIA (MEMORIA-CURSOR: 1) = X"0D"
                                   ADD 1 TO MEMORIA-CURSOR
                                   IF  CURPOS-COL = COL-FIM
                                       ADD 1 TO MEMORIA-MARGIN
                                   ELSE
                                       ADD 1 TO CURPOS-COL
                                   END-IF
                           END-PERFORM
                       END-IF
                  WHEN EDIT-CONTROL-CURSOR-LEFT
                       SET EDIT-CURSOR-LEFT TO TRUE
                       MOVE 3 TO ERRO
                  WHEN EDIT-CONTROL-CURSOR-RIGHT
                       SET EDIT-CURSOR-RIGHT TO TRUE
                       MOVE 3 TO ERRO
                  WHEN EDIT-HOME
                       PERFORM UNTIL MEMORIA-CURSOR < 2
                                  OR MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                              SUBTRACT 1 FROM MEMORIA-CURSOR
                       END-PERFORM
                  WHEN EDIT-CONTROL-HOME
                    OR EDIT-CONTROL-PAGE-UP
                       MOVE 0 TO MEMORIA-MARGIN
                       MOVE 1 TO MEMORIA-CURSOR
                                 MEMORIA-POINTER
                  WHEN EDIT-CONTROL-END
                    OR EDIT-CONTROL-PAGE-DOWN
                       MOVE 0 TO MEMORIA-MARGIN
                       PERFORM VARYING MEMORIA-CURSOR
                                  FROM LENGTH OF MEMORIA BY -1
                                 UNTIL MEMORIA-CURSOR = 1
                                 OR MEMORIA (MEMORIA-CURSOR: 1) <> SPACE
                               CONTINUE
                       END-PERFORM
                       IF   MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                            SUBTRACT 1 FROM MEMORIA-CURSOR
                       END-IF
                       MOVE 0 TO L
                       PERFORM VARYING MEMORIA-POINTER
                                  FROM MEMORIA-CURSOR BY -1
                                 UNTIL MEMORIA-POINTER = 1
                                    OR L = CWNOTE-VERTICAL-LENGTH
                                 IF MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                                    ADD 1 TO L
                                 END-IF
                       END-PERFORM
                       IF   MEMORIA (MEMORIA-POINTER: 1) NOT = 1
                            ADD 2 TO MEMORIA-POINTER
                       END-IF
                  WHEN EDIT-PAGE-DOWN
                       MOVE 0          TO LD
                       MOVE CURPOS-LIN TO L-SAVE
                       MOVE CURPOS-COL TO C-SAVE
                       MOVE MEMORIA-CURSOR  TO SAVE-CURSOR
                       MOVE MEMORIA-POINTER TO SAVE-POINTER
                       PERFORM VARYING MEMORIA-POINTER
                          FROM MEMORIA-POINTER BY 1
                         UNTIL MEMORIA-CURSOR = LENGTH OF MEMORIA
                            OR MEMORIA (MEMORIA-CURSOR: ) = SPACES
                            OR MEMORIA (MEMORIA-CURSOR: ) = X"0A"
                            OR LD = CWNOTE-VERTICAL-LENGTH
                               ADD 1 TO MEMORIA-CURSOR
                               IF MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                                  ADD 1 TO LD
                               END-IF
                       END-PERFORM
                       PERFORM 200-DISPLAY
                       MOVE L-SAVE TO CURPOS-LIN
                       MOVE C-SAVE TO CURPOS-COL
                       PERFORM UNTIL (CURPOS-LIN = 1 AND CURPOS-COL = 1)
                               OR MOUSE (CURPOS-LIN CURPOS-COL) NOT = 0
                               SUBTRACT 1 FROM CURPOS-COL
                               IF   CURPOS-COL  < 1
                                    SUBTRACT 1 FROM CURPOS-LIN
                                    MOVE C-SAVE TO CURPOS-COL
                               END-IF
                       END-PERFORM
                       IF   MOUSE (L-SAVE C-SAVE) NOT = 0
                            MOVE MOUSE (L-SAVE C-SAVE) TO MEMORIA-CURSOR
                       END-IF
                       IF   MEMORIA (MEMORIA-CURSOR: ) = SPACES
                            MOVE SAVE-CURSOR  TO MEMORIA-CURSOR
                            MOVE SAVE-POINTER TO MEMORIA-POINTER
                            MOVE 1            TO ERRO
                       END-IF
=>                WHEN EDIT-PAGE-UP
                       MOVE 0          TO LD
                       MOVE CURPOS-LIN TO L-SAVE
                       MOVE CURPOS-COL TO C-SAVE
                       MOVE MEMORIA-CURSOR  TO SAVE-CURSOR
                       MOVE MEMORIA-POINTER TO SAVE-POINTER
                       PERFORM VARYING MEMORIA-POINTER
                          FROM MEMORIA-POINTER BY -1
                         UNTIL MEMORIA-POINTER < 2
                            OR LD = CWNOTE-VERTICAL-LENGTH
                               SUBTRACT 1 FROM MEMORIA-CURSOR
                               IF MEMORIA (MEMORIA-POINTER: 1) = X"0D"
                                  ADD 1 TO LD
                               END-IF
                       END-PERFORM
                       PERFORM 200-DISPLAY
                       MOVE L-SAVE TO CURPOS-LIN
                       MOVE C-SAVE TO CURPOS-COL
                       PERFORM UNTIL (CURPOS-LIN = 1 AND CURPOS-COL = 1)
                               OR MOUSE (CURPOS-LIN CURPOS-COL) NOT = 0
                               SUBTRACT 1 FROM CURPOS-COL
                               IF   CURPOS-COL  < 1
                                    SUBTRACT 1 FROM CURPOS-LIN
                                    MOVE C-SAVE TO CURPOS-COL
                               END-IF
                       END-PERFORM
                       IF   MOUSE (L-SAVE C-SAVE) NOT = 0
                            MOVE MOUSE (L-SAVE C-SAVE) TO MEMORIA-CURSOR
                       END-IF
                       COMPUTE I = MEMORIA-POINTER + 1
                       IF  MEMORIA (I: 1) = X"0D"
                           PERFORM VARYING MEMORIA-POINTER
                                      FROM MEMORIA-POINTER BY -1
                                        UNTIL MEMORIA-POINTER < 2
                                         OR MEMORIA
                                          (MEMORIA-POINTER: 1) = X"0A"
                                   CONTINUE
                           END-PERFORM
                           IF  MEMORIA (MEMORIA-POINTER: 1) <> X"0A"
                           AND MEMORIA-POINTER > 1
                               SUBTRACT 1 FROM MEMORIA-POINTER
                           END-IF
                       END-IF
              END-EVALUATE
              EVALUATE ERRO
                  WHEN 0
                       PERFORM 200-DISPLAY THRU 200-99-FIM
                  WHEN 1
                       CALL X"E5"
              END-EVALUATE
           END-PERFORM

           IF   CWNOTE-UPDATE
                IF LB-WORKAREA NOT = SPACES
                   OPEN OUTPUT WORKAREA
                END-IF
                INITIALIZE TELA
                PERFORM VARYING B FROM LENGTH OF MEMORIA BY -1
                  UNTIL B = 0
                     OR MEMORIA (B: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF  B > 0
                    INSPECT MEMORIA (1: B)
                            CONVERTING ACENTOS-WINDOWS
                                    TO ACENTOS-850
                END-IF
                INITIALIZE TELA
                MOVE 1 TO L C
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > B
                        MOVE  MEMORIA (I: 1) TO WORKAREA-REG
                        IF WORKAREA-REG = X"0D"
                           ADD 1 TO L
                        END-IF
                        IF LB-WORKAREA NOT = SPACES
                           IF  (NOT CWUNIX-ON)
                           OR  (WORKAREA-REG NOT = X"0A")
                                WRITE WORKAREA-REG
                           END-IF
                        ELSE
                           IF (WORKAREA-REG NOT = X"0D")
                           AND(WORKAREA-REG NOT = X"0A")
                               MOVE WORKAREA-REG
                                 TO CWNOTE-MESSAGE (L) (C: 1)
                           END-IF
                        END-IF
                END-PERFORM
                PERFORM 200-DISPLAY THRU 200-99-FIM
                IF LB-WORKAREA NOT = SPACES
                   CLOSE WORKAREA
                END-IF
           END-IF

           IF   CWNOTE-TITLE NOT = SPACES
                EXEC COBOLware BoxW CLOSE END-EXEC
           END-IF

frango     subtract JANLIN from CWNOTE-LINE
frango     subtract JANCOL from CWNOTE-COLUMN
frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     DISPLAY JANPOS        UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       100-SKIP-MARGIN.

           ADD 1 TO MEMORIA-CURSOR
           IF  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
           OR  MEMORIA-CURSOR = LENGTH OF MEMORIA
               MOVE C TO CURPOS-COL
               SUBTRACT 2 FROM MEMORIA-CURSOR
           END-IF.

       100-99-FIM. EXIT.

       110-WORD.

           IF   ERRO = 3
                COMPUTE I = MEMORIA-CURSOR - 1
                IF (MEMORIA (MEMORIA-CURSOR: 1) <> SPACE
                AND MEMORIA (I: 1) = SPACE)
                OR  MEMORIA (MEMORIA-CURSOR: 1) = X"0D"
                OR  MEMORIA (MEMORIA-CURSOR: 1) = X"0A"
                    MOVE 0 TO ERRO
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       200-DISPLAY.

           IF  MEMORIA-CURSOR < MEMORIA-POINTER
               PERFORM VARYING MEMORIA-POINTER
                          FROM MEMORIA-CURSOR
                            BY -1
                       UNTIL MEMORIA-POINTER = 1
                          OR MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                       CONTINUE
               END-PERFORM
               IF  MEMORIA (MEMORIA-POINTER: 1) = X"0A"
                   ADD 1 TO MEMORIA-POINTER
               END-IF
           END-IF
           IF  MEMORIA-POINTER > 1
               COMPUTE I = MEMORIA-POINTER - 1
               PERFORM VARYING I FROM I BY -1
                         UNTIL I = 1
                            OR MEMORIA (I: 1) = X"0A"
                       CONTINUE
               END-PERFORM
               IF  I > 1
                   COMPUTE MEMORIA-POINTER = I + 1
               END-IF
           END-IF
           INITIALIZE TELA
           MOVE 1 TO L C
           MOVE CWNOTE-LINE   TO CURPOS-LIN
           MOVE CWNOTE-COLUMN TO CURPOS-COL
           PERFORM VARYING I FROM MEMORIA-POINTER
                               BY 1 UNTIL I > LENGTH OF MEMORIA
                                       OR L > CWNOTE-VERTICAL-LENGTH
                   IF  MEMORIA (I: 1) = X"0A"
                       EXIT PERFORM CYCLE
                   END-IF
                   ADD 1 TO COLUNA
                   IF  COLUNA > MEMORIA-MARGIN
                   AND (L NOT > CWNOTE-VERTICAL-LENGTH)
                   AND (C NOT > CWNOTE-HORIZONTAL-LENGTH)
                        COMPUTE LM = CWNOTE-LINE   + L - 1
                        COMPUTE CM = CWNOTE-COLUMN + C - 1
                        IF  LM NOT > LIN-FIM
                        AND CM NOT > COL-FIM
                            MOVE I TO MOUSE (LM CM)
                        END-IF
                        IF   MEMORIA (I: 1) = X"0D"
                             MOVE 0 TO COLUNA
                             ADD  1 TO L
                             MOVE 1 TO C
                             IF   I < MEMORIA-CURSOR
                                  ADD  1             TO CURPOS-LIN
                                  MOVE CWNOTE-COLUMN TO CURPOS-COL
                             END-IF
                        ELSE
                             MOVE MEMORIA (I: 1) TO TELA-LIN (L) (C: 1)
                             ADD  1 TO C
                             IF   I < MEMORIA-CURSOR
                                  ADD 1 TO CURPOS-COL
                             END-IF
                        END-IF
                   ELSE
                        IF   MEMORIA (I: 1) = X"0D"
                             MOVE 0 TO COLUNA
                             ADD  1 TO L
                             MOVE 1 TO C
                             IF   I < MEMORIA-CURSOR
                                  ADD  1             TO CURPOS-LIN
                                  MOVE CWNOTE-COLUMN TO CURPOS-COL
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM
           IF   CURPOS-COL > COL-FIM
                ADD 1 TO MEMORIA-MARGIN
                GO TO 200-DISPLAY
           END-IF
           IF   CURPOS-LIN > LIN-FIM
                SUBTRACT 1 FROM MEMORIA-CURSOR
                GO TO 200-DISPLAY
           END-IF.

       200-SCREEN.

           MOVE 1           TO L
           MOVE CWNOTE-LINE TO LINHAS
           PERFORM CWNOTE-VERTICAL-LENGTH TIMES
              IF   TELA-LIN (L) (1: CWNOTE-HORIZONTAL-LENGTH) NOT =
                   TELA-SAVE(L) (1: CWNOTE-HORIZONTAL-LENGTH)
      *            DISPLAY TELA-LIN (L)
      *                    LINE LINHAS COLUMN CWNOTE-COLUMN
      *                    WITH SIZE CWNOTE-HORIZONTAL-LENGTH
      *                              REVERSE-VIDEO
                   COMPUTE SCREEN-ROW    = LINHAS - 1
                   COMPUTE SCREEN-COLUMN = CWNOTE-COLUMN - 1
                   MOVE    CWNOTE-HORIZONTAL-LENGTH TO STRING-LENGTH
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                    TELA-LIN (L)
                                                    STRING-LENGTH
                   IF   CWNOTE-UPDATE
                        CALL "CBL_WRITE_SCR_N_ATTR"
                                       USING SCREEN-POSITION
                                             X"70"
                                             STRING-LENGTH
                   END-IF
                   MOVE TELA-LIN  (L) (1: CWNOTE-HORIZONTAL-LENGTH)
                     TO TELA-SAVE (L) (1: CWNOTE-HORIZONTAL-LENGTH)
              END-IF
              ADD 1 TO LINHAS L
           END-PERFORM.


       200-99-FIM. EXIT.
       END PROGRAM CWNOTE.
