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

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 VAZIO                   PIC  X(080)
              VALUE 'Sem informa‡äes'.
           05 LEN-VAZIO               PIC  9(003) VALUE 0.
           05 FROM-CWEHLP             PIC  9(001) VALUE 0.
           05 CORRENTE                PIC  X(008) VALUE SPACES.
           05 CWCASE                  PIC  X(003) VALUE SPACES.
           05 CWACCENT                PIC  X(003) VALUE SPACES.
           05 X91-RESULT       COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION     COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER    COMP-X PIC  9(002) VALUE 0.
           05 CORRENTE-ID      COMP-5 PIC S9(004) VALUE 0.
           05 ACCEPT-ID        COMP-5 PIC S9(004) VALUE 0.
           05 FRAME-ID         COMP-5 PIC S9(004) VALUE 0.
           05 TECLA                   PIC  9(003) VALUE 0. COPY CWEDIT.
           05 TECLA2                  PIC  9(003) VALUE 0. COPY CWKEYS.
           05 B                COMP-5 PIC S9(004) VALUE 0.
           05 I                COMP-5 PIC S9(004) VALUE 0.
           05 LINHAS           COMP-5 PIC S9(004) VALUE 0.
           05 ER-WORKAREA.
              10 FS-WORKAREA          PIC  X(002) VALUE "00".
              10 LB-WORKAREA          PIC  X(255) VALUE "HELP".
           05 TECLA-X          COMP-X PIC  9(002).
           05 TELA.
              10 L                    PIC  9(002).
              10 C                    PIC  9(003).
              10 M                    PIC  9(003).
              10 TELA-LIN OCCURS 25   PIC  X(80).
           05 SCREEN-POSITION.
              10 SCREEN-ROW              PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-COLUMN           PIC  9(002) COMP-X VALUE 0.
           05 STRING-LENGTH              PIC  9(004) COMP-X VALUE 0.

       COPY SP2 REPLACING ==X(2000)== BY ==X(32756)==.
       COPY CWSEND.

       LINKAGE SECTION.

       COPY CWNOTE.
       01  CWUSER-LK PIC X.

       PROCEDURE DIVISION USING PARAMETROS-CWNOTE CWUSER-LK.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 2
           AND  CWUSER-LK = "@"
                MOVE 1 TO FROM-CWEHLP
           ELSE
                MOVE 0 TO FROM-CWEHLP
           END-IF
           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           COMPUTE CWNOTE-HORIZONTAL-LENGTH =
                   CWNOTE-HORIZONTAL-LENGTH
                   - (CWNOTE-HORIZONTAL-LENGTH / 5) + 1
           DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
           ACCEPT CWCASE FROM ENVIRONMENT-VALUE
           INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   CWNOTE-OLDFILE = SPACES
                MOVE CWNOTE-FILE    TO LB-WORKAREA
           ELSE
                MOVE CWNOTE-OLDFILE TO LB-WORKAREA
           END-IF
           MOVE SPACE       TO CWNOTE-ARROW
           MOVE LOW-VALUES  TO SP2-FD-DATA
                               SP2-FD-VAR-LENS
           MOVE SPACES      TO SP2-FD-VAR-DATA
           MOVE "t"         TO SP2-FD-BOR-TYPE
           MOVE 0           TO SP2-FD-VAR-LEN LINHAS
           INITIALIZE TELA
           MOVE 1           TO L C
           IF LB-WORKAREA NOT = SPACES
              OPEN INPUT WORKAREA
              IF FS-WORKAREA = '30' OR '35'
                 OPEN OUTPUT WORKAREA
                 DISPLAY 'CWNOTE-EMPTY' UPON ENVIRONMENT-NAME
                 ACCEPT VAZIO FROM ENVIRONMENT-VALUE
                 EXEC COBOLware Pack String VAZIO
                      WIDTH LEN-VAZIO
                 END-EXEC
                 PERFORM VARYING B FROM 1 BY 1 UNTIL B > LEN-VAZIO
                         WRITE WORKAREA-REG FROM VAZIO(B:1)
                 END-PERFORM
                 MOVE 0 TO B
                 CLOSE WORKAREA
                 OPEN INPUT WORKAREA
              END-IF
              PERFORM UNTIL FS-WORKAREA > "09"
                         OR B = LENGTH OF SP2-FD-VAR-DATA
                      READ WORKAREA
                      IF  FS-WORKAREA < "10"
                          ADD 1 TO B
                          MOVE WORKAREA-REG TO SP2-FD-VAR-DATA (B: 1)
                          PERFORM 190-TELA THRU 190-99-FIM
                      END-IF
              END-PERFORM
              IF  B = LENGTH OF SP2-FD-VAR-DATA
                  READ WORKAREA
                  IF  FS-WORKAREA < "10"
                      MOVE "Tamanho do texto excede a capacidade"
                           TO  CWSEND-MSG
                      CALL "CWSEND" USING PARAMETROS-CWSEND
                      CLOSE WORKAREA
                      CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
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
                            TO SP2-FD-VAR-DATA (B: 1)
                      END-PERFORM
                      ADD 1 TO B MOVE X'0D' TO SP2-FD-VAR-DATA (B: 1)
                      ADD 1 TO B MOVE X'0A' TO SP2-FD-VAR-DATA (B: 1)
                      MOVE CWNOTE-MESSAGE (L) TO TELA-LIN (L)
                      IF   CWNOTE-MESSAGE (L) NOT = SPACES
                           MOVE L TO LINHAS
                      END-IF
              END-PERFORM
           END-IF
           IF   CWNOTE-VIEW
                PERFORM 200-DISPLAY THRU 200-99-FIM
                GOBACK
           END-IF

           IF  B > 0
               IF   CWACCENT = "OFF"
                    INSPECT SP2-FD-VAR-DATA (1: B)
                            CONVERTING ACENTOS-850
                                    TO ACENTOS-OFF
               ELSE
                    INSPECT SP2-FD-VAR-DATA (1: B)
                            CONVERTING ACENTOS-850
                                    TO ACENTOS-WINDOWS
               END-IF
               IF   CWCASE = "UPP"
                    INSPECT SP2-FD-VAR-DATA (1: B)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
               END-IF
               IF   CWCASE = "LOW"
                    INSPECT SP2-FD-VAR-DATA (1: B)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
               END-IF
           END-IF

           PERFORM 840-OPEN-WINDOW THRU 840-99-FIM
           CALL SP2    USING SP2-GET-PANEL-DEF SP2-PANEL-DEF
           MOVE LOW-VALUES        TO SP2-PD-CTRL-KEYS
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (01)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (02)
           MOVE SP2-KEY-TAB       TO SP2-PD-CTRL-KEY (03)
           MOVE SP2-KEY-BACKTAB   TO SP2-PD-CTRL-KEY (04)
           MOVE 500               TO SP2-PD-CTRL-KEY (05)
           IF   CWNOTE-KEY-ON = "Y" OR "y"
                             OR "S" OR "s" OR "*"
                MOVE SP2-KEY-F1      TO SP2-PD-CTRL-KEY (06)
                MOVE SP2-KEY-F2      TO SP2-PD-CTRL-KEY (07)
                MOVE SP2-KEY-F3      TO SP2-PD-CTRL-KEY (08)
                MOVE SP2-KEY-F4      TO SP2-PD-CTRL-KEY (09)
                MOVE SP2-KEY-F5      TO SP2-PD-CTRL-KEY (10)
                MOVE SP2-KEY-F6      TO SP2-PD-CTRL-KEY (11)
                MOVE SP2-KEY-F7      TO SP2-PD-CTRL-KEY (12)
                MOVE SP2-KEY-F8      TO SP2-PD-CTRL-KEY (13)
                MOVE SP2-KEY-F9      TO SP2-PD-CTRL-KEY (14)
                MOVE SP2-KEY-F10     TO SP2-PD-CTRL-KEY (15)
                IF   CWNOTE-UPDATE
                     MOVE SP2-KEY-ALT-Y   TO SP2-PD-CTRL-KEY (16)
                END-IF
                IF   CWNOTE-KEY-ON = "*"
                     MOVE SP2-KEY-ALT-0     TO SP2-PD-CTRL-KEY (01)
                     MOVE SP2-KEY-ALT-1     TO SP2-PD-CTRL-KEY (02)
                     MOVE SP2-KEY-ALT-2     TO SP2-PD-CTRL-KEY (03)
                     MOVE SP2-KEY-ALT-3     TO SP2-PD-CTRL-KEY (04)
                     MOVE SP2-KEY-ALT-4     TO SP2-PD-CTRL-KEY (05)
                     MOVE SP2-KEY-ALT-5     TO SP2-PD-CTRL-KEY (06)
                     MOVE SP2-KEY-ALT-6     TO SP2-PD-CTRL-KEY (07)
                     MOVE SP2-KEY-ALT-7     TO SP2-PD-CTRL-KEY (08)
                     MOVE SP2-KEY-ALT-8     TO SP2-PD-CTRL-KEY (09)
                     MOVE SP2-KEY-ALT-9     TO SP2-PD-CTRL-KEY (10)
                     MOVE SP2-KEY-ALT-A     TO SP2-PD-CTRL-KEY (11)
                     MOVE SP2-KEY-ALT-B     TO SP2-PD-CTRL-KEY (12)
                     MOVE SP2-KEY-ALT-C     TO SP2-PD-CTRL-KEY (13)
                     MOVE SP2-KEY-ALT-D     TO SP2-PD-CTRL-KEY (14)
                     MOVE SP2-KEY-ALT-E     TO SP2-PD-CTRL-KEY (15)
                     MOVE SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (16)
                     MOVE SP2-KEY-ALT-F     TO SP2-PD-CTRL-KEY (17)
                     MOVE SP2-KEY-ALT-F1    TO SP2-PD-CTRL-KEY (18)
                     MOVE SP2-KEY-ALT-F10   TO SP2-PD-CTRL-KEY (19)
                     MOVE SP2-KEY-ALT-F11   TO SP2-PD-CTRL-KEY (20)
                     MOVE SP2-KEY-ALT-F12   TO SP2-PD-CTRL-KEY (21)
                     MOVE SP2-KEY-ALT-F2    TO SP2-PD-CTRL-KEY (22)
                     MOVE SP2-KEY-ALT-F3    TO SP2-PD-CTRL-KEY (23)
                     MOVE SP2-KEY-ALT-F4    TO SP2-PD-CTRL-KEY (24)
                     MOVE SP2-KEY-ALT-F5    TO SP2-PD-CTRL-KEY (25)
                     MOVE SP2-KEY-ALT-F6    TO SP2-PD-CTRL-KEY (26)
                     MOVE SP2-KEY-ALT-F7    TO SP2-PD-CTRL-KEY (27)
                     MOVE SP2-KEY-ALT-F8    TO SP2-PD-CTRL-KEY (28)
                     MOVE SP2-KEY-ALT-F9    TO SP2-PD-CTRL-KEY (29)
                     MOVE SP2-KEY-ALT-G     TO SP2-PD-CTRL-KEY (30)
                     MOVE SP2-KEY-ALT-H     TO SP2-PD-CTRL-KEY (31)
                     MOVE SP2-KEY-ALT-I     TO SP2-PD-CTRL-KEY (32)
                     MOVE SP2-KEY-ALT-J     TO SP2-PD-CTRL-KEY (33)
                     MOVE SP2-KEY-ALT-K     TO SP2-PD-CTRL-KEY (34)
                     MOVE SP2-KEY-ALT-L     TO SP2-PD-CTRL-KEY (35)
                     MOVE SP2-KEY-ALT-M     TO SP2-PD-CTRL-KEY (36)
                     MOVE SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (37)
                     MOVE SP2-KEY-ALT-N     TO SP2-PD-CTRL-KEY (38)
                     MOVE SP2-KEY-ALT-O     TO SP2-PD-CTRL-KEY (39)
                     MOVE SP2-KEY-ALT-P     TO SP2-PD-CTRL-KEY (40)
                     MOVE SP2-KEY-ALT-Q     TO SP2-PD-CTRL-KEY (41)
                     MOVE SP2-KEY-ALT-R     TO SP2-PD-CTRL-KEY (42)
                     MOVE SP2-KEY-ALT-S     TO SP2-PD-CTRL-KEY (43)
                     MOVE SP2-KEY-ALT-T     TO SP2-PD-CTRL-KEY (44)
                     MOVE SP2-KEY-ALT-U     TO SP2-PD-CTRL-KEY (45)
                     MOVE SP2-KEY-ALT-V     TO SP2-PD-CTRL-KEY (46)
                     MOVE SP2-KEY-ALT-W     TO SP2-PD-CTRL-KEY (47)
                     MOVE SP2-KEY-ALT-X     TO SP2-PD-CTRL-KEY (48)
                     MOVE SP2-KEY-ALT-Y     TO SP2-PD-CTRL-KEY (49)
                     MOVE SP2-KEY-ALT-Z     TO SP2-PD-CTRL-KEY (50)
                     MOVE SP2-KEY-BACKTAB   TO SP2-PD-CTRL-KEY (51)
                     MOVE SP2-KEY-CTRL-F11  TO SP2-PD-CTRL-KEY (52)
                     MOVE SP2-KEY-CTRL-F12  TO SP2-PD-CTRL-KEY (53)
                     MOVE SP2-KEY-CTRL-PGDN TO SP2-PD-CTRL-KEY (54)
                     MOVE SP2-KEY-CTRL-PGUP TO SP2-PD-CTRL-KEY (55)
                     MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (56)
                     MOVE SP2-KEY-END       TO SP2-PD-CTRL-KEY (57)
                     MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (58)
                     MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (59)
                     MOVE SP2-KEY-F1        TO SP2-PD-CTRL-KEY (60)
                     MOVE SP2-KEY-F10       TO SP2-PD-CTRL-KEY (61)
                     MOVE SP2-KEY-F11       TO SP2-PD-CTRL-KEY (62)
                     MOVE SP2-KEY-F12       TO SP2-PD-CTRL-KEY (63)
                     MOVE SP2-KEY-F2        TO SP2-PD-CTRL-KEY (64)
                     MOVE SP2-KEY-F3        TO SP2-PD-CTRL-KEY (65)
                     MOVE SP2-KEY-F4        TO SP2-PD-CTRL-KEY (66)
                     MOVE SP2-KEY-F5        TO SP2-PD-CTRL-KEY (67)
                     MOVE SP2-KEY-F6        TO SP2-PD-CTRL-KEY (68)
                     MOVE SP2-KEY-F7        TO SP2-PD-CTRL-KEY (69)
                     MOVE SP2-KEY-F8        TO SP2-PD-CTRL-KEY (70)
                     MOVE SP2-KEY-F9        TO SP2-PD-CTRL-KEY (71)
                     MOVE SP2-KEY-HOME      TO SP2-PD-CTRL-KEY (72)
                     MOVE SP2-KEY-PGDN      TO SP2-PD-CTRL-KEY (73)
                     MOVE SP2-KEY-PGUP      TO SP2-PD-CTRL-KEY (74)
                     MOVE SP2-KEY-SHIFT-F1  TO SP2-PD-CTRL-KEY (75)
                     MOVE SP2-KEY-SHIFT-F10 TO SP2-PD-CTRL-KEY (76)
                     MOVE SP2-KEY-SHIFT-F11 TO SP2-PD-CTRL-KEY (77)
                     MOVE SP2-KEY-SHIFT-F12 TO SP2-PD-CTRL-KEY (78)
                     MOVE SP2-KEY-SHIFT-F2  TO SP2-PD-CTRL-KEY (79)
                     MOVE SP2-KEY-SHIFT-F3  TO SP2-PD-CTRL-KEY (80)
                     MOVE SP2-KEY-SHIFT-F4  TO SP2-PD-CTRL-KEY (81)
                     MOVE SP2-KEY-SHIFT-F5  TO SP2-PD-CTRL-KEY (82)
                     MOVE SP2-KEY-SHIFT-F6  TO SP2-PD-CTRL-KEY (83)
                     MOVE SP2-KEY-SHIFT-F7  TO SP2-PD-CTRL-KEY (84)
                     MOVE SP2-KEY-SHIFT-F8  TO SP2-PD-CTRL-KEY (85)
                     MOVE SP2-KEY-SHIFT-F9  TO SP2-PD-CTRL-KEY (86)
                     MOVE SP2-KEY-TAB       TO SP2-PD-CTRL-KEY (87)
                     MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (88)
                     MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (89)
                END-IF
           END-IF
           CALL "CWRESE"
           CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF

      *    COMPUTE SP2-FD-WIDTH  = (CWNOTE-HORIZONTAL-LENGTH * 10)
Frango     COMPUTE SP2-FD-WIDTH  = (CWNOTE-HORIZONTAL-LENGTH * 10) + 10
           COMPUTE SP2-FD-HEIGHT = SP2-WD-HEIGHT * 10
           MOVE "e"                    TO SP2-FD-CTRL-TYPE
           MOVE "h"                    TO SP2-FD-CURS-SKIP
FRANGO     IF   CWNOTE-VIEW-WINDOW
FRANGO          MOVE "p" TO SP2-FD-OUTPUT
FRANGO     END-IF

           MOVE B                         TO SP2-FD-INITIAL-LEN
           MOVE LENGTH OF SP2-FD-VAR-DATA TO SP2-FD-USER-LEN
                                             SP2-FD-MAX-LEN
                                             SP2-FD-PROG-LEN
           MOVE CWNOTE-HORIZONTAL-LENGTH  TO SP2-FD-ITEM-LEN
           MOVE "CWNOTE"                  TO SP2-FD-NAME
           CALL "CWSPID" USING SP2-FD-ID "FInsert"
           MOVE 2        TO SP2-FD-FONT-ID
           MOVE -1       TO SP2-FD-FONT-ID
           CALL SP2      USING SP2-SET-PANEL-FIELDS SP2-FD-VAR-DATA
           CALL SP2      USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           CALL "CWSPID" USING SP2-FD-ID "FDelete"
           MOVE SP2-PD-NAME TO SP2-CD-NEXT-PANEL
           MOVE SP2-FD-ID   TO SP2-CD-NEXT-FLD-ID
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
Frango     IF   CWNOTE-VIEW-WINDOW
Frango          MOVE "1" TO SP2-CD-WAIT-SW *> aceita enter
Frango     END-IF
           CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
           EVALUATE SP2-CD-KEY
                    WHEN SP2-KEY-TAB
                         MOVE ">" TO CWNOTE-ARROW
                    WHEN SP2-KEY-BACKTAB
                         MOVE "<" TO CWNOTE-ARROW
                    WHEN SP2-KEY-ALT-Y
                         MOVE "=" TO CWNOTE-ARROW
           END-EVALUATE
           IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
           OR                SP2-KEY-APP-CLOSE
           OR                SP2-KEY-CLOSE
                 CALL "CWCRTS" USING "S" X"FFFFFF"
           END-IF
           IF   CWNOTE-EDIT-ON
                PERFORM 100-CONVERTE-TECLA THRU 100-99-FIM
           END-IF

           IF   CWNOTE-UPDATE
                IF LB-WORKAREA NOT = SPACES
                   OPEN OUTPUT WORKAREA
                END-IF
                INITIALIZE TELA
                PERFORM VARYING B FROM LENGTH OF SP2-FD-VAR-DATA BY -1
                  UNTIL B = 0
                     OR SP2-FD-VAR-DATA (B: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF  B > 0
                    IF   CWACCENT = "OFF"
                         INSPECT SP2-FD-VAR-DATA (1: B)
                                 CONVERTING ACENTOS-WINDOWS
                                         TO ACENTOS-OFF
                    ELSE
                         INSPECT SP2-FD-VAR-DATA (1: B)
                                 CONVERTING ACENTOS-WINDOWS
                                         TO ACENTOS-850
                    END-IF
                    IF   CWCASE = "UPP"
                         INSPECT SP2-FD-VAR-DATA (1: B)
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                    END-IF
                    IF   CWCASE = "LOW"
                         INSPECT SP2-FD-VAR-DATA (1: B)
                             CONVERTING MAIUSCULAS TO MINUSCULAS
                    END-IF
                END-IF
                INITIALIZE TELA CWNOTE-ARRAY
                MOVE 1 TO L C
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > B
                        MOVE  SP2-FD-VAR-DATA (I: 1) TO WORKAREA-REG
                        IF LB-WORKAREA NOT = SPACES
                           WRITE WORKAREA-REG
                        ELSE
                           MOVE WORKAREA-REG
                             TO CWNOTE-MESSAGE (L) (C: 1)
                        END-IF
                        PERFORM 190-TELA THRU 190-99-FIM
                END-PERFORM
      *         PERFORM 200-DISPLAY THRU 200-99-FIM
                IF LB-WORKAREA NOT = SPACES
                   CLOSE WORKAREA
                END-IF
           END-IF

           CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           IF   CORRENTE NOT = LOW-VALUES
                MOVE CORRENTE TO SP2-ND-NAME
                CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
           END-IF

           IF   CWNOTE-TITLE = SPACES
           AND  (NOT CWNOTE-UPDATE-WINDOW)
                PERFORM 200-DISPLAY THRU 200-99-FIM
           END-IF.

       000-99-FIM. GOBACK.

       840-OPEN-WINDOW.

           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           MOVE LOW-VALUES       TO SP2-WD-DATA
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           MOVE SP2-WD-WINDOW-ID   TO CORRENTE-ID
           MOVE SP2-WD-NAME        TO CORRENTE
           IF   CWNOTE-TITLE = SPACES
                MOVE "n"        TO SP2-WD-BOR-TYPE
                MOVE "p"        TO SP2-WD-BOR-TYPE
           ELSE
                IF CWNOTE-TITLE(1: 1) = X"B5"
                   MOVE CWNOTE-TITLE(2: ) TO SP2-WD-TITLE
                ELSE
                   MOVE CWNOTE-TITLE TO SP2-WD-TITLE
                END-IF
                INSPECT SP2-WD-TITLE CONVERTING X"FFC6" TO "  "
                MOVE "f"          TO SP2-WD-BOR-TYPE
           END-IF
           MOVE "CWNOTE"        TO SP2-WD-NAME
KSAT       MOVE X'02'           TO SP2-WD-OPTIONS-3
           DISPLAY 'CWNOTE-WD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-WD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE 10              TO SP2-WD-CELL-WIDTH
           MOVE 20              TO SP2-WD-CELL-HEIGHT
           COMPUTE SP2-WD-WIDTH  = CWNOTE-HORIZONTAL-LENGTH + 1
           COMPUTE SP2-WD-HEIGHT = CWNOTE-VERTICAL-LENGTH + 0
           IF      SP2-WD-HEIGHT < 3
               ADD 1 TO SP2-WD-HEIGHT
           END-IF
Frango*    IF  (NOT CWNOTE-UPDATE)
Frango*    AND  LINHAS < CWNOTE-VERTICAL-LENGTH
Frango*    AND  LINHAS > 4
Frango*         COMPUTE SP2-WD-HEIGHT = LINHAS + 2
Frango*    END-IF
           IF   CWNOTE-LINE   = 1
           AND  CWNOTE-COLUMN = 1
                MOVE 15 TO SP2-WD-ROW
                MOVE 05 TO SP2-WD-COL
           ELSE
                CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                COMPUTE SP2-WD-ROW = CWNOTE-LINE   * 6
                COMPUTE SP2-WD-COL = (CWNOTE-COLUMN - 1) * 5
                IF   X91-PARAMETER > 1
                     COMPUTE SP2-WD-ROW = CWNOTE-LINE * 5
                END-IF
           END-IF
           MOVE -1               TO SP2-WD-TITLE-ROWS
           MOVE -1               TO SP2-WD-MENU-ROWS
           MOVE CORRENTE-ID      TO SP2-WD-OWNR-ID
           MOVE X"10"            TO SP2-WD-MORE-OPTIONS
           CALL SP2          USING SP2-OPEN-WINDOW SP2-WINDOW-DEF
           MOVE LOW-VALUES        TO SP2-PD-DATA
           DISPLAY 'CWNOTE-PD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-PD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE 0                 TO SP2-PD-ROW
           MOVE 0                 TO SP2-PD-COL
           MOVE "CWNOTE"          TO SP2-PD-NAME
           MOVE SP2-KEY-LEFT      TO SP2-PD-LEFT
           MOVE SP2-KEY-RIGHT     TO SP2-PD-RIGHT
           MOVE SP2-KEY-UP        TO SP2-PD-UP
           MOVE SP2-KEY-DOWN      TO SP2-PD-DOWN
           MOVE SP2-KEY-TAB       TO SP2-PD-TAB
           MOVE SP2-KEY-BACKTAB   TO SP2-PD-BACKTAB
           MOVE SP2-KEY-BACKSPAC  TO SP2-PD-BACKSPAC
           MOVE SP2-KEY-DELETE    TO SP2-PD-DELETE
           MOVE SP2-KEY-INSERT    TO SP2-PD-INSERT
           MOVE SP2-KEY-HOME      TO SP2-PD-HOME
           MOVE SP2-KEY-END       TO SP2-PD-END
           MOVE SP2-KEY-CTRL-HOME TO SP2-PD-HOME-PAN
           MOVE SP2-KEY-CTRL-END  TO SP2-PD-END-PAN
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (01)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (02)
           MOVE 500               TO SP2-PD-CTRL-KEY (03)
           CALL "CWRESE"
           CALL SP2            USING SP2-SET-PANEL-DEF
                                     SP2-PANEL-DEF.
       840-99-FIM. EXIT.

       100-CONVERTE-TECLA.

           COPY CWSPKY.
           IF   CWNOTE-KEY-ON = "Y" OR "y"
                             OR "S" OR "s"
                EVALUATE TRUE
                   WHEN EDIT-F1  MOVE 02 TO CWNOTE-KEY
                   WHEN EDIT-F2  MOVE 03 TO CWNOTE-KEY
                   WHEN EDIT-F3  MOVE 04 TO CWNOTE-KEY
                   WHEN EDIT-F4  MOVE 05 TO CWNOTE-KEY
                   WHEN EDIT-F5  MOVE 06 TO CWNOTE-KEY
                   WHEN EDIT-F6  MOVE 07 TO CWNOTE-KEY
                   WHEN EDIT-F7  MOVE 08 TO CWNOTE-KEY
                   WHEN EDIT-F8  MOVE 09 TO CWNOTE-KEY
                   WHEN EDIT-F9  MOVE 10 TO CWNOTE-KEY
                   WHEN EDIT-F10 MOVE 11 TO CWNOTE-KEY
                END-EVALUATE
                IF   CWNOTE-KEY NOT = 0
                     SET EDIT-ENTER TO TRUE
                END-IF
           ELSE
                IF   CWNOTE-KEY-ON = "*"
                AND (NOT EDIT-CURSOR-DOWN)
                AND (NOT EDIT-CURSOR-UP)
                AND (NOT EDIT-ESC)
                AND (NOT EDIT-CURSOR-LEFT)
                AND (NOT EDIT-CURSOR-RIGHT)
                AND (NOT EDIT-ENTER)
                AND (NOT EDIT-BACKSPACE)
                AND (NOT EDIT-CONTROL-PAGE-DOWN)
                AND (NOT EDIT-CONTROL-PAGE-UP)
                AND (NOT EDIT-END)
                AND (NOT EDIT-ESC)
                AND (NOT EDIT-HOME)
                AND (NOT EDIT-PAGE-DOWN)
                AND (NOT EDIT-PAGE-UP)
                     MOVE TECLA     TO TECLA-X
                     MOVE TECLA-X (1: ) TO CWNOTE-KEY-ON
                     SET EDIT-ENTER TO TRUE
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       190-TELA.

           IF  (C NOT > CWNOTE-HORIZONTAL-LENGTH)
           AND (L NOT > CWNOTE-VERTICAL-LENGTH)
                IF  (WORKAREA-REG NOT = X"0D")
                AND (WORKAREA-REG NOT = X"0A")
                    MOVE WORKAREA-REG TO TELA-LIN (L) (C: 1)
                    ADD  1 TO C
                END-IF
                IF   WORKAREA-REG = X"0D"
                     ADD  1 TO L LINHAS
                     MOVE 1 TO C
                END-IF
           END-IF.

       190-99-FIM. EXIT.

       200-DISPLAY.

           IF   FROM-CWEHLP = 1
                EXIT PARAGRAPH
           END-IF

           MOVE 1 TO L
           MOVE CWNOTE-LINE TO LINHAS
           PERFORM CWNOTE-VERTICAL-LENGTH TIMES
      *            DISPLAY TELA-LIN (L)
      *                    LINE LINHAS COLUMN CWNOTE-COLUMN
      *                    WITH SIZE CWNOTE-HORIZONTAL-LENGTH
                   COMPUTE SCREEN-ROW    = LINHAS - 1
                   COMPUTE SCREEN-COLUMN = CWNOTE-COLUMN - 1
                   MOVE    CWNOTE-HORIZONTAL-LENGTH TO STRING-LENGTH
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                              TELA-LIN (L)
                                              STRING-LENGTH
                   ADD 1 TO LINHAS L
           END-PERFORM

           CALL "CWUSER" USING "R". *> Refresh screen

       200-99-FIM. EXIT.
       END PROGRAM CWNOTE.
