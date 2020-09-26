       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSEND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/07/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Emite mensagen de erro "X(060)"              *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 OK                       PIC  9(002) VALUE 0.
           05 COLOR-STATUS      PIC  X      VALUE '0'.
              88 COLOR-ON                   VALUE '1'.
              88 COLOR-OFF                  VALUE '0' '3'.
              88 COLOR-CLOSED               VALUE '3'.
           05 TASK                     PIC  9(006) VALUE 0.
           05 CWIAEF                   PIC  X(003) VALUE SPACES.
           05 CWGETL                   PIC  X(003) VALUE SPACES.
           05 CWACCENT                 PIC  X(003) VALUE SPACES.
           05 CWLITS                   PIC  X(003) VALUE SPACES.
           05 CORRENTE-ID       COMP-5 PIC S9(004) VALUE 0.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       01  AREAS-DE-TRABALHO-1.
           05 CWUSER-MSG               PIC  X(003) VALUE SPACES.
           05 CWSEND-OK                PIC  X(002) VALUE SPACES.
           05 CHAR-X.
              10 CHAR-N         COMP-X PIC  9(002).
           05 COLW                     PIC  9(002) VALUE 0.
           05 SIM-NAO                  PIC  X(003) VALUE SPACES.
           05 PROPRIETARIA             PIC  X(008) VALUE SPACES.
           05 MSG-D                    PIC  X(074) VALUE SPACES.
           05 TIL                      PIC  9(002) VALUE 0.
           05 CX                       PIC  9(002) VALUE 0.
           05 MAIOR                    PIC  9(002) VALUE 0.
           05 C                        PIC  9(002) VALUE 0.
           05 SN                       PIC  9(002) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 PGM                      PIC  X(030) VALUE SPACES.
           05 D                        PIC  9(003) VALUE 0.
           05 L                        PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(002) VALUE 0.
           05 S                        PIC  9(002) VALUE 0.
           05 LENG                     PIC  9(003) VALUE 0.
           05 LENG2                    PIC  9(003) VALUE 0.
           05 TAMANHO                  PIC  9(002) VALUE 0.
           05 BOTOES                   PIC  9(002) VALUE 0.

       COPY CWBOXW.
       COPY CWLINE.
       COPY CWSPWS.
       COPY CWLOGW.
       COPY CWSTAT.

       LINKAGE SECTION.

       01  PARAMETROS-CWSEND.
           05 CWSEND-MSG                     PIC  X(074).
           05 CWSEND-SCREENS.
              10 CWSEND-CHAR   OCCURS 8      PIC  X(001).
              10 CWSEND-SCREEN OCCURS 8.
                20 BYTE-TXT OCCURS 10        PIC  X(001).
           05 CWSEND-OPTION                  PIC  9(001).
           05 CWSEND-OPTION-CHAR             PIC  X(001).
           05 CWSEND-TIMEOUT-STATUS          PIC  9(001).
              88 CWSEND-TIMEOUT-ENABLE            VALUE 1 5.
              88 CWSEND-TIMEOUT-RETRY             VALUE 5.
              88 CWSEND-TIMEOUT-DISABLE           VALUE 0.
           05 CWSEND-TIMEOUT-RETURN          PIC  9(001).
              88 CWSEND-TIMEOUT-ON                VALUE 1.
              88 CWSEND-TIMEOUT-OFF               VALUE 0.

       01  LB                                PIC  X(050).

       PROCEDURE DIVISION USING PARAMETROS-CWSEND LB.

       000-INICIO.

           DISPLAY "CWCOLOR-STATUS" UPON ENVIRONMENT-NAME
           ACCEPT  COLOR-STATUS     FROM ENVIRONMENT-VALUE
           ON 1
              DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
              ACCEPT CWLITS FROM ENVIRONMENT-VALUE
              INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
              ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
              INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   CWSEND-SCREENS = "        _~Ok_"
           OR   CWSEND-SCREENS = "        _~OK_"
           OR   CWSEND-SCREENS = "        _OK_"
           OR   CWSEND-SCREENS = "        _Ok_"
           OR   CWSEND-SCREENS = "        _~Ok"
           OR   CWSEND-SCREENS = "        _~OK"
           OR   CWSEND-SCREENS = "        _OK"
           OR   CWSEND-SCREENS = "        _Ok"
           OR   CWSEND-SCREENS = "        ~Ok"
           OR   CWSEND-SCREENS = "        ~OK"
           OR   CWSEND-SCREENS = "        OK"
           OR   CWSEND-SCREENS = "        Ok"
                MOVE SPACES TO CWSEND-SCREENS
           END-IF
           INSPECT CWSEND-MSG CONVERTING LOW-VALUE TO SPACE

           IF (CWSEND-MSG(1:28) = 'Falha de acesso ao FileShare')
              DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
              ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
              IF CWGETL = 'OFF'
              AND COLOR-ON
                 CALL "cobwareg" USING SP2-WD-TITLE TASK STATUS-DEF
              END-IF
           END-IF

           INITIALIZE AREAS-DE-TRABALHO-1
           DISPLAY "CWSEND-OK" UPON ENVIRONMENT-NAME
           ACCEPT  CWSEND-OK   FROM ENVIRONMENT-VALUE
           COPY CWSPPD.
           MOVE 4 TO CWLINE-LINE
           IF   CWSEND-MSG (1: 1) =  "^"
                MOVE 2 TO CWLINE-LINE
           END-IF

           IF   CWSEND-MSG NOT EQUAL SPACES
                IF  CWSEND-MSG (1: 44) =
                    "Componente em desenvolvimento ou manuten‡Æo:"
                    MOVE CWSEND-MSG (45: ) TO PGM
                    MOVE SPACES TO CWSEND-MSG
                    STRING 'Componente "' DELIMITED BY SIZE
                           PGM            DELIMITED BY SPACE
                           '" em desenvolvimento ou manuten‡Æo'
                                          DELIMITED BY SIZE
                           INTO CWSEND-MSG
                    MOVE "#" TO CWLOGW-FUNCTION
                    MOVE SPACES TO CWLOGW-TEXT
                    STRING 'Programa "' DELIMITED BY SIZE
                           PGM            DELIMITED BY SPACE
                           '" nÆo encontrado'
                                          DELIMITED BY SIZE
                           INTO CWLOGW-TEXT
                    DISPLAY 'CWSENDLOG' UPON ENVIRONMENT-NAME
                    DISPLAY 'ON'        UPON ENVIRONMENT-VALUE
                    CALL "CWLOGW" USING PARAMETROS-CWLOGW
                    DISPLAY 'CWSENDLOG' UPON ENVIRONMENT-NAME
                    DISPLAY 'OFF'       UPON ENVIRONMENT-VALUE
                END-IF
                PERFORM VARYING I FROM 1 BY 1
                        UNTIL CWSEND-MSG (I: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF  I > 1
                    MOVE CWSEND-MSG (I: ) TO MSG-D
                    MOVE MSG-D TO CWSEND-MSG
                END-IF
                MOVE 29    TO C
                PERFORM VARYING CX FROM 74 BY -1
                        UNTIL CWSEND-MSG (CX: 1) NOT = SPACE
                              CONTINUE
                END-PERFORM
                IF  (CWSEND-MSG (CX: 1) NOT = ".")
                AND (CWSEND-MSG (CX: 1) NOT = "?")
                AND (CWSEND-MSG (CX: 1) NOT = "!")
                AND (CWSEND-MSG (CX: 1) NOT = ":")
                AND (CWSEND-MSG (CX: 1) NOT = "_")
                AND (CWSEND-MSG (CX: 1) NOT = ")")
                AND CX < 74
                    ADD 1    TO CX
                    IF  CWSEND-SCREEN (2) = SPACE
                        MOVE "." TO CWSEND-MSG (CX: 1)
                    ELSE
                        ADD 1    TO CX
                        MOVE "?" TO CWSEND-MSG (CX: 1)
                    END-IF
                END-IF
                MOVE CWSEND-MSG TO MSG-D
                IF MSG-D(1:1) NOT = '('
                   INSPECT MSG-D CONVERTING "_" TO SPACE
                END-IF
                MOVE LOW-VALUES  TO SP2-MS-DATA
                MOVE 16          TO X91-FUNCTION
                CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                MOVE 0 TO I
                IF   X91-PARAMETER > 1
                AND (LB NOT = SPACE)
                     PERFORM VARYING I FROM 50 BY -1
                             UNTIL LB (I: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                END-IF
                IF   CWSEND-SCREENS = SPACES
                OR   CWSEND-MSG = "Execu‡Æo com erro:"
                     MOVE "b"      TO SP2-MS-ICON
                     MOVE "o"      TO SP2-MS-BUTTON
                     MOVE MSG-D    TO SP2-MS-TEXT
                     MOVE "Aviso:" TO SP2-MS-TITLE
                     MOVE 1        TO SP2-MS-LINE-CNT
                     IF   X91-PARAMETER > 1
                     AND (LB NOT = SPACE)
                         MOVE LB TO SP2-MS-TITLE
                     END-IF
                     PERFORM ACENTOS-MS THRU END-ACENTOS-MS
                     CALL SP2   USING SP2-DISPLAY-MESSAGE
                                      SP2-MESSAGE-DATA
                     MOVE 1        TO CWSEND-OPTION
                     GO TO 000-FIM
                ELSE
                     MOVE SPACES TO SIM-NAO
                     MOVE 0      TO SN
                     IF  CWSEND-OK = 'ON' OR 'on' OR 'On' OR 'oN'
                         PERFORM VARYING OK FROM 1 BY 1
                           UNTIL OK > 8
                             PERFORM VARYING S FROM 1 BY 1
                                       UNTIL S > LENGTH CWSEND-SCREEN(1)
                             IF CWSEND-SCREEN(OK)(S:3)= '~OK'  OR '~Ok '
                             OR CWSEND-SCREEN(OK)(S:4)= ' OK ' OR ' Ok '
                             OR CWSEND-SCREEN(OK)(S:4)= ' OK_' OR ' Ok_'
                                MOVE OK TO CWSEND-OPTION
                             END-IF
                             END-PERFORM
                         END-PERFORM
                     END-IF
                     IF   CWSEND-SCREENS (39: ) = SPACES
                          PERFORM VARYING I FROM 9 BY 1
                                    UNTIL I > 38
                                       OR SN = 3
                                  IF CWSEND-SCREENS (I: 4) = "~Sim"
                                                          OR "~SIM"
                                                          OR "~sim"
                                     ADD  1      TO SN
                                     MOVE "S" TO SIM-NAO (SN: 1)
                                  END-IF
                                  IF CWSEND-SCREENS (I: 2) = "~N"
                                                          OR "~n"
                                     ADD  1      TO SN
                                     MOVE "N" TO SIM-NAO (SN: 1)
                                  END-IF
                                  IF CWSEND-SCREENS (I: 9) = "~Cancelar"
                                                          OR "~CANCELAR"
                                                          OR "~cancelar"
                                     ADD  1      TO SN
                                     MOVE "C" TO SIM-NAO (SN: 1)
                                  END-IF
                          END-PERFORM
                          IF  (CWSEND-SCREEN (3) NOT = SPACES)
                          AND (SIM-NAO (3: 1)    NOT = "C")
                          OR  CWSEND-OPTION > 2
                               MOVE SPACES TO SIM-NAO
                          END-IF
                          IF   SIM-NAO = "SN " OR "NS "
                                      OR "SNC" OR "NSC"
                               MOVE "q"   TO SP2-MS-ICON
                               IF   SIM-NAO = "SN" OR "SNC"
                                    MOVE "y"   TO SP2-MS-BUTTON
                               ELSE
                                    MOVE "n"   TO SP2-MS-BUTTON
                               END-IF
                               IF   SIM-NAO (3: 1) = "C"
                                    MOVE "y" TO SP2-MS-CANCEL
                               END-IF
                               IF  CWSEND-OPTION = 2
                               AND(SIM-NAO = "SN" OR "SNC")
                                   MOVE "n" TO SP2-MS-BUTTON
                               END-IF
                               IF  CWSEND-OPTION = 2
                               AND(SIM-NAO = "NS" OR "NSC")
                                   MOVE "y" TO SP2-MS-BUTTON
                               END-IF
                               MOVE MSG-D      TO SP2-MS-TEXT
                               MOVE "DecisÆo:" TO SP2-MS-TITLE
                               MOVE 1          TO SP2-MS-LINE-CNT
                               PERFORM ACENTOS-MS THRU END-ACENTOS-MS
                               CALL SP2   USING SP2-DISPLAY-MESSAGE
                                                SP2-MESSAGE-DATA
                               INSPECT SP2-MS-REPLY
                                       CONVERTING 'sncy'
                                       to 'SNCS'
                               PERFORM VARYING I FROM 1 BY 1
                                 UNTIL I > 3
                                    OR SP2-MS-REPLY = SIM-NAO (I: 1)
                                      CONTINUE
                               END-PERFORM
                               IF  I < 3
                                   MOVE I TO CWSEND-OPTION
                               END-IF
                               MOVE SIM-NAO TO CWSEND-SCREENS (1: 3)
                               MOVE CWSEND-CHAR (CWSEND-OPTION)
                                 TO CWSEND-OPTION-CHAR
                               GO TO 000-FIM
                          END-IF
                     END-IF
                END-IF
                MOVE 5           TO CWBOXW-LINE
                MOVE SPACES      TO CWLINE-SCREENS
                MOVE 0           TO BOTOES
                                    MAIOR
                PERFORM VARYING S FROM 1 BY 1 UNTIL S > 8
                        MOVE CWSEND-CHAR   (S) TO CWLINE-CHAR   (S)
                        MOVE CWSEND-SCREEN (S) TO CWLINE-SCREEN (S)
                        MOVE 0                 TO TIL
                        PERFORM VARYING TAMANHO FROM 10 BY -1
                                UNTIL BYTE-TXT (S TAMANHO)
                                      NOT = SPACE
                                   OR CWSEND-SCREEN (S) = SPACES
                                      IF   BYTE-TXT (S TAMANHO)
                                         = X"7E"
                                          ADD 1 TO TIL
                                      END-IF
                        END-PERFORM
                        SUBTRACT TIL FROM TAMANHO
                        IF   CWLINE-SCREEN (S) NOT = SPACES
                             ADD 1 TO BOTOES
                             IF  TAMANHO > MAIOR
                                 MOVE TAMANHO TO MAIOR
                             END-IF
                        END-IF
                END-PERFORM
                MOVE 2  TO LENG
                           LENG2
                MOVE 1  TO VEZ
                MOVE 0  TO S
                PERFORM BOTOES TIMES
                   ADD     1 TO S
                   INSPECT CWLINE-SCREEN (S) (1: MAIOR)
                           CONVERTING " " TO "_"
                   IF  (LENG + MAIOR) < 76
                       ADD MAIOR TO LENG
                   END-IF
                   ADD MAIOR TO LENG2
                   IF  LENG2 > 76
                       ADD      1 TO VEZ
                       IF VEZ > 1
                          ADD      4 TO CWBOXW-VERTICAL-LENGTH
                          MOVE     MAIOR TO LENG2
                          IF VEZ > 2
                             SUBTRACT 4   FROM CWBOXW-LINE
                                               CWLINE-LINE
                          END-IF
                       END-IF
                   END-IF
                END-PERFORM
                COMPUTE CWBOXW-HORIZONTAL-LENGTH = LENG + 1
                IF   CX > LENG
                     MOVE CX TO CWBOXW-HORIZONTAL-LENGTH
                     COMPUTE CWBOXW-COLUMN
                     = ((CX - (MAIOR * BOTOES)) / 2) + 1
                ELSE
                     MOVE     2   TO CWBOXW-COLUMN
                     SUBTRACT 1 FROM CWBOXW-HORIZONTAL-LENGTH
                END-IF
                COMPUTE CWLINE-COLUMN = CWBOXW-COLUMN
                COMPUTE C             = CWBOXW-COLUMN
                COMPUTE L = CWBOXW-LINE + 2
                MOVE CWSEND-OPTION      TO CWLINE-OPTION
                IF   BOTOES = 1
                     COMPUTE CWLINE-COLUMN
                     = (CWBOXW-HORIZONTAL-LENGTH - (MAIOR + 2)) / 2
                END-IF
                MOVE CWSEND-TIMEOUT-STATUS TO CWLINE-TIMEOUT-STATUS
                perform open-panel
                MOVE LOW-VALUES      TO SP2-SD-DATA
                CALL "CWSPID" USING SP2-SD-ID "SInsert"
                COMPUTE SP2-SD-WIDTH = CX * 10
                MOVE 10       TO SP2-SD-HEIGHT
                MOVE MSG-D    TO SP2-SD-TEXT
                IF   CWLITS = "LOW"
                     INSPECT SP2-SD-TEXT
                             CONVERTING MAIUSCULAS TO MINUSCULAS
                END-IF
                IF   CWLITS = "UPP"
                     INSPECT SP2-SD-TEXT
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                END-IF
                IF   CWACCENT = "OFF"
                     INSPECT SP2-SD-TEXT
                              CONVERTING ACENTOS-850 TO ACENTOS-OFF
                ELSE
                     INSPECT SP2-SD-TEXT
                             CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                END-IF
                MOVE 1        TO SP2-SD-ROW
                MOVE 1        TO SP2-SD-COL
                MOVE 0        TO SP2-SD-FONT-ID
                CALL SP2   USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
                MOVE "S"             TO CWLINE-TYPE
                CALL "CWLINE"        USING PARAMETROS-CWLINE
                CANCEL "CWLINE"
                MOVE CWLINE-TIMEOUT-RETURN TO CWSEND-TIMEOUT-RETURN
                MOVE CWLINE-OPTION         TO CWSEND-OPTION
                MOVE CWLINE-OPTION-CHAR    TO CWSEND-OPTION-CHAR
                CALL "CWSPID" USING SP2-SD-ID "SDelete"
                CALL SP2   USING SP2-DELETE-STATIC SP2-STATIC-DEF
                CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
                IF   PROPRIETARIA NOT = LOW-VALUES
                     MOVE PROPRIETARIA TO SP2-ND-NAME
                     CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
                     CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                END-IF
           END-IF.

       000-FIM. GOBACK.

       open-panel.

           MOVE LOW-VALUES       TO SP2-WD-DATA
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           MOVE SP2-WD-NAME      TO PROPRIETARIA
           MOVE SP2-WD-WINDOW-ID TO CORRENTE-ID

           MOVE LOW-VALUES       TO SP2-WD-DATA
           MOVE "d"              TO SP2-WD-BOR-TYPE
           MOVE "CWSEND"         TO SP2-WD-NAME
                                    SP2-WD-PANEL-NAME
           COMPUTE SP2-WD-WIDTH = CWBOXW-HORIZONTAL-LENGTH  * 1
           IF   X91-PARAMETER > 1
           AND (LB NOT = SPACE)
           AND I > SP2-WD-WIDTH
               MOVE I TO SP2-WD-WIDTH
           END-IF
           MOVE  4              TO SP2-WD-HEIGHT
           MOVE 70              TO SP2-WD-ROW
           COMPUTE SP2-WD-COL = 200 - (SP2-WD-WIDTH * 2)
           DISPLAY 'CWIAEF'   UPON ENVIRONMENT-NAME
           ACCEPT   CWIAEF    FROM ENVIRONMENT-VALUE
           IF   CWIAEF = 'ON'
                SUBTRACT 30 FROM SP2-WD-ROW
                ADD  70       TO SP2-WD-COL
                DISPLAY 'CWIAEF-BOR-TYPE' UPON ENVIRONMENT-NAME
                ACCEPT   SP2-WD-BOR-TYPE  FROM ENVIRONMENT-VALUE
           ELSE
                DISPLAY 'CWSEND-BOR-TYPE' UPON ENVIRONMENT-NAME
                ACCEPT   SP2-WD-BOR-TYPE  FROM ENVIRONMENT-VALUE
           END-IF
           MOVE -1              TO SP2-WD-TITLE-ROWS
           MOVE -1              TO SP2-WD-MENU-ROWS
           IF   CWSEND-MSG (1: 1) =  "^"
                MOVE MSG-D (2: ) TO SP2-WD-TITLE
                IF  SP2-WD-TITLE(1: 1) = X"B5"
                     MOVE MSG-D (3: ) TO SP2-WD-TITLE
                END-IF
                INSPECT SP2-WD-TITLE CONVERTING X"FF" TO " "
                MOVE 2           TO SP2-WD-HEIGHT
                MOVE SPACES      TO MSG-D
           ELSE
                MOVE "Mensagem"  TO SP2-WD-TITLE
           END-IF
           IF   X91-PARAMETER > 1
           AND (LB NOT = SPACE)
               MOVE LB TO SP2-WD-TITLE
           END-IF
           MOVE CORRENTE-ID      TO SP2-WD-OWNR-ID

           IF   CWLITS = "LOW"
                INSPECT SP2-WD-TITLE CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-WD-TITLE CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
           ELSE
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-WINDOWS
           END-IF

           CALL SP2            USING SP2-OPEN-WINDOW SP2-WINDOW-DEF

           MOVE LOW-VALUES        TO SP2-PD-DATA
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE 0                 TO SP2-PD-ROW
           MOVE 0                 TO SP2-PD-COL
           MOVE "CWSEND"          TO SP2-PD-NAME
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
           MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (1)
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (2)
           MOVE SP2-KEY-F2        TO SP2-PD-CTRL-KEY (3)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (4)
           MOVE SP2-KEY-LEFT      to SP2-PD-CTRL-KEY (5)
           MOVE SP2-KEY-RIGHT     to SP2-PD-CTRL-KEY (6)
           MOVE SP2-KEY-DOWN      to SP2-PD-CTRL-KEY (7)
           MOVE SP2-KEY-UP        to SP2-PD-CTRL-KEY (8)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (9)
           CALL "CWRESE"
           CALL SP2            USING SP2-SET-PANEL-DEF SP2-PANEL-DEF
           MOVE SP2-PD-NAME       TO SP2-ND-NAME
           IF PROPRIETARIA NOT = LOW-VALUES
              CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
           END-IF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       FIM-open-panel. EXIT.

       ACENTOS-MS.

           IF   CWLITS = "LOW"
                INSPECT SP2-MS-TEXT
                        CONVERTING MAIUSCULAS TO MINUSCULAS
                INSPECT SP2-MS-TITLE
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-MS-TEXT
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                INSPECT SP2-MS-TITLE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-MS-TEXT
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
                INSPECT SP2-MS-TITLE
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT SP2-MS-TEXT
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                INSPECT SP2-MS-TITLE
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF.

       END-ACENTOS-MS.
       END PROGRAM CWSEND.

