      *           CALL "CWSPID"   USING MSG-ID "FInsert"                COBOLware
      *    CALL SP2          USING SP2-OPEN-WINDOW                      COBOLware
      *    CALL SP2    USING SP2-RESERVE-MEMORY SP2-NULL-PARM           COBOLware
      *    CALL SP2    USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF           COBOLware
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPROC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/09/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Janela de progresso ou processo              *
                      *  com button "Cancelar"                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 NEWMSG                   PIC  9(001) VALUE 0.
           05 PROP-ROW    COMP-5 PIC S9(004) VALUE 0.
           05 PROP-COL    COMP-5 PIC S9(004) VALUE 0.
           05 FECHANDO           PIC  9(001) VALUE 0.
           05 VEZ                PIC  9(018) VALUE 0.
           05 CORRENTE-ID COMP-5 PIC S9(004) VALUE 0.
           05 CORRENTE           PIC  X(008) VALUE SPACES.
           05 WS-MSG             PIC  X(046) VALUE SPACES.
           05 SV-MSG             PIC  X(046) VALUE SPACES.
           05 CWLITS             PIC  X(003) VALUE SPACES.
           05 CWACCENT           PIC  X(003) VALUE SPACES.
           05 L                  PIC  9(003) VALUE ZERO.
           05 Y                  PIC  9(003) VALUE ZERO.
           05 IDS.
              10 PERC-ID        COMP-5 PIC S9(004) VALUE 0.
              10 MSG-ID         COMP-5 PIC S9(004) VALUE 0.
              10 I                     PIC  9(003) VALUE ZERO.
              10 P                     PIC  9(003) VALUE ZERO.
              10 DIR                   PIC  9(001) VALUE ZERO.
              10 PA                    PIC  9(005) VALUE ZERO.
              10 PERC                  PIC  9(005) VALUE 0.
              10 OPCAO                 PIC  9(001) VALUE 0.
           05 BARRA-PROGRESSO.
              10 PERC-MSG              PIC  X(046) VALUE SPACES.
              10 PERC-TXT.
                 15 PERC-ED            PIC     ZZ9 VALUE 0.
                 15 FILLER             PIC  X(002) VALUE "%".
           05 VAZIO                    PIC  X(001) VALUE X"70".
           05 CHEIO                    PIC  X(001) VALUE X"1F".
           05 COLOR-STATUS      PIC  X      VALUE '0'.
              88 COLOR-ON                   VALUE '1'.
              88 COLOR-OFF                  VALUE '0' '3'.
              88 COLOR-CLOSED               VALUE '3'.

       COPY SP2.

       LINKAGE SECTION.

       COPY CWPROC.

       PROCEDURE DIVISION USING PARAMETROS-CWPROC.

       000-INICIO.

           ON 1
              INITIALIZE IDS.

           ON 2
              DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
              ACCEPT CWLITS FROM ENVIRONMENT-VALUE
              INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
              ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
              INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS.

           EVALUATE TRUE
                    WHEN CWPROC-SHOW
                         IF  PERC-ID = 0
                             PERFORM 100-OPEN  THRU 100-99-FIM
                         END-IF
                         PERFORM 200-SHOW  THRU 200-99-FIM
                    WHEN CWPROC-CLOSE
                         PERFORM 300-CLOSE THRU 300-99-FIM
           END-EVALUATE.

       000-99-FIM. GOBACK.

       100-OPEN.

           MOVE LOW-VALUES         TO SP2-WD-DATA
key        CALL SP2             USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           MOVE SP2-WD-NAME        TO CORRENTE
key        MOVE SP2-WD-WINDOW-ID   TO CORRENTE-ID
           move sp2-wd-row         to prop-row
           move sp2-wd-col         to prop-col
           INSPECT CWPROC-TEXTS CONVERTING X"00" TO SPACE

           MOVE LOW-VALUES      TO SP2-WD-DATA
KSAT       MOVE X'02'           TO SP2-WD-OPTIONS-3
           DISPLAY 'CWPROC-WD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-WD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE 08              to SP2-WD-CELL-WIDTH
           move 16              to SP2-WD-CELL-HEIGHT
>          MOVE "f"             TO SP2-WD-BOR-TYPE
           MOVE "d"             TO SP2-WD-BOR-TYPE
           MOVE "CWPROC"        TO SP2-WD-NAME
*CEDAE*    MOVE 51              TO SP2-WD-WIDTH
           MOVE 52              TO SP2-WD-WIDTH
           MOVE CWPROC-MSG      TO WS-MSG
           INSPECT WS-MSG CONVERTING MINUSCULAS
                                  TO MINUSCULAS
           IF  WS-MSG = "OFF"
               MOVE 0           TO SP2-WD-HEIGHT
               MOVE SPACES      TO CWPROC-MSG
               MOVE 50          TO SP2-WD-WIDTH
           ELSE
               MOVE 4           TO SP2-WD-HEIGHT
           END-IF
           COMPUTE SP2-WD-ROW = (CWPROC-LINE   - 1) * 7
           COMPUTE SP2-WD-COL = (CWPROC-COLUMN - 1) * 5
           MOVE -1              TO SP2-WD-TITLE-ROWS
           MOVE -1              TO SP2-WD-MENU-ROWS
           IF   CWPROC-HEADER = SPACES
                MOVE "Processamento" TO CWPROC-HEADER
           END-IF
           MOVE SPACE           TO SP2-WD-TITLE (1: 1)
           If   CWPROC-HEADER(1: 1) = X"B5"
                MOVE CWPROC-HEADER(2:) TO SP2-WD-TITLE
           ELSE
                MOVE CWPROC-HEADER     TO SP2-WD-TITLE
           END-IF
           INSPECT SP2-WD-TITLE CONVERTING X"FFC6" TO "  "
           IF CORRENTE = 'CWMENU'
              MOVE CORRENTE-ID  TO SP2-WD-OWNR-ID
              subtract prop-row from sp2-wd-row
              subtract prop-col from sp2-wd-col
           END-IF
           MOVE X"10"           TO SP2-WD-MORE-OPTIONS

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
           CALL SP2          USING SP2-OPEN-WINDOW
                                   SP2-WINDOW-DEF

           MOVE LOW-VALUES        TO SP2-PD-DATA
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE 0                 TO SP2-PD-ROW
           MOVE 0                 TO SP2-PD-COL
           MOVE "CWPROC"          TO SP2-PD-NAME
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (1)
           MOVE SP2-KEY-F2        TO SP2-PD-CTRL-KEY (2)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (3)
           MOVE "n"               TO SP2-PD-CURS-SHOW
           DISPLAY 'CWPROC-PD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-PD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           CALL SP2    USING SP2-RESERVE-MEMORY SP2-NULL-PARM
           CALL SP2    USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF

           IF  WS-MSG NOT = "OFF"
               MOVE LOW-VALUES    TO SP2-FD-DATA
                                     SP2-FD-VAR-LENS
               IF MSG-ID =  0
                  CALL "CWSPID"   USING MSG-ID "FInsert"
               END-IF
               MOVE MSG-ID        TO SP2-FD-ID
               MOVE 50            TO SP2-FD-PROG-OFF
               MOVE 18            TO SP2-FD-COL
               MOVE 03            TO SP2-FD-ROW
               MOVE "t"           TO SP2-FD-BOR-TYPE
               MOVE 14            TO SP2-FD-HEIGHT
               MOVE "p"           TO SP2-FD-CTRL-TYPE
               MOVE "~Cancelar"   TO SP2-FD-VAR-DATA
               MOVE "C"           TO SP2-FD-MNEMONIC
               MOVE 9             TO SP2-FD-VAR-LEN
                                     SP2-FD-VAR-LEN
                                     SP2-FD-INITIAL-LEN
               MOVE 100           TO SP2-FD-WIDTH
               MOVE SP2-KEY-F2    TO SP2-FD-HELP-KEY
               MOVE -1            TO SP2-FD-FONT-ID
=>             move "s" TO SP2-FD-PROG-CTRL
pd             MOVE "y" TO SP2-FD-PROG-CTRL
               CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           PERFORM 320-UPDATE-MSG THRU 320-99-FIM
           MOVE LOW-VALUES      TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
           IF PERC-ID = 0
              CALL "CWSPID" USING PERC-ID "FInsert"
           END-IF
           MOVE PERC-ID         TO SP2-FD-ID
           IF  WS-MSG = "OFF"
               MOVE 0           TO SP2-FD-ROW
           ELSE
               MOVE 1           TO SP2-FD-ROW
           END-IF
           MOVE 10              TO SP2-FD-HEIGHT
           MOVE -1              TO SP2-FD-FONT-ID
           MOVE VAZIO           TO SP2-FD-COLR
           MOVE 1               TO SP2-FD-COL
           MOVE 460             TO SP2-FD-WIDTH
           MOVE 490             TO SP2-FD-WIDTH
CEDAE      MOVE 500             TO SP2-FD-WIDTH
           MOVE 50              TO SP2-FD-VAR-LEN
==>                                SP2-FD-PROG-LEN
                                   SP2-FD-MAX-LEN
                                   SP2-FD-INITIAL-LEN
           MOVE 0               TO PERC-ED
                                   PA P
           MOVE X"00"           TO SP2-FD-CTRL-TYPE
           MOVE ALL "X" TO SP2-FD-VAR-DATA (1: SP2-FD-VAR-LEN)
           IF   CWPROC-EMPTY = 0
                MOVE SPACES TO PERC-TXT
                PERFORM VARYING L FROM LENGTH OF BARRA-PROGRESSO
                         BY -1 UNTIL L = 0
                              OR (BARRA-PROGRESSO(L:1) NOT = SPACE)
                        CONTINUE
                END-PERFORM
                ADD 2 TO L
           ELSE
                MOVE "   %" TO PERC-TXT
           END-IF
           IF  WS-MSG = "OFF"
               SUBTRACT 1 FROM SP2-FD-COL
           END-IF
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           CALL SP2   USING SP2-SET-PANEL-FIELDS BARRA-PROGRESSO
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           DISPLAY 'CWPROC' UPON ENVIRONMENT-NAME
           DISPLAY 'ON'     UPON ENVIRONMENT-VALUE
           MOVE 9 TO FECHANDO.

       100-99-FIM. EXIT.

       200-SHOW.

           IF   CWPROC-MSG NOT = PERC-MSG
                MOVE 1          TO NEWMSG
           END-IF

           IF CORRENTE = 'CWMENU'
           OR FECHANDO = 9
              IF FECHANDO = 9
                 MOVE 0 TO FECHANDO
              END-IF
              MOVE "CWPROC" TO SP2-ND-NAME CORRENTE
              CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
           END-IF

           IF   CWPROC-MSG NOT = SV-MSG
                PERFORM 320-UPDATE-MSG THRU 320-99-FIM
           END-IF

           IF   CWPROC-EMPTY = 0
                ADD 1 TO VEZ
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LENGTH OF VEZ
                        IF VEZ (Y:1) NOT = '0'
                           MOVE VEZ (Y:) TO BARRA-PROGRESSO (L:)
                           EXIT PERFORM
                        END-IF
                END-PERFORM
                ADD 1 TO P
                IF  P > 50
                    IF  DIR = 0
                        MOVE 1 TO DIR
                    ELSE
                        MOVE 0 TO DIR
                    END-IF
                    MOVE 1 TO P
                END-IF
           ELSE
                COMPUTE PERC    = CWPROC-FULL / CWPROC-EMPTY * 100
                MOVE PERC TO PERC-ED
                IF  PERC > 2 AND PERC < 101
                    COMPUTE P = PERC / 2
                END-IF
           END-IF

           IF  P NOT = PA
           AND P > 0
               MOVE P          TO PA
               MOVE LOW-VALUES TO SP2-FD-DATA
               MOVE PERC-ID    TO SP2-FD-ID
               IF  DIR = 0
                   MOVE 0  TO SP2-FD-PROG-OFF
                   MOVE P  TO SP2-FD-PROG-LEN
               ELSE
                   COMPUTE SP2-FD-PROG-OFF = P - 1
                   COMPUTE SP2-FD-PROG-LEN = 50 - P
               END-IF
               MOVE SP2-COLR-HIGHLIGHT TO SP2-FD-COLR
               MOVE CHEIO              TO SP2-FD-COLR
               CALL SP2   USING SP2-MARK-FIELD SP2-FIELD-DEF
           END-IF

           IF  WS-MSG NOT = "OFF"
               MOVE LOW-VALUES TO SP2-CD-DATA
               MOVE "k"        TO SP2-CD-WAIT-SW
               MOVE "CWPROC"   TO SP2-CD-NEXT-PANEL
               CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
               SET CWPROC-CONTINUE TO TRUE
               IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
               OR   SP2-KEY-APP-CLOSE
                    SET CWPROC-CANCELED TO TRUE
                    CALL "CWCRTS" USING "S" X"FFFFFF"
               ELSE
                    IF  (SP2-CD-KEY NOT = -1)
                    AND (SP2-CD-KEY NOT = 0)
                         MOVE 2 TO OPCAO
                         MOVE 0 TO I
                         IF  CWPROC-QUESTION = SPACES
                             MOVE "Cancelar"    TO CWPROC-QUESTION
                             MOVE CWPROC-HEADER TO CWPROC-QUESTION (10:)
                             PERFORM VARYING I FROM 50 BY -1 UNTIL I = 1
                                     OR CWPROC-QUESTION (I: 1)
                                                           NOT = SPACE
                                       CONTINUE
                             END-PERFORM
                             ADD 2 TO I
                             MOVE "?" TO CWPROC-QUESTION (I: )
                         END-IF
                         EXEC COBOLware SEND
                              MESSAGE CWPROC-QUESTION
                              CAPTION(1) "_~Sim_"
                              CAPTION(2) "_~NÆo_"
                              OPTION ;OPCAO
                         END-EXEC
                         IF  I NOT = 0
                             MOVE SPACES TO CWPROC-QUESTION
                         END-IF
                         IF  OPCAO = 1
                             SET CWPROC-CANCELED TO TRUE
                             PERFORM 300-CLOSE THRU 300-99-FIM
                         END-IF
                    END-IF
               END-IF
           END-IF

           IF (CWPROC-FULL NOT < CWPROC-EMPTY)
           AND CWPROC-FULL > 0
               PERFORM 300-CLOSE THRU 300-99-FIM
               MOVE 1 TO FECHANDO
           END-IF

           IF CORRENTE = 'CWMENU'
           OR FECHANDO = 1
              MOVE CORRENTE TO SP2-ND-NAME
              CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
              IF FECHANDO = 1
                 CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
              END-IF
              MOVE 0 TO FECHANDO
           END-IF.

           MOVE 0 TO NEWMSG.

       200-99-FIM. EXIT.

       300-CLOSE.

           DISPLAY 'CWPROC' UPON ENVIRONMENT-NAME
           DISPLAY 'OFF'    UPON ENVIRONMENT-VALUE
           MOVE 0 TO P VEZ

           IF    PERC-ID NOT = 0
                 MOVE "CWPROC"    TO SP2-ND-NAME
                 CALL SP2      USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
                 MOVE 0           TO CWPROC-FULL
                                     CWPROC-EMPTY
                 MOVE 15          TO CWPROC-LINE
                 MOVE 15          TO CWPROC-COLUMN
                 MOVE SPACES      TO CWPROC-HEADER
                                     CWPROC-MSG
                                     CWPROC-QUESTION
                 MOVE PERC-ID     TO SP2-FD-ID
                 CALL SP2      USING SP2-DELETE-FIELD SP2-FIELD-DEF
                 CALL "CWSPID" USING PERC-ID "FDelete"
                 MOVE 0           TO PERC-ID
                 IF  MSG-ID NOT = 0
                     MOVE MSG-ID      TO SP2-FD-ID
                     CALL SP2      USING SP2-DELETE-FIELD SP2-FIELD-DEF
                     CALL "CWSPID" USING MSG-ID "FDelete"
                     MOVE 0 TO MSG-ID
                 END-IF
                 CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           END-IF.


       300-99-FIM. EXIT.

       320-UPDATE-MSG.

           MOVE CWPROC-MSG TO PERC-MSG SV-MSG

           IF   CWLITS = "LOW"
                INSPECT PERC-MSG     CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT PERC-MSG     CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT PERC-MSG     CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
           ELSE
                INSPECT PERC-MSG CONVERTING ACENTOS-850
                                         TO ACENTOS-WINDOWS
           END-IF.

       320-99-FIM. EXIT.
       END PROGRAM CWPROC.
