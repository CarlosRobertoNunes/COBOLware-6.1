       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXD INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/09/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Caixa de di logo simples com at‚ 10 campos   *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 PROP-ROW          COMP-5 PIC S9(004) VALUE 0.
           05 PROP-COL          COMP-5 PIC S9(004) VALUE 0.
           05 CWINDF              PIC  X(010) VALUE SPACES.
           05 ENTER-TERMINATE     PIC  X(003) VALUE SPACES.
           05 TECLA               PIC  9(003) VALUE 0. COPY CWKEYS.
           05 MIL          COMP-X PIC  9(008) VALUE 1000.
           05 MFF                 PIC  X(003) VALUE SPACES.
           05 STATIC              PIC  X(001) VALUE X"70".
           05 ENTRY-ATTR          PIC  X(001) VALUE X"F0".
           05 DISABLE-ATTR        PIC  X(001) VALUE X"70".
           05 CURCOLOR            PIC  X(001) VALUE X"00".
           05 JANW                PIC  9(003) VALUE 0.
           05 CORRENTE-ID  COMP-5 PIC S9(004) VALUE 0.
           05 CORRENTE            PIC  X(010) VALUE SPACES.
           05 CWCASE              PIC  X(003) VALUE SPACES.
           05 CWACCENT            PIC  X(003) VALUE SPACES.
           05 CWLITS              PIC  X(003) VALUE SPACES.
           05 FIELDS              PIC  9(002) VALUE 0.
           05 FIELDS-OK           PIC  9(002) VALUE 0.
           05 L-PB                PIC  9(002) VALUE 0.
           05 C-PB                PIC  9(002) VALUE 0.
           05 m-PB                PIC  9(002) VALUE 0.
           05 PBS                 PIC  9(002) VALUE 0.
           05 Z                   PIC  9(002) VALUE 0.
           05 I                   PIC  9(002) VALUE 0.
           05 w                   PIC  9(002) VALUE 0.
           05 m                   PIC  9(002) VALUE 0.
           05 I-LAST              PIC  9(002) VALUE 0.
           05 Y                   PIC  9(002) VALUE 0.
           05 MAXCAP              PIC  9(002) VALUE 0.
           05 MAXLEN              PIC  9(002) VALUE 0.
           05 FD-ID        COMP-5 PIC S9(004) OCCURS 10.
           05 SD-ID        COMP-5 PIC S9(004) OCCURS 10.
           05 KEY-ID       COMP-5 PIC S9(004) OCCURS 10.
           05 OFF-TAB      COMP-5 PIC S9(004) OCCURS 10.
           05 CRT-STATUS          PIC  X(003) VALUE SPACES.
           05 VEZFF               PIC  9(003) VALUE 0.
           05 X91-RESULT       COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION     COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER    COMP-X PIC  9(002) VALUE 0.

       01  OFF-SETS.
           05 SIZE-FIELDS      PIC  9(004) VALUE 0.
           05 POSIT            PIC  9(001) VALUE 0.
           05 OFF-W            PIC  9(004) VALUE 0.
           05 FIELD-AREA.
              10 OCCURS 0 TO 380 DEPENDING ON SIZE-FIELDS
                               PIC  X(001).

       COPY CWSPWS.

       LINKAGE SECTION.

       COPY CWBOXD.
       01 DP PIC X.
       PROCEDURE DIVISION USING PARAMETROS-CWBOXD DP.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           CANCEL "CWAKEY"
           DISPLAY "CWENTER-TERMINATE" UPON ENVIRONMENT-NAME
           ACCEPT  ENTER-TERMINATE   FROM ENVIRONMENT-VALUE
           INSPECT ENTER-TERMINATE
                CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWFONT-STATIC-NAME"   UPON ENVIRONMENT-NAME
           ACCEPT  MFF             FROM ENVIRONMENT-VALUE
           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           CALL "CWATTR" USING STATIC ENTRY-ATTR DISABLE-ATTR CURCOLOR
           DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
           ACCEPT CWCASE FROM ENVIRONMENT-VALUE
           INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM

           COPY CWSPPD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   IF CWBOXD-PUSH-BUTTON (I)
                       MOVE 1 TO CWBOXD-SIZE (I)
                       perform varying w
                                  from length of CWBOXD-CAPTION(i)
                                    by -1
                               until w = 1
                               or CWBOXD-CAPTION(i) (w:1) not = space
                                  continue
                       end-perform
                       if w > m-pb
                          move w to m-pb
                       end-if
                   end-if
                   IF   CWBOXD-SIZE (I) NUMERIC
                   AND (CWBOXD-SIZE (I) NOT = 0)
                        IF   CWBOXD-SIZE (I) > MAXLEN
                             MOVE CWBOXD-SIZE (I) TO MAXLEN
                        END-IF
                        IF   CWBOXD-PIC (I) NOT = SPACES
                             PERFORM VARYING Y FROM 50
                                     BY -1
                                   UNTIL CWBOXD-PIC (I)(Y:1) NOT = SPACE
                                CONTINUE
                             END-PERFORM
                             IF   Y > MAXLEN
                                  MOVE Y TO MAXLEN
                             END-IF
                        END-IF
                        ADD  1                TO FIELDS
                        CALL "CWSPID" USING FD-ID (FIELDS) "FInsert"
                        IF NOT CWBOXD-PUSH-BUTTON (FIELDS)
                           CALL "CWSPID" USING SD-ID (FIELDS) "SInsert"
                        ELSE
                           IF FIELDS > 1
                              subtract 1 from fields
                              IF CWBOXD-PUSH-BUTTON (FIELDS)
                                 ADD 1 TO PBS
                              END-IF
                              add 1 to fields
                           END-IF
                        END-IF
                        PERFORM VARYING Y FROM 30 BY -1
                                UNTIL Y = 1
                                   OR CWBOXD-CAPTION(I)(Y:1) NOT = SPACE
                                      CONTINUE
                        END-PERFORM
                        IF  Y > MAXCAP
                            MOVE Y TO MAXCAP
                        END-IF
                   END-IF
           END-PERFORM

           MOVE LOW-VALUES         TO SP2-WD-DATA
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           move sp2-wd-row to prop-row
           move sp2-wd-col to prop-col
           MOVE SP2-WD-NAME        TO CORRENTE
           MOVE SP2-WD-WINDOW-ID   TO CORRENTE-ID
           IF SP2-WD-WINDOW-ID NOT = 0
              CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           END-IF

           MOVE LOW-VALUES        TO SP2-WD-DATA
           MOVE CWBOXD-COLOR(1:1) TO SP2-WD-COLR
KSAT       MOVE X'02'             TO SP2-WD-OPTIONS-3
           DISPLAY 'CWBOXD-WD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-WD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE 10                to SP2-WD-CELL-WIDTH
           MOVE 20                to SP2-WD-CELL-HEIGHT
           MOVE 08                to SP2-WD-CELL-WIDTH
           MOVE 16                to SP2-WD-CELL-HEIGHT
           MOVE "d"               TO SP2-WD-BOR-TYPE
           MOVE "CWBOXD"          TO SP2-WD-NAME
           COMPUTE SP2-WD-WIDTH = MAXCAP + MAXLEN + 5
           SUBTRACT PBS FROM FIELDS
           IF PBS NOT = 0
              IF (M-PB * (PBS + 1)) > SP2-WD-WIDTH
                 COMPUTE SP2-WD-WIDTH = (M-PB * (PBS + 1))
              END-IF
           END-IF
           COMPUTE SP2-WD-HEIGHT = FIELDS * 2
           ADD      PBS TO FIELDS
           COMPUTE SP2-WD-ROW = (CWBOXD-LINE   - 1) * 7
           COMPUTE SP2-WD-COL = (CWBOXD-COLUMN - 1) * 5
           subtract prop-row from sp2-wd-row
           subtract prop-col from sp2-wd-col
           MOVE -1              TO SP2-WD-TITLE-ROWS
           MOVE -1              TO SP2-WD-MENU-ROWS
           IF   CWBOXD-HEADER = SPACES
                MOVE "Di logo"  TO CWBOXD-HEADER
           END-IF
           IF   CWBOXD-HEADER(1: 1) = X"B5"
                MOVE CWBOXD-HEADER(2: ) TO SP2-WD-TITLE
           ELSE
                MOVE CWBOXD-HEADER      TO SP2-WD-TITLE
           END-IF

      *    INSPECT SP2-WD-TITLE CONVERTING X"FFC6" TO "  "
           INSPECT SP2-WD-TITLE CONVERTING X"FF" TO " "

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

           PERFORM VARYING JANW FROM LENGTH OF SP2-WD-TITLE
                               BY -1
                     UNTIL SP2-WD-TITLE (JANW : 1) NOT = SPACES
                   CONTINUE
           END-PERFORM
           ADD  5 TO JANW

           IF   SP2-WD-WIDTH < JANW
                MOVE JANW TO SP2-WD-WIDTH
           END-IF

           MOVE -2                TO SP2-WD-OWNR-ID
           MOVE CORRENTE-ID       TO SP2-WD-OWNR-ID
           MOVE X"10"             TO SP2-WD-MORE-OPTIONS
           DISPLAY "CWINDF" UPON ENVIRONMENT-NAME
           ACCEPT   CWINDF  FROM ENVIRONMENT-VALUE
           IF CWINDF NOT = SPACES
              CALL CWINDF USING SP2-WINDOW-DEF
                ON EXCEPTION
                   CONTINUE
              END-CALL
           END-IF
           CALL SP2            USING SP2-OPEN-WINDOW SP2-WINDOW-DEF

           MOVE LOW-VALUES        TO SP2-PD-DATA
           DISPLAY 'CWBOXD-PD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-PD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE 0                 TO SP2-PD-ROW
           MOVE 0                 TO SP2-PD-COL
           MOVE "CWBOXD"          TO SP2-PD-NAME
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
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (3)
           MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (4)
           MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (5)
           MOVE 5 TO Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               IF CWBOXD-PUSH-BUTTON (I)
               AND (FD-ID (I) NOT = 0)
               AND (CWBOXD-KEY (I) NOT = 0)
                  ADD 1 TO Y
                  PERFORM 010-SET-KEYS
                  MOVE SP2-PD-CTRL-KEY (Y) TO KEY-ID (I)
               END-IF
           END-PERFORM
pep   *    MOVE "n"               TO SP2-PD-CURS-SHOW
      *    MOVE "y"               TO SP2-PD-CURS-SHOW
           CALL "CWRESE"
           CALL SP2            USING SP2-SET-PANEL-DEF SP2-PANEL-DEF

           MOVE LOW-VALUES      TO SP2-SD-DATA
           MOVE 2               TO SP2-SD-COL
           ADD  1               TO MAXCAP
           COMPUTE SP2-SD-VAR-LEN = MAXCAP + 1
           MOVE SP2-SD-VAR-LEN  TO SP2-SD-TEXT-LEN
           COMPUTE SP2-SD-WIDTH = SP2-SD-VAR-LEN * 10
           MOVE -1              TO SP2-SD-FONT-ID

           MOVE LOW-VALUES      TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
           MOVE 0               TO Z
           MOVE 1               TO SP2-FD-ROW
           move 09              TO SP2-FD-HEIGHT
           MOVE -1              TO SP2-FD-FONT-ID
           COMPUTE SP2-FD-COL    = MAXCAP + 2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   IF  (NOT CWBOXD-PROTECTED (I))
                   AND  (CWBOXD-SIZE (I) NOT = 0)
                   AND (NOT  CWBOXD-PUSH-BUTTON (I))
                        MOVE I TO I-LAST
                   END-IF
           END-PERFORM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                                            OR Z > FIELDS
                MOVE 0          TO SP2-FD-VAR-LEN
                MOVE LOW-VALUES TO SP2-FD-OUTPUT
                                   SP2-FD-CURS-SKIP
                                   SP2-FD-TYPE
                                   SP2-FD-VAR-DATA
                                   SP2-FD-INIT-NUMS
                                   SP2-FD-SPEC-FMT
                                   SP2-FD-BLANK-ZERO
                                   SP2-FD-JUSTIFY
                MOVE "e"        TO SP2-FD-CTRL-TYPE
                MOVE ENTRY-ATTR TO SP2-FD-COLR
                MOVE CURCOLOR   TO SP2-FD-CUR-COLR
                MOVE "b"        TO SP2-FD-CURS-SHOW
                MOVE X"00"      TO SP2-FD-CURS-SKIP
                MOVE CWBOXD-SIZE (I) TO SP2-FD-FORMAT-LEN
                IF CWBOXD-SIZE (I) NOT = 0
                   ADD 1 TO Z
                   IF   CWBOXD-PIC (I) NOT = SPACES
                   IF   CWBOXD-PUSH-BUTTON (I)
                        continue
                   ELSE
                        MOVE CWBOXD-PIC (I) TO SP2-FD-VAR-DATA
                        PERFORM VARYING SP2-FD-FORMAT-LEN FROM 50
                                     BY -1
                                   UNTIL SP2-FD-VAR-DATA
                                     (SP2-FD-FORMAT-LEN: 1) NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        IF X91-PARAMETER > 1
                        AND DP = ','
                            INSPECT SP2-FD-VAR-DATA
                             (1:SP2-FD-FORMAT-LEN)
                              CONVERTING ",." TO ".,"
                        END-IF
                        IF  NOT CWBOXD-NUMERIC (I)
                            PERFORM VARYING Y FROM 1 BY 1
                                    UNTIL Y > SP2-FD-FORMAT-LEN
                               IF CWBOXD-PIC(I)(Y:1) = '9' OR 'Z' OR '*'
                                  SET CWBOXD-NUMERIC (I) TO TRUE
                               END-IF
                            END-PERFORM
                        END-IF
                   END-IF
                   END-IF
                   IF  CWBOXD-NUMERIC (I)
      *                MOVE 'y' TO SP2-FD-OUTPUT
                       MOVE 'r' TO SP2-FD-JUSTIFY
                   END-IF
                   MOVE FD-ID (I)       TO SP2-FD-ID
                   COMPUTE SP2-FD-WIDTH  = SP2-FD-FORMAT-LEN * 10
                   MOVE SIZE-FIELDS TO SP2-FD-PROG-OFF
                   COMPUTE OFF-W = SIZE-FIELDS + 1
                   MOVE OFF-W           TO OFF-TAB (I)
                   MOVE SP2-FD-FORMAT-LEN TO SP2-FD-MAX-LEN
                                             SP2-FD-INITIAL-LEN
                                             SP2-FD-VAR-LEN
                   MOVE CWBOXD-SIZE (I) TO SP2-FD-PROG-LEN
                   ADD  CWBOXD-SIZE (I) TO SIZE-FIELDS
                   MOVE CWBOXD-DATA (I) TO FIELD-AREA
                                           (OFF-W: SP2-FD-PROG-LEN)
                   IF   CWBOXD-SECURE (I)
                        MOVE "s"     TO SP2-FD-OUTPUT
                        MOVE X"00"   TO SP2-FD-CTRL-TYPE
                        MOVE "v"     TO SP2-FD-CURS-SHOW
                   END-IF
                   IF   CWBOXD-AUTO   (I)
                        MOVE "y"     TO SP2-FD-CURS-SKIP
                   END-IF
                   IF   CWBOXD-PROTECTED (I)
                        MOVE "g"          TO SP2-FD-OUTPUT
                        MOVE DISABLE-ATTR TO SP2-FD-COLR
                   ELSE
                        IF   NOT CWBOXD-PUSH-BUTTON (I)
                             MOVE I TO FIELDS-OK
                        END-IF
                   END-IF
                   IF   CWBOXD-NUMERIC (I)
                        MOVE "n"   TO SP2-FD-TYPE
                                      SP2-FD-INIT-NUMS
                        IF   CWBOXD-BLANK (I)
                             MOVE "y" TO SP2-FD-BLANK-ZERO
                        END-IF
                   END-IF
                   IF   CWBOXD-PIC (I) = SPACES
                        IF   CWBOXD-NUMERIC (I)
                             MOVE ALL "9" TO SP2-FD-VAR-DATA
                                             (1: SP2-FD-VAR-LEN)
                        ELSE
                             MOVE ALL "X" TO SP2-FD-VAR-DATA
                                             (1: SP2-FD-VAR-LEN)
                        END-IF
                   END-IF
                   IF   CWBOXD-PROTECTED (I)
                   AND  CWBOXD-SECURE (I)
                        MOVE ALL "*" TO FIELD-AREA
                                        (OFF-W: SP2-FD-MAX-LEN)
                   END-IF
                   MOVE "3"        TO SP2-FD-BOR-TYPE
                   MOVE "t"        TO SP2-FD-BOR-TYPE
                   MOVE "CWBOXD"   TO SP2-FD-NAME
                   MOVE I          TO SP2-FD-NAME (7: )
                   IF   CWCASE = "UPP"
                        MOVE "u" TO SP2-FD-CASE
                   ELSE
                        IF   CWCASE = "UPP"
                             MOVE "l" TO SP2-FD-CASE
                        END-IF
                   END-IF
                   if i = i-last
                      move 'n' to sp2-fd-curs-skip
                   end-if
                   IF   CWBOXD-PUSH-BUTTON (I)
                        IF  L-PB = 0
                            MOVE SP2-SD-ROW    TO L-PB
                            add  2             to l-pb
                        END-IF
                        IF  C-PB = 0
                            MOVE 2             TO C-PB
                        END-IF
                        MOVE LOW-VALUES        TO SP2-FD-DATA
                                                  SP2-FD-VAR-LENS
                        MOVE KEY-ID (I)        TO SP2-FD-HELP-KEY
                        move l-pb              to sp2-fd-row
                        move c-pb              to sp2-fd-col
                        MOVE X"8F"             TO SP2-FD-COLR
                        MOVE m-pb              TO SP2-FD-INITIAL-LEN
                                                  SP2-FD-VAR-LEN
                                                  SP2-FD-MAX-LEN
                        COMPUTE SP2-FD-WIDTH = (m-pb * 10) + 10
                        ADD m-pb               TO c-pb
                        ADD 2                  TO c-pb
                        MOVE CWBOXD-CAPTION(i) TO SP2-FD-VAR-DATA
                        PERFORM VARYING m FROM 1 BY 1
                                  UNTIL m > m-pb
                                     OR(SP2-FD-MNEMONIC NOT = LOW-VALUE)
                                IF   CWBOXD-CAPTION(i) (m: 1) = X"7E"
                                     ADD 1 TO m
                                      MOVE CWBOXD-CAPTION(i) (m: 1)
                                        TO SP2-FD-MNEMONIC
                                END-IF
                        END-PERFORM
                        IF   CWBOXD-ICON(I)
                             MOVE "i"    TO SP2-FD-CTRL-TYPE
                             MOVE CWBOXD-IMAGE (I)
                             TO SP2-FD-VAR-DATA (9: )
                             MOVE 58 TO SP2-FD-INITIAL-LEN
                             MOVE "f" to SP2-FD-SPEC-FMT
                             IF   CWBOXD-MICRO-ICON(I)
                                  MOVE 18 TO SP2-FD-WIDTH
                                  MOVE 10 TO SP2-FD-HEIGHT
                             END-IF
                        ELSE
                             MOVE "p" TO SP2-FD-CTRL-TYPE
                             IF  SP2-FD-MNEMONIC NOT = LOW-VALUE
                                 ADD      1   TO SP2-FD-INITIAL-LEN
                                                 SP2-FD-VAR-LEN
                                                 SP2-FD-MAX-LEN
                             END-IF
                             IF   CWLITS = "UPP"
                                  INSPECT SP2-FD-VAR-DATA
                                          (1:SP2-FD-MAX-LEN)
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                             END-IF
                             IF   CWLITS = "LOW"
                                  INSPECT SP2-FD-VAR-DATA
                                          (1:SP2-FD-MAX-LEN)
                                   CONVERTING MAIUSCULAS TO MINUSCULAS
                             END-IF
                             IF   CWACCENT = "OFF"
                                  INSPECT SP2-FD-VAR-DATA
                                  (1:SP2-FD-MAX-LEN)
                                          CONVERTING ACENTOS-850
                                                  TO ACENTOS-OFF
                             ELSE
                                 IF   MFF = "MF"
                                      INSPECT SP2-FD-VAR-DATA
                                              (1:SP2-FD-MAX-LEN)
                                          CONVERTING ACENTOS-850
                                                  TO ACENTOS-437
                                 ELSE
                                      INSPECT SP2-FD-VAR-DATA
                                              (1:SP2-FD-MAX-LEN)
                                          CONVERTING ACENTOS-850
                                                  TO ACENTOS-WINDOWS
                                 END-IF
                             END-IF
                        END-IF
                        IF   CWBOXD-PROTECTED (I)
                             MOVE "g"          TO SP2-FD-OUTPUT
                        end-if
                   end-if
                   MOVE Z               TO SP2-FD-FLD-NUM
                                           SP2-FD-TAB-NUM
                   CALL SP2     USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   IF   NOT CWBOXD-PUSH-BUTTON (I)
                        MOVE SP2-FD-ROW TO SP2-SD-ROW
                        MOVE SD-ID (I)  TO SP2-SD-ID
                        MOVE CWBOXD-CAPTION(I) TO SP2-SD-TEXT(1:MAXCAP)
                        IF   CWACCENT = "OFF"
                             INSPECT SP2-SD-TEXT (1: MAXCAP)
                                             CONVERTING ACENTOS-850
                                                     TO ACENTOS-OFF
                        ELSE
                             INSPECT SP2-SD-TEXT (1: MAXCAP)
                                             CONVERTING ACENTOS-850
                                                     TO ACENTOS-WINDOWS
                        END-IF
                        IF   CWLITS = "UPP"
                             INSPECT SP2-SD-TEXT (1: MAXCAP)
                                             CONVERTING MINUSCULAS
                                                     TO MAIUSCULAS
                        ELSE
                             IF   CWLITS = "LOW"
                                  INSPECT SP2-SD-TEXT (1: MAXCAP)
                                                  CONVERTING MAIUSCULAS
                                                          TO MINUSCULAS
                             END-IF
                        END-IF
                        CALL SP2   USING SP2-SET-STATIC-DEF
                                         SP2-STATIC-DEF
                   END-IF
                   ADD 2 TO SP2-FD-ROW
                END-IF
           END-PERFORM

           IF SIZE-FIELDS > 0
              IF   CWACCENT = "OFF"
                   INSPECT FIELD-AREA (1: SIZE-FIELDS)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-OFF
              ELSE
                   INSPECT FIELD-AREA (1: SIZE-FIELDS)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-WINDOWS
              END-IF
              CALL SP2   USING SP2-SET-PANEL-FIELDS FIELD-AREA
                               (1: SIZE-FIELDS)
           END-IF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           PERFORM UNTIL SP2-CD-KEY = SP2-KEY-ESC
                      OR(SP2-CD-KEY = SP2-KEY-ENTER
                      AND SP2-CD-NEXT-FLD-NUM = FIELDS-OK)
                      OR(SP2-CD-KEY = SP2-KEY-ENTER
                      AND ENTER-TERMINATE = 'ON')
                   CALL "CWCRTS" USING "G" CRT-STATUS
                   IF   CRT-STATUS = X"FFFFFF"
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        ADD  1           TO VEZFF
                        IF  VEZFF = 2
                            STOP RUN
                        END-IF
                   ELSE
                        CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                        MOVE 0 TO VEZFF
                        IF   SP2-CD-KEY = SP2-KEY-CLOSE
                             MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        END-IF
                   END-IF
                   IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
                   OR                SP2-KEY-APP-CLOSE
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        CALL "CWCRTS" USING "S" X"FFFFFF"
                   END-IF
                   IF (SP2-CD-KEY NOT = SP2-KEY-ENTER)
                   AND(SP2-CD-KEY NOT = SP2-KEY-DOWN)
                   AND(SP2-CD-KEY NOT = SP2-KEY-UP)
                   AND(SP2-CD-KEY NOT = ZERO)
                      PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 10
                           IF SP2-CD-KEY = KEY-ID (Y)
                           AND CWBOXD-PUSH-BUTTON (Y)
                              CALL "CWAKEY" USING CWBOXD-KEY(Y) MIL
                              EXIT PERFORM
                           END-IF
                      END-PERFORM
                      EXIT PERFORM
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-ENTER OR SP2-KEY-DOWN)
                   AND SP2-CD-NEXT-FLD-NUM < FIELDS-OK
                       COMPUTE I = SP2-CD-NEXT-FLD-NUM + 1
                       MOVE FD-ID (I) TO SP2-CD-NEXT-FLD-ID
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-UP)
                   AND (SP2-CD-NEXT-FLD-NUM > 1)
                       COMPUTE I = SP2-CD-NEXT-FLD-NUM - 1
                       MOVE FD-ID (I) TO SP2-CD-NEXT-FLD-ID
                   END-IF
           END-PERFORM
           IF SP2-CD-KEY = SP2-KEY-ESC
              SET CWBOXD-CANCELED TO TRUE
           ELSE
              SET CWBOXD-CONTINUE TO TRUE
           END-IF
           IF SIZE-FIELDS > 0
              IF   CWACCENT =  "OFF"
                   INSPECT FIELD-AREA (1: SIZE-FIELDS)
                                      CONVERTING ACENTOS-WINDOWS
                                              TO ACENTOS-OFF
              ELSE
                   INSPECT FIELD-AREA (1: SIZE-FIELDS)
                                      CONVERTING ACENTOS-WINDOWS
                                              TO ACENTOS-850
              END-IF
           END-IF

           PERFORM UNTIL FIELDS = 0
              IF   FD-ID (FIELDS) NOT = 0
                   CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   MOVE OFF-TAB (FIELDS)      TO OFF-W
                   MOVE CWBOXD-SIZE (FIELDS)  TO I
                   IF NOT CWBOXD-PROTECTED (FIELDS)
                      MOVE FIELD-AREA(OFF-W:I) TO CWBOXD-DATA(FIELDS)
                   END-IF
                   MOVE FD-ID (FIELDS)        TO SP2-FD-ID
                   CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   CALL "CWSPID" USING FD-ID (FIELDS) "FDelete"
                   MOVE 0 TO FD-ID (FIELDS)
              END-IF
              IF   SD-ID (FIELDS) NOT = 0
                   MOVE SD-ID (FIELDS) TO SP2-SD-ID
                   CALL SP2   USING SP2-DELETE-STATIC SP2-STATIC-DEF
                   CALL "CWSPID" USING SD-ID (FIELDS) "SDelete"
                   MOVE 0 TO SD-ID (FIELDS)
              END-IF
              MOVE 0 TO CWBOXD-SIZE (FIELDS)
              SUBTRACT 1 FROM FIELDS
           END-PERFORM

           MOVE 10     TO CWBOXD-LINE
                          CWBOXD-COLUMN
           MOVE SPACES TO CWBOXD-HEADER
           MOVE 62     TO CWBOXD-COLOR
           CALL SP2   USING SP2-CLOSE-WINDOW   SP2-WINDOW-DEF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       000-99-FIM. GOBACK.

       010-SET-KEYS.

           MOVE CWBOXD-KEY (I) TO TECLA

           EVALUATE TRUE
              WHEN TAB
                   MOVE SP2-KEY-TAB        TO SP2-PD-CTRL-KEY (Y)
              WHEN ENTER-KEY
                   MOVE SP2-KEY-ENTER      TO SP2-PD-CTRL-KEY (Y)
              WHEN ESC
                   MOVE SP2-KEY-ESC        TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-Q
                   MOVE SP2-KEY-ALT-Q      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-W
                   MOVE SP2-KEY-ALT-W      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-E
                   MOVE SP2-KEY-ALT-E      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-R
                   MOVE SP2-KEY-ALT-R      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-T
                   MOVE SP2-KEY-ALT-T      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-Y
                   MOVE SP2-KEY-ALT-Y      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-U
                   MOVE SP2-KEY-ALT-U      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-I
                   MOVE SP2-KEY-ALT-I      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-O
                   MOVE SP2-KEY-ALT-O      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-P
                   MOVE SP2-KEY-ALT-P      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-A
                   MOVE SP2-KEY-ALT-A      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-S
                   MOVE SP2-KEY-ALT-S      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-D
                   MOVE SP2-KEY-ALT-D      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F
                   MOVE SP2-KEY-ALT-F      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-G
                   MOVE SP2-KEY-ALT-G      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-H
                   MOVE SP2-KEY-ALT-H      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-J
                   MOVE SP2-KEY-ALT-J      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-K
                   MOVE SP2-KEY-ALT-K      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-L
                   MOVE SP2-KEY-ALT-L      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-Z
                   MOVE SP2-KEY-ALT-Z      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-X
                   MOVE SP2-KEY-ALT-X      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-C
                   MOVE SP2-KEY-ALT-C      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-V
                   MOVE SP2-KEY-ALT-V      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-B
                   MOVE SP2-KEY-ALT-B      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-N
                   MOVE SP2-KEY-ALT-N      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-M
                   MOVE SP2-KEY-ALT-M      TO SP2-PD-CTRL-KEY (Y)
              WHEN F1
                   MOVE SP2-KEY-F1         TO SP2-PD-CTRL-KEY (Y)
              WHEN F2
                   MOVE SP2-KEY-F2         TO SP2-PD-CTRL-KEY (Y)
              WHEN F3
                   MOVE SP2-KEY-F3         TO SP2-PD-CTRL-KEY (Y)
              WHEN F4
                   MOVE SP2-KEY-F4         TO SP2-PD-CTRL-KEY (Y)
              WHEN F5
                   MOVE SP2-KEY-F5         TO SP2-PD-CTRL-KEY (Y)
              WHEN F6
                   MOVE SP2-KEY-F6         TO SP2-PD-CTRL-KEY (Y)
              WHEN F7
                   MOVE SP2-KEY-F7         TO SP2-PD-CTRL-KEY (Y)
              WHEN F8
                   MOVE SP2-KEY-F8         TO SP2-PD-CTRL-KEY (Y)
              WHEN F9
                   MOVE SP2-KEY-F9         TO SP2-PD-CTRL-KEY (Y)
              WHEN F10
                   MOVE SP2-KEY-F10        TO SP2-PD-CTRL-KEY (Y)
              WHEN PAGE-UP
                   MOVE SP2-KEY-PGUP       TO SP2-PD-CTRL-KEY (Y)
              WHEN PAGE-DOWN
                   MOVE SP2-KEY-PGDN       TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F1
                   MOVE SP2-KEY-SHIFT-F1   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F2
                   MOVE SP2-KEY-SHIFT-F2   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F3
                   MOVE SP2-KEY-SHIFT-F3   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F4
                   MOVE SP2-KEY-SHIFT-F4   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F5
                   MOVE SP2-KEY-SHIFT-F5   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F6
                   MOVE SP2-KEY-SHIFT-F6   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F7
                   MOVE SP2-KEY-SHIFT-F7   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F8
                   MOVE SP2-KEY-SHIFT-F8   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F9
                   MOVE SP2-KEY-SHIFT-F9   TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F10
                   MOVE SP2-KEY-SHIFT-F10  TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F1
                   MOVE SP2-KEY-ALT-F1     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F2
                   MOVE SP2-KEY-ALT-F2     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F3
                   MOVE SP2-KEY-ALT-F3     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F4
                   MOVE SP2-KEY-ALT-F4     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F5
                   MOVE SP2-KEY-ALT-F5     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F6
                   MOVE SP2-KEY-ALT-F6     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F7
                   MOVE SP2-KEY-ALT-F7     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F8
                   MOVE SP2-KEY-ALT-F8     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F9
                   MOVE SP2-KEY-ALT-F9     TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F10
                   MOVE SP2-KEY-ALT-F10    TO SP2-PD-CTRL-KEY (Y)
              WHEN CONTROL-PAGE-DOWN
                   MOVE SP2-KEY-CTRL-PGDN  TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-1
                   MOVE SP2-KEY-ALT-1      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-2
                   MOVE SP2-KEY-ALT-2      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-3
                   MOVE SP2-KEY-ALT-3      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-4
                   MOVE SP2-KEY-ALT-4      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-5
                   MOVE SP2-KEY-ALT-5      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-6
                   MOVE SP2-KEY-ALT-6      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-7
                   MOVE SP2-KEY-ALT-7      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-8
                   MOVE SP2-KEY-ALT-8      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-9
                   MOVE SP2-KEY-ALT-9      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-0
                   MOVE SP2-KEY-ALT-0      TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-TRACE
                   MOVE SP2-KEY-ALT-MINUS  TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-EQUAL
                   MOVE SP2-KEY-ALT-EQUAL  TO SP2-PD-CTRL-KEY (Y)
              WHEN CONTROL-PAGE-UP
                   MOVE SP2-KEY-CTRL-PGUP  TO SP2-PD-CTRL-KEY (Y)
              WHEN F11
                   MOVE SP2-KEY-F11        TO SP2-PD-CTRL-KEY (Y)
              WHEN F12
                   MOVE SP2-KEY-F12        TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F11
                   MOVE SP2-KEY-SHIFT-F11  TO SP2-PD-CTRL-KEY (Y)
              WHEN SHIFT-F12
                   MOVE SP2-KEY-SHIFT-F12  TO SP2-PD-CTRL-KEY (Y)
              WHEN CONTROL-F11
                   MOVE SP2-KEY-CTRL-F11   TO SP2-PD-CTRL-KEY (Y)
              WHEN CONTROL-F12
                   MOVE SP2-KEY-CTRL-F12   TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F11
                   MOVE SP2-KEY-ALT-F11    TO SP2-PD-CTRL-KEY (Y)
              WHEN ALT-F12
                   MOVE SP2-KEY-ALT-F12    TO SP2-PD-CTRL-KEY (Y)
           END-EVALUATE.

       010-99-FIM. EXIT.

       END PROGRAM CWBOXD.
