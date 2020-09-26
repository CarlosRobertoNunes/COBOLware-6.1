       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWKBDC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/04/1996.
       SECURITY.      *************************************************
                      *                                               *
                      * Le o teclado substituindo a CBL_READ_KBD_CHAR *
                      * possibilitando o reconhecimento de teclas     *
                      * Versao para Windows SP2                       *
                      * Uso interno do COBOLware                      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  DIGITAVEIS.
           05 PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 PIC X(10) VALUE "1234567890".
           05 PIC X(19) VALUE "!$%&*()-+<>[]'/?,.".
           05 PIC X(01) VALUE '"'.
           05 PIC X(05) VALUE "{}_\ ".
           05 PIC X(51) VALUE
              "ÁÉÍÓÚÝáéíóúýÂÊÎÔÛâêîôûÀÈÌÒÙàèìòùÃÕÑãõñÄÏÖÜäëïöüÿÇçË".

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CD-KEY.
              10 CD-KEY-X PIC S9(4) COMP-5.
           05 TECLA-MS               PIC  9(003) VALUE 0. COPY CWKEYS.
           05 MENU-LIGADO            PIC  9(001) VALUE 0.
           05 GUIA            COMP-5 PIC S9(004) VALUE 0.
           05 BARRA-H         COMP-5 PIC S9(004) VALUE 0.
           05 BARRA-V         COMP-5 PIC S9(004) VALUE 0.
           05 BARRA-HD        COMP-5 PIC S9(004) VALUE 0.
           05 BARRA-VD        COMP-5 PIC S9(004) VALUE 0.
           05 DATA-AREA.
              10 LIXO                PIC  X(001) VALUE SPACE.
              10 BARRA-AREA.
                 15 BARRA-HN         PIC  9(005) VALUE 0.
                 15 BARRA-VN         PIC  9(005) VALUE 0.
           05 BARRA-SAVE.
              10 BARRA-HNS           PIC  9(005) VALUE 0.
              10 BARRA-VNS           PIC  9(005) VALUE 0.
           05 K               COMP-X PIC  9(002) VALUE 0.
           05 D               COMP-X PIC  9(002) VALUE 0.
           05 SEGUNDOS        COMP-X PIC  9(002) VALUE 0.
           05 VEZFF                  PIC  9(003) VALUE 0.
           05 CWACCENT               PIC  X(003) VALUE SPACES.
           05 CWCASE                 PIC  X(003) VALUE SPACES.
           05 PIC3                   PIC  9(003) VALUE 0.
           05 ACENTO                 PIC  X(001) VALUE SPACE.
           05 KEY-BUFFER             PIC  X(009) VALUE SPACES.
           05 X91-RESULT      COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION    COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER   COMP-X PIC  9(002) VALUE 0.
           05 LX                     PIC  9(002) VALUE 0.
           05 CX                     PIC  9(002) VALUE 0.
           05 ECHOKEY                PIC  X(003) VALUE SPACES.
           05 KEY-STATUS.
              10 KEY-TYPE            PIC  X(001).
              10 KEY-CODE-1          PIC  9(002) COMP-X.
              10 DATA-ITEM.
                 15 KEY-CODE-2       PIC  9(002) COMP-X.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC  X(001) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC  X(001) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 1.
           05 MIL              COMP-X PIC  9(008) VALUE 1000.

       COPY CWSPWS.
       COPY CWGETL.

       LINKAGE SECTION.

       01  CURPOS.
           05 CURPOS-LIN PIC  9(002).
           05 CURPOS-COL PIC  9(002).
       01  REDEFINES CURPOS.
           05 CURPOS-HP  PIC  9(004) COMP-X.
           05 CURPOS-VP  PIC  9(004) COMP-X.

       01  CARACTER      PIC  X(001).
       01  TECLA-EDIT    PIC  9(003). COPY CWEDIT.
       01  LK-STATUS     PIC  X(003).
       01  TECLA-SP2     PIC S9(004) COMP-5.

       PROCEDURE DIVISION USING CURPOS CARACTER  TECLA-EDIT
                                       LK-STATUS TECLA-SP2.

       000-INICIO.

           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           IF   CURPOS = "9797" OR "9696"
                IF  CURPOS = "9797"
                    MOVE SP2-MOUSE-LOCATE TO SP2-NP-RET-CODE
                ELSE
                    MOVE SP2-MOUSE-ARROW  TO SP2-NP-RET-CODE
                END-IF
                CALL SP2   USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM
                GOBACK
           END-IF
           MOVE 0 TO GUIA
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER > 2
           AND  TECLA-EDIT > 990
                IF   TECLA-EDIT = 999
                     MOVE CURPOS-HP TO BARRA-HN
                     MOVE CURPOS-VP TO BARRA-VN
                END-IF
                MOVE 0         TO TECLA-EDIT
                GOBACK
           END-IF

           IF   CURPOS = "9999"
           AND  MENU-LIGADO = 0
                MOVE LOW-VALUES   TO SP2-MD-DATA
                MOVE "CWKBDC"     TO SP2-MD-NAME
                MOVE 1            TO SP2-MDO-ID   (1) MENU-LIGADO
                                     SP2-MD-OPTION-CNT
                MOVE "~Sair"      TO SP2-MDO-TEXT (1)
                COMPUTE SP2-MD-VAR-LEN = SP2-MD-OPTION-CNT *
                  (SP2-MD-OPTN-LEN + SP2-MD-OPTC-LEN + SP2-MD-OPTV-LEN)
                CALL SP2   USING SP2-SET-MENU-DEF SP2-MENU-DEF
                CALL "CWUSER" USING X"4D"
           END-IF

           IF   CURPOS-LIN = 99
                MOVE LOW-VALUES      TO SP2-FD-DATA
                                        SP2-FD-VAR-LENS
                IF   BARRA-VD NOT = 0
                     MOVE BARRA-VD TO BARRA-V
                     MOVE 0        TO BARRA-VD
                ELSE
                     IF   BARRA-V = 0
                          CALL "CWSPID" USING BARRA-V "FInsert"
                     END-IF
                END-IF
                ADD  1                TO GUIA
                MOVE GUIA             TO SP2-FD-FLD-NUM
                                         SP2-FD-TAB-NUM
                MOVE BARRA-V          TO SP2-FD-ID
                MOVE "BARRA-V"        TO SP2-FD-NAME
                MOVE 780              TO SP2-FD-WIDTH
                MOVE "h"              TO SP2-FD-CTRL-TYPE
                MOVE "s"              TO SP2-FD-PROG-CTRL
                MOVE 5                TO SP2-FD-INITIAL-LEN
                                         SP2-FD-MAX-LEN
                                         SP2-FD-PROG-LEN
                MOVE 1                TO SP2-FD-PROG-OFF
                MOVE 21               TO SP2-FD-ROW
                MOVE 00               TO SP2-FD-COL
                MOVE X"00"            TO SP2-FD-OUTPUT
                                         SP2-FD-CUR-COLR
                                         SP2-FD-COLR
                MOVE "t"              TO SP2-FD-BOR-TYPE
                MOVE 10               TO SP2-FD-HEIGHT
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           IF   CURPOS-COL = 99
                MOVE LOW-VALUES      TO SP2-FD-DATA
                                        SP2-FD-VAR-LENS
                IF   BARRA-HD NOT = 0
                     MOVE BARRA-HD TO BARRA-H
                     MOVE 0        TO BARRA-HD
                ELSE
                     IF   BARRA-H = 0
                          CALL "CWSPID" USING BARRA-H "FInsert"
                     END-IF
                END-IF
                ADD  1                TO GUIA
                MOVE GUIA             TO SP2-FD-FLD-NUM
                                         SP2-FD-TAB-NUM
                MOVE BARRA-H          TO SP2-FD-ID
                MOVE "BARRA-H"        TO SP2-FD-NAME
                MOVE 20               TO SP2-FD-WIDTH
                MOVE "v"              TO SP2-FD-CTRL-TYPE
                MOVE "s"              TO SP2-FD-PROG-CTRL
                MOVE 5                TO SP2-FD-INITIAL-LEN
                                         SP2-FD-MAX-LEN
                                         SP2-FD-PROG-LEN
                MOVE 6                TO SP2-FD-PROG-OFF
                MOVE 0                TO SP2-FD-ROW
                MOVE 78               TO SP2-FD-COL
                MOVE X"00"            TO SP2-FD-OUTPUT
                                         SP2-FD-CUR-COLR
                                         SP2-FD-COLR
                MOVE "t"              TO SP2-FD-BOR-TYPE
                MOVE 210              TO SP2-FD-HEIGHT
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           IF   CURPOS-LIN = 98
           AND (BARRA-V NOT = 0)
                MOVE LOW-VALUES  TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
                MOVE BARRA-V     TO SP2-FD-ID
                CALL SP2      USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                CALL SP2      USING SP2-DELETE-FIELD  SP2-FIELD-DEF
                CALL "CWSPID" USING SP2-FD-ID "FDelete"
                MOVE 0           TO BARRA-V
           END-IF

           IF   CURPOS-LIN = 95
           AND (BARRA-V NOT = 0)
                MOVE LOW-VALUES  TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
                MOVE BARRA-V     TO SP2-FD-ID BARRA-VD
                MOVE 0           TO BARRA-V
                CALL SP2      USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                CALL SP2      USING SP2-DELETE-FIELD  SP2-FIELD-DEF
                MOVE "g"         TO SP2-FD-OUTPUT
                CALL SP2      USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           IF   CURPOS-COL = 98
           AND (BARRA-H NOT = 0)
                MOVE LOW-VALUES  TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
                MOVE BARRA-H     TO SP2-FD-ID
                CALL SP2      USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                CALL SP2      USING SP2-DELETE-FIELD  SP2-FIELD-DEF
                CALL "CWSPID" USING SP2-FD-ID "FDelete"
                MOVE 0           TO BARRA-H
           END-IF

           IF   CURPOS-COL = 95
           AND (BARRA-H NOT = 0)
                MOVE LOW-VALUES  TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
                MOVE BARRA-H     TO SP2-FD-ID BARRA-HD
                MOVE 0           TO BARRA-H
                CALL SP2      USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                CALL SP2      USING SP2-DELETE-FIELD  SP2-FIELD-DEF
                MOVE "g"         TO SP2-FD-OUTPUT
                CALL SP2      USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           IF   CURPOS-COL > 90
           OR   CURPOS-LIN > 90
                IF   CURPOS = "9898"
                     CALL SP2   USING SP2-CLEAR-MENU SP2-NULL-PARM
                     CALL "CWUSER" USING X"6D"
                     MOVE 0 TO MENU-LIGADO
                END-IF
                CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                GOBACK
           END-IF

           CALL "CWGETL" USING PARAMETROS-CWGETL
           IF   CWGETL-TIMEOUT NOT = 0
                IF   CWGETL-TIMEOUT > 255
                     MOVE 255 TO SEGUNDOS
                ELSE
                     MOVE CWGETL-TIMEOUT TO SEGUNDOS
                END-IF
           ELSE
                MOVE 0              TO SEGUNDOS
           END-IF
           MOVE X"00" TO CARACTER
           MOVE 0     TO TECLA-EDIT TECLA-MS
           ON   1
                COPY CWSPPD.
                DISPLAY "ECHOKEY" UPON ENVIRONMENT-NAME
                ACCEPT ECHOKEY FROM ENVIRONMENT-VALUE
                INSPECT ECHOKEY CONVERTING "ofn" TO "OFN"
                DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
                ACCEPT CWCASE FROM ENVIRONMENT-VALUE
                INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
                ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
                INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS.

           INITIALIZE KEY-STATUS
           IF   X91-PARAMETER > 4
                MOVE 0 TO TECLA-SP2
           END-IF
           IF   CURPOS = "0000"
                CALL "CBL_SET_CSR_POS" USING X"FFFF"
           END-IF
           IF   CURPOS NOT = "0000"
                COMPUTE ROW-NUMBER    = CURPOS-LIN - 1
                COMPUTE COLUMN-NUMBER = CURPOS-COL - 1
                CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION
           ELSE
                CALL "CBL_GET_CSR_POS" USING SCREEN-POSITION
luck            MOVE X"0000" TO SCREEN-POSITION
           END-IF
           IF ((SCREEN-POSITION NOT = X"0000")
           AND (SCREEN-POSITION NOT = X"FFFF"))
                CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           END-IF
           MOVE BARRA-AREA      TO BARRA-SAVE
           MOVE CARACTER-BUFFER TO SP2-FD-VAR-DATA
           MOVE LOW-VALUES TO SP2-CD-DATA
           CALL "CWUSER" USING "R" *> Resfresh screen
           CALL "CWKBBF" USING "G" SP2-CD-CTRL-FIELD-KEY
                                   SP2-CD-KEY
           MOVE SEGUNDOS (1: 1) TO SP2-CD-TIMEOUT
           IF  (SP2-CD-CTRL-FIELD-KEY NOT = 0)
           OR  (SP2-CD-KEY            NOT = 0)
               CALL "CWKBBF" USING "C"
           ELSE
               CALL "CWKBST" USING "C"
               CALL "CWCRTS" USING "G" KEY-STATUS
               IF  KEY-STATUS = X"FFFFFF"
                   MOVE SP2-KEY-ESC TO SP2-CD-KEY
                   ADD  1           TO VEZFF
                   IF   VEZFF > 2
                        STOP RUN
                   END-IF
               ELSE
                   IF ((BARRA-H NOT = 0)
                   AND (BARRA-V NOT = 0))
                   OR ((BARRA-HD NOT = 0)
                   AND (BARRA-VD NOT = 0))
                       MOVE "n"             TO SP2-CD-CURS-SW
                       IF  (BARRA-H NOT = 0)
                       AND (BARRA-V NOT = 0)
                            MOVE "y"  TO SP2-CD-CAPTURE-SW
                            CALL "CBL_WRITE_SCR_CHATTRS" USING
                                                         SCREEN-POSITION
                                                         CARACTER-BUFFER
                                                         X"9E"
                                                         X"0001"
                       END-IF
      *                MOVE BARRA-V     TO SP2-CD-NEXT-FLD-ID
                   ELSE
                       MOVE LOW-VALUES      TO SP2-FD-DATA
                                               SP2-FD-VAR-LENS
                       CALL "CWSPID" USING SP2-FD-ID "FInsert"
                       ADD  1               TO GUIA
                       MOVE GUIA            TO SP2-FD-FLD-NUM
                                               SP2-FD-TAB-NUM
                       MOVE "CHAR"          TO SP2-FD-NAME
                       MOVE "e"             TO SP2-FD-CTRL-TYPE
                       MOVE X"00"           TO SP2-FD-CTRL-TYPE
                       MOVE 1               TO SP2-FD-PROG-LEN
                                               SP2-FD-MAX-LEN
                                               SP2-FD-INITIAL-LEN
                                               SP2-FD-VAR-LEN
                       MOVE "a"             TO SP2-FD-PROG-CTRL
                       MOVE "y"             TO SP2-FD-CURS-SKIP
                       MOVE ROW-NUMBER      TO SP2-FD-ROW
                       MOVE COLUMN-NUMBER   TO SP2-FD-COL
                       CALL SP2          USING SP2-SET-FIELD-DEF
                                               SP2-FIELD-DEF
                       MOVE SP2-FD-ID       TO SP2-CD-NEXT-FLD-ID
                   END-IF
                   MOVE "1"      TO SP2-CD-WAIT-SW
                   IF ((BARRA-H NOT = 0)
                   OR  (BARRA-V NOT = 0))
                       CALL SP2   USING SP2-SET-PANEL-FIELDS DATA-AREA
                   END-IF
      *            CALL SP2   USING SP2-DISPLAY-WINDOW
      *                                  SP2-NULL-PARM
RDIG               MOVE "CWMENU" TO SP2-PD-NAME
RDIG               CALL SP2   USING SP2-GET-PANEL-DEF SP2-PANEL-DEF
RDIG               MOVE SP2-KEY-INSERT TO SP2-PD-CTRL-KEY (16)
RDIG               MOVE SP2-KEY-DELETE TO SP2-PD-CTRL-KEY (37)
RDIG               CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF
                   PERFORM TEST AFTER
                           UNTIL SP2-CD-KEY NOT = SP2-KEY-CTRL-P
      *                 CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                        CALL "CWKBBF" USING "G" SP2-CD-CTRL-FIELD-KEY
                                                SP2-CD-KEY
                        IF  SP2-CD-CTRL-FIELD-KEY = 0
                        AND SP2-CD-KEY = 0
                            CALL SP2   USING SP2-CONVERSE-PANEL
                                             SP2-CONVERSE-DATA
                        ELSE
                            IF  SP2-CD-CTRL-FIELD-KEY = 500
                                MOVE 0 TO SP2-CD-CTRL-FIELD-KEY
                            END-IF
                        END-IF
                        MOVE SP2-CD-KEY TO CD-KEY-X
                        IF   SP2-CD-KEY = SP2-KEY-CTRL-P
                             CALL "CWPRTS"
                             CANCEL "CWPRTS"
                             CALL SP2   USING SP2-DISPLAY-WINDOW
                                              SP2-NULL-PARM
                        END-IF
                   END-PERFORM
RDIG               MOVE SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (16)
RDIG               MOVE SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (37)
RDIG               CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF
                   IF  (SP2-CD-KEY = SP2-KEY-MOUSE OR SP2-KEY-ENTER)
                   AND BARRA-AREA = BARRA-SAVE
                   AND((BARRA-H NOT = 0)
                    OR (BARRA-V NOT = 0))
                       EVALUATE TRUE
                          WHEN SP2-CD-CURSOR-ROW = 21
                           AND SP2-CD-CURSOR-COL = 00
                               MOVE SP2-KEY-LEFT TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW = 21
                           AND SP2-CD-CURSOR-COL = 77
                               MOVE SP2-KEY-RIGHT TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW = 20
                           AND SP2-CD-CURSOR-COL > 77
                               MOVE SP2-KEY-DOWN TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW = 00
                           AND SP2-CD-CURSOR-COL > 77
                               MOVE SP2-KEY-UP TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW < 10
                           AND SP2-CD-CURSOR-COL > 77
                               MOVE SP2-KEY-PGUP  TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW > 09
                           AND SP2-CD-CURSOR-COL > 77
                               MOVE SP2-KEY-PGDN  TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW = 21
                           AND SP2-CD-CURSOR-COL > 39
                               MOVE SP2-KEY-CTRL-RIGHT TO SP2-CD-KEY
                          WHEN SP2-CD-CURSOR-ROW = 21
                           AND SP2-CD-CURSOR-COL < 40
                               MOVE SP2-KEY-CTRL-LEFT TO SP2-CD-KEY
                       END-EVALUATE
                   END-IF
                   IF ((BARRA-H NOT = 0)
                   OR  (BARRA-V NOT = 0))
                       CALL "CWUSER" USING X"02"
                   END-IF
                   IF  (BARRA-H NOT = 0)
                   AND (BARRA-V NOT = 0)
                   OR ((BARRA-HD NOT = 0)
                   AND (BARRA-VD NOT = 0))
                        IF  (BARRA-H NOT = 0)
                        AND (BARRA-V NOT = 0)
                           CALL "CBL_WRITE_SCR_CHATTRS"
                                USING SCREEN-POSITION
                                      CARACTER-BUFFER
                                      ATTRIBUTE-BUFFER
                                      STRING-LENGTH
                        END-IF
                   ELSE
                       CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                       CALL "CWSPID" USING SP2-FD-ID "FDelete"
                   END-IF
                   move 0 to k
                   if SP2-CD-KEY (2: 1) = X"00"
                      perform varying d from 1 by 1
                                 until d > length digitaveis
                                    or k = 1
                       if SP2-CD-KEY (1: 1) = digitaveis (D: 1)
                          MOVE SP2-CD-KEY (1: 1) TO CARACTER-BUFFER
                          move 1 to k
                       end-if
                      end-perform
                   end-if
                   If   K = 1
                        move SP2-KEY-CTRL-FIELD to SP2-CD-KEY
                        move  0 to SP2-CD-CTRL-FIELD-KEY
                   ELSE
                        IF   BARRA-AREA NOT = BARRA-SAVE
      *                 OR  (SP2-CD-NEXT-FLD-ID = BARRA-H
      *                 AND (SP2-CD-NEXT-FLD-ID NOT = 0))
      *                 OR  (SP2-CD-NEXT-FLD-ID = BARRA-V
      *                 AND (SP2-CD-NEXT-FLD-ID NOT = 0))
                             IF  BARRA-H NOT = 0
                                 MOVE BARRA-HN TO CURPOS-HP
                                 MOVE 999      TO TECLA-EDIT
                             END-IF
                             IF  BARRA-V NOT = 0
                                 MOVE BARRA-VN TO CURPOS-VP
                                 MOVE 999      TO TECLA-EDIT
                             END-IF
                             IF   CURPOS-HP = 0
                                  MOVE 1 TO CURPOS-HP
                                            BARRA-HN
                             END-IF
                             IF   CURPOS-VP = 0
                                  MOVE 1 TO CURPOS-VP
                                            BARRA-VN
                             END-IF
                        ELSE
                             IF  SP2-CD-KEY = SP2-KEY-MOUSE
                             AND((BARRA-H NOT = 0)
                              OR (BARRA-V NOT = 0))
                                 COMPUTE CURPOS-LIN = SP2-CD-CURSOR-ROW
                                                    + 1
                                 COMPUTE CURPOS-COL = SP2-CD-CURSOR-COL
                                                    + 1
                                 MOVE 998 TO TECLA-EDIT
                             END-IF
                        END-IF
                   END-if
                   IF   SP2-CD-KEY  = SP2-KEY-TIMEOUT
                   OR                 SP2-KEY-MENU
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        MOVE 0           TO SP2-CD-CTRL-FIELD-KEY
                   END-IF
               END-IF
           END-IF
           IF   TECLA-EDIT > 997
                MOVE TECLA-EDIT TO TECLA-MS
                GO TO 000-EXIT
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-CLOSE
           OR   SP2-KEY-SYS-SHUTDOWN
           OR   SP2-KEY-APP-CLOSE
                MOVE SP2-KEY-ESC TO SP2-CD-KEY
                CALL "CWCRTS" USING "G" KEY-STATUS
                IF   KEY-STATUS = HIGH-VALUES
                     STOP RUN
                ELSE
                     CALL "CWCRTS" USING "S" X"FFFFFF"
                END-IF
           END-IF
           IF   SP2-CD-CTRL-FIELD-KEY < 255
           AND  SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                MOVE 0               TO SP2-CD-KEY
                MOVE 3               TO KEY-TYPE
                MOVE CARACTER-BUFFER TO DATA-ITEM
                INSPECT DATA-ITEM CONVERTING ACENTOS-WINDOWS
                                          TO ACENTOS-850
                IF   CWACCENT = "OFF"
                     INSPECT DATA-ITEM CONVERTING ACENTOS-850
                          TO ACENTOS-OFF
                END-IF
                IF   CWCASE = "LOW"
                     INSPECT DATA-ITEM CONVERTING MAIUSCULAS
                          TO MINUSCULAS
                END-IF
                IF   CWCASE = "UPP"
                     INSPECT DATA-ITEM CONVERTING MINUSCULAS
                          TO MAIUSCULAS
                END-IF
                MOVE DATA-ITEM TO CARACTER CARACTER-BUFFER
                                  KEY-CODE-1 (1: 1)
                                  KEY-CODE-2 (1: 1)
      *         IF ((SCREEN-POSITION NOT = X"0000")
      *         AND (SCREEN-POSITION NOT = X"FFFF"))
      *             CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
      *                                                CARACTER-BUFFER
      *                                                ATTRIBUTE-BUFFER
      *                                                STRING-LENGTH
      *         END-IF
                GO TO 000-EXIT
           ELSE
                IF   SP2-CD-CTRL-FIELD-KEY NOT = 0
                     MOVE SP2-CD-CTRL-FIELD-KEY TO SP2-CD-KEY
                END-IF
                MOVE X"00" TO DATA-ITEM
           END-IF
           COPY CWSPKY.
           IF ECHOKEY = "ON"
           OR X91-PARAMETER > 2
              IF   X91-PARAMETER > 4
                   MOVE SP2-CD-KEY TO TECLA-SP2
              END-IF
              EVALUATE SP2-CD-KEY
                       COPY CWSPKS.
              END-EVALUATE
              IF   KEY-CODE-2 (1: 1) = SPACE
                   MOVE TECLA-EDIT TO KEY-CODE-2
              END-IF
           END-IF.
       000-EXIT.
      *    IF  (KEY-STATUS NOT = X"FFFFFF")
                MOVE LOW-VALUES TO SP2-CD-DATA
                MOVE "k"        TO SP2-CD-WAIT-SW
                MOVE "n"        TO SP2-CD-CAPTURE-SW
                CALL SP2     USING SP2-GET-INPUT SP2-CONVERSE-DATA
      *    END-IF
           CALL "CWAKEY" USING TECLA-MS MIL
           IF   TECLA-EDIT > 990
                GOBACK
           END-IF
           IF   X91-PARAMETER > 3
                MOVE KEY-STATUS TO LK-STATUS
           END-IF
           IF   ECHOKEY = "ON"
                MOVE KEY-TYPE                   TO KEY-BUFFER (01: 01)
                MOVE KEY-CODE-1                 TO PIC3
                MOVE PIC3                       TO KEY-BUFFER (03: 03)
                MOVE KEY-CODE-2                 TO PIC3
                MOVE PIC3                       TO KEY-BUFFER (07: 03)
                CALL "CBL_WRITE_SCR_CHATTRS" USING X"182C"
                                              KEY-BUFFER
                        X"0909090909090909090909090909090909"
                                              X"0009"
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWKBDC.
