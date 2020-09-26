       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLINE INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/06/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Menu em linha/buttons                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CWGETL                  PIC  X(003) VALUE SPACES.
           05 CWLITS                  PIC  X(003) VALUE SPACES.
           05 CWACCENT                PIC  X(003) VALUE SPACES.
           05 TESTE                   PIC  X(034) VALUE SPACES.
           05 TESTE2                  PIC  X(018) VALUE SPACES.
           05 TESTE3                  PIC  X(018) VALUE SPACES.
           05 REP                     PIC  X(003) VALUE SPACES.
           05 BUFFER                  PIC X(5000) VALUE SPACES.
           05 SEGUNDOS         COMP-X PIC  9(002) VALUE 0.
           05 CHAR-X.
              10 CHAR-N        COMP-X PIC  9(002).
           05 NEW-COL                 PIC S9(004) COMP-5.
           05 OLD-COL                 PIC  9(002) VALUE 0.
           05 TAMANHO                 PIC  9(002) VALUE 0.
           05 CRT-STATUS              PIC  X(003) VALUE SPACES.
           05 OK                      PIC  9(001) VALUE 0.
           05 VEZ                     PIC  9(001) VALUE 0.
           05 T                       PIC  9(002) VALUE 0.
           05 I                       PIC  9(002) VALUE 0.
           05 F                       PIC  9(002) VALUE 0.
           05 F1                      PIC  9(002) VALUE 0.
           05 FN                      PIC  9(002) VALUE 0.
           05 D                       PIC  9(002) VALUE 0.
           05 Y                       PIC  9(002) VALUE 0.
           05 X                       PIC  9(002) VALUE 0.
           05 OPT-DEF                 PIC  9(002) VALUE 0.
           05 LENG                    PIC  9(002).
           05 TABELA.
              10 OPCOES-CHAR                      VALUE SPACES.
                 15 CHAR     OCCURS 26.
                    20 ASCII          PIC  9(002) COMP-X.
              10 OPCOES-LOW                       VALUE SPACES.
                 15 CHAR-LOW OCCURS 26.
                    20 ASCII-LOW      PIC  9(002) COMP-X.
              10 TECLAS-FUNCAO                    VALUE LOW-VALUES.
                 15 FUNCAO  OCCURS 26 PIC S9(004) COMP-5.
              10 LINHA                PIC  9(002) VALUE 0.
              10 COLUNA               PIC  9(002) VALUE 0.
              10 LIMITE               PIC  9(003) VALUE 0.
              10 FD-DEF        COMP-5 PIC S9(004) VALUE 0.
              10 CUR-OP               PIC  9(002) VALUE 0.
              10 OPCOES               PIC  9(002) VALUE 0.
              10 OPCAO OCCURS 26.
                 15 OPCAO-TXT.
                    20 BYTE-TXT OCCURS 38
                                      PIC X(001).
                 15 FD-ID      COMP-5 PIC S9(004).

       COPY CWGETL.
       COPY CWSPWS.

       01  TECLAS.
           05 PIC X(021) VALUE "ESC               027".
           05 PIC X(021) VALUE "F1                315".
           05 PIC X(021) VALUE "F2                316".
           05 PIC X(021) VALUE "F3                317".
           05 PIC X(021) VALUE "F4                318".
           05 PIC X(021) VALUE "F5                319".
           05 PIC X(021) VALUE "F6                320".
           05 PIC X(021) VALUE "F7                321".
           05 PIC X(021) VALUE "F8                322".
           05 PIC X(021) VALUE "F9                323".
           05 PIC X(021) VALUE "F10               324".
           05 PIC X(021) VALUE "SHIFT-F1          340".
           05 PIC X(021) VALUE "SHIFT-F2          341".
           05 PIC X(021) VALUE "SHIFT-F3          342".
           05 PIC X(021) VALUE "SHIFT-F4          343".
           05 PIC X(021) VALUE "SHIFT-F5          344".
           05 PIC X(021) VALUE "SHIFT-F6          345".
           05 PIC X(021) VALUE "SHIFT-F7          346".
           05 PIC X(021) VALUE "SHIFT-F8          347".
           05 PIC X(021) VALUE "SHIFT-F9          348".
           05 PIC X(021) VALUE "SHIFT-F10         349".
           05 PIC X(021) VALUE "CONTROL-F1        350".
           05 PIC X(021) VALUE "CONTROL-F2        351".
           05 PIC X(021) VALUE "CONTROL-F3        352".
           05 PIC X(021) VALUE "CONTROL-F4        353".
           05 PIC X(021) VALUE "CONTROL-F5        354".
           05 PIC X(021) VALUE "CONTROL-F6        355".
           05 PIC X(021) VALUE "CONTROL-F7        356".
           05 PIC X(021) VALUE "CONTROL-F8        357".
           05 PIC X(021) VALUE "CONTROL-F9        358".
           05 PIC X(021) VALUE "CONTROL-F10       359".
           05 PIC X(021) VALUE "ALT-F1            360".
           05 PIC X(021) VALUE "ALT-F2            361".
           05 PIC X(021) VALUE "ALT-F3            362".
           05 PIC X(021) VALUE "ALT-F4            363".
           05 PIC X(021) VALUE "ALT-F5            364".
           05 PIC X(021) VALUE "ALT-F6            365".
           05 PIC X(021) VALUE "ALT-F7            366".
           05 PIC X(021) VALUE "ALT-F8            367".
           05 PIC X(021) VALUE "ALT-F9            368".
           05 PIC X(021) VALUE "ALT-F10           369".
           05 PIC X(021) VALUE "ALT-1             376".
           05 PIC X(021) VALUE "ALT-2             377".
           05 PIC X(021) VALUE "ALT-3             378".
           05 PIC X(021) VALUE "ALT-4             379".
           05 PIC X(021) VALUE "ALT-5             380".
           05 PIC X(021) VALUE "ALT-6             381".
           05 PIC X(021) VALUE "ALT-7             382".
           05 PIC X(021) VALUE "ALT-8             383".
           05 PIC X(021) VALUE "ALT-9             384".
           05 PIC X(021) VALUE "ALT-0             385".
           05 PIC X(021) VALUE "PAGE-UP           329".
           05 PIC X(021) VALUE "PAGE-DOWN         337".
           05 PIC X(021) VALUE "CONTROL-PAGE-UP   388".
           05 PIC X(021) VALUE "CONTROL-PAGE-DOWN 374".
           05 PIC X(021) VALUE "ALT-A             286".
           05 PIC X(021) VALUE "ALT-B             304".
           05 PIC X(021) VALUE "ALT-C             302".
           05 PIC X(021) VALUE "ALT-D             288".
           05 PIC X(021) VALUE "ALT-E             274".
           05 PIC X(021) VALUE "ALT-F             289".
           05 PIC X(021) VALUE "ALT-G             290".
           05 PIC X(021) VALUE "ALT-H             291".
           05 PIC X(021) VALUE "ALT-I             279".
           05 PIC X(021) VALUE "ALT-J             292".
           05 PIC X(021) VALUE "ALT-K             293".
           05 PIC X(021) VALUE "ALT-L             294".
           05 PIC X(021) VALUE "ALT-M             306".
           05 PIC X(021) VALUE "ALT-N             305".
           05 PIC X(021) VALUE "ALT-O             280".
           05 PIC X(021) VALUE "ALT-P             281".
           05 PIC X(021) VALUE "ALT-Q             272".
           05 PIC X(021) VALUE "ALT-R             275".
           05 PIC X(021) VALUE "ALT-S             287".
           05 PIC X(021) VALUE "ALT-T             276".
           05 PIC X(021) VALUE "ALT-U             278".
           05 PIC X(021) VALUE "ALT-V             303".
           05 PIC X(021) VALUE "ALT-W             273".
           05 PIC X(021) VALUE "ALT-X             301".
           05 PIC X(021) VALUE "ALT-Y             277".
           05 PIC X(021) VALUE "ALT-Z             300".
           05 PIC X(021) VALUE "F11               389".
           05 PIC X(021) VALUE "F12               390".
           05 PIC X(021) VALUE "SHIFT-F11         340".
           05 PIC X(021) VALUE "SHIFT-F12         341".
           05 PIC X(021) VALUE "CONTROL-F11       393".
           05 PIC X(021) VALUE "CONTROL-F12       394".
           05 PIC X(021) VALUE "ALT-F11           395".
           05 PIC X(021) VALUE "ALT-F12           396".
       01  REDEFINES TECLAS.
           05 OCCURS 89.
              10 TECLA-NOME PIC X(018).
              10 TECLA-COD  PIC 9(003).

       LINKAGE SECTION.

       01  PARAMETROS-CWLINE.
           05 CWLINE-LINE                    PIC  9(002).
           05 CWLINE-COLUMN                  PIC  9(002).
           05 CWLINE-LINE-END                PIC  9(002).
           05 CWLINE-COLUMN-END              PIC  9(002).
           05 CWLINE-TYPE                    PIC  X(001).
              88 CWLINE-BUTTONS                          VALUE "B" "b"
                                                               "S".
              88 CWLINE-REVERSED                         VALUE "R" "r".
              88 CWLINE-BUTTONS-NODEF                    VALUE "b".
              88 CWLINE-REVERSED-NODEF                   VALUE "r".
              88 CWLINE-NODEF                     VALUE "b" "r".
              88 CWLINE-CWSEND                    VALUE "S".
           05 CWLINE-SCREENS.
              10 CWLINE-CHAR   OCCURS 26     PIC  X(001).
              10 CWLINE-SCREEN OCCURS 26     PIC  X(034).
           05 CWLINE-POSITION  OCCURS 26.
              15 CWLINE-POSITION-LINE        PIC  9(002).
              15 CWLINE-POSITION-COLUMN      PIC  9(002).
           05 CWLINE-COLOR.
              10 CWLINE-COLOR-LOW            PIC  9(002) COMP-X.
              10 CWLINE-COLOR-HIGH           PIC  9(002) COMP-X.
           05 CWLINE-OPTION                  PIC  9(003).
           05 CWLINE-OPTION-CHAR             PIC  X(001).
           05 CWLINE-MOUSE                   PIC  X(001).
              88 CWLINE-SMALL                            VALUE "M".
           05 CWLINE-TIMEOUT-STATUS          PIC  9(001).
              88 CWLINE-TIMEOUT-ENABLE                   VALUE 1 5.
              88 CWLINE-TIMEOUT-RETRY                    VALUE 5.
              88 CWLINE-TIMEOUT-DISABLE                  VALUE 0.
           05 CWLINE-TIMEOUT-RETURN          PIC  9(001).
              88 CWLINE-TIMEOUT-ON                       VALUE 1.
              88 CWLINE-TIMEOUT-OFF                      VALUE 0.

       PROCEDURE DIVISION USING PARAMETROS-CWLINE.

       000-INICIO.

           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           MOVE LOW-VALUES TO SP2-CD-DATA.
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           CALL "CWKBST" USING "C"
           CALL "CWCRTS" USING "G" CRT-STATUS
           IF   CRT-STATUS = X"FFFFFF"
                STOP RUN
           END-IF
           COPY CWSPPD.
           DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
           ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
           IF CWGETL NOT = 'OFF'
              CALL "CWGETL" USING PARAMETROS-CWGETL
           END-IF
           SET CWLINE-TIMEOUT-OFF TO TRUE
           IF   CWGETL-TIMEOUT NOT = 0
           AND  CWLINE-TIMEOUT-ENABLE
                IF   CWGETL-TIMEOUT > 255
                     MOVE 255 TO SEGUNDOS
                ELSE
                     MOVE CWGETL-TIMEOUT TO SEGUNDOS
                END-IF
           ELSE
                MOVE 0              TO SEGUNDOS
           END-IF
           IF    CWLINE-TIMEOUT-RETRY
                 MOVE 5 TO SEGUNDOS
           END-IF
           MOVE SEGUNDOS (1: 1) TO SP2-CD-TIMEOUT
           IF   SEGUNDOS = 0
                MOVE X"00" TO SP2-CD-WAIT-SW
           ELSE
                MOVE "a"   TO SP2-CD-WAIT-SW
           END-IF
           IF   CWLINE-CWSEND
                continue
           ELSE
                EXEC COBOLware PROCESS (CLOSE) END-EXEC
                CALL "CWUSER" USING "R" *> Refresh screen
           END-IF
           MOVE CWLINE-LINE   TO LINHA
           MOVE CWLINE-COLUMN TO COLUNA OLD-COL
           COMPUTE NEW-COL = COLUNA * 1,25
           MOVE 0             TO SP2-CD-KEY FD-DEF
           PERFORM VARYING I FROM 1 BY 1
                            UNTIL I > 26
                               OR LINHA > CWLINE-LINE-END
               IF   CWLINE-SCREEN (I) NOT = SPACES
                    PERFORM 020-CHECK-FUNCTION-KEY THRU 020-99-FIM
                    ADD  1             TO OPCOES
                    MOVE LOW-VALUES    TO SP2-FD-DATA
                                          SP2-FD-VAR-LENS
                    MOVE LINHA         TO SP2-FD-ROW
                    IF   COLUNA > 77
                         SUBTRACT 1 FROM NEW-COL
                    END-IF
                    MOVE NEW-COL TO SP2-FD-COL
                    SUBTRACT 1 FROM SP2-FD-ROW SP2-FD-COL
                    MOVE SPACES            TO SP2-FD-VAR-DATA
                    PERFORM VARYING X FROM 34 BY -1
                      UNTIL CWLINE-SCREEN (I) (X: 1) NOT = SPACE
                            CONTINUE
                    END-PERFORM
                    IF   CWLINE-CHAR (OPCOES) NOT = SPACE
                         MOVE CWLINE-CHAR (I) TO CHAR (OPCOES)
                    END-IF
                    MOVE 0 TO TAMANHO
                    PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > X
                            IF   CWLINE-SCREEN (I) (Y: 1) = X"7E"
                                 ADD 1 TO Y
                                 IF  CHAR (OPCOES) = SPACE
                                     MOVE CWLINE-SCREEN (I) (Y: 1)
                                       TO CHAR (OPCOES)
                                          CWLINE-CHAR (I)
                                 END-IF
                            END-IF
                            ADD 1 TO TAMANHO
                            MOVE CWLINE-SCREEN (I) (Y: 1)
                              TO SP2-FD-VAR-DATA (TAMANHO: 1)
                    END-PERFORM
                    MOVE SP2-FD-VAR-DATA TO CWLINE-SCREEN (I)
                    MOVE OPCOES          TO SP2-FD-FLD-NUM
                                            SP2-FD-TAB-NUM
                    MOVE SPACES          TO SP2-FD-VAR-DATA
                    MOVE TAMANHO         TO X
                    MOVE 0               TO TAMANHO
                    PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > X
                        ADD  1 TO TAMANHO
                        IF  (CWLINE-SCREEN(I) (Y: 1) NOT = SPACE)
                        AND  CWLINE-SCREEN(I) (Y: 1) = CHAR (OPCOES)
                             MOVE X"7E" TO SP2-FD-VAR-DATA (TAMANHO: 1)
                             ADD 1 TO TAMANHO
                        END-IF
                        MOVE CWLINE-SCREEN(I) (Y: 1)
                          TO SP2-FD-VAR-DATA (TAMANHO: 1)
                    END-PERFORM
                    INSPECT SP2-FD-VAR-DATA (1: TAMANHO)
                            CONVERTING "_" TO " "
                    IF   CWLITS = "LOW"
                         INSPECT SP2-FD-VAR-DATA (1: TAMANHO)
                                 CONVERTING MAIUSCULAS TO MINUSCULAS
                    END-IF
                    IF   CWLITS = "UPP"
                         INSPECT SP2-FD-VAR-DATA (1: TAMANHO)
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                    END-IF
                    IF   CWACCENT = "OFF"
                         INSPECT SP2-FD-VAR-DATA (1: TAMANHO)
                                 CONVERTING ACENTOS-850 TO ACENTOS-OFF
                    ELSE
                         INSPECT SP2-FD-VAR-DATA (1: TAMANHO)
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-WINDOWS
                    END-IF
                    MOVE TAMANHO     TO SP2-FD-MAX-LEN
                                        SP2-FD-INITIAL-LEN
                    IF   CWLINE-BUTTONS
                    AND (NOT CWLINE-SMALL)
                         ADD  3     TO TAMANHO
                         MOVE X"01" TO SP2-FD-TYPE
                         MOVE 27    TO SP2-FD-HEIGHT
                         IF   CWLINE-CWSEND
                              SUBTRACT  5 FROM TAMANHO
                              MOVE 15 TO SP2-FD-HEIGHT
                         END-IF
                    ELSE
                         MOVE X"00" TO SP2-FD-TYPE
                         MOVE 10    TO SP2-FD-HEIGHT
                    END-IF

                    MOVE TAMANHO TO LENG
                    COMPUTE LIMITE = COLUNA
                                   + LENG
                                   + 1
                    IF   LIMITE > CWLINE-COLUMN-END
                         ADD  1             TO LINHA
                         IF   CWLINE-BUTTONS
                              ADD  3        TO LINHA
                         END-IF
                         MOVE CWLINE-COLUMN TO COLUNA
                         COMPUTE LIMITE = LENG + CWLINE-COLUMN + 1
                    END-IF
                    COMPUTE COLUNA = LIMITE + 1
                    COMPUTE SP2-FD-WIDTH = LENG * 11,5
                    IF  LENG > 10
                        COMPUTE SP2-FD-WIDTH = SP2-FD-WIDTH - (LENG * 2)
                        COMPUTE SP2-FD-COL = SP2-FD-COL + (LENG / 10)
                        SUBTRACT 1 FROM COLUNA
                    END-IF
                    MOVE "p"        TO SP2-FD-CTRL-TYPE
                    IF   CWLINE-CHAR (I) NOT = SPACE
                         MOVE CWLINE-CHAR (I) TO SP2-FD-MNEMONIC
                         INSPECT SP2-FD-MNEMONIC
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                         MOVE SP2-FD-MNEMONIC TO CHAR-X
                    ELSE
                         IF   FUNCAO (I) NOT = 0
                              MOVE FUNCAO (I) TO SP2-FD-HELP-KEY
                         END-IF
                    END-IF
                    IF   LINHA > CWLINE-LINE-END
                         SUBTRACT 1 FROM OPCOES
                    ELSE
                         CALL "CWSPID" USING SP2-FD-ID "FInsert"
                         MOVE SP2-FD-ID TO FD-ID (OPCOES)
                         MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                         IF   CWLINE-POSITION (I) = "9999"
                              MOVE "g" TO SP2-FD-OUTPUT
                         END-IF
                         MOVE 0 TO SP2-FD-FONT-ID
                         MOVE SPACES TO SP2-FD-NAME
                         STRING 'CWLINE-' OPCOES DELIMITED BY SIZE
                                     INTO SP2-FD-NAME
                         CALL SP2   USING SP2-SET-FIELD-DEF
                                          SP2-FIELD-DEF
                         IF   OPCOES = CWLINE-OPTION
                              MOVE SP2-FD-ID TO FD-DEF
                              MOVE OPCOES    TO CUR-OP
                         END-IF
                    END-IF
                    COMPUTE NEW-COL = NEW-COL
                                    +  (COLUNA - OLD-COL)
                    MOVE COLUNA TO OLD-COL
               END-IF
           END-PERFORM
           PERFORM 010-OPEN-PANEL THRU 010-99-FIM
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           CALL "CWCRTS" USING "G" CRT-STATUS
           IF   CRT-STATUS NOT = X"FFFFFF"
                MOVE FD-DEF     TO SP2-CD-NEXT-FLD-ID
                MOVE 0          TO OK
                MOVE LOW-VALUES TO OPCOES-LOW
                PERFORM VARYING D FROM 1 BY 1
                         UNTIL  D > OPCOES
                        IF  CHAR(D) NOT = SPACE
                            MOVE CHAR(D) TO CHAR-LOW(D)
                            INSPECT CHAR-LOW(D)
                                 CONVERTING MAIUSCULAS TO MINUSCULAS
                            IF CHAR-LOW(D) = ">"
                               MOVE "." TO CHAR-LOW(D)
                            ELSE
                               IF CHAR-LOW(D) = "<"
                                  MOVE "," TO CHAR-LOW(D)
                               END-IF
                            END-IF
                        END-IF
                END-PERFORM
                MOVE SP2-WD-NAME TO SP2-ND-NAME
                CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF

                PERFORM UNTIL OK = 1
                        PERFORM TEST AFTER UNTIL SP2-CD-KEY NOT = -4
                                CALL SP2   USING SP2-GET-INPUT
                                                 SP2-CONVERSE-DATA
                                IF   SP2-CD-KEY = SP2-KEY-ENTER
                                             OR SP2-KEY-MOUSE
                                     MOVE SP2-CD-NEXT-FLD-ID
                                       TO SP2-CD-BUTTON-ID
                                END-IF
                                IF   SP2-CD-KEY = SP2-KEY-LEFT
                                OR   SP2-KEY-UP
                                     MOVE -4 TO SP2-CD-KEY
                                     SUBTRACT 1 FROM CUR-OP
                                     IF  CUR-OP = 0
                                         MOVE OPCOES TO CUR-OP
                                     END-IF
                                     MOVE FD-ID (CUR-OP)
                                       TO SP2-CD-NEXT-FLD-ID
                                ELSE
                                     IF   SP2-CD-KEY = SP2-KEY-RIGHT
                                     OR   SP2-KEY-DOWN
                                          MOVE -4 TO SP2-CD-KEY
                                          ADD 1 TO CUR-OP
                                          IF  CUR-OP > OPCOES
                                              MOVE 1 TO CUR-OP
                                          END-IF
                                          MOVE FD-ID (CUR-OP)
                                            TO SP2-CD-NEXT-FLD-ID
                                     END-IF
                                END-IF
                                IF  SP2-CD-KEY = -4
                                OR  SP2-KEY-TIMEOUT
                                OR  SP2-KEY-ESC
                                OR  SP2-KEY-CLOSE
                                OR  SP2-KEY-SYS-SHUTDOWN
                                OR  SP2-KEY-APP-CLOSE
                                OR (SP2-CD-BUTTON-ID NOT = 0)
                                    CONTINUE
                                ELSE
                                    PERFORM VARYING D FROM 1 BY 1
                                              UNTIL D > OPCOES
                                            OR (SP2-CD-BUTTON-ID <> 0)
                                         IF ASCII (D)     = SP2-CD-KEY
                                         OR ASCII-LOW (D) = SP2-CD-KEY
                                         OR FUNCAO    (D) = SP2-CD-KEY
                                            MOVE FD-ID (D)
                                              TO SP2-CD-BUTTON-ID
                                            move d to i
                                            move 0 to sp2-cd-key
                                         END-IF
                                    END-PERFORM
                                    IF  SP2-CD-BUTTON-ID = 0
                                        MOVE -4 TO SP2-CD-KEY
                                    END-IF
                                END-IF
                        END-PERFORM
                        MOVE 0 TO VEZ
                        IF   SP2-CD-KEY = SP2-KEY-TIMEOUT
                             MOVE SP2-KEY-ESC TO SP2-CD-KEY
                             SET CWLINE-TIMEOUT-ON TO TRUE
                        END-IF
                        IF   SP2-CD-KEY = SP2-KEY-ESC
                        OR   SP2-KEY-CLOSE
                        OR   SP2-KEY-SYS-SHUTDOWN
                        OR   SP2-KEY-APP-CLOSE
                             MOVE 1 TO OK
                             IF ((NOT CWLINE-CWSEND)
                             AND (NOT SP2-CD-KEY = SP2-KEY-ESC))
                             OR  SP2-CD-KEY =  SP2-KEY-SYS-SHUTDOWN
                                  CALL "CWCRTS" USING "S" X"FFFFFF"
                             END-IF
                        ELSE
                             PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > OPCOES
                                          OR OK = 1
                                IF  FD-ID (I) = SP2-CD-BUTTON-ID
                                OR (SP2-CD-BUTTON-ID = 0
                                AND FD-ID (I) = SP2-CD-NEXT-FLD-ID)
                                    MOVE 1 TO OK
                                END-IF
                             END-PERFORM
                        END-IF
                END-PERFORM
           END-IF

           MOVE CWLINE-OPTION  TO OPT-DEF
           MOVE 0              TO CWLINE-OPTION
           MOVE SPACE          TO CWLINE-OPTION-CHAR
           PERFORM UNTIL OPCOES = 0
                   MOVE FD-ID (OPCOES) TO SP2-FD-ID
                   IF   SP2-FD-ID = SP2-CD-BUTTON-ID
                   OR (SP2-CD-BUTTON-ID = 0
                   AND SP2-FD-ID = SP2-CD-NEXT-FLD-ID)
                        MOVE OPCOES        TO CWLINE-OPTION
                        MOVE CHAR (OPCOES) TO CWLINE-OPTION-CHAR
                        INSPECT CWLINE-OPTION-CHAR CONVERTING
                                MINUSCULAS TO MAIUSCULAS
                   END-IF
                   CALL "CWSPID" USING SP2-FD-ID "FDelete"
                   CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   SUBTRACT 1 FROM OPCOES
           END-PERFORM
           IF   CRT-STATUS = X"FFFFFF"
           OR  ((SP2-CD-KEY = SP2-KEY-ESC OR SP2-KEY-CLOSE)
                                       AND (OPT-DEF NOT = 0))
                IF   CWLINE-NODEF
                OR   CWLINE-CWSEND
                     MOVE 0              TO CWLINE-OPTION
                     MOVE SPACE          TO CWLINE-OPTION-CHAR
                ELSE
                     MOVE OPT-DEF        TO CWLINE-OPTION
                     MOVE CHAR (OPT-DEF) TO CWLINE-OPTION-CHAR
                END-IF
           ELSE
                IF   SP2-CD-KEY = SP2-KEY-ESC
                OR   SP2-KEY-CLOSE
                OR   SP2-KEY-SYS-SHUTDOWN
                OR   SP2-KEY-APP-CLOSE
                     MOVE 0     TO CWLINE-OPTION
                     MOVE SPACE TO CWLINE-OPTION-CHAR
                     IF  (NOT CWLINE-NODEF)
                     AND (OPT-DEF NOT = 0)
                          MOVE OPT-DEF        TO CWLINE-OPTION
                          MOVE CHAR (OPT-DEF) TO CWLINE-OPTION-CHAR
                     END-IF
                     IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
                     OR   SP2-KEY-APP-CLOSE
                     OR  (SP2-KEY-CLOSE AND (NOT CWLINE-CWSEND))
                          CALL "CWCRTS" USING "S" X"FFFFFF"
                     END-IF
                END-IF
           END-IF

           MOVE "k" TO SP2-CD-WAIT-SW
           CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
           MOVE BUFFER TO SP2-PANEL-DEF
           CALL "CWRESE"
           CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.

       000-99-FIM. GOBACK.

       010-OPEN-PANEL.

           MOVE LOW-VALUES       TO SP2-WD-DATA
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF

           MOVE LOW-VALUES        TO SP2-PD-DATA
           MOVE SP2-WD-NAME       TO SP2-PD-NAME
           CALL SP2            USING SP2-GET-PANEL-DEF SP2-PANEL-DEF
           MOVE SP2-PANEL-DEF     TO BUFFER
           INITIALIZE SP2-PD-CTRL-KEYS

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
           MOVE SP2-KEY-LEFT      to SP2-PD-CTRL-KEY (4)
           MOVE SP2-KEY-RIGHT     to SP2-PD-CTRL-KEY (5)
           MOVE SP2-KEY-DOWN      to SP2-PD-CTRL-KEY (6)
           MOVE SP2-KEY-UP        to SP2-PD-CTRL-KEY (7)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (8)
           MOVE 8                 TO I
           PERFORM VARYING D FROM 1 BY 1
                     UNTIL D > 21
                   IF  CHAR (D) NOT = SPACES
                       MOVE CHAR (D) TO CHAR-X
                       INSPECT CHAR-X CONVERTING
                               MINUSCULAS TO MAIUSCULAS
                       ADD  1      TO I
                       MOVE CHAR-N TO SP2-PD-CTRL-KEY (I)
                       PERFORM 040-ATIVA-ALT THRU 040-99-FIM
                       IF    CHAR-X ALPHABETIC-UPPER
                             INSPECT CHAR-X CONVERTING
                                  MAIUSCULAS TO MINUSCULAS
                             MOVE CHAR-X TO CHAR-LOW (D)
                             ADD  1      TO I
                             MOVE CHAR-N TO SP2-PD-CTRL-KEY (I)
                             PERFORM 040-ATIVA-ALT THRU 040-99-FIM
                       END-IF
                   END-IF
           END-PERFORM
           PERFORM VARYING FN FROM 1 BY 1 UNTIL FN > 26
                   IF  FUNCAO (FN) NOT = 0
                       PERFORM VARYING T FROM 1 BY 1 UNTIL T > I
                           IF  FUNCAO (FN) = SP2-PD-CTRL-KEY (T)
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                       IF T > I
                          ADD  1           TO I
                          MOVE FUNCAO (FN) TO SP2-PD-CTRL-KEY (I)
                       END-IF
                   END-IF
           END-PERFORM

           MOVE "n"               TO SP2-PD-CURS-SHOW
           CALL "CWRESE"
           CALL SP2   USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF.

       010-99-FIM. EXIT.

       020-CHECK-FUNCTION-KEY.

           MOVE CWLINE-SCREEN (I) TO TESTE
           INSPECT TESTE CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE SPACES TO TESTE2
           STRING TESTE DELIMITED BY SPACE INTO TESTE2
           IF TESTE2 (1: 5) = 'CTRL-'
              MOVE 'CONTROL-'  TO TESTE3
              MOVE TESTE2 (6:) TO TESTE3(9:)
              MOVE TESTE3      TO TESTE2
           END-IF
           IF TESTE2 (1: 2) = 'PF'
              MOVE 'F'  TO TESTE3
              MOVE TESTE2 (3:) TO TESTE3(2:)
              MOVE TESTE3      TO TESTE2
           END-IF
           IF TESTE2 (1: 2) = 'PG'
              MOVE 'PAGE'      TO TESTE3
              MOVE TESTE2 (5:) TO TESTE3(5:)
              MOVE TESTE3      TO TESTE2
           END-IF
           IF TESTE2 (1:10) = "CONTROL-PG"
              MOVE 'CONTROL-PAGE'  TO TESTE3
              MOVE TESTE2 (11:) TO TESTE3(13:)
              MOVE TESTE3      TO TESTE2
           END-IF
           PERFORM TEST AFTER UNTIL TESTE2 (1:2) NOT = 'F0'
                   MOVE TESTE2 TO TESTE3
                   IF TESTE2 (1:2) = 'F0'
                      MOVE 'F' TO TESTE3
                      MOVE TESTE2 (3:1) TO TESTE3
                      MOVE TESTE3 TO TESTE2
                   END-IF
           END-PERFORM
           PERFORM VARYING T FROM 1 BY 1 UNTIL T > 89
                                            OR FUNCAO (I) > ZERO
                   IF   TESTE2 = TECLA-NOME (T)
                        MOVE TECLA-COD (T) TO FUNCAO (I)
                   END-IF
           END-PERFORM.

       020-99-FIM. EXIT.

       040-ATIVA-ALT.

           ADD 1  TO I
           MOVE 0 TO SP2-PD-CTRL-KEY (I)
           EVALUATE CHAR (D)
               WHEN "0" MOVE  SP2-KEY-ALT-0     TO SP2-PD-CTRL-KEY (I)
               WHEN "1" MOVE  SP2-KEY-ALT-1     TO SP2-PD-CTRL-KEY (I)
               WHEN "2" MOVE  SP2-KEY-ALT-2     TO SP2-PD-CTRL-KEY (I)
               WHEN "3" MOVE  SP2-KEY-ALT-3     TO SP2-PD-CTRL-KEY (I)
               WHEN "4" MOVE  SP2-KEY-ALT-4     TO SP2-PD-CTRL-KEY (I)
               WHEN "5" MOVE  SP2-KEY-ALT-5     TO SP2-PD-CTRL-KEY (I)
               WHEN "6" MOVE  SP2-KEY-ALT-6     TO SP2-PD-CTRL-KEY (I)
               WHEN "7" MOVE  SP2-KEY-ALT-7     TO SP2-PD-CTRL-KEY (I)
               WHEN "8" MOVE  SP2-KEY-ALT-8     TO SP2-PD-CTRL-KEY (I)
               WHEN "9" MOVE  SP2-KEY-ALT-9     TO SP2-PD-CTRL-KEY (I)
               WHEN "A" MOVE  SP2-KEY-ALT-A     TO SP2-PD-CTRL-KEY (I)
               WHEN "B" MOVE  SP2-KEY-ALT-B     TO SP2-PD-CTRL-KEY (I)
               WHEN "C" MOVE  SP2-KEY-ALT-C     TO SP2-PD-CTRL-KEY (I)
               WHEN "D" MOVE  SP2-KEY-ALT-D     TO SP2-PD-CTRL-KEY (I)
               WHEN "E" MOVE  SP2-KEY-ALT-E     TO SP2-PD-CTRL-KEY (I)
               WHEN "=" MOVE  SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (I)
               WHEN "F" MOVE  SP2-KEY-ALT-F     TO SP2-PD-CTRL-KEY (I)
               WHEN "G" MOVE  SP2-KEY-ALT-G     TO SP2-PD-CTRL-KEY (I)
               WHEN "H" MOVE  SP2-KEY-ALT-H     TO SP2-PD-CTRL-KEY (I)
               WHEN "I" MOVE  SP2-KEY-ALT-I     TO SP2-PD-CTRL-KEY (I)
               WHEN "J" MOVE  SP2-KEY-ALT-J     TO SP2-PD-CTRL-KEY (I)
               WHEN "K" MOVE  SP2-KEY-ALT-K     TO SP2-PD-CTRL-KEY (I)
               WHEN "L" MOVE  SP2-KEY-ALT-L     TO SP2-PD-CTRL-KEY (I)
               WHEN "M" MOVE  SP2-KEY-ALT-M     TO SP2-PD-CTRL-KEY (I)
               WHEN "-" MOVE  SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (I)
               WHEN "N" MOVE  SP2-KEY-ALT-N     TO SP2-PD-CTRL-KEY (I)
               WHEN "O" MOVE  SP2-KEY-ALT-O     TO SP2-PD-CTRL-KEY (I)
               WHEN "P" MOVE  SP2-KEY-ALT-P     TO SP2-PD-CTRL-KEY (I)
               WHEN "Q" MOVE  SP2-KEY-ALT-Q     TO SP2-PD-CTRL-KEY (I)
               WHEN "R" MOVE  SP2-KEY-ALT-R     TO SP2-PD-CTRL-KEY (I)
               WHEN "S" MOVE  SP2-KEY-ALT-S     TO SP2-PD-CTRL-KEY (I)
               WHEN "T" MOVE  SP2-KEY-ALT-T     TO SP2-PD-CTRL-KEY (I)
               WHEN "U" MOVE  SP2-KEY-ALT-U     TO SP2-PD-CTRL-KEY (I)
               WHEN "V" MOVE  SP2-KEY-ALT-V     TO SP2-PD-CTRL-KEY (I)
               WHEN "W" MOVE  SP2-KEY-ALT-W     TO SP2-PD-CTRL-KEY (I)
               WHEN "X" MOVE  SP2-KEY-ALT-X     TO SP2-PD-CTRL-KEY (I)
               WHEN "Y" MOVE  SP2-KEY-ALT-Y     TO SP2-PD-CTRL-KEY (I)
               WHEN "Z" MOVE  SP2-KEY-ALT-Z     TO SP2-PD-CTRL-KEY (I)
           END-EVALUATE

           IF SP2-PD-CTRL-KEY (I) = 0
              SUBTRACT 1 FROM I
           END-IF.

       040-99-FIM. EXIT.
       END PROGRAM CWLINE.

