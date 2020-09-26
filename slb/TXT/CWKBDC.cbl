       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWKBDC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/04/1996.
       SECURITY.      *************************************************
                      *                                               *
                      * Le o teclado substituindo a CBL_READ_KBD_CHAR *
                      * possibilitando o reconhecimento de teclas de  *
                      * forma independente do teminal Unix via ADIS   *
                      * Uso interno do COBOLware                      *
                      *                                               *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CWGETL                 PIC  X(003) VALUE SPACES.
           05 CRON                   PIC  X(003) VALUE SPACES.
           05 HORA-A                 PIC  X(006) VALUE SPACES.
           05 HORA-B                 PIC  X(006) VALUE SPACES.
           05 SEGUNDOS               PIC  9(004) VALUE 0.
           05 KEYBOARD-STATUS        PIC  9(002) COMP-X VALUE 0.
           05 CWACCENTCONTROL        PIC  X(003) VALUE SPACES.
           05 TECLA                  PIC  9(003) VALUE 0. COPY CWEDIT.
           05 TECLA-MS               PIC  9(003) VALUE 0. COPY CWKEYS.
           05 MIL             COMP-X PIC  9(008) VALUE 1000.
           05 REPINS                 PIC  9(001) VALUE 0.
              88 INSERT-OFF                      VALUE 0.
              88 INSERT-ON                       VALUE 1.
           05 BUF                    PIC  X(003) VALUE LOW-VALUES.
           05 CWACCENT               PIC  X(003) VALUE SPACES.
           05 CWCASE                 PIC  X(003) VALUE SPACES.
           05 PIC3                   PIC  9(003) VALUE 0.
           05 ACENTO                 PIC  X(001) VALUE SPACE.
           05 KEY-BUFFER             PIC  X(009) VALUE SPACES.
           05 X91-RESULT      COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION    COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER   COMP-X PIC  9(002) VALUE 0.
           05 XAF-FUNCTION           PIC  9(002) COMP-X VALUE 26.
           05 LX                     PIC  9(002) VALUE 0.
           05 CX                     PIC  9(002) VALUE 0.
           05 ECHOKEY                PIC  X(003) VALUE SPACES.
           05 TECLA-2                PIC  9(003) VALUE 0.
           05 KEY-STATUS.
              10 KEY-TYPE            PIC  X(001).
              10 KEY-CODE-1          PIC  9(002) COMP-X.
              10 DATA-ITEM.
                 88 ACENTUADO VALUE " " "‚" "¡" "¢" "£" "µ" "" "Ö" "à"
                                    "é" "ƒ" "ˆ" "Œ" "“" "–" "¶" "Ò" "×"
                                    "â" "ê" "…" "Š" "" "•" "—" "·" "Ô"
                                    "Þ" "ã" "ë" "Æ" "ä" "Ç" "å" "„" "‰"
                                    "‹" "”" "" "µ" "" "‹" "™" "š"
                                    "¤" "¥" "‡" "€".
                 15 KEY-CODE-2       PIC  9(002) COMP-X.
           05 REDEFINES KEY-STATUS.
              10 KEY-STRING          PIC  X(002).
                 88 KEY-TAB                  VALUE X"3208".
                 88 KEY-ENTER                VALUE X"300A".
                 88 KEY-SHIFT-TAB            VALUE X"3209".
                 88 KEY-ESC                  VALUE X"3100".
                 88 KEY-F1                   VALUE X"3101".
                 88 KEY-F2                   VALUE X"3102".
                 88 KEY-F3                   VALUE X"3103".
                 88 KEY-F4                   VALUE X"3104".
                 88 KEY-F5                   VALUE X"3105".
                 88 KEY-F6                   VALUE X"3106".
                 88 KEY-F7                   VALUE X"3107".
                 88 KEY-F8                   VALUE X"3108".
                 88 KEY-F9                   VALUE X"3109".
                 88 KEY-F10                  VALUE X"310A".
                 88 KEY-HOME                 VALUE X"3207".
                 88 KEY-CURSOR-UP            VALUE X"320C".
                 88 KEY-PAGE-UP              VALUE X"3149"
                                                   X"3500"
                                                   X"49FF".
                 88 KEY-CURSOR-LEFT          VALUE X"3203".
                 88 KEY-CURSOR-RIGHT         VALUE X"3204".
                 88 KEY-END                  VALUE X"320A".
                 88 KEY-CURSOR-DOWN          VALUE X"3030"
                                                   X"3600"
                                                   X"51FF".
                 88 KEY-PAGE-DOWN            VALUE X"3151".
                 88 KEY-INSERT               VALUE X"3217".
                 88 KEY-DEL                  VALUE X"3211".
                 88 KEY-SHIFT-F1             VALUE X"310B".
                 88 KEY-SHIFT-F2             VALUE X"310C".
                 88 KEY-SHIFT-F3             VALUE X"310D".
                 88 KEY-SHIFT-F4             VALUE X"310E".
                 88 KEY-SHIFT-F5             VALUE X"310F".
                 88 KEY-SHIFT-F6             VALUE X"3110".
                 88 KEY-SHIFT-F7             VALUE X"3111".
                 88 KEY-SHIFT-F8             VALUE X"3112".
                 88 KEY-SHIFT-F9             VALUE X"3113".
                 88 KEY-SHIFT-F10            VALUE X"3114".
                 88 KEY-CONTROL-F1           VALUE X"3115".
                 88 KEY-CONTROL-F2           VALUE X"3116".
                 88 KEY-CONTROL-F3           VALUE X"3117".
                 88 KEY-CONTROL-F4           VALUE X"3118".
                 88 KEY-CONTROL-F5           VALUE X"3119".
                 88 KEY-CONTROL-F6           VALUE X"311A".
                 88 KEY-CONTROL-F7           VALUE X"311B".
                 88 KEY-CONTROL-F8           VALUE X"311C".
                 88 KEY-CONTROL-F9           VALUE X"311D".
                 88 KEY-CONTROL-F10          VALUE X"311E".
                 88 KEY-ALT-F1               VALUE X"311F".
                 88 KEY-ALT-F2               VALUE X"3120".
                 88 KEY-ALT-F3               VALUE X"3121".
                 88 KEY-ALT-F4               VALUE X"3122".
                 88 KEY-ALT-F5               VALUE X"3123".
                 88 KEY-ALT-F6               VALUE X"3124".
                 88 KEY-ALT-F7               VALUE X"3125".
                 88 KEY-ALT-F8               VALUE X"3126".
                 88 KEY-ALT-F9               VALUE X"3127".
                 88 KEY-ALT-F10              VALUE X"3128".
                 88 KEY-CONTROL-CURSOR-LEFT  VALUE X"3239" X"3227".
      *          88 KEY-CONTROL-CURSOR-RIGHT VALUE X"3238".
                 88 KEY-CONTROL-PAGE-DOWN    VALUE X"3138".
                 88 KEY-CONTROL-PAGE-UP      VALUE X"3137".
                 88 KEY-F11                  VALUE X"315B".
                 88 KEY-F12                  VALUE X"315C".
                 88 KEY-SHIFT-F11            VALUE X"315D".
                 88 KEY-SHIFT-F12            VALUE X"315E".
                 88 KEY-CONTROL-F11          VALUE X"315F".
                 88 KEY-CONTROL-F12          VALUE X"3160".
                 88 KEY-ALT-F11              VALUE X"3161".
                 88 KEY-ALT-F12              VALUE X"3162".
      *          88 KEY-BACKSPACE            VALUE X"320E".
              10 FILLER              PIC  X(001).
           05 REDEFINES KEY-STATUS PIC X(003).
              88 KEY-CONTROL-HOME         VALUE X"321600".
              88 KEY-CONTROL-END          VALUE X"321400".
              88 KEY-CONTROL-CURSOR-RIGHT VALUE X"320800" X"322600".
              88 KEY-ALT-1                VALUE X"312900".
              88 KEY-ALT-2                VALUE X"312A00".
              88 KEY-ALT-3                VALUE X"312B00".
              88 KEY-ALT-4                VALUE X"312C00".
              88 KEY-ALT-5                VALUE X"312D00".
              88 KEY-ALT-6                VALUE X"312E00".
              88 KEY-ALT-7                VALUE X"312F00".
              88 KEY-ALT-8                VALUE X"313000".
              88 KEY-ALT-9                VALUE X"313100".
              88 KEY-ALT-0                VALUE X"313200".
              88 KEY-ALT-TRACE            VALUE X"313300".
              88 KEY-ALT-EQUAL            VALUE X"313400".
              88 KEY-ALT-A                VALUE X"314100".
              88 KEY-ALT-B                VALUE X"314200".
              88 KEY-ALT-C                VALUE X"314300".
              88 KEY-ALT-D                VALUE X"314400".
              88 KEY-ALT-E                VALUE X"314500".
              88 KEY-ALT-F                VALUE X"314600".
              88 KEY-ALT-G                VALUE X"314700".
              88 KEY-ALT-H                VALUE X"314800".
              88 KEY-ALT-I                VALUE X"314900".
              88 KEY-ALT-J                VALUE X"314A00".
              88 KEY-ALT-K                VALUE X"314B00".
              88 KEY-ALT-L                VALUE X"314C00".
              88 KEY-ALT-M                VALUE X"314D00".
              88 KEY-ALT-N                VALUE X"314E00".
              88 KEY-ALT-O                VALUE X"314F00".
              88 KEY-ALT-P                VALUE X"315000".
              88 KEY-ALT-Q                VALUE X"315100".
              88 KEY-ALT-R                VALUE X"315200".
              88 KEY-ALT-S                VALUE X"315300".
              88 KEY-ALT-T                VALUE X"315400".
              88 KEY-ALT-U                VALUE X"315500".
              88 KEY-ALT-V                VALUE X"315600".
              88 KEY-ALT-W                VALUE X"315700".
              88 KEY-ALT-X                VALUE X"315800".
              88 KEY-ALT-Y                VALUE X"315900".
              88 KEY-ALT-Z                VALUE X"315A00".
              88 KEY-BACKSPACE            VALUE X"320E08" X"320EFF".
              88 KEY-CONTROL-X            VALUE X"321418".
              88 KEY-CONTROL-Z            VALUE X"32131A".
              88 KEY-CONTROL-R            VALUE X"321212".
              88 KEY-CONTROL-O            VALUE X"32100F".
              88 KEY-CONTROL-U            VALUE X"331515".
              88 KEY-CONTROL-L            VALUE X"330C0C".
              88 KEY-CONTROL-F            VALUE X"320D06".
              88 KEY-CONTROL-V            VALUE X"331616".
              88 KEY-HAT                  VALUE X"311500"
                                                X"337E7E"
                                                X"335E5E"
                                                X"332727"
                                                X"336060"
                                                X"332222".
              88 KEY-TIL                  VALUE X"337E7E".
              88 KEY-CIRCUNFLEXO          VALUE X"335E5E".
              88 KEY-AGUDO                VALUE X"332727".
              88 KEY-CRASE                VALUE X"336060".
              88 KEY-TREMA                VALUE X"332222".
              88 KEY-ACENTO               VALUE X"337E7E"
                                                X"335E5E"
                                                X"332727"
                                                X"336060"
                                                X"332222".
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC  X(001) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC  9(002) COMP-X VALUE 0.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 1.

       COPY CWUNIX.
       COPY CWGETL.

       LINKAGE SECTION.

       01  CURPOS.
           05 CURPOS-LIN PIC  9(002).
           05 CURPOS-COL PIC  9(002).

       01  CARACTER      PIC  X(001).
       01  TECLA-EDIT    PIC  9(003).
       01  LK-STATUS     PIC  X(003).
       01  MODO-ACCEPT   PIC  X(001).
           88 ALFABETICO      VALUE "A" "B" "?".
       01  INSERT-QUERY PIC X(001).

       PROCEDURE DIVISION USING CURPOS CARACTER  TECLA-EDIT
                                       LK-STATUS MODO-ACCEPT
                                       INSERT-QUERY.

       000-INICIO.

           IF   CURPOS-COL > 90
           OR   CURPOS-LIN > 90
                GOBACK
           END-IF

           ON   1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                DISPLAY "CWACCENTCONTROL" UPON ENVIRONMENT-NAME
                ACCEPT CWACCENTCONTROL FROM ENVIRONMENT-VALUE
                INSPECT CWACCENTCONTROL CONVERTING "ofn" TO "OFN"
                DISPLAY "CWECHOKEY" UPON ENVIRONMENT-NAME
                ACCEPT ECHOKEY FROM ENVIRONMENT-VALUE
                INSPECT ECHOKEY CONVERTING "ofn" TO "OFN"
                DISPLAY "CWCASE-INPUT" UPON ENVIRONMENT-NAME
                ACCEPT CWCASE FROM ENVIRONMENT-VALUE
                IF  CWCASE = SPACES
                    DISPLAY "CWCASE_INPUT" UPON ENVIRONMENT-NAME
                    ACCEPT CWCASE FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
                ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
                INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER > 2
           AND  TECLA-EDIT > 990
                MOVE 0 TO TECLA-EDIT
                GOBACK
           END-IF
           IF   CURPOS = "0000"
                IF   CWUNIX-ON
                     MOVE "0202" TO CURPOS
                ELSE
                     CALL "CBL_SET_CSR_POS" USING X"FFFF"
                END-IF
           END-IF
           IF   CURPOS NOT = "0000"
                COMPUTE ROW-NUMBER    = CURPOS-LIN - 1
                COMPUTE COLUMN-NUMBER = CURPOS-COL - 1
                CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION
           ELSE
                MOVE X"0000" TO SCREEN-POSITION
           END-IF
     **    IF  TECLA NOT = 98
               CALL "CWATCH"
     **    END-IF
           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH.

       kbuffer.
           MOVE X"00" TO DATA-ITEM
           ON 1
                DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
                ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
                IF CWGETL NOT = 'OFF'
                   CALL "CWGETL" USING PARAMETROS-CWGETL
                END-IF.
           PERFORM TEST AFTER UNTIL((NOT KEY-HAT)
                                AND (NOT KEY-TYPE = 9))
                                 OR X91-PARAMETER = 4
                   IF  BUF = LOW-VALUES
                       IF CWUNIX-ON
                          CALL "UNIKBD" USING KEY-STATUS
                       ELSE
                          MOVE LOW-VALUES TO SEGUNDOS(1:)
                          IF CWGETL-TIMEOUT > 0
                          AND X91-PARAMETER < 4
                             ACCEPT HORA-A FROM TIME
                             PERFORM TEST AFTER
                                    UNTIL KEYBOARD-STATUS = 1
                                CALL "CBL_GET_KBD_STATUS"
                                      USING KEYBOARD-STATUS
                                CALL "CWATCH"
                                ACCEPT HORA-B FROM TIME
                                IF HORA-B NOT = HORA-A
                                   MOVE HORA-B TO HORA-A
                                   ADD 1 TO SEGUNDOS
                                   IF SEGUNDOS NOT < CWGETL-TIMEOUT
                                      MOVE 1 TO KEYBOARD-STATUS
                                      MOVE HIGH-VALUES TO SEGUNDOS(1:)
                                   END-IF
                                END-IF
                             END-PERFORM
                          END-IF
                          IF SEGUNDOS(1:) = HIGH-VALUES
                             MOVE X"310000" TO KEY-STATUS
                          ELSE
                             CALL "¯" USING XAF-FUNCTION KEY-STATUS
                          END-IF
                       END-IF
                       CALL "CWCRTS" USING "S" KEY-STATUS
                   ELSE
                       MOVE BUF        TO KEY-STATUS
                       MOVE LOW-VALUES TO BUF
                       CALL "CWKBST" USING X"00"
                   END-IF
                   IF  KEY-ACENTO
                   AND (CWACCENTCONTROL NOT = 'OFF')
                       IF  ACENTO = SPACE
                           MOVE DATA-ITEM TO ACENTO
                           IF  X91-PARAMETER > 4
                           AND ALFABETICO
                               MOVE ACENTO TO CARACTER-BUFFER
                               CALL "CBL_WRITE_SCR_CHARS" USING
                                                      SCREEN-POSITION
                                                      ACENTO
                                                      STRING-LENGTH
                           END-IF
                       ELSE
                           MOVE 3          TO KEY-TYPE
                           MOVE ACENTO     TO DATA-ITEM
                           MOVE KEY-CODE-2 TO KEY-CODE-1
                           MOVE SPACE      TO ACENTO
                       END-IF
                   END-IF
                   CALL "CWATCH"
           END-PERFORM
           IF  (KEY-TYPE = 3
           AND  KEY-CODE-1 = 16
           AND  KEY-CODE-2 = 16)
           OR  (CWUNIX-ON
           AND KEY-TYPE = 2
           AND KEY-CODE-1 = 12
           AND KEY-CODE-2 = 16)
                INITIALIZE KEY-STATUS
                CALL "CWPRTS"
                CANCEL "CWPRTS"
                go to kbuffer
           END-IF
           IF  ACENTO NOT = SPACE
               IF  KEY-TYPE NOT = 3
                   MOVE 3          TO KEY-TYPE
                   MOVE ACENTO     TO DATA-ITEM
                   MOVE KEY-CODE-2 TO KEY-CODE-1
               ELSE
                   EVALUATE ACENTO ALSO DATA-ITEM
                       WHEN "~" ALSO "n" MOVE "¤" TO DATA-ITEM
                       WHEN "~" ALSO "N" MOVE "¥" TO DATA-ITEM
                       WHEN "'" ALSO "c" MOVE "‡" TO DATA-ITEM
                       WHEN "'" ALSO "C" MOVE "€" TO DATA-ITEM
                       WHEN "'" ALSO "a" MOVE " " TO DATA-ITEM
                       WHEN "'" ALSO "e" MOVE "‚" TO DATA-ITEM
                       WHEN "'" ALSO "i" MOVE "¡" TO DATA-ITEM
                       WHEN "'" ALSO "o" MOVE "¢" TO DATA-ITEM
                       WHEN "'" ALSO "u" MOVE "£" TO DATA-ITEM
                       WHEN "'" ALSO "y" MOVE "ì" TO DATA-ITEM
                       WHEN "'" ALSO "A" MOVE "µ" TO DATA-ITEM
                       WHEN "'" ALSO "E" MOVE "" TO DATA-ITEM
                       WHEN "'" ALSO "I" MOVE "Ö" TO DATA-ITEM
                       WHEN "'" ALSO "O" MOVE "à" TO DATA-ITEM
                       WHEN "'" ALSO "U" MOVE "é" TO DATA-ITEM
                       WHEN "'" ALSO "Y" MOVE "í" TO DATA-ITEM
                       WHEN "^" ALSO "a" MOVE "ƒ" TO DATA-ITEM
                       WHEN "^" ALSO "e" MOVE "ˆ" TO DATA-ITEM
                       WHEN "^" ALSO "i" MOVE "Œ" TO DATA-ITEM
                       WHEN "^" ALSO "o" MOVE "“" TO DATA-ITEM
                       WHEN "^" ALSO "u" MOVE "–" TO DATA-ITEM
                       WHEN "^" ALSO "A" MOVE "¶" TO DATA-ITEM
                       WHEN "^" ALSO "E" MOVE "Ò" TO DATA-ITEM
                       WHEN "^" ALSO "I" MOVE "×" TO DATA-ITEM
                       WHEN "^" ALSO "O" MOVE "â" TO DATA-ITEM
                       WHEN "^" ALSO "U" MOVE "ê" TO DATA-ITEM
                       WHEN "`" ALSO "a" MOVE "…" TO DATA-ITEM
                       WHEN "`" ALSO "e" MOVE "Š" TO DATA-ITEM
                       WHEN "`" ALSO "i" MOVE "" TO DATA-ITEM
                       WHEN "`" ALSO "o" MOVE "•" TO DATA-ITEM
                       WHEN "`" ALSO "u" MOVE "—" TO DATA-ITEM
                       WHEN "`" ALSO "A" MOVE "·" TO DATA-ITEM
                       WHEN "`" ALSO "E" MOVE "Ô" TO DATA-ITEM
                       WHEN "`" ALSO "I" MOVE "Þ" TO DATA-ITEM
                       WHEN "`" ALSO "O" MOVE "ã" TO DATA-ITEM
                       WHEN "`" ALSO "U" MOVE "ë" TO DATA-ITEM
                       WHEN "~" ALSO "a" MOVE "Æ" TO DATA-ITEM
                       WHEN "~" ALSO "o" MOVE "ä" TO DATA-ITEM
                       WHEN "~" ALSO "A" MOVE "Ç" TO DATA-ITEM
                       WHEN "~" ALSO "A" MOVE "å" TO DATA-ITEM
                       WHEN '"' ALSO "a" MOVE "„" TO DATA-ITEM
                       WHEN '"' ALSO "e" MOVE "‰" TO DATA-ITEM
                       WHEN '"' ALSO "i" MOVE "‹" TO DATA-ITEM
                       WHEN '"' ALSO "o" MOVE "”" TO DATA-ITEM
                       WHEN '"' ALSO "u" MOVE "" TO DATA-ITEM
                       WHEN '"' ALSO "A" MOVE "µ" TO DATA-ITEM
                       WHEN '"' ALSO "E" MOVE "Ó" TO DATA-ITEM
                       WHEN '"' ALSO "I" MOVE "‹" TO DATA-ITEM
                       WHEN '"' ALSO "O" MOVE "™" TO DATA-ITEM
                       WHEN '"' ALSO "U" MOVE "š" TO DATA-ITEM
                       WHEN OTHER
                            IF  NOT ACENTUADO
                                MOVE KEY-STATUS  TO BUF
                                MOVE ACENTO      TO DATA-ITEM
                                CALL "CWKBST" USING X"FF"
                            END-IF
                   END-EVALUATE
                   MOVE 3          TO KEY-TYPE
                   MOVE KEY-CODE-2 TO KEY-CODE-1
               END-IF
           END-IF
           MOVE SPACES TO ACENTO
           IF   CWACCENT = "OFF"
                INSPECT DATA-ITEM CONVERTING ACENTOS-850 TO ACENTOS-OFF
           END-IF
           IF   CWCASE = "LOW"
                INSPECT DATA-ITEM CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWCASE = "UPP"
                INSPECT DATA-ITEM CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
      *    IF ((SCREEN-POSITION NOT = X"0000")
      *    AND (SCREEN-POSITION NOT = X"FFFF"))
      *    AND (DATA-ITEM NOT = X"00")
      *        MOVE DATA-ITEM TO CARACTER-BUFFER
      *        CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
      *                                           CARACTER-BUFFER
      *                                           ATTRIBUTE-BUFFER
      *                                           STRING-LENGTH
      *    END-IF
           IF  (KEY-STATUS NOT = X'321700')
           AND (KEY-STATUS NOT = '3RR')
                CALL "CBL_SET_CSR_POS" USING X"FFFF"
           END-IF
           MOVE X"00" TO CARACTER
           MOVE 0     TO TECLA
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
           END-IF
           IF   X91-PARAMETER > 3
                MOVE KEY-STATUS TO LK-STATUS
                IF  LK-STATUS = X"32000A"
                    MOVE X"320B0D" TO LK-STATUS
                END-IF
           END-IF
           MOVE 0 TO TECLA-MS
           EVALUATE TRUE
               WHEN CWUNIX-ON
                AND KEY-TYPE = 1
                AND KEY-CODE-1 = 081
                AND KEY-CODE-2 = 255
                    SET EDIT-PAGE-DOWN    TO TRUE
                    SET      PAGE-DOWN    TO TRUE
               WHEN CWUNIX-ON
                AND KEY-TYPE = 1
                AND KEY-CODE-1 = 073
                AND KEY-CODE-2 = 255
                    SET EDIT-PAGE-UP      TO TRUE
                    SET      PAGE-UP      TO TRUE
               WHEN CWUNIX-ON
                AND KEY-TYPE = 1
                AND KEY-CODE-1 = 012
                AND KEY-CODE-2 = 255
                    SET EDIT-F12                  TO TRUE
                    SET      F12                  TO TRUE
               WHEN CWUNIX-ON
                AND KEY-TYPE = 1
                AND KEY-CODE-1 = 011
                AND KEY-CODE-2 = 255
                    SET EDIT-F11                  TO TRUE
                    SET      F11                  TO TRUE
               WHEN KEY-ALT-1     SET EDIT-ALT-1      TO TRUE
                                  SET      ALT-1      TO TRUE
               WHEN KEY-ALT-2     SET EDIT-ALT-2      TO TRUE
                                  SET      ALT-2      TO TRUE
               WHEN KEY-ALT-3     SET EDIT-ALT-3      TO TRUE
                                  SET      ALT-3      TO TRUE
               WHEN KEY-ALT-4     SET EDIT-ALT-4      TO TRUE
                                  SET      ALT-4      TO TRUE
               WHEN KEY-ALT-5     SET EDIT-ALT-5      TO TRUE
                                  SET      ALT-5      TO TRUE
               WHEN KEY-ALT-6     SET EDIT-ALT-6      TO TRUE
                                  SET      ALT-6      TO TRUE
               WHEN KEY-ALT-7     SET EDIT-ALT-7      TO TRUE
                                  SET      ALT-7      TO TRUE
               WHEN KEY-ALT-8     SET EDIT-ALT-8      TO TRUE
                                  SET      ALT-8      TO TRUE
               WHEN KEY-ALT-9     SET EDIT-ALT-9      TO TRUE
                                  SET      ALT-9      TO TRUE
               WHEN KEY-ALT-0     SET EDIT-ALT-0      TO TRUE
                                  SET      ALT-0      TO TRUE
               WHEN KEY-ALT-TRACE SET EDIT-ALT-TRACE  TO TRUE
                                  SET      ALT-TRACE  TO TRUE
               WHEN KEY-ALT-EQUAL SET EDIT-ALT-EQUAL  TO TRUE
                                  SET      ALT-EQUAL  TO TRUE
               WHEN KEY-ALT-A     SET EDIT-ALT-A      TO TRUE
                                  SET      ALT-A      TO TRUE
               WHEN KEY-ALT-B     SET EDIT-ALT-B      TO TRUE
                                  SET      ALT-B      TO TRUE
               WHEN KEY-ALT-C     SET EDIT-ALT-C      TO TRUE
                                  SET      ALT-C      TO TRUE
               WHEN KEY-ALT-D     SET EDIT-ALT-D      TO TRUE
                                  SET      ALT-D      TO TRUE
               WHEN KEY-ALT-E     SET EDIT-ALT-E      TO TRUE
                                  SET      ALT-E      TO TRUE
               WHEN KEY-ALT-F     SET EDIT-ALT-F      TO TRUE
                                  SET      ALT-F      TO TRUE
               WHEN KEY-ALT-G     SET EDIT-ALT-G      TO TRUE
                                  SET      ALT-G      TO TRUE
               WHEN KEY-ALT-H     SET EDIT-ALT-H      TO TRUE
                                  SET      ALT-H      TO TRUE
               WHEN KEY-ALT-I     SET EDIT-ALT-I      TO TRUE
                                  SET      ALT-I      TO TRUE
               WHEN KEY-ALT-J     SET EDIT-ALT-J      TO TRUE
                                  SET      ALT-J      TO TRUE
               WHEN KEY-ALT-K     SET EDIT-ALT-K      TO TRUE
                                  SET      ALT-K      TO TRUE
               WHEN KEY-ALT-L     SET EDIT-ALT-L      TO TRUE
                                  SET      ALT-L      TO TRUE
               WHEN KEY-ALT-M     SET EDIT-ALT-M      TO TRUE
                                  SET      ALT-M      TO TRUE
               WHEN KEY-ALT-N     SET EDIT-ALT-N      TO TRUE
                                  SET      ALT-N      TO TRUE
               WHEN KEY-ALT-O     SET EDIT-ALT-O      TO TRUE
                                  SET      ALT-O      TO TRUE
               WHEN KEY-ALT-P     SET EDIT-ALT-P      TO TRUE
                                  SET      ALT-P      TO TRUE
               WHEN KEY-ALT-Q     SET EDIT-ALT-Q      TO TRUE
                                  SET      ALT-Q      TO TRUE
               WHEN KEY-ALT-R     SET EDIT-ALT-R      TO TRUE
                                  SET      ALT-R      TO TRUE
               WHEN KEY-ALT-S     SET EDIT-ALT-S      TO TRUE
                                  SET      ALT-S      TO TRUE
               WHEN KEY-ALT-T     SET EDIT-ALT-T      TO TRUE
                                  SET      ALT-T      TO TRUE
               WHEN KEY-ALT-U     SET EDIT-ALT-U      TO TRUE
                                  SET      ALT-U      TO TRUE
               WHEN KEY-ALT-V     SET EDIT-ALT-V      TO TRUE
                                  SET      ALT-V      TO TRUE
               WHEN KEY-ALT-W     SET EDIT-ALT-W      TO TRUE
                                  SET      ALT-W      TO TRUE
               WHEN KEY-ALT-X     SET EDIT-ALT-X      TO TRUE
                                  SET      ALT-X      TO TRUE
               WHEN KEY-ALT-Y     SET EDIT-ALT-Y      TO TRUE
                                  SET      ALT-Y      TO TRUE
               WHEN KEY-ALT-Z     SET EDIT-ALT-Z      TO TRUE
                                  SET      ALT-Z      TO TRUE
               WHEN KEY-BACKSPACE SET EDIT-BACKSPACE  TO TRUE
               WHEN KEY-CONTROL-X SET EDIT-CONTROL-X  TO TRUE
               WHEN KEY-CONTROL-Z SET EDIT-CONTROL-Z  TO TRUE
               WHEN KEY-CONTROL-R SET EDIT-CONTROL-R  TO TRUE
               WHEN KEY-CONTROL-O SET EDIT-CONTROL-O  TO TRUE
               WHEN KEY-CONTROL-L SET EDIT-CONTROL-L  TO TRUE
               WHEN KEY-CONTROL-U SET EDIT-CONTROL-U  TO TRUE
               WHEN KEY-CONTROL-F SET EDIT-CONTROL-F  TO TRUE
               WHEN KEY-CONTROL-V SET EDIT-CONTROL-V  TO TRUE
               WHEN KEY-CONTROL-CURSOR-LEFT
                 OR (KEY-TYPE = 2
                AND KEY-CODE-1 = 009 OR 39
                AND KEY-CODE-2 = 000)
                    SET EDIT-CONTROL-CURSOR-LEFT  TO TRUE
               WHEN KEY-CONTROL-CURSOR-RIGHT
                    SET EDIT-CONTROL-CURSOR-RIGHT TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 011
                AND KEY-CODE-2 = 009
                    SET EDIT-TAB                  TO TRUE
                    SET      TAB                  TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 016
                AND KEY-CODE-2 = 255
                    SET EDIT-INSERT               TO TRUE
                    PERFORM 010-REPINS THRU 010-99-FIM
                    IF   X91-PARAMETER > 5
                         go to kbuffer
                    END-IF
               WHEN KEY-TYPE = 0
                AND KEY-CODE-2 = 10 OR 13
                    SET EDIT-ENTER                TO TRUE
                    SET      ENTER-KEY            TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 000
                AND KEY-CODE-2 = 010
                    SET EDIT-ENTER                TO TRUE
                    SET      ENTER-KEY            TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 011
                    SET EDIT-CURSOR-DOWN          TO TRUE
                    SET      CURSOR-DOWN          TO TRUE
               WHEN KEY-TYPE = 1
                AND KEY-CODE-1 = 053
                AND KEY-CODE-2 = 000
                    SET EDIT-PAGE-UP              TO TRUE
                    SET      PAGE-UP              TO TRUE
               WHEN KEY-TYPE = 1
                AND KEY-CODE-1 = 054
                AND KEY-CODE-2 = 000
                    SET EDIT-PAGE-DOWN            TO TRUE
                    SET      PAGE-DOWN            TO TRUE
               WHEN KEY-TYPE = 1
                AND KEY-CODE-1 = 073
                AND KEY-CODE-2 = 255
                    SET EDIT-PAGE-UP              TO TRUE
                    SET      PAGE-UP              TO TRUE
               WHEN KEY-TYPE = 1
                AND KEY-CODE-1 = 081
                AND KEY-CODE-2 = 255
                    SET EDIT-PAGE-DOWN            TO TRUE
                    SET      PAGE-DOWN            TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 020
                AND KEY-CODE-2 = 005
                    SET EDIT-END                  TO TRUE
               WHEN KEY-TYPE = 2
                AND KEY-CODE-1 = 020
                AND KEY-CODE-2 = 255
                    SET EDIT-END                  TO TRUE
               WHEN KEY-PAGE-UP
                    SET EDIT-PAGE-UP              TO TRUE
                    SET      PAGE-UP              TO TRUE
               WHEN KEY-TAB
                    SET EDIT-TAB                  TO TRUE
                    SET      TAB                  TO TRUE
               WHEN KEY-ENTER
                    SET EDIT-ENTER                TO TRUE
                    SET      ENTER-KEY            TO TRUE
               WHEN KEY-SHIFT-TAB
                    SET EDIT-SHIFT-TAB            TO TRUE
                    SET      SHIFT-TAB            TO TRUE
               WHEN KEY-ESC
                    SET EDIT-ESC                  TO TRUE
                    SET      ESC                  TO TRUE
               WHEN KEY-F1
                    SET EDIT-F1                   TO TRUE
                    SET      F1                   TO TRUE
               WHEN KEY-F2
                    SET EDIT-F2                   TO TRUE
                    SET      F2                   TO TRUE
               WHEN KEY-F3
                    SET EDIT-F3                   TO TRUE
                    SET      F3                   TO TRUE
               WHEN KEY-F4
                    SET EDIT-F4                   TO TRUE
                    SET      F4                   TO TRUE
               WHEN KEY-F5
                    SET EDIT-F5                   TO TRUE
                    SET      F5                   TO TRUE
               WHEN KEY-F6
                    SET EDIT-F6                   TO TRUE
                    SET      F6                   TO TRUE
               WHEN KEY-F7
                    SET EDIT-F7                   TO TRUE
                    SET      F7                   TO TRUE
               WHEN KEY-F8
                    SET EDIT-F8                   TO TRUE
                    SET      F8                   TO TRUE
               WHEN KEY-F9
                    SET EDIT-F9                   TO TRUE
                    SET      F9                   TO TRUE
               WHEN KEY-F10
                    SET EDIT-F10                  TO TRUE
                    SET      F10                  TO TRUE
               WHEN KEY-HOME
                    SET EDIT-HOME                 TO TRUE
               WHEN KEY-CURSOR-UP
                    SET EDIT-CURSOR-UP            TO TRUE
                    SET      CURSOR-UP            TO TRUE
               WHEN KEY-PAGE-UP
                    SET EDIT-PAGE-UP              TO TRUE
                    SET      PAGE-UP              TO TRUE
               WHEN KEY-CURSOR-LEFT
                    SET EDIT-CURSOR-LEFT          TO TRUE
               WHEN KEY-CURSOR-RIGHT
                    SET EDIT-CURSOR-RIGHT         TO TRUE
               WHEN KEY-END
                    SET EDIT-END                  TO TRUE
               WHEN KEY-CURSOR-DOWN
                    SET EDIT-CURSOR-DOWN          TO TRUE
                    SET      CURSOR-DOWN          TO TRUE
               WHEN KEY-PAGE-DOWN
                    SET EDIT-PAGE-DOWN            TO TRUE
                    SET      PAGE-DOWN            TO TRUE
               WHEN KEY-INSERT
                    SET EDIT-INSERT               TO TRUE
                    PERFORM 010-REPINS THRU 010-99-FIM
                    IF   X91-PARAMETER > 5
                         go to kbuffer
                    END-IF
               WHEN KEY-DEL
                    SET EDIT-DEL                  TO TRUE
               WHEN KEY-SHIFT-F1
                    SET EDIT-SHIFT-F1             TO TRUE
                    SET      SHIFT-F1             TO TRUE
               WHEN KEY-SHIFT-F2
                    SET EDIT-SHIFT-F2             TO TRUE
                    SET      SHIFT-F2             TO TRUE
               WHEN KEY-SHIFT-F3
                    SET EDIT-SHIFT-F3             TO TRUE
                    SET      SHIFT-F3             TO TRUE
               WHEN KEY-SHIFT-F4
                    SET EDIT-SHIFT-F4             TO TRUE
                    SET      SHIFT-F4             TO TRUE
               WHEN KEY-SHIFT-F5
                    SET EDIT-SHIFT-F5             TO TRUE
                    SET      SHIFT-F5             TO TRUE
               WHEN KEY-SHIFT-F6
                    SET EDIT-SHIFT-F6             TO TRUE
                    SET      SHIFT-F6             TO TRUE
               WHEN KEY-SHIFT-F7
                    SET EDIT-SHIFT-F7             TO TRUE
                    SET      SHIFT-F7             TO TRUE
               WHEN KEY-SHIFT-F8
                    SET EDIT-SHIFT-F8             TO TRUE
                    SET      SHIFT-F8             TO TRUE
               WHEN KEY-SHIFT-F9
                    SET EDIT-SHIFT-F9             TO TRUE
                    SET      SHIFT-F9             TO TRUE
               WHEN KEY-SHIFT-F10
                    SET EDIT-SHIFT-F10            TO TRUE
                    SET      SHIFT-F10            TO TRUE
               WHEN KEY-CONTROL-F1
                    SET EDIT-CONTROL-F1           TO TRUE
                    SET      CONTROL-F1           TO TRUE
               WHEN KEY-CONTROL-F2
                    SET EDIT-CONTROL-F2           TO TRUE
                    SET      CONTROL-F2           TO TRUE
               WHEN KEY-CONTROL-F3
                    SET EDIT-CONTROL-F3           TO TRUE
                    SET      CONTROL-F3           TO TRUE
               WHEN KEY-CONTROL-F4
                    SET EDIT-CONTROL-F4           TO TRUE
                    SET      CONTROL-F4           TO TRUE
               WHEN KEY-CONTROL-F5
                    SET EDIT-CONTROL-F5           TO TRUE
                    SET      CONTROL-F5           TO TRUE
               WHEN KEY-CONTROL-F6
                    SET EDIT-CONTROL-F6           TO TRUE
                    SET      CONTROL-F6           TO TRUE
               WHEN KEY-CONTROL-F7
                    SET EDIT-CONTROL-F7           TO TRUE
                    SET      CONTROL-F7           TO TRUE
               WHEN KEY-CONTROL-F8
                    SET EDIT-CONTROL-F8           TO TRUE
                    SET      CONTROL-F8           TO TRUE
               WHEN KEY-CONTROL-F9
                    SET EDIT-CONTROL-F9           TO TRUE
                    SET      CONTROL-F9           TO TRUE
               WHEN KEY-CONTROL-F10
                    SET EDIT-CONTROL-F10          TO TRUE
                    SET      CONTROL-F10          TO TRUE
               WHEN KEY-ALT-F1
                    SET EDIT-ALT-F1               TO TRUE
                    SET      ALT-F1               TO TRUE
               WHEN KEY-ALT-F2
                    SET EDIT-ALT-F2               TO TRUE
                    SET      ALT-F2               TO TRUE
               WHEN KEY-ALT-F3
                    SET EDIT-ALT-F3               TO TRUE
                    SET      ALT-F3               TO TRUE
               WHEN KEY-ALT-F4
                    SET EDIT-ALT-F4               TO TRUE
                    SET      ALT-F4               TO TRUE
               WHEN KEY-ALT-F5
                    SET EDIT-ALT-F5               TO TRUE
                    SET      ALT-F5               TO TRUE
               WHEN KEY-ALT-F6
                    SET EDIT-ALT-F6               TO TRUE
                    SET      ALT-F6               TO TRUE
               WHEN KEY-ALT-F7
                    SET EDIT-ALT-F7               TO TRUE
                    SET      ALT-F7               TO TRUE
               WHEN KEY-ALT-F8
                    SET EDIT-ALT-F8               TO TRUE
                    SET      ALT-F8               TO TRUE
               WHEN KEY-ALT-F9
                    SET EDIT-ALT-F9               TO TRUE
                    SET      ALT-F9               TO TRUE
               WHEN KEY-ALT-F10
                    SET EDIT-ALT-F10              TO TRUE
                    SET      ALT-F10              TO TRUE
               WHEN KEY-CONTROL-END
                    SET EDIT-CONTROL-END          TO TRUE
               WHEN KEY-CONTROL-HOME
                    SET EDIT-CONTROL-HOME         TO TRUE
               WHEN KEY-CONTROL-PAGE-DOWN
                    SET EDIT-CONTROL-PAGE-DOWN    TO TRUE
                    SET      CONTROL-PAGE-DOWN    TO TRUE
               WHEN KEY-CONTROL-PAGE-UP
                    SET EDIT-CONTROL-PAGE-UP      TO TRUE
                    SET      CONTROL-PAGE-UP      TO TRUE
               WHEN KEY-F11
                    SET EDIT-F11                  TO TRUE
                    SET      F11                  TO TRUE
               WHEN KEY-F12
                    SET EDIT-F12                  TO TRUE
                    SET      F12                  TO TRUE
               WHEN KEY-SHIFT-F11
                    SET EDIT-SHIFT-F11            TO TRUE
                    SET      SHIFT-F11            TO TRUE
               WHEN KEY-SHIFT-F12
                    SET EDIT-SHIFT-F12            TO TRUE
                    SET      SHIFT-F12            TO TRUE
               WHEN KEY-CONTROL-F11
                    SET EDIT-CONTROL-F11          TO TRUE
                    SET      CONTROL-F11          TO TRUE
               WHEN KEY-CONTROL-F12
                    SET EDIT-CONTROL-F12          TO TRUE
                    SET      CONTROL-F12          TO TRUE
               WHEN KEY-ALT-F11
                    SET EDIT-ALT-F11              TO TRUE
                    SET      ALT-F11              TO TRUE
               WHEN KEY-ALT-F12
                    SET EDIT-ALT-F12              TO TRUE
                    SET      ALT-F12              TO TRUE
      *        WHEN KEY-BACKSPACE
      *             MOVE 300                      TO TECLA
               WHEN OTHER
                    MOVE DATA-ITEM TO CARACTER
           END-EVALUATE

           CALL "CWAKEY" USING TECLA-MS MIL

           IF   X91-PARAMETER > 5
                MOVE REPINS TO INSERT-QUERY
           END-IF

           IF   X91-PARAMETER > 2
                MOVE TECLA TO TECLA-EDIT
           END-IF

           ON 1 AND EVERY 1000
                CALL "CWLOCK" USING "X".

       000-99-FIM. GOBACK.

       010-REPINS.

           IF   INSERT-OFF
                SET INSERT-ON  TO TRUE
           ELSE
                SET INSERT-OFF TO TRUE
           END-IF

           IF   INSERT-ON
                CALL "CBL_WRITE_SCR_CHATTRS" USING X"184A"
                                             "Insert"
                                             "......"
                                             X"0006"
           ELSE
                CALL "CBL_WRITE_SCR_CHATTRS" USING X"184A"
                                             "      "
                                             X"070707070707"
                                             X"0006"
           END-IF.

       010-99-FIM. EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UNIKBD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/01/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Leitura de teclado para Unix                 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  I PIC 999 VALUE 0.
       01  ROTINAS.
           05  KBDCHAR   PIC X(18) VALUE "CBL+READ+KBD+CHAR".
           05  KBDSTATUS PIC X(18) VALUE "CBL+GET+KBD+STATUS".

       01 tabTeclas.
           05 pic x(31) value "                117 050-020-000". *> CONTROL-END
           05 pic x(31) value "                118 049-056-000". *> CONTROL-PAGE-DOWN
           05 pic x(31) value "                132 049-055-000". *> CONTROL-PAGE-UP
           05 pic x(31) value "006             370 050-013-006". *> CONTROL-F
           05 pic x(31) value "008             300 050-014-008". *> BACKSPACE
           05 pic x(31) value "009             009 050-011-009". *> TAB
           05 pic x(31) value "010             013 050-011-013". *> ENTER
           05 pic x(31) value "012             376 051-012-012". *> CONTROL-L
           05 pic x(31) value "015             379 050-016-015". *> CONTROL-O
           05 pic x(31) value "018             382 050-018-018". *> CONTROL-R
           05 pic x(31) value "021             385 051-021-021". *> CONTROL-U
           05 pic x(31) value "022             386 051-022-022". *> CONTROL-V
           05 pic x(31) value "024             388 050-020-024". *> CONTROL-X
           05 pic x(31) value "025             390 050-019-026". *> CONTROL-Z
           05 pic x(31) value "027             027 049-000-027". *> ESC
           05 pic x(31) value "027 045         251 049-052-000". *> ALT-TRACE
           05 pic x(31) value "027 048         250 049-050-000". *> ALT-0
           05 pic x(31) value "027 049         241 049-041-000". *> ALT-1
           05 pic x(31) value "027 050         242 049-042-000". *> ALT-2
           05 pic x(31) value "027 051         243 049-043-000". *> ALT-3
           05 pic x(31) value "027 052         244 049-044-000". *> ALT-4
           05 pic x(31) value "027 053         245 049-045-000". *> ALT-5
           05 pic x(31) value "027 054         246 049-046-000". *> ALT-6
           05 pic x(31) value "027 055         247 049-047-000". *> ALT-7
           05 pic x(31) value "027 056         248 049-048-000". *> ALT-8
           05 pic x(31) value "027 057         249 049-049-000". *> ALT-9
           05 pic x(31) value "027 061         252 049-052-000". *> ALT-EQUAL
           05 pic x(31) value "027 065         265 049-065-000". *> ALT-A
           05 pic x(31) value "027 066         266 049-066-000". *> ALT-B
           05 pic x(31) value "027 067         267 049-067-000". *> ALT-C
           05 pic x(31) value "027 068         268 049-068-000". *> ALT-D
           05 pic x(31) value "027 069         269 049-069-000". *> ALT-E
           05 pic x(31) value "027 070         270 049-070-000". *> ALT-F
           05 pic x(31) value "027 071         271 049-071-000". *> ALT-G
           05 pic x(31) value "027 072         272 049-072-000". *> ALT-H
           05 pic x(31) value "027 073         273 049-073-000". *> ALT-I
           05 pic x(31) value "027 074         274 049-074-000". *> ALT-J
           05 pic x(31) value "027 075         275 049-075-000". *> ALT-K
           05 pic x(31) value "027 076         276 049-076-000". *> ALT-L
           05 pic x(31) value "027 077         277 049-077-000". *> ALT-M
           05 pic x(31) value "027 078         278 049-078-000". *> ALT-N
           05 pic x(31) value "027 079         279 049-079-000". *> ALT-O
           05 pic x(31) value "027 079 065     141 057-009-000". *> CONTROL-CURSOR-UP
           05 pic x(31) value "027 079 066     145 057-009-000". *> CONTROL-CURSOR-DOWN
           05 pic x(31) value "027 079 067     116 050-038-000". *> CONTROL-CURSOR-RIGHT
           05 pic x(31) value "027 079 068     115 050-039-000". *> CONTROL-CURSOR-LEFT
           05 pic x(31) value "027 080         280 049-080-000". *> ALT-P
           05 pic x(31) value "027 081         281 049-081-000". *> ALT-Q
           05 pic x(31) value "027 082         282 049-082-000". *> ALT-R
           05 pic x(31) value "027 083         283 049-083-000". *> ALT-S
           05 pic x(31) value "027 084         284 049-084-000". *> ALT-T
           05 pic x(31) value "027 085         285 049-085-000". *> ALT-U
           05 pic x(31) value "027 086         286 049-086-000". *> ALT-V
           05 pic x(31) value "027 087         287 049-087-000". *> ALT-W
           05 pic x(31) value "027 088         288 049-088-000". *> ALT-X
           05 pic x(31) value "027 089         289 049-089-000". *> ALT-Y
           05 pic x(31) value "027 090         290 049-090-000". *> ALT-Z
           05 pic x(31) value "027 255 001 009 104 049-031-000". *> ALT-F1
           05 pic x(31) value "027 255 001 010 105 049-032-000". *> ALT-F2
           05 pic x(31) value "027 255 001 011 106 049-033-000". *> ALT-F3
           05 pic x(31) value "027 255 001 012 107 049-034-000". *> ALT-F4
           05 pic x(31) value "027 255 001 013 108 049-035-000". *> ALT-F5
           05 pic x(31) value "027 255 001 014 109 049-036-000". *> ALT-F6
           05 pic x(31) value "027 255 001 015 110 049-037-000". *> ALT-F7
           05 pic x(31) value "027 255 001 016 111 049-038-000". *> ALT-F8
           05 pic x(31) value "027 255 001 017 112 049-039-000". *> ALT-F9
           05 pic x(31) value "027 255 001 018 113 049-040-000". *> ALT-F10
           05 pic x(31) value "027 255 001 019 139 049-097-000". *> ALT-F11
           05 pic x(31) value "027 255 001 020 140 049-098-000". *> ALT-F12
           05 pic x(31) value "255 001 002     080 050-011-000". *> CURSOR-DOWN
           05 pic x(31) value "255 001 003     072 050-012-000". *> CURSOR-UP
           05 pic x(31) value "255 001 004     075 050-003-000". *> CURSOR-LEFT
           05 pic x(31) value "255 001 005     077 050-004-000". *> CURSOR-RIGHT
           05 pic x(31) value "255 001 006     071 050-007-000". *> HOME
           05 pic x(31) value "255 001 006     301 050-022-000". *> CONTROL-HOME
           05 pic x(31) value "255 001 009     059 049-001-000". *> F1
           05 pic x(31) value "255 001 009     094 049-021-000". *> CONTROL-F1
           05 pic x(31) value "255 001 010     060 049-002-000". *> F2
           05 pic x(31) value "255 001 010     095 049-022-000". *> CONTROL-F2
           05 pic x(31) value "255 001 011     061 049-003-000". *> F3
           05 pic x(31) value "255 001 011     096 049-023-000". *> CONTROL-F3
           05 pic x(31) value "255 001 012     062 049-004-000". *> F4
           05 pic x(31) value "255 001 012     097 049-024-000". *> CONTROL-F4
           05 pic x(31) value "255 001 013     063 049-005-000". *> F5
           05 pic x(31) value "255 001 013     098 049-025-000". *> CONTROL-F5
           05 pic x(31) value "255 001 014     064 049-006-000". *> F6
           05 pic x(31) value "255 001 014     099 049-026-000". *> CONTROL-F6
           05 pic x(31) value "255 001 015     065 049-007-000". *> F7
           05 pic x(31) value "255 001 015     100 049-027-000". *> CONTROL-F7
           05 pic x(31) value "255 001 016     066 049-008-000". *> F8
           05 pic x(31) value "255 001 016     101 049-028-000". *> CONTROL-F8
           05 pic x(31) value "255 001 017     067 049-009-000". *> F9
           05 pic x(31) value "255 001 017     102 049-029-000". *> CONTROL-F9
           05 pic x(31) value "255 001 018     068 049-010-000". *> F10
           05 pic x(31) value "255 001 018     103 049-030-000". *> CONTROL-F10
           05 pic x(31) value "255 001 019     084 049-011-000". *> SHIFT-F1
           05 pic x(31) value "255 001 019     133 049-091-000". *> F11
           05 pic x(31) value "255 001 019     135 049-093-000". *> SHIFT-F11
           05 pic x(31) value "255 001 019     137 049-095-000". *> CONTROL-F11
           05 pic x(31) value "255 001 020     085 049-012-000". *> SHIFT-F2
           05 pic x(31) value "255 001 020     134 049-092-000". *> F12
           05 pic x(31) value "255 001 020     136 049-094-000". *> SHIFT-F12
           05 pic x(31) value "255 001 020     138 049-096-000". *> CONTROL-F12
           05 pic x(31) value "255 001 021     086 049-013-000". *> SHIFT-F3
           05 pic x(31) value "255 001 022     087 049-014-000". *> SHIFT-F4
           05 pic x(31) value "255 001 023     088 049-015-000". *> SHIFT-F5
           05 pic x(31) value "255 001 024     089 049-016-000". *> SHIFT-F6
           05 pic x(31) value "255 001 025     090 049-017-000". *> SHIFT-F7
           05 pic x(31) value "255 001 026     091 049-018-000". *> SHIFT-F8
           05 pic x(31) value "255 001 027     092 049-019-000". *> SHIFT-F9
           05 pic x(31) value "255 001 028     093 049-020-000". *> SHIFT-F10
           05 pic x(31) value "255 001 074     083 050-017-000". *> DEL
           05 pic x(31) value "255 001 075     082 050-023-000". *> INSERT
           05 pic x(31) value "255 001 082     081 049-054-000". *> PAGE-DOWN
           05 pic x(31) value "255 001 083     073 049-053-000". *> PAGE-UP
           05 pic x(31) value "255 001 097     015 050-012-000". *> SHIFT-TAB
           05 pic x(31) value "255 001 104     079 050-010-000". *> END
       01 tabTeclasR redefines tabTeclas.
           05 teclaTab occurs 117 times indexed by ixTecla.
              10 teclaLinux   pic x(15).
              10 filler       pic x(01).
              10 teclaWindows pic 9(03).
              10 filler       pic x(01).
              10 TAB-TYPE     PIC 9(03).
              10 filler       pic x(01).
              10 TAB-CODE-1   PIC 9(03).
              10 filler       pic x(01).
              10 TAB-CODE-2   PIC 9(03).

       01 ZKBDREAD-VAR.
           05 ZKBDREAD-RET         pic  x(001)       value space.
           05 ZKBDREAD-RET-R       redefines ZKBDREAD-RET
                                   pic  x(001)                   comp-x.
           05 ZKBDREAD-ERR         pic  x(001)       value 0     comp-x.

       01 ZKBDSTAT-VAR.
           05 ZKBDSTAT-RET         pic  x(001)       value 0     comp-x.
           05 ZKBDSTAT-ERR         pic  x(001)       value 0     comp-x.

       77 chrAscii                 pic  9(003)       value 0.
       77 teclaStr                 pic  x(015)       value spaces.
       77 teclaStrPos              pic  9(002)       value 0.

       01 kbdTecla                 pic  9(003)       value 0.

       linkage section.

       01 crts.
          02  st-1 pic 9(2) comp-x.
          02  st-2 pic 9(2) comp-x.
          02  st-3 pic 9(2) comp-x.

       procedure division using crts.

       main.
           ON 1 INSPECT ROTINAS CONVERTING "+" TO "_".
           perform ZKBDREAD

           move ZKBDREAD-RET-R to chrAscii
           move chrAscii to teclaStr
           move 5 to teclaStrPos

           perform ZKBDSTATUS
           IF teclaStr = '027' OR '255'
              perform until ZKBDSTAT-RET = 0
                 perform ZKBDREAD
                 perform ZKBDSTATUS

                 move ZKBDREAD-RET-R to chrAscii
                 move chrAscii to teclaStr (teclaStrPos:3)
                 add 4 to teclaStrPos
              end-perform
           end-if

           set ixTecla to 1

           search teclaTab
               at end
                  if teclaStr (4:12) = spaces
                  and (teclaStr (1:3) not = '000')
                  and (teclaStr (1:3) not = '   ')
                  and teclaStrPos > 5
                     move teclaStr (1:3) to kbdTecla
                     perform varying i from 1 by 1 until i > 117
                          if kbdTecla = teclaWindows (i)
                             move TAB-TYPE     (i) to st-1
                             move TAB-CODE-1   (i) to st-2
                             move TAB-CODE-2   (i) to st-3
                          end-if
                     end-perform
                  else
                     move 0        to kbdTecla
                     MOVE 51       TO st-1
                     MOVE chrAscii TO st-2
                                      st-3
                  end-if
             when teclaLinux (ixTecla) = teclaStr
      *           move teclaWindows (ixTecla) to kbdTecla
                  move TAB-TYPE     (ixTecla) to st-1
                  move TAB-CODE-1   (ixTecla) to st-2
                  move TAB-CODE-2   (ixTecla) to st-3
           end-search

           goback.

       ZKBDREAD.
           Call KBDCHAR
                     using ZKBDREAD-RET
                 returning ZKBDREAD-ERR.

       ZKBDSTATUS.
           Call KBDSTATUS
                    using ZKBDSTAT-RET
                returning ZKBDSTAT-ERR.

       end program UNIKBD.
       END PROGRAM CWKBDC.
