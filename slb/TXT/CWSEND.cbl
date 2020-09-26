       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSEND INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/07/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Emite mensagen de erro "X(074)"              *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CWACCENT                 PIC  X(003) VALUE SPACES.
           05 CWLITS                   PIC  X(003) VALUE SPACES.
frango     05 JANPOS.
frango        10 JANLIN                PIC  9(002) VALUE 0.
frango        10 JANCOL                PIC  9(002) VALUE 0.
       01  AREAS-DE-TRABALHO-1.
           05 LB-USELOG                PIC  X(050) VALUE spaces.
           05 CWOBS                    PIC  X(5000)  VALUE SPACES.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 CWSEND-OK                PIC  X(002) VALUE SPACES.
           05 CRON                     PIC  X(003) VALUE SPACES.
           05 MSG-D                    PIC  X(074) VALUE SPACES.
           05 LB2                      PIC  X(074) VALUE SPACES.
           05 I                        PIC  9(002) VALUE 0.
           05 OK                       PIC  9(002) VALUE 0.
           05 PGM                      PIC  X(030) VALUE SPACES.
           05 CX                       PIC  9(002) VALUE 0.
           05 CX-M                     PIC  9(002) VALUE 0.
           05 MAIOR                    PIC  9(002) VALUE 0.
           05 SAVE-MAIOR               PIC  9(002) VALUE 0.
           05 C                        PIC  9(002) VALUE 0.
           05 L                        PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(002) VALUE 0.
           05 S                        PIC  9(002) VALUE 0.
           05 LENG                     PIC  9(003) VALUE 0.
           05 LENG2                    PIC  9(003) VALUE 0.
           05 TAMANHO                  PIC  9(002) VALUE 0.
           05 TT                       PIC  9(002) VALUE 0.
           05 LN                       PIC  9(002) VALUE 0.
           05 BOTOES                   PIC  9(002) VALUE 0.
           05 CBL-READ-WRITE-SCR-CHARS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC  X(080) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.

       COPY CWBOXW.
       COPY CWLINE.
       COPY CWLOGW.

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
           ON 1
              DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
              ACCEPT CWLITS FROM ENVIRONMENT-VALUE
              INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
              ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
              INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS.
           INITIALIZE AREAS-DE-TRABALHO-1
frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango     DISPLAY '0000'        UPON ENVIRONMENT-VALUE
           DISPLAY "CWSEND-OK" UPON ENVIRONMENT-NAME
           ACCEPT  CWSEND-OK   FROM ENVIRONMENT-VALUE
           MOVE 14 TO CWLINE-LINE
           CALL "CWATCH"
           INSPECT CWSEND-MSG CONVERTING LOW-VALUE TO SPACE
           IF   CWSEND-MSG NOT EQUAL SPACES
                IF CWSEND-MSG (1:4) = 'ORA-'
                   INSPECT CWSEND-MSG
                           CONVERTING ACENTOS-WINDOWS
                                   TO ACENTOS-850
                END-IF
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
                IF   CWSEND-MSG  (1: 1) = "^"
                     MOVE CWSEND-MSG (2: ) TO MSG-D
                ELSE
                     MOVE CWSEND-MSG  TO MSG-D
                END-IF
                IF MSG-D(1:1) NOT = '('
                   INSPECT MSG-D CONVERTING "_" TO SPACE
                END-IF
                IF   CWLITS = "LOW"
                     INSPECT msg-d
                             CONVERTING MAIUSCULAS TO MINUSCULAS
                END-IF
                IF   CWLITS = "UPP"
                     INSPECT msg-d
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                END-IF
                IF   CWACCENT = "OFF"
                     INSPECT msg-d
                             CONVERTING ACENTOS-850 TO ACENTOS-OFF
      *         ELSE
      *              INSPECT msg-d
      *                      CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                END-IF
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS
                     IF CRON = 'ON'
                        DISPLAY 'CWEXTCAR' UPON ENVIRONMENT-NAME
                        ACCEPT LB-USELOG   FROM ENVIRONMENT-VALUE
                        IF    LB-USELOG NOT = SPACES
                              MOVE MSG-D TO CWOBS
                              CALL "CWCLOG" USING "x" CWOBS
                        ELSE
                              DISPLAY MSG-D UPON CONSOLE
                        END-IF
                        GOBACK
                     END-IF
                ADD  2           TO CX
                MOVE 10          TO CWBOXW-LINE
                MOVE 26          TO CWBOXW-COLUMN
                MOVE "OPEN"      TO CWBOXW-FUNCTION
                MOVE 078         TO CWBOXW-COLOR-FRAME
                MOVE 078         TO CWBOXW-COLOR-BORDER
                MOVE 7           TO CWBOXW-VERTICAL-LENGTH
                MOVE CX          TO CWBOXW-HORIZONTAL-LENGTH
                                    CX-M
                MOVE 16          TO X91-FUNCTION
                CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                MOVE 0 TO I
                IF   X91-PARAMETER > 1
                AND (LB NOT = SPACE)
                     PERFORM VARYING I FROM 50 BY -1
                             UNTIL LB (I: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     ADD 4 TO I
                END-IF
                IF  I > CWBOXW-HORIZONTAL-LENGTH
                    MOVE I TO CWBOXW-HORIZONTAL-LENGTH
                END-IF
                MOVE SPACES      TO CWLINE-SCREENS
                IF   CWSEND-SCREENS = SPACES
                     ADD  3           TO CWBOXW-HORIZONTAL-LENGTH
mod>                 PERFORM UNTIL CWBOXW-HORIZONTAL-LENGTH > (CX + 7)
mod>                         OR CWBOXW-HORIZONTAL-LENGTH > 78
mod>                         ADD 1 TO CWBOXW-HORIZONTAL-LENGTH
mod>                 END-PERFORM
mod>                 IF CWBOXW-HORIZONTAL-LENGTH > 78
mod>                    MOVE 78 TO CWBOXW-HORIZONTAL-LENGTH
                     END-IF
                     PERFORM 010-CENTRALIZA THRU 010-99-FIM
                     CALL "CWBOXW" USING PARAMETROS-CWBOXW
                     MOVE MSG-D TO CARACTER-BUFFER
                     MOVE CX    TO STRING-LENGTH
                     MOVE 11 TO ROW-NUMBER
                     COMPUTE COLUMN-NUMBER = C - 1
                     CALL "CWTEXT" USING CARACTER-BUFFER
                               LENGTH OF CARACTER-BUFFER
                     CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                      CARACTER-BUFFER
                                                      STRING-LENGTH
                     MOVE "   Ok___"  TO CWLINE-SCREEN (1)
                     MOVE "O"         TO CWLINE-CHAR   (1)
                     COMPUTE CWLINE-COLUMN = CWBOXW-COLUMN
                                      + (CWBOXW-HORIZONTAL-LENGTH / 2)
                                      - 5
                ELSE
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
                     MOVE 0           TO BOTOES
                                         MAIOR
                     PERFORM VARYING S FROM 1 BY 1 UNTIL S > 8
                             MOVE CWSEND-CHAR   (S) TO CWLINE-CHAR   (S)
                             MOVE SPACES            TO CWLINE-SCREEN (S)
                             PERFORM VARYING TAMANHO FROM 10 BY -1
                                     UNTIL BYTE-TXT (S TAMANHO)
                                           NOT = SPACE
                                        OR CWSEND-SCREEN (S) = SPACES
                                           CONTINUE
                             END-PERFORM
                             MOVE 0 TO LN
                             PERFORM VARYING TT FROM 1 BY 1
                                     UNTIL TT > TAMANHO
                                           IF   BYTE-TXT (S TT) = X"7E"
                                                ADD 1 TO TT
                                                MOVE BYTE-TXT (S TT)
                                                  TO CWLINE-CHAR (S)
                                           END-IF
                                           ADD 1 TO LN
                                           MOVE BYTE-TXT (S TT)
                                            TO CWLINE-SCREEN (S) (LN: 1)
                             END-PERFORM
                             MOVE LN TO TAMANHO
                             IF   CWLINE-SCREEN (S) NOT = SPACES
                                  ADD 1 TO BOTOES
                                  IF  TAMANHO > MAIOR
                                      MOVE TAMANHO TO MAIOR
                                  END-IF
                             END-IF
                     END-PERFORM
                     MOVE MAIOR TO SAVE-MAIOR
                     ADD  6  TO MAIOR
                     MOVE 2  TO LENG
                     MOVE 0  TO VEZ
                     MOVE 0  TO S LENG2
                     PERFORM BOTOES TIMES
                        ADD     1 TO S
                        INSPECT CWLINE-SCREEN (S) (1: SAVE-MAIOR)
                                CONVERTING " " TO "_"
                        IF  (LENG + MAIOR) < 76
                            ADD MAIOR TO LENG
                            IF  LENG > LENG2
                                MOVE LENG TO LENG2
                            END-IF
                        ELSE
                            ADD  1 TO VEZ
                            MOVE 2 TO LENG
                            ADD  4 TO CWBOXW-VERTICAL-LENGTH
                            IF VEZ > 1
                               SUBTRACT 4   FROM CWBOXW-LINE
                                                 CWLINE-LINE
                            END-IF
                        END-IF
                     END-PERFORM
                     IF  CX > LENG2
                         COMPUTE LENG2 = CX + 3
                     END-IF
                     MOVE LENG2 TO CWBOXW-HORIZONTAL-LENGTH
                     COMPUTE CX = CWBOXW-HORIZONTAL-LENGTH - 3
                     COMPUTE CWBOXW-COLUMN = (80 - (CX + 2)) / 2
                     PERFORM 010-CENTRALIZA THRU 010-99-FIM
                     COMPUTE CWLINE-COLUMN = CWBOXW-COLUMN + 3
                     COMPUTE C             = CWBOXW-COLUMN + 3
                     CALL "CWBOXW" USING PARAMETROS-CWBOXW
                     IF    CWBOXW-HORIZONTAL-LENGTH < (CX-M + 4)
                           MOVE CX-M TO CWBOXW-HORIZONTAL-LENGTH
                           ADD    4  TO CWBOXW-HORIZONTAL-LENGTH
                     END-IF
                     COMPUTE CWLINE-COLUMN-END
                           = CWLINE-COLUMN + CWBOXW-HORIZONTAL-LENGTH
                     COMPUTE L = CWBOXW-LINE + 2
                     MOVE MSG-D TO CARACTER-BUFFER
                     MOVE CX    TO STRING-LENGTH
                     COMPUTE ROW-NUMBER    = L - 1
                     COMPUTE COLUMN-NUMBER = C - 1
                     CALL "CWTEXT" USING CARACTER-BUFFER
                               LENGTH OF CARACTER-BUFFER
                     CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                      CARACTER-BUFFER
                                                      STRING-LENGTH
                END-IF
                MOVE CWSEND-OPTION      TO CWLINE-OPTION
                IF   BOTOES = 1
                     COMPUTE CWLINE-COLUMN = (80 - (SAVE-MAIOR + 2)) / 2
                END-IF
                IF   X91-PARAMETER > 1
                AND (LB NOT = SPACE)
                     SUBTRACT 2 FROM I
                     ADD 2 TO CWBOXW-COLUMN
                     MOVE SPACES TO LB2
                     STRING " " DELIMITED BY SIZE
                            LB  DELIMITED BY SPACE
                            " " DELIMITED BY SIZE
                            INTO LB2
                     MOVE LB2 TO CARACTER-BUFFER
                     MOVE I   TO STRING-LENGTH
                     compute ROW-NUMBER    =  CWBOXW-LINE - 1
                     compute COLUMN-NUMBER =  CWBOXW-COLUMN - 1
                     CALL "CWTEXT" USING CARACTER-BUFFER
                               LENGTH OF CARACTER-BUFFER
                     CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                      CARACTER-BUFFER
                                                      STRING-LENGTH
                END-IF
                MOVE CWSEND-TIMEOUT-STATUS
                  TO CWLINE-TIMEOUT-STATUS
                CALL "CWLINE"        USING PARAMETROS-CWLINE
                MOVE CWLINE-TIMEOUT-RETURN
                  TO CWSEND-TIMEOUT-RETURN
                MOVE CWLINE-OPTION      TO CWSEND-OPTION
                MOVE CWLINE-OPTION-CHAR TO CWSEND-OPTION-CHAR
                MOVE "CLOSE"            TO CWBOXW-FUNCTION
                CALL "CWBOXW"        USING PARAMETROS-CWBOXW
             END-IF

frango       DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango       DISPLAY JANPOS        UPON ENVIRONMENT-VALUE
             CALL "CWATCH".

       000-FIM. EXIT PROGRAM.

       010-CENTRALIZA.

           PERFORM UNTIL ((CWBOXW-COLUMN
                        + CWBOXW-HORIZONTAL-LENGTH)
                        < 78)
                   AND (CWBOXW-COLUMN NOT >
                       (80 - CWBOXW-COLUMN + CWBOXW-HORIZONTAL-LENGTH))
                   OR CWBOXW-COLUMN = 1
                   SUBTRACT 1 FROM CWBOXW-COLUMN C
           END-PERFORM.

       010-99-FIM. EXIT.
       END PROGRAM CWSEND.

