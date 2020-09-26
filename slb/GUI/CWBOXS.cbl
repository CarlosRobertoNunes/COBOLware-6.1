       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXS INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/11/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre janela-menu e retorna a op‡Æo           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
       01  AREAS-DE-TRABALHO-2.
           05 CWBOXS-EXIT-CHAR        PIC  X(001) VALUE SPACE.
           05 CWBOXR                  PIC  X(003) VALUE SPACES.
           05 X                       PIC  9(003) VALUE 0.
           05 Y                       PIC  9(003) VALUE 0.
           05 I                       PIC  9(002) VALUE 0.
           05 OP                      PIC  9(002) VALUE 0.
           05 OPCOES                  PIC  9(002) VALUE 0.
           05 K             OCCURS 21 PIC  9(002) VALUE 0.
           05 L             OCCURS 21 PIC  9(002) VALUE 0.
           05 BUFFER                  PIC  X(080) VALUE SPACES.
           05 CHARS                               VALUE SPACES.
              10 CHAR       OCCURS 21 PIC  X(001).
           05 MAXIMO                  PIC  9(003) VALUE 0.
           05 MAXIMO-BUTTON           PIC  9(003) VALUE 0.
           05 TAMANHO                 PIC  9(003) VALUE 0.
           05 STRING-1                PIC  X(080) VALUE SPACES.
           05 STRING-2                PIC  X(080) VALUE SPACES.

       77   SAVE-1 PIC X(5000).
       77   SAVE-2 PIC X(5000).

       COPY CWBOXF.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXS.
           05 CWBOXS-LINE                 PIC  9(002).
           05 CWBOXS-COLUMN               PIC  9(002).
           05 CWBOXS-TYPE                 PIC  9(001).
           05 CWBOXS-OPTION               PIC  9(002).
           05 CWBOXS-OPTION-CHAR          PIC  X(001).
              88 CWBOXS-LEAVING                       VALUE "?".
           05 CWBOXS-TITLE                PIC  X(078).
           05 CWBOXS-ITENS.
              10 CWBOXS-CHARS.
                 15 CWBOXS-CHAR OCCURS 21 PIC  X(001).
              10 CWBOXS-TEXT    OCCURS 21 PIC  X(078).
           05 CWBOXS-KEY-ON               PIC  X(001).
           05 CWBOXS-KEY                  PIC  9(002).
           05 TECLA-X REDEFINES CWBOXS-KEY
                                   COMP-5 PIC S9(004).
           05 CWBOXS-COLOR-FRAME          PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-BORDER         PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-SHADE          PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-BARR-MENU      PIC  9(002) COMP-X.
           05 CWBOXS-ARROW                PIC  X(001).
           05 CWBOXS-ERASE                PIC  X(001).
           05 CWBOXS-TIMEOUT-STATUS       PIC  9(001).
              88 CWBOXS-TIMEOUT-ENABLE                VALUE 1.
              88 CWBOXS-TIMEOUT-DISABLE               VALUE 0.
           05 CWBOXS-TIMEOUT-RETURN       PIC  9(001).
              88 CWBOXS-TIMEOUT-ON                    VALUE 1.
              88 CWBOXS-TIMEOUT-OFF                   VALUE 0.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXS.

       000-INICIO.

           IF  CWBOXS-ERASE = "N" OR "n"
               DISPLAY "CWBOXR" UPON ENVIRONMENT-NAME
               ACCEPT  CWBOXR   FROM ENVIRONMENT-VALUE
               INSPECT CWBOXR CONVERTING MINUSCULAS TO MAIUSCULAS
               IF      CWBOXR = 'OFF'
                       CALL "TXBOXS" USING PARAMETROS-CWBOXS
               ELSE
                       CALL "CWBOXR" USING PARAMETROS-CWBOXS
               END-IF
               GOBACK
           END-IF
           MOVE PARAMETROS-CWBOXS TO SAVE-1
           MOVE PARAMETROS-CWBOXF TO SAVE-2
           MOVE SPACE             TO CWBOXS-ARROW CHARS

           IF   CWBOXS-LINE = 0
           OR   CWBOXS-LINE NOT NUMERIC
                MOVE 1 TO CWBOXS-LINE
           END-IF

           IF   CWBOXS-COLUMN = 0
           OR   CWBOXS-COLUMN NOT NUMERIC
                MOVE 1 TO CWBOXS-COLUMN
           END-IF

           PERFORM VARYING MAXIMO FROM LENGTH OF CWBOXS-TITLE
                        BY -1 UNTIL MAXIMO = 1
                                OR(CWBOXS-TITLE (MAXIMO: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                   IF  CWBOXS-TEXT (I) NOT = SPACES
                       CALL "CWVARX" USING CWBOXS-TEXT (I)
                                 LENGTH OF CWBOXS-TEXT (I)
                       ADD  1               TO OPCOES
                       MOVE I               TO K (OPCOES)
                       MOVE SPACES          TO BUFFER
                       PERFORM VARYING X FROM 78 BY -1
                         UNTIL CWBOXS-TEXT (I) (X: 1) NOT = SPACE
                               CONTINUE
                       END-PERFORM
                       MOVE X TO L (I)
                       IF   CWBOXS-CHAR (OPCOES) NOT = SPACE
                            MOVE CWBOXS-CHAR (I) TO CHAR (OPCOES)
                       END-IF
                       MOVE 0 TO TAMANHO MAXIMO-BUTTON
                       PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > X
                               IF   CWBOXS-TEXT (I) (Y: 1) = X"7E"
                                    ADD 1 TO Y
                                    IF  CHAR (OPCOES) = SPACE
                                        MOVE CWBOXS-TEXT (I) (Y: 1)
                                          TO CHAR (OPCOES)
                                             CWBOXS-CHAR (I)
                                    END-IF
                               END-IF
                               ADD 1 TO TAMANHO
                               MOVE CWBOXS-TEXT (I) (Y: 1)
                                 TO BUFFER (TAMANHO: 1)
                       END-PERFORM
                       MOVE BUFFER TO CWBOXS-TEXT (I)
                       IF   TAMANHO > MAXIMO
                            MOVE TAMANHO TO MAXIMO
                       END-IF
                   END-IF
           END-PERFORM

           IF  OPCOES = 0
               MOVE 0     TO CWBOXS-OPTION
               MOVE SPACE TO CWBOXS-OPTION-CHAR
               GOBACK
           END-IF
           MOVE CWBOXS-TITLE  TO CWBOXF-TITLE
           MOVE OPCOES        TO CWBOXF-VERTICAL-LENGTH
           MOVE MAXIMO        TO CWBOXF-HORIZONTAL-LENGTH
                                 CWBOXF-STRING-2-LENGTH
           MOVE CWBOXS-LINE   TO CWBOXF-LINE
           MOVE CWBOXS-COLUMN TO CWBOXF-COLUMN
           MOVE "CWREAD"      TO CWBOXF-PROGRAM
           MOVE X"0000"       TO CWBOXF-WORK-AREA(1: 2)
           MOVE CWBOXS-OPTION TO CWBOXF-WORK-AREA(4: 2)
           MOVE CWBOXS-KEY-ON TO CWBOXF-KEY-ON
EXT        MOVE 0             TO OP
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                   IF   CWBOXS-TEXT (I) NOT = SPACES
EXT   *                 MOVE I               TO STRING-1
EXT                     ADD  1               TO OP
EXT                     MOVE OP              TO STRING-1
                        MOVE CWBOXS-TEXT (I) TO STRING-2
                        IF CWBOXS-CHAR (I) NOT = SPACE
                           MOVE CWBOXS-CHAR(I) TO CWBOXF-WORK-AREA(3: 1)
                        ELSE
                           MOVE X"00"          TO CWBOXF-WORK-AREA(3: 1)
                        END-IF
                        CALL "CWREAD" USING "L" CWBOXF-ORDER
                                                STRING-1
                                                STRING-2
                                                CWBOXF-VERTICAL-LENGTH
                                                CWBOXF-WORK-AREA
                   END-IF
           END-PERFORM

           IF  CWBOXS-ERASE = "N" OR "n"
               SET CWBOXF-POP-UP TO TRUE
           ELSE
               SET CWBOXF-SHOW   TO TRUE
           END-IF

           IF CWBOXS-LEAVING
              DISPLAY "CWBOXS-EXIT" UPON ENVIRONMENT-NAME
              DISPLAY "ON"          UPON ENVIRONMENT-VALUE
           END-IF

           DISPLAY "CWBOXS-CHARS" UPON ENVIRONMENT-NAME
           DISPLAY         CHARS  UPON ENVIRONMENT-VALUE
           CALL "CWBOXF"   USING PARAMETROS-CWBOXF
           CALL "CWREAD" USING "D" CWBOXF-ORDER
                                   STRING-1
                                   STRING-2
                                   CWBOXF-VERTICAL-LENGTH
                                   CWBOXF-WORK-AREA
           IF  CWBOXF-WORK-AREA(1: 4) = X"0000FFFF"
           AND CWBOXS-LEAVING
           AND(CWBOXF-OPTION(3: 1) NOT = SPACES)
               MOVE CWBOXF-OPTION(3: 1) TO CWBOXS-OPTION-CHAR
               MOVE 99                  TO CWBOXS-OPTION
               GOBACK
           END-IF
           IF  CWBOXF-WORK-AREA(1: 4) = X"0000FFFF"
               INITIALIZE AREAS-DE-TRABALHO-2
               MOVE SAVE-1 TO PARAMETROS-CWBOXS
               MOVE SAVE-2 TO PARAMETROS-CWBOXF
               GO TO 000-INICIO
           END-IF
           IF  CWBOXF-OPTION = SPACES
               MOVE 0     TO CWBOXS-OPTION
               MOVE SPACE TO CWBOXS-OPTION-CHAR
           ELSE
               MOVE CWBOXF-OPTION (1: 2) TO CWBOXS-OPTION (1: 2)
               MOVE CWBOXF-OPTION (3: 1) TO CWBOXS-OPTION-CHAR
               IF CWBOXS-OPTION (1: 2) = "00"
                  MOVE CWBOXS-OPTION (3: 1) TO CWBOXS-ARROW
               END-IF
           END-IF
           MOVE CWBOXF-KEY-ON TO CWBOXS-KEY-ON
           MOVE CWBOXF-KEY    TO CWBOXS-KEY.

       000-99-FIM. EXIT PROGRAM.
       COPY SLB\TXT\CWBOXS.CBL REPLACING ==CWBOXS== BY ==TXBOXS==
                  ==CWGETL-MOUSE = 1== BY ==CWGETL-MOUSE = 9==.
       END PROGRAM CWBOXS.
