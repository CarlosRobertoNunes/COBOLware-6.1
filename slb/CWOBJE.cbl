       IDENTIFICATION DIVISION.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  22/12/2003.
       SECURITY.      *************************************************
                      *                                               *
                      *  Armazenagem de definicoes de objetos para    *
                      *  a CWUSER                                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJETOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJETOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-OBJETOS.

       DATA DIVISION.
       FILE SECTION.

       FD  OBJETOS
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 371 TO 16403 DEPENDING ON SIZE-REG
           VALUE OF FILE-ID IS LB-OBJETOS.

       01  OBJETOS-REG.
           05 OBJETOS-CHAVE.
              10 OBJETOS-WINDOW     COMP-X PIC  9(004).
              10 OBJETOS-SEQUENCE   COMP-X PIC  9(004).
           05 OBJETOS-DATA.
              10 OBJETOS-FIXO              PIC  X(367).
              10 OBJETOS-MOVEL  OCCURS 0 TO 16000
                               DEPENDING ON SIZE-MOVEL PIC X.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
Frango*    05 POP-WINDOW    COMP-X PIC  9(004) VALUE 7.
           05 CWLOGT               PIC  X(002) VALUE SPACES.
           05 DELETE-CWREAD        PIC  9(001) VALUE 0.
           05 STRINGS              PIC  9(004) VALUE 0.
           05 I                    PIC  9(004) VALUE 0.
           05 G                    PIC  9(004) VALUE 0.
           05 SIZE-MOVEL    COMP-X PIC  9(004) VALUE 0.
           05 SIZE-REG      COMP-X PIC  9(004) VALUE 0.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.
           05 ER-OBJETOS.
              10 FS-OBJETOS        PIC  X(002) VALUE "00".
              10 LB-OBJETOS        PIC  X(255) VALUE "$TEMP/cwobje$$".
frango     05 DIM.
frango        10 FILLER               PIC  X(001) VALUE 'W'.
frango        10 SCR.
frango           15 SCR-LIN           PIC  9(002) VALUE 1.
frango           15 SCR-COL           PIC  9(002) VALUE 1.
frango           15 SCR-SIZE          PIC  9(002) VALUE 80.
frango           15 SCR-LINES         PIC  9(002) VALUE 25.
frango        10 POP-WINDOW    COMP-X PIC  9(004) VALUE 7.

       COPY CWUNIX.

       LINKAGE SECTION.

       COPY CWOBJE.
       01  CWOBJE-FUNCTION.
           05                             PIC  X(001).
              88 CWOBJE-GET                          VALUE "g" "G".
              88 CWOBJE-OCCURS                       VALUE "o" "O".
              88 CWOBJE-CHECK-VALIDATE               VALUE "V".
           05 CWOBJE-OCCURS-NUMBER COMP-X PIC  9(004) VALUE 0.

       PROCEDURE DIVISION USING PARAMETROS-CWOBJE CWOBJE-FUNCTION.

       000-INICIO.

           ON 1
              OPEN I-O OBJETOS
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              DISPLAY "CWLOGT"     UPON ENVIRONMENT-NAME
              ACCEPT  CWLOGT       FROM ENVIRONMENT-VALUE
              INSPECT CWLOGT CONVERTING MINUSCULAS TO MAIUSCULAS.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF  CWUNIX-GUI
               IF  X91-PARAMETER = 1
                   CALL 'CWUSER' USING "o" PARAMETROS-CWOBJE
               ELSE
                   CALL 'CWUSER' USING "O" PARAMETROS-CWOBJE
                                           CWOBJE-FUNCTION
               END-IF
               GOBACK
           END-IF
           IF  PARAMETROS-CWOBJE (1:1) = 'W' OR 'w'
Frango*        MOVE PARAMETROS-CWOBJE (11:2) TO POP-WINDOW(1:2)
               MOVE PARAMETROS-CWOBJE (1:11) TO DIM
               GOBACK
           END-IF
           IF  CWLOGT = 'ON'
           AND X91-PARAMETER = 1
               CALL "CWLOGT" USING "CWOBJE" PARAMETROS-CWOBJE
                                  LENGTH OF PARAMETROS-CWOBJE
           END-IF
           EVALUATE TRUE
                    WHEN CWOBJE-SCROLL
                     AND (CWOBJE-PIXEL-WIDTH (1:8) = SPACES
                       OR ((not CWOBJE-VERTICAL)
                        AND (not CWOBJE-HORIZONTAL)))
                        CONTINUE
                    WHEN X91-PARAMETER > 1
                     AND CWOBJE-OCCURS
                         MOVE 0           TO CWOBJE-OCCURS-NUMBER
                         MOVE POP-WINDOW  TO OBJETOS-WINDOW
                         MOVE HIGH-VALUES TO OBJETOS-SEQUENCE(1:)
                         START OBJETOS KEY NOT > OBJETOS-CHAVE
                         IF FS-OBJETOS = '00'
                            READ OBJETOS PREVIOUS RECORD
                            IF  FS-OBJETOS = '00'
                            AND OBJETOS-WINDOW = POP-WINDOW
                                MOVE OBJETOS-SEQUENCE
                                  TO CWOBJE-OCCURS-NUMBER
                            END-IF
                         END-IF
                    WHEN X91-PARAMETER > 1
                     AND CWOBJE-GET
                         MOVE CWOBJE-OCCURS-NUMBER TO OBJETOS-SEQUENCE
                         MOVE POP-WINDOW           TO OBJETOS-WINDOW
                         READ OBJETOS
                         COMPUTE SIZE-MOVEL = SIZE-REG
                                            - LENGTH OF OBJETOS-FIXO
                         MOVE OBJETOS-DATA TO PARAMETROS-CWOBJE
                    WHEN CWOBJE-DROP
                         MOVE POP-WINDOW TO OBJETOS-WINDOW
                         MOVE 0          TO OBJETOS-SEQUENCE
                         START OBJETOS KEY NOT < OBJETOS-CHAVE
                         PERFORM UNTIL FS-OBJETOS > "00"
                                  READ OBJETOS NEXT RECORD
                                  IF  FS-OBJETOS < "10"
                                      IF OBJETOS-WINDOW NOT = POP-WINDOW
                                         EXIT PERFORM
                                      END-IF
                                      DELETE OBJETOS RECORD
                                  END-IF
                         END-PERFORM
                         IF OBJETOS-SEQUENCE > 0
                         AND OBJETOS-WINDOW = POP-WINDOW
                            DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
                            DISPLAY "X"          UPON ENVIRONMENT-VALUE
                         END-IF
                         IF   DELETE-CWREAD = 1
                              MOVE 0 TO DELETE-CWREAD
                              CALL "CWREAD" USING "D"
                                                  CWOBJE-ORDER
                                                  CWOBJE-STRING-1(1)
                                                  CWOBJE-STRING-2(1)
                                                  CWOBJE-VERTICAL-LENGTH
                                                  CWOBJE-WORK-AREA
                         END-IF
                         INITIALIZE PARAMETROS-CWOBJE
      *                  CALL "CWSCRE" USING X"00"
                    WHEN CWOBJE-SCROLL
                     AND (CWOBJE-THUMB-LENGTH NOT = 5)
                         EXEC COBOLware Send
                              Message
                        'Scroll bar requer vari vel THUMB com 5 bytes'
                         END-EXEC
                    WHEN OTHER
                         IF   CWOBJE-PUSH-BUTTON
                         AND (CWOBJE-IMAGE NOT = SPACES)
                             SET CWOBJE-ICON TO TRUE
                         END-IF
                         IF   CWOBJE-SMALL
                              IF   CWOBJE-PUSH-BUTTON
                                   SET CWOBJE-PUSH-MOUSE TO TRUE
                              ELSE
                                   IF CWOBJE-ICON
                                      SET CWOBJE-MICRO-ICON TO TRUE
                                   END-IF
                              END-IF
                              MOVE SPACE TO CWOBJE-FLAG
                         END-IF
                         PERFORM 010-LIMITS THRU 010-99-FIM
                         IF  CWOBJE-LIST-BOX
                         AND CWOBJE-STRING-2-LENGTH = 0
                         AND CWOBJE-STRING-1-LENGTH =
                             CWOBJE-HORIZONTAL-LENGTH
                             IF  CWUNIX-GUI
                                 ADD 2 TO CWOBJE-STRING-1-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                             ELSE
                                 ADD 1 TO CWOBJE-STRING-1-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                             END-IF
                         END-IF
                         IF  CWOBJE-LIST-BOX
                         AND CWOBJE-STRING-1-LENGTH = 0
                         AND CWOBJE-STRING-2-LENGTH =
                             CWOBJE-HORIZONTAL-LENGTH
                             IF  CWUNIX-GUI
                                 ADD 2 TO CWOBJE-STRING-2-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                             ELSE
                                 ADD 1 TO CWOBJE-STRING-2-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                             END-IF
                         END-IF
                         IF  (CWOBJE-LIST-BOX
                         OR   CWOBJE-COMBO-BOX)
                         AND  CWOBJE-PROGRAM = SPACES
                              MOVE OBJETOS-SEQUENCE (1: 2)
                                TO CWOBJE-WORK-AREA (1: 2)
                              MOVE CWOBJE-LABEL TO CWOBJE-WORK-AREA (4:)
                              PERFORM 022-LOAD-CWREAD THRU 022-99-FIM
                              MOVE 0 TO SIZE-MOVEL
                         ELSE
                              MOVE 0 TO STRINGS
                              PERFORM VARYING G FROM 1 BY 1
                                        UNTIL G > 100
                                      IF CWOBJE-STRING-1(G) NOT = SPACES
                                         ADD 1 TO STRINGS
                                      END-IF
                              END-PERFORM
                              COMPUTE SIZE-MOVEL = STRINGS * 160
                         END-IF
                         COMPUTE SIZE-REG = LENGTH OF OBJETOS-FIXO + 4
                                          + SIZE-MOVEL
                         INSPECT CWOBJE-OPTION
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                         MOVE PARAMETROS-CWOBJE TO OBJETOS-DATA
                         MOVE POP-WINDOW        TO OBJETOS-WINDOW
                         WRITE OBJETOS-REG
                         DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
                         DISPLAY "X"          UPON ENVIRONMENT-VALUE
                         INITIALIZE PARAMETROS-CWOBJE
           END-EVALUATE.

       000-99-FIM. GOBACK.

       010-LIMITS.

           MOVE POP-WINDOW  TO OBJETOS-WINDOW
           MOVE HIGH-VALUES TO OBJETOS-SEQUENCE(1:)
           START OBJETOS KEY NOT > OBJETOS-CHAVE
           IF FS-OBJETOS = '00'
              READ OBJETOS PREVIOUS RECORD
              IF  FS-OBJETOS = '00'
              AND OBJETOS-WINDOW = POP-WINDOW
                  ADD 1 TO OBJETOS-SEQUENCE
              ELSE
                  MOVE '23' TO FS-OBJETOS
              END-IF
           END-IF
           IF FS-OBJETOS > '09'
              MOVE POP-WINDOW TO OBJETOS-WINDOW
              MOVE 1          TO OBJETOS-SEQUENCE
           END-IF
           INITIALIZE OBJETOS-DATA

           IF   CWOBJE-LIST-BOX
                INSPECT CWOBJE-OPTION
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF

           IF   CWOBJE-PUSH-MOUSE
                MOVE 1 TO CWOBJE-VERTICAL-LENGTH
           END-IF

           IF   CWOBJE-LINE = 0
                MOVE 1 TO CWOBJE-LINE
           END-IF

           IF   CWOBJE-COLUMN = 0
                MOVE 1 TO CWOBJE-COLUMN
           END-IF

           IF   CWOBJE-VERTICAL-LENGTH = 0
                MOVE 1 TO CWOBJE-VERTICAL-LENGTH
           END-IF

           IF   CWOBJE-HORIZONTAL-LENGTH = 0
           AND (CWOBJE-PUSH-BUTTON
             OR CWOBJE-PUSH-MOUSE
             OR CWOBJE-TEXT)
                MOVE 1 TO CWOBJE-HORIZONTAL-LENGTH
                IF CWOBJE-LABEL NOT = SPACES
                   PERFORM VARYING CWOBJE-HORIZONTAL-LENGTH
                                   FROM 80 BY -1
                                   UNTIL CWOBJE-LABEL
                           (CWOBJE-HORIZONTAL-LENGTH: 1) NOT = SPACES
                           CONTINUE
                   END-PERFORM
                   PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > CWOBJE-HORIZONTAL-LENGTH
                           IF CWOBJE-LABEL (I: 1) = "~"
                              SUBTRACT 1 FROM CWOBJE-HORIZONTAL-LENGTH
                           END-IF
                   END-PERFORM
                   IF   CWOBJE-PUSH-BUTTON
                        ADD 4 TO CWOBJE-HORIZONTAL-LENGTH
                   END-IF
                END-IF
           END-IF

           PERFORM UNTIL CWOBJE-VERTICAL-LENGTH < (SCR-LINES - 1)
                     OR  CWOBJE-VERTICAL-LENGTH < 2
                   SUBTRACT 1 FROM CWOBJE-VERTICAL-LENGTH
           END-PERFORM

           PERFORM UNTIL CWOBJE-HORIZONTAL-LENGTH < (SCR-SIZE - 1)
                      OR CWOBJE-HORIZONTAL-LENGTH < 2
                   SUBTRACT 1 FROM CWOBJE-HORIZONTAL-LENGTH
           END-PERFORM

           PERFORM UNTIL (CWOBJE-LINE + CWOBJE-VERTICAL-LENGTH)
                                      < (SCR-LINES + 1)
                      or (CWOBJE-COMBO-BOX)
                    OR (CWOBJE-PUSH-MOUSE
                        AND CWOBJE-LINE < (SCR-LINES + 1))
                   OR CWOBJE-LINE < 2
                   SUBTRACT 1 FROM CWOBJE-LINE
           END-PERFORM

           PERFORM UNTIL (CWOBJE-COLUMN + CWOBJE-HORIZONTAL-LENGTH)
                       < (SCR-SIZE + 1)
                   OR CWOBJE-COLUMN < 2
                   SUBTRACT 1 FROM CWOBJE-COLUMN
           END-PERFORM.

       010-99-FIM. EXIT.

       022-LOAD-CWREAD.

           MOVE 1        TO DELETE-CWREAD
           MOVE "CWREAD" TO CWOBJE-PROGRAM
           IF   STRINGS = 0
                MOVE 99 TO STRINGS
           END-IF
           IF   CWOBJE-ORDER = 0
           AND (CWOBJE-STRING-1 (1) NOT = SPACES)
                MOVE 1 TO CWOBJE-ORDER
           END-IF
           IF   CWOBJE-RETURN = 0
                MOVE CWOBJE-ORDER TO CWOBJE-RETURN
           END-IF
           MOVE 0 TO STRINGS
           PERFORM VARYING G FROM 1 BY 1 UNTIL G > 100
                   IF ((CWOBJE-STRING-1 (G) NOT = SPACES)
                   AND  CWOBJE-ORDER = 1)
                   OR ((CWOBJE-STRING-2 (G) NOT = SPACES)
                   AND  CWOBJE-ORDER = 2)
                        ADD 1 TO STRINGS
                        CALL "CWREAD" USING "L" CWOBJE-ORDER
                                             CWOBJE-STRING-1 (G)
                                             CWOBJE-STRING-2 (G)
                                             CWOBJE-VERTICAL-LENGTH
                                             CWOBJE-WORK-AREA
                   END-IF
           END-PERFORM.

       022-99-FIM. EXIT.
