       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPROC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/09/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Janela de progresso de processamento         *
                      *  com button "Cancelar"                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
frango     05 JANPOS.
frango        10 JANLIN                PIC  9(002) VALUE 0.
frango        10 JANCOL                PIC  9(002) VALUE 0.
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 LARGURA                  PIC  9(002) VALUE 0.
           05 ALTURA                   PIC  9(002) VALUE 0.
           05 CHAR                     PIC  X(001) VALUE SPACE.
           05 WS-MSG                   PIC  X(046) VALUE SPACES.
           05 COLR                     PIC  X(001) VALUE SPACE.
           05 NEWMSG                   PIC  9(001) VALUE 0.
           05 STATUS-WINDOW            PIC  9(002) VALUE 0.
           05 IDS.
              10 Y                     PIC  9(003) VALUE ZERO.
              10 I                     PIC  9(003) VALUE ZERO.
              10 P                     PIC  9(003) VALUE ZERO.
              10 DIR                   PIC  9(001) VALUE ZERO.
              10 PA                    PIC  9(005) VALUE ZERO.
              10 PERC                  PIC  9(005) VALUE 0.
              10 OPCAO                 PIC  9(001) VALUE 0.
              10 VEZ                   PIC  9(001) VALUE 0.
           05 BARRA-PROGRESSO.
              10 PERC-MSG              PIC  X(046) VALUE SPACES.
              10 PERC-TXT.
                 15 PERC-ED            PIC     ZZ9 VALUE 0.
                 15 FILLER             PIC  X(001) VALUE "%".
           05 PROC-TXT                 PIC  X(050) VALUE SPACES.
           05 VAZIO                    PIC  X(001) VALUE X"70".
           05 CHEIO                    PIC  X(001) VALUE X"1F".
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER         PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER      PIC  9(002) COMP-X VALUE 0.
              10 CHARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER      PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH         PIC  9(004) COMP-X VALUE 0.

       LINKAGE SECTION.

       COPY CWPROC.

       PROCEDURE DIVISION USING PARAMETROS-CWPROC.

       000-INICIO.

           ON 1 INITIALIZE IDS.
           EVALUATE TRUE
                    WHEN CWPROC-SHOW
frango                   DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango                   ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango                   add JANLIN to CWPROC-LINE
frango                   add JANCOL to CWPROC-COLUMN
frango                   DISPLAY '0000'        UPON ENVIRONMENT-VALUE
                         IF   STATUS-WINDOW = 0
                              PERFORM 100-OPEN  THRU 100-99-FIM
                         END-IF
                         PERFORM 200-SHOW  THRU 200-99-FIM
frango                   subtract JANLIN from CWPROC-LINE
frango                   subtract JANCOL from CWPROC-COLUMN
frango                   DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango                   DISPLAY JANPOS        UPON ENVIRONMENT-VALUE
                    WHEN CWPROC-CLOSE
                         PERFORM 300-CLOSE THRU 300-99-FIM
           END-EVALUATE

           CALL "CWATCH".

       000-99-FIM. GOBACK.

       100-OPEN.

           INSPECT CWPROC-TEXTS CONVERTING X"00" TO SPACE
           MOVE 0               TO PA P
           MOVE 52              TO LARGURA
           MOVE CWPROC-MSG      TO WS-MSG
           INSPECT WS-MSG CONVERTING MINUSCULAS
                                TO MINUSCULAS
           MOVE 1               TO ALTURA
           IF  WS-MSG = "OFF"
               MOVE SPACES      TO CWPROC-MSG
               MOVE 50          TO LARGURA
           END-IF
           IF   CWPROC-HEADER = SPACES
                MOVE "Processamento" TO CWPROC-HEADER
      *    ELSE
txt   *         CALL "CWTEXT" USING CWPROC-HEADER
txt   *                   LENGTH OF CWPROC-HEADER
           END-IF
           EXEC COBOLware BoxWindow (OPEN)
                LINE   CWPROC-LINE
                COLUMN CWPROC-COLUMN
                WIDTH LARGURA
                HEIGHT ALTURA
                COLOR-FRAME 127
                COLOR-BORDER 127
           END-EXEC
           MOVE 1 TO STATUS-WINDOW
           PERFORM VARYING I FROM LENGTH OF CWPROC-HEADER
                               BY -1
                               UNTIL CWPROC-HEADER (I: 1) NOT = SPACES
                   CONTINUE
           END-PERFORM
           COMPUTE Y = CWPROC-COLUMN + 1
      *    DISPLAY (CWPROC-LINE, Y) CWPROC-HEADER WITH SIZE I HIGH
           MOVE I TO STRING-LENGTH
           compute ROW-NUMBER    = CWPROC-LINE - 1
           compute COLUMN-NUMBER = Y           - 1
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                            CWPROC-HEADER
                                            STRING-LENGTH
      *    IF  WS-MSG NOT = "OFF"
      *        EXECUTE COBOLware OBJECT (PUSH-BUTTON)
      *                  KEY ESC LINE 08 COLUMN 53 WIDTH 9
      *                  Caption "~Cancelar"
      *        END-EXEC
      *    END-IF

           MOVE CWPROC-MSG TO PERC-MSG
txt   *    CALL "CWTEXT" USING PERC-MSG LENGTH OF PERC-MSG
      *    INSPECT PERC-MSG CONVERTING ACENTOS-850
      *                             TO ACENTOS-WINDOWS
           MOVE CWPROC-LINE TO ROW-NUMBER
           IF  WS-MSG = "OFF"
               COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + 0
           ELSE
               COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + 1
           END-IF
           MOVE 0               TO PERC-ED
           PERFORM 150-NEWMSG THRU 150-99-FIM.

       100-99-FIM. EXIT.

       150-NEWMSG.

           MOVE CWPROC-MSG TO PERC-MSG PROC-TXT

           IF   CWPROC-EMPTY = 0
                MOVE PROC-TXT TO CHARACTER-BUFFER
                MOVE 50       TO STRING-LENGTH
           ELSE
                MOVE BARRA-PROGRESSO TO CHARACTER-BUFFER
                MOVE 46              TO STRING-LENGTH
           END-IF
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                            CHARACTER-BUFFER
                                            STRING-LENGTH.
      *                                          VAZIO.
       150-99-FIM. EXIT.

       200-SHOW.

           IF   CWPROC-MSG NOT = PERC-MSG
                MOVE 1          TO NEWMSG
                PERFORM 150-NEWMSG THRU 150-99-FIM
           END-IF

           IF   CWPROC-EMPTY = 0
                PERFORM 240-SERPENTE         THRU 240-99-FIM
           ELSE
                PERFORM 250-DISPLAY-PROGRESS THRU 250-99-FIM
           END-IF

           IF  WS-MSG NOT = "OFF"
               CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
               SET CWPROC-CONTINUE TO TRUE
               IF   KEY-STATUS   = 1
                    CALL "CBL_READ_KBD_CHAR" USING CHAR
                    IF   CHAR = X"1B"
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

           IF  CWPROC-FULL = CWPROC-EMPTY
           AND CWPROC-FULL > 0
               PERFORM 300-CLOSE THRU 300-99-FIM
           END-IF.

       200-99-FIM. EXIT.

       250-DISPLAY-PROGRESS.

           COMPUTE PERC    = CWPROC-FULL / CWPROC-EMPTY * 100
           MOVE PERC TO PERC-ED

           IF  (PERC > 2 AND PERC < 101) OR NEWMSG = 1
               COMPUTE P = PERC / 2
               IF  (PERC NOT = PA) OR (NEWMSG = 1 AND P > 0)
                    MOVE CWPROC-LINE TO ROW-NUMBER
                    IF  P < 47
                        COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + 47
                        MOVE 4          TO STRING-LENGTH
                        MOVE PERC-TXT   TO CHARACTER-BUFFER
                    ELSE
                        IF  P < 50
                            COMPUTE I = 50 - P
                            MOVE I          TO STRING-LENGTH
                            COMPUTE COLUMN-NUMBER = CWPROC-COLUMN
                                                  + 51 - I
                            COMPUTE I = 5 - I
                            MOVE PERC-TXT (I: ) TO CHARACTER-BUFFER
                        END-IF
                    END-IF
                    IF  WS-MSG = "OFF"
                        SUBTRACT 1 FROM COLUMN-NUMBER
                    END-IF
                    CALL "CBL_WRITE_SCR_CHARS_ATTR" USING
                                                    SCREEN-POSITION
                                                    CHARACTER-BUFFER
                                                    STRING-LENGTH
                                                    VAZIO
nena                COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + 1
                    MOVE P          TO STRING-LENGTH
                    MOVE BARRA-PROGRESSO (1: P) TO CHARACTER-BUFFER
                    IF  WS-MSG = "OFF"
                        SUBTRACT 1 FROM COLUMN-NUMBER
                    END-IF
      *             CALL "CBL_WRITE_SCR_CHARS_ATTR" USING
      *                                        SCREEN-POSITION
      *                                        CHARACTER-BUFFER
      *                                        STRING-LENGTH
      *                                        CHEIO
                    CALL "CBL_WRITE_SCR_N_ATTR" USING
                                               SCREEN-POSITION
                                               CHEIO
                                               STRING-LENGTH
                    MOVE PERC TO PA
               END-IF
           END-IF

           MOVE 0 TO NEWMSG.

       250-99-FIM. EXIT.

       240-SERPENTE.

           ADD 1 TO VEZ
           IF  VEZ NOT = 1
               GO TO 240-99-FIM
           END-IF

           ADD 1 TO P

           IF  P > 50
               IF  DIR = 0
                   MOVE 1 TO DIR
               ELSE
                   MOVE 0 TO DIR
               END-IF
               MOVE 1 TO P
           END-IF

           IF  DIR = 0
               MOVE VAZIO      TO COLR
           ELSE
               MOVE CHEIO      TO COLR
           END-IF
           MOVE CWPROC-LINE TO ROW-NUMBER
           COMPUTE I = 50 - P
           MOVE    I   TO STRING-LENGTH
           COMPUTE PA = P + 1
           COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + PA
           MOVE PROC-TXT (PA: ) TO CHARACTER-BUFFER
           IF  WS-MSG = "OFF"
               SUBTRACT 1 FROM COLUMN-NUMBER
           END-IF

           CALL "CBL_WRITE_SCR_CHARS_ATTR" USING
                                           SCREEN-POSITION
                                           CHARACTER-BUFFER
                                           STRING-LENGTH
                                           COLR
           IF  DIR = 0
               MOVE CHEIO   TO COLR
           ELSE
               MOVE VAZIO   TO COLR
           END-IF

           COMPUTE COLUMN-NUMBER = CWPROC-COLUMN + 1
           MOVE P          TO STRING-LENGTH
           MOVE PROC-TXT   TO CHARACTER-BUFFER
           IF  WS-MSG = "OFF"
               SUBTRACT 1 FROM CWPROC-COLUMN
           END-IF
           CALL "CBL_WRITE_SCR_CHARS_ATTR" USING
                                           SCREEN-POSITION
                                           CHARACTER-BUFFER
                                           STRING-LENGTH
                                           COLR.
       240-99-FIM. EXIT.

       300-CLOSE.

           IF   STATUS-WINDOW = 1
                MOVE 0 TO CWPROC-FULL
                          CWPROC-EMPTY
                          STATUS-WINDOW
                MOVE 15 TO CWPROC-LINE
                MOVE 15 TO CWPROC-COLUMN
                MOVE SPACES TO CWPROC-HEADER
                               CWPROC-MSG
                               CWPROC-QUESTION
                EXEC COBOLware BOXW (CLOSE) END-EXEC
           END-IF.

       300-99-FIM. EXIT.
       END PROGRAM CWPROC.
