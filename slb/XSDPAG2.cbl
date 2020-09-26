       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDPAG2.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  pagina 2 das telas                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 GLB-INDEX            PIC  9(002) VALUE ZEROS.
           05 GLB-LINE             PIC  9(002) VALUE ZEROS.
           05 MODE-SS              PIC  X(001) VALUE SPACE.
           05 TASK                 PIC  9(006) VALUE ZEROS.
           05 TECLA                PIC  9(003) VALUE ZEROS. COPY CWKEYS.

       LINKAGE SECTION.

       01  GLB-MESSAGE.
           05 GLB-MSGCOPY          PIC 9(02) OCCURS 21 TIMES.
           05 GLB-MSGHEADER        PIC X(16) OCCURS 21 TIMES.
           05 GLB-MSGTRAILER       PIC X(50) OCCURS 21 TIMES.

       01  WRK-MESSAGE.
           05 WRK-MSGCOPY          PIC 9(02).
           05 WRK-MSGHEADER        PIC X(16).
           05 WRK-MSGTRAILER       PIC X(50).

       01  GLB-MSGINDEX            PIC 9(02).
       01  P2-ISPEC                PIC X(05).
       01  P2-TEACH                PIC X(05).
       01  P2-FLAG                 PIC X(01).
       01  GLB-LSN                 PIC X(05).

       SCREEN SECTION.

       01 APAGA BACKGROUND-COLOR IS 01.
          05 LINE 01 COLUMN 01 BLANK SCREEN.

       01 TELA BACKGROUND-COLOR IS 01.
          05 LINE 01 COLUMN 36 PIC X(5) USING P2-ISPEC.
          05 LINE 01 COLUMN 45 PIC X(5) USING P2-TEACH.
          05 LINE 01 COLUMN 54 PIC X(1) USING P2-FLAG.

       PROCEDURE DIVISION USING GLB-MESSAGE GLB-MSGINDEX
                                P2-ISPEC P2-TEACH P2-FLAG
                                GLB-LSN.
       P-PAGE2.

           IF   P2-FLAG = 1
                MOVE SPACES TO GLB-MESSAGE
                               P2-FLAG
                MOVE 0      TO GLB-MSGINDEX
                GOBACK
           END-IF

           EXEC COBOLware Object Drop END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 63 WIDTH 5
                     CAPTION " ~Bye "
                     KEY F1
                     TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 68 WIDTH 5
                     CAPTION " ~Pg1 "
                     KEY ESC
                     TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 73 WIDTH 5
                     CAPTION " ~Xmt "
                     KEY PAGE-DOWN
                     TAB-OFF
           END-EXEC
           IF   GLB-LSN NUMERIC
           AND  GLB-LSN NOT = ZEROS
                MOVE GLB-LSN TO TASK(2:)
           ELSE
                CALL "XSDTASK" USING TASK
           END-IF
           MOVE SPACES TO P2-ISPEC
           MOVE SPACES TO P2-TEACH
           MOVE "@"    TO P2-FLAG

           MOVE "D" TO MODE-SS
           PERFORM SS-PAGE2-SCREEN THRU FIM-PAGE2-SCREEN
           IF GLB-MSGINDEX NOT = ZEROS
              MOVE 3 TO GLB-LINE
              PERFORM VARYING GLB-INDEX FROM 1 BY 1
                      UNTIL GLB-INDEX > 21
                 IF GLB-MSGHEADER(GLB-INDEX) NOT = SPACES OR
                    GLB-MSGTRAILER(GLB-INDEX) NOT = SPACES
                    ADD 1 TO GLB-LINE
                 END-IF
              END-PERFORM
              PERFORM VARYING GLB-INDEX FROM 1 BY 1
                      UNTIL GLB-INDEX > 21
                 IF GLB-MSGHEADER(GLB-INDEX) NOT = SPACES OR
                    GLB-MSGTRAILER(GLB-INDEX) NOT = SPACES
                    MOVE "D" TO MODE-SS
                    IF GLB-MSGCOPY(GLB-INDEX) NOT = 0
                    DISPLAY GLB-MSGCOPY(GLB-INDEX)
                            LINE GLB-LINE COLUMN 01
                    END-IF
                    DISPLAY GLB-MSGHEADER(GLB-INDEX)
                            LINE GLB-LINE COLUMN 04 WITH REVERSE-VIDEO
                            GLB-MSGTRAILER(GLB-INDEX)
                            LINE GLB-LINE COLUMN 20
                    SUBTRACT 1 FROM GLB-LINE
                 END-IF
              END-PERFORM
           END-IF

           MOVE "F" TO MODE-SS
           PERFORM SS-PAGE2-SCREEN THRU FIM-PAGE2-SCREEN

           EVALUATE TRUE
              WHEN ESC
                   GO TO FIM
              WHEN F1
                   MOVE 'BYE' TO P2-ISPEC
                   SET ENTER-KEY TO TRUE
              WHEN ENTER-KEY CONTINUE
              WHEN PAGE-DOWN CONTINUE
              WHEN OTHER
                   MOVE 1 TO GLB-MSGINDEX
                   MOVE "ERROR" TO GLB-MSGHEADER(GLB-MSGINDEX)
                   MOVE "Tecla Invalida"
                        TO GLB-MSGTRAILER(GLB-MSGINDEX)
                   PERFORM VARYING GLB-INDEX FROM 2 BY 1
                                             UNTIL GLB-INDEX > 21
                     MOVE SPACES TO GLB-MSGHEADER(GLB-INDEX)
                     MOVE SPACES TO GLB-MSGTRAILER(GLB-INDEX)
                   END-PERFORM
                   GO TO P-PAGE2
           END-EVALUATE

           IF P2-ISPEC = SPACES
              MOVE 1 TO GLB-MSGINDEX
              MOVE "ERROR" TO GLB-MSGHEADER(GLB-MSGINDEX)
              MOVE "Nome do Ispec deve ser informado"
                   TO GLB-MSGTRAILER(GLB-MSGINDEX)
              PERFORM VARYING GLB-INDEX FROM 2 BY 1 UNTIL GLB-INDEX > 21
                 MOVE SPACES TO GLB-MSGHEADER(GLB-INDEX)
                 MOVE SPACES TO GLB-MSGTRAILER(GLB-INDEX)
              END-PERFORM
              GO TO P-PAGE2
           END-IF

           MOVE SPACES TO GLB-MESSAGE
                          P2-FLAG
           MOVE 0      TO GLB-MSGINDEX
           INSPECT P2-ISPEC CONVERTING MINUSCULAS TO MAIUSCULAS.
       fim.
           EXEC COBOLware Object Drop END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 68 WIDTH 5
                     CAPTION " ~Pg2 "
                     KEY ESC
                     TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 73 WIDTH 5
                     CAPTION " ~Xmt "
                     KEY PAGE-DOWN
                     TAB-OFF
           END-EXEC.

       P-PAGE2-EXT. GOBACK.

       SS-PAGE2-SCREEN.

           IF   MODE-SS = "D"
                DISPLAY APAGA
                   "Task" LINE 01 COLUMN 69
                   TASK   LINE 01 COLUMN 74
           END-IF

           DISPLAY "Informe Nome do Ispec"
                        LINE 01 COLUMN 12
                    "" AT 0135
                    "" AT 0141
                    "" AT 0144
                    "" AT 0150
                    "" AT 0153
                    "" AT 0155.

           IF  MODE-SS = "A" OR "F"
               DISPLAY TELA
               ACCEPT TELA
               ACCEPT tecla FROM ESCAPE KEY
           END-IF.

       FIM-PAGE2-SCREEN. EXIT.

       000-99-FIM. GOBACK.

       END PROGRAM XSDPAG2.
