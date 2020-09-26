       IDENTIFICATION DIVISION.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/1998.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do COBOL COBRA em         *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Execucao de comandos do sistema operacioal    *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.
           05 LEN                      PIC  9(003) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 P                        PIC  9(003) VALUE 0.
           05 CMDLINE                  PIC  X(266) VALUE SPACES.
           05 PROGRAMA                 PIC  X(050) VALUE SPACES.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.

       COPY CWEXEC.

       LINKAGE SECTION.

       01  LINCMD         PIC X(255).
       01  LENGTH-LINCMD  PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING LINCMD LENGTH-LINCMD.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 0
                GOBACK
           ELSE
                IF   X91-PARAMETER = 1
                     PERFORM VARYING LEN FROM 1 BY 1
                             UNTIL LINCMD  (LEN:1)  = X'00'
                                OR LEN > 255
                             CONTINUE
                     END-PERFORM
                ELSE
                     IF LENGTH-LINCMD (1:1) NUMERIC
                        MOVE LENGTH-LINCMD (1: 3) TO LEN
                     ELSE
                        MOVE LENGTH-LINCMD TO LEN
                     END-IF
                END-IF
           END-IF

           MOVE SPACES TO CMDLINE PROGRAMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
                                         OR (LINCMD (I:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM

           IF  LINCMD (I:7) = 'runcob '
               ADD 7 TO I
               PERFORM VARYING I FROM I BY 1 UNTIL I > LEN
                                   OR LINCMD (I:1) NOT = SPACE
                       CONTINUE
               END-PERFORM
               PERFORM VARYING I FROM I BY 1 UNTIL I > LEN
                                   OR LINCMD (I:1) = '.' OR SPACE
                       CONTINUE
               END-PERFORM
               IF I > LEN
                  MOVE 256 TO LEN
               ELSE
                  PERFORM VARYING I FROM I BY -1
                                    UNTIL LINCMD (I:1) = SPACE
                                       OR '/' OR '\'
                          CONTINUE
                  END-PERFORM
                  ADD  1 TO I
                  MOVE 0 TO P
                  PERFORM VARYING I FROM I BY 1
                            UNTIL I > LEN
                               OR LINCMD(I:1) = '.' OR SPACE
                          ADD 1 TO P
                          MOVE LINCMD(I:1) TO PROGRAMA (P:1)
                  END-PERFORM
                  PERFORM VARYING I FROM I BY 1
                            UNTIL I > LEN
                               OR LINCMD(I:1) = SPACE
                          CONTINUE
                  END-PERFORM
                  PERFORM VARYING I FROM I BY 1
                            UNTIL I > LEN
                               OR (LINCMD(I:1) NOT = SPACE)
                          CONTINUE
                  END-PERFORM
                  MOVE 0 TO P
                  PERFORM VARYING I FROM I BY 1
                            UNTIL I > LEN
                          ADD 1 TO P
                          MOVE LINCMD(I:1) TO CMDLINE (P:1)
                  END-PERFORM
                  IF PROGRAMA = SPACES
                     MOVE 256 TO LEN
                  ELSE
                     CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                       CARACTER-BUFFER
                                                       ATTRIBUTE-BUFFER
                                                       STRING-LENGTH
                     INSPECT PROGRAMA
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                     DISPLAY CMDLINE UPON COMMAND-LINE
                     CALL PROGRAMA USING CMDLINE
                     DISPLAY "CWCANCELED" UPON ENVIRONMENT-NAME
                     DISPLAY PROGRAMA     UPON ENVIRONMENT-VALUE
                     CALL "CWSQLC" USING "K*"
                     CANCEL PROGRAMA
                     CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                        CARACTER-BUFFER
                                                        ATTRIBUTE-BUFFER
                                                        STRING-LENGTH
                     GOBACK
                  END-IF
               END-IF
           ELSE
               MOVE LINCMD (1: LEN) TO CWEXEC-COMANDO
           END-IF

           IF   LEN > 255
           OR   LEN = 0
           OR   LINCMD = SPACES
                EXEC COBOLware Send
                     Message 'Erro no uso da COBRASYS'
                END-EXEC
                MOVE 255 TO RETURN-CODE
                GOBACK
           END-IF

           SET CWEXEC-HIDE  TO TRUE
           MOVE CMDLINE     TO CWEXEC-COMMAND
           CALL "CWEXE2" USING PARAMETROS-CWEXEC.

       000-99-FIM. GOBACK.
