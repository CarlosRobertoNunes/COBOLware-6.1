       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPRTS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  12/12/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Imprime a tela corrente                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TEXTO ASSIGN TO DISK
                  LOCK MODE    IS EXCLUSIVE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-TEXTO.

       DATA DIVISION.
       FILE SECTION.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG PIC X(080).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 R                 COMP-X PIC  9(002) VALUE 0.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 SCREEN-POSITION.
              10 ROW-NUMBER            PIC 9(0002) COMP-X VALUE 0.
              10 COLUMN-NUMBER         PIC 9(0002) COMP-X VALUE 0.
           05 CHARACTER-BUFFER         PIC X(2000) VALUE SPACES.
           05 REDEFINES CHARACTER-BUFFER.
              10  WS-TELA OCCURS 025   PIC X(080).
           05 STRING-LENGTH            PIC 9(0004) COMP-X VALUE 1999.
           05 ER-TEXTO.
              10 FS-TEXTO              PIC  X(002) VALUE "00".
              10 LB-TEXTO              PIC  X(255) VALUE SPACES.
           05 L                        PIC  9(002) VALUE 0.
           05 TEMP                     PIC  X(255) VALUE SPACES.
           05 TELA-RADIO VALUE SPACES.
              10 TELA-LIN OCCURS 25.
                 15 TELA-COL PIC X(80).
       01  CmdLine                     PIC  X(255) VALUE SPACES.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  RADIOS.
           05 LIN-R PIC 99.
           05 COL-R PIC 99.
           05 SET-R PIC X(3).

       01  CB PIC X(2000).
       PROCEDURE DIVISION USING RADIOS CB.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 1
                SUBTRACT 1 FROM COL-R
      *         INSPECT SET-R CONVERTING '-+' TO ' *'
                INSPECT SET-R CONVERTING X'2D2B' TO X'2007'
                MOVE SET-R TO TELA-COL (LIN-R)(COL-R:3)
                GOBACK
           END-IF
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           DISPLAY "TEMP" UPON ENVIRONMENT-NAME
           ACCEPT TEMP    FROM ENVIRONMENT-VALUE
           IF   X91-PARAMETER = 2
                MOVE CB TO CHARACTER-BUFFER
           ELSE
                CALL "CBL_READ_SCR_CHARS" USING SCREEN-POSITION
                                                CHARACTER-BUFFER
                                                STRING-LENGTH
           END-IF
           IF CWUNIX-ON
              INSPECT CHARACTER-BUFFER
                      CONVERTING X'07090A00' TO X'2A2A2020'
           ELSE
              INSPECT CHARACTER-BUFFER
                      CONVERTING X'090A00' TO X'072020'
           END-IF
           MOVE SPACES TO LB-TEXTO
           IF   CWUNIX-ON
                MOVE 2000 TO STRING-LENGTH
                STRING TEMP  DELIMITED BY SPACE
                       "/screen.txt" DELIMITED BY SIZE
                        INTO LB-TEXTO
                OPEN OUTPUT TEXTO
                PERFORM VARYING L FROM 1 BY 1 UNTIL L > 25
                        WRITE TEXTO-REG FROM WS-TELA (L)
                END-PERFORM
                CLOSE TEXTO
           ELSE
                STRING TEMP  DELIMITED BY SPACE
                       "\screen.txt" DELIMITED BY SIZE
                        INTO LB-TEXTO
                PERFORM VARYING L FROM 1 BY 1 UNTIL L > 25
                        PERFORM VARYING R FROM 1 BY 1 UNTIL R > 80
                             IF TELA-COL(L)(R:1) NOT = SPACE
                                MOVE TELA-COL(L)(R:1) TO
                                     WS-TELA(L)(R:1)
                             END-IF
                        END-PERFORM
                        EXEC COBOLware Save File LB-TEXTO
                        UTF-8 RECORD WS-TELA(L)
                        END-EXEC
                END-PERFORM
                EXEC COBOLware Save File LB-TEXTO
                     CLOSE
                END-EXEC
           END-IF
           MOVE SPACES TO CMDLINE

           IF   CWUNIX-ON
                STRING "exec $EDITOR " DELIMITED BY SIZE
                        LB-TEXTO       DELIMITED BY SPACE
                  INTO CmdLine
           ELSE
                STRING "notepad " DELIMITED BY SIZE
                        LB-TEXTO DELIMITED BY SPACE
                        X"00" DELIMITED BY SIZE
                  INTO CmdLine
           END-IF

           EXEC COBOLware EXECsystem ASSYNCRONE
                COMMAND CmdLine
           END-EXEC.

       000-99-FIM. GOBACK.

       END PROGRAM CWPRTS.
