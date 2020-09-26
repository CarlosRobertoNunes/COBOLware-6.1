       IDENTIFICATION DIVISION.
       AUTHOR.        COBOLware Services Ltda.
      *PROGRAM-ID.    CWEXEC. Flagrada por causa da CWEXE2 qu‚ ‚ c¢pia
       DATE-WRITTEN.  09/10/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Executa comando do sistema operacional       *
                      *                                               *
                      *************************************************
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.                  DECIMAL-POINT IS COMMA
                                       CALL-CONVENTION 74 IS WINAPI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  RUN-UNIT-ID                PIC X(08) COMP-5.
       77  STACK-SIZE                 PIC X(04) COMP-5.
       77  FLAGS                      PIC X(04) COMP-5.
       01  CmdShow                    PIC 9(04) COMP-5.
       01  CmdStatus                  PIC 9(04) COMP-5.

       01  AREAS-DE-TRABALHO.
           05 PROGRAMA-COBOL          PIC  X(255) VALUE SPACES.
           05 Y                       PIC  9(002) COMP-X VALUE 0.
           05 CmdLine                 PIC  X(255) VALUE SPACES.
           05 RESULTADO               PIC  9(002) COMP-X VALUE 0.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
           05 X91-PARAMETRO.
              10 I                    PIC  9(002) COMP-X VALUE 0.
              10 COMSPEC              PIC  X(255) VALUE SPACES.

       COPY CWUNIX.
       COPY CWSEND.

       LINKAGE SECTION.

       COPY CWEXEC.

       PROCEDURE DIVISION USING PARAMETROS-CWEXEC.

       000-INICIO.

           MOVE 0 TO RETURN-CODE RESULTADO
           ON   1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   NOT CWUNIX-GUI
                CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           END-IF

           IF   CWEXEC-COMANDO (1: 1) = ":"
                MOVE SPACES TO PROGRAMA-COBOL
                MOVE 0      TO Y
                PERFORM VARYING I FROM 2 BY 1
                        UNTIL CWEXEC-COMANDO (I: 1) = SPACE
                           OR I > LENGTH OF CWEXEC-COMANDO
                              ADD  1 TO Y
                              MOVE CWEXEC-COMANDO (I: 1)
                                TO PROGRAMA-COBOL (Y: 1)
                END-PERFORM
                ADD 1 TO I
                MOVE CWEXEC-COMANDO (I: ) TO CMDLINE
                DISPLAY CMDLINE UPON COMMAND-LINE
                CALL PROGRAMA-COBOL
                     ON OVERFLOW
                        MOVE SPACES TO CWSEND-MSG
                        STRING "Imposs¡vel executar o programa "
                               DELIMITED BY SIZE
                        PROGRAMA-COBOL DELIMITED BY SPACE
                                INTO CWSEND-MSG
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                        MOVE 1 TO CWEXEC-RETORNO
                     END-CALL
                CANCEL PROGRAMA-COBOL
                GO TO FIM
           END-IF

           IF   CWEXEC-RETORNO = 3
           AND (NOT CWUNIX-DOS16)
           AND (NOT CWUNIX-WIN16)
                MOVE SPACES TO COMSPEC
                PERFORM VARYING I FROM 255 BY -1
                          UNTIL I = 1
                             OR CWEXEC-COMANDO (I: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                CALL "CBL_EXEC_RUN_UNIT" USING CWEXEC-COMANDO  (1: I)
                            BY VALUE LENGTH OF CWEXEC-COMANDO  (1: I)
                                            BY REFERENCE RUN-UNIT-ID
                                            BY VALUE     STACK-SIZE
                                               FLAGS
                GOBACK
           END-IF

           IF  (CWEXEC-RETORNO NOT = 3)
           AND (CWEXEC-HIDE)
           AND (CWEXEC-RETORNO NOT = 4)
           AND (CWEXEC-RETORNO NOT = 5)
           AND (CWUNIX-DOS32 OR CWUNIX-ON)
                CALL "CBL_CLEAR_SCR" USING " " X"07"
           END-IF

           IF   CWUNIX-DOS16
           OR   CWUNIX-WIN16
                IF   CWUNIX-DOS16
                     DISPLAY CWEXEC-COMANDO UPON COMMAND-LINE
                     CALL X"91" USING RESULTADO X"23" X"00"
                ELSE
                     MOVE SPACES TO COMSPEC
                     STRING " /C" CWEXEC-COMANDO DELIMITED BY SIZE
                                 INTO COMSPEC
                     DISPLAY COMSPEC UPON COMMAND-LINE
                     DISPLAY "COMSPEC" UPON ENVIRONMENT-NAME
                     ACCEPT   COMSPEC  FROM ENVIRONMENT-VALUE
                     PERFORM VARYING I FROM 255 BY -1
                                      UNTIL I = 0
                                         OR (COMSPEC (I: 1) NOT = SPACE)
                             CONTINUE
                     END-PERFORM
                     CALL X"91" USING RESULTADO X"23" X91-PARAMETRO
                END-IF
           ELSE
                MOVE CWEXEC-COMANDO TO CMDLINE
                PERFORM VARYING I FROM 255 BY -1
                                 UNTIL I = 0
                                    OR (CMDLINE (I: 1) NOT = SPACE)
                        CONTINUE
                END-PERFORM
                IF   CWUNIX-ON
                AND  CWEXEC-HIDE
                     ADD  1     TO I
                     MOVE ">/dev/null" TO CMDLINE (I: 1)
                     ADD  10    TO I
                END-IF
                ADD  1     TO I
                MOVE X"00" TO CMDLINE (I: 1)
                IF   CWUNIX-ON
                OR ((NOT CWEXEC-ASSYNCRONE) AND (NOT CWEXEC-HIDE))
                     IF CWEXEC-ASSYNCRONE AND CWUNIX-ON
                        MOVE ' &'  TO CMDLINE (I: 2)
                        ADD 2      TO I
                        MOVE X"00" TO CMDLINE (I: 1)
                     END-IF
                     CALL "system" USING CMDLINE
                ELSE
      *              IF CmdLine(1:1) NOT = "@"
      *                 MOVE "@"     TO COMSPEC
      *                 MOVE CmdLine TO COMSPEC(2:)
      *                 MOVE COMSPEC TO CmdLine
      *              END-IF
                     MOVE 7                      TO CmdShow
                     IF  CWEXEC-HIDE
                         MOVE 0                  TO CmdShow
                     END-IF
                     MOVE 0                      TO CmdStatus
                     CALL WINAPI "WinExec" USING BY REFERENCE CmdLine
                                                 BY VALUE     CmdShow
                                                 RETURNING    CmdStatus
                     IF CmdStatus = 2
      *                          OR 33
                        CALL "system" USING CMDLINE
                     END-IF
                END-IF
           END-IF

           IF  (RETURN-CODE NOT = 0)
           OR  (RESULTADO   NOT = 0)
                MOVE 1 TO CWEXEC-RETORNO
                IF  NOT CWEXEC-NOWARNING
                    MOVE "Execu‡Æo com erro:" TO CWSEND-MSG
                    CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
           ELSE
                MOVE 0 TO CWEXEC-RETORNO
           END-IF.

       FIM.
           IF   NOT CWUNIX-GUI
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
           END-IF

           MOVE SPACE TO CWEXEC-OPTION.

       000-99-FIM. GOBACK.
