      $Set NoCallFH
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLANG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  27/04/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Executa passo de integra‡Æo com linguagens   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HELP ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  LOCK MODE    IS EXCLUSIVE
                  FILE STATUS  IS FS-HELP.

       DATA DIVISION.
       FILE SECTION.

       FD  HELP
           VALUE OF FILE-ID LB-HELP.

       01  HELP-REG PIC X(080).

       WORKING-STORAGE SECTION.

       01  ER-HELP.
           05 FS-HELP PIC X(02) VALUE "00".
           05 LB-HELP PIC X(50) VALUE SPACES.

       LINKAGE SECTION.

       01  CWLANG-COMMAND PIC X(255).
       01  CWLANG-CONSOLE PIC X(014).
       01  CWLANG-ERRORS  PIC X(014).
       01  CWLANG-SOURCE  PIC X(050).
       01  CWLANG-EXE     PIC X(015).

       PROCEDURE DIVISION USING CWLANG-COMMAND
                                CWLANG-CONSOLE
                                CWLANG-ERRORS
                                CWLANG-SOURCE
                                CWLANG-EXE.

       000-INICIO.

           EXEC COBOLware ExecSystem NoWarning erase-off
                COMMAND CWLANG-COMMAND
           END-EXEC
           MOVE CWLANG-ERRORS TO LB-HELP
           PERFORM TEST AFTER UNTIL NOT FS-HELP = "9A"
                   OPEN INPUT HELP
                   IF  FS-HELP = "9A"
                       CALL "system" USING Z"sleep 1"
                   END-IF
           END-PERFORM
           IF FS-HELP < "10"
              READ HELP
              IF FS-HELP < "10"
                 CLOSE HELP
                 EXEC COBOLware Help
                      File CWLANG-ERRORS
                      Line 10 Column 10
                      Lines 5 Columns 50
                 END-EXEC
                 DELETE FILE HELP
                 MOVE CWLANG-CONSOLE TO LB-HELP DELETE FILE HELP
                 MOVE CWLANG-SOURCE  TO LB-HELP DELETE FILE HELP
                 MOVE CWLANG-EXE     TO LB-HELP DELETE FILE HELP
                 STOP RUN
              ELSE
                 CLOSE HELP
                 DELETE FILE HELP
              END-IF
           END-IF

           MOVE SPACES TO CWLANG-COMMAND.

       000-99-FIM. GOBACK.

       END PROGRAM CWLANG.
