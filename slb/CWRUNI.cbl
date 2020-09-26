       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRUNI.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  24/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Dispara programa em background (RUN-UNIT)    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  STACK-SIZE                  PIC X(004) COMP-5 VALUE 4096.
       01  FLAGS                       PIC X(004) COMP-5 VALUE 0.
       01  COMMAND-LINE-BUFFER         PIC X(128) VALUE SPACES.
       01  COMMAND-LINE-LEN            PIC X(004) COMP-5 VALUE 128.
       01  NEW-THREAD-ID               PIC X(008) COMP-5 VALUE 0.
       01  CMD                         PIC X(128) VALUE SPACES.

       LINKAGE SECTION.

       01   PROGRAMA                   PIC  X(128).
       01   RC                         PIC  9(001).

       PROCEDURE DIVISION USING PROGRAMA RC.

       000-INICIO.

           IF   PROGRAMA NOT = SPACES
                MOVE 0        TO RC
                MOVE PROGRAMA TO COMMAND-LINE-BUFFER
                ACCEPT CMD FROM COMMAND-LINE
                CALL "CBL_EXEC_RUN_UNIT" USING COMMAND-LINE-BUFFER
                                         BY VALUE COMMAND-LINE-LEN
                                         BY REFERENCE NEW-THREAD-ID
                                         BY VALUE STACK-SIZE
                                                  FLAGS
                ON   EXCEPTION
                     MOVE 1 TO RC
                END-CALL
                DISPLAY CMD UPON COMMAND-LINE
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWRUNI.
