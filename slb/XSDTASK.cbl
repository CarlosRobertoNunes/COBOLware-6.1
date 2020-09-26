      $Set CALLFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDTASK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Fornece numero da task para reports          *
                      *                                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT XSEED ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS XSEED-CHAVE
                  FILE STATUS IS FS-XSEED.

       DATA DIVISION.
       FILE SECTION.

       FD   XSEED
            LABEL RECORD IS STANDARD.

       01   XSEED-REG.
            05 XSEED-CHAVE PIC X(001).
            05 XSEED-TASK  PIC 9(006).
            05 XSEED-JOBS  PIC 9(006).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 VEZ       PIC 9(002) VALUE 1.
           05 ER-XSEED.
              10 FS-XSEED PIC X(002) VALUE "00".
              10 LB-XSEED PIC X(050) VALUE "xsdtask".
           05 TASKW     PIC 9(6) VALUE 0.

       LINKAGE SECTION.

       01 TASK PIC 9(06).

       PROCEDURE DIVISION USING TASK.

       000-INICIO.

           IF   TASK NUMERIC
           AND  TASK > 0
           AND  TASKW = 0
                MOVE TASK TO TASKW
                GOBACK
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
                OPEN I-O XSEED
                IF FS-XSEED = '39'
                   DELETE FILE XSEED
                   OPEN I-O XSEED
                END-IF
                IF  FS-XSEED > '09'
                    CALL 'CWISAM' USING ER-XSEED
                END-IF
                MOVE "T" TO XSEED-CHAVE
                READ XSEED WAIT
                     INVALID KEY
                     MOVE 0    TO XSEED-TASK
                     MOVE 0    TO XSEED-JOBS
                     MOVE "T"  TO XSEED-CHAVE
                     WRITE XSEED-REG
                     IF  FS-XSEED > '09'
                         CALL 'CWISAM' USING ER-XSEED
                     END-IF
                END-READ
                ADD 1 TO XSEED-TASK
                MOVE XSEED-TASK TO TASK
                                    TASKW
                REWRITE XSEED-REG
                CLOSE XSEED
           ELSE
                MOVE TASKW TO TASK
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM XSDTASK.
