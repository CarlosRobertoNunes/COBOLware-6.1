       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBINF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava e le arquivo binario                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BMPWORK ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS.

       DATA DIVISION.
       FILE SECTION.

       FD  BMPWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BMPWORK.

       01  BMPWORK-REG               PIC X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LB-BMPWORK         PIC  X(255) VALUE SPACES.
           05 I           COMP-5 PIC S9(004) VALUE 0.

       LINKAGE SECTION.

       01  FUNCAO PIC X(001).
       01  FS     PIC X(002).
       01  BUFFER PIC X(001).

       PROCEDURE DIVISION USING FUNCAO FS BUFFER.

       000-INICIO.

           EVALUATE FUNCAO
               WHEN "I"
                    PERFORM 100-MONTA-LABEL THRU 100-99-FIM
                    OPEN INPUT BMPWORK
               WHEN "D"
                    DELETE FILE BMPWORK
               WHEN "O"
                    PERFORM 100-MONTA-LABEL THRU 100-99-FIM
                    OPEN OUTPUT BMPWORK
               WHEN "C"
                    CLOSE BMPWORK
               WHEN "R"
                    READ BMPWORK INTO BUFFER
               WHEN "W"
                    WRITE BMPWORK-REG FROM BUFFER
           END-EVALUATE.

       000-99-FIM. GOBACK.

       100-MONTA-LABEL.

           PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > LENGTH LB-BMPWORK
                          OR BUFFER(I:1) = SPACE OR LOW-VALUE
                   CONTINUE
           END-PERFORM
           MOVE BUFFER (1: I - 1) TO LB-BMPWORK.

       100-99-FIM. EXIT.
       END PROGRAM CWBINF.
