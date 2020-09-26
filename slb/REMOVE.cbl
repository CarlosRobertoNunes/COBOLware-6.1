      $Set CALLFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    REMOVE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSREMOVE                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILENAME ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  FILE STATUS   IS FS-FILENAME.

       DATA DIVISION.

       FD  FILENAME
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FILENAME.

       01  FILENAME-REG PIC X(1).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                  COMP-X PIC  9(002) VALUE 0.
           05 SAVE-LABEL                PIC  X(050) VALUE SPACES.

       01  ER-FILENAME.
           10 FS-FILENAME               PIC  X(002).
           10 LB-FILENAME               PIC  X(255).

       LINKAGE SECTION.

       01  OLD-NAME     PIC X.
       01  LEN-OLD-NAME PIC 9(008) COMP-X.
       01  F-STATUS     PIC X(02).

       PROCEDURE DIVISION USING OLD-NAME
                                LEN-OLD-NAME
                                F-STATUS.

       000-INICIO.

           MOVE OLD-NAME(1: LEN-OLD-NAME) TO LB-FILENAME
           INSPECT LB-FILENAME CONVERTING LOW-VALUES TO SPACE
           DELETE FILE FILENAME

           IF  (FS-FILENAME NOT = "00")
           AND (FS-FILENAME NOT = X"390D")
               MOVE "30" TO F-STATUS
           ELSE
               MOVE "00" TO F-STATUS
           END-IF.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM REMOVE.

