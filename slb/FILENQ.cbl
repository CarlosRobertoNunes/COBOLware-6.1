      $SET CALLFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FILENQ.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Verificar existencia de arquivo               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILENAME ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  FILE STATUS   IS FS-FILENAME.

       DATA DIVISION.
       FILE SECTION.

       FD  FILENAME
           VALUE OF FILE-ID IS LB-FILENAME.

       01  FILENAME-REG PIC X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ER-FILENAME.
              10 FS-FILENAME PIC X(02) VALUE "00".
              10 LB-FILENAME PIC X(50) VALUE "00".

       LINKAGE SECTION.

       01  FS              PIC X(002).
       01  NOME-DE-ARQUIVO PIC X(050).

       PROCEDURE DIVISION USING FS NOME-DE-ARQUIVO.

       000-INICIO.

           MOVE NOME-DE-ARQUIVO TO LB-FILENAME
           OPEN INPUT FILENAME
           MOVE FS-FILENAME     TO FS
           CLOSE FILENAME.

       000-99-FIM. GOBACK.

       END PROGRAM FILENQ.
