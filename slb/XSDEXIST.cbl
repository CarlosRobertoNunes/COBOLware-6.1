      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDEXIST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Verifica existencia de arquivo               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       FILE-CONTROL.

            SELECT OPTIONAL FileName ASSIGN TO DISK
                   FILE STATUS GLB-STATUS
                   ORGANIZATION RELATIVE.

       DATA DIVISION.
       FILE SECTION.

       FD  FileName
           VALUE OF FILE-ID IS GLB-TITLE.

       01  FileName-REG PIC X.

       LINKAGE SECTION.

       01  GLB-TITLE         PIC X(060).
       01  GLB-STATUS        PIC X(002).

       PROCEDURE DIVISION USING GLB-STATUS
                                GLB-STATUS.

           OPEN INPUT FileName
           IF  GLB-STATUS < '10'
               CLOSE FileName
           END-IF
           GOBACK

       END PROGRAM XSDEXIST.
