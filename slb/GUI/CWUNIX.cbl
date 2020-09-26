       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUNIX.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Informa que o ambiente Windows GUI           *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       LINKAGE SECTION.

       COPY CWUNIX.

       PROCEDURE DIVISION USING PARAMETROS-CWUNIX.

           SET CWUNIX-GUI32 TO TRUE.

       END PROGRAM CWUNIX.
