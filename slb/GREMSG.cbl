       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GREMSG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/10/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Adaptador da vers∆o 3.0 p/CWSEND             *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY CWSEND.

       LINKAGE SECTION.

       01  MSG PIC X(30).

       PROCEDURE DIVISION USING MSG.

           CALL "CWEMSG" USING MSG
           GOBACK.

       END PROGRAM GREMSG.
