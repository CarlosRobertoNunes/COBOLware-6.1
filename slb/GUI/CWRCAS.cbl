       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRCAS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_SCR_CHARS                    *
                      *  Read character string                        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWRCAS-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING CWRCAS-POSITION
                                CHARACTER-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           INITIALIZE PARAMETROS-CWUSER
           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-CHAR
           CALL "CWUSER" USING X"13" PARAMETROS-CWUSER
           MOVE cwuser-CHARACTERS (1: STRING-LENGTH)
             TO CHARACTER-BUFFER  (1: STRING-LENGTH).

       000-99-FIM. GOBACK.

       END PROGRAM CWRCAS.
