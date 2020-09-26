       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWCAS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_CHARS_ATTR              *
                      *  Write character string with attribute        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWWCAS-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.
       01  ATTRIBUTE           PIC X(001).

       PROCEDURE DIVISION USING CWWCAS-POSITION
                                CHARACTER-BUFFER
                                STRING-LENGTH
                                ATTRIBUTE.
       000-INICIO.

           INITIALIZE PARAMETROS-CWUSER
           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-CHAR
                                 cwuser-LENGTH-ATTR
           MOVE CHARACTER-BUFFER  (1: STRING-LENGTH)
             TO cwuser-CHARACTERS (1: STRING-LENGTH)
           INSPECT cwuser-ATTRIBUTES (1: STRING-LENGTH)
                   CONVERTING LOW-VALUE TO ATTRIBUTE
           CALL "CWUSER"   USING X"14" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.

       END PROGRAM CWWCAS.
