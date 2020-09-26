       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRCAT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_SCR_CHATTRS                  *
                      *  Read character & attribute strings           *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWRCAT-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  ATTRIBUTE-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING CWRCAT-POSITION
                                CHARACTER-BUFFER
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           INITIALIZE PARAMETROS-CWUSER
           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-CHAR
                                 cwuser-LENGTH-ATTR
           CALL "CWUSER" USING X"13" PARAMETROS-CWUSER
           IF  STRING-LENGTH = 1999
               MOVE cwuser-CHARACTERS (1: 2000)
                 TO CHARACTER-BUFFER  (1: 2000)
               MOVE cwuser-ATTRIBUTES (1: 2000)
                 TO ATTRIBUTE-BUFFER  (1: 2000)
           ELSE
               MOVE cwuser-CHARACTERS (1: STRING-LENGTH)
                 TO CHARACTER-BUFFER  (1: STRING-LENGTH)
               MOVE cwuser-ATTRIBUTES (1: STRING-LENGTH)
                 TO ATTRIBUTE-BUFFER  (1: STRING-LENGTH)
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWRCAT.
