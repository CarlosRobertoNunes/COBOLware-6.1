       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWCST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_CHARS                   *
                      *  Write character string                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01 SCWWCST-POSITION.
          05 ROW-NUMBER       PIC  9(002) COMP-X.
          05 COLUMN-NUMBER    PIC  9(002) COMP-X.
       01 CHARACTER-BUFFER    PIC X(2000).
       01 STRING-LENGTH       PIC  9(004) COMP-X.

       PROCEDURE DIVISION USING SCWWCST-POSITION
                                CHARACTER-BUFFER
                                STRING-LENGTH.

       000-INICIO.

           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-CHAR
           MOVE CHARACTER-BUFFER  (1: STRING-LENGTH)
             TO cwuser-CHARACTERS (1: STRING-LENGTH)
           CALL "CWUSER" USING X"14" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.
       END PROGRAM CWWCST.
