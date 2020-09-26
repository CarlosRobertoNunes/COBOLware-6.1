       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWAST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_ATTRS                   *
                      *  Write attribute string                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWWAST-POSITION.
           05 ROW-NUMBER       PIC  9(002) COMP-X.
           05 COLUMN-NUMBER    PIC  9(002) COMP-X.
       01  ATTRIBUTE-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC  9(004) COMP-X.

       PROCEDURE DIVISION USING CWWAST-POSITION
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.

       000-INICIO.

           INITIALIZE PARAMETROS-CWUSER
           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-ATTR
           MOVE ATTRIBUTE-BUFFER  (1: STRING-LENGTH)
             TO cwuser-ATTRIBUTES (1: STRING-LENGTH)
           CALL "CWUSER"   USING X"14" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.
       END PROGRAM CWWAST.
