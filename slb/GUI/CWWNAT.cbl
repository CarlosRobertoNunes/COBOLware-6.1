       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWNAT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_N_ATTR                  *
                      *  Repeat write attribute                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWWNAT-POSITION.
           05 ROW-NUMBER          PIC 9(002) COMP-X.
           05 COLUMN-NUMBER       PIC 9(002) COMP-X.

       01  ATTRIBUTE              PIC X.
       01  FILL-LENGTH            PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING CWWNAT-POSITION
                                ATTRIBUTE
                                FILL-LENGTH.
       000-INICIO.

           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE FILL-LENGTH   TO cwuser-LENGTH-ATTR
           MOVE LOW-VALUES    TO cwuser-ATTRIBUTES
           INSPECT cwuser-ATTRIBUTES (1: FILL-LENGTH)
                   CONVERTING LOW-VALUE TO ATTRIBUTE
           CALL "CWUSER"   USING X"14" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.
       END PROGRAM CWWNAT.
