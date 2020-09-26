       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWNCH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_N_CHAR                  *
                      *  Repeat write character                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWWNCH-POSITION.
           05 ROW-NUMBER          PIC 9(002) COMP-X.
           05 COLUMN-NUMBER       PIC 9(002) COMP-X.

       01  CHARACTERx             PIC X.
       01  FILL-LENGTH            PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING CWWNCH-POSITION
                                CHARACTERx
                                FILL-LENGTH.
       000-INICIO.

           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE FILL-LENGTH   TO cwuser-LENGTH-CHAR
           MOVE LOW-VALUES    TO cwuser-ATTRIBUTES
           INSPECT cwuser-CHARACTERS (1: cwuser-LENGTH-CHAR)
                   CONVERTING LOW-VALUE TO CHARACTERx
           CALL "CWUSER"   USING X"14" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.
       END PROGRAM CWWNCH.
