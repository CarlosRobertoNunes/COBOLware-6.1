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

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CRON                   PIC  X(003) VALUE SPACES.

       LINKAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER          PIC 9(002) COMP-X.
           05 COLUMN-NUMBER       PIC 9(002) COMP-X.

       01  ATTRIBUTE              PIC X.
       01  FILL-LENGTH            PIC 9(004) COMP-X.


       PROCEDURE DIVISION USING SCREEN-POSITION
                                ATTRIBUTE
                                FILL-LENGTH.
       000-INICIO.

           ON   1
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                             ATTRIBUTE
                                             FILL-LENGTH.

       000-99-FIM. GOBACK.
       END PROGRAM CWWNAT.
