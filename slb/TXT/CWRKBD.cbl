       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRKBD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_KBD_CHAR Modo Texto          *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CRON                   PIC  X(003) VALUE SPACES.

       LINKAGE SECTION.

       01   CHAR PIC X.

       PROCEDURE DIVISION USING CHAR.

       000-INICIO.

           ON   1
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           CALL "CBL_READ_KBD_CHAR" USING CHAR.

       000-99-FIM. GOBACK.
       END PROGRAM CWRKBD.
