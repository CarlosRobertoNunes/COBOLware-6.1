       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGTCP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_GET_CSR_POS Get cursor position   *
                      *                                               *
                      *************************************************

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CRON                      PIC  X(03) VALUE SPACES.
       LINKAGE SECTION.

       01  CURPOS PIC XX.

       PROCEDURE DIVISION USING CURPOS.

           ON 1
              DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
              ACCEPT  CRON   FROM ENVIRONMENT-VALUE
              INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS.
           IF   CRON = 'ON'
                GOBACK
           END-IF
           CALL "CBL_GET_CSR_POS" USING CURPOS
           GOBACK.

       END PROGRAM CWGTCP.
