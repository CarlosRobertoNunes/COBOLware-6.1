       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRAST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_SCR_ATTRS                    *
                      *  Read attribute string                        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  AREAS-DE-TRABALHO.
           05 CRON                   PIC  X(003) VALUE SPACES.
           05 MINUSCULAS             PIC  X(049) VALUE
              "abcdefghijklmnopqrstuvwxyz†Ç°¢£ÏÖäçïóÑâîÅÉàåìñ∆‰á".
           05 MAIUSCULAS             PIC  X(049) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZµê÷‡ÈÌ∑‘Î„Îé”ôö∂“◊‚Í«ÂÄ".
       LINKAGE SECTION.

       01 SCREEN-POSITION.
          05 ROW-NUMBER       PIC  9(002) COMP-X.
          05 COLUMN-NUMBER    PIC  9(002) COMP-X.
       01 ATTRIBUTE-BUFFER    PIC X(2000).
       01 STRING-LENGTH       PIC  9(004) COMP-X.

       PROCEDURE DIVISION USING SCREEN-POSITION
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.

       000-INICIO.

           ON   1
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   CRON = 'ON'
                GOBACK
           END-IF

            CALL "CBL_READ_SCR_ATTRS"
                 USING SCREEN-POSITION
                       ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                       STRING-LENGTH.

       000-99-FIM. GOBACK.
       END PROGRAM CWRAST.
