       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRCAS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_SCR_CHARS                    *
                      *  Read character string                        *
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
           05 ACENTOS-850       PIC  X(034) VALUE
              "ê†Ç°¢£ÏÉàåìñÖäçïó«Â•∆‰§éôöÑâãîÅòÄá".
           05 ACENTOS-437       PIC  X(034) VALUE
              "ê†Ç°¢£yÉàåìñÖäçïóéô•Ñî§éôöÑâãîÅòÄá".
       COPY CWUNIX.

       LINKAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING SCREEN-POSITION
                                CHARACTER-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           ON   1
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           CALL "CBL_READ_SCR_CHARS"
                 USING SCREEN-POSITION
                       CHARACTER-BUFFER (1: STRING-LENGTH)
                       STRING-LENGTH

           IF   CWUNIX-WINDOWS
                INSPECT CHARACTER-BUFFER (1: STRING-LENGTH)
                        CONVERTING ACENTOS-437
                                TO ACENTOS-850
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWRCAS.
