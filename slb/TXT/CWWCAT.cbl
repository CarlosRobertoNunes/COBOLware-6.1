       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWCAT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_CHATTRS                 *
                      *  Write character &attribute strings           *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CRON                   PIC  X(003) VALUE SPACES.
           05 TEXT-BUFFER            PIC X(2000).
           05 CWACCENT               PIC  X(003) VALUE SPACES.
           05 CWLITS                 PIC  X(003) VALUE SPACES.
           05 LIXO                   PIC X(2000) VALUE ALL X"FF".
           05 LIXO-ATTR              PIC X(2000) VALUE ALL X"00".

       COPY CWUNIX.

       LINKAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  ATTRIBUTE-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING SCREEN-POSITION
                                CHARACTER-BUFFER
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           ON   1
                DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
                ACCEPT CWLITS FROM ENVIRONMENT-VALUE
                INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
                ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
                INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                     CONVERTING MINUSCULAS TO MAIUSCULAS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           MOVE CHARACTER-BUFFER (1: STRING-LENGTH)
             TO TEXT-BUFFER      (1: STRING-LENGTH)

           IF   CWLITS = "LOW"
                INSPECT TEXT-BUFFER (1: STRING-LENGTH)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT TEXT-BUFFER (1: STRING-LENGTH)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT TEXT-BUFFER (1: STRING-LENGTH)
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                IF   CWUNIX-WINDOWS
                     INSPECT TEXT-BUFFER (1: STRING-LENGTH)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-437
                END-IF
           END-IF

           IF   CWUNIX-ON
                CALL "CBL_WRITE_SCR_CHATTRS"
                USING SCREEN-POSITION
                      LIXO (1: STRING-LENGTH)
                      LIXO-ATTR (1: STRING-LENGTH)
                      STRING-LENGTH
                INSPECT TEXT-BUFFER (1: STRING-LENGTH)
                        CONVERTING X"0010111A1B1E1F1819" TO " ><><^v^v"
           END-IF

           CALL "CBL_WRITE_SCR_CHATTRS"
                USING SCREEN-POSITION
                      TEXT-BUFFER      (1: STRING-LENGTH)
                      ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                      STRING-LENGTH.

       000-99-FIM. GOBACK.

       END PROGRAM CWWCAT.
