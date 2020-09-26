       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUNIX.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica se o ambiente ‚ Unix                *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.

       77  VEZ PIC 9(001) VALUE 0.
       77  DOS PIC X(005) VALUE SPACES.
       77  PWD PIC X(005) VALUE SPACES.
       77  SP2 PIC X(005) VALUE SPACES.

       01  OS-INFORMATION.
           05 OS-PARAMETER-SIZE     PIC XX COMP-X VALUE 13.
           05 OS-TYPE               PIC 9(2) COMP-X.
           05 OS-VERSION            PIC XXXX COMP-X.
           05 OS-DBCS-SUPPORT       PIC X COMP-X.
           05 OS-CHAR-CODING        PIC 9(2) COMP-X.
           05 OS-COUNTRY-CODE       PIC XX COMP-X.
           05 OS-CODE-PAGE          PIC XX COMP-X.

       LINKAGE SECTION.

       COPY CWUNIX.

       PROCEDURE DIVISION USING PARAMETROS-CWUNIX.

       000-INICIO.

           IF   DOS = SPACES
                DISPLAY "PWD" UPON ENVIRONMENT-NAME
                ACCEPT  PWD   FROM ENVIRONMENT-VALUE
                DISPLAY "CWUNIX" UPON ENVIRONMENT-NAME
                ACCEPT  DOS      FROM ENVIRONMENT-VALUE
                DISPLAY "SP2END" UPON ENVIRONMENT-NAME
                ACCEPT  SP2      FROM ENVIRONMENT-VALUE
                CALL "CBL_GET_OS_INFO" USING OS-INFORMATION.

           IF PWD(1:1) = '/'
              SET CWUNIX-ON  TO TRUE
           ELSE
              IF   OS-TYPE < 3
              OR  (OS-TYPE = 5 OR 131)
                   SET CWUNIX-OFF TO TRUE
                   IF  OS-TYPE = 131
                       SET CWUNIX-WIN32 TO TRUE
                       IF   DOS = "DOS32"
                            SET CWUNIX-DOS32 TO TRUE
                       END-IF
                       IF   DOS = "WIN32"
                            SET CWUNIX-WIN32 TO TRUE
                       END-IF
                       IF  SP2 = "6"
                           SET CWUNIX-GUI32 TO TRUE
                       END-IF
                   ELSE
                       IF  OS-TYPE = 5
                           SET CWUNIX-WIN16 TO TRUE
                           IF  SP2 = "6"
                               SET CWUNIX-GUI16 TO TRUE
                           END-IF
                       END-IF
                       IF  OS-TYPE = 1 OR 2
                           SET CWUNIX-DOS16 TO TRUE
                       END-IF
                   END-IF
              ELSE
                   SET CWUNIX-ON  TO TRUE
              END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWUNIX.
