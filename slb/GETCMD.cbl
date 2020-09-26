       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GETCMD.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  30/10/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Obtem conteudo da linha de comando            *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-CMDLIN.
           10 BYTE-CMDLIN OCCURS 255 PIC X.

       LINKAGE SECTION.

       01  LINSIZ    PIC 9(002) COMP-X.
       01  CMDLIN    PIC X(255).

       PROCEDURE DIVISION USING LINSIZ
                                CMDLIN.
       000-INICIO.

           ACCEPT WS-CMDLIN FROM COMMAND-LINE
           PERFORM VARYING LINSIZ FROM 255
                               BY -1
                               UNTIL BYTE-CMDLIN (LINSIZ) NOT = SPACE
                   CONTINUE
           END-PERFORM
           MOVE WS-CMDLIN TO CMDLIN (1: LINSIZ).

       000-99-FIM. GOBACK.
       END PROGRAM GETCMD.
