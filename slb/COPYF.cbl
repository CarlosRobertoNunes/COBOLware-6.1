       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COPYF.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Copiar arquivo                                *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

       01  FS              PIC X(002).
       01  NOME-ANTIGO     PIC X(050).
       01  NOME-NOVO       PIC X(050).

       PROCEDURE DIVISION USING FS NOME-ANTIGO NOME-NOVO.

       000-INICIO.

            CALL "CBL_COPY_FILE" USING NOME-ANTIGO
                                       NOME-NOVO
                             RETURNING RETURN-CODE
            IF RETURN-CODE NOT = 0
               CALL "FS_COPY_FILE" USING NOME-ANTIGO
                                          NOME-NOVO
                                RETURNING RETURN-CODE
               ON EXCEPTION CONTINUE END-CALL
            END-IF

            EVALUATE RETURN-CODE
                WHEN 00000 MOVE "00" TO FS
                WHEN 03385 MOVE "30" TO FS
                WHEN OTHER MOVE "40" TO FS
            END-EVALUATE.

       000-99-FIM. GOBACK.
       END PROGRAM COPYF.
