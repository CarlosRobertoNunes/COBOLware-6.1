       IDENTIFICATION DIVISION.
       PROGRAM-ID.    KILL.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Eliminar arquivo                              *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  FILE-NAME PIC X(050) VALUE SPACES.

       LINKAGE SECTION.

       01  FS              PIC X(002).
       01  NOME-DE-ARQUIVO PIC X(050).

       PROCEDURE DIVISION USING FS NOME-DE-ARQUIVO.

       000-INICIO.

            MOVE NOME-DE-ARQUIVO TO FILE-NAME
            CALL "CBL_DELETE_FILE" USING FILE-NAME
                               RETURNING RETURN-CODE
            IF RETURN-CODE NOT = 0
               CALL "FS_DELETE_FILE" USING FILE-NAME
                                  RETURNING RETURN-CODE
               ON EXCEPTION CONTINUE END-CALL
            END-IF

            EVALUATE RETURN-CODE
                WHEN 00000      MOVE "00" TO FS
                WHEN 03385      MOVE "30" TO FS
                WHEN 0221839360 MOVE "30" TO FS
                WHEN OTHER      MOVE "40" TO FS
            END-EVALUATE.

       000-99-FIM. GOBACK.
       END PROGRAM KILL.
