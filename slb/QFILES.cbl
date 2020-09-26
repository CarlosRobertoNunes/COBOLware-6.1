      $SET CALLFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    QFILES.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/02/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica lista de arquivos abertos.          *
                      *                                               *
                      *************************************************

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 FILENAMES           PIC X(050) VALUE "$TEMP/QFILES.###".

       PROCEDURE DIVISION.

       000-INICIO.

           CALL "CWSQLC" USING "?" FILENAMES.
           EXEC COBOLware Help
                File FILENAMES
           END-EXEC.

       000-99-FIM. GOBACK.

       END PROGRAM QFILES.
