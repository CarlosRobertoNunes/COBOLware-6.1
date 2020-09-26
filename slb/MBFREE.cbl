      $Set CallFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MBFREE.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * liberar registros bloqueados                  *
                      *                                               *
                      *************************************************

       PROCEDURE DIVISION.

       000-INICIO.

            COMMIT.

       000-99-FIM. GOBACK.
       END PROGRAM MBFREE.
