       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DSKRES.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Obtem drive corrente                          *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

       01  FS             PIC X(002).

       PROCEDURE DIVISION USING FS.

       000-INICIO.

           CALL "PC_READ_DRIVE" USING FS (1: 1)
           ON EXCEPTION CONTINUE.
           MOVE ":"                TO FS (2: 1).

       000-99-FIM. GOBACK.
       END PROGRAM DSKRES.
