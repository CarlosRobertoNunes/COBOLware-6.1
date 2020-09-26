       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRMODE.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  02/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Redirecionar arquivo ASSIGNed TO PRINTER      *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 CMDLINE                  PIC X(023) VALUE
              "PRN2FILE COBSPn.LST>nul".
           05 PARAMETRO.
              10 SIZE-BINARIO          PIC 9(002) COMP-X VALUE 0.

       LINKAGE SECTION.

       01  MODO PIC X.

       PROCEDURE DIVISION USING MODO.

       000-INICIO.

           IF   MODO = "P" OR "p"
                CALL "CWSQLC" USING "P"
           ELSE
                CALL "CWSQLC" USING "S"
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM PRMODE.
