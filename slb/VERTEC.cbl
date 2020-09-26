       IDENTIFICATION DIVISION.
       PROGRAM-ID.    VERTEC.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/96.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Ler condicionalmente o buffer do teclado      *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
       77  CHR-W                   PIC  9(002) COMP-X.

       LINKAGE SECTION.

       01  CHR.
           05 CHR-1 PIC 9(002) COMP-X.
           05 CHR-2 PIC 9(002) COMP-X.
           05 CHR-3 PIC 9(002) COMP-X.

       PROCEDURE DIVISION USING CHR.

       000-INICIO.

           MOVE 0 TO CHR-1 CHR-2 CHR-3
           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
           IF   KEY-STATUS = 0
                MOVE 048 TO CHR-1
                            CHR-2
                            CHR-3
           ELSE
                MOVE SPACE TO CHR
                CALL "CBL_READ_KBD_CHAR" USING CHR-2
                IF  CHR-2 = 0
                    CALL "CBL_READ_KBD_CHAR" USING CHR-2
                END-IF
                IF CHR-2 (1:1) = X'1B'
                   MOVE X'1B' TO CHR
                END-IF
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM VERTEC.
