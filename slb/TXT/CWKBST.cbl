       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWKBST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_GET_KBD_STATUS                    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  BUF PIC 9 VALUE 0.
       LINKAGE SECTION.

       01  KEY-STATUS               PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING KEY-STATUS.

       000-INICIO.

           IF   KEY-STATUS = 255
                MOVE 1 TO BUF
                GOBACK
           END-IF

           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS

           IF  BUF = 1
               IF   KEY-STATUS = 1
                    MOVE 0 TO BUF
               ELSE
                    MOVE 1 TO KEY-STATUS
               END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWKBST.
