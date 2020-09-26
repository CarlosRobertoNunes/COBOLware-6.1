       IDENTIFICATION DIVISION.
       PROGRAM-ID.    KBDAVAIL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSKBDAVAIL                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  KEY-STATUS               PIC  9(002) COMP-X VALUE 0.

       LINKAGE SECTION.

       01  KEY-STATUS-MS PIC X(02).

       PROCEDURE DIVISION USING KEY-STATUS-MS.

       000-INICIO.

           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
           IF  KEY-STATUS = 0
               MOVE "30" TO KEY-STATUS-MS
           ELSE
               MOVE "00" TO KEY-STATUS-MS
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM KBDAVAIL.
