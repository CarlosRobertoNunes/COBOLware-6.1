       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGTCP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_GET_CSR_POS Get cursor position   *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01   CURPOS.
            10 CURPOS-LIN           PIC  9(002) VALUE ZERO.
            10 CURPOS-COL           PIC  9(002) VALUE ZERO.

       LINKAGE SECTION.

       01  CURSOR-POSITION.
           05 ROW-NUMBER            PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-NUMBER         PIC  9(002) COMP-X VALUE 0.

       PROCEDURE DIVISION USING CURSOR-POSITION.

           CALL "CWCURS"  USING "G" CURPOS
           COMPUTE COLUMN-NUMBER = CURPOS-COL - 1
           COMPUTE ROW-NUMBER    = CURPOS-LIN - 1
           GOBACK.

       END PROGRAM CWGTCP.
