       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSTCP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_SET_CSR_POS                       *
                      *  Set cursor position                          *
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

           COMPUTE CURPOS-COL = COLUMN-NUMBER + 1
           COMPUTE CURPOS-LIN = ROW-NUMBER    + 1
           CALL "CWCURS"  USING "S" CURPOS
           GOBACK.

       END PROGRAM CWSTCP.
