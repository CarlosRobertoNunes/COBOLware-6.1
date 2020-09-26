       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXE6X.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/08/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula X"E6"                                 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 SCREEN-POSITION.
              10 ROW-NUMBER         PIC  9(002) COMP-X VALUE 255.
              10 COLUMN-NUMBER      PIC  9(002) COMP-X VALUE 255.

       LINKAGE SECTION.

       01  AD-FUNCTION      COMP PIC 9(02).
       01  AD-POSITION.
           05 AD-LIN        COMP PIC 9(02).
           05 AD-COL        COMP PIC 9(02).

       PROCEDURE DIVISION USING AD-FUNCTION AD-POSITION.

       000-INICIO.

           IF AD-FUNCTION = 27
              MOVE AD-LIN       TO ROW-NUMBER
              MOVE AD-COL       TO COLUMN-NUMBER
              CALL "CWSTCP"  USING SCREEN-POSITION
           ELSE
              CALL "CWGTCP"   USING SCREEN-POSITION
              MOVE ROW-NUMBER    TO AD-LIN
              MOVE COLUMN-NUMBER TO AD-COL
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWXE6X.
