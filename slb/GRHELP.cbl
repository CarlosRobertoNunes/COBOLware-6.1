       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRHELP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibiliza chamada GRHELP da vers∆o 3     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       COPY CWHELP.

       LINKAGE SECTION.

       01  PARAMETROS-GRHELP.
           05 GRHELP-FILE                    PIC  X(050) VALUE SPACES.
           05 GRHELP-LINE                    PIC  9(002) VALUE 1.
           05 GRHELP-COLUMN                  PIC  9(002) VALUE 1.
           05 GRHELP-TYPE                    PIC  9(001) VALUE 0.
           05 GRHELP-VERTICAL-LENGTH         PIC  9(002) VALUE 22.
           05 GRHELP-HORIZONTAL-LENGTH       PIC  9(002) VALUE 78.
           05 GRHELP-COLOR-SCREEN     COMP-X PIC  9(002) VALUE 078.
           05 GRHELP-COLOR-FRAME      COMP-X PIC  9(002) VALUE 078.
           05 GRHELP-COLOR-SHADE      COMP-X PIC  9(002) VALUE 008.

       PROCEDURE DIVISION USING PARAMETROS-GRHELP.

           MOVE GRHELP-FILE              TO CWHELP-FILE
           MOVE GRHELP-LINE              TO CWHELP-LINE
           MOVE GRHELP-COLUMN            TO CWHELP-COLUMN
           MOVE GRHELP-TYPE              TO CWHELP-TYPE
           MOVE GRHELP-VERTICAL-LENGTH   TO CWHELP-VERTICAL-LENGTH
           MOVE GRHELP-HORIZONTAL-LENGTH TO CWHELP-HORIZONTAL-LENGTH
           MOVE GRHELP-COLOR-SCREEN      TO CWHELP-COLOR-FRAME
           MOVE GRHELP-COLOR-FRAME       TO CWHELP-COLOR-BORDER
           MOVE GRHELP-COLOR-SHADE       TO CWHELP-COLOR-SHADE
           CALL "CWHELP" USING PARAMETROS-CWHELP.

