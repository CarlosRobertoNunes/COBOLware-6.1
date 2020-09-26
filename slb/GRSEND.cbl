       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRSEND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibilza chmada GRSEND da vers∆o 3       *
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

       COPY CWSEND.

       LINKAGE SECTION.

       01  PARAMETROS-GRSEND.
           05 GRSEND-MSG                     PIC  X(060)  VALUE SPACES.
           05 GRSEND-SCREENS                              VALUE SPACES.
              10 GRSEND-CHAR   OCCURS 8      PIC  X(001).
              10 GRSEND-SCREEN OCCURS 8      PIC  X(010).
           05 GRSEND-OPTION                  PIC  9(001)  VALUE 0.
           05 GRSEND-OPTION-CHAR             PIC  X(001)  VALUE SPACE.

       PROCEDURE DIVISION USING PARAMETROS-GRSEND.

           MOVE GRSEND-MSG         TO CWSEND-MSG
           MOVE GRSEND-SCREENS     TO CWSEND-SCREENS
           MOVE GRSEND-OPTION      TO CWSEND-OPTION
           MOVE GRSEND-OPTION-CHAR TO CWSEND-OPTION-CHAR
           CALL "CWSEND" USING PARAMETROS-CWSEND
           MOVE CWSEND-OPTION      TO GRSEND-OPTION
           MOVE CWSEND-OPTION-CHAR TO GRSEND-OPTION-CHAR.
