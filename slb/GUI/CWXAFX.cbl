       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXAFX.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/07/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula X"AF"                                 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CARACTER      PIC  X(001) VALUE SPACES.
           05 TECLA-EDIT    PIC  9(003) VALUE 0.
           05 I             PIC  9(003) VALUE 0.
           05 A             PIC  9(003) VALUE 0.

       01  ADIS-FUNCTIONS.
           05 Terminate-accept              PIC X(006) VALUE "00    ".
           05 Terminate-program             PIC X(006) VALUE "01 011".
           05 Carriage-Return               PIC X(006) VALUE "02 013".
           05 Cursor-left                   PIC X(006) VALUE "03 331".
           05 Cursor-right                  PIC X(006) VALUE "04 333".
           05 Cursor-up                     PIC X(006) VALUE "05 327".
           05 Cursor-down                   PIC X(006) VALUE "06 336".
           05 Move-to-start-of-screen       PIC X(006) VALUE "07 327".
           05 Move-to-next-tab-stop         PIC X(006) VALUE "08 009".
           05 Move-to-previous-tab-stop     PIC X(006) VALUE "09 271".
           05 Move-to-end-of-screen         PIC X(006) VALUE "10 335".
           05 Move-to-next-field            PIC X(006) VALUE "11 271".
           05 Move-to-previous-field        PIC X(006) VALUE "12 335".
           05 Change-case-of-character      PIC X(006) VALUE "13 006".
           05 Backspace-character           PIC X(006) VALUE "14 008".
           05 Retype-character              PIC X(006) VALUE "15 025".
           05 Insert-single-character       PIC X(006) VALUE "16 015".
           05 Delete-character              PIC X(006) VALUE "17 339".
           05 Restore-character             PIC X(006) VALUE "18 018".
           05 Clear-to-end-of-field         PIC X(006) VALUE "19 026".
           05 Clear-field                   PIC X(006) VALUE "20 024".
           05 Clear-to-end-of-screen        PIC X(006) VALUE "21    ".
           05 Clear-screen                  PIC X(006) VALUE "22 375".
           05 Set-insert-mode               PIC X(006) VALUE "23 338".
           05 Set-replace-mode              PIC X(006) VALUE "24 338".
           05 Reset-field                   PIC X(006) VALUE "25 001".
           05 Move-to-start-of-field        PIC X(006) VALUE "26    ".
           05 Move-to-mouse-position        PIC X(006) VALUE "27    ".
       01  REDEFINES ADIS-FUNCTIONS.
           05 OCCURS 28.
              10 FUNCAO-ADIS   PIC 9(002).
              10 TECLA-LIGADA  PIC X(001).
              10 TECLA-SP2     PIC 9(003).

       LINKAGE SECTION.

       01  XAF-FUNCTION   PIC 9(2) COMP-X.
       01  XAF-PARAMETERS.
           05  ADIS-KEY-SETTING        PIC 9(02) COMP-X.
           05  FILLER                  PIC X(01).
           05  FIRST-ADIS-KEY          PIC 9(02) COMP-X.
           05  NUMBER-OF-ADIS-KEYS     PIC 9(02) COMP-X.
       01  KEY-STATUS REDEFINES XAF-PARAMETERS.
           10 KEY-TYPE            PIC  X(001).
           10 KEY-CODE-1          PIC  9(002) COMP-X.
           10 KEY-CODE-2          PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING XAF-FUNCTION XAF-PARAMETERS.

       000-INICIO.

           EVALUATE XAF-FUNCTION
               WHEN 1
                    COMPUTE I = FIRST-ADIS-KEY + 1
                    PERFORM NUMBER-OF-ADIS-KEYS TIMES
                            MOVE "E" TO TECLA-LIGADA (I)
                            ADD  1   TO I
                    END-PERFORM
               WHEN 2
                    COMPUTE I = FIRST-ADIS-KEY + 1
                    PERFORM NUMBER-OF-ADIS-KEYS TIMES
                            MOVE SPACE TO TECLA-LIGADA (I)
                            ADD  1     TO I
                    END-PERFORM
               WHEN 26
                    CALL "CWKBDC" USING "0000"  CARACTER TECLA-EDIT
                                                XAF-PARAMETERS (1: 3)
               WHEN OTHER
                    CALL X"AF" USING XAF-FUNCTION XAF-PARAMETERS
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWXAFX.
