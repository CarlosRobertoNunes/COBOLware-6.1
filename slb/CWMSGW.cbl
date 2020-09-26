       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMSGW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  24/02/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *   Exibe caracteres no video preservando os    *
                      *   atributos pre-existentes                    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC  X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.
           05 TXT-LENGTH              PIC  9(008) COMP-X VALUE 0.

       LINKAGE SECTION.

       01   CWMSGW-POSIT.
            05 CWMSGW-LINE            PIC  9(002).
            05 CWMSGW-COLUMN          PIC  9(002).
            05 CWMSGW-LENGTH          PIC  9(002).

       01   CWMSGW-STRING             PIC  X(2000).

       01   CWMSGW-LENGTH2            PIC  9(008) COMP-X.

       PROCEDURE DIVISION USING CWMSGW-POSIT CWMSGW-STRING
                                CWMSGW-LENGTH2.

       000-INICIO.

           COMPUTE ROW-NUMBER    = CWMSGW-LINE     - 1
           COMPUTE COLUMN-NUMBER = CWMSGW-COLUMN   - 1
           IF   CWMSGW-LENGTH NOT NUMERIC
           OR   CWMSGW-LENGTH = "00"
                MOVE CWMSGW-LENGTH2 TO STRING-LENGTH TXT-LENGTH
           ELSE
                MOVE CWMSGW-LENGTH  TO STRING-LENGTH TXT-LENGTH
           END-IF
           MOVE CWMSGW-STRING (1: STRING-LENGTH) TO CARACTER-BUFFER
*******    CALL "CWTEXT" USING CARACTER-BUFFER TXT-LENGTH
           INSPECT CARACTER-BUFFER CONVERTING X"5F" TO SPACE
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                            CARACTER-BUFFER
                                            STRING-LENGTH.

       000-99-FIM. GOBACK.

       END PROGRAM CWMSGW.

