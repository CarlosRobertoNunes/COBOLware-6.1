       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGETK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  24/10/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Leitura de teclado                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CURPOS.
              10 CURPOS-LIN PIC  9(002).
              10 CURPOS-COL PIC  9(002).

       LINKAGE SECTION.

       COPY CWGETK.

       PROCEDURE DIVISION USING PARAMETROS-CWGETK.

       000-INICIO.

            MOVE CWGETK-LINE     TO CURPOS-LIN
            MOVE CWGETK-COLUMN   TO CURPOS-COL
            MOVE 0               TO CWGETK-LINE
                                    CURPOS-COL
            CALL "CWKBDC" USING CURPOS CWGETK-CHARACTER CWGETK-KEY.

       000-99-FIM. GOBACK.

       END PROGRAM CWGETK.
