       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGETF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/06/2009.
       SECURITY.      *************************************************
                      * Obtem                                         *
                      * Foco do cursor no uso de um objeto ou tecla   *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 FIELD               PIC  X(030) VALUE SPACES.
           05 SUBSCRIPT           PIC  9(003) VALUE 0.
           05 WS-LENGTH           PIC  X(004) VALUE SPACES.

       LINKAGE SECTION.

       COPY CWGETF.

       PROCEDURE DIVISION USING PARAMETROS-CWGETF.

       000-INICIO.

           IF CWGETF-FUNCTION = "S" OR 's'
              MOVE CWGETF-FIELD      TO FIELD
              MOVE CWGETF-SUBSCRIPT  TO SUBSCRIPT
              MOVE CWGETF-LENGTH(1:) TO WS-LENGTH
           ELSE
              MOVE FIELD     TO CWGETF-FIELD
              MOVE SUBSCRIPT TO CWGETF-SUBSCRIPT
              MOVE WS-LENGTH TO CWGETF-LENGTH(1:)
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWGETF.
