       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCURS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      * Armazena ou retorna posicao do cursor SET/GET *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01   CURPOS.
            10 CURPOS-LIN           PIC  9(002) VALUE 1.
            10 CURPOS-COL           PIC  9(002) VALUE 1.

       LINKAGE SECTION.

       01  FUNCAO                   PIC  X(001).
       01  CURPOS-LK                PIC  X(004).

       PROCEDURE DIVISION USING FUNCAO CURPOS-LK.

       000-INICIO.

           IF   FUNCAO = "G" OR "g"
                MOVE CURPOS    TO CURPOS-LK
           ELSE
                MOVE CURPOS-LK TO CURPOS
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CWCURS.
