       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCRTS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Armazene e retorna (Set/Get) CRT STATUS      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TECLA               PIC 9(003) VALUE 0.
           05 KEY-STATUS          PIC X(003) VALUE X"300000".

       LINKAGE SECTION.

       01  FUNCAO PIC X(001).
       01  KS     PIC X(003).

       PROCEDURE DIVISION USING FUNCAO KS.

       000-INICIO.

           IF  KEY-STATUS = X"FFFFFF"
               MOVE KEY-STATUS TO KS
           ELSE
               IF  FUNCAO = "S" or "s"
                   MOVE KS TO KEY-STATUS
               ELSE
                   ACCEPT TECLA FROM ESCAPE KEY
                   MOVE KEY-STATUS TO KS
               END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWCRTS.
