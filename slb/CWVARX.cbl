      $SET NOMS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWVARX.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/03/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Extrai conte£do de vari vel de ambiente para *
                      *  a propria vari vel fornecida como parƒmetro  *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       LINKAGE SECTION.

       01  VARIAVEL PIC X(255).
       01  TAMANHO  PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING VARIAVEL TAMANHO.

       000-INICIO.

           IF   VARIAVEL (1: 1) = "$"
                DISPLAY VARIAVEL(2: TAMANHO - 1) UPON ENVIRONMENT-NAME
                MOVE SPACES TO VARIAVEL(1: TAMANHO)
                ACCEPT VARIAVEL(1: TAMANHO) FROM ENVIRONMENT-VALUE
           END-IF

           INSPECT VARIAVEL(1: TAMANHO) CONVERTING X"98" TO X"20".

       000-99-FIM. GOBACK.
       END PROGRAM CWVARX.
