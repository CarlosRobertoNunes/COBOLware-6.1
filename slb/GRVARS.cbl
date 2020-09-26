       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRVARS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/10/2000.
       SECURITY.      *************************************************
                      *                                               *
                      * Retorna conteudo de variavel de ambiente      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

       01  PARAMETROS-GRVARS.
           05 GRVARS-NOME                    PIC  X(012).
           05 GRVARS-CONTEUDO                PIC  X(080).

       PROCEDURE DIVISION USING PARAMETROS-GRVARS.

       000-INICIO.

           IF   GRVARS-NOME = SPACES
                MOVE SPACES TO GRVARS-CONTEUDO
           ELSE
                DISPLAY GRVARS-NOME     UPON ENVIRONMENT-NAME
                ACCEPT  GRVARS-CONTEUDO FROM ENVIRONMENT-VALUE
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM GRVARS.
