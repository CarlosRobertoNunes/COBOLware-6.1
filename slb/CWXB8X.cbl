       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXB8X.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/07/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula rotina X"B8" Le ou escreve tela       *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  STRING-LENGTH       PIC 9(004) COMP-X.

       LINKAGE SECTION.

       01  FUNCAO                     PIC 9(02) BINARY.

       01  POSICOES.
           05 TAMANHO                 PIC 9(04) BINARY.
           05 INT                     PIC 9(04) BINARY.
           05 INB                     PIC 9(04) BINARY.

       01  CARACTERES                 PIC X(2000).
       01  ATRIBUTOS                  PIC X(2000).

       01  FUNCAO-C                   PIC 9(02) COMP.

       01  POSICOES-C.
           05 TAMANHO-C               PIC 9(04) COMP.
           05 INT-C                   PIC 9(04) COMP.
           05 INB-C                   PIC 9(04) COMP.

       PROCEDURE DIVISION USING FUNCAO POSICOES CARACTERES ATRIBUTOS.

       000-INICIO.


           IF  FUNCAO > 1
               SET ADDRESS FUNCAO-C   TO ADDRESS FUNCAO
               SET ADDRESS POSICOES-C TO ADDRESS POSICOES
               MOVE TAMANHO-C TO STRING-LENGTH
               COMPUTE ROW-NUMBER     = INT-C - 1
               COMPUTE COLUMN-NUMBER  = INB-C - 1
               IF  FUNCAO-C = 0
                   CALL "CBL_READ_SCR_CHATTRS"
                        USING SCREEN-POSITION
                              CARACTERES(1:TAMANHO)
                              ATRIBUTOS(1:TAMANHO)
                              STRING-LENGTH
               ELSE
                   CALL "CBL_WRITE_SCR_CHATTRS"
                        USING SCREEN-POSITION
                              CARACTERES(1:TAMANHO)
                              ATRIBUTOS(1:TAMANHO)
                              STRING-LENGTH
               END-IF
           ELSE
               MOVE TAMANHO TO STRING-LENGTH
               COMPUTE ROW-NUMBER     = INT - 1
               COMPUTE COLUMN-NUMBER  = INB - 1
               IF  FUNCAO = 0
                   CALL "CBL_READ_SCR_CHATTRS"
                        USING SCREEN-POSITION
                              CARACTERES(1:TAMANHO)
                              ATRIBUTOS(1:TAMANHO)
                              STRING-LENGTH
               ELSE
                   CALL "CBL_WRITE_SCR_CHATTRS"
                        USING SCREEN-POSITION
                              CARACTERES(1:TAMANHO)
                              ATRIBUTOS(1:TAMANHO)
                              STRING-LENGTH
               END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWXB8X.
