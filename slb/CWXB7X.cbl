       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXB7X.
       DATE-WRITTEN.  26/11/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simulador da rotina X"B7" (Tela)             *
                      *  Fun‡ao 0 - Ler Caracteres                    *
                      *         1 - Grava Caracteres                  *
                      *         2 - Ler Atributos                     *
                      *         3 - Grava Atributos                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER           PIC 9(0002) COMP-X.
           05 COLUMN-NUMBER        PIC 9(0002) COMP-X.

       LINKAGE SECTION.

       01  FUNCAO                  PIC 9(0002) COMP-X.
       01  PARAMETRO.
           04 STRING-LENGTH        PIC 9(0004) COMP-X.
           04 POSICAO-TELA         PIC 9(0004) COMP-X.
           04 POSICAO-BUFFER       PIC 9(0004) COMP-X.
       01  BUFFER                  PIC X(2000).

       PROCEDURE DIVISION USING FUNCAO PARAMETRO BUFFER.

       000-INICIO.

           COMPUTE ROW-NUMBER     = POSICAO-TELA / 80
           COMPUTE COLUMN-NUMBER  = POSICAO-TELA - (ROW-NUMBER * 80)

           EVALUATE FUNCAO
               WHEN 0 CALL "CBL_READ_SCR_CHARS"
                           USING SCREEN-POSITION
                                 BUFFER (POSICAO-BUFFER: STRING-LENGTH)
                                 STRING-LENGTH
               WHEN 1 CALL "CBL_WRITE_SCR_CHARS"
                           USING SCREEN-POSITION
                                 BUFFER (POSICAO-BUFFER: STRING-LENGTH)
                                 STRING-LENGTH
               WHEN 2 CALL "CBL_READ_SCR_ATTRS"
                           USING SCREEN-POSITION
                                 BUFFER (POSICAO-BUFFER: STRING-LENGTH)
                                 STRING-LENGTH
               WHEN 3 CALL "CBL_WRITE_SCR_ATTRS"
                           USING SCREEN-POSITION
                                 BUFFER (POSICAO-BUFFER: STRING-LENGTH)
                                 STRING-LENGTH
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWXB7X.
