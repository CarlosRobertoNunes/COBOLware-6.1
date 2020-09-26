       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FRAME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/06/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exibi‡Æo de tela padrÆo alternativa.         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  I        PIC 99    VALUE 0.
       77  C        PIC 99    VALUE 0.
       77  empresa  PIC X(30) VALUE SPACES.
       77  sistema  PIC X(30) VALUE SPACES.
       77  programa PIC X(08) VALUE SPACES.
       77  usuario  PIC X(30) VALUE SPACES.
       77  tracos   PIC X(80) VALUE ALL X'C4'.
       77  espacos  PIC X(80) VALUE spaces.
       77  curdir   PIC X(50) VALUE spaces.

       LINKAGE SECTION.

       01  OPTION PIC X.
       01  PARAMETRO PIC X(80).
       01  REDEFINES PARAMETRO.
           05 DESCRICAO-OPCAO PIC X(34).
       01  REDEFINES PARAMETRO.
           05 RODAPE          PIC X(68).
       01  REDEFINES PARAMETRO.
           05 OBS             PIC X(29).
       01  PARAMETRO2 PIC X(80).

       PROCEDURE DIVISION USING OPTION PARAMETRO PARAMETRO2.

       000-INICIO.

           EVALUATE OPTION
               WHEN 'f' *> Exibe observa‡Æo de fun‡Æo de cadastro
                    DISPLAY (4, 1) OBS
               WHEN 'F' *> Exibe tela padrÆo e moldura
                    EXEC COBOLware GetSystem
                     SCREEN-APLICATION;sistema
                     SCREEN-COMPANY;empresa
                     PROGRAM;programa
                     USER;usuario
                    END-EXEC
                    DISPLAY (1, 1) ERASE
                    DISPLAY (1, 1) empresa
                    DISPLAY (2, 1) sistema
                    DISPLAY (3, 1) tracos
                    DISPLAY (23, 1) tracos
                    MOVE 80 TO C
                    EXEC COBOLware Pack String usuario WIDTH I END-EXEC
                    compute C = 80 - I + 1
                    DISPLAY (1, C) usuario with size I
                    MOVE 80 TO C
                    EXEC COBOLware Pack String programa WIDTH I END-EXEC
                    compute C = 80 - I + 1
                    DISPLAY (2, C) programa with size I
               WHEN 'E' *> Limpa area de trabalho da tela
                    PERFORM VARYING I FROM 4 BY 1
                            UNTIL I > 25
                            IF I <> 23
                               DISPLAY (I, 1) ESPACOS
                            END-IF
                    END-PERFORM
               WHEN 'O' *> Exibe Titulo da op‡Æo selecionada
                    DISPLAY (2, 33) DESCRICAO-OPCAO
               WHEN 'M' *> Apaga linha de mensagem
                    DISPLAY (24, 1) ESPACOS
               WHEN 'P' *> Nome do programa
                    DISPLAY (25, 1) ESPACOS WITH SIZE 79
                    move spaces to programa
                    display (2, 73) programa
                    move rodape (1:8) TO PROGRAMA
                    EXEC COBOLware Pack String programa WIDTH I END-EXEC
                    compute C = 80 - I + 1
                    DISPLAY (2, C) programa with size I
               WHEN 'I' *> Atualiza empresa e sistema
                    move rodape(1:30) to empresa
                    move parametro2(1:30) to sistema
                    DISPLAY (1, 1) empresa
                    DISPLAY (2, 1) sistema
               WHEN 'N' *> Atualiza usuario e pasta corrente
                    move spaces to usuario
                    display (1, 51) usuario
                    move rodape(1:29) to usuario
                    MOVE 80 TO C
                    EXEC COBOLware Pack String usuario WIDTH I END-EXEC
                    compute C = 80 - I + 1
                    DISPLAY (1, C) usuario with size I
                    move parametro2(1:50) to curdir
      *             DISPLAY (x, y) curdir
               WHEN 'R' *> Exibe rodape
                    DISPLAY (25, 3) rodape
           END-EVALUATE.

       000-99-FIM. GOBACK.
       END PROGRAM FRAME.
