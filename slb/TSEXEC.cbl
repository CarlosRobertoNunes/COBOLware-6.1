     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID.    TSEXEC.
     3 AUTHOR.        COBOLware Services Ltda.
     4 DATE-WRITTEN.  11/12/2017.
     5 SECURITY.      *************************************************
     6                *                                               *
     7                *  Descreva a finalidade do seu novo programa   *
     8                *                                               *
     9                *************************************************
    10 ENVIRONMENT DIVISION.
    11 CONFIGURATION SECTION.
    12 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
    13 INPUT-OUTPUT SECTION.
    14 FILE-CONTROL.
    15
    16 DATA DIVISION.
    17 FILE SECTION.
    18
    19 WORKING-STORAGE SECTION.
    20
    21 01  AREAS-DE-TRABALHO.
    22     05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.
   125
   126 LINKAGE SECTION.
   127
   128 SCREEN SECTION.
   129
   130 PROCEDURE DIVISION.
   131
   132 000-INICIO.
   133
   134     exec cobolware execsystem
   135         command 'notepad teste.txt'
   136     end-exec
   137     exec cobolware send message 'Encerrou nodepad'
   138     end-exec.
   139
   140 000-99-FIM. GOBACK.
   141
   142 100-PROCESSAMENTO.
   143
   144 100-99-FIM. EXIT.
   145
   146 800-INICIAIS.
   147
   148 800-99-FIM. EXIT.
   149
   150 900-FINAIS.
   151
   152 900-99-FIM. EXIT.
   153
   154 END PROGRAM TSEXEC.
