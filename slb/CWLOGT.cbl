       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOGT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/03/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava log para depurar telas                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TELA ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS TELA-CHAVE
                  LOCK MODE    IS EXCLUSIVE.

       DATA DIVISION.
       FILE SECTION.

       FD  TELA
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 28 TO 20028 DEPENDING ON SZ-TELA
           VALUE OF FILE-ID IS LB-TELA.

       01  TELA-REG.
           05 TELA-CHAVE.
              10 TELA-SEQ    PIC 9(0008).
              10 TELA-MODULO PIC X(0006).
           05 TELA-TAMANHO   PIC 9(008) COMP-X.
           05 TELA-FUNCAO    PIC X(010).
           05 TELA-DADOS.
              10 TELA-BYTE PIC X OCCURS 1 TO 20000 DEPENDING ON
                 TELA-TAMANHO.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CHAVE                PIC  9(008) VALUE 0.
           05 I                    PIC  9(004) VALUE 0.
           05 SZ-TELA              PIC  9(006) VALUE 0.
           05 FIELDS               PIC  9(004) VALUE 0.
           05 ER-TELA.
              10 FS-TELA           PIC  X(002) VALUE "00".
              10 LB-TELA           PIC  X(255) VALUE "CWUSER-LOG".
           05 USUARIO              PIC  X(030) VALUE SPACES.
           05 TASK                 PIC  9(006) VALUE 0.
           05 PROGRAMAX            PIC  X(008) VALUE SPACES.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  MODULO         PIC X(006).
       01  MATRIZ         PIC X(001).
       01  TAMANHO-MATRIZ PIC 9(008) COMP-X.
       01  FUNCAO         PIC X(10).

       PROCEDURE DIVISION USING MODULO
                                MATRIZ
                                TAMANHO-MATRIZ
                                FUNCAO.

       000-INICIO.

           ON 1
              CALL "CWGETU" USING USUARIO TASK PROGRAMAX "?"
              MOVE TASK TO LB-TELA (8: )
              OPEN OUTPUT TELA
              CLOSE TELA.

           OPEN I-O TELA
           COMPUTE SZ-TELA = TAMANHO-MATRIZ + 28
           ADD  1                        TO CHAVE
           MOVE MODULO                   TO TELA-MODULO
           MOVE CHAVE                    TO TELA-SEQ
           MOVE TAMANHO-MATRIZ           TO TELA-TAMANHO
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF  X91-PARAMETER > 3
               MOVE FUNCAO TO TELA-FUNCAO
           ELSE
               MOVE SPACES TO TELA-FUNCAO
           END-IF
           MOVE MATRIZ(1:TAMANHO-MATRIZ) TO TELA-DADOS
           WRITE TELA-REG
           CLOSE TELA.

       000-99-FIM. GOBACK.

       900-99-FIM. EXIT.

       END PROGRAM CWLOGT.
