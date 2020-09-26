      $Set CallFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSODBC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/05/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Descreva a finalidade do seu novo programa   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FileName ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FileName-CHAVE
                  ALTERNATE RECORD KEY IS FileName-DESCRICAO
                                          WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-FileName.

       DATA DIVISION.
       FILE SECTION.

       FD  FileName
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FileName.

       01  FileName-REG.
           05 FileName-CHAVE.
              10 FileName-CODIGO          PIC  9(005).
           05 FileName-DESCRICAO          PIC  X(030).
           05 FileName-PRECO              PIC  9(008)V99.
           05 FileName-TIPO               PIC  9(001).
              88 FileName-PECA                         VALUE 1.
              88 FileName-ACABADO                      VALUE 2.
              88 FileName-MATERIAL                     VALUE 3.
           05 FileName-OPCOES.
              10 FileName-IMPORTADO       PIC  9(001).
              10 FileName-GARANTIA        PIC  9(001).
              10 FileName-DURAVEL         PIC  9(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 FS-FileName              PIC  X(002) VALUE "00".
           05 LB-FileName              PIC  X(050) VALUE "FileName".
       PROCEDURE DIVISION.

       000-INICIO.

           OPEN INPUT FileName
           PERFORM VARYING FileName-CODIGO FROM 1 BY 1
                                UNTIL FS-FILENAME > '10'
                  READ FileName
                  IF FS-FileName < '09'
                     DISPLAY FileName-CODIGO FILENAME-DESCRICAO
                  END-IF
           END-PERFORM.

       000-99-FIM. GOBACK.

       END PROGRAM TSODBC.
