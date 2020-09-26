       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWATTW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/02/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Amazena e fonece atributos da CWBOXW         *
                      *                                               *
                      *************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT WINWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WINWORK-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-WINWORK.

       DATA DIVISION.
       FILE SECTION.

       FD  WINWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINWORK.

       01  WINWORK-REG.
           02 WINWORK-CHAVE              PIC  X(002).
           02 WINWORK-ATRIBUTOS          PIC X(2000).

       WORKING-STORAGE SECTION.

       01  ER-WINWORK.
           10 FS-WINWORK            PIC  X(002) VALUE "00".
           10 LB-WINWORK            PIC  X(255) VALUE "cwATTW".

       LINKAGE SECTION.

       01  FUNCAO PIC X.
       01  BUFFER PIC X(2000).
       01  CHAVE  PIC XX.

       PROCEDURE DIVISION USING FUNCAO BUFFER CHAVE.

       000-INICIO.

           DISPLAY 'CWATTW' UPON ENVIRONMENT-NAME
           DISPLAY 'CWATCH' UPON ENVIRONMENT-VALUE

           ON 1 OPEN I-O WINWORK MOVE LOW-VALUES TO WINWORK-REG.

           IF CHAVE < WINWORK-CHAVE
              DELETE WINWORK RECORD
           END-IF

           MOVE CHAVE TO WINWORK-CHAVE
           READ WINWORK

           IF FS-WINWORK > '09'
              MOVE LOW-VALUES TO WINWORK-ATRIBUTOS
              WRITE WINWORK-REG
           END-IF

           IF FUNCAO = "S" OR "s"
              MOVE BUFFER TO WINWORK-ATRIBUTOS
              REWRITE WINWORK-REG
           ELSE
              MOVE WINWORK-ATRIBUTOS TO BUFFER
           END-IF

           DISPLAY 'CWATTW' UPON ENVIRONMENT-NAME
           DISPLAY 'CWATTW' UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       END PROGRAM CWATTW.
