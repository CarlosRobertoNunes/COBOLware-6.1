      $Set CallFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPGID.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/08/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tabela de programas cadastrados no menu      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT PROGRAMAS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS PROGRAMAS-PROGRAMA
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-PROGRAMAS.

       DATA DIVISION.
       FILE SECTION.

       FD  PROGRAMAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PROGRAMAS.

       01  PROGRAMAS-REG.
           05 PROGRAMAS-PROGRAMA     PIC  X(008).
           05 PROGRAMAS-DESCRICAO    PIC  X(034).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 WS-PROGRAMA         PIC  X(030) VALUE SPACES.
           05 ER-PROGRAMAS.
              10 FS-PROGRAMAS     PIC  X(002) VALUE "00".
              10 LB-PROGRAMAS     PIC  X(255) VALUE "cwidpg".

       LINKAGE SECTION.

       01  FUNCAO    PIC X.
       01  PROGRAMA  PIC X(8).
       01  DESCRICAO PIC X(34).

       PROCEDURE DIVISION USING FUNCAO PROGRAMA DESCRICAO.

       000-INICIO.

           ON 1 OPEN I-O PROGRAMAS
                INITIALIZE PROGRAMAS-REG.

           IF FUNCAO = 'W'
              INSPECT PROGRAMA CONVERTING MINUSCULAS TO MAIUSCULAS
              INSPECT PROGRAMA CONVERTING LOW-VALUES TO SPACES
              IF (PROGRAMA NOT = SPACES)
              AND(PROGRAMA NOT = 'COBWARE')
                 MOVE PROGRAMA  TO PROGRAMAS-REG
                 MOVE DESCRICAO TO PROGRAMAS-DESCRICAO
                 WRITE PROGRAMAS-REG
              END-IF
           ELSE
              CALL    "CWUSERPGM" USING "G" WS-PROGRAMA
              MOVE   WS-PROGRAMA    TO PROGRAMA
              INSPECT PROGRAMA CONVERTING LOW-VALUES TO SPACES
              IF (PROGRAMA NOT = SPACES)
              AND (PROGRAMA NOT = PROGRAMAS-PROGRAMA)
                  MOVE   PROGRAMA       TO PROGRAMAS-PROGRAMA
                  READ PROGRAMAS
                  IF  FS-PROGRAMAS NOT = '00'
                      MOVE SPACES TO PROGRAMA
                                     PROGRAMAS-REG
                      MOVE PROGRAMA TO WS-PROGRAMA
                      CALL "CWUSERPGM" USING "S" WS-PROGRAMA
                  END-IF
               END-IF
               MOVE PROGRAMAS-PROGRAMA  TO PROGRAMA
               MOVE PROGRAMAS-DESCRICAO TO DESCRICAO
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWPGID.
