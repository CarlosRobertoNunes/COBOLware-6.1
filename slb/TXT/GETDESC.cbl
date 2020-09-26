      $Set NoCallFH
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GETDESC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/09/2016.
       SECURITY.      *************************************************
                      *                                               *
                      *  Obtem a descriá∆o da opá∆o no menu a partir  *
                      *  o nome do programa.                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TEXTO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  RESERVE       NO ALTERNATE AREA
                  FILE STATUS   IS FS-TEXTO.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT TABELA ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS TABELA-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-TABELA.

       DATA DIVISION.
       FILE SECTION.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG                  PIC  X(2008).

       FD  TABELA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "tabela$$".

       01  TABELA-REG.
           05 TABELA-CHAVE     PIC  X(8).
           05 TABELA-NM-OPCAO  PIC  X(034).

       WORKING-STORAGE SECTION.

       77  TIPO                 PIC  X(002) VALUE SPACES.
       77  ELEMENTO             PIC  X(030) VALUE SPACES.
       77  ARQUIVO              PIC  X(050) VALUE SPACES.
       77  USUARIO              PIC  X(030) VALUE SPACES.
       77  TASK                 PIC  9(006) VALUE ZERO.
       77  PROGRAMA             PIC  X(008) VALUE SPACES.
       77  I                    PIC  9(006) VALUE ZERO.
       77  LB-TEXTO             PIC  X(030) VALUE SPACES.
       77  FS-TEXTO             PIC  X(002) VALUE "00".
       77  FS-TABELA            PIC  X(002) VALUE "00".
       77  MINUSCULAS             PIC  X(049) VALUE
              "abcdefghijklmnopqrstuvwxyz†Ç°¢£ÏÖäçïóÑâîÅÉàåìñ∆‰á".
       77  MAIUSCULAS             PIC  X(049) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZµê÷‡ÈÌ∑‘Î„Îé”ôö∂“◊‚Í«ÂÄ".

       01  CWCONF-REG99.
           05 FILLER                                PIC  X(002).
           05 CWCONF-PAGINA                         PIC  9(004).
           05 FILLER                                PIC  X(026).
           05 CWCONF-OPCOES.
              10 CWCONF-OPCAO OCCURS 26.
                 15 CWCONF-NIVEL                    PIC  9(001).
                 15 CWCONF-CHECK                    PIC  X(001).
                 15 CWCONF-FATOR-P-99        COMP-X PIC  9(002).
                 15 CWCONF-FATOR-S-99        COMP-X PIC  9(002).
                 15 CWCONF-PROG                     PIC  X(008).
                 15 CWCONF-SIZE-P-99         COMP-X PIC  9(002).
                 15 CWCONF-SIZE-S-99         COMP-X PIC  9(002).
                 15 CWCONF-PASS                     PIC  X(006).
                 15 CWCONF-HELP                     PIC  X(020).
                 15 CWCONF-NO-OPCAO                 PIC  9(002).
                 15 CWCONF-NM-OPCAO                 PIC  X(034).

       LINKAGE SECTION.

       01  PROG      PIC  X(8).
       01  NM-OPCAO  PIC  X(34).


       PROCEDURE DIVISION USING PROG NM-OPCAO.

       INICIO.

           CALL "CWGETU" USING USUARIO TASK PROGRAMA "?"

           ON 1
              STRING "GETDES." TASK DELIMITED BY SIZE INTO LB-TEXTO
              OPEN I-O TABELA
              MOVE LB-TEXTO TO ARQUIVO
              MOVE "99" TO TIPO
              PERFORM CARREGA-TEXTO THRU FIM-CARREGA-TEXTO
              MOVE LB-TEXTO TO ARQUIVO
              MOVE "SM" TO TIPO
              PERFORM CARREGA-TEXTO THRU FIM-CARREGA-TEXTO.

           IF PROG = SPACES
              MOVE PROGRAMA TO TABELA-CHAVE
           ELSE
              MOVE PROG     TO TABELA-CHAVE
           END-IF

           INSPECT TABELA-CHAVE CONVERTING
                   MINUSCULAS TO MAIUSCULAS

           READ TABELA

           IF   FS-TABELA < "10"
                MOVE TABELA-NM-OPCAO TO NM-OPCAO
           ELSE
                MOVE SPACES          TO NM-OPCAO
           END-IF.


       FIM. GOBACK.

       CARREGA-TEXTO.

           CALL "CWEXIM" USING "E" TIPO ELEMENTO ARQUIVO
           OPEN INPUT TEXTO
           PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                   READ TEXTO
                   IF FS-TEXTO < "10"
                      MOVE TEXTO-REG TO CWCONF-REG99
                      PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
                           IF CWCONF-PROG       (I) NOT = SPACES
                              CALL "CWCODE" USING "D"
                                    CWCONF-SIZE-P-99  (I)
                                    CWCONF-FATOR-P-99 (I)
                                    CWCONF-PROG       (I)
                              IF   (CWCONF-PROG (I) NOT = "CWBOXS")
                              AND  (CWCONF-PROG (I) NOT = "GRBOXS")
                              AND  (CWCONF-PROG (I) NOT = "CWMENU")
                              AND  (CWCONF-PROG (I) NOT = "GRMENU")
                                   MOVE CWCONF-PROG (I) TO TABELA-CHAVE
                                   MOVE CWCONF-NM-OPCAO (I)
                                   TO TABELA-NM-OPCAO
                                   INSPECT TABELA-CHAVE CONVERTING
                                           MINUSCULAS TO MAIUSCULAS
                                   WRITE TABELA-REG
                              END-IF
                           END-IF
                      END-PERFORM
                   END-IF
           END-PERFORM
           CLOSE TEXTO
           DELETE FILE TEXTO.

       FIM-CARREGA-TEXTO. EXIT.

       END PROGRAM GETDESC.
