       IDENTIFICATION DIVISION.
       PROGRAM-ID.    KEYPROG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/05/2002.
       SECURITY.      *************************************************
                      *                                               *
                      * Gera bath com lista dos programas cadastrados *
                      * no CWCONF.                                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWCONF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWCONF-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CWCONF.

           SELECT PROGS ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-PROGS.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *          Configuracao do COBOLware 4.0                         *
      ******************************************************************

       FD  CWCONF
           LABEL RECORD IS STANDARD
           RECORDING MODE IS V
           RECORD CONTAINS 32 TO 2008 CHARACTERS
           VALUE OF FILE-ID IS LB-CWCONF.

       01  CWCONF-REG.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO                        PIC  X(002).
              10 CWCONF-ELEMENTO                    PIC  X(030).

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

       01  CWCONF-REGJB.
           05 FILLER                                PIC  X(002).
           05 CWCONF-JOB                            PIC  X(030).
           05 CWCONF-JOB-MODULO                     PIC  X(050).
           05 CWCONF-JOB-TIPO                       PIC  9(001).
              88 CWCONF-JOB-BINARIO                           VALUE 1.
              88 CWCONF-JOB-COBOL                             VALUE 2.
           05 CWCONF-JOB-PARAMETRO                  PIC  X(060).
           05 CWCONF-JOB-PROXIMO-RC-OK              PIC  X(007).
           05 CWCONF-JOB-PROXIMO-NAO-OK             PIC  X(007).
           05 CWCONF-JOB-MENSAGEM                   PIC  X(050).

       01  CWCONF-REGRT.
           05 FILLER                                PIC  X(032).
           05 CWCONF-TOT-ROTINAS                    PIC  9(002).
           05 CWCONF-ROTINAS.
              10 CWCONF-ROTINA            OCCURS 84 PIC  X(008).

       FD  PROGS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PROGS.

       01  PROGS-REG.
           05 PROGS-PKZIP     PIC X(080).

       WORKING-STORAGE SECTION.

       01  FUNCAO   PIC X(001) VALUE "E".
       01  TIPO     PIC X(002) VALUE SPACES.
       01  ELEMENTO PIC X(030) VALUE SPACES.
       01  ARQUIVO  PIC X(050) VALUE "menu.cw".
       01  AREAS-DE-TRABALHO.
           05 K2                       PIC  9(002) VALUE ZERO.
           05 Y                 COMP-X PIC  9(008) VALUE ZERO.
           05 K                        PIC  9(002) VALUE ZERO.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 PROGRAMA2                PIC  X(012) VALUE SPACES.
           05 DESCRICAO                PIC  X(035) VALUE SPACES.
           05 NEW-ITEM                 PIC  X(080) VALUE SPACES.
           05 TESTE-OPCAO              PIC  X(001) VALUE SPACE.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 MAX                      PIC  9(002) VALUE 0.
           05 OC                       PIC  9(002) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
           05 REDEFINES PROGRAMA.
              10 PK-X OCCURS 8 COMP-X  PIC  9(002).
           05 TECLA               PIC 9(002) VALUE 0. COPY CWKEYS.
           05 ER-CWCONF.
              10 FS-CWCONF             PIC  X(002) VALUE "00".
              10 LB-CWCONF             PIC  X(050) VALUE "cwconf".
           05 ER-PROGS.
              10 FS-PROGS              PIC  X(002) VALUE "00".
              10 LB-PROGS              PIC  X(050) VALUE "PROGS.BAT".

       PROCEDURE DIVISION.

       000-INICIO.

           CALL "CWEXIM" USING FUNCAO TIPO ELEMENTO ARQUIVO
           OPEN INPUT CWCONF
                      OUTPUT PROGS
           WRITE PROGS-REG FROM "@Echo Off"
           WRITE PROGS-REG FROM "If Exist PROGS.LOG Erase PROGS.LOG"
           WRITE PROGS-REG FROM "If Exist %1.zip Erase %1.zip"
           WRITE PROGS-REG FROM "Makepath progs"
           WRITE PROGS-REG FROM "If Exist progs\*.int erase progs\*.int"
           IF   FS-CWCONF > "09"
                CALL "CWISAM" USING ER-CWCONF
           END-IF
           IF   FS-PROGS > "09"
                CALL "CWISAM" USING ER-PROGS
           END-IF
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                   READ CWCONF NEXT RECORD IGNORE LOCK
                   IF   FS-CWCONF = "9D"
                        CALL "CWISAM" USING ER-CWCONF
                   END-IF
                   IF   FS-CWCONF < "10"
                   AND (CWCONF-TIPO = "99" OR "SM" OR "RT")
                        IF   CWCONF-TIPO = "RT"
                             MOVE 84 TO MAX
                        END-IF
                        IF   CWCONF-TIPO = "99"
                             MOVE 26 TO MAX
                        END-IF
                        IF   CWCONF-TIPO = "SM"
                             MOVE 13 TO MAX
                        END-IF
                        PERFORM 810-CRIA-LISTA THRU 810-99-FIM
                                VARYING OC
                                   FROM 1
                                     BY 1 UNTIL OC > MAX
                   END-IF
           END-PERFORM.
           WRITE PROGS-REG FROM "Pkzip %1 progs\*.int menu.cw"
           WRITE PROGS-REG FROM "Erase progs\*.int"
           WRITE PROGS-REG FROM "Erase menu.cw"
           WRITE PROGS-REG FROM "Rd progs"
           CLOSE CWCONF PROGS.

       000-99-FIM. GOBACK.

       810-CRIA-LISTA.

           IF   CWCONF-TIPO = "RT"
                MOVE CWCONF-ROTINA (OC) TO PROGRAMA
           ELSE
                MOVE CWCONF-PROG  (OC) TO PROGRAMA
                IF   PROGRAMA NOT = SPACES
                     CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (OC)
                                             CWCONF-FATOR-P-99 (OC)
                                             PROGRAMA
                END-IF
           END-IF
           IF   PROGRAMA = SPACES
                GO TO 810-99-FIM
           END-IF
           INSPECT PROGRAMA
                   CONVERTING MINUSCULAS TO MAIUSCULAS

           MOVE "Echo Compactando " TO PROGS-REG
           IF   CWCONF-TIPO = "RT"
                MOVE "Rotina" TO DESCRICAO
           ELSE
                MOVE CWCONF-NM-OPCAO(OC) TO TESTE-OPCAO
                IF   TESTE-OPCAO = "/"
                     MOVE CWCONF-NM-OPCAO(OC) (3: ) TO DESCRICAO
                ELSE
                     MOVE CWCONF-NM-OPCAO(OC)       TO DESCRICAO
                END-IF
           END-IF
           PERFORM 820-AJUSTA-STRING THRU 820-99-FIM
           MOVE PROGRAMA            TO PROGRAMA2 TESTE-OPCAO
           IF   PROGRAMA = "CWBOXS"
                MOVE ":"             TO PROGRAMA2
                MOVE CWCONF-HELP(OC) TO PROGRAMA2 (2: )
           END-IF
           IF  (PROGRAMA2 (1: 1) NOT = ":" )
           AND (PROGRAMA2 (1: 2) NOT = "CW")
               IF  PROGRAMA2 (1: 1) = "/"
                   MOVE CWCONF-CHAVE    TO SALVA-CHAVE
                   MOVE "JB"            TO CWCONF-REG
                   MOVE PROGRAMA2 (2: ) TO CWCONF-JOB
                   READ CWCONF IGNORE LOCK
                   IF   FS-CWCONF NOT = "00"
                        MOVE "$ERRO$" TO PROGRAMA2
                   ELSE
                        IF  CWCONF-JOB-COBOL
                            MOVE CWCONF-JOB-MODULO TO PROGRAMA2
                            INSPECT PROGRAMA2
                                    CONVERTING MINUSCULAS
                                            TO MAIUSCULAS
                        ELSE
                            MOVE "~" TO PROGRAMA2
                        END-IF
                   END-IF
                   MOVE SALVA-CHAVE TO CWCONF-CHAVE
                   READ CWCONF IGNORE LOCK
               END-IF
               IF  PROGRAMA2 NOT = "~"
                   MOVE SPACES TO PROGS-REG
                   STRING "Echo Copiando " DELIMITED BY SIZE
                          PROGRAMA2 DELIMITED BY SPACE
                          ".INT ..." DELIMITED BY SIZE
                          DESCRICAO DELIMITED BY SIZE
                     INTO PROGS-REG
                   WRITE PROGS-REG
                   MOVE SPACES TO PROGS-REG
                   STRING "If Exist %CIL%\" DELIMITED BY SIZE
                          PROGRAMA2 DELIMITED BY SPACE
                          ".INT Copy %CIL%\" DELIMITED BY SIZE
                          PROGRAMA2 DELIMITED BY SPACE
                          ".INT progs>> nul"
                          DELIMITED BY SIZE
                     INTO PROGS-REG
                   WRITE PROGS-REG
                   MOVE SPACES TO PROGS-REG
                   STRING "If Not Exist %CIL%\" DELIMITED BY SIZE
                          PROGRAMA2 DELIMITED BY SPACE
                          ".INT Echo %CIL%\" DELIMITED BY SIZE
                          PROGRAMA2 DELIMITED BY SPACE
                          ".INT nÆo encontrado>> PROGS.LOG"
                          DELIMITED BY SIZE
                     INTO PROGS-REG
                   WRITE PROGS-REG
               END-IF
           END-IF.

       810-99-FIM. EXIT.

       820-AJUSTA-STRING.

           MOVE SPACES TO NEW-ITEM
           MOVE 0      TO K2
           MOVE 35     TO Y
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > Y
                   IF   DESCRICAO (K: 1) = X"7E"
                        ADD 1 TO K
                   END-IF
                   ADD 1 TO K2
                   MOVE DESCRICAO (K: 1) TO NEW-ITEM (K2: 1)
           END-PERFORM
           CALL "CWTEXT" USING NEW-ITEM Y
           MOVE NEW-ITEM    TO DESCRICAO.

       820-99-FIM. EXIT.

       900-FINAIS.

       900-99-FIM. EXIT.

       END PROGRAM KEYPROG.
