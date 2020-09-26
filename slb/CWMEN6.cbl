       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN6 INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao do menu                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT VELHOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS VELHOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-VELHOS.

           SELECT NOVOS     ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS NOVOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-NOVOS.

           SELECT GRUPOS    ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS GRUPOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-GRUPOS.

       DATA DIVISION.
       FILE SECTION.

       FD  VELHOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-VELHOS.

       01  VELHOS-REG.
           05 VELHOS-CHAVE       PIC  X(008).

       FD  NOVOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-NOVOS.

       01  NOVOS-REG.
           05 NOVOS-CHAVE       PIC  X(008).

       FD  GRUPOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GRUPOS.

       01  GRUPOS-REG.
           05 GRUPOS-CHAVE       PIC  X(022).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LOCKNEW                  PIC  X(003) VALUE SPACES.
           05 ER-VELHOS.
              10 FS-VELHOS             PIC  X(002) VALUE "00".
              10 LB-VELHOS             PIC  X(255) VALUE "eximV###".
           05 ER-NOVOS.
              10 FS-NOVOS              PIC  X(002) VALUE "00".
              10 LB-NOVOS              PIC  X(255) VALUE "eximN###".
           05 ER-GRUPOS.
              10 FS-GRUPOS             PIC  X(002) VALUE "00".
              10 LB-GRUPOS             PIC  X(255) VALUE "eximG###".
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 TERM       PIC X(05) VALUE SPACES.
           05 MODULO     PIC X(15) VALUE SPACES.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 LIXO                     PIC  X(030) VALUE SPACES.
           05 ERRO                     PIC  9(001) VALUE 0.
           05 XB3                      PIC  X(001) VALUE SPACE.
           05 XC4                      PIC  X(001) VALUE SPACE.
           05 XBF                      PIC  X(001) VALUE SPACE.
           05 TIPO-REG                 PIC  X(002) VALUE SPACES.
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 CURSOR-POSITION.
              15 ROW-CURSOR            PIC  9(002) COMP-X VALUE 24.
              15 COLUMN-CURSOR         PIC  9(002) COMP-X VALUE 02.
           05 TELA-TEXTO               PIC  X(012) VALUE "f10-~Prefixo".
           05 TESTA-COMANDO            PIC  X(001) VALUE SPACE.
              88 C-OK VALUE "A" "a" "D" "d" "C" "c" "M" "m" "P" "p"
                            "F" "f" '"'.
           05 HHMMSSDD.
              10 FILLER                PIC  X(004).
              10 SS                    PIC  9(002).
              10 DD                    PIC  9(002).
           05 WS                       PIC  X(034) VALUE SPACES.
           05 STATUS-DELETE            PIC  9(001) VALUE ZERO.
           05 STATUS-TELA              PIC  9(001) VALUE 1.
           05 OPCAO                    PIC  9(002) VALUE ZERO.
           05 VEZ                      PIC  9(001) VALUE ZERO.
           05 P                        PIC S9(002) VALUE ZERO.
           05 P1                       PIC S9(002) VALUE ZERO.
           05 W                        PIC  9(002) VALUE ZERO.
           05 L                        PIC  9(002) VALUE ZERO.
           05 I                        PIC  9(002) VALUE ZERO.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 R                        PIC  9(002) VALUE ZERO.
           05 C                        PIC  9(002) VALUE ZERO.
           05 X                        PIC S9(002) VALUE ZERO.
           05 XX                       PIC  X(001) VALUE SPACE.
           05 X-A                      PIC S9(002) VALUE ZERO.
           05 LY                       PIC  9(002) VALUE ZERO.
           05 CY                       PIC  9(002) VALUE ZERO.
           05 CY-1                     PIC  9(002) VALUE ZERO.
           05 CURPOS.
              10 LX                    PIC  9(002) VALUE ZERO.
              10 CX                    PIC  9(002) VALUE ZERO.
           05 CX-A                     PIC  9(002) VALUE ZERO.
           05 TAMANHO                  PIC  9(002) VALUE ZERO.
           05 PAGINA                   PIC  9(004) VALUE 1.
           05 SALVA-PAGINA             PIC  9(004) VALUE ZERO.
           05 SALVA-MODULO             PIC  X(015) VALUE SPACES.
           05 TIPO                     PIC  X(014) VALUE SPACES.
           05 TELA                     PIC  9(001) VALUE 1.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 MSG-1                    PIC  X(030) VALUE
              "Comandos inconsistentes".
           05 COMANDOS                             VALUE SPACES.
              10 COMANDO OCCURS 26     PIC  X(001).
           05 MOVIDAS                              VALUE SPACES.
              10 SALVA-LINHA OCCURS 26 PIC  X(076).
           05 WORK-COMANDOS                        VALUE SPACES.
              10 SALVA-COMANDO OCCURS 26 PIC  X(001).
              10 NOVA-LINHA    OCCURS 26 PIC  X(076).
           05 TELA-EXPORTADA             PIC  X(988) VALUE SPACES.
           05 OPCOES                                 VALUE SPACES.
              10 TELA-OPCAO  OCCURS 13.
                 15 TELA-NIVEL                    PIC  9(001).
                 15 CWCHECK-E                     PIC  X(001).
                 15 TELA-FATOR-P-99        COMP-X PIC  9(002).
                 15 TELA-FATOR-S-99        COMP-X PIC  9(002).
                 15 TELA-PROG                     PIC  X(008).
                 15 TELA-SIZE-P-99         COMP-X PIC  9(002).
                 15 TELA-SIZE-S-99         COMP-X PIC  9(002).
                 15 TELA-PASS                     PIC  X(006).
                 15 TELA-HELP                     PIC  X(020).
                 15 TELA-NO-OPCAO                 PIC  9(002).
                 15 TELA-NM-OPCAO                 PIC  X(034).
           05 REDEFINES OPCOES.
              10 PIC X(4). 10 PROG01 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG02 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG03 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG04 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG05 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG06 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG07 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG08 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG09 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG10 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG11 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG12 PIC X(8). 10 PIC X(64).
              10 PIC X(4). 10 PROG13 PIC X(8). 10 PIC X(64).
           05 CBL-WRITE-SCR-CHARS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC  X(080) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.

       COPY CWBOXS.
       COPY CWCONF.
       COPY CWSEND.

       SCREEN SECTION.

       01  TELA-DATA.
           05 LINE 09 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(01) BLANK ZERO.
           05 LINE 09 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(01).
           05 LINE 09 COLUMN 42 PIC X(001) USING CWCHECK-E(01).
           05 LINE 09 COLUMN 44 PIC X(008) USING PROG01.
           05 LINE 09 COLUMN 53 PIC X(006) USING TELA-PASS(01).
           05 LINE 09 COLUMN 60 PIC X(020) USING TELA-HELP(01).
           05 LINE 10 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(02) BLANK ZERO.
           05 LINE 10 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(02).
           05 LINE 10 COLUMN 42 PIC X(001) USING CWCHECK-E(02).
           05 LINE 10 COLUMN 44 PIC X(008) USING PROG02.
           05 LINE 10 COLUMN 53 PIC X(006) USING TELA-PASS(02).
           05 LINE 10 COLUMN 60 PIC X(020) USING TELA-HELP(02).
           05 LINE 11 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(03) BLANK ZERO.
           05 LINE 11 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(03).
           05 LINE 11 COLUMN 42 PIC X(001) USING CWCHECK-E(03).
           05 LINE 11 COLUMN 44 PIC X(008) USING PROG03.
           05 LINE 11 COLUMN 53 PIC X(006) USING TELA-PASS(03).
           05 LINE 11 COLUMN 60 PIC X(020) USING TELA-HELP(03).
           05 LINE 12 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(04) BLANK ZERO.
           05 LINE 12 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(04).
           05 LINE 12 COLUMN 42 PIC X(001) USING CWCHECK-E(04).
           05 LINE 12 COLUMN 44 PIC X(008) USING PROG04.
           05 LINE 12 COLUMN 53 PIC X(006) USING TELA-PASS(04).
           05 LINE 12 COLUMN 60 PIC X(020) USING TELA-HELP(04).
           05 LINE 13 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(05) BLANK ZERO.
           05 LINE 13 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(05).
           05 LINE 13 COLUMN 42 PIC X(001) USING CWCHECK-E(05).
           05 LINE 13 COLUMN 44 PIC X(008) USING PROG05.
           05 LINE 13 COLUMN 53 PIC X(006) USING TELA-PASS(05).
           05 LINE 13 COLUMN 60 PIC X(020) USING TELA-HELP(05).
           05 LINE 14 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(06) BLANK ZERO.
           05 LINE 14 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(06).
           05 LINE 14 COLUMN 42 PIC X(001) USING CWCHECK-E(06).
           05 LINE 14 COLUMN 44 PIC X(008) USING PROG06.
           05 LINE 14 COLUMN 53 PIC X(006) USING TELA-PASS(06).
           05 LINE 14 COLUMN 60 PIC X(020) USING TELA-HELP(06).
           05 LINE 15 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(07) BLANK ZERO.
           05 LINE 15 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(07).
           05 LINE 15 COLUMN 42 PIC X(001) USING CWCHECK-E(07).
           05 LINE 15 COLUMN 44 PIC X(008) USING PROG07.
           05 LINE 15 COLUMN 53 PIC X(006) USING TELA-PASS(07).
           05 LINE 15 COLUMN 60 PIC X(020) USING TELA-HELP(07).
           05 LINE 16 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(08) BLANK ZERO.
           05 LINE 16 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(08).
           05 LINE 16 COLUMN 42 PIC X(001) USING CWCHECK-E(08).
           05 LINE 16 COLUMN 44 PIC X(008) USING PROG08.
           05 LINE 16 COLUMN 53 PIC X(006) USING TELA-PASS(08).
           05 LINE 16 COLUMN 60 PIC X(020) USING TELA-HELP(08).
           05 LINE 17 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(09) BLANK ZERO.
           05 LINE 17 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(09).
           05 LINE 17 COLUMN 42 PIC X(001) USING CWCHECK-E(09).
           05 LINE 17 COLUMN 44 PIC X(008) USING PROG09.
           05 LINE 17 COLUMN 53 PIC X(006) USING TELA-PASS(09).
           05 LINE 17 COLUMN 60 PIC X(020) USING TELA-HELP(09).
           05 LINE 18 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(10) BLANK ZERO.
           05 LINE 18 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(10).
           05 LINE 18 COLUMN 42 PIC X(001) USING CWCHECK-E(10).
           05 LINE 18 COLUMN 44 PIC X(008) USING PROG10.
           05 LINE 18 COLUMN 53 PIC X(006) USING TELA-PASS(10).
           05 LINE 18 COLUMN 60 PIC X(020) USING TELA-HELP(10).
           05 LINE 19 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(11) BLANK ZERO.
           05 LINE 19 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(11).
           05 LINE 19 COLUMN 42 PIC X(001) USING CWCHECK-E(11).
           05 LINE 19 COLUMN 44 PIC X(008) USING PROG11.
           05 LINE 19 COLUMN 53 PIC X(006) USING TELA-PASS(11).
           05 LINE 19 COLUMN 60 PIC X(020) USING TELA-HELP(11).
           05 LINE 20 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(12) BLANK ZERO.
           05 LINE 20 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(12).
           05 LINE 20 COLUMN 42 PIC X(001) USING CWCHECK-E(12).
           05 LINE 20 COLUMN 44 PIC X(008) USING PROG12.
           05 LINE 20 COLUMN 53 PIC X(006) USING TELA-PASS(12).
           05 LINE 20 COLUMN 60 PIC X(020) USING TELA-HELP(12).
           05 LINE 21 COLUMN 05 PIC 9(001)
                 USING TELA-NIVEL(13) BLANK ZERO.
           05 LINE 21 COLUMN 07 PIC X(034) USING TELA-NM-OPCAO(13).
           05 LINE 21 COLUMN 42 PIC X(001) USING CWCHECK-E(13).
           05 LINE 21 COLUMN 44 PIC X(008) USING PROG13.
           05 LINE 21 COLUMN 53 PIC X(006) USING TELA-PASS(13).
           05 LINE 21 COLUMN 60 PIC X(020) USING TELA-HELP(13).

       01  TELA-TXT.
           05 LINE 08 COLUMN 42 VALUE "Confirmar".
           05 LINE 07 COLUMN 03 VALUE "Comando".
           05 LINE 08 COLUMN 05 VALUE "N¡vel".
           05 LINE 08 COLUMN 15 VALUE "Descri‡Æo no menu".
           05 LINE 07 COLUMN 44 VALUE "Programa".
           05 LINE 08 COLUMN 53 VALUE "Senha".
           05 LINE 08 COLUMN 60 VALUE "Help/P gina/Path".

       01  TELA-PAGINA.
           05 LINE 07 COLUMN 15 VALUE "P gina".
           05 LINE 07 COLUMN 22 PIC Z(004) USING PAGINA.
           05 LINE 07 COLUMN 27 PIC X(014) FROM TIPO.

       01  TELA-MODULO.
           05 LINE 07 COLUMN 56 VALUE "M¢dulo".
           05 LINE 07 COLUMN 63 PIC X(015) USING MODULO.

       01  TELA-COMANDOS AUTO.
           05 LINE 09 COLUMN 03 PIC X(001) USING COMANDO (01).
           05 LINE 10 COLUMN 03 PIC X(001) USING COMANDO (02).
           05 LINE 11 COLUMN 03 PIC X(001) USING COMANDO (03).
           05 LINE 12 COLUMN 03 PIC X(001) USING COMANDO (04).
           05 LINE 13 COLUMN 03 PIC X(001) USING COMANDO (05).
           05 LINE 14 COLUMN 03 PIC X(001) USING COMANDO (06).
           05 LINE 15 COLUMN 03 PIC X(001) USING COMANDO (07).
           05 LINE 16 COLUMN 03 PIC X(001) USING COMANDO (08).
           05 LINE 17 COLUMN 03 PIC X(001) USING COMANDO (09).
           05 LINE 18 COLUMN 03 PIC X(001) USING COMANDO (10).
           05 LINE 19 COLUMN 03 PIC X(001) USING COMANDO (11).
           05 LINE 20 COLUMN 03 PIC X(001) USING COMANDO (12).
           05 LINE 21 COLUMN 03 PIC X(001) USING COMANDO (13).

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS         THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO    THRU 100-99-FIM
                   UNTIL ESC
           PERFORM 900-FINAIS           THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           CALL "CWMODE" USING "G" PAGINA MODULO TIPO-REG
           IF   TIPO-REG = "99"
                IF TELA = 1
                   MOVE "Principal (A)" TO TIPO
                ELSE
                   MOVE "Principal (B)" TO TIPO
                END-IF
           ELSE
                IF TELA = 1
                   MOVE "Secund rio (A)" TO TIPO
                ELSE
                   MOVE "Secund rio (B)" TO TIPO
                END-IF
           END-IF
           DISPLAY TELA-PAGINA
                   TELA-MODULO

           MOVE 0 TO TECLA

           PERFORM DECLARA-RODAPE THRU FIM-DECLARA-RODAPE
           IF   STATUS-TELA = 1
                MOVE 1 TO X
                PERFORM 160-ACCEPT-TELA-DADOS THRU 160-99-FIM
                        UNTIL ESC
                               OR F3
                               OR F4
                               OR F5
                               OR F8
                               OR F9
                               OR F10
                               OR PAGE-UP
                               OR PAGE-DOWN
                IF   NOT F8
                     PERFORM 140-SALVA-TELA THRU 140-99-FIM
                END-IF
           ELSE
                ACCEPT TELA-COMANDOS
                ACCEPT TECLA FROM ESCAPE KEY
           END-IF
           EXEC COBOLware OBJECT DROP END-EXEC

           EVALUATE TRUE
           WHEN F8
                MOVE    SPACES           TO COMANDOS
                PERFORM 110-LER-CWCONF THRU 110-99-FIM
           WHEN F10
                IF   STATUS-TELA = 1
                     MOVE 0 TO STATUS-TELA
                     MOVE "f10-~Texto  " TO TELA-TEXTO
                ELSE
                     IF   COMANDOS = SPACES
                          MOVE 1 TO STATUS-TELA
                          MOVE "f10-~Prefixo" TO TELA-TEXTO
                          DISPLAY TELA-COMANDOS
                     END-IF
                END-IF
           WHEN F9
                IF   COMANDOS NOT = SPACES
                     MOVE SPACES TO MOVIDAS
                     MOVE ZERO   TO VEZ R C
                     PERFORM TEST AFTER
                        UNTIL COMANDOS = SPACES
                           OR VEZ GREATER 1
                        MOVE SPACES TO WORK-COMANDOS
                        MOVE ZERO   TO Y
                        ADD  1      TO VEZ
                        PERFORM 130-EXECUTA-COMANDOS THRU 130-99-FIM
                                VARYING I FROM 1 BY 1 UNTIL I GREATER 13
                        MOVE SPACES TO COMANDOS
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I GREATER 13
                                MOVE SALVA-COMANDO (I) TO COMANDO    (I)
                                MOVE NOVA-LINHA    (I) TO TELA-OPCAO (I)
                        END-PERFORM
                     END-PERFORM
                     MOVE SPACES TO COMANDOS
                     DISPLAY TELA-DATA
                     PERFORM 140-SALVA-TELA THRU 140-99-FIM
                ELSE
                     CONTINUE
                END-IF
           WHEN F3
           AND  STATUS-TELA = 1
                MOVE OPCOES TO TELA-EXPORTADA
           WHEN F4
           AND  STATUS-TELA = 1
                MOVE 1 TO CWSEND-OPTION
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > 13
                            OR CWSEND-OPTION = 2
                        IF  CWCHECK-E     (I) NOT = X"00"
                            MOVE SPACE TO CWCHECK-E     (I)
                        END-IF
                        IF (TELA-NIVEL    (I) NOT = 0)
                        OR (CWCHECK-E     (I) NOT = SPACES)
                        OR (TELA-PROG     (I) NOT = SPACES)
                        OR (TELA-PASS     (I) NOT = SPACES)
                        OR (TELA-HELP     (I) NOT = SPACES)
                        OR (TELA-NO-OPCAO (I) NOT = 0)
                        OR (TELA-NM-OPCAO (I) NOT = SPACES)
                           MOVE 2 TO CWSEND-OPTION
                        END-IF
                END-PERFORM
                IF CWSEND-OPTION = 2
                   MOVE SPACES     TO CWSEND-SCREENS
                   MOVE "  ~Sim__" TO CWSEND-SCREEN (1)
                   MOVE "  ~NÆo__" TO CWSEND-SCREEN (2)
                   MOVE "Apagar itens ?" TO CWSEND-MSG
                   CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
                IF CWSEND-OPTION = 1
                   INITIALIZE OPCOES
                   PERFORM 140-SALVA-TELA THRU 140-99-FIM
                   DISPLAY TELA-DATA
                END-IF
           WHEN F5
           AND  STATUS-TELA = 1
                MOVE 1 TO CWSEND-OPTION
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > 13
                            OR CWSEND-OPTION = 2
                        IF  CWCHECK-E     (I) NOT = X"00"
                            MOVE SPACE TO CWCHECK-E     (I)
                        END-IF
                        IF (TELA-NIVEL    (I) NOT = 0)
                        OR (CWCHECK-E     (I) NOT = SPACES)
                        OR (TELA-PROG     (I) NOT = SPACES)
                        OR (TELA-PASS     (I) NOT = SPACES)
                        OR (TELA-HELP     (I) NOT = SPACES)
                        OR (TELA-NO-OPCAO (I) NOT = 0)
                        OR (TELA-NM-OPCAO (I) NOT = SPACES)
                           MOVE 2 TO CWSEND-OPTION
                        END-IF
                END-PERFORM
                IF CWSEND-OPTION = 2
                   MOVE SPACES     TO CWSEND-SCREENS
                   MOVE "  ~Sim__" TO CWSEND-SCREEN (1)
                   MOVE "  ~NÆo__" TO CWSEND-SCREEN (2)
                   MOVE "Sobrepor itens ?" TO CWSEND-MSG
                   CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
                IF CWSEND-OPTION = 1
                   MOVE TELA-EXPORTADA TO OPCOES
                   PERFORM 140-SALVA-TELA THRU 140-99-FIM
                   DISPLAY TELA-DATA
                END-IF
           WHEN ESC
                CONTINUE
           WHEN PAGE-DOWN
                MOVE 1      TO STATUS-TELA
                MOVE SPACES TO COMANDOS
                ADD  1      TO TELA
                IF   TELA = 3
                     ADD  1 TO PAGINA
                     MOVE 1 TO TELA
                END-IF
                PERFORM 110-LER-CWCONF THRU 110-99-FIM
           WHEN PAGE-UP
           AND (PAGINA GREATER 1
           OR   TELA = 2)
                MOVE 1      TO STATUS-TELA
                MOVE SPACES TO COMANDOS
                SUBTRACT 1 FROM TELA
                IF  TELA = ZERO
                    IF   PAGINA GREATER 1
                         MOVE     2   TO TELA
                         SUBTRACT 1 FROM PAGINA
                    ELSE
                         MOVE 1 TO TELA
                    END-IF
                END-IF
                PERFORM 110-LER-CWCONF THRU 110-99-FIM
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-LER-CWCONF.

           SET CWSQLC-UNLOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE TIPO-REG  TO CWCONF-REG99
                   MOVE PAGINA    TO CWCONF-PAGINA
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   SET CWSQLC-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM

           IF   FS-CWCONF = "23"
                INITIALIZE CWCONF-OPCOES
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE TIPO-REG TO CWCONF-REG99
                MOVE PAGINA   TO CWCONF-PAGINA
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   FS-CWCONF > "09"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           MOVE ZERO TO Y X-A

           IF   TELA = 1
                PERFORM 120-MONTA-TELA THRU 120-99-FIM
                        VARYING I FROM 1 BY 1 UNTIL I GREATER 13
           ELSE
                PERFORM 120-MONTA-TELA THRU 120-99-FIM
                        VARYING I FROM 14 BY 1 UNTIL I GREATER 26
           END-IF

           DISPLAY TELA-DATA.

       110-99-FIM. EXIT.

       120-MONTA-TELA.

           IF   CWCONF-PROG (I) NOT = SPACES
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        CWCONF-PROG       (I)
                CALL "CWCODE" USING "D" CWCONF-SIZE-S-99  (I)
                                        CWCONF-FATOR-S-99 (I)
                                        CWCONF-PASS       (I)
           END-IF

           ADD  1                TO Y
           MOVE CWCONF-OPCAO (I) TO TELA-OPCAO (Y).

       120-99-FIM. EXIT.

       130-EXECUTA-COMANDOS.

           MOVE COMANDO (I) TO TESTA-COMANDO

           EVALUATE TRUE
               WHEN NOT C-OK
                    ADD  1              TO Y
                    MOVE TELA-OPCAO (I) TO NOVA-LINHA (Y)
               WHEN COMANDO (I) = "A" OR "a"
                    ADD  1              TO Y
                    MOVE TELA-OPCAO (I) TO NOVA-LINHA (Y)
                    ADD  1              TO Y
               WHEN COMANDO (I) = "D" OR "d"
                    MOVE SPACE TO COMANDO (I)
               WHEN COMANDO (I) = "C" OR "c"
                    MOVE SPACE          TO COMANDO (I)
                    ADD  1              TO C
                                           Y
                    MOVE TELA-OPCAO (I) TO SALVA-LINHA (C)
                                           NOVA-LINHA  (Y)
               WHEN COMANDO (I) = '"'
                    MOVE SPACE          TO COMANDO (I)
                    ADD  1              TO Y
                    MOVE TELA-OPCAO (I) TO NOVA-LINHA  (Y)
                    ADD  1              TO Y
                    MOVE TELA-OPCAO (I) TO NOVA-LINHA  (Y)
               WHEN COMANDO (I) = "M" OR "m"
                    MOVE SPACE          TO COMANDO (I)
                    ADD  1              TO C
                    MOVE TELA-OPCAO (I) TO SALVA-LINHA (C)
               WHEN COMANDO (I) = "P" OR "p"
                    IF   C GREATER R
                         MOVE SPACE           TO COMANDO (I)
                         ADD  1               TO R Y
                         MOVE SALVA-LINHA (R) TO NOVA-LINHA (Y)
                         ADD  1               TO Y
                         MOVE TELA-OPCAO  (I) TO NOVA-LINHA (Y)
                    ELSE
                         ADD  1              TO Y
                         MOVE TELA-OPCAO (I) TO NOVA-LINHA    (Y)
                         MOVE COMANDO    (I) TO SALVA-COMANDO (Y)
                    END-IF
               WHEN COMANDO (I) = "F" OR "f"
                    IF   C GREATER R
                         MOVE SPACE           TO COMANDO (I)
                         ADD  1               TO Y
                         MOVE TELA-OPCAO  (I) TO NOVA-LINHA (Y)
                         ADD  1               TO R Y
                         MOVE SALVA-LINHA (R) TO NOVA-LINHA (Y)
                    ELSE
                         ADD  1              TO Y
                         MOVE TELA-OPCAO (I) TO NOVA-LINHA    (Y)
                         MOVE COMANDO    (I) TO SALVA-COMANDO (Y)
           END-EVALUATE.

       130-99-FIM. EXIT.

       140-SALVA-TELA.

           MOVE ZERO TO Y

           IF   TELA = 1
                PERFORM 150-KRIPTO THRU 150-99-FIM
                        VARYING I FROM 1 BY 1 UNTIL I GREATER 13
           ELSE
                PERFORM 150-KRIPTO THRU 150-99-FIM
                        VARYING I FROM 14 BY 1 UNTIL I GREATER 26
           END-IF

           MOVE ZERO TO OPCAO

           PERFORM VARYING I FROM 1 BY 1 UNTIL I GREATER 26
                   IF   CWCONF-PROG (I) = SPACES
                        MOVE ZERO   TO CWCONF-NO-OPCAO (I)
                        MOVE SPACES TO CWCONF-PASS (I)
                   ELSE
                        ADD  1     TO OPCAO
                        MOVE OPCAO TO CWCONF-NO-OPCAO (I)
                   END-IF
           END-PERFORM

           SET CWSQLC-REWRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE    TIPO-REG TO CWCONF-REG99
           MOVE    PAGINA   TO CWCONF-PAGINA
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       140-99-FIM. EXIT.

       150-KRIPTO.

           ADD  1              TO Y

           IF   TELA-NIVEL (Y) NOT NUMERIC
                MOVE 0 TO TELA-NIVEL (Y)
           END-IF

           MOVE TELA-OPCAO (Y) TO CWCONF-OPCAO (I)
           IF   CWCONF-CHECK (I) = "0"
                MOVE SPACE TO CWCONF-CHECK (I)
           END-IF
           IF   CWCONF-PROG (I) NOT = SPACES
                MOVE 8 TO CWCONF-SIZE-P-99 (I)
                CALL "CWCODE" USING "C" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        CWCONF-PROG (I)
                MOVE 6 TO CWCONF-SIZE-S-99 (I)
                CALL "CWCODE" USING "C" CWCONF-SIZE-S-99  (I)
                                        CWCONF-FATOR-S-99 (I)
                                        CWCONF-PASS (I)
           END-IF.

       150-99-FIM. EXIT.

       160-ACCEPT-TELA-DADOS.

           ACCEPT TELA-DATA
           ACCEPT TECLA FROM ESCAPE KEY
           IF   TIPO-REG = "SM"
                MOVE 0 TO ERRO
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 13
                        MOVE TELA-PROG (I) TO PROGRAMA
                        INSPECT PROGRAMA
                                CONVERTING MINUSCULAS TO MAIUSCULAS
                        IF   PROGRAMA = "CWMENU"
                        OR   TELA-PROG (I) (1: 1) = "?"
                             MOVE "?" TO TELA-PROG (I) (1: 1)
                             MOVE 1   TO ERRO
                        END-IF
                END-PERFORM
                IF   ERRO = 1
                     GO TO 160-ACCEPT-TELA-DADOS
                END-IF
           END-IF

           IF   F6
           OR   F2
                IF  X = 1
                    MOVE PAGINA TO SALVA-PAGINA
                    MOVE MODULO TO SALVA-MODULO
                    IF  F6
                        ACCEPT TELA-PAGINA
                    ELSE
                        ACCEPT TELA-MODULO
                    END-IF
                    IF  PAGINA NOT = SALVA-PAGINA
                    OR  MODULO NOT = SALVA-MODULO
                        PERFORM 140-SALVA-TELA THRU 140-99-FIM
                        PERFORM 110-LER-CWCONF THRU 110-99-FIM
                        IF  FS-CWCONF > "09"
                            MOVE SALVA-PAGINA TO PAGINA
                            PERFORM 110-LER-CWCONF THRU 110-99-FIM
                        END-IF
                    END-IF
                    CALL "CWMODE" USING "S" PAGINA MODULO TIPO-REG
                END-IF
           END-IF.

       160-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           MOVE 8                     TO CWBOXS-LINE
                                         CWBOXS-COLUMN
           MOVE "Tipo_de_manuten‡Æo:" TO CWBOXS-TITLE
           MOVE "~Exportar"           TO CWBOXS-TEXT (1)
           MOVE "~Importar"           TO CWBOXS-TEXT (2)
           MOVE "menu ~Principal"     TO CWBOXS-TEXT (3)
           MOVE "menu ~Secund rio"    TO CWBOXS-TEXT (4)
           MOVE "defini‡Æo de ~Jobs"  TO CWBOXS-TEXT (5)
           move 3                     TO CWBOXS-OPTION
           CALL "CWBOXS"           USING PARAMETROS-CWBOXS

           EVALUATE CWBOXS-OPTION
      *        WHEN 1 CALL "CWEXIM" USING "E" "  " LIXO
      *                     CANCEL "CWEXIM"
      *                     GOBACK
      *        WHEN 2 CALL "CWEXIM" USING "I" "  " LIXO
      *                     CANCEL "CWEXIM"
      *                     GOBACK
               WHEN 1 CALL "CWEXMN"
                            CANCEL "CWEXMN"
                            GOBACK
               WHEN 2 CALL "CWIMMN"
                            CANCEL "CWIMMN"
                            GOBACK
               WHEN 3 MOVE "99" TO TIPO-REG
               WHEN 4 MOVE "SM" TO TIPO-REG
               WHEN 5 CALL "CWMEND"
                            CANCEL "CWMEND"
                            GOBACK
               WHEN OTHER GOBACK
           END-EVALUATE

           DISPLAY 'CWLOCKNEW' UPON ENVIRONMENT-NAME
           ACCEPT     LOCKNEW  FROM ENVIRONMENT-VALUE
           INSPECT LOCKNEW
                   CONVERTING MINUSCULAS
                           TO MAIUSCULAS
           IF  LOCKNEW = 'ON'
               OPEN I-O VELHOS NOVOS GRUPOS
               SET CWSQLC-OPEN TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                           FS-CWCONF KCO PCO
                       IF FS-CWCONF < '10'
                       AND (CWCONF-TIPO = "99" OR "SM")
                            PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > 26
                            IF  CWCONF-PROG (I) NOT = SPACES
                               CALL "CWCODE" USING "D"
                                 CWCONF-SIZE-P-99  (I)
                                 CWCONF-FATOR-P-99 (I)
                                 CWCONF-PROG       (I)
                               INSPECT CWCONF-PROG (I)
                                       CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
                               IF (CWCONF-PROG(I) NOT = 'CWMENU')
                               AND(CWCONF-PROG(I) NOT = 'CWBOXS')
                                   MOVE CWCONF-PROG (I) TO VELHOS-CHAVE
                                   READ VELHOS
                                   IF FS-VELHOS = '23'
                                      WRITE VELHOS-REG
                                   END-IF
                               END-IF
                            END-IF
                       END-IF
               END-PERFORM
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
           END-IF
           CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
           DISPLAY "TERM" UPON ENVIRONMENT-NAME
           ACCEPT   TERM  FROM ENVIRONMENT-VALUE
           IF   TERM = "linux"
                MOVE "|"   TO XB3
                MOVE SPACE TO XBF
                MOVE "-"   TO XC4
           ELSE
               MOVE BASE-MOLDURA (2) (4: 1) TO XB3
               MOVE BASE-MOLDURA (2) (3: 1) TO XBF
               MOVE BASE-MOLDURA (2) (2: 1) TO XC4
           END-IF
           DISPLAY TELA-TXT
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF NOT = "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           MOVE "f10-~Prefixo" TO TELA-TEXTO
           PERFORM 110-LER-CWCONF THRU 110-99-FIM.

       800-99-FIM. EXIT.

       900-FINAIS.

           EXEC COBOLware OBJECT DROP END-EXEC
           PERFORM VARYING PAGINA FROM 1 BY 1 UNTIL FS-CWCONF > '09'
                                                OR PAGINA = 9999
                   MOVE '99'      TO CWCONF-REG99
                   MOVE PAGINA    TO CWCONF-PAGINA
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   SET CWSQLC-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
           END-PERFORM
           SUBTRACT 1 FROM PAGINA
           IF PAGINA = 9999 OR 1
              MOVE 1 TO PAGINA
           ELSE
              SUBTRACT 1 FROM PAGINA
              MOVE '00' TO FS-CWCONF
           END-IF
           PERFORM UNTIL PAGINA = 1
                     OR FS-CWCONF > '09'
              MOVE '99'      TO CWCONF-REG99
              MOVE PAGINA    TO CWCONF-PAGINA
              SET CWSQLC-READ TO TRUE
              SET CWSQLC-EQUAL TO TRUE
              SET CWSQLC-LOCK TO TRUE
              CALL "CWCONF" USING CWSQLC
                                  CWCONF-REG
                                  FS-CWCONF
                                  KCO PCO
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
                 IF (CWCONF-PROG     (I) NOT = SPACES)
                 OR (CWCONF-NM-OPCAO (I) NOT = SPACES)
                    MOVE 1 TO PAGINA
                    EXIT PERFORM
                 END-IF
              END-PERFORM
              IF I > 26
                 SET CWSQLC-DELETE TO TRUE
                 CALL "CWCONF" USING CWSQLC
                                     CWCONF-REG
                                     FS-CWCONF
                                     KCO PCO
                 SUBTRACT 1 FROM PAGINA
              END-IF
           END-PERFORM
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF  LOCKNEW = 'ON'
               SET CWSQLC-OPEN TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                           FS-CWCONF KCO PCO
                       IF FS-CWCONF < '10'
                       AND (CWCONF-TIPO = "99" OR "SM")
                            PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > 26
                            IF  CWCONF-PROG (I) NOT = SPACES
                               CALL "CWCODE" USING "D"
                                 CWCONF-SIZE-P-99  (I)
                                 CWCONF-FATOR-P-99 (I)
                                 CWCONF-PROG       (I)
                               INSPECT CWCONF-PROG (I)
                                       CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
                               IF (CWCONF-PROG(I) NOT = 'CWMENU')
                               AND(CWCONF-PROG(I) NOT = 'CWBOXS')
                                   MOVE CWCONF-PROG (I) TO VELHOS-CHAVE
                                   READ VELHOS
                                   IF FS-VELHOS = '23'
                                      MOVE VELHOS-CHAVE TO NOVOS-CHAVE
                                      READ NOVOS
                                      IF FS-NOVOS = '23'
                                         WRITE NOVOS-REG
                                      END-IF
                                   END-IF
                               END-IF
                            END-IF
                       END-IF
               END-PERFORM
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               MOVE 'GU' TO CWCONF-CHAVE
               SET CWSQLC-START TO TRUE
               SET CWSQLC-NOT-LESS TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               PERFORM UNTIL FS-CWCONF > '09'
                       SET CWSQLC-IGNORE-LOCK TO TRUE
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF CWCONF-TIPO NOT = 'GU'
                          EXIT PERFORM
                       ELSE
                          IF CWCONF-NOME-GRUPO NOT = GRUPOS-CHAVE
                             MOVE CWCONF-NOME-GRUPO TO GRUPOS-CHAVE
                             WRITE GRUPOS-REG
                          END-IF
                       END-IF
               END-PERFORM
               MOVE LOW-VALUES TO GRUPOS-CHAVE
               START GRUPOS KEY NOT LESS GRUPOS-CHAVE
               PERFORM UNTIL FS-GRUPOS > '09'
                       READ GRUPOS NEXT RECORD
                       IF FS-GRUPOS = '00'
                          MOVE LOW-VALUES TO NOVOS-CHAVE
                          START NOVOS KEY NOT LESS NOVOS-CHAVE
                          PERFORM UNTIL FS-NOVOS > '09'
                             READ NOVOS NEXT RECORD
                             IF FS-NOVOS < '10'
                                MOVE 'GU' TO CWCONF-REG
                                MOVE GRUPOS-CHAVE TO CWCONF-NOME-GRUPO
                                MOVE NOVOS-CHAVE  TO CWCONF-PROG-GRUPO
                                MOVE '1'          TO CWCONF-ACESSO-GRUPO
                                SET  CWSQLC-WRITE TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                             END-IF
                          END-PERFORM
                       END-IF
               END-PERFORM
               CLOSE VELHOS NOVOS GRUPOS
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
           END-IF.

       900-99-FIM. EXIT.

       DECLARA-RODAPE.

           IF   TIPO-REG = "SM"
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG01 FIELD PROG01
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG02 FIELD PROG02
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG03 FIELD PROG03
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG04 FIELD PROG04
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG05 FIELD PROG05
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG06 FIELD PROG06
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG07 FIELD PROG07
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG08 FIELD PROG08
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG09 FIELD PROG09
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG10 FIELD PROG10
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG11 FIELD PROG11
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG12 FIELD PROG12
                END-EXEC
                EXEC COBOLware Object validate
                     PROGRAM "CWMENJ" USING PROG13 FIELD PROG13
                END-EXEC
           END-IF

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 03 WIDTH 05
                     CAPTION "[esc]"
                     KEY ESC TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 09 WIDTH 04
                     CAPTION "pg~Up"
                     KEY PAGE-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 14 WIDTH 04
                     CAPTION "pg~Dn"
                     KEY PAGE-DOWN TAB-OFF
           END-EXEC
           IF   STATUS-TELA = 1
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 19 WIDTH 06
                          CAPTION "f3-~Mem"
                          KEY F3 TAB-OFF
                END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 26 WIDTH 08
                          CAPTION "f4-~Limpa"
                          KEY F4 TAB-OFF
                END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 35 WIDTH 08
                          CAPTION "f5-~Recup"
                          KEY F5 TAB-OFF
                END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 07 COLUMN 12 WIDTH 02
                          CAPTION "f~6"
                          KEY F6 TAB-OFF
                END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 07 COLUMN 53 WIDTH 02
                          CAPTION "f~2"
                          KEY F2 TAB-OFF
                END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 44 WIDTH 09
                          CAPTION "f8-~Aborta"
                          KEY F8 TAB-OFF
                END-EXEC
           ELSE
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 54 WIDTH 06
                          CAPTION "f9-~Cmd"
                          KEY F9 TAB-OFF
                END-EXEC
           END-IF
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 61 WIDTH 11
                     CAPTION TELA-TEXTO
                     KEY F10 TAB-OFF
           END-EXEC.

       FIM-DECLARA-RODAPE. EXIT.

       END PROGRAM CWMEN6.
