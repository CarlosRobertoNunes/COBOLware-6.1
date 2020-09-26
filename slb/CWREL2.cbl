      $Set CallFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL2 INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/06/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 2 - Classificacao e impressao         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAVEPROMPT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SAVEPROMPT-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-SAVEPROMPT.

           COPY CAMPOS.SEL.
           COPY FORMATOS.SEL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT SORTWK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SORTWK-CHAVE WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-SORTWK.

       DATA DIVISION.
       FILE SECTION.
       COPY CAMPOS.FD.
       COPY FORMATOS.FD.

       FD  SAVEPROMPT
           RECORD VARYING FROM 37 TO 9937 DEPENDING ON SZ-SAVEPROMPT
           VALUE OF FILE-ID IS LB-SAVEPROMPT.

       01  SAVEPROMPT-REG.
           05 SAVEPROMPT-CHAVE.
              10 SAVEPROMPT-RELATORIO      PIC X(07).
              10 SAVEPROMPT-USUARIO        PIC X(30).
           05 SAVEPROMPT-DADOS.
              10 SAVEPROMPT-100.
                 15 OCCURS 100.
                    20 SAVEPROMPT-DATANAME PIC X(09).
                    20 SAVEPROMPT-MENSAGEM PIC X(30).
                    20 SAVEPROMPT-COLUNA   PIC X(30).
              10 SAVEPROMPT-RESPOSTAS OCCURS 100.
                 15 SAVEPROMPT-CONTEUDO    PIC X(30).

       FD  SORTWK
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 256 TO 20254 DEPENDING ON SZ-SORTWK
           VALUE OF FILE-ID IS LB-SORTWK.

       01  SORTWK-REG.
           05 SORTWK-CHAVE.
              10 SORTWK-BYTE-CHAVE PIC X(256).
              10 REDEFINES SORTWK-BYTE-CHAVE.
                 15 SORTWK-BYTE-X  PIC 9(002) COMP-X OCCURS 256.
           05 SORTWK-REGISTRO.
              10 SORTWK-BYTE-REGISTRO PIC X(001) OCCURS 19998.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 DESTINO               PIC  X(050) VALUE SPACES.
           05 COMANDO               PIC  X(255) VALUE SPACES.
           05 ERRO                  PIC  9(001) VALUE 0.
           05 SL                    PIC  9(002) VALUE 0.
           05 REVERSOES      COMP-X PIC  9(002) VALUE 0 OCCURS 256.
           05 REV            COMP-X PIC  9(002) VALUE 0.
           05 REVW           COMP-X PIC  9(002) VALUE 0.
           05 REVPOS         COMP-X PIC  9(002) VALUE 0.
           05 TAGS                  PIC  9(004) VALUE 0.
           05 OPERACAO-MAX          PIC  9(004) VALUE 0.
           05 START-VALOR           PIC  X(030) VALUE SPACES.
           05 OPT                   PIC  X(004) VALUE SPACES.
           05 OP                    PIC  9(004) VALUE 0.
           05 OP2                   PIC  9(004) VALUE 0.
           05 OP3                   PIC  9(004) VALUE 0.
           05 OP-ACM                PIC  9(004) VALUE 0.
           05 OP-POSIT              PIC  9(004) VALUE 0.
           05 OP-TAMANHO            PIC  9(003) VALUE 0.
           05 OP-DEC                PIC  9(002) VALUE 0.
           05 RESULTADO-DEC         PIC  9(002) VALUE 0.
           05 OPERANDO-1            PIC S9(018) VALUE 0.
           05 OPERANDO-2            PIC S9(018) VALUE 0.
           05 OPERANDO-3            PIC S9(018) VALUE 0.
           05 OPERANDO-4 REDEFINES OPERANDO-3
                                    PIC S9(012)V9(06).
           05 OPERANDO-5 REDEFINES OPERANDO-3
                                    PIC S9(006)V9(12).
           05 DEC-1                 PIC  9(002) VALUE 0.
           05 DEC-2                 PIC  9(002) VALUE 0.
           05 DEC-D                 PIC  9(002) VALUE 0.
           05 BY10                  PIC  9(002) VALUE 0.
           05 DEC10                 PIC  9(002) VALUE 0.
           05 JAN-28                PIC  9(002) VALUE 28.
           05 RELYEAR               PIC  X(002) VALUE SPACES.
           05 COL-W                 PIC S9(003) VALUE 0.
           05 CORES                 PIC  X(256)        VALUE SPACES.
           05 MOLDURAS              PIC  X(072)        VALUE SPACES.
           05 X91-RESULT            PIC  9(002) COMP-X VALUE 0.
           05 X91-F15               PIC  9(002) COMP-X VALUE 15.
           05 X91-F16               PIC  9(002) COMP-X VALUE 16.
           05 X91-PARAMETER         PIC  9(002) COMP-X VALUE 0.
           05 DATA-A                PIC  X(010) VALUE SPACES.
           05 DATA-D                PIC  X(010) VALUE SPACES.
           05 TPI                   PIC  9(001) VALUE 0.
           05 TPC                   PIC  9(002) VALUE 0.
           05 DX                    PIC  9(002) VALUE 0.
           05 PGMR                  PIC  X(008) VALUE SPACES.
           05 SALVA-DATA            PIC  X(010) VALUE SPACES.
           05 DATA-L                PIC  9(008) VALUE 0.
           05 DECORRIDOS            PIC  9(002) VALUE 0.
           05 TEXTO-999             PIC  X(999) VALUE SPACES.
           05 MINUSCULAS            PIC  X(026) VALUE
              "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS            PIC  X(026) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ACENTOS               PIC  X(036) VALUE
              "†Ç°¢£ÖäçïóÑîÅÉàìáÜêãüñëíò©ùéôöèâåÄ‰∆".
           05 SEM-ACENTOS           PIC  X(036) VALUE
              "AEIOUAEIOUAOUAEOCAEIOUAEIOUAOUAEOCOA".
           05 COND                  PIC  X(001) VALUE SPACE.
           05 STATUS-LICENCA        PIC  X(001) VALUE SPACE.
           05 LICENCA               PIC  9(009) VALUE 0.
           05 REDEFINES LICENCA.
              10 LICENCA-SERIECW    PIC  9(004).
              10 LICENCA-KEY        PIC  9(005).
           05 KEY-STATUS            PIC  9(002) COMP-X VALUE 0.
           05 CARACTER              PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                    PIC  9(002) COMP-X.
           05 RELATORIO             PIC  X(007) VALUE SPACES.
           05 LIMITE-IN             PIC  9(009)        VALUE 0.
           05 LIMITE-OUT            PIC  9(009)        VALUE 0.
           05 PROCESSADOS           PIC  9(009)        VALUE 0.
           05 SELECIONADOS          PIC  9(009) COMP-3 VALUE 0.
           05 LISTADOS              PIC  9(009) COMP-3 VALUE 0.
           05 PROCESSADOS-ED        PIC  ZZZ.ZZZ.ZZ9 VALUE 0.
           05 SELECIONADOS-ED       PIC  ZZZ.ZZZ.ZZ9 VALUE 0.
           05 LISTADOS-ED           PIC  ZZZ.ZZZ.ZZ9 VALUE 0.
           05 ASC                   PIC  9(002) COMP-X.
           05 TIPO                  PIC  X(001) VALUE SPACE.
           05 DESLOCAR              PIC S9(003) VALUE 0.
           05 LIN-SIZE              PIC  9(003) VALUE 0.
           05 MAX-SIZE              PIC  9(003) VALUE 0.
           05 RELDIR                PIC  X(050) VALUE SPACES.
           05 DATANAME              PIC  X(030) VALUE SPACES.
      *    05 USUARIO               PIC  X(030) VALUE SPACES.
      *    05 REDEFINES USUARIO.
      *       10 U OCCURS 30        PIC  9(002) COMP-X.
           05 PADRAO                PIC  9(001) VALUE 0.
              88 COBOLware                      VALUE 1.
              88 LIVRE                          VALUE 2.
              88 ARQUIVO                        VALUE 3.
           05 LINHA                 PIC  9(004) VALUE 9997.
           05 DATA-WS               PIC  X(030) VALUE SPACES.
           05 ANO-WS                PIC  9(002) VALUE 0.
           05 ANO-J                 PIC  9(002) VALUE 0.
           05 PA                    PIC  9(002) VALUE 0.
           05 PM                    PIC  9(002) VALUE 0.
           05 PD                    PIC  9(002) VALUE 0.
           05 DM                    PIC  9(002) VALUE 0.
           05 PX                    PIC  9(004) VALUE 0.
           05 MASCARA               PIC  X(030) VALUE SPACES.
           05 COM-INCLUSAO          PIC  9(001) VALUE 0.
           05 ACAO                  PIC  X(001) VALUE SPACE.
           05 CONDICAO              PIC  X(002) VALUE SPACES.
              88 IGUAL                          VALUE "=" ">=" "<=".
              88 MENOR                          VALUE "<" "<=" "<>".
              88 MAIOR                          VALUE ">" ">=" "<>".
           05 N-14                  PIC  9(014) VALUE ZERO.
           05 N-11                  PIC  9(011) VALUE ZERO.
           05 CGC-CIC        COMP-3 PIC  9(014) VALUE ZERO.
           05 CGC-CIC-ED            PIC  X(018) VALUE SPACES.
           05 FLAG-ACM              PIC  X(001) VALUE SPACE.
           05 DT4                   PIC  9(004) VALUE 0.
           05 DT6                   PIC  9(006) VALUE 0.
           05 DT8                   PIC  9(008) VALUE 0.
           05 OBJETO-V              PIC  9(004) COMP-X VALUE 0.
           05 ELEMENTO              PIC  X(500) VALUE SPACES.
           05 DADOS                 PIC  X(500) VALUE SPACES.
           05 DEC                   PIC S9(018) VALUE 0.
           05 OK                    PIC  9(001) VALUE 0.
           05 SELECIONADO           PIC  9(001) VALUE 0.
           05 I                     PIC  9(004) VALUE 0.
           05 V                     PIC  9(004) VALUE 0.
           05 Q                     PIC  9(004) VALUE 0.
           05 QX                    PIC  9(001) VALUE 0.
           05 I2                    PIC  9(004) VALUE 0.
           05 Y                     PIC  9(004) VALUE 0.
           05 YY                    PIC  9(004) VALUE 0.
           05 Z                     PIC  9(004) VALUE 0.
           05 Y2                    PIC  9(004) VALUE 0.
           05 Y3                    PIC S9(004) VALUE 0.
           05 K                     PIC  9(004) VALUE 0.
           05 K2                    PIC  9(004) VALUE 0.
           05 P                     PIC  9(004) VALUE 0.
           05 D                     PIC  9(004) VALUE 0.
           05 A                     PIC  9(004) VALUE 0.
           05 ACM                   PIC  9(004) VALUE 0.
           05 E                     PIC  9(004) VALUE 0.
           05 M                     PIC  9(004) VALUE 0.
           05 M1                    PIC  9(004) VALUE 0.
           05 M2                    PIC  9(004) VALUE 0.
           05 M3                    PIC  9(004) VALUE 0.
           05 M4                    PIC  9(004) VALUE 0.
           05 M5                    PIC  9(004) VALUE 0.
           05 M6                    PIC  9(004) VALUE 0.
           05 L                     PIC  9(003) VALUE 0.
           05 C                     PIC  9(003) VALUE 0.
           05 R                     PIC  9(003) VALUE 1.
           05 RS                    PIC  9(004) COMP-X VALUE 0.
           05 S                     PIC  9(003) VALUE 0.
           05 S2                    PIC  9(003) VALUE 0.
           05 T                     PIC  9(003) VALUE 0.
           05 TZ                    PIC  9(003) VALUE 0.
           05 M-I                   PIC  9(004) VALUE 0.
           05 M-T                   PIC  9(004) VALUE 0.
           05 M-I2                  PIC  9(004) VALUE 0.
           05 TECLA                 PIC  9(002) VALUE 0. COPY CWKEYS.
           05 TECLA2                PIC  9(003) VALUE 0. COPY CWEDIT.
           05 LIN-W                 PIC  9(002) VALUE 0.
           05 LIN-P                 PIC  9(002) VALUE 0.
           05 LIN-P2                PIC  9(002) VALUE 0.
           05 COL-P                 PIC  9(002) VALUE 0.
           05 COL-P2                PIC  9(002) VALUE 0.
           05 SZ-FORMATOS           PIC  9(003) VALUE 0.
           05 SZ-SAVEPROMPT         PIC  9(004) VALUE 0.
           05 OBJETO-L              PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-R              PIC  9(004) COMP-X VALUE 0.
           05 OBJETO                PIC  9(004) COMP-X VALUE 0.
           05 RESULTADO             PIC  9(004) COMP-X VALUE 0.
           05 PROVEDOR              PIC  X(008) VALUE SPACES.
           05 PROVEDOR-C            PIC  X(008) VALUE SPACES.
           05 SZ-SORTWK             PIC S9(004) COMP-5 VALUE 256.
           05 OPERADOR              PIC  X(030) VALUE SPACES.
           05 TASK                  PIC  9(006) VALUE 0.
           05 PROGRAMA              PIC  X(008) VALUE SPACES.
           05 ER-TEXTO.
              10 FS-TEXTO           PIC  X(002) VALUE "00".
              10 LB-TEXTO           PIC  X(255) VALUE SPACES.
           05 ER-SORTWK.
              10 FS-SORTWK          PIC  X(002) VALUE "00".
              10 LB-SORTWK          PIC  X(255) VALUE SPACES.
           05 ER-FORMATOS.
              10 FS-FORMATOS        PIC  X(002) VALUE "00".
              10 LB-FORMATOS        PIC  X(255) VALUE SPACES.
           05 ER-SAVEPROMPT.
              10 FS-SAVEPROMPT      PIC  X(002) VALUE "00".
              10 LB-SAVEPROMPT      PIC  X(255) VALUE SPACES.
           05 ER-CAMPOS.
              10 FS-CAMPOS          PIC  X(002) VALUE "00".
              10 LB-CAMPOS          PIC  X(255) VALUE SPACES.
           05 TMP                   PIC  X(050) VALUE SPACES.
           05 TMP-LB                PIC  X(012) VALUE "CW700000.TMP".
           05 CAB                   PIC  9(003) VALUE 0.
           05 CABX                  PIC  9(001) VALUE 0.
           05 CABP                  PIC  9(001) VALUE 0.
           05 DET                   PIC  9(003) VALUE 0.
           05 DET-NULL              PIC  9(003) VALUE 0.
           05 TOT                   PIC  9(003) VALUE 0.
           05 SIZE-PAGE      COMP-X PIC  9(002) VALUE 0.
           05 ALPHA                 PIC  X(500) VALUE SPACES.
           05 ALPHA-2               PIC  X(500) VALUE SPACES.
           05 NUMERICO              PIC S9(018) VALUE 0.
           05 NUMERICO-2            PIC S9(018) VALUE 0.
           05 NUMERICO-M            PIC  9(018) VALUE 0.
           05 NUMERICO-I            PIC  9(018) VALUE 0.
           05 VARIAVEIS-RELATOR.
              10 RELATOR-PAGINA     PIC  9(009) VALUE 0.
              10 RELATOR-FOLHA      PIC  9(009) VALUE 0.
              10 RELATOR-DATA       PIC  9(008) VALUE 0.
              10 RELATOR-HORA       PIC  9(006) VALUE 0.
              10 RELATOR-LINHA      PIC  9(009) VALUE 0.
              10 RELATOR-EMPRESA    PIC  X(030) VALUE SPACES.
              10 RELATOR-SISTEMA    PIC  X(030) VALUE SPACES.
              10 RELATOR-USUARIO    PIC  X(030) VALUE SPACES.
              10 RELATOR-E-MAIL     PIC  X(042) VALUE SPACES.
              10 RELATOR-DIA-SEMANA PIC  X(007) VALUE SPACES.
              10 RELATOR-CODIGO     PIC  X(007) VALUE SPACES.
              10 RELATOR-DESCRICAO  PIC  X(030) VALUE SPACES.
           05 FILLER VALUE SPACES.
              10 OCCURS 100.
                 15 PROMPT-TIPO     PIC  X(01).
                 15 PROMPT-TAMANHO  PIC  9(02) COMP-X.
                 15 PROMPT-DEC      PIC  9(02) COMP-X.
                 15 PROMPT-MASCARA  PIC  X(08).
                 15 SALTO           PIC  X(01).
           05 ATRIBUTOS.
              10                    PIC  X(009) VALUE '6/"'.
              10                    PIC  X(009) VALUE '8/"'.
              10                    PIC  X(009) VALUE "Azul".
              10                    PIC  X(009) VALUE "Carta".
              10                    PIC  X(009) VALUE "Condensar".
              10                    PIC  X(009) VALUE "Draft".
              10                    PIC  X(009) VALUE "Expandir".
              10                    PIC  X(009) VALUE "Italico".
              10                    PIC  X(009) VALUE "Negrito".
              10                    PIC  X(009) VALUE "Normal".
              10                    PIC  X(009) VALUE "Paisagem".
              10                    PIC  X(009) VALUE "Reset".
              10                    PIC  X(009) VALUE "Salto".
              10                    PIC  X(009) VALUE "Sublinhar".
              10                    PIC  X(009) VALUE "Verde".
              10                    PIC  X(009) VALUE "Vermelho".
           05 REDEFINES ATRIBUTOS.
              10 ATRIBUTO           PIC  X(009) OCCURS 16.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER      PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER   PIC  9(002) COMP-X VALUE 1.
              10 TEXTO-78           PIC  X(200) VALUE SPACES.
           05 RELCONT               PIC  X(001) VALUE "1".
           05 RESPOSTA              PIC  X(001) VALUE SPACE.
              88 PARAR      VALUE "Y" "y".

       01  MAPA-STATUS-LINHA VALUE SPACES.
           05 STATUS-LINHA OCCURS 100 PIC X.
           05 MAPA-FLAGS.
              10 FLAG      OCCURS 100 PIC X.

       01  MAPA-QUEBRAS VALUE ALL "0".
           05 NIVEL-QUEBRA  OCCURS 99 PIC 9.
           05 NIVEL-MAX               PIC 9.
           05 NIVEL-TAMANHO OCCURS  7 PIC 9(003).

       01  MAPA VALUE LOW-VALUES.
           05 MAPA-LIN OCCURS 100.
              10 CX OCCURS 500 PIC X.

       01  POSICOES-OBJETOS-R VALUE LOW-VALUES.
           05 OBJETO-ATTR
              OCCURS 1 TO 22000 DEPENDING ON OBJETO-R INDEXED BY ORX.
              10 OBJETO-RL        PIC 9(2) COMP-X.
              10 OBJETO-RC        PIC 9(2) COMP-X.
              10 OBJETO-TAMANHO   PIC 9(2) COMP-X.
              10 OBJETO-DEC       PIC 9(2) COMP-X.
              10 OBJETO-TIPO      PIC X(1).
                 88 OBJETO-RESULTADO VALUE "+" "-" "*" "/" "P" "R".
              10 OBJETO-DATANAME  PIC X(30).
              10 OBJETO-DATANAME2 PIC X(30).
              10 OBJETO-OUT       PIC 9(4) COMP-X.
              10 OBJETO-PROMPT    PIC 9(4) COMP-X.
              10 OBJETO-ACM       PIC 9(4) COMP-X.

       01  TAG-REG.
           10 TAG-FROM    PIC  X(030).
           10 TAG-NAME    PIC  X(030).
           10 TAG-TIPO    PIC  X(001).
              88 TAG-VALOR               VALUE "V" "v".
              88 TAG-NUMERICO            VALUE "9" "E" "e" "Z" "z"
                                               "V" "v" "C" "c".
           10 TAG-LEN     PIC  9(003).
           10 TAG-DEC     PIC  9(002).

       01  POSICOES-MOVE VALUE LOW-VALUES.
           05 MV-OBJETOS        PIC 9(4) COMP-X.
           05 MV-LAST           PIC 9(4) COMP-X.
           05 MV-ATTR OCCURS 1 TO 3000 DEPENDING ON MV-OBJETOS.
              10 MV-COLUNA      PIC 9(4) COMP-X.
              10 MV-POSIT       PIC 9(4) COMP-X.
              10 MV-TAMANHO     PIC 9(2) COMP-X.
              10 MV-DEC         PIC 9(2) COMP-X.
              10 MV-POSIT-OUT   PIC 9(4) COMP-X.
              10 MV-TIPO        PIC X.
              10 MV-ACM         PIC 9(2) COMP-X.
              10 MV-MASCARA     PIC X(30).

       01  PROMPTS VALUE SPACES.
           05 PROMPT-100.
              07 PROMPT-ATRIBUTOS OCCURS 100.
                 10 PROMPT-DATANAME PIC X(09).
                 10 PROMPT-MENSAGEM PIC X(30).
                 10 PROMPT-COLUNA   PIC X(30).
           05 PROMPT-RESPOSTAS OCCURS 100.
              10 PROMPT-CONTEUDO    PIC X(30).

       01  CONDICOES VALUE LOW-VALUES.
           05 CONDICAO-QTDE          PIC 9(02) COMP-X.
           05 CONDICAO-ATRIBUTOS OCCURS 1 TO 100
                                 DEPENDING ON CONDICAO-QTDE.
              10 CONDICAO-ACAO       PIC X(01).
              10 CONDICAO-DATANAME   PIC X(30).
              10 REDEFINES CONDICAO-DATANAME.
                 15 CONDICAO-POSIT   PIC 9(04) COMP-X.
                 15 CONDICAO-POSIT2  PIC 9(04) COMP-X.
                 15 CONDICAO-PROMPT  PIC 9(02) COMP-X.
                 15 CONDICAO-PROMPT2 PIC 9(02) COMP-X.
                 15 CONDICAO-SIZE    PIC 9(02) COMP-X.
                 15 CONDICAO-SIZE2   PIC 9(02) COMP-X.
                 15 CONDICAO-J1      PIC X(01).
                 15 CONDICAO-J2      PIC X(01).
                 15 CONDICAO-M1      PIC X(10).
                 15 CONDICAO-M2      PIC X(10).
              10 CONDICAO-CONDICAO   PIC X(02).
              10 CONDICAO-OPERANDO   PIC X(30).
              10 CONDICAO-LITERAL    PIC X(01).
              10 CONDICAO-TIPO       PIC X(01).
              10 CONDICAO-DATANAME2  PIC X(30).

       01  OPERACOES VALUE SPACES.
           05 OPERACAO-ATRIBUTOS OCCURS 100.
              10 OPERACAO-DATANAME  PIC  X(09).
              10 OPERACAO-INT       PIC  9(02).
              10 OPERACAO-DEC       PIC  9(01).
              10 OPERACAO-OPERANDO1 PIC  X(30).
              10 OPERACAO-LIT1  REDEFINES OPERACAO-OPERANDO1
                                    PIC S9(12)V9(06).
              10 OPERACAO-OP-1LIT   PIC  X(01).
              10 OPERACAO-OPERADOR  PIC  X(01).
              10 OPERACAO-OPERANDO2 PIC  X(30).
              10 OPERACAO-LIT2 REDEFINES OPERACAO-OPERANDO2
                                    PIC S9(12)V9(06).
              10 OPERACAO-OP-2LIT   PIC  X(01).
              10 OPERACAO-COLUNA    PIC  9(04).
              10 OPERACAO-MEMORIA   PIC S9(12)V9(06) COMP-3.
              10 OPERACAO-INTEIRO REDEFINES OPERACAO-MEMORIA
                                    PIC S9(18) COMP-3.
           05 OPERACAO-WORK         PIC  X(18).
           05 OPERACAO-1            PIC S9(12)V9(06) COMP-3.
           05 OPERACAO-1I REDEFINES OPERACAO-1
                                    PIC S9(18)       COMP-3.
           05 OPERACAO-2            PIC S9(12)V9(06) COMP-3.
           05 OPERACAO-2I REDEFINES OPERACAO-2
                                    PIC S9(18)       COMP-3.
           05 OPERACAO-OPERANDO     PIC  X(30).
           05 OPERACAO-LIT REDEFINES OPERACAO-OPERANDO
                                    PIC S9(12)V9(06).
           05 OPERACAO-CHECK        PIC  X(01).

      *01  CONTROL-COLORS            PIC X(38) VALUE
      *    "BACKGROUND-COLOR X,FOREGROUND-COLOR X".
      *01  REDEFINES CONTROL-COLORS.
      *    10                        PIC X(017).
      *    10 B                      PIC 9(001).
      *    10                        PIC X(018).
      *    10 F                      PIC 9(001).

       COPY CWSEND.
       COPY CWACOR.
       COPY CWTIME.
       COPY CWIMPR.
       COPY CWBOXW.
       COPY CWBOXF.
       COPY CWUNIX.
       COPY CWCONF.
       COPY CWSAVE.
       COPY CWPATH.
       COPY CWEXEC.
       COPY RELATOR.
           05 ANTERIOR       PIC X(10254).
           05 ACUMULADORES.
              10 RELATOR-QUEBRA PIC 9(09) COMP-3 OCCURS 7.
              10 NIVEL OCCURS 8.
                 15 RELATOR-SELECIONADOS PIC  9(09) COMP-3.
                 15 ACUMULADOR           PIC S9(18) COMP-3 OCCURS 100.
                 15 ACUMULADOR-FLAG      PIC  X(01)        OCCURS 100.

       LINKAGE SECTION.

       01   LK-RELATORIO PIC X(007).
       01   COMMAREA     PIC X(2000).

       SCREEN SECTION.

       01  DATAS AUTO.
       02  FOREGROUND-COLOR CWACOR-F (09)
           BACKGROUND-COLOR CWACOR-B (09).
           05 DT-4.
              15 LINE LIN-P COLUMN COL-P PIC 99/99 USING DT4 BLANK ZERO.
              15 LINE LIN-P COLUMN PLUS 2 VALUE "[DD/MM]".
           05 DT-6.
              15 LINE LIN-P COLUMN COL-P PIC 99/9999 USING DT6
                 BLANK ZERO.
              15 LINE LIN-P COLUMN PLUS 2 VALUE "[MM/AAAA]".
           05 DT-8.
              15 LINE LIN-P COLUMN COL-P PIC 99/99/9999 USING DT8
                 BLANK ZERO.
              15 LINE LIN-P COLUMN PLUS 2 VALUE "[DD/MM/AAAA]".

       01  TELA-TPI.
           04 FOREGROUND-COLOR CWACOR-F (11)
              BACKGROUND-COLOR CWACOR-B (11).
           05 LINE 23 COLUMN TPC PIC X FROM PGMR (TPI: 1).

       01  TELA-TPC.
           04 FOREGROUND-COLOR CWACOR-F (10)
              BACKGROUND-COLOR CWACOR-B (10).
           05 LINE 23 COLUMN TPC PIC X FROM SPACE.

       PROCEDURE DIVISION USING LK-RELATORIO COMMAREA.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           IF  NOT CWUNIX-GUI
               CALL "CWMSGW" USING "235416" "[esc]-Interrompe"
           END-IF

           PERFORM UNTIL RELATOR-FIM
                      OR CWIMPR-END-PRINT
                   CALL "CWATCH"
                   IF   CWUNIX-GUI
                        EXEC COBOLware PROCESS (SHOW)
                             HEADER CWIMPR-TITLE
                             LINE 16 COLUMN 12
                             MESSAGE "Gerando relat¢rio"
                           QUESTION "Deseja interromper o relat¢rio ? "
                             CANCEL RESPOSTA;RESPOSTA
                        END-EXEC
                   END-IF
                   IF  PARAR
                       SET RELATOR-FIM TO TRUE
                       SET CWIMPR-END-PRINT TO TRUE
                   ELSE
                       IF PROVEDOR-C = SPACES
                          MOVE PROVEDOR TO PROVEDOR-C
                          CALL PROVEDOR-C USING PARAMETROS-RELATOR
                                                COMMAREA
                                                PARAMETROS-CWIMPR
                            ON EXCEPTION
                               INSPECT PROVEDOR-C CONVERTING MAIUSCULAS
                                                          TO MINUSCULAS
                               CALL PROVEDOR-C USING PARAMETROS-RELATOR
                                                     COMMAREA
                                                     PARAMETROS-CWIMPR
                               ON EXCEPTION
                                  STRING "Provedor " DELIMITED BY SIZE
                                         PROVEDOR DELIMITED BY SPACE
                                         " n∆o encontrado"
                                         DELIMITED BY SIZE
                                         INTO CWSEND-MSG
                                  CALL "CWSEND" USING PARAMETROS-CWSEND
                                  CLOSE FORMATOS CAMPOS SORTWK
                                  GOBACK
                               END-CALL
                          END-CALL
                       ELSE
                          CALL PROVEDOR-C USING PARAMETROS-RELATOR
                                                COMMAREA
                                                PARAMETROS-CWIMPR
                       END-IF
                       IF   NOT CWUNIX-GUI
                            PERFORM 115-CHECK-TECLA THRU 115-99-FIM
                       END-IF
                   END-IF
                   IF   LIMITE-IN NOT = 0
                   AND  PROCESSADOS = LIMITE-IN
                        SET RELATOR-FIM TO TRUE
                   END-IF
                   IF   NOT RELATOR-FIM
                        ADD  1           TO PROCESSADOS
                        IF  PROCESSADOS = 1
                            CALL "CBL_SET_CSR_POS" USING X"FFFF"
                        END-IF
                        MOVE PROCESSADOS TO PROCESSADOS-ED
                        IF   RELCONT = "1"
                        AND  (NOT CWUNIX-GUI)
                             CALL "CBL_WRITE_SCR_CHARS" USING X"0B19"
                                                        PROCESSADOS-ED
                                                        X"000B"
                        END-IF
                        PERFORM 110-RELEASE THRU 110-99-FIM
                   END-IF
           END-PERFORM
           INITIALIZE ACUMULADORES
           MOVE LOW-VALUES TO SORTWK-CHAVE
           START SORTWK KEY NOT LESS SORTWK-CHAVE
           PERFORM UNTIL FS-SORTWK > "09"
                      OR CWIMPR-END-PRINT
                      OR PARAR
                   CALL "CWATCH"
                   READ SORTWK NEXT RECORD
                        NOT AT END
                            PERFORM VARYING REVPOS FROM 1 BY 1
                                      UNTIL REVPOS > REV
                                    MOVE REVERSOES (REVPOS) TO REVW
                                    COMPUTE SORTWK-BYTE-X (REVW) = 255
                                          - SORTWK-BYTE-X (REVW)
                            END-PERFORM
                            ADD  1        TO LISTADOS
                            IF  LISTADOS = 1
                            AND SIZE-PAGE = 0
                            AND (NOT ARQUIVO)
                                PERFORM 131-CABECALHO THRU 131-99-FIM
                            END-IF
                            MOVE LISTADOS TO LISTADOS-ED
                            IF   RELCONT = "1"
                            AND  (NOT CWUNIX-GUI)
                                 CALL "CBL_WRITE_SCR_CHARS"
                                       USING X"0F19" LISTADOS-ED X"000B"
                            END-IF
                            PERFORM 130-IMPRESSAO THRU 130-99-FIM
                            MOVE    SORTWK-REG      TO ANTERIOR
                   END-READ
                   IF   LIMITE-OUT NOT = 0
                   AND  LISTADOS = LIMITE-OUT
                        MOVE "10" TO FS-SORTWK
                   END-IF
           END-PERFORM

           IF   SELECIONADOS = 0
                PERFORM 131-CABECALHO THRU 131-99-FIM
           END-IF

           MOVE    1            TO Q
           PERFORM 140-QUEBRA THRU 140-99-FIM
                   VARYING I FROM NIVEL-MAX BY -1
                     UNTIL I = 0
           MOVE    8            TO I
           MOVE    4            TO Q
           PERFORM 140-QUEBRA THRU 140-99-FIM.

       100-99-FIM. EXIT.

       101-LER-PRONAMREL.

           MOVE PROVEDOR          TO CAMPOS-PROVEDOR
           IF   CAMPOS-DATANAME (1: 3) = "(=)"
                MOVE RELATORIO TO CAMPOS-RELATORIO
                PERFORM VARYING OP3 FROM 1 BY 1
                          UNTIL OP3 > 12
                             OR CAMPOS-DATANAME (OP3:3) = "[+]"
                           CONTINUE
                END-PERFORM
                IF   OP3 > 12
                AND (CAMPOS-DATANAME (OP3:3) NOT = "[+]")
                     MOVE 0 TO OP-ACM
                ELSE
                     MOVE 1      TO OP-ACM
                     MOVE SPACES TO CAMPOS-DATANAME (OP3:3)
                END-IF
                PERFORM VARYING OP3 FROM 1 BY 1
                          UNTIL OP3 > OPERACAO-MAX
                        OR CAMPOS-DATANAME (4:) = OPERACAO-DATANAME(OP3)
                           CONTINUE
                END-PERFORM
                IF   OP3 > OPERACAO-MAX
                     MOVE "23" TO FS-CAMPOS
                ELSE
                     MOVE "00"                    TO FS-CAMPOS
                     MOVE OP3                     TO CAMPOS-POSIT
                     COMPUTE CAMPOS-TAMANHO = OPERACAO-INT  (OP3)
                                            + OPERACAO-DEC  (OP3)
                     MOVE OPERACAO-DEC      (OP3) TO CAMPOS-DEC
                     MOVE "V"                     TO CAMPOS-TIPO
                     MOVE PROVEDOR                TO CAMPOS-PROVEDOR
                     MOVE RELATORIO               TO CAMPOS-RELATORIO
                     MOVE "(=)"                   TO CAMPOS-DATANAME
                     MOVE OPERACAO-COLUNA   (OP3) TO CAMPOS-COLUNA
                     MOVE OPERACAO-DATANAME (OP3) TO CAMPOS-DATANAME
                                                     (4: )
                     MOVE SPACE                   TO CAMPOS-ACUMULADOR
                     IF   OP-ACM = 1
                          MOVE   CAMPOS-DATANAME TO DATANAME
                          MOVE   SPACES TO CAMPOS-DATANAME
                          STRING DATANAME DELIMITED BY SPACE
                                    "[+]" DELIMITED BY SIZE
                                     INTO CAMPOS-DATANAME
                          MOVE 0     TO CAMPOS-POSIT
                          MOVE "+"   TO CAMPOS-ACUMULADOR
                     END-IF
                END-IF
            ELSE
                MOVE SPACES TO CAMPOS-RELATORIO
                READ CAMPOS KEY IS CAMPOS-PRONAMREL
            END-IF.

       101-99-FIM. EXIT.

       110-RELEASE.

           PERFORM 111-EFETUA-OPERACOES THRU 111-99-FIM
                   VARYING M FROM 1 BY 1 UNTIL M > OPERACAO-MAX

           IF   CONDICAO-QTDE = 0
                MOVE 1 TO SELECIONADO
           ELSE
                IF   COM-INCLUSAO = 0
                     MOVE 1 TO SELECIONADO
                ELSE
                     MOVE "I" TO ACAO
                     MOVE 0   TO SELECIONADO OK
                     PERFORM 120-CHECK-CONDICAO THRU 120-99-FIM
                             VARYING M FROM 1 BY 1
                               UNTIL M > CONDICAO-QTDE
                                  OR OK = 1
                     IF   OK = 1
                          MOVE 1 TO SELECIONADO
                     END-IF
                END-IF
                MOVE 0   TO OK
                MOVE "E" TO ACAO
                PERFORM 120-CHECK-CONDICAO THRU 120-99-FIM
                        VARYING M FROM 1 BY 1
                          UNTIL M > CONDICAO-QTDE
                             OR OK = 1
                IF   OK = 1
                     MOVE 0 TO SELECIONADO
                END-IF
           END-IF

           IF   SELECIONADO = 1
                MOVE SPACES TO SORTWK-REG
                PERFORM VARYING M FROM 1 BY 1 UNTIL M > MV-OBJETOS
                IF  MV-TIPO (M) NOT = 'X'
                    COMPUTE Y = MV-POSIT     (M)
                              + MV-TAMANHO   (M) - 1
                    EVALUATE RELATOR-ESTRUTURA (Y: 1)
0-                      WHEN X"7D" MOVE X"70" to RELATOR-ESTRUTURA(Y:1)
1-                      WHEN X"4A" MOVE X"71" to RELATOR-ESTRUTURA(Y:1)
2-                      WHEN X"4B" MOVE X"72" to RELATOR-ESTRUTURA(Y:1)
3-                      WHEN X"4C" MOVE X"73" to RELATOR-ESTRUTURA(Y:1)
4-                      WHEN X"4D" MOVE X"74" to RELATOR-ESTRUTURA(Y:1)
5-                      WHEN X"4E" MOVE X"75" to RELATOR-ESTRUTURA(Y:1)
6-                      WHEN X"4F" MOVE X"76" to RELATOR-ESTRUTURA(Y:1)
7-                      WHEN X"50" MOVE X"77" to RELATOR-ESTRUTURA(Y:1)
8-                      WHEN X"51" MOVE X"78" to RELATOR-ESTRUTURA(Y:1)
9-                      WHEN X"52" MOVE X"79" to RELATOR-ESTRUTURA(Y:1)
0                       WHEN X"7B" MOVE X"30" to RELATOR-ESTRUTURA(Y:1)
1                       WHEN X"41" MOVE X"31" to RELATOR-ESTRUTURA(Y:1)
2                       WHEN X"42" MOVE X"32" to RELATOR-ESTRUTURA(Y:1)
3                       WHEN X"43" MOVE X"33" to RELATOR-ESTRUTURA(Y:1)
4                       WHEN X"44" MOVE X"34" to RELATOR-ESTRUTURA(Y:1)
5                       WHEN X"45" MOVE X"35" to RELATOR-ESTRUTURA(Y:1)
6                       WHEN X"46" MOVE X"36" to RELATOR-ESTRUTURA(Y:1)
7                       WHEN X"47" MOVE X"37" to RELATOR-ESTRUTURA(Y:1)
8                       WHEN X"48" MOVE X"38" to RELATOR-ESTRUTURA(Y:1)
9                       WHEN X"49" MOVE X"39" to RELATOR-ESTRUTURA(Y:1)
                    END-EVALUATE
                END-IF
                IF   MV-TIPO (M) = "j"
                     ADD 1 TO M
                     MOVE RELATOR-ESTRUTURA
                          (MV-POSIT (M): MV-TAMANHO (M))
                       TO ANO-J (1: 2)
                     SUBTRACT 1 FROM M
                     IF   ANO-J < JAN-28
                          MOVE "20" TO SORTWK-REG(MV-POSIT-OUT (M): 2)
                     ELSE
                          MOVE "19" TO SORTWK-REG(MV-POSIT-OUT (M): 2)
                     END-IF
                     IF RELATOR-ESTRUTURA
                        (MV-POSIT (M): MV-TAMANHO (M)) = ALL "0"
                        MOVE "00" TO SORTWK-REG(MV-POSIT-OUT (M): 2)
                     END-IF
                ELSE
                     IF   MV-COLUNA (M) = 0
                          MOVE PROCESSADOS
                            TO SORTWK-REG
                              (MV-POSIT-OUT (M): MV-TAMANHO   (M))
                     ELSE
                          IF  MV-TIPO (M) = "=" OR "@"
                              MOVE MV-POSIT (M)        TO OP
                              MOVE OPERACAO-INTEIRO (OP) TO NUMERICO
                              MOVE NUMERICO (1: 18)
                                TO SORTWK-REG
                               (MV-POSIT-OUT (M): MV-TAMANHO   (M))
                          ELSE
                              MOVE RELATOR-ESTRUTURA
                               (MV-POSIT     (M): MV-TAMANHO   (M))
                                TO SORTWK-REG
                               (MV-POSIT-OUT (M): MV-TAMANHO   (M))
                          END-IF
                     END-IF
                END-IF
                END-PERFORM
                ADD  1            TO SELECIONADOS
                MOVE SELECIONADOS TO SELECIONADOS-ED
                IF   RELCONT = "1"
                AND  (NOT CWUNIX-GUI)
                     CALL "CBL_WRITE_SCR_CHARS" USING X"0D19"
                                                SELECIONADOS-ED X"000B"
                END-IF
                IF  (NOT CWUNIX-GUI)
                    PERFORM 115-CHECK-TECLA THRU 115-99-FIM
                    CALL "CWREL8"   USING SORTWK-REG (1:999) R "D"
                    PERFORM VARYING REVPOS FROM 1 BY 1
                              UNTIL REVPOS > REV
                            MOVE REVERSOES (REVPOS) TO REVW
                            COMPUTE SORTWK-BYTE-X (REVW) = 255
                                  - SORTWK-BYTE-X (REVW)
                    END-PERFORM
                END-IF
                WRITE SORTWK-REG
           END-IF.

       110-99-FIM. EXIT.

       111-EFETUA-OPERACOES.

           EVALUATE TRUE
LD             WHEN OPERACAO-OPERANDO1 (M) (1: 7) = "Prompt-"
LD                  PERFORM VARYING P FROM 1 BY 1
LD                          UNTIL PROMPT-DATANAME (P) = SPACES
LD                       OR PROMPT-DATANAME (P)
LD                             = OPERACAO-OPERANDO1 (M) (8:)
LD                       CONTINUE
LD                  END-PERFORM
LD                  IF  PROMPT-DATANAME (P) = OPERACAO-OPERANDO1(M)(8:)
LD                      MOVE 0                  TO NUMERICO
LD                      MOVE PROMPT-TAMANHO (P) TO OP-TAMANHO
LD                      MOVE PROMPt-DEC     (P) TO OP-DEC
LD                      COMPUTE Y3 = 12 - (OP-TAMANHO - OP-DEC) + 1
LD                      PERFORM UNTIL Y3 > 0
LD                              ADD      1   TO Y3
LD                              SUBTRACT 1 FROM OP-TAMANHO
LD                      END-PERFORM
LD                      MOVE Y3         TO Y
LD                      MOVE PROMPT-CONTEUDO (P)
LD                                      TO NUMERICO (Y: )
LD                      MOVE NUMERICO   TO OPERACAO-1I
LD                  END-IF
               WHEN OPERACAO-OP-1LIT (M)  = "$"
                    MOVE OPERACAO-LIT1 (M) TO OPERACAO-1
               WHEN OPERACAO-OPERANDO1 (M) (1: 1) = "("
                    MOVE OPERACAO-OPERANDO1 (M) (2: 4) TO OP
                    MOVE OPERACAO-MEMORIA(OP)          TO OPERACAO-1
               WHEN OTHER
                    MOVE 0                             TO NUMERICO
                    MOVE OPERACAO-OPERANDO1 (M) (1: 4) TO OP-POSIT
                    MOVE OPERACAO-OPERANDO1 (M) (5: 3) TO OP-TAMANHO
                    MOVE OPERACAO-OPERANDO1 (M) (8: 2) TO OP-DEC
                    COMPUTE Y3 = 12 - (OP-TAMANHO - OP-DEC) + 1
                    PERFORM UNTIL Y3 > 0
                            ADD      1   TO Y3
                            SUBTRACT 1 FROM OP-TAMANHO
                    END-PERFORM
                    MOVE Y3         TO Y
                    MOVE RELATOR-ESTRUTURA (OP-POSIT: OP-TAMANHO)
                                    TO NUMERICO (Y: )
                    PERFORM 112-EBCDIC THRU 112-99-FIM
                    MOVE NUMERICO   TO OPERACAO-1I
           END-EVALUATE

           EVALUATE TRUE
LD             WHEN OPERACAO-OPERANDO2 (M) (1: 7) = "Prompt-"
LD                  PERFORM VARYING P FROM 1 BY 1
LD                          UNTIL PROMPT-DATANAME (P) = SPACES
LD                       OR PROMPT-DATANAME (P)
LD                             = OPERACAO-OPERANDO2 (M) (8:)
LD                       CONTINUE
LD                  END-PERFORM
LD                  IF  PROMPT-DATANAME (P) = OPERACAO-OPERANDO2(M)(8:)
LD                      MOVE 0                  TO NUMERICO
LD                      MOVE PROMPT-TAMANHO (P) TO OP-TAMANHO
LD                      MOVE PROMPt-DEC     (P) TO OP-DEC
LD                      COMPUTE Y3 = 12 - (OP-TAMANHO - OP-DEC) + 1
LD                      PERFORM UNTIL Y3 > 0
LD                              ADD      1   TO Y3
LD                              SUBTRACT 1 FROM OP-TAMANHO
LD                      END-PERFORM
LD                      MOVE Y3         TO Y
LD                      MOVE PROMPT-CONTEUDO (P)
LD                                      TO NUMERICO (Y: )
LD                      MOVE NUMERICO   TO OPERACAO-2I
LD                  END-IF
               WHEN OPERACAO-OP-2LIT (M)  = "$"
                    MOVE OPERACAO-LIT2 (M) TO OPERACAO-2
               WHEN OPERACAO-OPERANDO2 (M) (1: 1) = "("
                    MOVE OPERACAO-OPERANDO2 (M) (2: 4) TO OP
                    MOVE OPERACAO-MEMORIA(OP)          TO OPERACAO-2
               WHEN OTHER
                    MOVE 0                             TO NUMERICO
                    MOVE OPERACAO-OPERANDO2 (M) (1: 4) TO OP-POSIT
                    MOVE OPERACAO-OPERANDO2 (M) (5: 3) TO OP-TAMANHO
                    MOVE OPERACAO-OPERANDO2 (M) (8: 2) TO OP-DEC
                    COMPUTE Y3 = 12 - (OP-TAMANHO - OP-DEC) + 1
                    PERFORM UNTIL Y3 > 0
                            ADD      1   TO Y3
                            SUBTRACT 1 FROM OP-TAMANHO
                    END-PERFORM
                    MOVE Y3         TO Y
                    MOVE RELATOR-ESTRUTURA (OP-POSIT: OP-TAMANHO)
                                    TO NUMERICO-2 (Y: )
                    PERFORM 113-EBCDIC-2 THRU 113-99-FIM
                    MOVE NUMERICO-2 TO OPERACAO-2I
           END-EVALUATE

           EVALUATE OPERACAO-OPERADOR (M)
               WHEN "+"
                   COMPUTE OPERACAO-MEMORIA(M) = OPERACAO-1 + OPERACAO-2
               WHEN "-"
                   COMPUTE OPERACAO-MEMORIA(M) = OPERACAO-1 - OPERACAO-2
               WHEN "*"
                   COMPUTE OPERACAO-MEMORIA(M) = OPERACAO-1 * OPERACAO-2
               WHEN "/"
                   COMPUTE OPERACAO-MEMORIA(M) = OPERACAO-1 / OPERACAO-2
               WHEN "P"
                   COMPUTE OPERACAO-MEMORIA(M)
                         = OPERACAO-1 ** OPERACAO-2
               WHEN "R"
                    COMPUTE OPERACAO-MEMORIA(M)
                         = OPERACAO-1 ** (1 / OPERACAO-2)
           END-EVALUATE.

       111-99-FIM. EXIT.

       112-EBCDIC.

           IF Y = LENGTH NUMERICO
              EVALUATE NUMERICO (Y: 1)
0-                  WHEN X"7D" MOVE X"70" to NUMERICO (Y: 1)
1-                  WHEN X"4A" MOVE X"71" to NUMERICO (Y: 1)
2-                  WHEN X"4B" MOVE X"72" to NUMERICO (Y: 1)
3-                  WHEN X"4C" MOVE X"73" to NUMERICO (Y: 1)
4-                  WHEN X"4D" MOVE X"74" to NUMERICO (Y: 1)
5-                  WHEN X"4E" MOVE X"75" to NUMERICO (Y: 1)
6-                  WHEN X"4F" MOVE X"76" to NUMERICO (Y: 1)
7-                  WHEN X"50" MOVE X"77" to NUMERICO (Y: 1)
8-                  WHEN X"51" MOVE X"78" to NUMERICO (Y: 1)
9-                  WHEN X"52" MOVE X"79" to NUMERICO (Y: 1)
0                   WHEN X"7B" MOVE X"30" to NUMERICO (Y: 1)
1                   WHEN X"41" MOVE X"31" to NUMERICO (Y: 1)
2                   WHEN X"42" MOVE X"32" to NUMERICO (Y: 1)
3                   WHEN X"43" MOVE X"33" to NUMERICO (Y: 1)
4                   WHEN X"44" MOVE X"34" to NUMERICO (Y: 1)
5                   WHEN X"45" MOVE X"35" to NUMERICO (Y: 1)
6                   WHEN X"46" MOVE X"36" to NUMERICO (Y: 1)
7                   WHEN X"47" MOVE X"37" to NUMERICO (Y: 1)
8                   WHEN X"48" MOVE X"38" to NUMERICO (Y: 1)
9                   WHEN X"49" MOVE X"39" to NUMERICO (Y: 1)
              END-EVALUATE
           END-IF.

       112-99-FIM. EXIT.

       113-EBCDIC-2.

           IF Y = LENGTH NUMERICO
              EVALUATE NUMERICO-2 (Y: 1)
0-                  WHEN X"7D" MOVE X"70" to NUMERICO-2 (Y: 1)
1-                  WHEN X"4A" MOVE X"71" to NUMERICO-2 (Y: 1)
2-                  WHEN X"4B" MOVE X"72" to NUMERICO-2 (Y: 1)
3-                  WHEN X"4C" MOVE X"73" to NUMERICO-2 (Y: 1)
4-                  WHEN X"4D" MOVE X"74" to NUMERICO-2 (Y: 1)
5-                  WHEN X"4E" MOVE X"75" to NUMERICO-2 (Y: 1)
6-                  WHEN X"4F" MOVE X"76" to NUMERICO-2 (Y: 1)
7-                  WHEN X"50" MOVE X"77" to NUMERICO-2 (Y: 1)
8-                  WHEN X"51" MOVE X"78" to NUMERICO-2 (Y: 1)
9-                  WHEN X"52" MOVE X"79" to NUMERICO-2 (Y: 1)
0                   WHEN X"7B" MOVE X"30" to NUMERICO-2 (Y: 1)
1                   WHEN X"41" MOVE X"31" to NUMERICO-2 (Y: 1)
2                   WHEN X"42" MOVE X"32" to NUMERICO-2 (Y: 1)
3                   WHEN X"43" MOVE X"33" to NUMERICO-2 (Y: 1)
4                   WHEN X"44" MOVE X"34" to NUMERICO-2 (Y: 1)
5                   WHEN X"45" MOVE X"35" to NUMERICO-2 (Y: 1)
6                   WHEN X"46" MOVE X"36" to NUMERICO-2 (Y: 1)
7                   WHEN X"47" MOVE X"37" to NUMERICO-2 (Y: 1)
8                   WHEN X"48" MOVE X"38" to NUMERICO-2 (Y: 1)
9                   WHEN X"49" MOVE X"39" to NUMERICO-2 (Y: 1)
              END-EVALUATE
           END-IF.

       113-99-FIM. EXIT.

       115-CHECK-TECLA.

           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS

           IF   KEY-STATUS   = 1
                CALL "CWKBDC" USING "0000" CARACTER TECLA2
                EVALUATE TRUE
                     WHEN EDIT-CURSOR-LEFT
                     AND  R > 1
                          SUBTRACT 1 FROM R
                          CALL "CWREL8"   USING SORTWK-REG (1:999) R "R"
                     WHEN EDIT-CURSOR-RIGHT
                     AND  R < LENGTH SORTWK-REG - 78
                          ADD 1 TO R
                          CALL "CWREL8"   USING SORTWK-REG (1:999) R "R"
                     WHEN EDIT-ESC
                          MOVE "Deseja interromper o relat¢rio ? "
                            TO CWSEND-MSG
                          MOVE SPACES       TO CWSEND-SCREENS
                          MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                          MOVE "~Continuar" TO CWSEND-SCREEN (2)
                          CALL "CWSEND" USING PARAMETROS-CWSEND
                          IF   CWSEND-OPTION = 1
                               IF  (LISTADOS NOT = 0)
                               AND (NOT CWIMPR-END-PRINT)
                                    SET CWIMPR-CLOSE TO TRUE
                                   CALL "CWIMPR" USING PARAMETROS-CWIMPR
                               END-IF
                               SET CWIMPR-END-PRINT TO TRUE
                          END-IF
                          CALL "CBL_SET_CSR_POS" USING X"FFFF"
                END-EVALUATE
           END-IF.

       115-99-FIM. EXIT.

       120-CHECK-CONDICAO.

           IF  (CONDICAO-ACAO (M) NOT = ACAO)
           OR   CONDICAO-CONDICAO (M) = SPACES
                GO TO 120-99-FIM
           END-IF

           MOVE CONDICAO-POSIT    (M) TO I
           MOVE CONDICAO-POSIT2   (M) TO I2
           MOVE CONDICAO-SIZE     (M) TO K
           MOVE CONDICAO-SIZE2    (M) TO K2

           IF   CONDICAO-SIZE     (M) = 0
           AND  CONDICAO-PROMPT   (M) = 0
                MOVE 18 TO K
           END-IF

           IF   CONDICAO-SIZE2    (M) = 0
           AND  CONDICAO-PROMPT2  (M) = 0
                MOVE 18 TO K2
           END-IF

           IF   CONDICAO-TIPO (M) = "D"
                IF   K > K2
                     MOVE K2 TO K
                ELSE
                     IF   K2 > K
                          MOVE K TO K2
                     END-IF
                END-IF
           END-IF

           IF   CONDICAO-LITERAL (M) = "L"
           AND  CONDICAO-TIPO    (M) = "X"
           AND  K2 > 30
                MOVE 30 TO K2
           END-IF

           MOVE CONDICAO-CONDICAO (M) TO CONDICAO
           COMPUTE Y = 18 - K + 1
           INITIALIZE NUMERICO NUMERICO-2
                      ALPHA    ALPHA-2

           MOVE CONDICAO-PROMPT   (M) TO P
           IF   P = 0
                IF   CONDICAO-TIPO     (M) = "X"
                     MOVE RELATOR-ESTRUTURA (I: K) TO ALPHA
                ELSE
                     IF   CONDICAO-TIPO (M) = "D"
                          MOVE    CONDICAO-M1      (M)      TO MASCARA
                          MOVE    CONDICAO-SIZE    (M)      TO DX
                          MOVE    RELATOR-ESTRUTURA (I: DX) TO DATA-A
                          PERFORM 121-COMPARA-DATA THRU 121-99-FIM
                          MOVE    DATA-D   (1: K)    TO NUMERICO (Y: K)
                     ELSE
                          IF   CONDICAO-DATANAME2 (M) (1: 3) = "(=)"
                               MOVE OPERACAO-INTEIRO (I) TO NUMERICO
                               IF   CONDICAO-M1 (M) (1: 2) NUMERIC
                                    MOVE CONDICAO-M1 (M) (1: 2)
                                      TO DEC10
                               ELSE
                                    MOVE 0               TO DEC10
                               END-IF
                               IF  DEC10 > 6
                                   COMPUTE BY10 = DEC10 - 6
                                   PERFORM BY10 TIMES
                                       COMPUTE NUMERICO = NUMERICO * 10
                                   END-PERFORM
                               END-IF
                               IF  DEC10 < 6
                                   COMPUTE BY10 = 6 - DEC10
                                   PERFORM BY10 TIMES
                                       COMPUTE NUMERICO = NUMERICO / 10
                                   END-PERFORM
                               END-IF
                          ELSE
                               MOVE RELATOR-ESTRUTURA (I: K)
                                 TO NUMERICO (Y: K)
                               PERFORM 112-EBCDIC THRU 112-99-FIM
                          END-IF
                     END-IF
                     IF   NUMERICO NOT = 0
                          IF   CONDICAO-J1 (M) = "j"
                          AND  A = 2
                               MOVE DATA-D (1: 2) TO ANO-J
                               IF   ANO-J > JAN-28
                                    MOVE "19" TO NUMERICO (Y - 2: 2)
                               ELSE
                                    MOVE "20" TO NUMERICO (Y - 2: 2)
                               END-IF
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE PROMPT-TAMANHO    (P) TO K
                IF   CONDICAO-TIPO     (M) = "X"
                     MOVE PROMPT-CONTEUDO (P) (1: K) TO ALPHA
                ELSE
                     IF   CONDICAO-J2 (M) = "j"
                          ADD      2   TO K
                          SUBTRACT 2 FROM Y
                     END-IF
                     MOVE PROMPT-CONTEUDO (P) (1: K) TO NUMERICO (Y:K)
                     IF   CONDICAO-J2 (M) = "j"
                          ADD      2   TO Y
                          SUBTRACT 2 FROM K
                     END-IF
Rofer                INSPECT NUMERICO(Y:K)
Rofer                 CONVERTING SPACES TO ZEROS
                END-IF
           END-IF

           MOVE CONDICAO-PROMPT2  (M) TO P
           IF   P = 0
                IF   CONDICAO-LITERAL (M) = "L"
                     IF   CONDICAO-TIPO (M) = "X"
                          MOVE CONDICAO-OPERANDO (M) (1: K2) TO ALPHA-2
                     ELSE
                          MOVE CONDICAO-OPERANDO (M) (1: K2)
                            TO NUMERICO-2 (Y: K2)
                     END-IF
                ELSE
                     IF   CONDICAO-TIPO     (M) = "X"
                          MOVE RELATOR-ESTRUTURA (I2: K2) TO ALPHA-2
                     ELSE
                     IF   CONDICAO-TIPO (M) = "D"
                          MOVE    CONDICAO-M2      (M)       TO MASCARA
                          MOVE    CONDICAO-SIZE2   (M)       TO DX
                          MOVE    RELATOR-ESTRUTURA (I2: DX) TO DATA-A
                          PERFORM 121-COMPARA-DATA THRU 121-99-FIM
                          MOVE    DATA-D (1: K2) TO NUMERICO-2 (Y: K2)
                     ELSE
                          IF   CONDICAO-OPERANDO (M) (1: 3) = "(=)"
                               MOVE OPERACAO-INTEIRO (I2) TO NUMERICO-2
                               IF   CONDICAO-M2 (M) (1: 2) NUMERIC
                                    MOVE CONDICAO-M2 (M) (1: 2)
                                      TO DEC10
                               ELSE
                                    MOVE 0               TO DEC10
                               END-IF
                               IF  DEC10 > 6
                                   COMPUTE BY10 = DEC10 - 6
                                   PERFORM BY10 TIMES
                                       COMPUTE NUMERICO-2
                                             = NUMERICO-2 * 10
                                   END-PERFORM
                               END-IF
                               IF  DEC10 < 6
                                   COMPUTE BY10 = 6 - DEC10
                                   PERFORM BY10 TIMES
                                       COMPUTE NUMERICO-2
                                             = NUMERICO-2 / 10
                                   END-PERFORM
                               END-IF
                          ELSE
                               MOVE RELATOR-ESTRUTURA (I2: K2)
                                 TO NUMERICO-2 (Y: K2)
                               PERFORM 113-EBCDIC-2 THRU 113-99-FIM
                          END-IF
                     END-IF
                     IF   NUMERICO-2 NOT = 0
                          IF   CONDICAO-J2 (M) = "j"
                          AND  A = 2
                               MOVE DATA-D (1: 2) TO ANO-J
                               IF   ANO-J > JAN-28
                                    MOVE "19" TO NUMERICO-2 (Y - 2: 2)
                               ELSE
                                    MOVE "20" TO NUMERICO-2 (Y - 2: 2)
                               END-IF
                          END-IF
                     END-IF
                   END-IF
                END-IF
           ELSE
                MOVE PROMPT-TAMANHO (P) TO K2
                IF   CONDICAO-TIPO  (M) = "X"
                     MOVE PROMPT-CONTEUDO (P) (1: K2) TO ALPHA-2
                ELSE
                     IF   CONDICAO-J1 (M) = "j"
                          ADD      2   TO K2
                          SUBTRACT 2 FROM Y
                     END-IF
                     MOVE PROMPT-CONTEUDO(P) (1: K2) TO NUMERICO-2(Y:K2)
                     IF   CONDICAO-J1 (M) = "j"
                          ADD      2   TO Y
                          SUBTRACT 2 FROM K2
                     END-IF
Rofer                INSPECT NUMERICO-2(Y:K2)
Rofer                 CONVERTING SPACES TO ZEROS
                END-IF
           END-IF

           IF   CONDICAO-TIPO (M) = "X"
                IF ((IGUAL AND ALPHA (1: K) = ALPHA-2 (1: K2))
                OR  (MENOR AND ALPHA (1: K) < ALPHA-2 (1: K2))
                OR  (MAIOR AND ALPHA (1: K) > ALPHA-2 (1: K2)))
                    MOVE 1 TO OK
               END-IF
           ELSE
                IF ((IGUAL AND NUMERICO = NUMERICO-2)
                OR  (MENOR AND NUMERICO < NUMERICO-2)
                OR  (MAIOR AND NUMERICO > NUMERICO-2))
                    MOVE 1 TO OK
               END-IF
           END-IF.

       120-99-FIM. EXIT.

       121-COMPARA-DATA.

           MOVE SPACES TO DATA-D
           MOVE M TO M1
           MOVE 0 TO D M A PD PM PA
           PERFORM VARYING E
                      FROM 1 BY 1 UNTIL E > 10
                  IF  MASCARA (E: 1) = "D"
                      ADD 1 TO D
                      IF   PD = 0
                           MOVE E TO PD
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "M"
                      ADD 1 TO M
                      IF   PM = 0
                           MOVE E TO PM
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "A"
                      ADD 1 TO A
                      IF   PA = 0
                           MOVE E TO PA
                      END-IF
                  END-IF
           END-PERFORM

           MOVE 1 TO DM

           IF   A NOT = 0
                MOVE DATA-A (PA: A) TO DATA-D (DM: A)
                ADD  A              TO DM
           END-IF

           IF   M NOT = 0
                MOVE DATA-A (PM: M) TO DATA-D (DM: M)
                ADD  M              TO DM
           END-IF

           IF   D NOT = 0
                MOVE DATA-A (PD: D) TO DATA-D (DM: D)
                ADD  D              TO DM
           END-IF

           MOVE M1 TO M.

       121-99-FIM.

       130-IMPRESSAO.

           IF ARQUIVO
              PERFORM VARYING OBJETO FROM 1 BY 1
                      UNTIL OBJETO > TAGS
                  MOVE OBJETO-OUT  (OBJETO) TO M
mega2            IF OBJETO-DATANAME2 (OBJETO) (1: 8) = 'Relator-'
mega2               MOVE OBJETO-DATANAME2 (OBJETO) (9: ) TO DATANAME
mega2               PERFORM 191-RELATOR THRU 191-99-FIM
mega2 *             MOVE MASCARA TO ELEMENTO
mega2 *             PERFORM 195-EDITAR  THRU 195-99-FIM
mega2 *             MOVE ELEMENTO (1: S)    TO CWSAVE-DATA (OBJETO)
mega2 *             EXIT PERFORM CYCLE
mega2            END-IF
                 IF CWSAVE-NUMERIC (OBJETO)
                    MOVE SPACES TO MASCARA
mega2               IF OBJETO-DATANAME2 (OBJETO) (1: 8) NOT = 'Relator-'
                       MOVE SORTWK-REG (MV-POSIT-OUT(M): MV-TAMANHO (M))
                         TO DADOS
                    END-IF
                    MOVE 1      TO P
                    MOVE MV-TAMANHO (M) TO S S2
                    MOVE '9' TO TIPO
                    IF CWSAVE-DEC (OBJETO) NUMERIC
                       MOVE 'V' TO TIPO
                       MOVE CWSAVE-LEN (OBJETO) TO I
                       SUBTRACT CWSAVE-DEC (OBJETO) FROM I
                       MOVE ALL '9' TO MASCARA (P: I)
                       ADD  I       TO P
                       MOVE ','     TO MASCARA (P:1)
                       MOVE CWSAVE-DEC (OBJETO) TO I DEC
                       ADD  1                   TO P
                       MOVE ALL '9' TO MASCARA (P: I)
                       ADD  I       TO P
                    ELSE
                       MOVE 0 TO DEC
                       MOVE ALL '9' TO MASCARA (P: MV-TAMANHO (M))
                       ADD  MV-TAMANHO (M) TO P
                    END-IF
                    IF CWSAVE-SIGNAL (OBJETO)
                       MOVE 'V' TO TIPO
                       MOVE '-' TO MASCARA(P:1)
                    END-IF
                    MOVE MASCARA TO ELEMENTO
                    PERFORM 195-EDITAR THRU 195-99-FIM
                    MOVE ELEMENTO (1: S)    TO CWSAVE-DATA (OBJETO)
                 ELSE
mega2               IF OBJETO-DATANAME2 (OBJETO) (1: 8) = 'Relator-'
                       MOVE DADOS TO CWSAVE-DATA (OBJETO)
                    ELSE
                       MOVE SORTWK-REG(MV-POSIT-OUT(M): MV-TAMANHO (M))
                        TO CWSAVE-DATA (OBJETO)
                    END-IF
                 END-IF
              END-PERFORM
              EXEC COBOLware PROCESS SHOW
                   HEADER "Gerando arquivo"
                   LINE 10 COLUMN 10
                   EMPTY SELECIONADOS
                   MESSAGE CWSAVE-FILE
                   QUESTION "Deseja interromper o geraá∆o ? "
                   CANCEL RESPOSTA;RESPOSTA
                   FULL LISTADOS
              END-EXEC
              IF NOT PARAR
                 CALL 'CWSAVE' USING PARAMETROS-CWSAVE
              END-IF
              EXIT PARAGRAPH
           END-IF

           IF   CAB > 0
                IF   NOT COBOLware
                     IF   LINHA > SIZE-PAGE
                     AND (SIZE-PAGE NOT = 0)
                          PERFORM 131-CABECALHO THRU 131-99-FIM
                     END-IF
                ELSE
                     MOVE    1               TO CABP
                     PERFORM 131-CABECALHO THRU 131-99-FIM
                     MOVE    0               TO CABP
                END-IF
           END-IF

           ON   1
                MOVE SORTWK-REG TO ANTERIOR.

           MOVE    1            TO Q
           PERFORM 140-QUEBRA THRU 140-99-FIM
                   VARYING I FROM NIVEL-MAX BY -1
                     UNTIL I = 0

           ON   1
                MOVE "10" TO FS-SORTWK.

           MOVE    2            TO Q
           PERFORM 140-QUEBRA THRU 140-99-FIM
      *            VARYING I FROM NIVEL-MAX BY -1
      *              UNTIL I = 0
                   VARYING I FROM 1 BY 1
                     UNTIL I > nivel-max
           MOVE    3 TO Q
           ADD     1 TO RELATOR-SELECIONADOS (8)
           IF   NIVEL-MAX = 0
                ADD 1 TO RELATOR-SELECIONADOS (1)
           ELSE
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > NIVEL-MAX
                        ADD 1 TO RELATOR-SELECIONADOS (I)
                END-PERFORM
           END-IF

           MOVE    "00"          TO FS-SORTWK
           PERFORM 150-ACUMULAR         THRU 150-99-FIM
                   VARYING M FROM 1 BY 1 UNTIL M > MV-OBJETOS

           IF   NIVEL-MAX = 0
                MOVE 1         TO I
           ELSE
                MOVE NIVEL-MAX TO I
           END-IF

           PERFORM 160-DETALHE THRU 160-99-FIM
           MOVE    0             TO CABX.

       130-99-FIM. EXIT.

       131-CABECALHO.

           IF   CABX = 0
                MOVE 1 TO LINHA
                ADD  1 TO RELATOR-PAGINA
                          RELATOR-FOLHA
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > CAB
                                                 OR CWIMPR-END-PRINT
                        IF  (MAPA-LIN (Y) NOT = LOW-VALUES)
                            PERFORM 180-SAIDA THRU 180-99-FIM
                        END-IF
                END-PERFORM
                MOVE 1 TO CABX
          END-IF.

       131-99-FIM. EXIT.

       140-QUEBRA.

           IF  (SORTWK-REG (1: NIVEL-TAMANHO (I)) NOT =
                ANTERIOR   (1: NIVEL-TAMANHO (I)))
           OR   FS-SORTWK = "10"
                IF   Q = 1 OR 4
                     IF   I < 8
                          ADD 1 TO RELATOR-QUEBRA (I)
                     END-IF
                     COMPUTE Y = DET + 1
                     PERFORM 145-PRE-DETALHE THRU 145-99-FIM
                             VARYING Y FROM Y BY 1 UNTIL Y > TOT
                     INITIALIZE NIVEL (I)
                END-IF
                IF   Q = 2
                     COMPUTE Y = CAB + 1
                     COMPUTE yy = CAB + 1
                     PERFORM 145-PRE-DETALHE THRU 145-99-FIM
                             VARYING Y FROM Y BY 1 UNTIL Y > DET
                END-IF
           END-IF.

       140-99-FIM. EXIT.

       145-PRE-DETALHE.

           IF   NIVEL-QUEBRA (Y) = I
           OR  (Q = 4 AND STATUS-LINHA (Y) = "T")
                IF   SALTO (Y) = 1
                AND (Q = 2 OR 4)
                     ACCEPT CWIMPR-TIME-REPORT FROM TIME
                     MOVE    Y                   TO Y2
                     MOVE    0                   TO RELATOR-PAGINA
                     PERFORM 131-CABECALHO     THRU 131-99-FIM
                     MOVE    Y2                  TO Y
                END-IF
                PERFORM 180-SAIDA THRU 180-99-FIM
                IF   SALTO (Y) = 1
                AND  Q = 1
                     MOVE    Y                    TO Y2
                     MOVE    0                    TO RELATOR-PAGINA
                     PERFORM 131-CABECALHO      THRU 131-99-FIM
                     MOVE    Y2                   TO Y
                     ACCEPT  CWIMPR-TIME-REPORT FROM TIME
                END-IF
           END-IF.

       145-99-FIM. EXIT.

       150-ACUMULAR.

           IF   MV-TIPO (M) = "+" OR "@"
                MOVE MV-ACM (M) TO Y
                MOVE 0            TO NUMERICO
                COMPUTE K = 18 - MV-TAMANHO (M) + 1
                MOVE SORTWK-REG (MV-POSIT-OUT (M): MV-TAMANHO (M))
                 TO NUMERICO   (K: MV-TAMANHO (M))
                IF   NIVEL-MAX = 0
                     ADD  NUMERICO      TO ACUMULADOR (1 Y)
                     MOVE MV-TIPO (M) TO ACUMULADOR-FLAG (1 Y)
                ELSE
                     PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > NIVEL-MAX
                             ADD  NUMERICO      TO ACUMULADOR (I Y)
                             MOVE MV-TIPO (M) TO ACUMULADOR-FLAG (I Y)
                     END-PERFORM
                END-IF
                ADD  NUMERICO      TO ACUMULADOR (8 Y)
                MOVE MV-TIPO (M) TO ACUMULADOR-FLAG (8 Y)
           END-IF.

       150-99-FIM. EXIT.

       160-DETALHE.

           IF   CAB = 0
                MOVE 1 TO Y
           ELSE
                COMPUTE Y = CAB + 1
           END-IF
           PERFORM VARYING Y FROM Y BY 1 UNTIL Y > DET
                                            OR CWIMPR-END-PRINT
                   IF (STATUS-LINHA (Y) = SPACE or "K" or "t"
                   AND (Y NOT = DET-NULL))
                       PERFORM 180-SAIDA THRU 180-99-FIM
                   END-IF
           END-PERFORM.

       160-99-FIM. EXIT.

       180-SAIDA.

           MOVE SPACES TO CWIMPR-DETAIL
           PERFORM VARYING OBJETO FROM 1 BY 1
                   UNTIL OBJETO > OBJETO-R
                     OR OBJETO-RL (OBJETO) > Y
                   IF   OBJETO-RL (OBJETO) = Y
                        IF OBJETO-RESULTADO (OBJETO)
                           PERFORM 189-RESULTADO      THRU 189-99-FIM
                        ELSE
                           PERFORM 190-FORMATA-OBJETO THRU 190-99-FIM
                        END-IF
                   END-IF
           END-PERFORM

           if   flag (Y) not = "*"
           IF   CABP = 1
                EVALUATE Y
                    WHEN 1 MOVE CWIMPR-DETAIL TO CWIMPR-TITLE
                    WHEN 2 MOVE CWIMPR-DETAIL TO CWIMPR-SUB-TITLE
                    WHEN 3 MOVE CWIMPR-DETAIL TO CWIMPR-HEADER-1
                    WHEN 4 MOVE CWIMPR-DETAIL TO CWIMPR-HEADER-2
                    WHEN 5 MOVE CWIMPR-DETAIL TO CWIMPR-HEADER-3
                    WHEN 6 MOVE CWIMPR-DETAIL TO CWIMPR-HEADER-4
                    WHEN 7 MOVE CWIMPR-DETAIL TO CWIMPR-HEADER-5
                END-EVALUATE
           ELSE
                IF   NOT CWUNIX-GUI
                     PERFORM 115-CHECK-TECLA THRU 115-99-FIM
                     MOVE CWIMPR-DETAIL TO TEXTO-999
                     CALL "CWREL8"   USING TEXTO-999 R "D"
                END-IF
                IF   LIVRE
                AND  SALTO (Y) = SPACE
                AND  Y = 1
                AND  RELATOR-FOLHA > 1
                     MOVE CWIMPR-DETAIL TO ELEMENTO
                     MOVE X"0C"         TO CWIMPR-DETAIL
                     MOVE ELEMENTO      TO CWIMPR-DETAIL (2: )
                END-IF
                ADD  1           TO LINHA
                IF   NOT CWIMPR-END-PRINT
                     CALL "CWIMPR" USING PARAMETROS-CWIMPR
                END-IF
           END-IF.

       180-99-FIM. EXIT.

       189-RESULTADO.

           MOVE 1        TO ERRO
           MOVE OBJETO   TO RESULTADO

           PERFORM VARYING OBJETO FROM 1 BY 1
                   UNTIL OBJETO > OBJETO-R
                      OR (OBJETO-DATANAME (OBJETO)
                      = OBJETO-DATANAME (RESULTADO)
                      AND (NOT OBJETO-RESULTADO (OBJETO)))
                   CONTINUE
           END-PERFORM
           IF OBJETO > OBJETO-R
              MOVE SPACES TO CWSEND-SCREENS
              STRING 'Operando-1 "'     DELIMITED BY SIZE
                     OBJETO-DATANAME (RESULTADO)
                                        DELIMITED BY SPACE
                     '" n∆o definido' DELIMITED BY SIZE
                INTO CWSEND-MSG
              CALL "CWSEND" USING PARAMETROS-CWSEND
           ELSE
              MOVE OBJETO-DEC (OBJETO)     TO DEC-1
              PERFORM 190-FORMATA-OBJETO THRU 190-99-FIM
              MOVE DADOS (1:18)            TO OPERANDO-1
              PERFORM VARYING OBJETO FROM 1 BY 1
                      UNTIL OBJETO > OBJETO-R
                         OR (OBJETO-DATANAME (OBJETO)
                         = OBJETO-DATANAME2 (RESULTADO)
                         AND (NOT OBJETO-RESULTADO (OBJETO)))
                      CONTINUE
              END-PERFORM
              IF OBJETO > OBJETO-R
                 MOVE SPACES TO CWSEND-SCREENS
                 STRING 'Operando-2 "'     DELIMITED BY SIZE
                        OBJETO-DATANAME2 (RESULTADO)
                                           DELIMITED BY SPACE
                        '" n∆o definido' DELIMITED BY SIZE
                   INTO CWSEND-MSG
                 CALL "CWSEND" USING PARAMETROS-CWSEND
              ELSE
                 MOVE OBJETO-DEC (OBJETO)        TO DEC-2
                 PERFORM 190-FORMATA-OBJETO    THRU 190-99-FIM
                 MOVE DADOS (1:18)               TO OPERANDO-2
                 MOVE 0                          TO RESULTADO-DEC
                 MOVE OBJETO-RL      (RESULTADO) TO L
                 MOVE OBJETO-RC      (RESULTADO) TO C
                 MOVE OBJETO-TAMANHO (RESULTADO) TO S
                 MOVE MAPA-LIN (L) (C: S)        TO ELEMENTO
                 PERFORM VARYING RS FROM 1 BY 1
                         UNTIL ELEMENTO (RS:1) = SPACE
                      IF ELEMENTO (RS:1) = ','
                         MOVE 1 TO RESULTADO-DEC
                         ADD  2 TO RS
                      END-IF
                      IF RESULTADO-DEC > 0
                      AND(ELEMENTO (RS:1) = '9' OR 'Z')
                         ADD  1 TO RESULTADO-DEC
                      END-IF
                 END-PERFORM
                 IF DEC-1 > DEC-2
                    COMPUTE DEC-D = DEC-1 - DEC-2
                    PERFORM DEC-D TIMES
                            COMPUTE OPERANDO-2 = OPERANDO-2 * 10
                    END-PERFORM
                 END-IF
                 IF DEC-2 > DEC-1
                    COMPUTE DEC-D = DEC-2 - DEC-1
                    PERFORM DEC-D TIMES
                            COMPUTE OPERANDO-1 = OPERANDO-1 * 10
                    END-PERFORM
                 END-IF
                 PERFORM DEC-1 TIMES
                         COMPUTE OPERANDO-1 = OPERANDO-1 * 10
                 END-PERFORM
                 PERFORM DEC-2 TIMES
                         COMPUTE OPERANDO-2 = OPERANDO-2 * 10
                 END-PERFORM
                 EVALUATE OBJETO-TIPO (RESULTADO)
                     WHEN "+"
                         COMPUTE OPERANDO-3 = OPERANDO-1 + OPERANDO-2
                     WHEN "-"
                         COMPUTE OPERANDO-3 = OPERANDO-1 - OPERANDO-2
                     WHEN "*"
                         COMPUTE OPERANDO-4 = OPERANDO-1 * OPERANDO-2
                         MOVE 6 TO RESULTADO-DEC
                     WHEN "/"
                         COMPUTE OPERANDO-5 = OPERANDO-1 / OPERANDO-2
                         MOVE 12 TO RESULTADO-DEC
                     WHEN "P"
                         COMPUTE OPERANDO-4 = OPERANDO-1 ** OPERANDO-2
                         MOVE 6  TO RESULTADO-DEC
                     WHEN "R"
                         COMPUTE OPERANDO-5 =
                                 OPERANDO-1 ** (1 / OPERANDO-2)
                         MOVE 12 TO RESULTADO-DEC
                 END-EVALUATE
                 MOVE OPERANDO-3    TO DADOS
                 MOVE 'V'           TO TIPO
                 ADD  DEC-D         TO RESULTADO-DEC
                 MOVE RESULTADO-DEC TO DEC
                 MOVE 0             TO ERRO
                 MOVE 18            TO S
              END-IF
           END-IF

           MOVE RESULTADO TO OBJETO
           MOVE 0         TO RESULTADO

           IF ERRO = 0
              PERFORM 195-EDITAR THRU 195-99-FIM
              MOVE ELEMENTO (1: S)    TO CWIMPR-DETAIL (C: S)
           END-IF.

       189-99-FIM. EXIT.

       190-FORMATA-OBJETO.

           MOVE OBJETO-RL       (OBJETO) TO L
           MOVE OBJETO-RC       (OBJETO) TO C
           MOVE OBJETO-TAMANHO  (OBJETO) TO S S2
           MOVE MAPA-LIN (L) (C: S)      TO ELEMENTO SALVA-DATA
           MOVE OBJETO-OUT      (OBJETO) TO M
           MOVE 0                        TO DEC
           EVALUATE TRUE
               WHEN OBJETO-DATANAME (OBJETO) (1: 8) = "Relator-"
                    MOVE OBJETO-DATANAME (OBJETO) (9: ) TO DATANAME
                    PERFORM 191-RELATOR THRU 191-99-FIM
                    PERFORM 195-EDITAR THRU 195-99-FIM
               WHEN OBJETO-ACM (OBJETO) NOT = 0
                    MOVE OBJETO-ACM (OBJETO) TO K
                    MOVE ACUMULADOR (I K)    TO NUMERICO
                    MOVE NUMERICO (1: 18)    TO DADOS (1: )
                    MOVE OBJETO-DEC (OBJETO) TO DEC
                    IF   ACUMULADOR-FLAG (I K) = "@"
                         MOVE 6 TO DEC
                    END-IF
                    MOVE 18                  TO S
                    MOVE "V"                 TO TIPO
                    PERFORM 195-EDITAR THRU 195-99-FIM
               WHEN OBJETO-PROMPT (OBJETO) NOT = 0
                    MOVE OBJETO-PROMPT   (OBJETO) TO M
                    MOVE PROMPT-CONTEUDO (M)      TO DADOS
                    MOVE PROMPT-TAMANHO  (M)      TO S
                    MOVE PROMPT-DEC      (M)      TO DEC
                    MOVE PROMPT-TIPO     (M)      TO TIPO
                    MOVE PROMPT-MASCARA  (M)      TO MASCARA
                    PERFORM 195-EDITAR THRU 195-99-FIM
               WHEN OBJETO-OUT (OBJETO) NOT = 0
                    MOVE MV-POSIT-OUT (M) TO P
                    MOVE MV-TAMANHO   (M) TO S
                    MOVE MV-TIPO      (M) TO TIPO
                    IF   TIPO = "=" OR "@"
                         MOVE "V" TO TIPO
                    END-IF
                    MOVE MV-DEC       (M) TO DEC
                    MOVE SORTWK-REG (P: S)  TO DADOS
                    IF   Q = 1
                         MOVE ANTERIOR (P: S) TO DADOS
                    END-IF
                    IF   TIPO = "j" OR "a"
                         MOVE "D"               TO TIPO
                         MOVE 4                 TO S
                         MOVE 1                 TO K
                         MOVE SPACES            TO MASCARA DADOS
                         MOVE M                 TO M2
                         MOVE SORTWK-REG (P: S) TO DADOS   (K: S)
                         IF   Q = 1
                              MOVE ANTERIOR (P: S) TO DADOS (K: S)
                         END-IF
                         MOVE ALL "A"           TO MASCARA (K: S)
                         ADD  S                 TO K
                         ADD  1                 TO M2
                         IF   MV-TIPO (M) = "j"
                              ADD  1 TO M2
                         END-IF
                         IF   MV-TIPO (M2) = "m"
                              MOVE MV-TAMANHO   (M2) TO S
                              MOVE MV-POSIT-OUT (M2) TO P
                              MOVE SORTWK-REG (P: S)   TO DADOS   (K: S)
                              IF   Q = 1
                                   MOVE ANTERIOR (P: S) TO DADOS (K: S)
                              END-IF
                              MOVE ALL "M"             TO MASCARA (K: S)
                              ADD  S                   TO K
                              ADD  1                   TO M2
                         END-IF
                         IF   MV-TIPO (M2) = "d"
                              MOVE MV-TAMANHO   (M2) TO S
                              MOVE MV-POSIT-OUT (M2) TO P
                              MOVE SORTWK-REG (P: S)   TO DADOS   (K: S)
                              IF   Q = 1
                                   MOVE ANTERIOR (P: S) TO DADOS (K: S)
                              END-IF
                              MOVE ALL "D"             TO MASCARA (K: S)
                              ADD  S                   TO K
                         END-IF
                         COMPUTE S = K - 1
                    END-IF
                    IF   TIPO = "m"
                         MOVE "D"                 TO TIPO
                         MOVE 1                   TO K
                         MOVE SPACES              TO MASCARA DADOS
                         MOVE M                   TO M2
                         MOVE MV-POSIT-OUT (M2) TO P
                         MOVE SORTWK-REG (P: S)   TO DADOS   (K: S)
                         IF   Q = 1
                              MOVE ANTERIOR (P: S) TO DADOS (K: S)
                         END-IF
                         MOVE ALL "M"             TO MASCARA (K: S)
                         ADD  S                   TO K
                         ADD  1                   TO M2
                         IF   MV-TIPO (M2) = "d"
                              MOVE MV-TAMANHO   (M2) TO S
                              MOVE MV-POSIT-OUT (M2) TO P
                              MOVE SORTWK-REG (P: S)   TO DADOS   (K: S)
                              IF   Q = 1
                                   MOVE ANTERIOR (P: S) TO DADOS (K: S)
                              END-IF
                              MOVE ALL "D"             TO MASCARA (K: S)
                              ADD  S                   TO K
                         END-IF
                         COMPUTE S = K - 1
                    END-IF
                    IF   TIPO = "d"
                         MOVE "D"                 TO TIPO
                         MOVE SPACES              TO MASCARA DADOS
                         MOVE SORTWK-REG (P: S)   TO DADOS   (1: S)
                         IF   Q = 1
                              MOVE ANTERIOR (P: S) TO DADOS (1: S)
                         END-IF
                         MOVE ALL "D"             TO MASCARA (1: S)
                    END-IF
                    PERFORM 195-EDITAR THRU 195-99-FIM
           END-EVALUATE

           IF   OBJETO-TIPO (OBJETO) = "Z"
                INSPECT SALVA-DATA CONVERTING "DMAHS" TO "00000"
                IF ((TIPO = "D" OR "H")
                AND  ELEMENTO (1: 10) = SALVA-DATA)
                 OR((TIPO = "9" OR "V")
                AND  NUMERICO = 0)
                     MOVE SPACES TO ELEMENTO
                END-IF
           END-IF
           MOVE OBJETO-TAMANHO (OBJETO) TO S
           IF   RESULTADO = 0
                IF ARQUIVO
                   IF OBJETO NOT > TAGS
                      MOVE ELEMENTO (1: S)    TO CWSAVE-DATA (OBJETO)
                   END-IF
                ELSE
                   MOVE ELEMENTO (1: S)    TO CWIMPR-DETAIL (C: S)
                END-IF
           END-IF.

       190-99-FIM. EXIT.

       191-RELATOR.

           EVALUATE TRUE
               WHEN DATANAME (1: 6) = "PAGINA"
                    MOVE RELATOR-PAGINA            TO DADOS
                    MOVE LENGTH OF RELATOR-PAGINA  TO S
                    MOVE "9"                       TO TIPO
               WHEN DATANAME (1: 5) = "FOLHA"
                    MOVE RELATOR-FOLHA             TO DADOS
                    MOVE LENGTH OF RELATOR-FOLHA   TO S
                    MOVE "9"                       TO TIPO
               WHEN DATANAME (1: 4) = "DATA"
                    MOVE RELATOR-DATA              TO DADOS
                    MOVE LENGTH OF RELATOR-DATA    TO S
                    MOVE "D"                       TO TIPO
                    MOVE "AAAAMMDD"                TO MASCARA
               WHEN DATANAME (1: 4) = "HORA"
                    MOVE RELATOR-HORA              TO DADOS
                    MOVE LENGTH OF RELATOR-HORA    TO S
                    MOVE "H"                       TO TIPO
                    MOVE "HHMMSS"                  TO MASCARA
               WHEN DATANAME (1: 5) = "LINHA"
                    MOVE RELATOR-LINHA             TO DADOS
                    MOVE LENGTH OF RELATOR-LINHA   TO S
                    MOVE "9"                       TO TIPO
               WHEN DATANAME (1: 7) = "EMPRESA"
                    MOVE RELATOR-EMPRESA           TO DADOS
                    MOVE LENGTH OF RELATOR-EMPRESA TO S
                    MOVE "X"                       TO TIPO
               WHEN DATANAME (1: 7) = "SISTEMA"
                    MOVE RELATOR-SISTEMA           TO DADOS
                    MOVE LENGTH OF RELATOR-SISTEMA TO S
                    MOVE "X"                       TO TIPO
               WHEN DATANAME (1: 7) = "USUARIO"
                    MOVE RELATOR-E-MAIL            TO DADOS
                    MOVE LENGTH OF RELATOR-E-MAIL  TO S
                    MOVE "X"                       TO TIPO
               WHEN DATANAME (1: 6) = "E-MAIL"
                    MOVE RELATOR-E-MAIL            TO DADOS
                    MOVE LENGTH OF RELATOR-E-MAIL  TO S
                    MOVE "X"                       TO TIPO
               WHEN DATANAME (1: 10) = "DIA-SEMANA"
                    MOVE RELATOR-DIA-SEMANA TO DADOS (1: )
                    MOVE 7                  TO S
                    MOVE "X"                TO TIPO
               WHEN DATANAME (1: 12) = "SELECIONADOS"
                    MOVE RELATOR-SELECIONADOS (I) TO NUMERICO
                    MOVE NUMERICO (10: 9)  TO DADOS (1: )
                    MOVE 9                 TO S
                    MOVE "9"               TO TIPO
               WHEN DATANAME (1: 7) = "QUEBRA("
                    MOVE DATANAME (8: 1)     TO QX (1: 1)
                    MOVE RELATOR-QUEBRA (QX) TO NUMERICO
                    MOVE NUMERICO (10: 9)    TO DADOS (1: )
                    MOVE 9                   TO S
                    MOVE "9"                 TO TIPO
               WHEN DATANAME (1: 6) = "CODIGO"
                    MOVE RELATOR-CODIGO            TO DADOS
                    MOVE LENGTH OF RELATOR-CODIGO  TO S
                    MOVE "X"                       TO TIPO
               WHEN DATANAME (1: 9) = "DESCRICAO"
                    MOVE RELATOR-DESCRICAO           TO DADOS
                    MOVE LENGTH OF RELATOR-DESCRICAO TO S
                    MOVE "X"                         TO TIPO
           END-EVALUATE.

       191-99-FIM. EXIT.

       195-EDITAR.

           IF   TIPO = "+"
                MOVE "V" TO TIPO
           END-IF

           EVALUATE TRUE
               WHEN TIPO = "D"
                    MOVE    "A"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
                    MOVE    "M"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
                    MOVE    "D"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
               WHEN TIPO = "H"
                    MOVE    "S"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
                    MOVE    "M"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
                    MOVE    "H"               TO TIPO
                    PERFORM 196-AJUSTA-DATA THRU 196-99-FIM
               WHEN TIPO = "C"
                    MOVE 0                 TO NUMERICO
                    COMPUTE K = 18 - S + 1
                    MOVE DADOS (1: S)      TO NUMERICO (K: S)
                    MOVE NUMERICO          TO NUMERICO-M
                                              CGC-CIC
                    CALL "CWECNP"       USING CGC-CIC
                                              CGC-CIC-ED
                    MOVE NUMERICO-M        TO ELEMENTO
                    IF   CGC-CIC-ED NOT = ALL X"B0"
                         MOVE CGC-CIC-ED TO ELEMENTO
                    END-IF
               WHEN TIPO = "X"
                    MOVE DADOS (1: S)      TO ELEMENTO
               WHEN TIPO = "9" OR "V" OR "E"
                    INSPECT ELEMENTO CONVERTING "HMSDA" TO "99999"
                    MOVE 0                 TO NUMERICO
                    COMPUTE K = 18 - S + 1
                    MOVE DADOS (1: S)      TO NUMERICO (K: S)
                    IF  (NUMERICO POSITIVE
                    AND  ELEMENTO (S2: 1) = "-")
                    OR  (NUMERICO = 0
                    AND  ELEMENTO (S2: 1) = "-" OR "+")
                         MOVE SPACE TO ELEMENTO (S2: 1)
                    ELSE
                         IF   ELEMENTO (S2: 1) = "+"
                              MOVE "-" TO ELEMENTO (S2: 1)
                         END-IF
                    END-IF
                    MOVE NUMERICO          TO NUMERICO-M
                                              NUMERICO-I
                    PERFORM VARYING K2 FROM K BY 1 UNTIL K2 = 18
                                    OR NUMERICO-M (K2: 1) NOT = "0"
                            CONTINUE
                    END-PERFORM
                    IF DEC NOT = 0
                       PERFORM VARYING T FROM 1 BY 1
                               UNTIL ELEMENTO (T: 1) = "," OR SPACE
                               CONTINUE
                       END-PERFORM
                       IF   ELEMENTO (T: 1) = ","
                            COMPUTE K = 18 - DEC + 1
                            ADD 1 TO T
                            PERFORM DEC TIMES
                                    IF   ELEMENTO (T: 1) = "9" OR "Z"
                                         MOVE NUMERICO-M (K: 1)
                                           TO ELEMENTO (T: 1)
                                         ADD 1 TO K T
                                    END-IF
                            END-PERFORM
                       END-IF
                       MOVE 0            TO NUMERICO-I
                       COMPUTE K  = 18 - S + 1
                       COMPUTE S = S - DEC
                       COMPUTE K2 = 18 - S + 1
                       MOVE NUMERICO-M (K: S) TO NUMERICO-I (K2: S)
                       PERFORM VARYING K2 FROM 1 BY 1 UNTIL K2 = 18
                                    OR NUMERICO-I (K2: 1) NOT = "0"
                               CONTINUE
                       END-PERFORM
                    END-IF
                    PERFORM VARYING T FROM 1 BY 1
                            UNTIL ELEMENTO (T: 1) = "," OR " "
                            CONTINUE
                    END-PERFORM
                    SUBTRACT  1 FROM T
                    MOVE     18   TO S
                    PERFORM VARYING TZ
                            FROM 1
                              BY 1 UNTIL ELEMENTO (TZ: 1) = "9" OR SPACE
                            CONTINUE
                    END-PERFORM
                    PERFORM UNTIL T = 0
nena=>                   IF   S < K2
                              IF   TIPO = "V"
                              OR   T < TZ
                                   if   elemento (t: 1) = "Z"
                                        MOVE SPACES TO ELEMENTO (T: 1)
                                   else
                                        if   elemento (t: 1) = "9"
                                             move "0" to elemento (t: 1)
                                        end-if
                                   end-if
                              ELSE
                                   IF   ELEMENTO (T: 1) = "9"
                                        MOVE "0" TO ELEMENTO (T: 1)
                                   END-IF
                              END-IF
                         ELSE
                              IF   ELEMENTO (T: 1) = "9" OR "Z"
                                                  OR "*" OR "$"
                                   MOVE NUMERICO-I (S: 1)
                                     TO ELEMENTO   (T: 1)
                                   SUBTRACT 1 FROM S
                              END-IF
                         END-IF
                         SUBTRACT 1 FROM T
                    END-PERFORM
                    IF  TIPO = "9" OR "V"
                        PERFORM VARYING T FROM 1 BY 1 UNTIL T > TZ
                                IF   elemento (T: 2) = " ."
                                     MOVE SPACES TO elemento (T: 2)
                                END-IF
                        END-PERFORM
                    END-IF
                    IF  TIPO = "E" OR "9" OR "Z"
                        PERFORM VARYING T FROM 1 BY 1 UNTIL T > TZ
                                IF   elemento (T: 2) = "$$"
                                     MOVE " $" TO elemento (T: 2)
                                END-IF
                        END-PERFORM
                    END-IF
           END-EVALUATE.

       195-99-FIM. EXIT.

       196-AJUSTA-DATA.

           MOVE 8 TO S
           PERFORM VARYING K FROM 10 BY -1 UNTIL K = 0
                                              OR S = 0
                   IF   ELEMENTO (K: 1) = TIPO
                        PERFORM VARYING S FROM S BY -1 UNTIL S = 0
                                     OR MASCARA (S: 1) = TIPO
                                CONTINUE
                        END-PERFORM
                        IF   MASCARA (S: 1) = TIPO
                             MOVE DADOS (S: 1) TO ELEMENTO (K: 1)
                             SUBTRACT 1 FROM S
                        END-IF
                   END-IF
           END-PERFORM.

       196-99-FIM. EXIT.

       800-INICIAIS.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
      *    CALL X"91" USING X91-RESULT X91-F16 X91-PARAMETER
      *    IF   X91-PARAMETER < 2
      *         MOVE "Rodar sob o CWMENU do COBOLware 4.0" TO CWSEND-MSG
      *         CALL "CWSEND" USING PARAMETROS-CWSEND
      *         GOBACK
      *    END-IF
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT = "00"
                CALL "CWCONF" USING "ISAM"
                GOBACK
           END-IF

           MOVE "RL" TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-RELATOR       TO RELDIR
                MOVE CWCONF-RELATOR-YEAR  TO JAN-28
                MOVE CWCONF-RELATOR-COUNT TO RELCONT
           END-IF
           DISPLAY "CWRELCOUNT" UPON ENVIRONMENT-NAME
           MOVE SPACES TO OPT
           ACCEPT  OPT         FROM ENVIRONMENT-VALUE
                IF  OPT = SPACES
                    DISPLAY "cwrelcount" UPON ENVIRONMENT-NAME
                    ACCEPT  OPT          FROM ENVIRONMENT-VALUE
                END-IF
           INSPECT OPT CONVERTING MINUSCULAS TO MAIUSCULAS
           IF  OPT = "ON" OR "1"
               MOVE "1" TO RELCONT
           ELSE
               IF  OPT = "OFF" OR "0"
                   MOVE "0" TO RELCONT
               END-IF
           END-IF
           IF   RELDIR = SPACES
                MOVE "relator" TO RELDIR
           END-IF

           SET   CWTIME-REVERSED  TO TRUE
           SET   CWTIME-TODAY     TO TRUE
           CALL "CWTIME" USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL   TO RELATOR-DATA CWTIME-DATE
           MOVE CWTIME-TIME-FINAL   TO RELATOR-HORA
           SET   CWTIME-WEEK      TO TRUE
           CALL "CWTIME" USING PARAMETROS-CWTIME
           MOVE CWTIME-WEEK-CHAR    TO RELATOR-DIA-SEMANA
           CALL "CBL_CREATE_DIR" USING RELDIR
           STRING RELDIR    DELIMITED BY SPACE
                  "\FORMATOS" DELIMITED BY SIZE
                    INTO LB-FORMATOS
           INSPECT LB-FORMATOS CONVERTING "\" TO "/"
           STRING RELDIR     DELIMITED BY SPACE
                  "\COLUNAS" DELIMITED BY SIZE
                    INTO LB-CAMPOS
           INSPECT LB-CAMPOS CONVERTING "\" TO "/"
           EXEC COBOLware GETSystem
                 REPORT-COMPANY;RELATOR-EMPRESA
                 REPORT-APLICATION;RELATOR-SISTEMA
           END-EXEC
           CALL "CWGETU"       USING RELATOR-USUARIO TASK PROGRAMA "?"
           MOVE "PS"              TO CWCONF-CHAVE
           MOVE RELATOR-USUARIO   TO CWCONF-NOME
           SET CWSQLC-READ        TO TRUE
           SET CWSQLC-EQUAL       TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-RELATOR-IN  TO LIMITE-IN
                MOVE CWCONF-RELATOR-OUT TO LIMITE-OUT
                MOVE CWCONF-E-MAIL      TO RELATOR-E-MAIL
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
      *    MOVE SPACES TO USUARIO
           MOVE 0      TO I2

           OPEN INPUT FORMATOS
           IF   FS-FORMATOS > "09"
                CALL "CWISAM" USING ER-FORMATOS
                GOBACK
           END-IF
      *    CALL "CWACOR"         USING PARAMETROS-CWACOR
      *    MOVE CWACOR-F (9)        TO F
      *    MOVE CWACOR-B (9)        TO B
           DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           IF   TMP = SPACES
                DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                ACCEPT  TMP       FROM ENVIRONMENT-VALUE
           END-IF

           MOVE TASK (2: 5) TO TMP-LB (4: 5)
           IF   TMP = SPACE
                MOVE RELDIR TO TMP
           END-IF

           STRING TMP    DELIMITED BY SPACE
                  "\"    DELIMITED BY SIZE
                  TMP-LB DELIMITED BY SPACE
             INTO LB-SORTWK

           IF   CWUNIX-ON
                INSPECT LB-SORTWK CONVERTING "\" TO "/"
           END-IF

           OPEN OUTPUT SORTWK
           IF   FS-SORTWK NOT = "00"
                CALL "CWISAM" USING ER-SORTWK
                CLOSE FORMATOS
                GOBACK
           END-IF

           CLOSE SORTWK

           MOVE LK-RELATORIO TO RELATORIO
           INSPECT RELATORIO CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   RELATORIO = SPACES
                MOVE "R"            TO CWBOXF-WORK-AREA
                MOVE 08             TO CWBOXF-LINE
                MOVE 04             TO CWBOXF-COLUMN
                MOVE "CWREL6"       TO CWBOXF-PROGRAM
                MOVE SPACES         TO CWBOXF-OPTION
                MOVE 1              TO CWBOXF-ORDER
                MOVE 10             TO CWBOXF-VERTICAL-LENGTH
                MOVE 41             TO CWBOXF-HORIZONTAL-LENGTH
                MOVE 08             TO CWBOXF-STRING-1-LENGTH
                MOVE 39             TO CWBOXF-STRING-2-LENGTH
                MOVE 62             TO CWBOXF-COLOR-FRAME
                MOVE 62             TO CWBOXF-COLOR-BORDER
                MOVE "_Relat¢rios_" TO CWBOXF-TITLE
                CALL "CWBOXF" USING PARAMETROS-CWBOXF
                IF   CWBOXF-OPTION = SPACES
                     GOBACK
                ELSE
                     MOVE CWBOXF-OPTION TO RELATORIO
                END-IF
           END-IF

           OPEN INPUT CAMPOS
           IF   FS-CAMPOS > "09"
                CALL "CWISAM" USING ER-CAMPOS
                CLOSE FORMATOS
                GOBACK
           END-IF

           MOVE "1"       TO FORMATOS-REG
           MOVE RELATORIO TO FORMATOS-REPORT
                             CWIMPR-REPORT RELATOR-CODIGO
           MOVE 0         TO FORMATOS-OBJETO
           READ FORMATOS WITH LOCK
           IF   FS-FORMATOS > "09"
                MOVE SPACES TO CWSEND-SCREENS
                STRING 'Relat¢rio "'     DELIMITED BY SIZE
                       RELATORIO         DELIMITED BY SPACE
                       '" em manutená∆o' DELIMITED BY SIZE
                  INTO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                CLOSE FORMATOS CAMPOS
                GOBACK
           END-IF

      *    IF   FORMATOS-EXPORT
      *         MOVE SPACES TO CWSEND-SCREENS
      *         STRING 'Exportador "'     DELIMITED BY SIZE
      *                RELATORIO         DELIMITED BY SPACE
      *                '" em manutená∆o' DELIMITED BY SIZE
      *           INTO CWSEND-MSG
      *         CALL "CWSEND" USING PARAMETROS-CWSEND
      *         CLOSE FORMATOS CAMPOS
      *         GOBACK
      *    END-IF

           MOVE FORMATOS-DATANAME TO RELATOR-DESCRICAO CWIMPR-TITLE
           IF   PROGRAMA = "CWREL2"
                IF   FORMATOS-DATANAME NOT = SPACES
                AND (NOT CWUNIX-GUI)
                     CALL "CWMSGW" USING "050330" FORMATOS-DATANAME
                END-IF
                IF   NOT CWUNIX-GUI
                     MOVE  8 TO TPI
                     MOVE 78 TO TPC
                     STRING RELATORIO DELIMITED BY SPACE
                            "*"       DELIMITED BY SIZE
                                  INTO PGMR
                     PERFORM UNTIL TPI = 0
                             IF PGMR (TPI: 1) NOT = SPACE
                                DISPLAY TELA-TPI
                                SUBTRACT 1 FROM TPC
                             END-IF
                             SUBTRACT 1 FROM TPI
                     END-PERFORM
                     PERFORM UNTIL TPC < 71
                             DISPLAY TELA-TPC
                             SUBTRACT 1 FROM TPC
                     END-PERFORM
                END-IF
           END-IF

           STRING RELDIR     DELIMITED BY SPACE
                  "\PROMPTS" DELIMITED BY SIZE
            INTO LB-SAVEPROMPT

           INSPECT LB-SAVEPROMPT CONVERTING "\" TO "/"

           MOVE FORMATOS-PADRAO    TO PADRAO
           MOVE FORMATOS-TAMANHO   TO CAB
           IF ARQUIVO
              MOVE FORMATOS-ARQUIVO TO CWSAVE-FILE
              MOVE FORMATOS-DESTINO TO DESTINO
              MOVE FORMATOS-DELIM   TO CWSAVE-DELIMITER
              IF CWSAVE-FILE = SPACES
                 PERFORM TEST AFTER UNTIL FS-TEXTO = "00"
                    SET CWPATH-WITH-DIR     TO TRUE
                    SET CWPATH-WITH-DRIVES  TO TRUE
                    SET CWPATH-WITH-NEWDIR  TO TRUE
                    SET CWPATH-WITH-NEWFILE TO TRUE
                    MOVE "_Exportar_para:_"  TO CWPATH-TITLE
                    MOVE SPACES              TO CWPATH-DEFAULT
                                                CWPATH-PATH
                    CALL "CWPATH" USING PARAMETROS-CWPATH
                    MOVE CWPATH-FILE TO CWSAVE-FILE
                    IF CWSAVE-FILE = SPACES
                       EXIT PERFORM
                       GOBACK
                    END-IF
                    IF   CWUNIX-ON
                         INSPECT CWSAVE-FILE CONVERTING "\" TO "/"
                    END-IF
                    CALL "CWBINF" USING 'I' FS-TEXTO CWSAVE-FILE
                    IF   FS-TEXTO = "30" OR "35"
                         CALL "CWBINF" USING 'O' FS-TEXTO CWSAVE-FILE
                         IF   FS-TEXTO NOT = "00"
                              MOVE CWSAVE-FILE TO LB-TEXTO
                              CALL "CWISAM" USING ER-TEXTO
                         END-IF
                    ELSE
                         IF   FS-TEXTO < "10"
                              MOVE SPACES TO CWSEND-MSG
                              STRING "Arquivo: "     DELIMITED BY SIZE
                                      CWSAVE-FILE    DELIMITED BY SPACE
                                      " j† existe !" DELIMITED BY SIZE
                                    INTO CWSEND-MSG
                              MOVE SPACES      TO CWSEND-SCREENS
                              MOVE "~Sobrepor" TO CWSEND-SCREEN  (1)
                              MOVE "~Outro"    TO CWSEND-SCREEN  (2)
                              MOVE "~Cancelar" TO CWSEND-SCREEN  (3)
                              CALL "CWSEND" USING PARAMETROS-CWSEND
                              CALL "CWBINF" USING 'C' FS-TEXTO
                              EVALUATE CWSEND-OPTION
                                       WHEN 1 CALL "CWBINF" USING 'O'
                                                   FS-TEXTO CWSAVE-FILE
                                       WHEN 2 MOVE "99" TO FS-TEXTO
                                       WHEN 3 MOVE SPACES TO CWSAVE-FILE
                                              EXIT PERFORM
                               END-EVALUATE
                    END-IF
                 END-PERFORM
                 IF CWSAVE-FILE = SPACES
                    CLOSE FORMATOS CAMPOS
                    GOBACK
                 END-IF
                 CALL "CWBINF" USING 'C' FS-TEXTO CWSAVE-FILE
              ELSE
                 CALL 'CWFILE' USING CWSAVE-FILE
              END-IF
           ELSE
              MOVE FORMATOS-SIZE-PAGE TO SIZE-PAGE
              IF   COBOLware
                   IF   SIZE-PAGE < 99
                        MOVE SIZE-PAGE TO CWIMPR-SIZE-PAGE
                        IF    SIZE-PAGE = 0
                              MOVE 99 TO CWIMPR-SIZE-PAGE
                        END-IF
                   ELSE
                        SET CWIMPR-SET-SIZE-PAGE TO TRUE
                        MOVE SIZE-PAGE TO CWIMPR-SPECIAL-DATE-TIME
                        IF   NOT CWIMPR-END-PRINT
                             CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        END-IF
                        MOVE SPACES TO CWIMPR-TIME-REPORT
                   END-IF
              ELSE
                   MOVE 99 TO CWIMPR-SIZE-PAGE
              END-IF
           END-IF
           MOVE FORMATOS-DATANAME  TO CWIMPR-NOTE
           MOVE FORMATOS-PROVEDOR  TO PROVEDOR
           INSPECT PROVEDOR CONVERTING X"00" TO SPACE
           MOVE FORMATOS-MASCARA  TO MAPA-STATUS-LINHA
           MOVE FORMATOS-MASCARA(101: 50) TO MAPA-FLAGS
           MOVE FORMATOS-FLAGS    TO MAPA-FLAGS (51: 50)
           MOVE FORMATOS-OBJETOS  TO OBJETO-L

           MOVE "2"           TO FORMATOS-TIPO-REG
           START FORMATOS KEY NOT LESS FORMATOS-CHAVE
           MOVE  0            TO M

           PERFORM UNTIL FS-FORMATOS NOT = "00"
                   MOVE SPACES TO FORMATOS-REG
                   READ FORMATOS NEXT RECORD
                   CALL "CWATCH"
                   IF   FS-FORMATOS < "10"
                   AND((FORMATOS-TIPO-REG NOT = "2")
                    OR (FORMATOS-REPORT   NOT = RELATORIO))
                        MOVE "10" TO FS-FORMATOS
                   ELSE
                        MOVE FORMATOS-TIPO TO TIPO
                        INSPECT TIPO CONVERTING MINUSCULAS TO MAIUSCULAS
                        IF   TIPO = "C"
                             ADD  1 TO OPERACAO-MAX
                             MOVE FORMATOS-MASCARA
                               TO OPERACAO-ATRIBUTOS (OPERACAO-MAX)
                        END-IF
                   END-IF
           END-PERFORM

           MOVE 0             TO FORMATOS-OBJETO
           MOVE RELATORIO     TO FORMATOS-REPORT
           MOVE "2"           TO FORMATOS-TIPO-REG
           START FORMATOS KEY NOT LESS FORMATOS-CHAVE

           PERFORM UNTIL FS-FORMATOS NOT = "00"
                   MOVE SPACES TO FORMATOS-REG
                   READ FORMATOS NEXT RECORD
                   CALL "CWATCH"
                   IF   FS-FORMATOS < "10"
                   AND((FORMATOS-TIPO-REG NOT = "2")
                    OR (FORMATOS-REPORT   NOT = RELATORIO))
                        MOVE "10" TO FS-FORMATOS
                   END-IF
                   IF   FS-FORMATOS < "10"
                        IF   FORMATOS-TIPO = "q"
                             MOVE SPACE TO FORMATOS-TIPO
                        END-IF
                        MOVE FORMATOS-TIPO TO TIPO
                        INSPECT TIPO CONVERTING MINUSCULAS TO MAIUSCULAS
                        IF TIPO = "O" OR "Q" OR "P" OR "S" OR "C" OR "3"
                           EVALUATE TIPO
                                WHEN "3"
                                     IF MV-LAST = 0
                                        MOVE 1 TO MV-LAST
                                     END-IF
                                     ADD 1 TO TAGS OBJETO-R
                                     MOVE FORMATOS-STRING TO TAG-REG
                                     MOVE TAG-NAME TO CWSAVE-DATANAME
                                                                 (TAGS)
                                     MOVE TAG-LEN  TO CWSAVE-LEN (TAGS)
                                     IF TAG-DEC NOT = 0
                                        MOVE TAG-DEC  TO CWSAVE-DEC
                                                         (TAGS)
                                     END-IF
                                     IF TAG-NUMERICO
                                        SET CWSAVE-NUMERIC(TAGS) TO TRUE
                                     END-IF
                                     IF TAG-VALOR
                                        SET CWSAVE-SIGNAL (TAGS) TO TRUE
                                     END-IF
MEGA                                 IF  (FORMATOS-DATANAME (1: 7)
MEGA                                     NOT = "Prompt-")
MEGA                                 AND (FORMATOS-DATANAME (1: 8)
MEGA                                     NOT = "Relator-")
                                          PERFORM 805-REGISTRA-MOVE
                                              THRU 805-99-FIM
mega2                                ELSE
mega2                                     MOVE TAG-NAME
mega2                                       TO OBJETO-DATANAME
mega2                                          (OBJETO-R)
mega2                                     MOVE FORMATOS-DATANAME
mega2                                       TO OBJETO-DATANAME2
mega2                                          (OBJETO-R)
MEGA                                 END-IF
                                WHEN "O"
                                     PERFORM 804-REGISTRA-ORDENACAO
                                        THRU 804-99-FIM
                                WHEN "Q"
                                     IF  FORMATOS-LINHA < 100
                                     MOVE FORMATOS-COLUNA
                                       TO NIVEL-QUEBRA(FORMATOS-LINHA)
                                      IF  NIVEL-QUEBRA(FORMATOS-LINHA)
                                          > NIVEL-MAX
                                          MOVE NIVEL-QUEBRA
                                                (FORMATOS-LINHA)
                                          TO NIVEL-MAX
                                      END-IF
                                      END-IF
                                WHEN "P"
                                     MOVE FORMATOS-MASCARA
                                       TO PROMPT-ATRIBUTOS
                                          (FORMATOS-LINHA)
                                WHEN "C"
                                     PERFORM 850-CARREGA-OPERACOES
                                        THRU 850-99-FIM
                                WHEN "S"
                                     PERFORM 802-REGISTRA-CONDICAO
                                        THRU 802-99-FIM
                           END-EVALUATE
                        ELSE
                           IF   FORMATOS-LINHA > TOT
                                MOVE FORMATOS-LINHA TO TOT
                                MOVE 0              TO DESLOCAR
                           END-IF
                           ADD  DESLOCAR        TO FORMATOS-COLUNA
                           EVALUATE TRUE
                             WHEN FORMATOS-ALFANUMERICO
                                  MOVE ALL "X" TO ELEMENTO
                             WHEN FORMATOS-EDITADO
                                  MOVE FORMATOS-MASCARA TO ELEMENTO
                             WHEN FORMATOS-LITERAL
                                  MOVE FORMATOS-STRING  TO ELEMENTO
                             WHEN FORMATOS-NUMERICO
                                  MOVE ALL "9" TO ELEMENTO
                             WHEN FORMATOS-ESTILO
                                  MOVE "\"     TO ELEMENTO
                                  ADD  1       TO DESLOCAR
                                                  FORMATOS-TAMANHO
                                  PERFORM VARYING M
                                             FROM 1 BY 1 UNTIL M > 16
                                          IF   FORMATOS-DATANAME =
                                               ATRIBUTO (M)
                                               COMPUTE ASC = 96 + M
                                               MOVE ASC (1: 1)
                                                 TO ELEMENTO (2: 1)
                                          END-IF
                                  END-PERFORM
                                  IF   ELEMENTO (1: 2) = "\m"
                                       MOVE 1 TO SALTO(FORMATOS-LINHA)
                                       SUBTRACT 2 FROM DESLOCAR
                                       MOVE SPACES TO ELEMENTO
                                  END-IF
                           END-EVALUATE
                           ADD  1     TO OBJETO-R OBJETO-V
                           MOVE ELEMENTO TO MAPA-LIN (FORMATOS-LINHA)
                                   (FORMATOS-COLUNA: FORMATOS-TAMANHO)
                           COMPUTE LIN-SIZE = FORMATOS-COLUNA
                                            + FORMATOS-TAMANHO + 1
                           IF   LIN-SIZE > MAX-SIZE
                                MOVE LIN-SIZE TO MAX-SIZE
                           END-IF
                           IF   FORMATOS-LINHA > DET
                           AND (STATUS-LINHA (FORMATOS-LINHA)
                               = SPACE OR "K" or "t")
                                MOVE FORMATOS-LINHA TO DET
                           END-IF
                           MOVE FORMATOS-LINHA  TO OBJETO-RL(OBJETO-R)
                           MOVE FORMATOS-COLUNA TO OBJETO-RC(OBJETO-R)
                           MOVE FORMATOS-TIPO
                             TO OBJETO-TIPO (OBJETO-R)
                           MOVE FORMATOS-TAMANHO TO OBJETO-TAMANHO
                                                   (OBJETO-R)
                           IF   FORMATOS-LITERAL
                                MOVE "Texto"
                                  TO OBJETO-DATANAME (OBJETO-R)
                           ELSE
                                PERFORM 807-ORDEM THRU 807-99-FIM
                                MOVE FORMATOS-DATANAME
                                  TO OBJETO-DATANAME  (OBJETO-R)
                                MOVE FORMATOS-DATANAME2
                                  TO OBJETO-DATANAME2 (OBJETO-R)
                                IF  (FORMATOS-DATANAME (1: 7)
                                    NOT = "Prompt-")
                                AND (FORMATOS-DATANAME (1: 8)
                                    NOT = "Relator-")
                                    PERFORM 805-REGISTRA-MOVE
                                       THRU 805-99-FIM
                                END-IF
                           END-IF
                        END-IF
                   END-IF
           END-PERFORM

           CLOSE FORMATOS

           IF   DET = 0
           OR   DET = CAB
                IF   DET = 0
                     MOVE 1 TO DET
                END-IF
                PERFORM VARYING DET FROM DET BY 1
                         UNTIL  DET > 98
                            OR  MAPA-LIN (DET) = LOW-VALUES
                         CONTINUE
                END-PERFORM
                MOVE DET TO DET-NULL
           END-IF

           MOVE MAX-SIZE TO CWIMPR-HORIZONTAL-LENGTH

      *    IF   MAX-SIZE < 81
      *         SET CWIMPR-SIZE-080 TO TRUE
      *    ELSE
      *         IF   MAX-SIZE > 132
      *              SET CWIMPR-SIZE-220 TO TRUE
      *         END-IF
      *    END-IF

           IF   PROMPTS NOT = SPACES
                OPEN INPUT SAVEPROMPT
                IF   FS-SAVEPROMPT = "00"
                     MOVE SPACES          TO SAVEPROMPT-REG
                     MOVE RELATOR-USUARIO TO SAVEPROMPT-USUARIO
                     MOVE RELATORIO       TO SAVEPROMPT-RELATORIO
                     READ SAVEPROMPT IGNORE LOCK
                     IF   FS-SAVEPROMPT = "00"
                     AND  SAVEPROMPT-100 = PROMPT-100
                          MOVE SAVEPROMPT-DADOS TO PROMPTS
                     END-IF
                     CLOSE SAVEPROMPT
                END-IF
                MOVE 8 TO LIN-P
                MOVE 1 TO I Y
                PERFORM 810-EXIBE-PROMPTS THRU 810-99-FIM
                PERFORM UNTIL ESC OR I > 100
                        CALL "CWATCH"
                        PERFORM VARYING Y FROM 30 BY -1
                                 UNTIL Y = 1
                                       OR PROMPT-MENSAGEM (I) (Y: 1)
                                          NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        MOVE 3 TO COL-P
                        DISPLAY PROMPT-MENSAGEM (I)
                                LINE LIN-P COLUMN COL-P WITH SIZE Y
      *                              CONTROL CONTROL-COLORS
                        ADD Y TO COL-P
                        ADD 1 TO COL-P
                        MOVE PROVEDOR          TO CAMPOS-PROVEDOR
                        MOVE PROMPT-COLUNA (I) TO CAMPOS-DATANAME
                        PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                        PERFORM 820-ACEITAR-CONTEUDO THRU 820-99-FIM
                        IF   CURSOR-UP
                             IF   I > 1
                                  SUBTRACT 1 FROM I
                                  IF   LIN-P > 8
                                       SUBTRACT 1 FROM LIN-P
                                  ELSE
                                       MOVE I TO Y
                                       PERFORM 810-EXIBE-PROMPTS
                                          THRU 810-99-FIM
                                  END-IF
                             END-IF
                        ELSE
                             ADD 1 TO I LIN-P
                             IF   PROMPT-MENSAGEM (I) = SPACES
                                  MOVE 101 TO I
                             ELSE
                                  IF   LIN-P > 20
                                       MOVE 20 TO LIN-P
                                       COMPUTE Y = I - 12
                                       PERFORM 810-EXIBE-PROMPTS
                                        THRU 810-99-FIM
                                  END-IF
                             END-IF
                        END-IF
                END-PERFORM
                IF   ESC
                     CLOSE CAMPOS
                     GOBACK
                END-IF
           END-IF

           OPEN I-O SAVEPROMPT
           IF   FS-SAVEPROMPT > "10"
                OPEN OUTPUT SAVEPROMPT
           END-IF
           MOVE RELATOR-USUARIO TO SAVEPROMPT-USUARIO
           MOVE RELATORIO       TO SAVEPROMPT-RELATORIO
           READ SAVEPROMPT
           MOVE PROMPTS         TO SAVEPROMPT-DADOS
           PERFORM VARYING SZ-SAVEPROMPT FROM 9937 BY -1
                     UNTIL SZ-SAVEPROMPT = 37
                     OR (SAVEPROMPT-REG (SZ-SAVEPROMPT: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM

           IF   FS-SAVEPROMPT = "23"
                WRITE SAVEPROMPT-REG
           ELSE
                REWRITE SAVEPROMPT-REG
           END-IF

           CLOSE SAVEPROMPT

           PERFORM VARYING  ROW-NUMBER FROM 6 BY 1
                      UNTIL ROW-NUMBER > 20
                   CALL "CBL_READ_SCR_CHARS" USING SCREEN-POSITION
                                                    TEXTO-78
                                                    X"004E"
                   CALL "CWREL8"   USING TEXTO-78 (1:999) R "D"
           END-PERFORM

           OPEN I-O SORTWK
           PERFORM VARYING P FROM 1 BY 1
                   UNTIL P > CONDICAO-QTDE
                   IF   CONDICAO-ACAO (P) = "I"
                        MOVE 1 TO COM-INCLUSAO
                   END-IF
           END-PERFORM
           PERFORM 801-AJUSTA-PROMPT THRU 801-99-FIM
                   VARYING P FROM 1 BY 1
                     UNTIL P > 100
                       OR PROMPT-DATANAME (P) = SPACES

           PERFORM VARYING OBJETO-L FROM 1 BY 1
                      UNTIL OBJETO-L > OBJETO-R
                   MOVE OBJETO-DATANAME (OBJETO-L) TO FORMATOS-DATANAME
                   IF    FORMATOS-DATANAME (1: 7) = "Prompt-"
                         PERFORM VARYING M FROM 1 BY 1
                                   UNTIL M > 100
                                        OR PROMPT-DATANAME (M)
                                         = FORMATOS-DATANAME(8:)
                           CONTINUE
                   END-PERFORM
                   IF   PROMPT-DATANAME (M) = FORMATOS-DATANAME (8:)
                        MOVE M TO OBJETO-PROMPT (OBJETO-L)
                   END-IF
                END-IF
           END-PERFORM

           CANCEL "CWREL7"

           IF   RELCONT = "1"
           AND  (NOT CWUNIX-GUI)
                MOVE 10             TO CWBOXW-LINE
                MOVE 22             TO CWBOXW-COLUMN
                MOVE 07             TO CWBOXW-VERTICAL-LENGTH
                MOVE 36             TO CWBOXW-HORIZONTAL-LENGTH
                MOVE 62             TO CWBOXW-COLOR-FRAME
                MOVE 62             TO CWBOXW-COLOR-BORDER
                SET  CWBOXW-OPEN    TO TRUE
                CALL "CWBOXW"    USING PARAMETROS-CWBOXW
                SET  CWBOXW-POPUP   TO TRUE
                CALL "CWBOXW"    USING PARAMETROS-CWBOXW
                CALL "CWMSGW" USING "123811" "Processados"
                CALL "CWMSGW" USING "122611" PROCESSADOS-ED
                CALL "CWMSGW" USING "143812" "Selecionados"
                CALL "CWMSGW" USING "142611" SELECIONADOS-ED
                IF ARQUIVO
                   CALL "CWMSGW" USING "163809" "Gravados "
                ELSE
                   CALL "CWMSGW" USING "163809" "Impressos"
                END-IF
                CALL "CWMSGW" USING "162611" LISTADOS-ED
           END-IF

           CALL "CBL_SET_CSR_POS" USING X"FFFF"

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONDICAO-QTDE
                   IF  (CONDICAO-PROMPT  (I) NOT = 0)
                   OR  (CONDICAO-PROMPT2 (I) NOT = 0)
                   OR  CONDICAO-LITERAL  (I) = "L"
                       PERFORM 812-AJUSTA-ORDENADAS THRU 812-99-FIM
                               VARYING Y FROM 1 BY 1
                                 UNTIL Y > RELATOR-ORDENADAS
                   END-IF
           END-PERFORM.

       800-99-FIM. EXIT.

       801-AJUSTA-PROMPT.

           MOVE PROMPT-COLUNA (P) TO CAMPOS-DATANAME
           PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
           IF  FS-CAMPOS = "23"
               GO TO 801-99-FIM
           END-IF
           MOVE CAMPOS-TAMANHO TO PROMPT-TAMANHO (P)
           MOVE CAMPOS-DEC     TO PROMPT-DEC     (P)
           MOVE CAMPOS-TIPO    TO PROMPT-TIPO    (P)
           IF   NOT CAMPOS-DATA
                GO TO 801-99-FIM
           END-IF
           PERFORM 821-SELECIONA-PICTURE THRU 821-99-FIM
           MOVE M TO M3
           MOVE 0 TO D M A PD PM PA
           PERFORM VARYING E
                      FROM 1 BY 1 UNTIL E > LENGTH MASCARA
                  IF  MASCARA (E: 1) = "D"
                      ADD 1 TO D
                      IF   PD = 0
                           MOVE E TO PD
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "M"
                      ADD 1 TO M
                      IF   PM = 0
                           MOVE E TO PM
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "A"
                      ADD 1 TO A
                      IF   PA = 0
                           MOVE E TO PA
                      END-IF
                  END-IF
           END-PERFORM

           MOVE 1 TO DM

           IF   A NOT = 0
                IF   A = 2
                     MOVE PROMPT-CONTEUDO (P) (PA: A) TO ANO-WS (1: A)
                     IF  ANO-WS > JAN-28
                         MOVE "19" TO DATA-WS
                     ELSE
                         MOVE "20" TO DATA-WS
                     END-IF
                     ADD 2 TO DM
                END-IF
                MOVE PROMPT-CONTEUDO (P) (PA: A) TO DATA-WS (DM: A)
                ADD  A                           TO DM
           END-IF

           IF   M NOT = 0
                MOVE PROMPT-CONTEUDO (P) (PM: M) TO DATA-WS (DM: M)
                ADD  M                           TO DM
           END-IF

           IF   D NOT = 0
                MOVE PROMPT-CONTEUDO (P) (PD: D) TO DATA-WS (DM: D)
                ADD  D                           TO DM
           END-IF
           COMPUTE PROMPT-TAMANHO (P) = DM - 1
           MOVE DATA-WS TO PROMPT-CONTEUDO (P)
           MOVE M3 TO M.

       801-99-FIM. EXIT.

       802-REGISTRA-CONDICAO.

           IF   FORMATOS-LINHA > CONDICAO-QTDE
                MOVE FORMATOS-LINHA TO CONDICAO-QTDE
           END-IF
           MOVE FORMATOS-MASCARA TO CONDICAO-ATRIBUTOS (FORMATOS-LINHA)
           MOVE "X"              TO CONDICAO-TIPO      (FORMATOS-LINHA)

           MOVE CONDICAO-DATANAME (FORMATOS-LINHA) TO FORMATOS-DATANAME
                                     CONDICAO-DATANAME2 (FORMATOS-LINHA)
           MOVE LOW-VALUES TO CONDICAO-DATANAME (FORMATOS-LINHA)

           IF   FORMATOS-DATANAME (1: 7) = "Prompt-"
                PERFORM VARYING M FROM 1 BY 1
                          UNTIL M > 100
                          OR PROMPT-DATANAME (M) = FORMATOS-DATANAME(8:)
                        CONTINUE
                END-PERFORM
                IF   PROMPT-DATANAME (M) = FORMATOS-DATANAME (8:)
                     MOVE M TO CONDICAO-PROMPT (FORMATOS-LINHA)
                     MOVE PROMPT-TAMANHO (M)
                       TO CONDICAO-SIZE   (FORMATOS-LINHA)
                ELSE
                     MOVE SPACES TO CONDICAO-CONDICAO (FORMATOS-LINHA)
                     GO TO 802-99-FIM
                END-IF
           ELSE
                MOVE FORMATOS-DATANAME TO CAMPOS-DATANAME
                PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                IF  FS-CAMPOS = "23"
                    MOVE SPACES   TO CONDICAO-CONDICAO (FORMATOS-LINHA)
                    PERFORM 808-ALERTA-COLUMA THRU 808-99-FIM
                    GO TO 802-99-FIM
                END-IF
                MOVE CAMPOS-POSIT   TO CONDICAO-POSIT (FORMATOS-LINHA)
                MOVE CAMPOS-TAMANHO TO CONDICAO-SIZE  (FORMATOS-LINHA)
      *         IF   CAMPOS-DATANAME (1: 3) = "(=)"
      *              MOVE 0 TO CONDICAO-SIZE  (FORMATOS-LINHA)
      *         END-IF
                IF    NOT CAMPOS-ALFANUMERICO
                      MOVE "9" TO CONDICAO-TIPO (FORMATOS-LINHA)
                END-IF
                IF    CAMPOS-DATA
                      MOVE "D" TO CONDICAO-TIPO (FORMATOS-LINHA)
                      MOVE CAMPOS-MASCARA TO CONDICAO-M1(FORMATOS-LINHA)
                ELSE
                      IF  CAMPOS-DEC NOT = 0
                          MOVE CAMPOS-DEC TO CONDICAO-M1(FORMATOS-LINHA)
                      END-IF
                END-IF
                IF    CONDICAO-LITERAL (FORMATOS-LINHA) = "L"
                      MOVE    FORMATOS-LINHA     TO P
                      PERFORM 803-FORMATA-DATA THRU 803-99-FIM
                ELSE
                      IF    CAMPOS-DATA
                            MOVE    "S"                 TO COND
                            PERFORM 805-REGISTRA-MOVE THRU 805-99-FIM
                            MOVE COND TO CONDICAO-J1 (FORMATOS-LINHA)
                      END-IF
                END-IF
           END-IF

           IF   CONDICAO-LITERAL (FORMATOS-LINHA) = "L"
                MOVE CONDICAO-SIZE  (FORMATOS-LINHA)
                  TO CONDICAO-SIZE2 (FORMATOS-LINHA)
                GO TO 802-99-FIM
           END-IF

           MOVE CONDICAO-OPERANDO (FORMATOS-LINHA) TO FORMATOS-DATANAME

           IF   FORMATOS-DATANAME (1: 7) = "Prompt-"
                PERFORM VARYING M FROM 1 BY 1
                          UNTIL M > 100
                          OR PROMPT-DATANAME (M) = FORMATOS-DATANAME(8:)
                        CONTINUE
                END-PERFORM
                IF   PROMPT-DATANAME (M) = FORMATOS-DATANAME (8:)
                     MOVE M TO CONDICAO-PROMPT2 (FORMATOS-LINHA)
                     MOVE PROMPT-TAMANHO (M)
                       TO CONDICAO-SIZE2  (FORMATOS-LINHA)
                ELSE
                     MOVE SPACES TO CONDICAO-CONDICAO (FORMATOS-LINHA)
                     GO TO 802-99-FIM
                END-IF
           ELSE
                MOVE FORMATOS-DATANAME TO CAMPOS-DATANAME
                PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                IF  FS-CAMPOS = "23"
                    MOVE SPACES TO CONDICAO-CONDICAO (FORMATOS-LINHA)
                    PERFORM 808-ALERTA-COLUMA THRU 808-99-FIM
                    GO TO 802-99-FIM
                END-IF
                MOVE CAMPOS-POSIT   TO CONDICAO-POSIT2(FORMATOS-LINHA)
                MOVE CAMPOS-TAMANHO TO CONDICAO-SIZE2 (FORMATOS-LINHA)
                IF   CAMPOS-DATANAME (1: 3) = "(=)"
                AND  CAMPOS-DEC NOT = 0
                     MOVE CAMPOS-DEC TO CONDICAO-M2(FORMATOS-LINHA)
                END-IF
                IF    NOT CAMPOS-ALFANUMERICO
                      MOVE "9" TO CONDICAO-TIPO (FORMATOS-LINHA)
                END-IF
                IF    CAMPOS-DATA
                      MOVE "D" TO CONDICAO-TIPO (FORMATOS-LINHA)
                      MOVE "S" TO COND
                      MOVE CAMPOS-MASCARA TO CONDICAO-M2(FORMATOS-LINHA)
                      PERFORM 805-REGISTRA-MOVE THRU 805-99-FIM
                      MOVE COND TO CONDICAO-J2 (FORMATOS-LINHA)
                      IF  CAMPOS-DEC NOT = 0
                          MOVE CAMPOS-DEC TO CONDICAO-M2(FORMATOS-LINHA)
                      END-IF
                END-IF
           END-IF.

       802-99-FIM. EXIT.

       803-FORMATA-DATA.

           IF   NOT CAMPOS-DATA
                GO TO 803-99-FIM
           END-IF
           MOVE M      TO M4
           MOVE SPACES TO DATA-WS MASCARA
           PERFORM 821-SELECIONA-PICTURE THRU 821-99-FIM
           MOVE 0 TO D M A PD PM PA
           PERFORM VARYING E
                      FROM 1 BY 1 UNTIL E > LENGTH MASCARA
                  IF  MASCARA (E: 1) = "D"
                      ADD 1 TO D
                      IF   PD = 0
                           MOVE E TO PD
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "M"
                      ADD 1 TO M
                      IF   PM = 0
                           MOVE E TO PM
                      END-IF
                  END-IF
                  IF  MASCARA (E: 1) = "A"
                      ADD 1 TO A
                      IF   PA = 0
                           MOVE E TO PA
                      END-IF
                  END-IF
           END-PERFORM

           MOVE 1 TO DM

           IF   A NOT = 0
                IF   A = 2
                     MOVE CONDICAO-OPERANDO (P) (PA: A) TO ANO-WS (1: A)
                     IF  ANO-WS > JAN-28
                         MOVE "19" TO DATA-WS
                     ELSE
                         MOVE "20" TO DATA-WS
                     END-IF
                     ADD 2 TO DM
                END-IF
                MOVE CONDICAO-OPERANDO (P) (PA: A) TO DATA-WS (DM: A)
                ADD  A                             TO DM
           END-IF

           IF   M NOT = 0
                MOVE CONDICAO-OPERANDO (P) (PM: M) TO DATA-WS (DM: M)
                ADD  M                             TO DM
           END-IF

           IF   D NOT = 0
                MOVE CONDICAO-OPERANDO (P) (PD: D) TO DATA-WS (DM: D)
                ADD  D                             TO DM
           END-IF
           COMPUTE CONDICAO-SIZE2   (P) = DM - 1
           MOVE DATA-WS TO CONDICAO-OPERANDO (P)
           MOVE M4      TO M.

       803-99-FIM. EXIT.

       804-REGISTRA-ORDENACAO.

           MOVE FORMATOS-ORDENADAS TO RELATOR-ORDENADAS

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RELATOR-ORDENADAS
                   MOVE FORMATOS-ORDENADA (I) TO RELATOR-COLUNA (I)
                   MOVE LOW-VALUES            TO RELATOR-MENOR  (I)
                   MOVE HIGH-VALUES           TO RELATOR-MAIOR  (I)
           END-PERFORM

           MOVE 1                  TO MV-LAST

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RELATOR-ORDENADAS
                   MOVE    RELATOR-COLUNA (I) TO FORMATOS-DATANAME
                   PERFORM 805-REGISTRA-MOVE THRU 805-99-FIM
                   COMPUTE NIVEL-TAMANHO (I) = MV-LAST - 1
                   IF   FORMATOS-DESCENDE (I) = "D"
                        MOVE MV-POSIT-OUT (MV-OBJETOS) TO REVPOS
                        PERFORM MV-TAMANHO (MV-OBJETOS) TIMES
                                ADD  1      TO REV
                                MOVE REVPOS TO REVERSOES (REV)
                                ADD  1      TO REVPOS
                        END-PERFORM
                   END-IF
           END-PERFORM.

       804-99-FIM. EXIT.

       805-REGISTRA-MOVE.

           IF   FORMATOS-DATANAME = "Ordem"
                MOVE 0 TO CAMPOS-COLUNA
                          CAMPOS-POSIT
                          CAMPOS-DEC
                          CAMPOS-MASCARA
                MOVE 9 TO CAMPOS-TAMANHO
                          CAMPOS-TIPO
           ELSE
                MOVE SPACE             TO FLAG-ACM
                MOVE FORMATOS-DATANAME TO CAMPOS-DATANAME
                PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                IF   FS-CAMPOS = "23"
                     PERFORM 808-ALERTA-COLUMA THRU 808-99-FIM
                     GO TO 805-99-FIM
                END-IF
           END-IF

           IF  (COND NOT = "S")
           AND (TIPO NOT = "O")
                MOVE CAMPOS-DEC TO OBJETO-DEC (OBJETO-R)
           END-IF

           IF   CAMPOS-ACUMULADOR = "+"
                MOVE LENGTH OF CAMPOS-DATANAME TO M
                SUBTRACT 2 FROM M
                PERFORM UNTIL M = 1
                           OR CAMPOS-DATANAME (M: 3) = "[+]"
                        SUBTRACT 1 FROM M
                END-PERFORM
                MOVE SPACES TO CAMPOS-DATANAME (M: 3)
                PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                MOVE "+"    TO FLAG-ACM
           END-IF

           IF   CAMPOS-DATANAME (1: 3) = "(=)"
                MOVE 18  TO CAMPOS-TAMANHO
                MOVE 06  TO CAMPOS-DEC
                MOVE "=" TO CAMPOS-TIPO
           END-IF

           PERFORM VARYING M FROM 1 BY 1 UNTIL M > MV-OBJETOS
                               OR CAMPOS-COLUNA = MV-COLUNA (M)
                   CONTINUE
           END-PERFORM
           IF   OBJETO-R NOT = 0
                IF   COND = "S"
                AND (OBJETO-OUT (OBJETO-R) NOT = 0)
                AND (OBJETO-OUT (OBJETO-R) NOT = M)
                    ADD 1 TO OBJETO-R
                END-IF
                IF   M NOT > MV-OBJETOS
                     IF   MV-TIPO (M) = "j"
                          MOVE "j" TO COND
                     END-IF
                     MOVE M TO OBJETO-OUT (OBJETO-R)
                     IF   FLAG-ACM = "+"
                          IF   MV-ACM (M) = 0
                               ADD  1   TO ACM
                               MOVE ACM TO MV-ACM   (M)
                                           OBJETO-ACM (OBJETO-R)
                               IF   MV-TIPO  (M) = "="
                                    MOVE "@" TO MV-TIPO  (M)
                               ELSE
                                    MOVE "+" TO MV-TIPO  (M)
                               END-IF
                          ELSE
                               MOVE MV-ACM (M) TO OBJETO-ACM(OBJETO-R)
                          END-IF
                     END-IF
                     GO TO 805-99-FIM
                END-IF
           END-IF

           IF   CAMPOS-DATA
           AND (CAMPOS-MASCARA NOT = SPACES)
                PERFORM 806-REGISTRA-DATA THRU 806-99-FIM
                GO TO 805-99-FIM
           END-IF

           ADD 1                  TO MV-OBJETOS

           IF  (OBJETO-R NOT = 0)
           AND  OBJETO-OUT (OBJETO-R) = 0
           AND (FORMATOS-DATANAME NOT = "Ordem")
                MOVE MV-OBJETOS TO OBJETO-OUT (OBJETO-R)
           END-IF

           MOVE CAMPOS-COLUNA   TO MV-COLUNA    (MV-OBJETOS)
           MOVE CAMPOS-POSIT    TO MV-POSIT     (MV-OBJETOS)
           MOVE CAMPOS-TAMANHO  TO MV-TAMANHO   (MV-OBJETOS)
           MOVE CAMPOS-DEC      TO MV-DEC       (MV-OBJETOS)
           MOVE CAMPOS-MASCARA  TO MV-MASCARA   (MV-OBJETOS)
           MOVE CAMPOS-TIPO     TO MV-TIPO      (MV-OBJETOS)
           MOVE MV-LAST         TO MV-POSIT-OUT (MV-OBJETOS)
           ADD  CAMPOS-TAMANHO  TO MV-LAST
           IF   MV-LAST > SZ-SORTWK
                MOVE MV-LAST TO SZ-SORTWK
           END-IF
           IF   FLAG-ACM = "+"
                MOVE "+" TO MV-TIPO       (MV-OBJETOS)
                IF   CAMPOS-DATANAME (1: 3) = "(=)"
                     MOVE "@" TO MV-TIPO  (MV-OBJETOS)
                END-IF
                ADD  1   TO ACM
                MOVE ACM TO MV-ACM        (MV-OBJETOS)
                IF   OBJETO-R NOT = 0
                     MOVE ACM TO OBJETO-ACM (OBJETO-R)
                END-IF
           END-IF.

       805-99-FIM. EXIT.

       806-REGISTRA-DATA.

           MOVE M TO M5
           MOVE 0 TO D M A PD PM PA
           PERFORM VARYING E
                      FROM 1 BY 1 UNTIL E > LENGTH CAMPOS-MASCARA
                  IF  CAMPOS-MASCARA (E: 1) = "D"
                      ADD 1 TO D
                      IF   PD = 0
                           MOVE E TO PD
                      END-IF
                  END-IF
                  IF  CAMPOS-MASCARA (E: 1) = "M"
                      ADD 1 TO M
                      IF   PM = 0
                           MOVE E TO PM
                      END-IF
                  END-IF
                  IF  CAMPOS-MASCARA (E: 1) = "A"
                      ADD 1 TO A
                      IF   PA = 0
                           MOVE E TO PA
                      END-IF
                  END-IF
           END-PERFORM

           IF   A NOT = 0
                IF A = 2
                   ADD 1                  TO MV-OBJETOS
                   IF   OBJETO-R NOT = 0
                   AND  OBJETO-OUT (OBJETO-R) = 0
                        MOVE MV-OBJETOS TO OBJETO-OUT     (OBJETO-R)
                   END-IF
                   MOVE CAMPOS-COLUNA  TO MV-COLUNA   (MV-OBJETOS)
                   MOVE CAMPOS-POSIT   TO MV-POSIT    (MV-OBJETOS)
                   MOVE CAMPOS-TAMANHO TO MV-TAMANHO  (MV-OBJETOS)
                   MOVE MV-LAST      TO MV-POSIT-OUT(MV-OBJETOS)
                   ADD  2              TO MV-LAST
                   MOVE "j"            TO MV-TIPO       (MV-OBJETOS)
                                          COND
                   IF   MV-LAST > SZ-SORTWK
                        MOVE MV-LAST TO SZ-SORTWK
                   END-IF
                END-IF
                ADD 1                  TO MV-OBJETOS
                IF  (OBJETO-R NOT = 0)
                AND  OBJETO-OUT (OBJETO-R) = 0
                     MOVE MV-OBJETOS TO OBJETO-OUT     (OBJETO-R)
                END-IF
                COMPUTE PX = CAMPOS-POSIT + PA - 1
                MOVE CAMPOS-COLUNA     TO MV-COLUNA    (MV-OBJETOS)
                MOVE PX                TO MV-POSIT     (MV-OBJETOS)
                MOVE A                 TO MV-TAMANHO   (MV-OBJETOS)
                MOVE MV-LAST         TO MV-POSIT-OUT (MV-OBJETOS)
                ADD  A                 TO MV-LAST
                MOVE "a"               TO MV-TIPO       (MV-OBJETOS)
                IF   MV-LAST > SZ-SORTWK
                     MOVE MV-LAST TO SZ-SORTWK
                END-IF
           END-IF

           IF   M NOT = 0
                ADD 1                  TO MV-OBJETOS
                IF  (OBJETO-R NOT = 0)
                AND  OBJETO-OUT (OBJETO-R) = 0
                     MOVE MV-OBJETOS TO OBJETO-OUT     (OBJETO-R)
                END-IF
                COMPUTE PX = CAMPOS-POSIT + PM - 1
                MOVE CAMPOS-COLUNA     TO MV-COLUNA    (MV-OBJETOS)
                MOVE PX                TO MV-POSIT     (MV-OBJETOS)
                MOVE M                 TO MV-TAMANHO   (MV-OBJETOS)
                MOVE MV-LAST         TO MV-POSIT-OUT (MV-OBJETOS)
                ADD  M                 TO MV-LAST
                MOVE "m"               TO MV-TIPO       (MV-OBJETOS)
                IF   MV-LAST > SZ-SORTWK
                     MOVE MV-LAST TO SZ-SORTWK
                END-IF
           END-IF

           IF   D NOT = 0
                ADD 1                  TO MV-OBJETOS
                IF  (OBJETO-R NOT = 0)
                AND  OBJETO-OUT (OBJETO-R) = 0
                     MOVE MV-OBJETOS TO OBJETO-OUT     (OBJETO-R)
                END-IF
                COMPUTE PX = CAMPOS-POSIT + PD - 1
                MOVE CAMPOS-COLUNA     TO MV-COLUNA    (MV-OBJETOS)
                MOVE PX                TO MV-POSIT     (MV-OBJETOS)
                MOVE D                 TO MV-TAMANHO   (MV-OBJETOS)
                MOVE MV-LAST         TO MV-POSIT-OUT (MV-OBJETOS)
                ADD  D                 TO MV-LAST
                MOVE "d"               TO MV-TIPO       (MV-OBJETOS)
                IF   MV-LAST > SZ-SORTWK
                     MOVE MV-LAST TO SZ-SORTWK
                END-IF
           END-IF

           MOVE M5 TO M.

       806-99-FIM. EXIT.

       807-ORDEM.

           ON   1
                MOVE    FORMATOS-DATANAME   TO DATANAME
                MOVE    "Ordem"             TO FORMATOS-DATANAME
                PERFORM 805-REGISTRA-MOVE THRU 805-99-FIM
                MOVE    DATANAME           TO FORMATOS-DATANAME.

       807-99-FIM. EXIT.

       808-ALERTA-COLUMA.

           IF   (NOT FORMATOS-ESTILO)
           AND  (FORMATOS-DATANAME NOT = SPACES)
                 MOVE SPACES TO CWSEND-MSG
                                CWSEND-SCREENS
                 STRING "Coluna "         DELIMITED BY SIZE
                        FORMATOS-DATANAME DELIMITED BY SPACE
                        " n∆o encontrada" DELIMITED BY SIZE
                        INTO CWSEND-MSG
                 CALL "CWSEND" USING PARAMETROS-CWSEND
                 CANCEL PROVEDOR
                 CLOSE FORMATOS CAMPOS SORTWK
                 GOBACK
           END-IF.

       808-99-FIM. EXIT.

       810-EXIBE-PROMPTS.

           MOVE 8     TO LIN-W
           PERFORM 13 TIMES
                   DISPLAY SPACES
                                LINE LIN-W COLUMN 3 WITH SIZE 70
                   IF   Y < 100
                   AND  PROMPT-MENSAGEM (Y) NOT = SPACES
                        PERFORM VARYING SL FROM 30
                                 BY -1
                                 UNTIL PROMPT-MENSAGEM (Y) (SL: 1)
                                       NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        DISPLAY PROMPT-MENSAGEM (Y)
                                LINE LIN-W COLUMN 3 WITH SIZE SL
      *                            CONTROL CONTROL-COLORS
                        PERFORM 811-EXIBE-LINHA THRU 811-99-FIM
                        ADD 1 TO Y
                   END-IF
                   ADD 1 TO LIN-W
           END-PERFORM.

       810-99-FIM. EXIT.

       811-EXIBE-LINHA.

           MOVE I     TO I2
           MOVE Y     TO Y2
           MOVE LIN-P TO LIN-P2
           MOVE COL-P TO COL-P2
           MOVE Y     TO I
           MOVE LIN-W TO LIN-P
           MOVE 3     TO COL-P
           PERFORM VARYING Y FROM 30 BY -1
                    UNTIL Y = 1
                          OR PROMPT-MENSAGEM (I) (Y: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           ADD Y TO COL-P
           ADD 1 TO COL-P
           MOVE PROMPT-COLUNA (I) TO CAMPOS-DATANAME
           PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
           IF   CAMPOS-TAMANHO > 30
                MOVE 30 TO CAMPOS-TAMANHO
           END-IF
           EVALUATE TRUE
               WHEN CAMPOS-DATA
                    PERFORM 821-SELECIONA-PICTURE THRU 821-99-FIM
                    EVALUATE TRUE
                       WHEN MASCARA = "DDMM"
                            MOVE PROMPT-CONTEUDO (I) (1: 4) TO DT4(1: 4)
                            DISPLAY DT-4
                            MOVE "MMDD" TO PROMPT-MASCARA  (I)
                       WHEN MASCARA = "MMAAAA"
                            MOVE PROMPT-CONTEUDO (I) (1: 6) TO DT6(1: 6)
                            DISPLAY DT-6
                            MOVE "AAAAMM" TO PROMPT-MASCARA  (I)
                       WHEN MASCARA = "DDMMAAAA"
                            MOVE PROMPT-CONTEUDO (I) (1: 8) TO DT8(1: 8)
                            DISPLAY DT-8
                            MOVE "AAAAMMDD" TO PROMPT-MASCARA  (I)
                       WHEN OTHER
                            DISPLAY PROMPT-CONTEUDO (I)
                                    LINE LIN-P COLUMN COL-P
                                    WITH SIZE CAMPOS-TAMANHO
      *                          CONTROL CONTROL-COLORS
                    END-EVALUATE
               WHEN CAMPOS-ALFANUMERICO
                    DISPLAY PROMPT-CONTEUDO (I)
                           LINE LIN-P COLUMN COL-P
                           WITH SIZE CAMPOS-TAMANHO
      *                          CONTROL CONTROL-COLORS
               WHEN OTHER
                    CALL "CWREL7"   USING "D" PROMPT-CONTEUDO (I)
                                              CAMPOS-TIPO
                                              CAMPOS-TAMANHO
                                              CAMPOS-DEC
                                              LIN-P COL-P TECLA
           END-EVALUATE
           MOVE I2     TO I
           MOVE Y2     TO Y
           MOVE LIN-P2 TO LIN-P
           MOVE COL-P2 TO COL-P.

       811-99-FIM. EXIT.

       812-AJUSTA-ORDENADAS.

           EVALUATE TRUE
               WHEN CONDICAO-DATANAME2 (I) = RELATOR-COLUNA (Y)
               AND  CONDICAO-LITERAL   (I) = "L"
                    MOVE    CONDICAO-OPERANDO (I) TO START-VALOR
                    MOVE    CONDICAO-CONDICAO (I) TO CONDICAO
                    PERFORM 813-LIMITES         THRU 813-99-FIM
               WHEN CONDICAO-OPERANDO  (I) = RELATOR-COLUNA (Y)
               AND (CONDICAO-LITERAL   (I) NOT = "L")
               AND (CONDICAO-PROMPT    (I) NOT = 0)
                    MOVE    CONDICAO-PROMPT (I) TO Z
                    MOVE    PROMPT-CONTEUDO (Z) TO START-VALOR
                    MOVE    CONDICAO-CONDICAO (I) TO CONDICAO
                    EVALUATE CONDICAO
                        WHEN ">=" MOVE "<=" TO CONDICAO
                        WHEN "<=" MOVE ">=" TO CONDICAO
                        WHEN "<"  MOVE ">"  TO CONDICAO
                        WHEN ">"  MOVE "<"  TO CONDICAO
                    END-EVALUATE
                    PERFORM 813-LIMITES       THRU 813-99-FIM
               WHEN CONDICAO-DATANAME2 (I) = RELATOR-COLUNA (Y)
               AND (CONDICAO-LITERAL   (I) NOT = "L")
               AND (CONDICAO-PROMPT2   (I) NOT = 0)
                    MOVE    CONDICAO-PROMPT2 (I) TO Z
                    MOVE    PROMPT-CONTEUDO  (Z) TO START-VALOR
                    MOVE    CONDICAO-CONDICAO (I) TO CONDICAO
                    PERFORM 813-LIMITES        THRU 813-99-FIM
           END-EVALUATE.

       812-99-FIM. EXIT.

       813-LIMITES.

           IF  (CONDICAO = ">" OR ">=" OR "=")
           AND (START-VALOR < RELATOR-MENOR (Y)
           OR   RELATOR-MENOR (Y) = LOW-VALUES)
           AND  CONDICAO-ACAO (I) = "I"
                MOVE START-VALOR TO RELATOR-MENOR (Y)
           END-IF

           IF  (CONDICAO = ">" OR ">=" OR "=")
           AND (START-VALOR > RELATOR-MAIOR  (Y)
           OR   RELATOR-MAIOR  (Y) = HIGH-VALUES)
           AND  CONDICAO-ACAO (I) = "E"
                MOVE START-VALOR TO RELATOR-MAIOR  (Y)
           END-IF.

       813-99-FIM. EXIT.

       820-ACEITAR-CONTEUDO.

           MOVE CAMPOS-TAMANHO TO PROMPT-TAMANHO   (I)
           MOVE CAMPOS-DEC     TO PROMPT-DEC       (I)
           MOVE CAMPOS-TIPO    TO PROMPT-TIPO      (I)
           IF   CAMPOS-TAMANHO > 30
                MOVE 30 TO CAMPOS-TAMANHO
           END-IF
           EVALUATE TRUE
               WHEN CAMPOS-DATA
                    PERFORM 821-SELECIONA-PICTURE THRU 821-99-FIM
                    EVALUATE TRUE
                       WHEN MASCARA = "DDMM"
                            MOVE PROMPT-CONTEUDO (I) (1: 4) TO DT4(1: 4)
                            ACCEPT DT-4
                            MOVE DT4(1: 4) TO PROMPT-CONTEUDO (I) (1: 4)
                            MOVE 4         TO CAMPOS-TAMANHO
                       WHEN MASCARA = "MMAAAA"
                            MOVE PROMPT-CONTEUDO (I) (1: 6) TO DT6(1: 6)
                            ACCEPT DT-6
                            MOVE DT6(1: 6) TO PROMPT-CONTEUDO (I) (1: 6)
                            MOVE 6         TO CAMPOS-TAMANHO
                       WHEN MASCARA = "DDMMAAAA"
                            MOVE PROMPT-CONTEUDO (I) (1: 8) TO DT8(1: 8)
                            ACCEPT DT-8
                            MOVE DT8(1: 8) TO PROMPT-CONTEUDO (I) (1: 8)
                            MOVE 8         TO CAMPOS-TAMANHO
                       WHEN OTHER
                            DISPLAY PROMPT-CONTEUDO (I)
                                    LINE LIN-P COLUMN COL-P
                                    WITH SIZE CAMPOS-TAMANHO
      *                          CONTROL CONTROL-COLORS
                            ACCEPT  PROMPT-CONTEUDO (I)
                                    LINE LIN-P COLUMN COL-P
                                    WITH SIZE CAMPOS-TAMANHO UPDATE
      *                          CONTROL CONTROL-COLORS
                    END-EVALUATE
                    ACCEPT TECLA FROM ESCAPE KEY
               WHEN CAMPOS-ALFANUMERICO
                    ACCEPT PROMPT-CONTEUDO (I)
                           LINE LIN-P COLUMN COL-P
                           WITH SIZE CAMPOS-TAMANHO PROMPT UPDATE
      *                          CONTROL CONTROL-COLORS
                    ACCEPT TECLA FROM ESCAPE KEY
               WHEN OTHER
                    CALL "CWREL7"   USING "A" PROMPT-CONTEUDO (I)
                                              CAMPOS-TIPO
                                              CAMPOS-TAMANHO
                                              CAMPOS-DEC
                                              LIN-P COL-P TECLA
ld    *             CALL "CWREL7"   USING "D" PROMPT-CONTEUDO (I)
ld    *                                       CAMPOS-TIPO
ld    *                                       CAMPOS-TAMANHO
ld    *                                       CAMPOS-DEC
ld    *                                       LIN-P COL-P TECLA
           END-EVALUATE.

       820-99-FIM. EXIT.

       821-SELECIONA-PICTURE.

           MOVE SPACES TO MASCARA
           MOVE M TO M6
           MOVE 0 TO D M A
           PERFORM VARYING E
                      FROM 1 BY 1 UNTIL E > LENGTH CAMPOS-MASCARA
                  IF  CAMPOS-MASCARA (E: 1) = "D"
                      ADD 1 TO D
                  END-IF
                  IF  CAMPOS-MASCARA (E: 1) = "M"
                      ADD 1 TO M
                  END-IF
                  IF  CAMPOS-MASCARA (E: 1) = "A"
                      ADD 1 TO A
                  END-IF
           END-PERFORM

           MOVE 1 TO DM

           IF   D NOT = 0
                MOVE ALL "D" TO MASCARA (DM: D)
                ADD  D       TO DM
           END-IF

           IF   M NOT = 0
                MOVE ALL "M" TO MASCARA (DM: M)
                ADD  M       TO DM
           END-IF

           IF   A NOT = 0
                IF   A = 2
                     MOVE 4 TO A
                END-IF
                MOVE ALL "A" TO MASCARA (DM: A)
                ADD  A       TO DM
           END-IF

           MOVE M6 TO M.

       821-99-FIM. EXIT.

       840-AJUSTA-TAMANHO.

           PERFORM VARYING SZ-FORMATOS
                     FROM LENGTH FORMATOS-REG BY -1
                    UNTIL FORMATOS-REG (SZ-FORMATOS: 1) NOT  = SPACE
                   CONTINUE
           END-PERFORM.

       840-99-FIM. EXIT.

       850-CARREGA-OPERACOES.

           ADD  1 TO OP2
           MOVE 0 TO OPERACAO-MEMORIA (OP2)

           MOVE OPERACAO-OP-1LIT   (OP2) TO OPERACAO-CHECK
           MOVE OPERACAO-OPERANDO1 (OP2) TO OPERACAO-OPERANDO
           PERFORM 860-CHECK-OPERACAO THRU 860-99-FIM
           MOVE OPERACAO-OPERANDO  TO OPERACAO-OPERANDO1 (OP2)

           MOVE OPERACAO-OP-2LIT   (OP2) TO OPERACAO-CHECK
           MOVE OPERACAO-OPERANDO2 (OP2) TO OPERACAO-OPERANDO
           PERFORM 860-CHECK-OPERACAO     THRU 860-99-FIM
           MOVE OPERACAO-OPERANDO  TO OPERACAO-OPERANDO2 (OP2).

       850-99-FIM. EXIT.

       860-CHECK-OPERACAO.

           IF   OPERACAO-CHECK = "$"
                MOVE ALL "0" TO OPERACAO-WORK
                MOVE 0       TO V S
                PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 30
                      OR OPERACAO-OPERANDO (I: 1) = ","
                         CONTINUE
                END-PERFORM
                IF  I < 31
                    MOVE I TO V
                END-IF
                PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 30
                      OR OPERACAO-OPERANDO (I: 1) = "-"
                         CONTINUE
                END-PERFORM
                IF  I < 31
                    MOVE 1 TO S
                END-IF
                IF  V NOT = 0
                    ADD  1  TO V
                    MOVE 12 TO Y
                    PERFORM VARYING I FROM V BY 1
                              UNTIL OPERACAO-OPERANDO (I: 1) NOT NUMERIC
                            ADD 1 TO Y
                            MOVE OPERACAO-OPERANDO (I: 1)
                              TO OPERACAO-WORK (Y: 1)
                    END-PERFORM
                END-IF
                PERFORM VARYING V FROM 1 BY 1
                          UNTIL OPERACAO-OPERANDO (V: 1) NOT NUMERIC
                        CONTINUE
                END-PERFORM
                MOVE 13 TO Y
                SUBTRACT 1 FROM V
                PERFORM VARYING I FROM V BY -1
                          UNTIL I = 0
                          IF Y > 1
                             SUBTRACT 1 FROM Y
                             MOVE OPERACAO-OPERANDO (I: 1)
                                TO OPERACAO-WORK (Y: 1)
                          END-IF
                END-PERFORM
                MOVE OPERACAO-WORK TO OPERACAO-OPERANDO
                IF  S = 1
                    COMPUTE OPERACAO-LIT = OPERACAO-LIT * -1
                END-IF
           ELSE
                MOVE OPERACAO-OPERANDO TO CAMPOS-DATANAME
                PERFORM 101-LER-PRONAMREL THRU 101-99-FIM
                IF   FS-CAMPOS = "23"
                     PERFORM 808-ALERTA-COLUMA THRU 808-99-FIM
                     GO TO 860-99-FIM
                ELSE
                     IF   OPERACAO-OPERANDO (1: 3) = "(=)"
                          MOVE SPACES TO OPERACAO-OPERANDO
                          STRING "(" CAMPOS-POSIT ")" DELIMITED BY SIZE
                            INTO OPERACAO-OPERANDO
                     ELSE
                          MOVE SPACES TO OPERACAO-OPERANDO
                          STRING CAMPOS-POSIT
                                 CAMPOS-TAMANHO
                                 CAMPOS-DEC DELIMITED BY SIZE
                            INTO OPERACAO-OPERANDO
                     END-IF
                END-IF
           END-IF.

       860-99-FIM. EXIT.

       900-FINAIS.

           IF   ARQUIVO
                IF DESTINO NOT = SPACES
                   MOVE 0 TO Y
                   PERFORM VARYING I FROM 1 BY 1
                    UNTIL I > LENGTH DESTINO
                     ADD 1 TO Y
                     IF DESTINO (I: 1) = '$'
                        MOVE CWSAVE-FILE TO CWEXEC-COMMAND (Y:)
                        PERFORM VARYING Y FROM Y BY 1
                                UNTIL CWEXEC-COMMAND (Y: I) = SPACE
                                CONTINUE
                        END-PERFORM
                        SUBTRACT 1 FROM Y
                     ELSE
                        MOVE DESTINO (I: 1) TO CWEXEC-COMMAND (Y: 1)
                     END-IF
                   END-PERFORM
                END-IF
                SET CWSAVE-CLOSE TO TRUE
                CALL 'CWSAVE' USING PARAMETROS-CWSAVE
                IF PARAR
                   CALL 'CBL_DELETE_FILE' USING CWSAVE-FILE
                ELSE
                   IF DESTINO NOT = SPACES
                      set CWEXEC-NOWARNING  to true
                      set CWEXEC-ASSYNCRONE to true
                      CALL "CWEXE2" USING  PARAMETROS-CWEXEC
                   END-IF
                END-IF
           ELSE
                IF   NOT CWIMPR-END-PRINT
                     SET CWIMPR-CLOSE TO TRUE
                     CALL "CWIMPR" USING PARAMETROS-CWIMPR
                END-IF
           END-IF

           IF   RELCONT = "1"
           AND  (NOT CWUNIX-GUI)
                CALL "CBL_WRITE_SCR_CHARS" USING X"0F19"
                                     LISTADOS-ED X"000B"
           END-IF

           CANCEL PROVEDOR
           CANCEL "CWREL8"
           CLOSE SORTWK FORMATOS CAMPOS.

       900-99-FIM. EXIT.
       END PROGRAM CWREL2.
