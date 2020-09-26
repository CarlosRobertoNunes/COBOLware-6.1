      $SET NOWRITELOCK CALLFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/05/1999.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 1 - Editor de formatos                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAMPOS.SEL.
           COPY FORMATOS.SEL.
           COPY CABS.SEL.

           SELECT HELP    ASSIGN TO DISK
                  ORGANIZATION   IS BINARY SEQUENTIAL
                  LOCK MODE      IS EXCLUSIVE
                  FILE STATUS    IS FS-HELP.

           SELECT LISTA   ASSIGN TO DISK
                  ORGANIZATION   IS LINE SEQUENTIAL
                  LOCK MODE      IS EXCLUSIVE
                  FILE STATUS    IS FS-LISTA.

       DATA DIVISION.
       FILE SECTION.

       COPY CAMPOS.FD.
       COPY FORMATOS.FD.
       COPY CABS.FD.

       FD  HELP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HELP.

       01  HELP-REG               PIC X(001).

       FD  LISTA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LISTA.

       01  LISTA-REG               PIC X(046).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 LEGENDA              PIC  X(065) VALUE "[esc]-Sa¡da
      -       "              Cabe‡alho  Detalhe  Quebra  Total".
           05 WORKNAME             PIC  X(030) VALUE SPACES.
           05 FLAG-RODAPE          PIC  X(002) VALUE '00'.
           05 FLAG-RODAPE-A        PIC  X(002) VALUE '22'.
           05 LINHA-RODAPE         PIC  9(002) VALUE 23.
           05 TTELA                PIC  9(002) VALUE 14.
           05 DELETAR-CABS         PIC  9(001) VALUE 0.
           05 SEQ                  PIC  9(004) VALUE 0.
           05 COLADOS              PIC  9(004) VALUE 0.
           05 PROCESSADOS          PIC  9(004) VALUE 0.
           05 RECORTADOS           PIC  9(004) VALUE 0.
           05    MARCADOS          PIC  9(004) VALUE 0.
           05 DESMARCADOS          PIC  9(004) VALUE 0.
           05 NOME-ARQUIVO         PIC  X(050) VALUE SPACES.
           05 DESTINO              PIC  X(050) VALUE SPACES.
           05 DELIM                PIC  X(001) VALUE '|'.
           05 UNICO                PIC  X(030) VALUE SPACES.
           05 PONTO                PIC  9(004) VALUE 1.
           05 PONTO-A              PIC  9(004) VALUE 1.
           05 T                    PIC  9(004) VALUE 0.
           05 TD                   PIC  9(004) VALUE 0.
           05 OP-DEC               PIC  9(002) VALUE 0.
           05 FSSERVER             PIC  X(050) VALUE SPACES.
           05 PERC-TIT             PIC  X(050) VALUE SPACES.
           05 COLUNA               PIC  X(030) VALUE SPACES.
           05 L8                   PIC  9(002) VALUE 8.
           05 L9                   PIC  9(002) VALUE 9.
           05 L22                  PIC  9(002) VALUE 22.
           05 TOPO                 PIC  9(002) VALUE 8.
           05 TOPO2.
              10 SETE              PIC  9(002) VALUE 7.
              10                   PIC  X(004) VALUE "05..".
           05 QUINZE               PIC  9(002) VALUE 15.
           05 MARGIN               PIC  9(002) VALUE 3.
           05 BOT                  PIC  9(002) VALUE 22.
           05 BOTM                 PIC  9(002) VALUE 86.
           05 ALTURA               PIC  9(002) VALUE 14.
           05 ALTURA2              PIC  9(002) VALUE 14.
           05 NO-GET-GUI           PIC  9(001) VALUE 0.
           05 MULTI-USER           PIC  9(001) VALUE 0.
           05 RELATOR-RESTRICT     PIC  X(003) VALUE SPACES.
           05 W.
              10 LW                PIC  9(002) VALUE 0.
              10 CW                PIC  9(002) VALUE 0.
              10 SW                PIC  9(002) VALUE 0.
           05 WORK-SINAL           PIC  X(001) VALUE SPACE.
           05 WORK-NUMBER          PIC  X(030) VALUE SPACES.
           05 WORK-NUMBER2         PIC  X(030) VALUE SPACES.
           05 WN                   PIC  9(002) COMP-X VALUE 0.
           05 WN2                  PIC  9(002) COMP-X VALUE 0.
           05 FIM-OP               PIC  9(002) COMP-X VALUE 0.
           05 NUM-OP               PIC  9(002) COMP-X VALUE 0.
           05 ESPACOS              PIC  X(080) VALUE SPACES.
           05 POP                  PIC  X(080) VALUE SPACES.
           05 RETURN-STATUS        PIC  9(002) COMP-X VALUE 0.
           05 SIZE-OLD-DIR         PIC  9(002) COMP-X VALUE 50.
           05 OPEN-MASK            PIC  9(001) VALUE 0.
           05 WORK-OPERACOES       PIC  9(001) VALUE 0.
           05 OLD-DRIVE            PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY        PIC  X(050) VALUE SPACES.
           05 X91-F15              PIC  9(002) COMP-X VALUE 15.
           05 CURPOS.
              10 CURPOS-LIN        PIC  9(002) VALUE 0.
              10 CURPOS-COL        PIC  9(002) VALUE 0.
           05 REDEFINES CURPOS.
              10 CURPOS-HP         PIC  9(004) COMP-X.
              10 CURPOS-VP         PIC  9(004) COMP-X.
           05 CURPOS-SAVE.
              10 CURPOS-LINS       PIC  9(002) VALUE 0.
              10 CURPOS-COLS       PIC  9(002) VALUE 0.
           05 RELOGO               PIC  X(003) VALUE SPACES.
           05 CAB-ON               PIC  9(001) VALUE 0.
           05 CAB-I                PIC  9(004) COMP-X VALUE 0.
           05 ATT                  PIC  X(020) VALUE SPACES.
           05 CORES                PIC  X(256) VALUE SPACES.
           05 MOLDURAS             PIC  X(072) VALUE SPACES.
           05 X91-RESULT           PIC  9(002) COMP-X VALUE 0.
           05 X91-F16              PIC  9(002) COMP-X VALUE 16.
           05 X91-PARAMETER        PIC  9(002) COMP-X VALUE 0.
           05 HORA.
              10 HH                PIC  9(002) VALUE 0.
              10 MM                PIC  9(002) VALUE 0.
              10 SS                PIC  9(002) VALUE 0.
           05 RELDIR               PIC  X(050) VALUE SPACES.
           05 CURDIR               PIC  X(050) VALUE SPACES.
           05 KA                   PIC  9(003) VALUE 200.
           05 KB                   PIC  9(001) VALUE 0.
           05 FLAG-ACM             PIC  X(001) VALUE SPACE.
           05 E                    PIC  9(002) VALUE 0.
           05 PADRAO               PIC  9(001) VALUE 0.
              88 COBOLware                     VALUE 1.
              88 LIVRE                         VALUE 2.
              88 ARQUIVO                       VALUE 3.
           05 PADRAO-L             PIC  X(009) VALUE 'relat¢rio'.
           05 VALUE 'COBOLwareLivre    Arquivo  '.
              10 PADRAO-TXT        PIC  X(009) OCCURS 3.
           05 CR7                  PIC  9(002) VALUE 46.
           05 E2                   PIC  9(002) VALUE 0.
           05 D                    PIC  9(001) VALUE 0.
           05 M                    PIC  9(001) VALUE 0.
           05 A                    PIC  9(001) VALUE 0.
           05 DT4                  PIC  9(004) VALUE 0.
           05 DT6                  PIC  9(006) VALUE 0.
           05 DT8                  PIC  9(008) VALUE 0.
           05 SIZE-PAGE     COMP-X PIC  9(003) VALUE 59.
           05 SAVE-WORK-AREA       PIC  X(030) VALUE SPACES.
           05 OPERADOR             PIC  X(030) VALUE SPACES.
           05 TASK                 PIC  9(006) VALUE 0.
           05 PROGRAMA             PIC  X(008) VALUE SPACES.
           05 ACENTOS              PIC  X(36) VALUE
              " ‚¡¢£…Š•—„”ƒˆ“‡†‹Ÿ–‘’˜©Ž™š‰Œ€äÆ".
           05 SEM-ACENTOS          PIC  X(36) VALUE
              "AEIOUAEIOUAOUAEOCAEIOUAEIOUAOUAEOCOA".
           05 LL-CARACTERS         PIC  X(074) VALUE SPACES.
           05 LL-ATTR              PIC  X(074) VALUE SPACES.
           05 TIPO-ED              PIC  X(008) VALUE SPACES.
           05 SAVE-LIN             PIC  X(078) VALUE SPACES.
           05 MAPA-LIN-M-99        PIC  X(500) VALUE SPACES.
           05 MAPA-LIN-99          PIC  X(500) VALUE SPACES.
           05 NIVEL-QUEBRA-99      PIC  9(001) VALUE 0.
           05 STATUS-LINHA-99      PIC  X(001) VALUE SPACE.
           05 SAVE-CURSOR          PIC  X(002) VALUE SPACES.
           05 SAVE-CURSOR2         PIC  X(002) VALUE SPACES.
           05 AUTO-COLUNA          PIC  9(001) VALUE 0.
           05 SO-DETALHE           PIC  9(001) VALUE 0.
           05 APAGOU-DATANAME      PIC  9(001) VALUE 0.
           05 LENAME               PIC  9(002) VALUE 0.
           05 MASCARA              PIC  X(030) VALUE SPACES.
           05 MASCARA-E            PIC  X(050) VALUE SPACES.
           05 TEST-TAMANHO         PIC  9(003) VALUE 0.
           05 TAMANHO-E            PIC  9(003) VALUE 0.
           05 TAMANHO-ED           PIC  Z(003) VALUE 0.
           05 OBS                  PIC  X(053) VALUE SPACES.
           05 OBS-ATTR             PIC  X(080) VALUE ALL X"0A".
           05 CQS                  PIC  9(001) VALUE 0.
           05 CAMPO                PIC  9(002) VALUE 0.
           05 ERRO                 PIC  9(001) VALUE 0.
           05 INCLUINDO            PIC  9(001) VALUE 0.
           05 L                    PIC  9(002) VALUE 0.
           05 I                    PIC  9(008) VALUE 0 COMP-X.
           05 O                    PIC  9(008) VALUE 0 COMP-X.
           05 Y                    PIC  9(004) VALUE 0 COMP-X.
           05 R                    PIC  9(004) VALUE 0 COMP-X.
           05 R2                   PIC  9(004) VALUE 0 COMP-X.
           05 I-SAVE               PIC  9(008) VALUE 0 COMP-X.
           05 K                    PIC  9(004) VALUE 0.
           05 LL                   PIC  9(002) VALUE 0.
           05 K-ED                 PIC  Z(004) VALUE 0.
           05 VAZIO                PIC  X(001) VALUE SPACES.
           05 RELATORIO            PIC  X(007) VALUE SPACES.
           05 OPT-BOX              PIC  9(003) VALUE 0.
           05 CAB                  PIC  9(003) VALUE 0.
           05 DET                  PIC  9(003) VALUE 0.
           05 TOT                  PIC  9(003) VALUE 0.
           05 TOT2                 PIC  9(003) VALUE 0.
           05 PROP                 PIC  X(001) VALUE SPACE.
           05 RESTO                PIC  9(002) VALUE 0.
           05 MODULOS              PIC  9(002) VALUE 0.
           05 COLUNAS              PIC  9(004) VALUE 0.
           05 MAX-NAME             PIC  9(002) VALUE 30.
           05 MAIOR-NOME           PIC  9(002) VALUE 8.
           05 RC                   PIC  9(002) COMP-X VALUE 0.
           05 AR                   PIC  9(001) VALUE 0.
           05 VAGO                 PIC  9(001) VALUE 0.
           05 KEY-STATUS           PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-DATA                              VALUE LOW-VALUES.
              10 MOUSE-EVENT-TYPE  PIC  9(004) COMP-X.
              10 MOUSE-EVENT-TIME  PIC  9(008) COMP-X.
              10 MOUSE-EVENT-ROW   PIC  9(004) COMP-X.
              10 MOUSE-EVENT-COL   PIC  9(004) COMP-X.
           05 MOUSE-READ-TYPE      PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-HANDLE         PIC  X(004) COMP-X VALUE 1.
           05 MOUSE-BUTTONS        PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION.
              10 ROW-MOUSE         PIC  9(004) COMP-X.
              10 COLUMN-MOUSE      PIC  9(004) COMP-X.
           05 MOUSE-POSITION-A.
              10 ROW-MOUSE-A       PIC  9(004) COMP-X.
              10 COLUMN-MOUSE-A    PIC  9(004) COMP-X.
           05 CURSOR-POSITION-E    PIC  X(002).
           05 CURSOR-POSITION-A    PIC  X(002).
           05 CURSOR-POSITION.
              10 CURSOR-LIN        PIC  9(002) COMP-X VALUE 00.
              10 CURSOR-COL        PIC  9(002) COMP-X VALUE 00.
           05 CURSOR-POSITION-S.
              10 CURSOR-LIN-S      PIC  9(002) COMP-X VALUE 00.
              10 CURSOR-COL-S      PIC  9(002) COMP-X VALUE 00.
           05 SZ-FORMATOS          PIC  9(003) VALUE 0.
           05 ER-FORMATOS.
              10 FS-FORMATOS       PIC  X(002) VALUE "00".
              10 LB-FORMATOS       PIC  X(255) VALUE SPACES.
           05 ER-LISTA.
              10 FS-LISTA          PIC  X(002) VALUE "00".
              10 LB-LISTA          PIC  X(100) VALUE '$TEMP/cwrel1.###'.
           05 ER-TEXTO.
              10 FS-TEXTO          PIC  X(002) VALUE "00".
              10 LB-TEXTO          PIC  X(100) VALUE SPACES.
           05 ER-CAMPOS.
              10 FS-CAMPOS         PIC  X(002) VALUE "00".
              10 LB-CAMPOS         PIC  X(255) VALUE SPACES.
           05 ER-CABS.
              10 FS-CABS           PIC  X(002) VALUE "00".
              10 LB-CABS           PIC  X(255) VALUE "CABS".
           05 ER-HELP.
              10 FS-HELP           PIC  X(002) VALUE "00".
              10 LB-HELP           PIC  X(255) VALUE SPACES.
           05 TMP                  PIC  X(050) VALUE SPACES.
           05 TMP-LB               PIC  X(012) VALUE "CW600000.TMP".
           05 STATUS-TECLA         PIC  9(001) VALUE 0.
              88 TECLA-COMUM                   VALUE 0.
              88 TECLA-ESPECIAL                VALUE 1.
           05 ELEMENTO             PIC  X(500) VALUE SPACES.
           05 ARRASTADO            PIC  X(500) VALUE SPACES.
           05 TIPO                 PIC  X(001).
           05 DATANAME             PIC  X(030)        VALUE SPACES.
           05 DATANAME2            PIC  X(030)        VALUE SPACES.
           05 DESCENDE             PIC  X(001)        VALUE SPACE.
           05 NAME-DELETED         PIC  X(030)        VALUE SPACES.
           05 NAME-OLD             PIC  X(030)        VALUE SPACES.
           05 TAMANHO              PIC  9(002) COMP-X VALUE 0.
           05 VARIANTE-M           PIC  9(001)        VALUE 1.
           05 OBJETO-M             PIC  9(002) COMP-X VALUE 0.
           05 OBJETO-D             PIC  9(002) COMP-X VALUE 0.
           05 OBJETO-L             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-MW            PIC  9(002) COMP-X VALUE 0.
           05 OBJETO-T             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-T1            PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-T2            PIC  9(004) COMP-X VALUE 0.
           05 tags                 PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-R             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-Y             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-V             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-A             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-S             PIC  9(004) COMP-X VALUE 0.
           05 OBJETO-Q             PIC  9(004) COMP-X VALUE 0.
           05 MOVER-M              PIC  X(001) VALUE SPACE.
           05 CHAR                 PIC  X(001) VALUE SPACE.
           05 DEC-X REDEFINES CHAR PIC  9(002) COMP-X.
           05 DEC                  PIC  9(003) VALUE 0.
           05 HEX                  PIC  X(002) VALUE "XX".
           05 FIM                  PIC  X(001) VALUE "N".
           05 LIN-W                PIC S9(003) VALUE 0.
           05 LIN-ESC              PIC S9(003) VALUE 0.
           05 LIN-W2               PIC S9(003) VALUE 0.
           05 COL-W                PIC S9(003) VALUE 0.
           05 COL-ESC              PIC S9(003) VALUE 0.
           05 COL-W2               PIC S9(003) VALUE 0.
           05 COL-B                PIC S9(003) VALUE 0.
           05 LIN-M                PIC  9(002) VALUE 0.
           05 COL-M                PIC  9(003) VALUE 0.
           05 LIN-E                PIC  9(002) VALUE 0.
           05 LIN-EA               PIC  9(002) VALUE 0.
           05 COL-E                PIC  9(003) VALUE 0.
           05 COL-EA               PIC  9(003) VALUE 0.
           05 LIN-I                PIC  9(002) VALUE 0.
           05 COL-I                PIC  9(003) VALUE 0.
           05 LIN-R                PIC  9(002) VALUE 0.
           05 COL-R                PIC  9(003) VALUE 0.
           05 LIN-T                PIC  9(002) VALUE 0.
           05 LIN-P                PIC  9(002) VALUE 0.
           05 COL-T                PIC  9(003) VALUE 0.
           05 LIN-A                PIC  9(002) VALUE 0.
           05 POS-S.
              10 LIN-S             PIC  9(002) VALUE 0.
              10 COL-S             PIC  9(002) VALUE 0.
              10                   PIC  X(002) VALUE "01".
           05 COL-A                PIC  9(003) VALUE 0.
           05 COL-SW               PIC  9(003) VALUE 0.
           05 P                    PIC  9(003) VALUE 0.
           05 C                    PIC  9(003) VALUE 0.
           05 C0                   PIC  9(002) VALUE 0.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER     PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER  PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER   PIC  X(078) VALUE SPACES.
              10 ATTRIBUTE-BUFFER  PIC  X(078) VALUE SPACES.
              10 REDEFINES ATTRIBUTE-BUFFER.
                 15 AB             PIC  9(002) COMP-X OCCURS 78.
              10 STRING-LENGTH     PIC  9(004) COMP-X VALUE 78.
              10 STRING-START      PIC  9(004) COMP-X VALUE 1.
           05 CARACTER             PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                   PIC  9(002) COMP-X.
           05 SCREEN-POSITION-D.
              10 ROW-NUMBER-D      PIC  9(002) COMP-X VALUE 0.
              10 COLUMN-NUMBER-D   PIC  9(002) COMP-X VALUE 0.
           05 TECLA2               PIC  9(002) VALUE 0.
           05 TECLA                PIC  9(003) VALUE 0. COPY CWEDIT.
                                                        COPY CWKEYS.
           05 CARACTER-ARR         PIC  X(078) VALUE SPACES.
           05 ATTRIBUTE-ARR        PIC  X(078) VALUE ALL X"9E".
      *    05 ATTRIBUTE-ARR        PIC  X(078) VALUE ALL X"2E".
           05 CARACTER-SAV         PIC  X(078) VALUE SPACES.
           05 ATTRIBUTE-SAV        PIC  X(078) VALUE SPACES.
           05 CURSOR-SAV                              VALUE SPACES.
              10 CURSOR-SAV-ROW    PIC  9(002) COMP-X.
              10 CURSOR-SAV-COL    PIC  9(002) COMP-X.
           05 STRING-SAV           PIC  9(004) COMP-X VALUE 78.
           05 COPIADO                                 VALUE SPACES.
              10 COPY-RL           PIC 9(2) COMP-X.
              10 COPY-RC           PIC 9(2) COMP-X.
              10 COPY-TAMANHO      PIC 9(2) COMP-X.
              10 COPY-TIPO         PIC X(1).
              10 COPY-DATANAME     PIC X(30).
              10 COPY-DATANAME2    PIC X(30).
              10 COPY-ELEMENTO     PIC X(500).
           05 OPT-INS              PIC 9(002) VALUE 0.
           05 OPT-REM              PIC 9(002) VALUE 0.
           05 SETAS.
              10 SETA-DOWN         PIC  X(001) VALUE X"19".
              10 SETA-UP           PIC  X(001) VALUE X"18".
              10 SETA-LEFT         PIC  X(001) VALUE X"1B".
              10 SETA-RIGHT        PIC  X(001) VALUE X"1A".
          05 ALL-X02               PIC X(78) VALUE ALL X"02".
          05 ALL-X0A               PIC X(78) VALUE ALL X"0A".
          05 ALL-X0B               PIC X(78) VALUE ALL X"0B".
          05 ALL-X0C               PIC X(78) VALUE ALL X"0C".
          05 ALL-X0D               PIC X(78) VALUE ALL X"0D".
          05 ALL-X0E               PIC X(78) VALUE ALL X"0E".
          05 ALL-X1E               PIC X(78) VALUE ALL X"1E".
          05 ALL-X3E               PIC X(78) VALUE ALL X"3E".
          05 ALL-X4B               PIC X(78) VALUE ALL X"4B".
          05 ALL-X4E               PIC X(78) VALUE ALL X"4E".
          05 ALL-X5E               PIC X(78) VALUE ALL X"5E".
          05 ALL-X6E               PIC X(78) VALUE ALL X"6E".
          05 ALL-XB0               PIC X(78) VALUE ALL X"B0".
          05 ALL-XB2               PIC X(78) VALUE ALL X"B2".

       01  MAPA-ORDENACAO VALUE "0".
           05 COLUNAS-ORDENADAS        PIC 9(001).
           05 COLUNA-ORDENADA OCCURS 7 PIC X(030).
           05 COLUNA-DESCENDE OCCURS 7 PIC X(001).

       01  MAPA-STATUS-LINHA VALUE SPACES.
           05 STATUS-LINHA OCCURS 100 PIC X.
           05 MAPA-FLAGS.
              10 FLAG      OCCURS 100 PIC X.

       01  MAPA-QUEBRAS VALUE ALL "0".
           05 NIVEL-QUEBRA OCCURS 99 PIC 9.

       01  MAPAS.
           02  MAPA VALUE SPACES.
               05 MAPA-LIN OCCURS 100.
                  10 CX OCCURS 500 PIC X.
           02  MAPA-MOUSE VALUE LOW-VALUES.
               05 MAPA-LIN-M OCCURS 100.
                  10 CX-M OCCURS 500 PIC 9(4) COMP-X.
       01  TAG-MAP REDEFINES MAPAS.
           02 TAG OCCURS 1 TO 1100 DEPENDING ON TAGS.
              10 TAG-ID      PIC  9(004).
              10 TAG-FLAG    PIC  X(001).
              10 TAG-FROM    PIC  X(030).
              10 TAG-NAME    PIC  X(030).
              10 TAG-TIPO    PIC  X(001).
              10 TAG-LEN     PIC  9(003).
              10 TAG-DEC     PIC  9(002).
           02 RECORTADO OCCURS 1 TO 1100 DEPENDING ON RECORTADOS.
              10 REC-ID      PIC  9(004).
              10 REC-FLAG    PIC  X(001).
              10 REC-FROM    PIC  X(030).
              10 REC-NAME    PIC  X(030).
              10 REC-TIPO    PIC  X(001).
              10 REC-LEN     PIC  9(003).
              10 REC-DEC     PIC  9(002).

       01  POSICOES-OBJETOS-M VALUE LOW-VALUES.
           05 OCCURS 255.
              10 OBJETO-ML PIC 9(2) COMP-X.
              10 OBJETO-MC PIC 9(2) COMP-X.

       01  POSICOES-OBJETOS-R VALUE LOW-VALUES.
           05 OBJETO-ATTR
              OCCURS 1 TO 22000 DEPENDING ON OBJETO-R INDEXED BY ORX.
              10 OBJETO-RL        PIC 9(2) COMP-X.
              10 OBJETO-RC        PIC 9(2) COMP-X.
              10 OBJETO-TAMANHO   PIC 9(2) COMP-X.
              10 OBJETO-TIPO      PIC X(1).
                 88 OBJETO-RESULTADO VALUE "+" "-" "*" "/" "P" "R".
              10 OBJETO-DATANAME  PIC X(30).
              10 OBJETO-DATANAME2 PIC X(30).

       01  PROMPTS VALUE SPACES.
           05 PROMPT-ATRIBUTOS OCCURS 100.
              10 PROMPT-DATANAME PIC X(09).
              10 PROMPT-MENSAGEM PIC X(30).
              10 PROMPT-COLUNA   PIC X(30).

       01  CONDICOES VALUE SPACES.
           05 CONDICAO-ATRIBUTOS OCCURS 100.
              10 CONDICAO-TIPO      PIC X(01).
              10 CONDICAO-DATANAME  PIC X(30).
              10 CONDICAO-CONDICAO  PIC X(02).
              10 CONDICAO-OPERANDO.
                 11 CONDICAO-NUMERO PIC X(18).
                 11                 PIC X(12).
              10 CONDICAO-LITERAL   PIC X(01).
              10 CONDICAO-TAMANHO   PIC 9(02).
              10 CONDICAO-MASCARA   PIC X(10).

       01  OPERACOES VALUE SPACES.
           05 OPERACAO-ATRIBUTOS OCCURS 100.
              10 OPERACAO-DATANAME  PIC X(09).
              10 OPERACAO-INT       PIC 9(02).
              10 OPERACAO-DEC       PIC 9(01).
              10 OPERACAO-OPERANDO1 PIC X(30).
              10 OPERACAO-OP-1LIT   PIC X(01).
              10 OPERACAO-OPERADOR  PIC X(01).
              10 OPERACAO-OPERANDO2 PIC X(30).
              10 OPERACAO-OP-2LIT   PIC X(01).
              10 OPERACAO-COLUNA    PIC 9(04).

       01  TABELA-HEX.
           05 FILLER                         PIC  X(055) VALUE
              "000102030405060708090A0B0C0D0E0F101112131415161718191A1".
           05 FILLER                         PIC  X(054) VALUE
              "B1C1D1E1F202122232425262728292A2B2C2D2E2F3031323334353".
           05 FILLER                         PIC  X(054) VALUE
              "63738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F505".
           05 FILLER                         PIC  X(054) VALUE
              "152535455565758595A5B5C5D5E5F606162636465666768696A6B6".
           05 FILLER                         PIC  X(039) VALUE
              "C6D6E6F707172737475767778797A7B7C7D7E7F".
           05 FILLER                         PIC  X(055) VALUE
              "808182838485868788898A8B8C8D8E8F909192939495969798999A9".
           05 FILLER                         PIC  X(054) VALUE
              "B9C9D9E9FA0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B".
           05 FILLER                         PIC  X(054) VALUE
              "6B7B8B9BABBBCBDBEBFC0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D".
           05 FILLER                         PIC  X(054) VALUE
              "1D2D3D4D5D6D7D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBE".
           05 FILLER                         PIC  X(039) VALUE
              "CEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
       01  REDEFINES TABELA-HEX.
           10 CHAR-HEX PIC XX OCCURS 255.

       01  PRO-CAB.
           05                     PIC  X(010) VALUE "PAGINA".
           05                     PIC  X(010) VALUE "FOLHA".
           05                     PIC  X(010) VALUE "DATA".
           05                     PIC  X(010) VALUE "HORA".
           05                     PIC  X(010) VALUE "USUARIO".
           05                     PIC  X(010) VALUE "E-MAIL".
           05                     PIC  X(010) VALUE "EMPRESA".
           05                     PIC  X(010) VALUE "SISTEMA".
           05                     PIC  X(010) VALUE "DIA-SEMANA".
           05                     PIC  X(010) VALUE "CODIGO    ".
           05                     PIC  X(010) VALUE "DESCRICAO ".
       01  REDEFINES PRO-CAB.
           05 RELATOR-CAB         PIC  X(010) OCCURS 11.

       01  TEXTO-REG.
           05 TEXTO-CHAVE.
              10 TEXTO-OBJETO      PIC  9(005).
              10                   PIC  X(001).
           05 TEXTO-DETALHE.
              10 TEXTO-LINHA       PIC  9(002).
              10                   PIC  X(001).
              10 TEXTO-COLUNA      PIC  9(003).
              10                   PIC  X(001).
              10 TEXTO-TIPO        PIC  X(001).
           05 REDEFINES TEXTO-DETALHE.
              10 TEXTO-OBJETOS     PIC  9(005).
              10                   PIC  X(001).
              10 TEXTO-PADRAO      PIC  9(001).
              10                   PIC  X(001).
           05                      PIC  X(001).
           05 TEXTO-TAMANHO        PIC  9(003).
           05                      PIC  X(001).
           05 TEXTO-STRING         PIC  X(219).
           05                      PIC  X(001).
           05 TEXTO-PROVEDOR       PIC  X(008).
           05                      PIC  X(001).
           05                      PIC  X(050).
           05                      PIC  X(001).
           05 TEXTO-CAB            PIC  9(002).
           05                      PIC  X(001).
           05 TEXTO-SIZE-PAGE      PIC  9(003).
       01  TEXTO-BYTE              PIC  X(001).
       01  WS-TELA.
           05 TCAMPO OCCURS 22.
              06 CWCHECK-FLAG     PIC X(001).
              06 TSEQ             PIC 9(004).
              06 NOME             PIC X(030).
              06 ORIGEM           PIC X(030).
              06 TLEN             PIC 9(003).
              06 TDEC             PIC 9(002).

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(044) VALUE
              "Campo Nome                           Tamanho".
       02  LINHA-02.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 CLIC-I                         PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-NOME                      PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-LEN                       PIC  Z(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DEC                       PIC  Z(002) VALUE ZEROS.

       COPY CWBOXS.
       COPY CWSEND.
       COPY CWBOXF.
       COPY CWBOXW.
       COPY CWMOUS.
       COPY CWPATH.
       COPY CWUNIX.
       COPY CWHELP.
       COPY CWCONF.

       LINKAGE SECTION.

       01  COMMAREA PIC X(2000).

       SCREEN SECTION.

       01  RODAPE.
           05 LINE 23 COLUMN 02 PIC X(68) FROM SPACES.

       01  CTAC-LIT-CWREL1.
           05 LINE 08 COLUMN 03 VALUE "Relat¢rio".
           05 LINE 10 COLUMN 03 VALUE "Formato".
           05 LINE 12 COLUMN 03 VALUE "Provedor".

       01  CTAC-VAR-CWREL1.
           05 T-RELATORIO
              LINE 08 COLUMN 16 PIC X(007) USING RELATORIO.
           05 T-RESTO.
              06 T-FORMATO
                 LINE 10 COLUMN 16 PIC X(009) USING FORMATOS-PADRAO.
              06 T-PROVEDOR
                 LINE 12 COLUMN 16 PIC X(039) USING FORMATOS-PROVEDOR.

       01  D-RELATORIO.
           05 LINE 10 COLUMN 16 PIC X(009) USING FORMATOS-PADRAO.
           05 LINE 12 COLUMN 16 PIC X(039) USING FORMATOS-PROVEDOR.
           05 A-RELATORIO.
              06 LINE 14 COLUMN 03 VALUE "Descri‡Æo".
              06 LINE 14 COLUMN 16 PIC X(30) USING FORMATOS-DATANAME.
              06 LINE 16 COLUMN 03 VALUE "Linhas/Folha".
              06 T-SIZE-PAGE
                 LINE 16 COLUMN 16 PIC Z(03) USING SIZE-PAGE.

       01  D-ARQUIVO.
           05 LINE 10 COLUMN 16 PIC X(009) USING FORMATOS-PADRAO.
           05 LINE 12 COLUMN 16 PIC X(039) USING FORMATOS-PROVEDOR.
           05 A-ARQUIVO.
              06 LINE 10 COLUMN 26 PIC X(50) USING NOME-ARQUIVO.
              06 LINE 14 COLUMN 03 VALUE "Descri‡Æo".
              06 LINE 14 COLUMN 16 PIC X(30) USING FORMATOS-DATANAME.
              06 LINE 16 COLUMN 03 VALUE "Destino     ".
              06 LINE 16 COLUMN 16 PIC X(50) USING DESTINO.
              06 LINE 18 COLUMN 03 VALUE "Delimitador ".
              06 LINE 18 COLUMN 16 PIC X(01) USING DELIM.

       01  DATAS AUTO.
       02  FOREGROUND-COLOR 6 BACKGROUND-COLOR 1 HIGHLIGHT.
           05 DT-4 BLANK ZERO
              LINE L COLUMN 46 PIC 99/99      USING DT4.
           05 DT-6 BLANK ZERO
              LINE L COLUMN 46 PIC 99/99/99   USING DT6.
           05 DT-6X BLANK ZERO
              LINE L COLUMN 46 PIC 99/9999    USING DT6.
           05 DT-8 BLANK ZERO
              LINE L COLUMN 46 PIC 99/99/9999 USING DT8.
           05 TELA-HORA.
              10 LINE L COLUMN 46 PIC 9(02) USING HH.
              10 LINE L COLUMN 48 VALUE ":".
              10 LINE L COLUMN 49 PIC 9(02) USING MM.
              10 LINE L COLUMN 51 VALUE ":".
              10 LINE L COLUMN 52 PIC 9(02) USING SS.

       01  CTAC-LIT-EXPO.
           05 LINE 21 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 20 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 19 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 18 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 17 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 16 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 15 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 14 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 13 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 12 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 11 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 10 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 09 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 08 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 07 COLUMN 02 PIC X(78) FROM SPACES.
           05 LINE 07 COLUMN 05 VALUE "Campo".
           05 LINE 07 COLUMN 11 VALUE "Nome".
           05 LINE 07 COLUMN 42 VALUE "Tamanho".
           05 LINE 07 COLUMN 50 VALUE "Origem".

       01  CTAC-VAR-EXPO.
           05 LINE 08 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(01).
           05 LINE 08 COLUMN 06 PIC Z(004) FROM  TSEQ(01).
           05 LINE 08 COLUMN 11 PIC X(030) FROM  NOME(01).
           05 LINE 08 COLUMN 42 PIC Z(003) FROM  TLEN(01).
           05 LINE 08 COLUMN 46 PIC Z(002) FROM  TDEC(01).
           05 LINE 08 COLUMN 50 PIC X(030) FROM  ORIGEM(01).
           05 LINE 09 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(02).
           05 LINE 09 COLUMN 06 PIC Z(004) FROM  TSEQ(02).
           05 LINE 09 COLUMN 11 PIC X(030) FROM  NOME(02).
           05 LINE 09 COLUMN 42 PIC Z(003) FROM  TLEN(02).
           05 LINE 09 COLUMN 46 PIC Z(002) FROM  TDEC(02).
           05 LINE 09 COLUMN 50 PIC X(030) FROM  ORIGEM(02).
           05 LINE 10 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(03).
           05 LINE 10 COLUMN 06 PIC Z(004) FROM  TSEQ(03).
           05 LINE 10 COLUMN 11 PIC X(030) FROM  NOME(03).
           05 LINE 10 COLUMN 42 PIC Z(003) FROM  TLEN(03).
           05 LINE 10 COLUMN 46 PIC Z(002) FROM  TDEC(03).
           05 LINE 10 COLUMN 50 PIC X(030) FROM  ORIGEM(03).
           05 LINE 11 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(04).
           05 LINE 11 COLUMN 06 PIC Z(004) FROM  TSEQ(04).
           05 LINE 11 COLUMN 11 PIC X(030) FROM  NOME(04).
           05 LINE 11 COLUMN 42 PIC Z(003) FROM  TLEN(04).
           05 LINE 11 COLUMN 46 PIC Z(002) FROM  TDEC(04).
           05 LINE 11 COLUMN 50 PIC X(030) FROM  ORIGEM(04).
           05 LINE 12 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(05).
           05 LINE 12 COLUMN 06 PIC Z(004) FROM  TSEQ(05).
           05 LINE 12 COLUMN 11 PIC X(030) FROM  NOME(05).
           05 LINE 12 COLUMN 42 PIC Z(003) FROM  TLEN(05).
           05 LINE 12 COLUMN 46 PIC Z(002) FROM  TDEC(05).
           05 LINE 12 COLUMN 50 PIC X(030) FROM  ORIGEM(05).
           05 LINE 13 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(06).
           05 LINE 13 COLUMN 06 PIC Z(004) FROM  TSEQ(06).
           05 LINE 13 COLUMN 11 PIC X(030) FROM  NOME(06).
           05 LINE 13 COLUMN 42 PIC Z(003) FROM  TLEN(06).
           05 LINE 13 COLUMN 46 PIC Z(002) FROM  TDEC(06).
           05 LINE 13 COLUMN 50 PIC X(030) FROM  ORIGEM(06).
           05 LINE 14 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(07).
           05 LINE 14 COLUMN 06 PIC Z(004) FROM  TSEQ(07).
           05 LINE 14 COLUMN 11 PIC X(030) FROM  NOME(07).
           05 LINE 14 COLUMN 42 PIC Z(003) FROM  TLEN(07).
           05 LINE 14 COLUMN 46 PIC Z(002) FROM  TDEC(07).
           05 LINE 14 COLUMN 50 PIC X(030) FROM  ORIGEM(07).
           05 LINE 15 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(08).
           05 LINE 15 COLUMN 06 PIC Z(004) FROM  TSEQ(08).
           05 LINE 15 COLUMN 11 PIC X(030) FROM  NOME(08).
           05 LINE 15 COLUMN 42 PIC Z(003) FROM  TLEN(08).
           05 LINE 15 COLUMN 46 PIC Z(002) FROM  TDEC(08).
           05 LINE 15 COLUMN 50 PIC X(030) FROM  ORIGEM(08).
           05 LINE 16 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(09).
           05 LINE 16 COLUMN 06 PIC Z(004) FROM  TSEQ(09).
           05 LINE 16 COLUMN 11 PIC X(030) FROM  NOME(09).
           05 LINE 16 COLUMN 42 PIC Z(003) FROM  TLEN(09).
           05 LINE 16 COLUMN 46 PIC Z(002) FROM  TDEC(09).
           05 LINE 16 COLUMN 50 PIC X(030) FROM  ORIGEM(09).
           05 LINE 17 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(10).
           05 LINE 17 COLUMN 06 PIC Z(004) FROM  TSEQ(10).
           05 LINE 17 COLUMN 11 PIC X(030) FROM  NOME(10).
           05 LINE 17 COLUMN 42 PIC Z(003) FROM  TLEN(10).
           05 LINE 17 COLUMN 46 PIC Z(002) FROM  TDEC(10).
           05 LINE 17 COLUMN 50 PIC X(030) FROM  ORIGEM(10).
           05 LINE 18 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(11).
           05 LINE 18 COLUMN 06 PIC Z(004) FROM  TSEQ(11).
           05 LINE 18 COLUMN 11 PIC X(030) FROM  NOME(11).
           05 LINE 18 COLUMN 42 PIC Z(003) FROM  TLEN(11).
           05 LINE 18 COLUMN 46 PIC Z(002) FROM  TDEC(11).
           05 LINE 18 COLUMN 50 PIC X(030) FROM  ORIGEM(11).
           05 LINE 19 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(12).
           05 LINE 19 COLUMN 06 PIC Z(004) FROM  TSEQ(12).
           05 LINE 19 COLUMN 11 PIC X(030) FROM  NOME(12).
           05 LINE 19 COLUMN 42 PIC Z(003) FROM  TLEN(12).
           05 LINE 19 COLUMN 46 PIC Z(002) FROM  TDEC(12).
           05 LINE 19 COLUMN 50 PIC X(030) FROM  ORIGEM(12).
           05 LINE 20 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(13).
           05 LINE 20 COLUMN 06 PIC Z(004) FROM  TSEQ(13).
           05 LINE 20 COLUMN 11 PIC X(030) FROM  NOME(13).
           05 LINE 20 COLUMN 42 PIC Z(003) FROM  TLEN(13).
           05 LINE 20 COLUMN 46 PIC Z(002) FROM  TDEC(13).
           05 LINE 20 COLUMN 50 PIC X(030) FROM  ORIGEM(13).
           05 LINE 21 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(14).
           05 LINE 21 COLUMN 06 PIC Z(004) FROM  TSEQ(14).
           05 LINE 21 COLUMN 11 PIC X(030) FROM  NOME(14).
           05 LINE 21 COLUMN 42 PIC Z(003) FROM  TLEN(14).
           05 LINE 21 COLUMN 46 PIC Z(002) FROM  TDEC(14).
           05 LINE 21 COLUMN 50 PIC X(030) FROM  ORIGEM(14).

       01  CTAC-LIT-EXPOG.
           05 LINE 01 COLUMN 05 VALUE "Campo".
           05 LINE 01 COLUMN 11 VALUE "Nome".
           05 LINE 01 COLUMN 42 VALUE "Tamanho".
           05 LINE 01 COLUMN 50 VALUE "Origem".

       01  CTAC-VAR-EXPOG.
           05 LINE 02 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(01).
           05 LINE 02 COLUMN 06 PIC Z(004) FROM  TSEQ(01).
           05 LINE 02 COLUMN 11 PIC X(030) FROM  NOME(01).
           05 LINE 02 COLUMN 42 PIC Z(003) FROM  TLEN(01).
           05 LINE 02 COLUMN 46 PIC Z(002) FROM  TDEC(01).
           05 LINE 02 COLUMN 50 PIC X(030) FROM  ORIGEM(01).
           05 LINE 03 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(02).
           05 LINE 03 COLUMN 06 PIC Z(004) FROM  TSEQ(02).
           05 LINE 03 COLUMN 11 PIC X(030) FROM  NOME(02).
           05 LINE 03 COLUMN 42 PIC Z(003) FROM  TLEN(02).
           05 LINE 03 COLUMN 46 PIC Z(002) FROM  TDEC(02).
           05 LINE 03 COLUMN 50 PIC X(030) FROM  ORIGEM(02).
           05 LINE 04 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(03).
           05 LINE 04 COLUMN 06 PIC Z(004) FROM  TSEQ(03).
           05 LINE 04 COLUMN 11 PIC X(030) FROM  NOME(03).
           05 LINE 04 COLUMN 42 PIC Z(003) FROM  TLEN(03).
           05 LINE 04 COLUMN 46 PIC Z(002) FROM  TDEC(03).
           05 LINE 04 COLUMN 50 PIC X(030) FROM  ORIGEM(03).
           05 LINE 05 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(04).
           05 LINE 05 COLUMN 06 PIC Z(004) FROM  TSEQ(04).
           05 LINE 05 COLUMN 11 PIC X(030) FROM  NOME(04).
           05 LINE 05 COLUMN 42 PIC Z(003) FROM  TLEN(04).
           05 LINE 05 COLUMN 46 PIC Z(002) FROM  TDEC(04).
           05 LINE 05 COLUMN 50 PIC X(030) FROM  ORIGEM(04).
           05 LINE 06 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(05).
           05 LINE 06 COLUMN 06 PIC Z(004) FROM  TSEQ(05).
           05 LINE 06 COLUMN 11 PIC X(030) FROM  NOME(05).
           05 LINE 06 COLUMN 42 PIC Z(003) FROM  TLEN(05).
           05 LINE 06 COLUMN 46 PIC Z(002) FROM  TDEC(05).
           05 LINE 06 COLUMN 50 PIC X(030) FROM  ORIGEM(05).
           05 LINE 07 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(06).
           05 LINE 07 COLUMN 06 PIC Z(004) FROM  TSEQ(06).
           05 LINE 07 COLUMN 11 PIC X(030) FROM  NOME(06).
           05 LINE 07 COLUMN 42 PIC Z(003) FROM  TLEN(06).
           05 LINE 07 COLUMN 46 PIC Z(002) FROM  TDEC(06).
           05 LINE 07 COLUMN 50 PIC X(030) FROM  ORIGEM(06).
           05 LINE 08 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(07).
           05 LINE 08 COLUMN 06 PIC Z(004) FROM  TSEQ(07).
           05 LINE 08 COLUMN 11 PIC X(030) FROM  NOME(07).
           05 LINE 08 COLUMN 42 PIC Z(003) FROM  TLEN(07).
           05 LINE 08 COLUMN 46 PIC Z(002) FROM  TDEC(07).
           05 LINE 08 COLUMN 50 PIC X(030) FROM  ORIGEM(07).
           05 LINE 09 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(08).
           05 LINE 09 COLUMN 06 PIC Z(004) FROM  TSEQ(08).
           05 LINE 09 COLUMN 11 PIC X(030) FROM  NOME(08).
           05 LINE 09 COLUMN 42 PIC Z(003) FROM  TLEN(08).
           05 LINE 09 COLUMN 46 PIC Z(002) FROM  TDEC(08).
           05 LINE 09 COLUMN 50 PIC X(030) FROM  ORIGEM(08).
           05 LINE 10 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(09).
           05 LINE 10 COLUMN 06 PIC Z(004) FROM  TSEQ(09).
           05 LINE 10 COLUMN 11 PIC X(030) FROM  NOME(09).
           05 LINE 10 COLUMN 42 PIC Z(003) FROM  TLEN(09).
           05 LINE 10 COLUMN 46 PIC Z(002) FROM  TDEC(09).
           05 LINE 10 COLUMN 50 PIC X(030) FROM  ORIGEM(09).
           05 LINE 11 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(10).
           05 LINE 11 COLUMN 06 PIC Z(004) FROM  TSEQ(10).
           05 LINE 11 COLUMN 11 PIC X(030) FROM  NOME(10).
           05 LINE 11 COLUMN 42 PIC Z(003) FROM  TLEN(10).
           05 LINE 11 COLUMN 46 PIC Z(002) FROM  TDEC(10).
           05 LINE 11 COLUMN 50 PIC X(030) FROM  ORIGEM(10).
           05 LINE 12 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(11).
           05 LINE 12 COLUMN 06 PIC Z(004) FROM  TSEQ(11).
           05 LINE 12 COLUMN 11 PIC X(030) FROM  NOME(11).
           05 LINE 12 COLUMN 42 PIC Z(003) FROM  TLEN(11).
           05 LINE 12 COLUMN 46 PIC Z(002) FROM  TDEC(11).
           05 LINE 12 COLUMN 50 PIC X(030) FROM  ORIGEM(11).
           05 LINE 13 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(12).
           05 LINE 13 COLUMN 06 PIC Z(004) FROM  TSEQ(12).
           05 LINE 13 COLUMN 11 PIC X(030) FROM  NOME(12).
           05 LINE 13 COLUMN 42 PIC Z(003) FROM  TLEN(12).
           05 LINE 13 COLUMN 46 PIC Z(002) FROM  TDEC(12).
           05 LINE 13 COLUMN 50 PIC X(030) FROM  ORIGEM(12).
           05 LINE 14 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(13).
           05 LINE 14 COLUMN 06 PIC Z(004) FROM  TSEQ(13).
           05 LINE 14 COLUMN 11 PIC X(030) FROM  NOME(13).
           05 LINE 14 COLUMN 42 PIC Z(003) FROM  TLEN(13).
           05 LINE 14 COLUMN 46 PIC Z(002) FROM  TDEC(13).
           05 LINE 14 COLUMN 50 PIC X(030) FROM  ORIGEM(13).
           05 LINE 15 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(14).
           05 LINE 15 COLUMN 06 PIC Z(004) FROM  TSEQ(14).
           05 LINE 15 COLUMN 11 PIC X(030) FROM  NOME(14).
           05 LINE 15 COLUMN 42 PIC Z(003) FROM  TLEN(14).
           05 LINE 15 COLUMN 46 PIC Z(002) FROM  TDEC(14).
           05 LINE 15 COLUMN 50 PIC X(030) FROM  ORIGEM(14).
           05 LINE 16 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(15).
           05 LINE 16 COLUMN 06 PIC Z(004) FROM  TSEQ(15).
           05 LINE 16 COLUMN 11 PIC X(030) FROM  NOME(15).
           05 LINE 16 COLUMN 42 PIC Z(003) FROM  TLEN(15).
           05 LINE 16 COLUMN 46 PIC Z(002) FROM  TDEC(15).
           05 LINE 16 COLUMN 50 PIC X(030) FROM  ORIGEM(15).
           05 LINE 17 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(16).
           05 LINE 17 COLUMN 06 PIC Z(004) FROM  TSEQ(16).
           05 LINE 17 COLUMN 11 PIC X(030) FROM  NOME(16).
           05 LINE 17 COLUMN 42 PIC Z(003) FROM  TLEN(16).
           05 LINE 17 COLUMN 46 PIC Z(002) FROM  TDEC(16).
           05 LINE 17 COLUMN 50 PIC X(030) FROM  ORIGEM(16).
           05 LINE 18 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(17).
           05 LINE 18 COLUMN 06 PIC Z(004) FROM  TSEQ(17).
           05 LINE 18 COLUMN 11 PIC X(030) FROM  NOME(17).
           05 LINE 18 COLUMN 42 PIC Z(003) FROM  TLEN(17).
           05 LINE 18 COLUMN 46 PIC Z(002) FROM  TDEC(17).
           05 LINE 18 COLUMN 50 PIC X(030) FROM  ORIGEM(17).
           05 LINE 19 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(18).
           05 LINE 19 COLUMN 06 PIC Z(004) FROM  TSEQ(18).
           05 LINE 19 COLUMN 11 PIC X(030) FROM  NOME(18).
           05 LINE 19 COLUMN 42 PIC Z(003) FROM  TLEN(18).
           05 LINE 19 COLUMN 46 PIC Z(002) FROM  TDEC(18).
           05 LINE 19 COLUMN 50 PIC X(030) FROM  ORIGEM(18).
           05 LINE 20 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(19).
           05 LINE 20 COLUMN 06 PIC Z(004) FROM  TSEQ(19).
           05 LINE 20 COLUMN 11 PIC X(030) FROM  NOME(19).
           05 LINE 20 COLUMN 42 PIC Z(003) FROM  TLEN(19).
           05 LINE 20 COLUMN 46 PIC Z(002) FROM  TDEC(19).
           05 LINE 20 COLUMN 50 PIC X(030) FROM  ORIGEM(19).
           05 LINE 21 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(21).
           05 LINE 21 COLUMN 06 PIC Z(004) FROM  TSEQ(21).
           05 LINE 21 COLUMN 11 PIC X(030) FROM  NOME(21).
           05 LINE 21 COLUMN 42 PIC Z(003) FROM  TLEN(21).
           05 LINE 21 COLUMN 46 PIC Z(002) FROM  TDEC(21).
           05 LINE 21 COLUMN 50 PIC X(030) FROM  ORIGEM(21).
           05 LINE 22 COLUMN 04 PIC X(001) USING CWCHECK-FLAG(22).
           05 LINE 22 COLUMN 06 PIC Z(004) FROM  TSEQ(22).
           05 LINE 22 COLUMN 11 PIC X(030) FROM  NOME(22).
           05 LINE 22 COLUMN 42 PIC Z(003) FROM  TLEN(22).
           05 LINE 22 COLUMN 46 PIC Z(002) FROM  TDEC(22).
           05 LINE 22 COLUMN 50 PIC X(030) FROM  ORIGEM(22).

       PROCEDURE DIVISION USING COMMAREA.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           IF   ARQUIVO
                INITIALIZE WS-TELA
                IF   CWUNIX-GUI
                     MOVE 22 TO TTELA
                     MOVE 24 TO LINHA-RODAPE
                     DISPLAY CTAC-LIT-EXPOG
                     CALL "CWMSGW" USING "241607" RELATORIO
                     CALL "CWMSGW" USING "242409" PADRAO-TXT (PADRAO)
                ELSE
                     DISPLAY CTAC-LIT-EXPO
                     CALL "CWMSGW" USING "231607" RELATORIO
                     CALL "CWMSGW" USING "232409" PADRAO-TXT (PADRAO)
                END-IF
      *         PERFORM 002-RODAPE-LAYOUT THRU 002-99-FIM
                PERFORM 001-EDIT-LAYOUT   THRU 001-99-FIM
                      UNTIL FIM = "S"
           ELSE
                PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                        UNTIL FIM = "S"
           END-IF
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       001-EDIT-LAYOUT.

           PERFORM 003-EXIBE-TELA THRU 003-99-FIM
           IF CURSOR-DOWN
              EXEC COBOLware SetFocus
                   FIELD CWCHECK-FLAG SUBSCRIPT TD
              END-EXEC
           END-IF

           IF   PONTO > 1
                MOVE 1 TO FLAG-RODAPE(1:1)
           ELSE
                MOVE '0' TO FLAG-RODAPE(1:1)
           END-IF

           IF ((PONTO + TTELA - 1) < tags AND (tags < 1024))
           OR  (PONTO + TTELA - 1) = tags
               MOVE 1  TO FLAG-RODAPE(2:1)
           ELSE
               MOVE '0' TO FLAG-RODAPE(2:1)
           END-IF

           IF   FLAG-RODAPE NOT = FLAG-RODAPE-A
                MOVE FLAG-RODAPE TO FLAG-RODAPE-A
                EXEC COBOLware OBJECT DROP END-EXEC
                PERFORM 002-RODAPE-LAYOUT THRU 002-99-FIM
           END-IF

           IF   CWUNIX-GUI
                DISPLAY CTAC-VAR-EXPOG
                ACCEPT  CTAC-VAR-EXPOG
           ELSE
                DISPLAY CTAC-VAR-EXPO
                ACCEPT  CTAC-VAR-EXPO
           END-IF

           ACCEPT  TECLA FROM ESCAPE KEY

           IF   ESC
                MOVE 'S' TO FIM
                PERFORM 102-SAIDA   THRU 102-99-FIM
           ELSE
                MOVE 0 TO T
                PERFORM TTELA TIMES
                        COMPUTE SEQ = PONTO + T
                        ADD 1   TO T
                        IF  SEQ NOT > tags
                            MOVE CWCHECK-FLAG  (T) TO TAG-FLAG (SEQ)
                        END-IF
                END-PERFORM
                move 0 TO MARCADOS
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > tags
                       IF TAG-FLAG (I) = '1'
                          ADD 1 TO MARCADOS
                       END-IF
                END-PERFORM
                EVALUATE TRUE
                WHEN F2
                     MOVE 1                  TO I CWBOXS-OPTION
                     MOVE 07                 TO CWBOXS-LINE
                     MOVE 50                 TO CWBOXS-COLUMN
                     MOVE "_A‡Æo:_"          TO CWBOXS-TITLE
                     MOVE SPACES             TO CWBOXS-ITENS
                     IF tags = 0
                        MOVE 'A'             TO CWBOXS-OPTION-CHAR
                        MOVE "~Autom tico"   TO CWBOXS-TEXT (I)
                     ELSE
                        MOVE "~Alterar"      TO CWBOXS-TEXT (I)
                        MOVE 'I'             TO CWBOXS-OPTION-CHAR
                     END-IF
                     IF (RECORTADOS NOT = 0)
                     AND(MARCADOS   NOT = 0)
                        ADD  1               TO I
                        MOVE "~Colar abaixo" TO CWBOXS-TEXT (I)
                        IF CWBOXS-OPTION-CHAR = 'C'
                           MOVE I TO CWBOXS-OPTION
                        END-IF
                        ADD  1               TO I
                        MOVE "~colar aci~Ma" TO CWBOXS-TEXT (I)
                        IF CWBOXS-OPTION-CHAR = 'M'
                           MOVE I TO CWBOXS-OPTION
                        END-IF
                     END-IF
                     IF MARCADOS NOT = 0
                        ADD  1               TO I
                        MOVE "~Excluir"      TO CWBOXS-TEXT (I)
                        IF CWBOXS-OPTION-CHAR = 'E'
                           MOVE I TO CWBOXS-OPTION
                        END-IF
                     END-IF
                     ADD  1                  TO I
                     MOVE "~Filtros"         TO CWBOXS-TEXT (I)
                     IF CWBOXS-OPTION-CHAR = 'F'
                        MOVE I TO CWBOXS-OPTION
                     END-IF
                     IF (tags + MARCADOS) < 1025
                        ADD  1                 TO I
                        IF MARCADOS NOT = 0
                           MOVE "~Inserir abaixo" TO CWBOXS-TEXT (I)
                           IF CWBOXS-OPTION-CHAR = 'I'
                              MOVE I TO CWBOXS-OPTION
                           END-IF
                           ADD  1                 TO I
                           MOVE "in~Serir acima"  TO CWBOXS-TEXT (I)
                           IF CWBOXS-OPTION-CHAR = 'S'
                              MOVE I TO CWBOXS-OPTION
                           END-IF
                        ELSE
                           MOVE "~Inserir"        TO CWBOXS-TEXT (I)
                           IF CWBOXS-OPTION-CHAR = 'I'
                              MOVE I TO CWBOXS-OPTION
                           END-IF
                        END-IF
                     END-IF
                     ADD  1                  TO I
                     MOVE "o~Pera‡äes"       TO CWBOXS-TEXT (I)
                     IF CWBOXS-OPTION-CHAR = 'P'
                        MOVE I TO CWBOXS-OPTION
                     END-IF
                     ADD  1                  TO I
                     MOVE "or~Dena‡Æo"       TO CWBOXS-TEXT (I)
                     IF CWBOXS-OPTION-CHAR = 'D'
                        MOVE I TO CWBOXS-OPTION
                     END-IF
                     IF MARCADOS NOT = 0
                        ADD  1               TO I
                        MOVE "~Recortar"     TO CWBOXS-TEXT (I)
                        IF CWBOXS-OPTION-CHAR = 'R'
                           MOVE I TO CWBOXS-OPTION
                        END-IF
                     END-IF
                     CALL "CWBOXS"        USING PARAMETROS-CWBOXS
                     EVALUATE CWBOXS-OPTION-CHAR
                         WHEN 'A'
                              IF tags = 0
                                 PERFORM 010-AUTOMATICO
                                    THRU 010-99-FIM
                              ELSE
                                 move 0 TO PROCESSADOS
                                 PERFORM 011-ALTERAR
                                    THRU 011-99-FIM
                                         VARYING I
                                            FROM 1 BY 1
                                           UNTIL I > tags
                              END-IF
                         WHEN 'C'
                              PERFORM 020-COLAR   THRU 020-99-FIM
                         WHEN 'M'
                              PERFORM 020-COLAR   THRU 020-99-FIM
                         WHEN 'E'
                              PERFORM 030-EXCLUIR THRU 030-99-FIM
                         WHEN 'F'
                              EXEC COBOLware OBJECT DROP END-EXEC
                              PERFORM 190-FILTROS THRU 190-99-FIM
                              PERFORM 002-RODAPE-LAYOUT THRU 002-99-FIM
                         WHEN 'I'
                              MOVE 1 TO I
                              PERFORM 050-INSERIR THRU 050-99-FIM
                         WHEN 'S'
                              MOVE 1 TO I
                              PERFORM 050-INSERIR THRU 050-99-FIM
                         WHEN 'P'
                              EXEC COBOLware OBJECT DROP END-EXEC
                              PERFORM 198-OPERACOES     THRU 198-99-FIM
                              PERFORM 002-RODAPE-LAYOUT THRU 002-99-FIM
                         WHEN 'D'
                              EXEC COBOLware OBJECT DROP END-EXEC
                              PERFORM 142-ORDENACAO THRU 142-99-FIM
                              PERFORM 002-RODAPE-LAYOUT THRU 002-99-FIM
                         WHEN 'R'
                              PERFORM 080-RECORTAR  THRU 080-99-FIM
                                      VARYING I
                                         FROM 1 BY 1
                                        UNTIL I > tags
                              PERFORM 035-REMOVER THRU 035-99-FIM
                   END-EVALUATE
                WHEN PAGE-UP   AND FLAG-RODAPE(1:1) = '1'
                     IF TTELA > PONTO
                        SUBTRACT TTELA FROM PONTO
                     ELSE
                        MOVE 1 TO PONTO
                     END-IF
                WHEN PAGE-DOWN AND FLAG-RODAPE(2:1) = '1'
                     ADD TTELA TO PONTO
                WHEN CURSOR-DOWN
                 AND (TD NOT < TTELA)
                 AND TSEQ (TD) < tags
                     ADD 1     TO PONTO
                WHEN CURSOR-UP AND PONTO > 1
                     SUBTRACT 1 FROM PONTO
                WHEN CONTROL-PAGE-UP
                     MOVE 1 TO PONTO
                WHEN CONTROL-PAGE-DOWN
                     IF (tags - TTELA + 1) > 0
                        COMPUTE PONTO = tags - TTELA + 1
                     ELSE
                        MOVE 1 TO PONTO
                     END-IF
                END-EVALUATE
           END-IF.

       001-99-FIM. EXIT.

       002-RODAPE-LAYOUT.

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE LINHA-RODAPE COLUMN 03 WIDTH 11
                     CAPTION "[esc]-~Sa¡da"
                     KEY ESC TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                 LINE LINHA-RODAPE COLUMN 32 WIDTH 11
                 CAPTION " f2-~Editar"
                 KEY F2 TAB-OFF
           END-EXEC.

           IF FLAG-RODAPE(1:1) = '1'
              EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                        LINE LINHA-RODAPE COLUMN 43 WIDTH 15
                        CAPTION " pgup-~Anterior "
                        KEY PAGE-UP TAB-OFF
              END-EXEC
           END-IF

           IF FLAG-RODAPE(2:1) = '1'
              EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                        LINE LINHA-RODAPE COLUMN 58 WIDTH 14
                        CAPTION " pgdn-~Pr¢ximo "
                        KEY PAGE-DOWN TAB-OFF
              END-EXEC
           END-IF.

       002-99-FIM. EXIT.

       003-EXIBE-TELA.

           INITIALIZE WS-TELA
           MOVE PONTO TO PONTO-A
           MOVE 0     TO T
           PERFORM TTELA TIMES
                   COMPUTE SEQ = PONTO + T
                   ADD 1   TO T
                   IF  SEQ > tags
                       INITIALIZE TCAMPO (T)
                       EXEC COBOLware SetFocus
                            FIELD CWCHECK-FLAG SUBSCRIPT T
                            PROTECT
                       END-EXEC
                   ELSE
                       MOVE SEQ            TO TSEQ         (T)
                       MOVE TAG-FLAG (SEQ) TO CWCHECK-FLAG (T)
                       MOVE TAG-NAME (SEQ) TO NOME         (T)
                       MOVE TAG-FROM (SEQ) TO ORIGEM       (T)
                       MOVE TAG-LEN  (SEQ) TO TLEN         (T)
                       MOVE TAG-DEC  (SEQ) TO TDEC         (T)
                       MOVE T              TO TD
      *                EXEC COBOLware SetFocus
      *                     FIELD CWCHECK-FLAG SUBSCRIPT T
      *                     UNPROTECT
      *                END-EXEC
                   END-IF
           END-PERFORM.

       003-99-FIM. EXIT.

       011-ALTERAR.

           IF TAG-FLAG (I) = '1'
              ADD  1            TO PROCESSADOS
              MOVE TAG-TIPO (I) TO CAMPOS-TIPO
              IF CAMPOS-VALOR
              OR CAMPOS-NUMERICO
                 EXEC COBOLware BoxDialog
                      LINE 11 COLUMN 22 HEADER TAG-FROM(I)
                      Caption(1) "Nome"
                         Size(1) 30
                         Data(1) TAG-NAME(I);TAG-NAME(I)
                      Caption(2) "Tamanho"
                         Size(2)  3 Numeric(2) PIC(2) ZZZ
                         Data(2) TAG-LEN(I);TAG-LEN(I)
                      Caption(3) "Decimais"
                         Size(3) 2 Numeric(3) PIC(3) ZZ
                         Data(3) TAG-DEC(I);TAG-DEC(I)
                 END-EXEC
              ELSE
                 EXEC COBOLware BoxDialog
                      LINE 11 COLUMN 22 HEADER TAG-FROM(I)
                      Caption(1) "Nome"
                         Size(1) 30
                         Data(1) TAG-NAME(I);TAG-NAME(I)
                      Caption(2) "Tamanho"
                         Size(2)  3 Numeric(2) PIC(2) ZZZ
                         Data(2) TAG-LEN(I);TAG-LEN(I)
                 END-EXEC
              END-IF
              IF (TAG-LEN(I) + TAG-DEC(I) > 18)
              AND (NOT CAMPOS-ALFANUMERICO)
                 EXEC COBOLware Send SCREENS SPACES
                      Message 'Campo num‚rico excede 18 d¡gitos'
                 END-EXEC
                 GO TO 011-ALTERAR
              END-IF
              IF  TAG-LEN(I) < TAG-DEC(I)
              AND (NOT CAMPOS-ALFANUMERICO)
                 EXEC COBOLware Send SCREENS SPACES
                      Message 'Decimais excedem o tamanho'
                 END-EXEC
                 GO TO 011-ALTERAR
              END-IF
              IF TAG-LEN(I) > 255
                 EXEC COBOLware Send SCREENS SPACES
                      Message 'Campo excede 255 caracteres'
                 END-EXEC
                 GO TO 011-ALTERAR
              END-IF
              MOVE '0' TO TAG-FLAG (I)
              PERFORM 003-EXIBE-TELA THRU 003-99-FIM
           END-IF.

       011-99-FIM. EXIT.

       010-AUTOMATICO.

           MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
           MOVE 0                 TO CAMPOS-COLUNA
           START CAMPOS KEY NOT LESS CAMPOS-CHAVE
           PERFORM UNTIL FS-CAMPOS > '09'
                      OR (CAMPOS-PROVEDOR NOT = FORMATOS-PROVEDOR)
                   READ CAMPOS NEXT RECORD
                   IF FS-CAMPOS < '10'
      *            AND NOT (CAMPOS-NOFILE)
                   AND(CAMPOS-DATANAME (1:8) NOT = 'Relator-')
                   AND(CAMPOS-DATANAME (1:7) NOT = 'Prompt-')
                   AND CAMPOS-ACUMULADOR = SPACE
                   AND CAMPOS-PROVEDOR  = FORMATOS-PROVEDOR
                   AND (CAMPOS-DATANAME NOT = SPACES)
                   AND (CAMPOS-RELATORIO = RELATORIO
                     OR CAMPOS-RELATORIO = SPACES)
                       ADD  1               TO tags
                       MOVE tags        TO TAG-ID   (tags)
                       MOVE '0'             TO TAG-FLAG (tags)
                       PERFORM 060-SUJERE-NOME THRU 060-99-FIM
                       MOVE CAMPOS-DATANAME TO TAG-FROM (tags)
                       MOVE CAMPOS-TIPO     TO TAG-TIPO (tags)
                       MOVE CAMPOS-TAMANHO  TO TAG-LEN  (tags)
                       MOVE CAMPOS-DEC      TO TAG-DEC  (tags)
                   END-IF
           END-PERFORM.

       010-99-FIM. EXIT.

       020-COLAR.

           MOVE SPACES TO CWSEND-MSG
           IF RECORTADOS = 1
              MOVE REC-NAME (1) TO UNICO
           ELSE
              PERFORM VARYING Y FROM 1 BY 1
                       UNTIL Y > LENGTH RECORTADOS
                         OR RECORTADOS (Y:1) NOT = '0'
                      CONTINUE
              END-PERFORM
              MOVE RECORTADOS (Y: ) TO UNICO
           END-IF

            PERFORM VARYING Y FROM LENGTH UNICO BY -1
                      UNTIL UNICO (Y: 1) NOT = SPACE
                      CONTINUE
            END-PERFORM

            MOVE 2             TO CWSEND-OPTION
            MOVE SPACES        TO CWSEND-SCREENS
            MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
            MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)

            IF RECORTADOS = 1
               STRING 'Colar "' UNICO(1:Y)
                      '" recortado ?'
                      DELIMITED BY SIZE
                INTO CWSEND-MSG
            ELSE
                MOVE "_~Ver_"      TO CWSEND-SCREEN (3)
                STRING 'Colar os ' UNICO(1:Y)
                       ' itens recortados ?'
                       DELIMITED BY SIZE
                INTO CWSEND-MSG
            END-IF

            PERFORM TEST AFTER UNTIL CWSEND-OPTION NOT = 3
                    CALL "CWSEND" USING PARAMETROS-CWSEND
                    EVALUATE CWSEND-OPTION
                        WHEN 1
                             PERFORM 025-COLAGEM THRU 025-99-FIM
                        WHEN 3
                             CALL 'CWFILE' USING LB-LISTA
                             OPEN OUTPUT LISTA
                             WRITE LISTA-REG FROM LINHA-01
                             WRITE LISTA-REG FROM SPACES
                             PERFORM VARYING I FROM 1 BY 1
                                              UNTIL I > RECORTADOS
                                     MOVE I           TO CLIC-I
                                     MOVE REC-NAME(I) TO CLIC-NOME
                                     MOVE REC-LEN (I) TO CLIC-LEN
                                     MOVE REC-DEC (I) TO CLIC-DEC
                                     WRITE LISTA-REG FROM LINHA-02
                             END-PERFORM
                             WRITE LISTA-REG FROM SPACES
                             STRING UNICO(1:Y) ' campos recortados'
                                    DELIMITED BY SIZE INTO LISTA-REG(8:)
                             WRITE LISTA-REG
                             WRITE LISTA-REG FROM SPACES
                             CLOSE LISTA
                             MOVE LB-LISTA TO CWHELP-FILE
                             MOVE 10       TO CWHELP-LINE
                             MOVE 10       TO CWHELP-COLUMN
                             MOVE 10       TO CWHELP-VERTICAL-LENGTH
                             MOVE 46       TO CWHELP-HORIZONTAL-LENGTH
                             CALL "CWHELP" USING PARAMETROS-CWHELP
                             DELETE FILE LISTA
                    END-EVALUATE
            END-PERFORM.

       020-99-FIM. EXIT.

       025-COLAGEM.

           MOVE 0    TO COLADOS R DESMARCADOS
           MOVE tags TO O
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > O
                   ADD 1 TO COLADOS
                   IF TAG-FLAG (I) = '1' OR (R NOT < RECORTADOS)
                      MOVE '0' TO TAG-FLAG (I)
                      ADD  1   TO DESMARCADOS
                      IF CWBOXS-OPTION-CHAR = 'C'
                         MOVE COLADOS TO TAG-ID (I)
                      END-IF
                      PERFORM TEST AFTER
                              UNTIL DESMARCADOS < PROCESSADOS
                              IF R < RECORTADOS
                                 ADD 1 TO R COLADOS tags
                                 MOVE RECORTADO (R) TO TAG      (tags)
                                 MOVE '0'           TO TAG-FLAG (tags)
                                 MOVE COLADOS       TO TAG-ID   (tags)
                                 INITIALIZE RECORTADO (R)
                              ELSE
                                 EXIT PERFORM
                              END-IF
                      END-PERFORM
                      IF CWBOXS-OPTION-CHAR = 'M'
                         ADD  1       TO COLADOS
                         MOVE COLADOS TO TAG-ID (I)
                      END-IF
                   ELSE
                      MOVE '0'     TO TAG-FLAG (I)
                      MOVE COLADOS TO TAG-ID (I)
                   END-IF
           END-PERFORM

           SORT TAG ASCENDING TAG-ID
           MOVE 0 TO RECORTADOS.

       025-99-FIM. EXIT.

       030-EXCLUIR.

           move 0 TO PROCESSADOS
           PERFORM VARYING I
                   FROM 1 BY 1
                  UNTIL I > tags
                  IF TAG-FLAG (I) = '1'
                     ADD  1            TO PROCESSADOS
                     MOVE TAG-NAME (I) TO UNICO
                  END-IF
           END-PERFORM
           PERFORM TEST AFTER UNTIL CWSEND-OPTION = 1 OR 2
               EVALUATE PROCESSADOS
                   WHEN 1
                        MOVE SPACES TO CWSEND-MSG
                        PERFORM VARYING Y FROM LENGTH UNICO BY -1
                                  UNTIL UNICO (Y: 1) NOT = SPACE
                                  CONTINUE
                        END-PERFORM
                        STRING 'Excluir campo "' UNICO(1:Y) '" ?'
                                DELIMITED BY SIZE
                          INTO CWSEND-MSG
                        MOVE 2             TO CWSEND-OPTION
                        MOVE SPACES        TO CWSEND-SCREENS
                        MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
                        MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                        IF   CWSEND-OPTION = 1
                             PERFORM 035-REMOVER THRU 035-99-FIM
                        END-IF
                   WHEN OTHER
                        MOVE SPACES TO CWSEND-MSG
                        PERFORM VARYING Y FROM 1 BY 1
                                 UNTIL Y > LENGTH PROCESSADOS
                                   OR PROCESSADOS (Y:1) NOT = '0'
                                CONTINUE
                        END-PERFORM
                        MOVE PROCESSADOS (Y: ) TO UNICO
                        PERFORM VARYING Y FROM LENGTH UNICO BY -1
                                  UNTIL UNICO (Y: 1) NOT = SPACE
                                  CONTINUE
                        END-PERFORM
                        STRING 'Excluir os ' UNICO(1:Y)
                                ' itens selecionados ?'
                                DELIMITED BY SIZE
                          INTO CWSEND-MSG
                        MOVE 2             TO CWSEND-OPTION
                        MOVE SPACES        TO CWSEND-SCREENS
                        MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
                        MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)
                        MOVE "_~Ver_"      TO CWSEND-SCREEN (3)
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                        IF   CWSEND-OPTION = 1
                             PERFORM 035-REMOVER THRU 035-99-FIM
                        END-IF
                        IF   CWSEND-OPTION = 3
                             CALL 'CWFILE' USING LB-LISTA
                             OPEN OUTPUT LISTA
                             WRITE LISTA-REG FROM LINHA-01
                             WRITE LISTA-REG FROM SPACES
                             PERFORM VARYING I FROM 1 BY 1
                                              UNTIL I > tags
                                     IF TAG-FLAG (I) = '1'
                                        MOVE I           TO CLIC-I
                                        MOVE TAG-NAME(I) TO CLIC-NOME
                                        MOVE TAG-LEN (I) TO CLIC-LEN
                                        MOVE TAG-DEC (I) TO CLIC-DEC
                                        WRITE LISTA-REG FROM LINHA-02
                                     END-IF
                             END-PERFORM
                             WRITE LISTA-REG FROM SPACES
                             STRING UNICO(1:Y)
                             ' campos selecionados para exclusÆo'
                                  DELIMITED BY SIZE INTO LISTA-REG(8:)
                             WRITE LISTA-REG
                             WRITE LISTA-REG FROM SPACES
                             CLOSE LISTA
                             MOVE LB-LISTA TO CWHELP-FILE
                             MOVE 10       TO CWHELP-LINE
                             MOVE 10       TO CWHELP-COLUMN
                             MOVE 10       TO CWHELP-VERTICAL-LENGTH
                             MOVE 46       TO CWHELP-HORIZONTAL-LENGTH
                             CALL "CWHELP" USING PARAMETROS-CWHELP
                             DELETE FILE LISTA
                        END-IF
               END-EVALUATE
           END-PERFORM.

       030-99-FIM. EXIT.

       035-REMOVER.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > tags
                   IF TAG-FLAG (I) = '1'
                   OR TAG-NAME (I) = SPACES
                      MOVE 9999 TO TAG-ID (I)
                   END-IF
           END-PERFORM

           SORT TAG ASCENDING TAG-ID

           MOVE 0 TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > tags
                   IF TAG-FLAG (I) = '1'
                   OR TAG-NAME (I) = SPACES
                      INITIALIZE TAG (I)
                   ELSE
                      ADD  1 TO Y
                      MOVE Y TO TAG-ID (I)
                   END-IF
                   MOVE '0'  TO TAG-FLAG (I)
           END-PERFORM

           MOVE Y TO tags.

       035-99-FIM. EXIT.

       050-INSERIR.

           IF MARCADOS = 0
           OR TAG-FLAG (I) = '1'
              ADD  1  TO PROCESSADOS
              PERFORM 051-SELECIONA-COLUNA THRU 051-99-FIM
              IF CWBOXF-OPTION NOT = SPACES
                 MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                 MOVE CWBOXF-OPTION     TO CAMPOS-DATANAME
                 MOVE RELATORIO         TO CAMPOS-RELATORIO
                 READ CAMPOS KEY CAMPOS-PRONAMREL
                 IF FS-CAMPOS = '23'
                    MOVE SPACES  TO CAMPOS-RELATORIO
                    READ CAMPOS KEY CAMPOS-PRONAMREL
                 END-IF
                 IF MARCADOS = 0
                    MOVE tags TO I
                 END-IF
                 ADD  1                TO tags
                 MOVE I                TO I-SAVE
                 MOVE CAMPOS-DATANAME  TO TAG-FROM (tags)
                 MOVE CAMPOS-TIPO      TO TAG-TIPO (tags)
                 MOVE CAMPOS-TAMANHO   TO TAG-LEN  (tags)
                 MOVE CAMPOS-DEC       TO TAG-DEC  (tags)
                 MOVE '1'              TO TAG-FLAG (tags)
                 MOVE tags         TO I
                 PERFORM 060-SUJERE-NOME THRU 060-99-FIM
                 PERFORM 011-ALTERAR THRU 011-99-FIM
                 MOVE I-SAVE           TO I
                 IF TAG-NAME (tags) = SPACES
                 OR TAG-LEN  (tags) = 0
                    SUBTRACT 1 FROM tags
                 ELSE
                    IF CWBOXS-OPTION-CHAR = 'I'
                       COMPUTE TAG-ID (tags) = I + 1
                    ELSE
                       MOVE I TO TAG-ID (tags)
                    END-IF
                    MOVE 0 TO SEQ TAG-ID (I)
                    COMPUTE O = tags - 1
                    PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > O
                        ADD 1 TO SEQ
                        IF SEQ = TAG-ID (tags)
                           ADD 1 TO SEQ
                        END-IF
                        MOVE SEQ TO TAG-ID (Y)
                    END-PERFORM
pop                 MOVE '0'     TO TAG-FLAG (I)
pop                                 TAG-FLAG (tags)
                    IF MARCADOS > 0
                       SUBTRACT 1 FROM MARCADOS
                    END-IF
                    SORT TAG ASCENDING TAG-ID
                    PERFORM 003-EXIBE-TELA THRU 003-99-FIM
                 END-IF
              END-IF
           END-IF

           IF (MARCADOS NOT = 0)
           AND I < tags
               ADD 1 TO I
               GO TO 050-INSERIR
           END-IF.

       050-99-FIM. EXIT.

       051-SELECIONA-COLUNA.

           MOVE SPACES        TO CWBOXF-OPTION
           MOVE 07            TO CWBOXF-LINE
           MOVE 45            TO CWBOXF-COLUMN
           MOVE 1             TO CWBOXF-RETURN
           SET  CWBOXF-SHOW   TO TRUE
           MOVE LB-CABS       TO CWBOXF-WORK-AREA
           MOVE 2             TO CWBOXF-ORDER
           MOVE 1             TO CWBOXF-RETURN DELETAR-CABS
           MOVE 'CWREL5'      TO CWBOXF-PROGRAM
           MOVE "_Origem:_"   TO CWBOXF-TITLE

           ON 1
              OPEN OUTPUT CABS
              MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
              MOVE 0                 TO CAMPOS-COLUNA
              MOVE 11                TO MAIOR-NOME
              START CAMPOS KEY NOT LESS CAMPOS-CHAVE
              PERFORM UNTIL FS-CAMPOS > '09'
                         OR (CAMPOS-PROVEDOR NOT = FORMATOS-PROVEDOR)
                      READ CAMPOS NEXT RECORD
                      IF FS-CAMPOS < '10'
                      AND NOT (CAMPOS-NOFILE)
                      AND CAMPOS-ACUMULADOR = SPACE
                      AND CAMPOS-PROVEDOR  = FORMATOS-PROVEDOR
                      AND (CAMPOS-DATANAME NOT = SPACES)
                      AND (CAMPOS-RELATORIO = RELATORIO
                        OR CAMPOS-RELATORIO = SPACES)
                           WRITE CABS-REG FROM CAMPOS-DATANAME
                           PERFORM VARYING MAX-NAME
                                      FROM LENGTH CAMPOS-DATANAME
                                        BY -1
                                     UNTIL MAX-NAME = MAIOR-NOME
                                        OR MAX-NAME = 1
                                     OR CAMPOS-DATANAME(MAX-NAME: 1)
                                           NOT = SPACE
                                   CONTINUE
                           END-PERFORM
                           IF  MAX-NAME > MAIOR-NOME
                               MOVE MAX-NAME TO MAIOR-NOME
                           END-IF
                      END-IF
              END-PERFORM
              CLOSE CABS.

           MOVE 15            TO CWBOXF-VERTICAL-LENGTH
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = MAIOR-NOME + 2
           CALL "CWBOXF" USING PARAMETROS-CWBOXF.

       051-99-FIM. EXIT.

       060-SUJERE-NOME.

           IF CAMPOS-DATANAME (1:3) = '(=)'
              MOVE 'Resultado-'         TO WORKNAME
              MOVE CAMPOS-DATANAME (4:) TO WORKNAME(11:)
           ELSE
              MOVE CAMPOS-DATANAME TO WORKNAME
           END-IF
           INSPECT WORKNAME CONVERTING MAIUSCULAS TO MINUSCULAS
           INSPECT WORKNAME(1:1) CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE    0  TO Y
           PERFORM VARYING E FROM 1 BY 1 UNTIL E > LENGTH WORKNAME
                   IF WORKNAME(E:1) = '-'
                      ADD 1 TO E
                      INSPECT WORKNAME(E:1)
                              CONVERTING MINUSCULAS TO MAIUSCULAS
                   END-IF
                   IF WORKNAME(E:1) NOT = SPACE
                      ADD 1 TO Y
                      MOVE WORKNAME(E:1) TO TAG-NAME (tags) (Y:1)
                   END-IF
           END-PERFORM
           MOVE 1 TO C
           PERFORM VARYING O FROM 1 BY 1
                     UNTIL O = tags
                        OR Y > 22
                   IF TAG-NAME (O) = TAG-NAME (tags)
                      MOVE 'Copia' TO TAG-NAME (tags) (Y + 1:)
                      IF C > 1
                         PERFORM VARYING P FROM 1 BY 1
                                 UNTIL C (P:1) NOT = '0'
                                CONTINUE
                         END-PERFORM
                         MOVE C (P:) TO TAG-NAME (tags) (Y + 6:)
                      END-IF
                      MOVE 0       TO O
                      ADD  1       TO C
                   END-IF
           END-PERFORM.

       060-99-FIM. EXIT.

       080-RECORTAR.

           IF TAG-FLAG (I) = '1'
              ADD  1       TO RECORTADOS
              MOVE TAG (I) TO RECORTADO (RECORTADOS)
              MOVE '0'     TO REC-FLAG  (RECORTADOS)
          END-IF.

       080-99-FIM. EXIT.

       100-PROCESSAMENTO.

           MOVE    0                      TO TECLA
           PERFORM 120-POSICIONA-CURSOR THRU 120-99-FIM
           MOVE ALL X"F8"          TO CWMOUS-LINE (23) (2: 13)
           MOVE 251                TO CWMOUS-POSIT (07 01)
                                      CWMOUS-POSIT (07 80)
           MOVE 252                TO CWMOUS-POSIT (21 01)
                                      CWMOUS-POSIT (21 80)
           MOVE 253                TO CWMOUS-POSIT (22 02)
                                      CWMOUS-POSIT (06 02)
           MOVE 254                TO CWMOUS-POSIT (22 79)
                                      CWMOUS-POSIT (06 79)
           MOVE 0                  TO CWMOUS-KEY
           CALL "CWMOUS"        USING PARAMETROS-CWMOUS

           IF   CWMOUS-KEY = 249
           OR   CWMOUS-KEY = 250
                CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                    MOUSE-POSITION
                MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                COMPUTE LIN-S = ROW-MOUSE      + 1
                COMPUTE COL-S = COLUMN-MOUSE   + 1
                IF  (LIN-S > 7)
                AND (COL-S > 4)
                AND (LIN-S < 22)
                AND (COL-S < 80)
                    IF   LIN-S = LIN-A
                    AND  COL-S = COL-A
                         SET EDIT-ENTER TO TRUE
                    END-IF
                    PERFORM 120-POSICIONA-CURSOR THRU 120-99-FIM
                    PERFORM 130-MONITOR          THRU 130-99-FIM
                    MOVE LIN-S TO LIN-A
                    MOVE COL-S TO COL-A
                END-IF
           END-IF
           IF   CWMOUS-KEY < 248
           AND  CWMOUS-KEY > 0
                MOVE    OBJETO-ML (CWMOUS-KEY) TO LIN-S
                MOVE    OBJETO-MC (CWMOUS-KEY) TO COL-S
                PERFORM 120-POSICIONA-CURSOR THRU 120-99-FIM
                PERFORM 130-MONITOR          THRU 130-99-FIM
                SET     EDIT-ENTER             TO TRUE
           END-IF
           EVALUATE CWMOUS-KEY
                    WHEN 248 MOVE "S"              TO FIM
                    WHEN 251 SET EDIT-CURSOR-UP    TO TRUE
                    WHEN 252 SET EDIT-CURSOR-DOWN  TO TRUE
                    WHEN 253 SET EDIT-CURSOR-LEFT  TO TRUE
                    WHEN 254 SET EDIT-CURSOR-RIGHT TO TRUE
                    WHEN 255
                         PERFORM 105-LER-TECLA THRU 105-99-FIM
                         IF  TECLA-COMUM
                             PERFORM 150-TECLA-COMUM THRU 150-99-FIM
                         END-IF
           END-EVALUATE

           IF   EDIT-ENTER
                IF   CWMOUS-KEY = 255 OR 249 OR 250
                     MOVE 1 TO KEY-STATUS
                     IF  CWUNIX-ON
                     OR  CWUNIX-GUI
                         CONTINUE
                     ELSE
                         CALL "CBL_GET_MOUSE_POSITION"
                              USING MOUSE-HANDLE
                                    MOUSE-POSITION
                         MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                     END-IF
                END-IF
                PERFORM 140-EDITAR-OBJETO THRU 140-99-FIM
                MOVE    0                   TO TECLA
           ELSE
                PERFORM 106-PROCESSA-TECLA THRU 106-99-FIM
                EVALUATE TRUE
                    WHEN EDIT-DEL
                     AND (CX-M (LIN-E COL-E) NOT = 0)
                         MOVE CX-M (LIN-E COL-E) TO OBJETO-MW
                         SUBTRACT 1 FROM OBJETO-TAMANHO (OBJETO-MW)
                         MOVE LIN-E TO LIN-W
                         MOVE COL-E TO COL-W
                         PERFORM 101-SHIFT-LEFT THRU 101-99-FIM
                         IF   OBJETO-TIPO (OBJETO-MW) = "E"
                              MOVE OBJETO-RL (OBJETO-MW) TO LIN-W
                              MOVE OBJETO-RC (OBJETO-MW) TO COL-W
                              IF   CX (LIN-W COL-W) = "."
                                   SUBTRACT 1
                                       FROM OBJETO-TAMANHO (OBJETO-MW)
                                   PERFORM 101-SHIFT-LEFT
                                      THRU 101-99-FIM
                              END-IF
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                         MOVE OBJETO-MW TO OBJETO-D
                         PERFORM 135-OBS THRU 135-99-FIM
                         CALL "CBL_WRITE_SCR_CHATTRS" USING X"1800"
                                                            OBS
                                                            OBS-ATTR
                                                            X"0035"
                END-EVALUATE
           END-IF

           PERFORM 130-MONITOR THRU 130-99-FIM
           PERFORM 102-SAIDA   THRU 102-99-FIM.

       100-99-FIM. EXIT.

       101-SHIFT-LEFT.

           ADD  1     TO COL-W
           PERFORM UNTIL COL-W > 500
                      OR CX-M(LIN-W COL-W) NOT = OBJETO-MW
                   COMPUTE COL-W2 = COL-W - 1
                   MOVE CX-M (LIN-W COL-W) TO CX-M (LIN-W COL-W2)
                   MOVE CX   (LIN-W COL-W) TO CX   (LIN-W COL-W2)
                   ADD  1                  TO COL-W
           END-PERFORM
           SUBTRACT 1 FROM COL-W
           MOVE SPACE TO  CX   (LIN-W COL-W)
           MOVE 0     TO  CX-M (LIN-W COL-W).

       101-99-FIM. EXIT.

       102-SAIDA.

           IF   FIM = "S"
                MOVE 3              TO CWSEND-OPTION
                MOVE "Encerrando, salvar altera‡äes ?" TO CWSEND-MSG
                MOVE SPACES          TO CWSEND-SCREENS
                MOVE "___~Sim_"      TO CWSEND-SCREEN (1)
                MOVE "___~NÆo_"      TO CWSEND-SCREEN (2)
                MOVE "_~Cancelar_"   TO CWSEND-SCREEN (3)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 0 OR 3
                     MOVE "N" TO FIM
                END-IF
           END-IF.

       102-99-FIM. EXIT.

       105-LER-TECLA.

           SET TECLA-COMUM TO TRUE
           CALL "CBL_GET_CSR_POS" USING CURSOR-POSITION
           IF   NO-GET-GUI = 1
                MOVE 0 TO NO-GET-GUI
           ELSE
              COMPUTE CURPOS-HP = COL-I / 1,46
              COMPUTE CURPOS-VP = LIN-I
              CALL "CWKBDC" USING CURPOS CARACTER "999"
              COMPUTE CURPOS-LIN = CURSOR-LIN + 1
              COMPUTE CURPOS-COL = CURSOR-COL + 1
              MOVE CURPOS TO CURPOS-SAVE
              IF   CWUNIX-GUI
                   SUBTRACT 1 FROM CURPOS-COL
              END-IF
              CALL "CWKBDC" USING CURPOS CARACTER TECLA
              IF   CWUNIX-GUI
                   ADD 1 TO CURPOS-COL
              END-IF
           END-IF
           EVALUATE TECLA
               WHEN 009 SET EDIT-CONTROL-CURSOR-RIGHT TO TRUE
               WHEN 015 SET EDIT-CONTROL-CURSOR-LEFT  TO TRUE
               WHEN 998
                    EVALUATE TRUE
                        WHEN CURPOS-LIN < 2
                             SET  EDIT-CURSOR-UP    TO TRUE
                             MOVE CURPOS-SAVE       TO CURPOS
                        WHEN CURPOS-LIN > 21
                             SET  EDIT-CURSOR-DOWN  TO TRUE
                             MOVE CURPOS-SAVE       TO CURPOS
                        WHEN CURPOS-COL > 78
                             SET  EDIT-CURSOR-RIGHT TO TRUE
                             MOVE CURPOS-SAVE       TO CURPOS
                        WHEN CURPOS-COL < 5
                             SET  EDIT-CURSOR-LEFT  TO TRUE
                             MOVE CURPOS-SAVE       TO CURPOS
                        WHEN CURPOS = CURPOS-SAVE
                             SET  EDIT-ENTER        TO TRUE
                        WHEN OTHER
                             COMPUTE CURSOR-LIN = CURPOS-LIN - 1
                             COMPUTE CURSOR-COL = CURPOS-COL - 1
                             CALL "CBL_SET_CSR_POS"
                                  USING CURSOR-POSITION
                             COMPUTE LIN-S = LIN-S +
                                             (CURPOS-LIN - CURPOS-LINS)
                             COMPUTE COL-S = COL-S +
                                             (CURPOS-COL - CURPOS-COLS)
                             PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    END-EVALUATE
      *        WHEN 073 SET EDIT-PAGE-DOWN            TO TRUE
      *        WHEN 081 SET EDIT-PAGE-UP              TO TRUE
           END-EVALUATE
           IF   TECLA NOT = 0
                MOVE SPACE         TO CARACTER
                SET TECLA-ESPECIAL TO TRUE
           END-IF
           IF   EDIT-ESC
                MOVE "S"           TO FIM
           END-IF
           MOVE CX-M (LIN-E COL-E) TO OBJETO-T.

       105-99-FIM. EXIT.

       106-PROCESSA-TECLA.

           EVALUATE TRUE
                    WHEN TECLA = 999
                         MOVE CURPOS-VP TO LIN-I
                         IF   LIN-I > BOTM
                              MOVE BOTM TO LIN-I
                         END-IF
                         COMPUTE COL-I = 1,46 * CURPOS-HP
                         IF  COL-I < 1
                             MOVE 1 TO COL-I
                         END-IF
                         IF  COL-I > 426
                             MOVE 426 TO COL-I
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-HOME
                         MOVE TOPO TO LIN-S
                         MOVE 05   TO COL-S
                    WHEN EDIT-CONTROL-PAGE-DOWN
                         MOVE BOTM TO LIN-I
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-CONTROL-PAGE-UP
                         MOVE 1 TO LIN-I
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-PAGE-DOWN
                      OR EDIT-CONTROL-CURSOR-DOWN
                         COMPUTE LIN-W = LIN-I + (ALTURA - 1)
                         IF  LIN-W < BOTM
                             COMPUTE LIN-I = LIN-I + (ALTURA - 1)
                         ELSE
                             MOVE BOTM TO LIN-I
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-PAGE-UP
                      OR EDIT-CONTROL-CURSOR-UP
                         COMPUTE LIN-W = LIN-I - (ALTURA - 1)
                         IF  LIN-W > 1
                             COMPUTE LIN-I = LIN-I - (ALTURA - 1)
                         ELSE
                             MOVE 1 TO LIN-I
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-CURSOR-DOWN
                         IF   LIN-S < 21
                         AND  CWMOUS-KEY = 255
                              ADD 1 TO LIN-S
                         ELSE
                              IF  LIN-I < BOTM
                                  ADD 1 TO LIN-I
                                  PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                              END-IF
                         END-IF
                    WHEN EDIT-CURSOR-RIGHT
                         IF   COL-S < 79
                         AND  CWMOUS-KEY = 255
                              ADD 1 TO COL-S
                         ELSE
                              IF  COL-I < 426
                                  ADD 1 TO COL-I
                                  IF   COL-S > 5
                                  AND  CWMOUS-KEY NOT = 255
                                       SUBTRACT 1 FROM COL-S
                                  END-IF
                                  PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                              END-IF
                         END-IF
                    WHEN EDIT-CURSOR-UP
                         IF   LIN-S > TOPO
                         AND  CWMOUS-KEY = 255
                              SUBTRACT 1 FROM LIN-S
                         ELSE
                              IF  LIN-I > 1
                                  SUBTRACT 1 FROM LIN-I
                                  PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                              END-IF
                         END-IF
                    WHEN EDIT-CURSOR-LEFT
                      OR EDIT-BACKSPACE
                         IF   COL-S > 5
                         AND  CWMOUS-KEY = 255
                              SUBTRACT 1 FROM COL-S
                         ELSE
                              IF  COL-I > 1
                                  IF   COL-S < 79
                                  AND  CWMOUS-KEY NOT = 255
                                       ADD 1 TO COL-S
                                  END-IF
                                  SUBTRACT 1 FROM COL-I
                                  PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                              END-IF
                         END-IF
                         IF   EDIT-BACKSPACE
                         AND  COL-E > 1
                              SUBTRACT 1 FROM COL-E
                              SET EDIT-DEL TO TRUE
                         END-IF
                    WHEN EDIT-CONTROL-CURSOR-LEFT
                         COMPUTE COL-W = COL-I - 10
                         IF  COL-W > 0
                             SUBTRACT 10 FROM COL-I
                         ELSE
                             MOVE 1 TO COL-I
                         END-IF
                         COMPUTE COL-W = COL-S - 10
                         IF  COL-W > 4
                             SUBTRACT 10 FROM COL-S
                         ELSE
                             MOVE 5 TO COL-S
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                    WHEN EDIT-CONTROL-CURSOR-RIGHT
                         COMPUTE COL-W = COL-I + 10
                         IF   COL-W < 426
                              ADD 10 TO COL-I
                         ELSE
                              MOVE 426 TO COL-I
                         END-IF
                         COMPUTE COL-W = COL-S + 10
                         IF   COL-W < 79
                              ADD 10 TO COL-S
                         ELSE
                              MOVE 79 TO COL-S
                         END-IF
                         PERFORM 110-EXIBE-TELA THRU 110-99-FIM
           END-EVALUATE.

       106-99-FIM. EXIT.

       110-EXIBE-TELA.

           PERFORM VARYING TOT FROM 1 BY 1
                              UNTIL TOT = 99
                                 OR STATUS-LINHA (TOT) = "T"
                   CONTINUE
           END-PERFORM

           IF   TOT NOT = 0
                PERFORM VARYING TOT2 FROM 99 BY -1
                                UNTIL TOT2 = TOT
                                 OR STATUS-LINHA (TOT2) = "T"
                        CONTINUE
               END-PERFORM
           ELSE
               MOVE 0 TO TOT2
           END-IF

           MOVE SPACES                     TO CURSOR-POSITION-A
           MOVE 0                          TO AR
                                              VAGO
           MOVE LIN-I                      TO LIN-T
           MOVE COL-I                      TO COL-T
           MOVE LOW-VALUES                 TO CWMOUS-CONTROL
           MOVE SPACES                     TO CWMOUS-MENSAGES
           MOVE 25                         TO CWMOUS-LIN
           MOVE 01                         TO CWMOUS-COL
           MOVE LENGTH OF OBS              TO CWMOUS-LENGTH
           MOVE 14                         TO CWMOUS-COLOR
           MOVE 1                          TO COLUMN-NUMBER
           MOVE 6                          TO ROW-NUMBER
           MOVE 7                          TO LIN-M
           IF  CWUNIX-GUI
               MOVE 0                      TO COLUMN-NUMBER
               SUBTRACT 6 FROM ROW-NUMBER LIN-M
           END-IF
           MOVE 78                         TO STRING-LENGTH
           MOVE SPACES                     TO CARACTER-BUFFER (1: 3)
           MOVE MAPA-LIN (100) (COL-T: 78) TO CARACTER-BUFFER (4:  )
           IF   CWUNIX-GUI
      *         MOVE ALL X"27"             TO ATTRIBUTE-BUFFER
      *         MOVE ALL X"37"             TO ATTRIBUTE-BUFFER
      *         MOVE ALL X"01"             TO ATTRIBUTE-BUFFER
                MOVE ALL X"70"             TO ATTRIBUTE-BUFFER
           ELSE
                MOVE ALL-X0A               TO ATTRIBUTE-BUFFER
           END-IF
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH
           MOVE 0   TO OBJETO-M
           PERFORM ALTURA TIMES
              IF   COBOLware
                   COMPUTE LIN-P = LIN-T - CAB
                   MOVE LIN-P TO CARACTER-BUFFER
              ELSE
                   MOVE LIN-T TO CARACTER-BUFFER
              END-IF
              MOVE MAPA-LIN (LIN-T) (COL-T: 78) TO CARACTER-BUFFER
                                                   (4: )
              INSPECT CARACTER-BUFFER CONVERTING X"00" TO " "
              IF   CWUNIX-GUI
                   MOVE ALL X"70"   TO ATTRIBUTE-BUFFER
              ELSE
                   MOVE ALL-X0A     TO ATTRIBUTE-BUFFER
              END-IF
              ADD 1 TO ROW-NUMBER LIN-M
              IF   LIN-T NOT > CAB
                   MOVE ALL-X3E        TO ATTRIBUTE-BUFFER (4: )
              ELSE
                   IF   LIN-T NOT > DET
                        MOVE ALL-X1E    TO ATTRIBUTE-BUFFER (4: )
                        MOVE ALL-X0E    TO ATTRIBUTE-BUFFER (1: 2)
                        MOVE " D"       TO CARACTER-BUFFER  (1: 2)
                   END-IF
                   IF  STATUS-LINHA (LIN-T) = "Q"
                       MOVE ALL-X4E     TO ATTRIBUTE-BUFFER (4: )
                       IF  LIN-T < DET
                       OR (CAB = DET
                       AND VAGO = 0)
                           MOVE ALL-X4B  TO ATTRIBUTE-BUFFER (4: )
                       END-IF
                       MOVE " ?" TO CARACTER-BUFFER  (1: 2)
                       IF   NIVEL-QUEBRA (LIN-T) NOT = 0
                            MOVE NIVEL-QUEBRA   (LIN-T)
                              TO CARACTER-BUFFER (2: 1)
                       END-IF
                       MOVE ALL-X0C TO ATTRIBUTE-BUFFER (2: 1)
                   END-IF
                   IF  STATUS-LINHA (LIN-T) = "K"
                       MOVE ALL-X0C    TO ATTRIBUTE-BUFFER (1: 2)
                   END-IF
                   IF  STATUS-LINHA (LIN-T) = "T"
                       MOVE ALL-X5E    TO ATTRIBUTE-BUFFER (4: )
                       MOVE ALL-X0D    TO ATTRIBUTE-BUFFER (1: 2)
                       MOVE " T"       TO CARACTER-BUFFER  (1: 2)
                   END-IF
              END-IF
              IF   MAPA-LIN (LIN-T) = SPACES
              AND  LIN-T > DET
              AND  LIN-T > CAB
              AND  STATUS-LINHA (LIN-T) = SPACES
                   MOVE 1         TO VAGO
                   IF   CWUNIX-OFF
                        MOVE ALL-XB0 TO CARACTER-BUFFER  (4: )
                        MOVE ALL-X02 TO ATTRIBUTE-BUFFER (4: )
                   ELSE
                        MOVE ALL-X02  TO ATTRIBUTE-BUFFER (4: )
                   END-IF
              END-IF
              IF   COBOLware
              AND (LIN-T NOT > CAB)
                  MOVE ALL-X0E TO ATTRIBUTE-BUFFER (1: 2)
                  EVALUATE LIN-T
                      WHEN 1 MOVE "TL" TO CARACTER-BUFFER (1: 2)
                      WHEN 2 MOVE "ST" TO CARACTER-BUFFER (1: 2)
                      WHEN 3 MOVE "H1" TO CARACTER-BUFFER (1: 2)
                      WHEN 4 MOVE "H2" TO CARACTER-BUFFER (1: 2)
                      WHEN 5 MOVE "H3" TO CARACTER-BUFFER (1: 2)
                      WHEN 6 MOVE "H4" TO CARACTER-BUFFER (1: 2)
                      WHEN 7 MOVE "H5" TO CARACTER-BUFFER (1: 2)
                  END-EVALUATE
              ELSE
                  IF   LIN-T NOT > CAB
                       MOVE ALL-X0B TO ATTRIBUTE-BUFFER (1: 2)
                       MOVE " C"    TO CARACTER-BUFFER  (1: 2)
                  END-IF
              END-IF
              MOVE FLAG (LIN-T) TO CARACTER-BUFFER (3: 1)
              IF   FLAG (LIN-T) = "*"
                   IF    CWUNIX-GUI
                         MOVE ALL X"70" TO ATTRIBUTE-BUFFER (4: )
                   ELSE
                         MOVE ALL X"08" TO ATTRIBUTE-BUFFER (4: )
                   END-IF
              END-IF
              CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 STRING-LENGTH
              MOVE LIN-T TO LIN-W
              MOVE COL-T TO COL-W
              MOVE "N"   TO MOVER-M
              MOVE 0     TO OBJETO-S
              PERFORM VARYING COL-B FROM 4 BY 1 UNTIL COL-B > 78
                      COMPUTE COL-M = COL-B + 1
                      IF   CX-M (LIN-W COL-W) NOT = 0
                           IF   CX-M (LIN-W COL-W) NOT = OBJETO-S
                                MOVE CX-M (LIN-W COL-W) TO OBJETO-S
                                MOVE "N"                TO MOVER-M
                           END-IF
                           IF   MOVER-M = "N"
                                MOVE    "S"      TO MOVER-M
                                ADD     1        TO OBJETO-M
                                MOVE    LIN-M    TO OBJETO-ML (OBJETO-M)
                                MOVE    COL-M    TO OBJETO-MC (OBJETO-M)
                                MOVE    OBJETO-S TO OBJETO-D
                                PERFORM 135-OBS THRU 135-99-FIM
                                MOVE    OBS TO CWMOUS-STRING (OBJETO-M)
                           END-IF
                           MOVE OBJETO-M TO CWMOUS-POSIT (LIN-M COL-M)
                      ELSE
                           MOVE "N" TO MOVER-M
                           EVALUATE VARIANTE-M
                               WHEN 1
                                  MOVE 2   TO VARIANTE-M
                                  MOVE 250 TO CWMOUS-POSIT (LIN-M COL-M)
                               WHEN 2
                                  MOVE 1   TO VARIANTE-M
                                  MOVE 249 TO CWMOUS-POSIT (LIN-M COL-M)
                           END-EVALUATE
                      END-IF
                      ADD  1        TO COL-W
              END-PERFORM
              ADD 1 TO LIN-T
           END-PERFORM.

       110-99-FIM. EXIT.

       120-POSICIONA-CURSOR.

           COMPUTE ROW-NUMBER    = LIN-S - 1
           COMPUTE COLUMN-NUMBER = COL-S - 1
           CALL "CBL_SET_CSR_POS"  USING SCREEN-POSITION
           MOVE LIN-S              TO CWMOUS-CURSOR-LIN
           MOVE COL-S              TO CWMOUS-CURSOR-COL.

       120-99-FIM. EXIT.

       130-MONITOR.

           COMPUTE LIN-E = LIN-I + LIN-S - TOPO
           COMPUTE COL-E = COL-I + COL-S - 5

           IF  (LIN-E = LIN-EA
           AND  COL-E = COL-EA)
           AND (CWMOUS-KEY NOT = 255)
                MOVE 0 TO CWMOUS-KEY
                GO TO 130-99-FIM
           END-IF

           MOVE     LIN-E            TO LIN-EA
           MOVE     COL-E            TO COL-EA
           MOVE     CX (LIN-E COL-E) TO CHAR
           IF   CHAR = X"00"
                MOVE SPACE TO CHAR
           END-IF
           MOVE     DEC-X            TO DEC
           ADD      1                TO DEC
           MOVE     CHAR-HEX (DEC)   TO HEX
           SUBTRACT 1              FROM DEC

           MOVE 62                         TO COLUMN-NUMBER
           MOVE 24                         TO ROW-NUMBER
           MOVE 17                         TO STRING-LENGTH
           MOVE "00,000 ' '=XX/000"        TO CARACTER-BUFFER
           MOVE LIN-E                      TO CARACTER-BUFFER (01: 02)
           MOVE LIN-E                      TO CARACTER-BUFFER (01: 02)
           MOVE COL-E                      TO CARACTER-BUFFER (04: 03)
           MOVE CHAR                       TO CARACTER-BUFFER (09: 01)
           MOVE HEX                        TO CARACTER-BUFFER (12: 02)
           MOVE DEC                        TO CARACTER-BUFFER (15: 03)
           CALL "CBL_GET_CSR_POS"  USING SAVE-CURSOR
           MOVE CX-M (LIN-E COL-E) TO OBJETO-D
           PERFORM 135-OBS THRU 135-99-FIM
           IF    CWUNIX-GUI
                 SUBTRACT 1 FROM ROW-NUMBER
                 CALL "CBL_WRITE_SCR_CHATTRS" USING X"1701"
                                                    OBS
                                                    OBS-ATTR
                                                    X"0035"
           ELSE
                 CALL "CBL_WRITE_SCR_CHATTRS" USING X"1800"
                                                    OBS
                                                    OBS-ATTR
                                                    X"0035"
           END-IF
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              OBS-ATTR
                                              STRING-LENGTH
           CALL "CBL_SET_CSR_POS"  USING SAVE-CURSOR.

       130-99-FIM. EXIT.

       135-OBS.

           MOVE SPACES TO OBS
           MOVE OBJETO-TAMANHO  (OBJETO-D) TO TAMANHO-ED

           IF  OBJETO-D NOT = 0
               IF  (OBJETO-DATANAME2 (OBJETO-D) NOT = SPACES)
               AND (OBJETO-DATANAME2 (OBJETO-D) NOT = LOW-VALUES)
                  MOVE SPACES TO OBS
                  STRING OBJETO-DATANAME (OBJETO-D)   DELIMITED BY SPACE
                         ' '                          DELIMITED BY SIZE
                         OBJETO-TIPO     (OBJETO-D)   DELIMITED BY SPACE
                         ' '                          DELIMITED BY SIZE
                         OBJETO-DATANAME2(OBJETO-D)   DELIMITED BY SPACE
                    INTO OBS
                  IF OBS (31:) = SPACES
                     MOVE TAMANHO-ED  TO OBS (32: )
                     MOVE "Resultado" TO OBS (36: )
                  ELSE
                     PERFORM VARYING R FROM LENGTH TAMANHO-ED BY -1
                             UNTIL R < 2 OR TAMANHO-ED (R:1) = SPACE
                             CONTINUE
                     END-PERFORM
                     PERFORM VARYING R2 FROM LENGTH OBS BY -1
                             UNTIL OBS (R2:1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     ADD 1 TO R2
                     STRING
                         TAMANHO-ED (R:) ' Resultado' DELIMITED BY SIZE
                       INTO OBS (R2:)
                  END-IF
               ELSE
                  IF OBJETO-TIPO (OBJETO-D) = "E"
                     PERFORM VARYING K
                             FROM 1 BY 1
                            UNTIL K > LENAME - 3
                           OR OBJETO-DATANAME (OBJETO-D) (K: 3) = "[+]"
                              CONTINUE
                     END-PERFORM
                  END-IF
                  MOVE OBJETO-DATANAME (OBJETO-D) TO OBS
                  MOVE        TAMANHO-ED          TO OBS (32: )
                  EVALUATE OBJETO-TIPO (OBJETO-D)
                      WHEN "L" MOVE SPACES              TO OBS (36: )
                      WHEN "A" MOVE "Estilo"            TO OBS (36: )
                      WHEN "X" MOVE "Alfanum‚rico"      TO OBS (36: )
                      WHEN "E"
                           IF OBJETO-DATANAME (OBJETO-D) (K: 3) = "[+]"
                              MOVE "Acumulador editado" TO OBS (36: )
                           ELSE
                              MOVE "Num‚rico editado"   TO OBS (36: )
                           END-IF
                      WHEN "9" MOVE "Num‚rico"          TO OBS (36: )
                  END-EVALUATE
               END-IF
           END-IF.

       135-99-FIM. EXIT.

       140-EDITAR-OBJETO.

           IF  CX-M (LIN-E COL-E) NOT = 0
               MOVE "_A‡Æo:_"                  TO CWBOXS-TITLE
               MOVE 1                          TO CWBOXS-OPTION
               MOVE SPACES                     TO CWBOXS-ITENS
               MOVE "~Copiar"                  TO CWBOXS-TEXT (1)
               MOVE "~Excluir"                 TO CWBOXS-TEXT (2)
               MOVE "~Filtros"                 TO CWBOXS-TEXT (3)
               MOVE "~Linha"                   TO CWBOXS-TEXT (4)
               MOVE "~Mover"                   TO CWBOXS-TEXT (5)
               MOVE "o~Pera‡äes"               TO CWBOXS-TEXT (6)
               MOVE "or~Dena‡Æo"               TO CWBOXS-TEXT (8)
               MOVE 7                          TO OPT-BOX
               MOVE CX-M (LIN-E COL-E)         TO OBJETO-S
               MOVE SPACES                     TO CWBOXS-TITLE
               IF  OBJETO-DATANAME2 (OBJETO-S) = SPACES OR LOW-VALUES
                   MOVE  OBJETO-DATANAME (OBJETO-S) TO MASCARA
               ELSE
                   MOVE SPACES TO MASCARA
                   STRING OBJETO-DATANAME (OBJETO-S) DELIMITED BY SPACE
                          ' '                        DELIMITED BY SIZE
                          OBJETO-TIPO     (OBJETO-S) DELIMITED BY SIZE
                          ' '                        DELIMITED BY SIZE
                          OBJETO-DATANAME2(OBJETO-S) DELIMITED BY SPACE
                     INTO MASCARA
               END-IF
               PERFORM VARYING I FROM LENGTH MASCARA BY -1
                       UNTIL MASCARA (I: 1) NOT = " "
                       CONTINUE
               END-PERFORM
               INSPECT MASCARA (1: I) CONVERTING " " TO "_"
               MOVE SPACES TO CWBOXS-TITLE
               STRING "_"             DELIMITED BY SIZE
                      MASCARA (1: I)  DELIMITED BY SIZE
                      ":_"            DELIMITED BY SIZE
                           INTO CWBOXS-TITLE
               COMPUTE TAMANHO = I + 2
               IF   STATUS-LINHA (LIN-E) = "Q"
                    MOVE "~N¡veis" TO CWBOXS-TEXT   (7)
                    ADD  1         TO OPT-BOX
                    IF  TAMANHO < 12
                        MOVE 12 TO TAMANHO
                    END-IF
               END-IF
               IF  (LIN-E - 1) = CAB
               AND NOT (PADRAO = 1 AND CAB = 7)
                   PERFORM 14Y-CHECK-CAB THRU 14Y-99-FIM
                   IF   CAB-ON = 1
                        MOVE "~Tornar cabe‡alho" TO CWBOXS-TEXT   (11)
                        ADD  1                   TO OPT-BOX
                        IF  TAMANHO < 16
                            MOVE 16 TO TAMANHO
                        END-IF
                   END-IF
               END-IF
               IF  LIN-E = CAB
               OR  STATUS-LINHA (LIN-E) = "Q" OR "T"
                   MOVE "~Tornar detalhe" TO CWBOXS-TEXT   (11)
                   ADD  1                 TO OPT-BOX
                   IF  TAMANHO < 14
                       MOVE 14 TO TAMANHO
                   END-IF
               END-IF
               IF  TAMANHO < 9
                   MOVE 9 TO TAMANHO
               END-IF
               MOVE "~Recortar" TO CWBOXS-TEXT   (10)
               MOVE LIN-S       TO CWBOXS-LINE
               MOVE COL-S       TO CWBOXS-COLUMN
               IF   CWBOXS-LINE < TOPO
                    MOVE TOPO TO CWBOXS-LINE
               END-IF
               ADD  COL-S      TO TAMANHO
               ADD  4          TO TAMANHO
               SUBTRACT 2    FROM CWBOXS-LINE
               PERFORM UNTIL TAMANHO < 79
                       SUBTRACT 1 FROM TAMANHO
                                       CWBOXS-COLUMN
               END-PERFORM
               MOVE "N" TO PROP
               IF   OBJETO-TIPO (OBJETO-S) = "E" OR "9" OR "Z"
                    MOVE OBJETO-RL (OBJETO-S) TO LIN-W
                    MOVE OBJETO-RC (OBJETO-S) TO COL-W
                    PERFORM OBJETO-TAMANHO (OBJETO-S) TIMES
                         IF  CX (LIN-W COL-W) = "9" OR "D" OR "M" OR "A"
                             MOVE "S" TO PROP
                         END-IF
                         ADD 1 TO COL-W
                    END-PERFORM
               END-IF
               IF  PROP = "S"
               OR  OBJETO-TIPO (OBJETO-S) = "Z"
                   MOVE "~Op‡äes" TO CWBOXS-TEXT   (9)
                   ADD  1         TO OPT-BOX
               END-IF
               ADD  1         TO OPT-BOX
               IF   FLAG (LIN-E) = "*"
                    MOVE "(~+)ativa"   TO CWBOXS-TEXT (11)
               ELSE
                    MOVE "(~-)inativa" TO CWBOXS-TEXT (11)
               END-IF
               PERFORM UNTIL (CWBOXS-LINE + OPT-BOX) < bot
                       SUBTRACT 1 FROM CWBOXS-LINE
               END-PERFORM
      *        CALL "CBL_GET_CSR_POS" USING CURSOR-POSITION
               IF  CWUNIX-ON
               OR  CWUNIX-GUI
                   CONTINUE
               ELSE
                   CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                       MOUSE-POSITION-A
               END-IF
               CALL "CWBOXS" USING PARAMETROS-CWBOXS
               EVALUATE TRUE
                        WHEN CWBOXS-OPTION-CHAR = "+"
                             MOVE SPACE TO FLAG (LIN-E)
                        WHEN CWBOXS-OPTION-CHAR = "-"
                             MOVE "*"   TO FLAG (LIN-E)
                        WHEN CWBOXS-OPTION-CHAR = "P"
                             CALL "CWKBDC" USING "9595"
                             PERFORM 198-OPERACOES THRU 198-99-FIM
                             CALL "CWKBDC" USING "9999"
                        WHEN CWBOXS-OPTION-CHAR = "F"
                             PERFORM 190-FILTROS THRU 190-99-FIM
                        WHEN CWBOXS-OPTION-CHAR = "L"
                             MOVE "_Linha:_" TO CWBOXS-TITLE
                             MOVE SPACES     TO CWBOXS-ITENS
                             MOVE "~Apagar"   TO CWBOXS-TEXT   (1)
                             MOVE "~Apagar"   TO CWBOXS-TEXT   (1)
                             MOVE "~Inserir"  TO CWBOXS-TEXT   (2)
                             IF   FLAG (LIN-E) = "*"
                                  MOVE "(~+)ativa"   TO CWBOXS-TEXT (3)
                             ELSE
                                  MOVE "(~-)inativa" TO CWBOXS-TEXT (3)
                             END-IF
                             CALL "CWBOXS" USING PARAMETROS-CWBOXS
                             EVALUATE CWBOXS-OPTION-CHAR
                                 WHEN "A" PERFORM 149-APAGAR-LINHA
                                             THRU 149-99-FIM
                                 WHEN "I" PERFORM 148-INSERIR-LINHA
                                             THRU 148-99-FIM
                                 WHEN "+" MOVE SPACE TO FLAG (LIN-E)
                                 WHEN "-" MOVE "*"   TO FLAG (LIN-E)
                             END-EVALUATE
                        WHEN CWBOXS-OPTION-CHAR = "D"
                             PERFORM 142-ORDENACAO THRU 142-99-FIM
                        WHEN CWBOXS-OPTION-CHAR = "C" OR "R"
                             MOVE OBJETO-ATTR (OBJETO-S) TO COPIADO
                             MOVE OBJETO-RL      (OBJETO-S) TO LIN-W
                             MOVE OBJETO-RC      (OBJETO-S) TO COL-W
                             MOVE OBJETO-TAMANHO (OBJETO-S) TO TAMANHO
                             MOVE MAPA-LIN (LIN-W) (COL-W: TAMANHO)
                               TO COPY-ELEMENTO
                             IF   CWBOXS-OPTION-CHAR = "R"
                                  PERFORM 145-APAGA-OBJETO
                                     THRU 145-99-FIM
                             END-IF
                        WHEN CWBOXS-OPTION-CHAR = "E"
                             MOVE SPACES TO CWSEND-MSG
                             IF   OBJETO-DATANAME (OBJETO-S) = "Texto"
                                  MOVE OBJETO-RL     (OBJETO-S) TO LIN-W
                                  MOVE OBJETO-RC     (OBJETO-S) TO COL-W
                                  MOVE OBJETO-TAMANHO (OBJETO-S)
                                    TO TAMANHO-E
                                  MOVE MAPA-LIN (LIN-W)
                                                (COL-W: TAMANHO-E)
                                    TO MASCARA-E
                                  IF   TAMANHO-E > 40
                                       MOVE 40    TO TAMANHO-E
                                       MOVE "..." TO MASCARA-E (38: )
                                  END-IF
                                  STRING 'Excluir texto "'
                                               DELIMITED BY SIZE
                                         MASCARA-E (1: TAMANHO-E)
                                               DELIMITED BY SIZE
                                         '" ?' DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                             ELSE
                                  EVALUATE OBJETO-TIPO (OBJETO-S)
                                      WHEN "A"
                                  STRING "Excluir estilo "
                                               DELIMITED BY SIZE
                                    OBJETO-DATANAME (OBJETO-S) (1: I)
                                               DELIMITED BY SIZE
                                          " ?" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                                      WHEN OTHER
                                 IF OBJETO-DATANAME2 (OBJETO-S) = SPACES
                                    OR LOW-VALUES
                                  STRING "Excluir coluna "
                                               DELIMITED BY SIZE
                                    OBJETO-DATANAME (OBJETO-S) (1: I)
                                               DELIMITED BY SIZE
                                          " ?" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                                 ELSE
                                  STRING "Excluir resultado "
                                               DELIMITED BY SIZE
                                    OBJETO-DATANAME (OBJETO-S) (1: I)
                                               DELIMITED BY SPACE
                                    ' ' DELIMITED BY SIZE
                                    OBJETO-TIPO      (OBJETO-S)
                                               DELIMITED BY SIZE
                                    ' ' DELIMITED BY SIZE
                                    OBJETO-DATANAME2 (OBJETO-S)
                                               DELIMITED BY SPACE
                                          " ?" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                                 END-IF
                                  END-EVALUATE
                             END-IF
                             MOVE 1       TO CWSEND-OPTION
                             MOVE SPACES     TO CWSEND-SCREENS
                             MOVE "_~Sim_"   TO CWSEND-SCREEN (1)
                             MOVE "_~NÆo_"   TO CWSEND-SCREEN (2)
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             IF   CWSEND-OPTION = 1
                                  PERFORM 145-APAGA-OBJETO
                                     THRU 145-99-FIM
                             END-IF
                        WHEN CWBOXS-OPTION-CHAR = "M"
                             ADD  1 TO CURSOR-LIN CURSOR-COL
                             MOVE CURSOR-LIN TO CURSOR-LIN-S
                             MOVE CURSOR-COL TO CURSOR-COL-S
                             PERFORM UNTIL CWMOUS-POSIT
                                         (CURSOR-LIN CURSOR-COL) NOT =
                                           CWMOUS-POSIT
                                         (CURSOR-LIN-S CURSOR-COL-S)
                                       or CURSOR-COL-S = 1
                                     SUBTRACT 1 FROM CURSOR-COL-S
                             END-PERFORM
                             SUBTRACT 1 FROM CURSOR-LIN-S
                             MOVE CURSOR-POSITION-S TO CURSOR-POSITION
                             CALL "CBL_SET_CSR_POS"
                                                 USING CURSOR-POSITION
      *                      CALL "CBL_GET_MOUSE_POSITION"
      *                              USING MOUSE-HANDLE
      *                                    MOUSE-POSITION
      *                      IF   MOUSE-POSITION NOT = MOUSE-POSITION-A
                             if   key-status = 0
                                  MOVE CURSOR-LIN   TO ROW-MOUSE-A
                                  MOVE CURSOR-COL   TO COLUMN-MOUSE-A
                                  MOVE CURSOR-LIN-S TO ROW-MOUSE-A
                                  MOVE CURSOR-COL-S TO COLUMN-MOUSE-A
                                  IF  CWUNIX-ON
                                  OR  CWUNIX-GUI
                                      CONTINUE
                                  ELSE
                                      CALL "CBL_SET_MOUSE_POSITION"
                                              USING MOUSE-HANDLE
                                                   MOUSE-POSITION-A
                                      move MOUSE-POSITION-A
                                        to MOUSE-POSITION
                                  END-IF
                             end-if
      *                      END-IF
                             MOVE 0                         TO AR
                                                               TECLA
                             MOVE OBJETO-RL      (OBJETO-S) TO LIN-W
                                                               LIN-ESC
                             MOVE OBJETO-RC      (OBJETO-S) TO COL-W
                                                               COL-ESC
                             MOVE OBJETO-TAMANHO (OBJETO-S) TO TAMANHO
                             MOVE MAPA-LIN (LIN-W) (COL-W: TAMANHO)
                               TO ARRASTADO
                             MOVE OBJETO-TIPO    (OBJETO-S) TO TIPO
                             MOVE OBJETO-DATANAME(OBJETO-S) TO DATANAME
                             MOVE OBJETO-DATANAME2(OBJETO-S)
                                                            TO DATANAME2
                             MOVE OBJETO-S                  TO OBJETO-A
                             PERFORM 145-APAGA-OBJETO THRU 145-99-FIM
      *                      COMPUTE LIN-W = CURSOR-LIN + 1
      *                      COMPUTE COL-W = CURSOR-COL + 1
                             COMPUTE LIN-W = CURSOR-LIN
                             COMPUTE COL-W = CURSOR-COL
                             IF  CWUNIX-ON
                             OR  CWUNIX-GUI
                                 MOVE 1 TO NO-GET-GUI
                             END-IF
                             CALL "CWKBDC" USING "9797"
                             PERFORM 180-ARRASTANDO THRU 180-99-FIM
                                     UNTIL EDIT-ENTER
                             CALL "CWKBDC" USING "9696"
                        WHEN CWBOXS-OPTION-CHAR = "N"
                             PERFORM 141-DEFINE-QUEBRA THRU 141-99-FIM
                        WHEN CWBOXS-OPTION-CHAR = "T"
                            IF   STATUS-LINHA (LIN-E) = "Q" OR "T"
                                 IF   STATUS-LINHA (LIN-E) = "Q"
                                      MOVE "K" TO STATUS-LINHA (LIN-E)
                                 ELSE
                                      MOVE "t" TO STATUS-LINHA (LIN-E)
                                 END-IF
                                 MOVE 0     TO NIVEL-QUEBRA (LIN-E)
                                 PERFORM 147-ULTIMO-DETALHE
                                    THRU 147-99-FIM
                            END-IF
                            IF  (LIN-E - 1) = CAB
                                ADD 1 TO CAB
                            ELSE
                                SUBTRACT 1 FROM CAB
                            END-IF
                            PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                        WHEN CWBOXS-OPTION-CHAR = "O"
                             PERFORM UNTIL (CWBOXS-COLUMN + I + 18) < 78
                                     SUBTRACT 1 FROM CWBOXS-COLUMN
                             END-PERFORM
                             MOVE LIN-S      TO CWBOXS-LINE
                             MOVE SPACES     TO CWBOXS-ITENS
                             MOVE "_Op‡Æo:_" TO CWBOXS-TITLE
                             MOVE SPACES     TO CWBOXS-TEXT   (1)
                             STRING "~Brancos se " DELIMITED BY SIZE
                                    OBJETO-DATANAME (OBJETO-S) (1: I)
                                                   DELIMITED BY SIZE
                                    " = 0"         DELIMITED BY SIZE
                                            INTO CWBOXS-TEXT   (1)
                             MOVE "~NÆo"      TO CWBOXS-TEXT   (2)
                             IF   OBJETO-TIPO (OBJETO-S) = "Z"
                                  MOVE 1 TO CWBOXS-OPTION
                             ELSE
                                  MOVE 2 TO CWBOXS-OPTION
                             END-IF
                             CALL "CWBOXS" USING PARAMETROS-CWBOXS
                             MOVE OBJETO-RC      (OBJETO-S) TO COL-W
                             MOVE OBJETO-TAMANHO (OBJETO-S) TO TAMANHO
                             EVALUATE CWBOXS-OPTION
                                 WHEN 1
                                      MOVE "Z" TO OBJETO-TIPO (OBJETO-S)
                                 WHEN 2
                                      MOVE "E" TO OBJETO-TIPO (OBJETO-S)
                                      IF   MAPA-LIN (LIN-W)
                                           (COL-W: TAMANHO) = ALL "9"
                                           MOVE "9"
                                             TO OBJETO-TIPO (OBJETO-S)
                                      END-IF
                             END-EVALUATE
               END-EVALUATE
           ELSE
               MOVE "_A‡Æo:_"        TO CWBOXS-TITLE
               MOVE SPACES           TO CWBOXS-ITENS
               MOVE 5                TO OPT-BOX
               MOVE 12               TO TAMANHO
               MOVE "~Apagar linha"  TO CWBOXS-TEXT   (1)
               IF  (COPIADO NOT = SPACES)
               AND  MAPA-LIN (LIN-E) (COL-E: COPY-TAMANHO) = SPACES
                    MOVE    "S"                   TO MOVER-M
                    MOVE    COPY-DATANAME         TO DATANAME
                    MOVE    COPY-TAMANHO          TO TEST-TAMANHO
                    PERFORM 185-VALIDAR-ENTRADA THRU 185-99-FIM
                    IF   MOVER-M = "S"
                         MOVE "~Colar" TO CWBOXS-TEXT   (2)
                         ADD  1        TO OPT-BOX
                    END-IF
               END-IF
               MOVE "~Filtros"    TO CWBOXS-TEXT   (3)
               MOVE "~Inserir"    TO CWBOXS-TEXT   (4)
               IF  (STATUS-LINHA (LIN-E) = "Q")
                    MOVE "~N¡veis" TO CWBOXS-TEXT   (5)
                    ADD  1         TO OPT-BOX
                    IF  TAMANHO < 12
                        MOVE 12 TO TAMANHO
                    END-IF
               END-IF
               MOVE "o~Pera‡äes"  TO CWBOXS-TEXT   (6)
               MOVE "or~Dena‡Æo"  TO CWBOXS-TEXT   (7)
               ADD  2             TO OPT-BOX
               IF  (LIN-E - 1) = CAB
               AND NOT (PADRAO = 1 AND CAB = 7)
                   PERFORM 14Y-CHECK-CAB THRU 14Y-99-FIM
                   IF   CAB-ON = 1
                        MOVE "~Tornar cabe‡alho" TO CWBOXS-TEXT   (8)
                        ADD  1                   TO OPT-BOX
                        IF  TAMANHO < 16
                            MOVE 16 TO TAMANHO
                        END-IF
                   END-IF
               END-IF
               IF  LIN-E = CAB
               OR  STATUS-LINHA (LIN-E) = "Q" OR "T"
                   MOVE "~Tornar detalhe" TO CWBOXS-TEXT   (8)
                   ADD  1                 TO OPT-BOX
                   IF  TAMANHO < 14
                       MOVE 14 TO TAMANHO
                   END-IF
               END-IF
               ADD  1         TO OPT-BOX
               IF   FLAG (LIN-E) = "*"
                    MOVE "(~+)ativa"   TO CWBOXS-TEXT (9)
               ELSE
                    MOVE "(~-)inativa" TO CWBOXS-TEXT (9)
               END-IF
               MOVE 1             TO CWBOXS-OPTION
               MOVE LIN-S         TO CWBOXS-LINE
               MOVE COL-S         TO CWBOXS-COLUMN
               SUBTRACT 1 FROM CWBOXS-LINE
                               CWBOXS-COLUMN
               PERFORM UNTIL (CWBOXS-COLUMN + TAMANHO) < 77
                       SUBTRACT 1 FROM CWBOXS-COLUMN
               END-PERFORM
               PERFORM UNTIL (CWBOXS-LINE + OPT-BOX) < bot
                       SUBTRACT 1 FROM CWBOXS-LINE
               END-PERFORM
               IF  ((LIN-E - 1) = CAB)
               OR  (LIN-E = CAB)
               OR  (COPIADO NOT = SPACES)
               OR  (STATUS-LINHA (LIN-E) = "Q" OR "T" OR "K")
                    CALL "CWBOXS" USING PARAMETROS-CWBOXS
                    MOVE 0   TO AUTO-COLUNA
               ELSE
                    MOVE "I" TO CWBOXS-OPTION-CHAR
                    MOVE 1   TO AUTO-COLUNA
               END-IF
               MOVE 0            TO SO-DETALHE
               IF STATUS-LINHA (LIN-E) = "K" OR "t"
                  PERFORM VARYING OBJETO-Q
                             FROM 1 BY 1
                            UNTIL OBJETO-Q > OBJETO-R
                               OR SO-DETALHE = 1
                     IF OBJETO-RL (OBJETO-Q) = LIN-E
                     AND (OBJETO-DATANAME (OBJETO-Q) NOT = "Texto")
                     AND (OBJETO-DATANAME (OBJETO-Q) NOT = SPACES)
                        PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I > NIVEL-QUEBRA (LIN-E)
                                     OR OBJETO-DATANAME (OBJETO-Q)
                                        = COLUNA-ORDENADA (I)
                                CONTINUE
                        END-PERFORM
                        IF I > NIVEL-QUEBRA (LIN-E)
                           PERFORM VARYING Y
                                      FROM 1 BY 1
                                     UNTIL Y > LENAME - 3
                                        OR OBJETO-DATANAME (OBJETO-Q)
                                           (Y: 3) = "[+]"
                                        OR (OBJETO-TIPO (OBJETO-Q)
                                           NOT = "E")
                                   CONTINUE
                           END-PERFORM
                           IF Y > LENAME - 3
                              MOVE 1 TO SO-DETALHE
                           END-IF
                        END-IF
                     END-IF
                  END-PERFORM
               END-IF
               EVALUATE CWBOXS-OPTION-CHAR
                   WHEN "+"
                        MOVE SPACE TO FLAG (LIN-E)
                   WHEN "-"
                        MOVE "*"   TO FLAG (LIN-E)
                   WHEN "P" PERFORM 198-OPERACOES THRU 198-99-FIM
                   WHEN "F" PERFORM 190-FILTROS      THRU 190-99-FIM
                   WHEN "A" PERFORM 149-APAGAR-LINHA THRU 149-99-FIM
                   WHEN "D"
                        PERFORM 142-ORDENACAO THRU 142-99-FIM
                   WHEN "C" PERFORM 144-COLAR          THRU 144-99-FIM
                   WHEN "I"
                        IF   AUTO-COLUNA = 1
                             MOVE "_A‡Æo:_" TO CWBOXS-TITLE
                             MOVE SPACES            TO CWBOXS-ITENS
                             MOVE "~Apagar linha"   TO CWBOXS-TEXT (1)
                             MOVE "~Filtros"        TO CWBOXS-TEXT (2)
                             MOVE "inserir ~Coluna" TO CWBOXS-TEXT (3)
                             MOVE "inserir ~Estilo" TO CWBOXS-TEXT (4)
                             MOVE "inserir ~Linha"  TO CWBOXS-TEXT (5)
                             MOVE "o~Pera‡äes"      TO CWBOXS-TEXT (8)
                             MOVE "or~Dena‡Æo"      TO CWBOXS-TEXT (9)
                             MOVE 3                 TO CWBOXS-OPTION
                             MOVE 14                TO TAMANHO
                             IF   LIN-E > TOT2
                             AND  TOT > 0
                                  MOVE 2      TO CWBOXS-OPTION
                                  MOVE SPACES TO CWBOXS-TEXT   (1)
                                                 CWBOXS-TEXT   (3)
                                                 CWBOXS-TEXT   (4)
                                                 CWBOXS-TEXT   (5)
                                  MOVE 9      TO TAMANHO
                             END-IF
                        ELSE
                             MOVE "_Inserir:_" TO CWBOXS-Title
                             MOVE SPACES       TO CWBOXS-ITENS
                             MOVE "~Coluna"    TO CWBOXS-TEXT   (1)
                             MOVE "~Estilo"    TO CWBOXS-TEXT   (2)
                             MOVE "~Linha"     TO CWBOXS-TEXT   (3)
                             MOVE 1            TO CWBOXS-OPTION
                             IF   TAMANHO < 10
                                  MOVE 10 TO TAMANHO
                             END-IF
                             IF   LIN-E > TOT2
                             AND  TOT > 0
                                  MOVE SPACES TO CWBOXS-TEXT   (3)
                             END-IF
                        END-IF
                        IF   STATUS-LINHA (LIN-E) = SPACE
                        OR  (STATUS-LINHA (LIN-E) = "K" OR "t"
                        AND  SO-DETALHE = 0)
                             MOVE "tornar ~Quebra" TO CWBOXS-TEXT   (6)
                             IF   LIN-E > TOT2
                             AND  TOT > 0
                                  MOVE SPACES TO CWBOXS-TEXT   (6)
                             ELSE
                                  IF   TAMANHO < 13
                                       MOVE 13 TO TAMANHO
                                  END-IF
                             END-IF
                             IF  LIN-E > DET
                                 MOVE "tornar ~Total"
                                                  TO CWBOXS-TEXT   (7)
                             END-IF
                             IF   TAMANHO < 12
                                  MOVE 12 TO TAMANHO
                             END-IF
                        ELSE
                             IF AUTO-COLUNA = 1
                                MOVE "or~Dena‡Æo" TO CWBOXS-TEXT   (9)
                             END-IF
                        END-IF
                        ADD  1  TO OPT-BOX
                        IF   FLAG (LIN-E) = "*"
                             MOVE "(~+)ativa"   TO CWBOXS-TEXT (10)
                        ELSE
                             MOVE "(~-)inativa" TO CWBOXS-TEXT (10)
                        END-IF
                        IF  STATUS-LINHA (LIN-E) = "Q" OR "T"
                        AND (FLAG (LIN-E) NOT = "*")
                             MOVE "(~=)resultado" TO CWBOXS-TEXT (11)
                        END-IF
                        PERFORM UNTIL (CWBOXS-LINE + OPT-BOX) < bot
                                SUBTRACT 1 FROM CWBOXS-LINE
                        END-PERFORM
                        PERFORM UNTIL (CWBOXS-COLUMN + TAMANHO) < 77
                                SUBTRACT 1 FROM CWBOXS-COLUMN
                        END-PERFORM
                        CALL "CWBOXS" USING PARAMETROS-CWBOXS
                        EVALUATE CWBOXS-OPTION-CHAR
                             WHEN "="
                                  PERFORM INSERIR-RESULTADO
                                     THRU FIM-INSERIR-RESULTADO
                             WHEN "+"
                                  MOVE SPACE TO FLAG (LIN-E)
                             WHEN "-"
                                  MOVE "*"   TO FLAG (LIN-E)
                             WHEN "F"
                                  PERFORM 190-FILTROS THRU 190-99-FIM
                             WHEN "A"
                                  PERFORM 149-APAGAR-LINHA
                                     THRU 149-99-FIM
                             WHEN "D"
                                  PERFORM 142-ORDENACAO THRU 142-99-FIM
                             WHEN "C"
                                  PERFORM 146-INSERIR-COLUNA
                                     THRU 146-99-FIM
                             WHEN "E"
                                  PERFORM 14X-INSERIR-ESTILO
                                     THRU 14X-99-FIM
                             WHEN "L"
                                  PERFORM 148-INSERIR-LINHA
                                     THRU 148-99-FIM
                             WHEN "Q"
                                 MOVE "Q" TO STATUS-LINHA (LIN-E)
                                 PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                                 PERFORM 141-DEFINE-QUEBRA
                                    THRU 141-99-FIM
                             WHEN "T"
                                 MOVE "T" TO STATUS-LINHA  (LIN-E)
                                 MOVE 0   TO NIVEL-QUEBRA  (LIN-E)
                                 PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                             WHEN "P"
                                 PERFORM 198-OPERACOES THRU 198-99-FIM
                        END-EVALUATE
                   WHEN "N"
                        PERFORM 141-DEFINE-QUEBRA THRU 141-99-FIM
                   WHEN "T"
                        IF   STATUS-LINHA (LIN-E) = "Q" OR "T"
                             MOVE 0      TO NIVEL-QUEBRA  (LIN-E)
                             IF  STATUS-LINHA (LIN-E) = "Q"
                                 MOVE "K" TO STATUS-LINHA (LIN-E)
                             ELSE
                                 MOVE "t" TO STATUS-LINHA (LIN-E)
                             END-IF
                             PERFORM 147-ULTIMO-DETALHE THRU 147-99-FIM
                        ELSE
                             IF  (LIN-E - 1) = CAB
                                 ADD 1 TO CAB
                             ELSE
                                 SUBTRACT 1 FROM CAB
                             END-IF
                        END-IF
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
               END-EVALUATE
           END-IF

           IF   CWBOXS-OPTION-CHAR = "+" OR "-"
                PERFORM 110-EXIBE-TELA THRU 110-99-FIM
           END-IF.

       140-99-FIM. EXIT.

       14Y-CHECK-CAB.

           MOVE 1 TO CAB-ON
           IF   RELATOR-RESTRICT NOT = "OFF"
           PERFORM VARYING CAB-I FROM 1 BY 1
                     UNTIL CAB-I > OBJETO-R
                        OR CAB-ON = 0
                    IF  OBJETO-RL (CAB-I) = LIN-E
                    AND(OBJETO-DATANAME (CAB-I) (1: 8) NOT = "Relator-")
                    AND(OBJETO-DATANAME (CAB-I)        NOT = "Texto")
                        MOVE 0 TO CAB-ON
                    END-IF
           END-PERFORM.

       14Y-99-FIM. EXIT.

       141-DEFINE-QUEBRA.

           IF   COLUNAS-ORDENADAS = 0
                PERFORM 142-ORDENACAO THRU 142-99-FIM
                        UNTIL COLUNAS-ORDENADAS NOT = 0
           END-IF
           IF   COLUNAS-ORDENADAS = 1
                MOVE 1 TO NIVEL-QUEBRA (LIN-E)
           ELSE
                MOVE SPACES               TO CWBOXS-ITENS
                MOVE NIVEL-QUEBRA (LIN-E) TO CWBOXS-OPTION
                MOVE 13                   TO CWBOXS-LINE
                MOVE 05                   TO CWBOXS-COLUMN
                MOVE "_Niveis_de_quebra:_________________Modo"
                  TO CWBOXS-TITLE
                MOVE 0 TO CQS
                PERFORM COLUNAS-ORDENADAS TIMES
                  ADD  1                     TO CQS
                  MOVE CQS                   TO CWBOXS-CHAR (CQS)
                                                CWBOXS-TEXT (CQS)
                  MOVE "-"                   TO CWBOXS-TEXT (CQS) (3:)
                  MOVE COLUNA-ORDENADA (CQS) TO CWBOXS-TEXT (CQS) (5:)
                  MOVE COLUNA-DESCENDE (CQS) TO CWBOXS-TEXT (CQS) (36:)
                END-PERFORM
                PERFORM TEST AFTER UNTIL NIVEL-QUEBRA (LIN-E) NOT = 0
                         CALL "CWBOXS"   USING PARAMETROS-CWBOXS
                         IF   CWBOXS-OPTION NOT = 0
                              MOVE CWBOXS-OPTION TO NIVEL-QUEBRA (LIN-E)
                         END-IF
                END-PERFORM
           END-IF

           PERFORM 110-EXIBE-TELA    THRU 110-99-FIM.

       141-99-FIM. EXIT.

       142-ORDENACAO.

           MOVE CWBOXF-COLOR-FRAME   TO CWBOXW-COLOR-FRAME
           MOVE CWBOXF-COLOR-BORDER  TO CWBOXW-COLOR-BORDER
           MOVE 13                   TO CWBOXW-LINE
                                        CWBOXF-LINE
           MOVE 05                   TO CWBOXW-COLUMN
           MOVE 07                   TO CWBOXW-VERTICAL-LENGTH
                                        CWBOXF-VERTICAL-LENGTH
                   IF  MAIOR-NOME < 9
                       MOVE 9  TO MAIOR-NOME
                   END-IF
           COMPUTE CWBOXW-HORIZONTAL-LENGTH = MAIOR-NOME + 6
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = MAIOR-NOME + 2
           IF   CWBOXF-HORIZONTAL-LENGTH < 21
                MOVE 21 TO CWBOXF-HORIZONTAL-LENGTH
           END-IF
           SET  CWBOXW-OPEN TO TRUE
           CALL "CWKBDC" USING "9595"
           CALL "CWBOXW" USING PARAMETROS-CWBOXW
           MOVE SPACES        TO CWBOXF-OPTION
           MOVE "O"           TO CWBOXF-WORK-AREA (1: 1)
           MOVE 44            TO CWBOXF-COLUMN
           MOVE 2             TO CWBOXF-RETURN CWBOXF-ORDER
           SET  CWBOXF-SHOW   TO TRUE
           ADD  2             TO CWBOXW-COLUMN
           MOVE CWBOXW-LINE   TO LW
           MOVE CWBOXW-COLUMN TO CW
           MOVE 11            TO SW
           CALL "CWMSGW" USING W " Ordena‡Æo "
           COMPUTE SW = MAIOR-NOME + 5

           PERFORM DISPLAY-ORDEM

           PERFORM TEST AFTER UNTIL DATANAME = SPACES
              MOVE "CWREL4"              TO CWBOXF-PROGRAM
              PERFORM 143-HELP THRU 143-99-FIM
              MOVE CWBOXF-OPTION         TO DATANAME
              MOVE 0                     TO APAGOU-DATANAME
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > CQS
                 IF  DATANAME = COLUNA-ORDENADA (I)
                 AND (DATANAME NOT = SPACES)
                     IF  CWBOXF-EDIT = 60
                         IF   COLUNA-DESCENDE (I) = "D"
                              MOVE SPACE    TO COLUNA-DESCENDE (I)
                         ELSE
                              MOVE "D"      TO COLUNA-DESCENDE (I)
                         END-IF
                         MOVE 3             TO APAGOU-DATANAME
                         EXIT PERFORM
                     END-IF
                     IF  I = CQS
                         SUBTRACT 1 FROM COLUNAS-ORDENADAS
                         MOVE 1       TO APAGOU-DATANAME
                         MOVE SPACES  TO COLUNA-ORDENADA (I)
                                         COLUNA-DESCENDE (I)
                                         DATANAME
                         PERFORM UNTIL I NOT < CQS
                                 MOVE I TO Y
                                 ADD  1 TO I
                                 IF I < 8
                                    IF COLUNA-ORDENADA (I) NOT = SPACES
                                       MOVE COLUNA-ORDENADA (I)
                                         TO COLUNA-ORDENADA (Y)
                                       MOVE COLUNA-DESCENDE (I)
                                         TO COLUNA-DESCENDE (Y)
                                       MOVE SPACES
                                         TO COLUNA-ORDENADA (I)
                                            COLUNA-DESCENDE (I)
                                    END-IF
                                 END-IF
                         END-PERFORM
                     ELSE
                         MOVE 2 TO APAGOU-DATANAME
                     END-IF
                 END-IF
              END-PERFORM
              IF  (DATANAME NOT = SPACES)
              AND  CQS < 7
              AND  APAGOU-DATANAME = 0
                   ADD  1        TO CQS
                   MOVE CQS      TO COLUNAS-ORDENADAS
                   MOVE DATANAME TO COLUNA-ORDENADA   (CQS)
                   IF  CWBOXF-EDIT = 60
                       MOVE "D"  TO COLUNA-DESCENDE   (CQS)
                   END-IF
              END-IF
              PERFORM DISPLAY-ORDEM
           END-PERFORM

           SET  CWBOXW-CLOSE TO TRUE
           CALL "CWBOXW" USING PARAMETROS-CWBOXW
           CALL "CWKBDC" USING "9999"

           IF   APAGOU-DATANAME = 1
                IF   NOT ARQUIVO
                     MOVE CWBOXF-LINE TO CWBOXW-LINE
                     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99
                             IF   STATUS-LINHA (I) = "Q"
                             AND  NIVEL-QUEBRA (I) > COLUNAS-ORDENADAS
                                  MOVE "K" TO STATUS-LINHA (I)
                             END-IF
                     END-PERFORM
                     PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                END-IF
                GO TO 142-ORDENACAO
           END-IF.

           CANCEL "CWREL9"
           CALL   "CWREL9" USING MAPA-ORDENACAO.

       142-99-FIM. EXIT.

       DISPLAY-ORDEM.

           MOVE CWBOXW-LINE   TO LW
           MOVE 0 TO CQS
           PERFORM COLUNAS-ORDENADAS TIMES
                   ADD  1      TO CQS LW
                   MOVE SPACES TO POP
                   STRING CQS " " COLUNA-ORDENADA (CQS) (1: MAIOR-NOME)
                              " " COLUNA-DESCENDE (CQS)
                              DELIMITED BY SIZE INTO POP
                   CALL "CWMSGW" USING W POP
           END-PERFORM.

       FIM-DISPLAY-ORDEM. EXIT.

       143-HELP.

           MOVE SPACES TO LB-TEXTO
           STRING FORMATOS-PROVEDOR DELIMITED BY SPACE
                  ".###" DELIMITED BY SIZE
                  INTO LB-TEXTO
           CALL "CWFILE" USING LB-TEXTO
           CALL "CWBINF" USING 'O' FS-TEXTO LB-TEXTO
           OPEN INPUT HELP
           PERFORM UNTIL FS-HELP > '09'
                   READ HELP
                   IF FS-HELP < '10'
                      CALL "CWBINF" USING 'W' FS-TEXTO HELP-REG
                   END-IF
           END-PERFORM
           CLOSE HELP
           CALL "CWBINF" USING 'C' FS-TEXTO
           MOVE LB-TEXTO TO CWHELP-FILE
           MOVE 10      TO CWHELP-LINE
           MOVE 10      TO CWHELP-COLUMN
           MOVE 10      TO CWHELP-VERTICAL-LENGTH
           MOVE 60      TO CWHELP-HORIZONTAL-LENGTH
           PERFORM TEST AFTER
                  UNTIL CWBOXF-EDIT = 42 OR 78 OR 12336 OR 27
                        OR (CWBOXF-WORK-AREA (1: 1) = "O"
                            AND (CWBOXF-EDIT = 60 oR 13)
                            AND (CWBOXF-OPTION NOT = SPACES))
                   IF   LB-HELP NOT = SPACES
                        MOVE "_Colunas:_ (F1-Help)" TO CWBOXF-TITLE
                        SET CWBOXF-EDIT-ON TO TRUE
                   ELSE
                        IF  CWBOXF-WORK-AREA (1: 1) = "O"
                            MOVE "_Colunas:_[F2]_Modo_"
                              TO CWBOXF-TITLE
                            SET CWBOXF-EDIT-ON TO TRUE
                        ELSE
                            MOVE "_Colunas:_" TO CWBOXF-TITLE
                            MOVE "N"          TO CWBOXF-KEY-ON
                        END-IF
                   END-IF
                   CALL "CWBOXF" USING PARAMETROS-CWBOXF
                   IF   CWBOXF-EDIT = 59
                        CALL "CWHELP" USING PARAMETROS-CWHELP
                   END-IF
           END-PERFORM
           CALL "CWBINF" USING 'D' FS-TEXTO.

       143-99-FIM. EXIT.

       144-COLAR.

           ADD  1             TO OBJETO-R
           MOVE COPIADO       TO OBJETO-ATTR    (OBJETO-R)
           MOVE LIN-E         TO OBJETO-RL      (OBJETO-R)
                                 LIN-W
           MOVE COL-E         TO OBJETO-RC      (OBJETO-R)
                                 COL-W
           MOVE COPY-ELEMENTO TO MAPA-LIN (LIN-W)
                                          (COL-W: COPY-TAMANHO)
           PERFORM COPY-TAMANHO TIMES
                   MOVE OBJETO-R TO CX-M (LIN-W COL-W)
                   ADD 1 TO COL-W
           END-PERFORM
           PERFORM 147-ULTIMO-DETALHE THRU 147-99-FIM
           PERFORM 110-EXIBE-TELA     THRU 110-99-FIM.

       144-99-FIM. EXIT.

       145-APAGA-OBJETO.

           MOVE OBJETO-RL      (OBJETO-S) TO LIN-W
           MOVE OBJETO-RC      (OBJETO-S) TO COL-W
           MOVE OBJETO-TAMANHO (OBJETO-S) TO TAMANHO
           MOVE SPACES           TO MAPA-LIN (LIN-W) (COL-W: TAMANHO)
           MOVE LOW-VALUES       TO OBJETO-ATTR (OBJETO-S)
           PERFORM TAMANHO TIMES
                   MOVE 0 TO CX-M (LIN-W COL-W)
                   ADD  1 TO COL-W
           END-PERFORM
           PERFORM 147-ULTIMO-DETALHE THRU 147-99-FIM
           PERFORM 130-MONITOR        THRU 130-99-FIM.

       145-APAGOU.

           PERFORM 110-EXIBE-TELA THRU 110-99-FIM.

       145-99-FIM. EXIT.

       INSERIR-RESULTADO.

           MOVE "_Primeiro_operando:_" TO CWBOXS-TITLE
           MOVE SPACES                 TO CWBOXS-ITENS
           MOVE 0                      TO R2
           MOVE 20                     TO MAIOR-NOME
           PERFORM VARYING R FROM 1 BY 1 UNTIL R > OBJETO-R OR R2 = 21
                   IF OBJETO-RL (R) = LIN-E
                   AND (OBJETO-TIPO (R) = '9' OR 'E')
                       ADD  1                   TO R2
                       MOVE OBJETO-DATANAME (R) TO CWBOXS-CAPTION (R2)
                       PERFORM VARYING MAIOR-NOME
                                  FROM MAIOR-NOME BY 1
                                     UNTIL CWBOXS-CAPTION (R2)
                                     (MAIOR-NOME: 1) = SPACE
                               CONTINUE
                       END-PERFORM
                   END-IF
           END-PERFORM
           IF R2 NOT = 0
              MOVE LIN-S   TO CWBOXS-LINE
              MOVE COL-S   TO CWBOXS-COLUMN
              PERFORM UNTIL (CWBOXS-COLUMN + MAIOR-NOME + 4) < 78
                      SUBTRACT 1 FROM CWBOXS-COLUMN
              END-PERFORM
              PERFORM UNTIL (CWBOXS-LINE + 2 + R2) < 24
                      SUBTRACT 1 FROM CWBOXS-LINE
              END-PERFORM
              MOVE 1 TO CWBOXS-OPTION
              CALL "CWBOXS"  USING PARAMETROS-CWBOXS
              IF   CWBOXS-OPTION NOT = 0
                   ADD 1 TO OBJETO-R
                   MOVE CWBOXS-CAPTION (CWBOXS-OPTION)
                                        TO OBJETO-DATANAME (OBJETO-R)
                   MOVE LIN-E           TO OBJETO-RL (OBJETO-R) LIN-W
                   MOVE COL-E           TO OBJETO-RC (OBJETO-R) COL-W
                   MOVE SPACES          TO CWBOXS-TITLE
                                           CWBOXS-ITENS
                   MOVE "~+ Soma"       TO CWBOXS-TEXT (1)
                   MOVE "~- Subtrai"    TO CWBOXS-TEXT (2)
                   MOVE "~* Multiplica" TO CWBOXS-TEXT (3)
                   MOVE "~/ Divide"     TO CWBOXS-TEXT (4)
                   MOVE "~Potˆncia"     TO CWBOXS-TEXT (5)
                   MOVE "~Raiz"         TO CWBOXS-TEXT (6)
                   MOVE 4               TO CWBOXS-OPTION
                   CALL "CWBOXS" USING PARAMETROS-CWBOXS
                   IF   CWBOXS-OPTION NOT = 0
                        MOVE CWBOXS-OPTION-CHAR TO OBJETO-TIPO
                                                   (OBJETO-R)
                        MOVE "_Segundo_operando:_" TO CWBOXS-TITLE
                        MOVE SPACES                TO CWBOXS-ITENS
                        MOVE 0 TO R2
                        PERFORM VARYING R FROM 1 BY 1
                                  UNTIL (R > OBJETO-R - 1) OR R2 = 21
                                IF OBJETO-RL (R) = LIN-E
                                AND (OBJETO-TIPO (R) = '9' OR 'E')
                                AND (OBJETO-DATANAME (R)
                                    NOT = OBJETO-DATANAME (OBJETO-R))
                                    ADD  1                   TO R2
                                    MOVE OBJETO-DATANAME (R)
                                      TO CWBOXS-CAPTION (R2)
                                END-IF
                        END-PERFORM
                        MOVE 1 TO CWBOXS-OPTION
                        CALL "CWBOXS"  USING PARAMETROS-CWBOXS
                        IF   CWBOXS-OPTION NOT = 0
                             MOVE CWBOXS-CAPTION (CWBOXS-OPTION)
                               TO OBJETO-DATANAME2 (OBJETO-R)
                        END-IF
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        INITIALIZE OBJETO-ATTR (OBJETO-R)
                        SUBTRACT 1 FROM OBJETO-R
                   ELSE
                        MOVE 0      TO TAMANHO
                        MOVE SPACES TO ELEMENTO
                        MOVE COL-E  TO R
                        PERFORM UNTIL TAMANHO = 23
                                IF  MAPA-LIN (LIN-E) (R: 1) NOT = SPACES
                                    EXIT PERFORM
                                ELSE
                                    ADD 1 TO TAMANHO
                                    MOVE 'Z' TO ELEMENTO (TAMANHO:1)
                                END-IF
                                ADD 1 TO R
                        END-PERFORM
                        MOVE '+' TO ELEMENTO (TAMANHO:1)
                        MOVE TAMANHO
                          TO OBJETO-TAMANHO (OBJETO-R)
                        PERFORM TAMANHO TIMES
                                MOVE OBJETO-R TO CX-M (LIN-W COL-W)
                                ADD 1 TO COL-W
                        END-PERFORM
                        MOVE FORMATOS-PROVEDOR    TO CAMPOS-PROVEDOR
                        MOVE OBJETO-DATANAME (OBJETO-R)
                                                  TO CAMPOS-DATANAME
                        MOVE RELATORIO            TO CAMPOS-RELATORIO
                        READ CAMPOS KEY IS CAMPOS-PRONAMREL
                        IF   FS-CAMPOS = "23"
                             MOVE SPACES TO CAMPOS-RELATORIO
                             READ CAMPOS KEY IS CAMPOS-PRONAMREL
                        END-IF
                        MOVE CAMPOS-DEC     TO OP-DEC
                        MOVE FORMATOS-PROVEDOR    TO CAMPOS-PROVEDOR
                        MOVE OBJETO-DATANAME2 (OBJETO-R)
                                                  TO CAMPOS-DATANAME
                        MOVE RELATORIO            TO CAMPOS-RELATORIO
                        READ CAMPOS KEY IS CAMPOS-PRONAMREL
                        IF   FS-CAMPOS = "23"
                             MOVE SPACES TO CAMPOS-RELATORIO
                             READ CAMPOS KEY IS CAMPOS-PRONAMREL
                        END-IF
                        IF OBJETO-TIPO (OBJETO-R) = 'R' OR '/'
                           IF CAMPOS-DEC NOT = 0
                              ADD CAMPOS-DEC     TO OP-DEC
                           ELSE
                              ADD     OP-DEC     TO OP-DEC
                           END-IF
                           IF OP-DEC = 0
                              MOVE 4 TO OP-DEC
                           END-IF
                        ELSE
                           IF CAMPOS-DEC > OP-DEC
                              MOVE CAMPOS-DEC TO OP-DEC
                           END-IF
                        END-IF
                        COMPUTE R = TAMANHO - 1
                        IF OP-DEC NOT = 0
                           SUBTRACT OP-DEC FROM R
                           MOVE ',' TO ELEMENTO(R:1)
                           SUBTRACT 1 FROM R
                           INSPECT ELEMENTO (R:) CONVERTING 'Z' TO '9'
                        ELSE
                           MOVE '9' TO ELEMENTO(R:1)
                        END-IF
                        IF R > 3
                           SUBTRACT 3 FROM R
                           IF R > 1
                              MOVE '.' TO ELEMENTO(R:1)
                           END-IF
                           PERFORM UNTIL R NOT > 4
                                   SUBTRACT 4 FROM R
                                   IF R > 1
                                      MOVE '.' TO ELEMENTO(R:1)
                                   END-IF
                           END-PERFORM
                        END-IF
                        MOVE ELEMENTO TO MAPA-LIN (LIN-E)
                                                  (COL-E: TAMANHO)
                        MOVE 0 TO COL-EA
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                   END-IF
              END-IF
           ELSE
              MOVE "Linha nÆo cont‚m valores" TO CWSEND-MSG
              MOVE SPACES          TO CWSEND-SCREENS
              CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF.

       FIM-INSERIR-RESULTADO. EXIT.

       146-INSERIR-COLUNA.

           MOVE SPACES        TO CWBOXF-OPTION
           MOVE LIN-S         TO CWBOXF-LINE
           MOVE COL-S         TO CWBOXF-COLUMN
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = MAIOR-NOME + 2
           MOVE 1             TO CWBOXF-RETURN
           SET  CWBOXF-SHOW   TO TRUE
           SUBTRACT 1 FROM CWBOXF-LINE
                           CWBOXF-COLUMN
           PERFORM UNTIL (CWBOXF-COLUMN + CWBOXF-HORIZONTAL-LENGTH)
                                        < 77
                   SUBTRACT 1 FROM CWBOXF-COLUMN
           END-PERFORM
           PERFORM UNTIL (CWBOXF-LINE + 7)  < 24
                   SUBTRACT 1 FROM CWBOXF-LINE
           END-PERFORM
           MOVE STATUS-LINHA (LIN-E) TO CWBOXF-WORK-AREA (1: 1)
           IF  (LIN-E NOT > DET)
           AND  CWBOXF-WORK-AREA (1: 1) = "Q"
                MOVE "q" TO CWBOXF-WORK-AREA (1: 1)
           END-IF
           MOVE 5                    TO CWBOXF-VERTICAL-LENGTH
           MOVE 2                    TO CWBOXF-ORDER
           MOVE 1                    TO CWBOXF-RETURN
           IF   LIN-E NOT > CAB
           AND (CAB NOT = 0)
           AND (RELATOR-RESTRICT NOT = "OFF")
                OPEN OUTPUT CABS
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > COLUNAS-ORDENADAS
                        MOVE COLUNA-ORDENADA (I) TO CABS-REG
                        WRITE CABS-REG
                END-PERFORM
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                        MOVE "Relator-"      TO CABS-REG
                        MOVE RELATOR-CAB (I) TO CABS-REG (9: )
                        WRITE CABS-REG
                END-PERFORM
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99
                        IF   PROMPT-DATANAME (I) NOT = SPACE
                             MOVE "Prompt-"           TO CABS-REG
                             MOVE PROMPT-DATANAME (I) TO CABS-REG (8: )
                             WRITE CABS-REG
                        END-IF
                END-PERFORM
                CLOSE CABS
                MOVE SPACES           TO CWBOXF-OPTION
                MOVE CWBOXF-WORK-AREA TO SAVE-WORK-AREA
                MOVE LB-CABS          TO CWBOXF-WORK-AREA
                MOVE "CWREL5"         TO CWBOXF-PROGRAM
                PERFORM 143-HELP THRU 143-99-FIM
                DELETE FILE CABS
                MOVE "CWREL4"         TO CWBOXF-PROGRAM
                MOVE SAVE-WORK-AREA   TO CWBOXF-WORK-AREA
                IF   CWBOXF-OPTION NOT = SPACES
                     MOVE FORMATOS-PROVEDOR    TO CAMPOS-PROVEDOR
                     MOVE CWBOXF-OPTION        TO CAMPOS-DATANAME
                     MOVE SPACES               TO CAMPOS-RELATORIO
                     IF   CWBOXF-OPTION (1: 7) = "Prompt-"
                     OR   CAMPOS-DATANAME (1: 3) = "(=)"
                          MOVE RELATORIO       TO CAMPOS-RELATORIO
                     END-IF
                     READ CAMPOS KEY IS CAMPOS-PRONAMREL
                     IF   FS-CAMPOS < "09"
                          MOVE CAMPOS-COLUNA (1: 4)
                            TO CWBOXF-OPTION (1: 4)
                     ELSE
                          MOVE SPACES TO CWBOXF-OPTION
                     END-IF
                END-IF
           ELSE
                MOVE   "CWREL4"          TO CWBOXF-PROGRAM
                PERFORM 143-HELP THRU 143-99-FIM
           END-IF
           IF   CWBOXF-OPTION NOT = SPACES
                MOVE ALL "."              TO ELEMENTO
                MOVE FORMATOS-PROVEDOR    TO CAMPOS-PROVEDOR
                MOVE CWBOXF-OPTION (1: 4) TO CAMPOS-COLUNA (1: 4)
                READ CAMPOS
                MOVE LIN-E                TO LIN-W
                MOVE COL-E                TO COL-W
                MOVE CAMPOS-TAMANHO       TO TAMANHO
                IF   CAMPOS-EDITADO
                     MOVE CAMPOS-MASCARA TO MASCARA
                     IF   MASCARA NOT = SPACES
                          PERFORM VARYING TAMANHO FROM LENGTH OF MASCARA
                               BY -1
                            UNTIL MASCARA (TAMANHO: 1) NOT = SPACE
                                 CONTINUE
                          END-PERFORM
                          MOVE "E"     TO CAMPOS-TIPO
                          MOVE MASCARA TO ELEMENTO
                     END-IF
                END-IF
                MOVE "S"         TO MOVER-M
                IF  CAMPOS-CGC-CIC
                    IF   TAMANHO = 11
                         MOVE "999.999.999-99" TO ELEMENTO
                         MOVE 14               TO TAMANHO
                         MOVE "E"              TO CAMPOS-TIPO
                    ELSE
                         IF   TAMANHO = 14
                              MOVE "99.999.999/9999-99" TO ELEMENTO
                              MOVE 18               TO TAMANHO
                              MOVE "E"              TO CAMPOS-TIPO
                         END-IF
                    END-IF
                END-IF
                IF  CAMPOS-VALOR
                OR (CAMPOS-DEC NOT = 0)
                    MOVE   SPACES    TO ELEMENTO
                    SUBTRACT CAMPOS-DEC FROM TAMANHO
                    DIVIDE 3 INTO TAMANHO GIVING MODULOS
                           REMAINDER RESTO
                    MOVE RESTO TO TAMANHO
                    IF   TAMANHO NOT = 0
                         IF   CAMPOS-VALOR
                              MOVE ALL "Z" TO ELEMENTO (1: TAMANHO)
                         ELSE
                              MOVE ALL "9" TO ELEMENTO (1: TAMANHO)
                         END-IF
                         ADD  1   TO TAMANHO
                         MOVE "." TO ELEMENTO (TAMANHO: 1)
                    END-IF
                    PERFORM MODULOS TIMES
                            ADD  1      TO TAMANHO
                            IF   CAMPOS-VALOR
                                 MOVE "ZZZ." TO ELEMENTO (TAMANHO: 4)
                            ELSE
                                 MOVE "999." TO ELEMENTO (TAMANHO: 4)
                            END-IF
                            ADD  3      TO TAMANHO
                    END-PERFORM
                    IF   TAMANHO NOT = 0
                         SUBTRACT 1 FROM TAMANHO
                         IF   TAMANHO NOT = 0
                              MOVE "9" TO ELEMENTO (TAMANHO: 1)
                         END-IF
                    END-IF
                    IF   CAMPOS-DEC NOT = 0
                         ADD  1   TO TAMANHO
                         MOVE "," TO ELEMENTO (TAMANHO: 1)
                         ADD 1 TO TAMANHO
                         MOVE ALL "9" TO ELEMENTO(TAMANHO: CAMPOS-DEC)
                         ADD CAMPOS-DEC  TO TAMANHO
                         SUBTRACT 1 FROM TAMANHO
                    END-IF
                    IF  (CAMPOS-DATANAME (1: 7) NOT = "Relator")
                    AND  CAMPOS-VALOR
                         ADD 1    TO TAMANHO
                         MOVE "-" TO ELEMENTO (TAMANHO: 1)
                    END-IF
                    MOVE "E"             TO CAMPOS-TIPO
                END-IF
      *         MOVE LIN-E       TO LIN-W
      *         MOVE COL-E       TO COL-W
      *         PERFORM TAMANHO TIMES
      *                 IF   CX-M (LIN-W COL-W) NOT = 0
      *                      MOVE "N" TO MOVER-M
      *                 END-IF
      *                 ADD 1 TO COL-W
      *         END-PERFORM
                IF   MAPA-LIN (LIN-E) (COL-E: TAMANHO) = SPACES
                     EVALUATE TRUE
                       WHEN CAMPOS-ALFANUMERICO
                            MOVE ALL "X" TO ELEMENTO CAMPOS-TIPO
                       WHEN CAMPOS-LITERAL
                            MOVE FORMATOS-STRING TO ELEMENTO
                            MOVE "L"             TO CAMPOS-TIPO
                       WHEN CAMPOS-NUMERICO
                            MOVE ALL "9" TO ELEMENTO CAMPOS-TIPO
                       WHEN CAMPOS-HORA
                            IF   CAMPOS-MASCARA = SPACES
                                 EVALUATE CAMPOS-TAMANHO
                                     WHEN 6 MOVE "HH:MM:SS" TO ELEMENTO
                                     WHEN 4 MOVE "HH:MM"    TO ELEMENTO
                                     WHEN 2 MOVE "HH"       TO ELEMENTO
                                 END-EVALUATE
                            ELSE
                                 MOVE CAMPOS-MASCARA TO ELEMENTO
                            END-IF
                            MOVE "E"             TO CAMPOS-TIPO
                       WHEN CAMPOS-DATA
                            MOVE "E"             TO CAMPOS-TIPO
                            PERFORM 151-EDIT-DATE THRU 151-99-FIM
                            MOVE CAMPOS-MASCARA TO ELEMENTO
                     END-EVALUATE
                     IF  CAMPOS-MASCARA NOT = SPACES
                         PERFORM VARYING TAMANHO FROM 30 BY -1
                         UNTIL CAMPOS-MASCARA (TAMANHO: 1) NOT = SPACE
                                 CONTINUE
                         END-PERFORM
                     END-IF
                     IF   MAPA-LIN (LIN-E) (COL-E: TAMANHO) = SPACES
                          ADD  1     TO OBJETO-R
                          MOVE LIN-E TO OBJETO-RL (OBJETO-R)
                          MOVE COL-E TO OBJETO-RC (OBJETO-R)
                          MOVE TAMANHO
                            TO OBJETO-TAMANHO (OBJETO-R)
                          MOVE CAMPOS-TIPO
                            TO OBJETO-TIPO    (OBJETO-R)
                          MOVE CAMPOS-DATANAME
                            TO OBJETO-DATANAME(OBJETO-R)
                          MOVE LIN-E           TO LIN-W
                          MOVE COL-E           TO COL-W
                          MOVE ELEMENTO        TO MAPA-LIN (LIN-W)
                                                  (COL-W: TAMANHO)
                          PERFORM TAMANHO TIMES
                                  MOVE OBJETO-R TO CX-M (LIN-W COL-W)
                                  ADD 1 TO COL-W
                          END-PERFORM
                          IF   LIN-E > DET
                          AND (STATUS-LINHA (LIN-E) = SPACE
                                                  OR "K" OR "t")
                               MOVE LIN-E TO DET
                          END-IF
                          PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                     ELSE
                          CALL X"E5"
                     END-IF
                ELSE
                     CALL X"E5"
                END-IF
           END-IF

           MOVE 0 TO LIN-EA
           MOVE 0 TO COL-EA
           PERFORM 130-MONITOR THRU 130-99-FIM.

       146-99-FIM. EXIT.

       14X-INSERIR-ESTILO.

           MOVE "_Estilos:_"  TO CWBOXS-TITLE
           MOVE 1             TO CWBOXS-OPTION
           MOVE LIN-S         TO CWBOXS-LINE
           MOVE COL-S         TO CWBOXS-COLUMN
           SUBTRACT 1 FROM CWBOXS-LINE
                           CWBOXS-COLUMN
           PERFORM UNTIL (CWBOXS-COLUMN + 13) < 79
                   SUBTRACT 1 FROM CWBOXS-COLUMN
           END-PERFORM
           PERFORM UNTIL (CWBOXS-LINE + 16)  < 24
                   SUBTRACT 1 FROM CWBOXS-LINE
           END-PERFORM
           MOVE SPACES       TO CWBOXS-ITENS
           MOVE '~6/"'       TO CWBOXS-TEXT   (01)
           MOVE '~8/"'       TO CWBOXS-TEXT   (02)
           MOVE "~Azul"      TO CWBOXS-TEXT   (03)
           MOVE "~Carta"     TO CWBOXS-TEXT   (04)
           MOVE "c~Ondensar" TO CWBOXS-TEXT   (05)
           MOVE "~Draft"     TO CWBOXS-TEXT   (06)
           MOVE "~Expandir"  TO CWBOXS-TEXT   (07)
           MOVE "~It lico"   TO CWBOXS-TEXT   (08)
           MOVE "~Negrito"   TO CWBOXS-TEXT   (09)
           MOVE "nor~Mal"    TO CWBOXS-TEXT   (10)
           MOVE "~Paisagem"  TO CWBOXS-TEXT   (11)
           MOVE "~Reset"     TO CWBOXS-TEXT   (12)
           MOVE "~Salto"     TO CWBOXS-TEXT   (13)
           MOVE "su~Blinhar" TO CWBOXS-TEXT   (14)
           MOVE "~Verde"     TO CWBOXS-TEXT   (15)
           MOVE "vermel~Ho"  TO CWBOXS-TEXT   (16)
           CALL "CWBOXS"  USING PARAMETROS-CWBOXS
           IF   CWBOXS-OPTION NOT = 0
                ADD  1                 TO OBJETO-R
                MOVE LIN-E             TO OBJETO-RL      (OBJETO-R)
                MOVE COL-E             TO OBJETO-RC      (OBJETO-R)
                MOVE 1                 TO OBJETO-TAMANHO (OBJETO-R)
                MOVE "A"               TO OBJETO-TIPO    (OBJETO-R)
                MOVE CWBOXS-TEXT   (CWBOXS-OPTION)
                                       TO OBJETO-DATANAME(OBJETO-R)
                INSPECT OBJETO-DATANAME(OBJETO-R)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
                INSPECT OBJETO-DATANAME(OBJETO-R) (1: 1)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE LIN-E             TO LIN-W
                MOVE COL-E             TO COL-W
                MOVE X"FE"             TO MAPA-LIN (LIN-W) (COL-W: 1)
                MOVE OBJETO-R          TO CX-M (LIN-W COL-W)
                ADD  1                 TO COL-W
                IF   LIN-E > DET
                AND (STATUS-LINHA (LIN-E) = SPACE OR "K" or "t")
                     MOVE LIN-E TO DET
                END-IF
                PERFORM 110-EXIBE-TELA THRU 110-99-FIM
           END-IF

           MOVE 0 TO LIN-EA
           MOVE 0 TO COL-EA
           PERFORM 130-MONITOR THRU 130-99-FIM.

       14X-99-FIM. EXIT.

       147-ULTIMO-DETALHE.

           MOVE 0 TO DET
           PERFORM VARYING OBJETO-MW
                      FROM 1 BY 1
                     UNTIL OBJETO-MW > OBJETO-R
                   MOVE OBJETO-RL (OBJETO-MW) TO LIN-W
                   IF   LIN-W > DET
                   AND (STATUS-LINHA (LIN-W) = SPACE OR "K" or "t")
                        MOVE LIN-W TO DET
                   END-IF
           END-PERFORM.

       147-99-FIM. EXIT.

       148-INSERIR-LINHA.

           MOVE MAPA-LIN     (99) TO MAPA-LIN-99
           MOVE MAPA-LIN-M   (99) TO MAPA-LIN-M-99
           MOVE NIVEL-QUEBRA (99) TO NIVEL-QUEBRA-99
           MOVE STATUS-LINHA (99) TO STATUS-LINHA-99

           PERFORM VARYING I FROM 98 BY -1 UNTIL I < LIN-E
                   COMPUTE Y = I + 1
                   MOVE MAPA-LIN     (I) TO MAPA-LIN     (Y)
                   MOVE MAPA-LIN-M   (I) TO MAPA-LIN-M   (Y)
                   MOVE NIVEL-QUEBRA (I) TO NIVEL-QUEBRA (Y)
                   MOVE STATUS-LINHA (I) TO STATUS-LINHA (Y)
           END-PERFORM

           IF   MAPA-LIN-99 NOT = SPACES
                MOVE MAPA-LIN-99     TO MAPA-LIN     (LIN-E)
                MOVE MAPA-LIN-M-99   TO MAPA-LIN-M   (LIN-E)
                MOVE NIVEL-QUEBRA-99 TO NIVEL-QUEBRA (LIN-E)
                MOVE STATUS-LINHA-99 TO STATUS-LINHA (LIN-E)
           ELSE
                MOVE SPACES          TO MAPA-LIN     (LIN-E)
                INITIALIZE              MAPA-LIN-M   (LIN-E)
                MOVE 0               TO NIVEL-QUEBRA (LIN-E)
                MOVE SPACE           TO STATUS-LINHA (LIN-E)
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > OBJETO-R
                   IF  (OBJETO-RL (I) NOT = 0)
                   AND (OBJETO-RL (I) NOT < LIN-E)
                        ADD 1 TO OBJETO-RL (I)
                        IF  OBJETO-RL (I) = 100
                            MOVE LIN-E TO OBJETO-RL (I)
                        END-IF
                   END-IF
           END-PERFORM

           IF   LIN-E NOT > CAB
                ADD 1 TO CAB
           END-IF

           IF   LIN-E NOT > DET
                ADD 1 TO DET
           END-IF

           PERFORM 147-ULTIMO-DETALHE THRU 147-99-FIM

           PERFORM 110-EXIBE-TELA THRU 110-99-FIM.

       148-99-FIM. EXIT.

       149-APAGAR-LINHA.

           IF   MAPA-LIN (LIN-E) NOT = SPACES
                COMPUTE ROW-NUMBER = LIN-S - 1
                IF   CWUNIX-GUI
                     MOVE 3 TO COLUMN-NUMBER
                ELSE
                     MOVE 4 TO COLUMN-NUMBER
                END-IF
                MOVE 75 TO STRING-LENGTH
                CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
                MOVE CARACTER-BUFFER TO SAVE-LIN
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH
                        ADD 128 TO AB (I)
                END-PERFORM
                IF   SAVE-LIN = SPACES
                AND  CWUNIX-OFF
                     MOVE ALL-XB2 TO CARACTER-BUFFER
                END-IF
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
                MOVE "Apagar linha ?" TO CWSEND-MSG
                MOVE 2                TO CWSEND-OPTION
                MOVE SPACES           TO CWSEND-SCREENS
                MOVE "_~Sim_"         TO CWSEND-SCREEN (1)
                MOVE "_~NÆo_"         TO CWSEND-SCREEN (2)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION NOT = 1
                     PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I > STRING-LENGTH
                             SUBTRACT 128 FROM AB (I)
                     END-PERFORM
                     IF   SAVE-LIN = SPACES
                          MOVE SAVE-LIN TO CARACTER-BUFFER
                     END-IF
                     CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                        CARACTER-BUFFER
                                                        ATTRIBUTE-BUFFER
                                                        STRING-LENGTH
                     GO TO 149-99-FIM
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > OBJETO-R
                        IF   OBJETO-RL (I) = LIN-E
                             MOVE I TO OBJETO-S
                             PERFORM 145-APAGA-OBJETO
                        END-IF
                END-PERFORM
           ELSE
                IF   LIN-E NOT > DET
                     SUBTRACT 1 FROM DET
                END-IF
           END-IF

           PERFORM VARYING I FROM LIN-E BY 1 UNTIL I = 99
                   COMPUTE Y = I + 1
                   MOVE MAPA-LIN     (Y) TO MAPA-LIN     (I)
                   MOVE MAPA-LIN-M   (Y) TO MAPA-LIN-M   (I)
                   MOVE NIVEL-QUEBRA (Y) TO NIVEL-QUEBRA (I)
                   MOVE STATUS-LINHA (Y) TO STATUS-LINHA (I)
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > OBJETO-R
                   IF   OBJETO-RL (I) > LIN-E
                        SUBTRACT 1 FROM OBJETO-RL (I)
                   END-IF
           END-PERFORM

           IF  (LIN-E NOT > CAB)
           AND (CAB NOT = 0)
                SUBTRACT 1 FROM CAB
           END-IF

           PERFORM 110-EXIBE-TELA THRU 110-99-FIM.

       149-99-FIM. EXIT.

       150-TECLA-COMUM.

           IF   LIN-E > TOT2
           AND  TOT > 0
                CALL X"E5"
                GO TO 150-99-FIM
           END-IF

           IF  CARACTER = SPACE
               MOVE X"00" TO CARACTER
           END-IF

           IF  OBJETO-T NOT = 0
           AND(OBJETO-TIPO (OBJETO-T) = "E" OR "9" OR "Z"
                           OR OBJETO-RESULTADO (OBJETO-T))
               MOVE LIN-E TO LIN-W
               MOVE COL-E TO COL-W
               MOVE CX (LIN-W COL-W) TO CHAR
               IF  CARACTER = "z"
                   MOVE "Z" TO CARACTER
               END-IF
               EVALUATE TRUE
                   WHEN OBJETO-RESULTADO (OBJETO-T)
                   AND((CARACTER = "," OR ".")
                   AND (CARACTER NOT = CHAR)
                   OR((CARACTER = "9" OR "Z") AND (CHAR = "." OR ",")))
                       MOVE OBJETO-RL (OBJETO-T) TO LIN-R
                       MOVE OBJETO-RC (OBJETO-T) TO COL-R
                       MOVE OBJETO-TAMANHO (OBJETO-T) TO TAMANHO
                       IF CARACTER = "." AND (CHAR NOT = ',')
                          PERFORM VARYING COL-W FROM COL-E BY -1
                                  UNTIL COL-W < COL-R
                                     OR X'00' = CARACTER
                                    IF CX (LIN-E COL-W) = ','
                                       MOVE X'00' TO CARACTER
                                    END-IF
                          END-PERFORM
                          IF X'00' = CARACTER
                             CALL X"E5"
                             EXIT PARAGRAPH
                          END-IF
                       END-IF
                       IF CARACTER = ","
                          INSPECT MAPA-LIN (LIN-R)
                                            (COL-R: TAMANHO)
                                  CONVERTING '.,9' TO 'ZZZ'
                          MOVE CARACTER TO CX (LIN-E COL-E)
                          MOVE MAPA-LIN (LIN-R) (COL-R: TAMANHO)
                                                    TO ELEMENTO
                          MOVE 0 TO OP-DEC
                          PERFORM VARYING R FROM 1 BY 1
                                    UNTIL R > TAMANHO
                                  IF ELEMENTO (R: 1) = ','
                                     ADD 1 TO OP-DEC R
                                  ELSE
                                     IF OP-DEC > 0
                                     AND ELEMENTO(R:1) ='Z'
                                         ADD 1 TO OP-DEC
                                     END-IF
                                  END-IF
                          END-PERFORM
                          COMPUTE R = TAMANHO - 1
                          IF OP-DEC NOT = 0
                             SUBTRACT OP-DEC FROM R
                             MOVE ',' TO ELEMENTO(R:1)
                             SUBTRACT 1 FROM R
                             INSPECT ELEMENTO (R:) CONVERTING 'Z' TO '9'
                          ELSE
                             MOVE '9' TO ELEMENTO(R:1)
                          END-IF
                          IF R > 3
                             SUBTRACT 3 FROM R
                             IF R > 1
                                MOVE '.' TO ELEMENTO(R:1)
                             END-IF
                             PERFORM UNTIL R NOT > 4
                                     SUBTRACT 4 FROM R
                                     IF R > 1
                                        MOVE '.' TO ELEMENTO(R:1)
                                     END-IF
                             END-PERFORM
                          END-IF
                          MOVE ELEMENTO TO MAPA-LIN (LIN-R)
                                                    (COL-R: TAMANHO)
                       ELSE
                          MOVE CARACTER TO CX (LIN-E COL-E)
                       END-IF
                       MOVE 0 TO COL-EA
                       PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                   WHEN((CX (LIN-E COL-E) = "9" or "Z" OR "*" OR "$")
                   AND  (CARACTER = "Z" OR "$" OR "*"))
                        PERFORM UNTIL COL-W = 0
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                IF   CX (LIN-W COL-W) = "9" or "Z"
                                                     OR "*" OR "$"
                                     MOVE CARACTER TO CX (LIN-W COL-W)
                                END-IF
                                SUBTRACT 1 FROM COL-W
                        END-PERFORM
                        add 1 to col-w
                        perform until col-w > 500
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                IF CX (LIN-W COL-W) = "Z" OR "*" OR "$"
                                   MOVE CARACTER TO CX (LIN-W COL-W)
                                END-IF
                                add 1 to col-w
                        end-perform
                        IF NOT OBJETO-RESULTADO (OBJETO-T)
                           MOVE "E" TO OBJETO-TIPO (OBJETO-T)
                        END-IF
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                   WHEN CARACTER = "+" OR "-"
                        PERFORM UNTIL COL-W > 500
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                ADD 1 TO COL-W
                        END-PERFORM
                        SUBTRACT 1 FROM COL-W
                        IF   CX (LIN-W COL-W) = "+" OR "-"
                             MOVE CARACTER TO CX (LIN-W COL-W)
                        END-IF
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                   WHEN((CX (LIN-E COL-E) = "Z" OR "*" OR "$")
                   AND   CARACTER = "9")
                        PERFORM UNTIL COL-W = 500
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                IF   CX (LIN-W COL-W) = "Z" OR "*"
                                                            OR "$"
                                     MOVE CARACTER TO CX (LIN-W COL-W)
                                END-IF
                                ADD 1 TO COL-W
                        END-PERFORM
                        IF NOT OBJETO-RESULTADO (OBJETO-T)
                           MOVE "E" TO OBJETO-TIPO (OBJETO-T)
                        END-IF
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                   WHEN((CX (LIN-E COL-E) = "*" OR "$")
                   AND  (CARACTER = "*" OR "$" OR "Z"))
                   OR  ((CX (LIN-E COL-E) = "*" OR "$")
                   AND   CARACTER = "Z")
                        PERFORM UNTIL COL-W = 0
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                      SUBTRACT 1 FROM COL-W
                        END-PERFORM
                        ADD 1 TO COL-W
                        PERFORM UNTIL COL-W = 500
                                   OR CX-M (LIN-W COL-W) NOT = OBJETO-T
                                IF   CX (LIN-W COL-W) = "*" OR "$"
                                     MOVE CARACTER TO CX (LIN-W COL-W)
                                END-IF
                                ADD 1 TO COL-W
                        END-PERFORM
                        PERFORM 110-EXIBE-TELA THRU 110-99-FIM
               END-EVALUATE
           END-IF

           IF (OBJETO-T = 0
           OR  OBJETO-TIPO (OBJETO-T) = "L")
               MOVE CARACTER TO CX (LIN-E COL-E)
               IF    CWUNIX-GUI
                     SUBTRACT 1 FROM COL-S
               END-IF
               IF CARACTER = X'00'
                  CALL "CWMSGW" USING POS-S X'20'
               ELSE
                  CALL "CWMSGW" USING POS-S CARACTER
               END-IF
               IF    CWUNIX-GUI
                     ADD 1 TO COL-S
               END-IF
               SET EDIT-CURSOR-RIGHT TO TRUE
               IF  COL-E > 1
                   COMPUTE COL-W2 = COL-E - 1
                   MOVE CX-M (LIN-E COL-W2) TO OBJETO-T1
                   IF  COL-S > 4
                       COMPUTE COL-SW = COL-S - 1
                       MOVE CWMOUS-POSIT (LIN-S COL-SW) TO OBJETO-MW
                   ELSE
                       MOVE 0                           TO OBJETO-MW
                   END-IF
                   IF  OBJETO-TIPO (OBJETO-T1) = "L"
                   AND OBJETO-T = 0
                       ADD 1 TO OBJETO-TAMANHO (OBJETO-T1)
                       MOVE OBJETO-T1 TO CX-M (LIN-E COL-E)
                       MOVE OBJETO-MW TO CWMOUS-POSIT (LIN-S COL-S)
                       COMPUTE COL-W2 = COL-E + 1
                       MOVE CX-M (LIN-E COL-W2) TO OBJETO-T2
                       IF  OBJETO-T2 NOT = 0
                       AND OBJETO-TIPO (OBJETO-T2) = "L"
                           ADD OBJETO-TAMANHO(OBJETO-T2)
                            TO OBJETO-TAMANHO(OBJETO-T1)
                           MOVE COL-S TO COL-SW
                           PERFORM OBJETO-TAMANHO(OBJETO-T2) TIMES
                                  MOVE OBJETO-T1 TO CX-M (LIN-E COL-W2)
                                  ADD  1 TO COL-SW
                                  IF   COL-SW < 80
                                       MOVE OBJETO-MW
                                      TO CWMOUS-POSIT (LIN-M COL-SW)
                                  END-IF
                                  ADD 1 TO COL-W2
                           END-PERFORM
                           MOVE LOW-VALUES TO OBJETO-ATTR (OBJETO-T2)
                       END-IF
                   ELSE
                       PERFORM 160-NOVO-TEXTO THRU 160-99-FIM
                   END-IF
               ELSE
                   PERFORM 160-NOVO-TEXTO  THRU 160-99-FIM
               END-IF
           END-IF.

       150-99-FIM. EXIT.

       151-EDIT-DATE.

           IF   CAMPOS-MASCARA = SPACES
                EVALUATE CAMPOS-TAMANHO
                   WHEN 2
                        PERFORM VARYING E FROM 1 BY 1
                                UNTIL E > LENAME - 3
                           EVALUATE CAMPOS-DATANAME (E: 3)
                            WHEN "DIA" MOVE "DD" TO CAMPOS-MASCARA
                            WHEN "MES" MOVE "MM" TO CAMPOS-MASCARA
                            WHEN "ANO" MOVE "AA" TO CAMPOS-MASCARA
                           END-EVALUATE
                        END-PERFORM
                        IF   CAMPOS-MASCARA = SPACES
                             PERFORM VARYING E FROM 1 BY 1
                                     UNTIL E > LENAME - 2
                                EVALUATE CAMPOS-DATANAME (E: 2)
                                 WHEN "DD" MOVE "DD" TO CAMPOS-MASCARA
                                 WHEN "MM" MOVE "MM" TO CAMPOS-MASCARA
                                 WHEN "AA" MOVE "AA" TO CAMPOS-MASCARA
                                END-EVALUATE
                             END-PERFORM
                        END-IF
                   WHEN 4
                        MOVE "MM/AA"      TO CAMPOS-MASCARA
                   WHEN 6
                        MOVE "DD/MM/AA"   TO CAMPOS-MASCARA
                   WHEN 8
                        MOVE "DD/MM/AAAA" TO CAMPOS-MASCARA
                END-EVALUATE
           ELSE
                MOVE CAMPOS-MASCARA TO MASCARA
                MOVE SPACES           TO CAMPOS-MASCARA
                MOVE 0                TO D M A
                PERFORM VARYING E
                           FROM 1 BY 1 UNTIL E > LENGTH MASCARA
                       IF  MASCARA (E: 1) = "D"
                           ADD 1 TO D
                       END-IF
                       IF  MASCARA (E: 1) = "M"
                           ADD 1 TO M
                       END-IF
                       IF  MASCARA (E: 1) = "A"
                           ADD 1 TO A
                       END-IF
                END-PERFORM
                MOVE 0 TO E
                IF   D NOT = 0
                     PERFORM D TIMES
                             ADD  1   TO E
                             MOVE "D" TO CAMPOS-MASCARA (E: 1)
                     END-PERFORM
                     ADD  1   TO E
                     MOVE "/" TO CAMPOS-MASCARA (E: 1)
                END-IF
                IF   M NOT = 0
                     PERFORM M TIMES
                             ADD  1   TO E
                             MOVE "M" TO CAMPOS-MASCARA (E: 1)
                     END-PERFORM
                     ADD  1   TO E
                     MOVE "/" TO CAMPOS-MASCARA (E: 1)
                END-IF
                IF   A NOT = 0
                     IF   A = 2
                          MOVE 4 TO A
                     END-IF
                     PERFORM A TIMES
                             ADD  1   TO E
                             MOVE "A" TO CAMPOS-MASCARA (E: 1)
                     END-PERFORM
                END-IF
           END-IF

           IF   CAMPOS-MASCARA = SPACES
                MOVE ALL "9" TO CAMPOS-MASCARA (1: CAMPOS-TAMANHO)
           END-IF.

       151-99-FIM. EXIT.

       160-NOVO-TEXTO.

           IF   OBJETO-T = 0
                ADD  1        TO OBJETO-R
                MOVE OBJETO-R TO CX-M (LIN-E COL-E)
                MOVE LIN-E    TO OBJETO-RL       (OBJETO-R)
                MOVE COL-E    TO OBJETO-RC       (OBJETO-R)
                MOVE "L"      TO OBJETO-TIPO     (OBJETO-R)
                MOVE 1        TO OBJETO-TAMANHO  (OBJETO-R)
                MOVE "Texto"  TO OBJETO-DATANAME (OBJETO-R)
                ADD  1        TO OBJETO-M
                MOVE OBJETO-M TO CWMOUS-POSIT (LIN-S COL-S)
                MOVE LIN-S    TO OBJETO-ML (OBJETO-M)
                MOVE COL-S    TO OBJETO-MC (OBJETO-M)
                IF   LIN-E > DET
                AND (STATUS-LINHA (LIN-E) = SPACE OR "K" or "t")
                     MOVE LIN-E TO DET
                     MOVE 78    TO STRING-LENGTH
                     IF   CWUNIX-GUI
                          MOVE 0     TO COLUMN-NUMBER-D
                     ELSE
                          MOVE 1     TO COLUMN-NUMBER-D
                     END-IF
                     PERFORM VARYING LIN-W FROM TOPO BY 1
                               UNTIL LIN-W > LIN-S
                     COMPUTE ROW-NUMBER-D = LIN-W - 1
                     CALL "CBL_READ_SCR_CHATTRS" USING
                                                 SCREEN-POSITION-D
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 STRING-LENGTH
                     IF    ATTRIBUTE-BUFFER (4: 75) = ALL-X02 (1: 75)
                     OR    ATTRIBUTE-BUFFER (4: 75) = LOW-VALUES
                           INSPECT CARACTER-BUFFER
                                   CONVERTING X"00" TO " "
                           MOVE ALL-X1E   TO ATTRIBUTE-BUFFER (4: )
                           MOVE ALL-X0E   TO ATTRIBUTE-BUFFER (1: 2)
                           MOVE " D"      TO CARACTER-BUFFER  (1: 2)
                           IF   CWUNIX-OFF
                                INSPECT CARACTER-BUFFER CONVERTING
                                        X"B0" TO SPACE
                                IF   CWUNIX-GUI
                                AND  CARACTER-BUFFER (3: 1) = "*"
                                     MOVE ALL X"70"
                                       TO ATTRIBUTE-BUFFER (4: )
                                END-IF
                           END-IF
                           CALL "CBL_WRITE_SCR_CHATTRS" USING
                                                       SCREEN-POSITION-D
                                                       CARACTER-BUFFER
                                                       ATTRIBUTE-BUFFER
                                                       STRING-LENGTH
                     END-IF
                     END-PERFORM
                END-IF
           END-IF.

       160-99-FIM. EXIT.

       170-EXIBE-OBJETO.

           MOVE 0 TO TECLA
           ADD  1 TO COL-S
           IF  COL-S = 80
               MOVE 79 TO COL-S
               IF  COL-I < 426
                   ADD  1  TO COL-I
               END-IF
           END-IF
           PERFORM 110-EXIBE-TELA THRU 110-99-FIM.

       170-99-FIM. EXIT.

       180-ARRASTANDO.

           IF  CWUNIX-ON
           OR  CWUNIX-GUI
               CONTINUE
           ELSE
                CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                    MOUSE-POSITION

           END-IF
           IF  (MOUSE-POSITION NOT = MOUSE-POSITION-A
           AND  MOUSE-POSITION NOT = LOW-VALUES)
           OR   CWUNIX-GUI
                MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                IF   ROW-MOUSE < 21
                AND  ROW-MOUSE > (TOPO - 1)
                AND  COLUMN-MOUSE < 79
                AND  COLUMN-MOUSE > MARGIN
                     MOVE ROW-MOUSE      TO CURSOR-LIN
                     MOVE COLUMN-MOUSE   TO CURSOR-COL
                ELSE
                     EVALUATE TRUE
                         WHEN CWUNIX-GUI
                              CONTINUE
                         WHEN ROW-MOUSE > 20
                              SET EDIT-CURSOR-DOWN  TO TRUE
                         WHEN ROW-MOUSE < 7
                              SET EDIT-CURSOR-UP    TO TRUE
                         WHEN COLUMN-MOUSE > 78
                              SET EDIT-CURSOR-RIGHT TO TRUE
                         WHEN COLUMN-MOUSE < 4
                              SET EDIT-CURSOR-LEFT  TO TRUE
                     END-EVALUATE
                     MOVE 255 TO CWMOUS-KEY
                     PERFORM 106-PROCESSA-TECLA THRU 106-99-FIM
                     COMPUTE CURSOR-LIN = LIN-S - 1
                     COMPUTE CURSOR-COL = COL-S - 1
                     IF (NOT EDIT-ENTER)
                     AND(NOT EDIT-ESC)
                         MOVE 0   TO TECLA
                     END-IF
                     IF   EDIT-ESC
                          MOVE LIN-ESC    TO LIN-E
                          MOVE COL-ESC    TO COL-E
                          MOVE SPACE      TO FIM
                          SET  EDIT-ENTER TO TRUE
                     END-IF
                END-IF
           END-IF

           IF   NOT EDIT-ENTER
                IF   MULTI-USER = 1
                     MOVE 1 TO KEY-STATUS
                ELSE
                     CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                END-IF
                IF   KEY-STATUS = 1
                     MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                     COMPUTE LIN-S = CURSOR-LIN + 1
                     COMPUTE COL-S = CURSOR-COL + 1
                     PERFORM 105-LER-TECLA      THRU 105-99-FIM
                     MOVE 255 TO CWMOUS-KEY
                     PERFORM 106-PROCESSA-TECLA THRU 106-99-FIM
                     COMPUTE CURSOR-LIN = LIN-S - 1
                     COMPUTE CURSOR-COL = COL-S - 1
                     IF  (NOT EDIT-ENTER)
                     AND (NOT EDIT-ESC)
                          MOVE 0   TO TECLA
                     END-IF
                     IF   EDIT-ESC
                          MOVE LIN-ESC    TO LIN-E
                          MOVE COL-ESC    TO COL-E
                          MOVE SPACE      TO FIM
                          SET  EDIT-ENTER TO TRUE
                     END-IF
                ELSE
                     MOVE 0 TO MOUSE-EVENT-TYPE
                     CALL "CBL_READ_MOUSE_EVENT" USING MOUSE-HANDLE
                                                       MOUSE-DATA
                                                       MOUSE-READ-TYPE
                    IF   MOUSE-EVENT-TYPE > 1
                         SET EDIT-ENTER TO TRUE
                         IF   MOUSE-EVENT-TYPE = 4
                              MOVE LIN-ESC    TO LIN-E
                              MOVE COL-ESC    TO COL-E
                              MOVE SPACE      TO FIM
                              SET  EDIT-ENTER TO TRUE
                         END-IF
                    END-IF
                END-IF
           END-IF

           IF   CURSOR-POSITION NOT = CURSOR-POSITION-A
                MOVE CURSOR-POSITION TO CURSOR-POSITION-A
                COMPUTE LIN-S = CURSOR-LIN + 1
                COMPUTE COL-S = CURSOR-COL + 1
                PERFORM 120-POSICIONA-CURSOR THRU 120-99-FIM
                PERFORM 130-MONITOR          THRU 130-99-FIM
                COMPUTE LIN-W = CURSOR-LIN + 1
                COMPUTE COL-W = CURSOR-COL + 1
                IF  AR = 1
                    IF  CWUNIX-GUI
                        SUBTRACT 1 FROM CURSOR-SAV-COL
                    END-IF
                    CALL "CBL_WRITE_SCR_CHATTRS" USING CURSOR-SAV
                                                       CARACTER-SAV
                                                       ATTRIBUTE-SAV
                                                       STRING-SAV
                    IF  CWUNIX-GUI
                        ADD 1 to CURSOR-SAV-COL
                    END-IF
                END-IF
                MOVE TAMANHO   TO STRING-LENGTH
                IF   COL-W + TAMANHO > 79
                     COMPUTE STRING-LENGTH = 80 - COL-W
                END-IF
                MOVE ARRASTADO TO CARACTER-BUFFER
                MOVE 1         TO AR
                IF  CWUNIX-GUI
                    SUBTRACT 1 FROM CURSOR-COL
                END-IF
                CALL "CBL_READ_SCR_CHATTRS" USING CURSOR-POSITION
                                                  CARACTER-SAV
                                                  ATTRIBUTE-SAV
                                                  STRING-LENGTH
                IF  CWUNIX-GUI
                    ADD 1 to CURSOR-COL
                END-IF
                MOVE STRING-LENGTH   TO STRING-SAV
                MOVE CURSOR-POSITION TO CURSOR-SAV
                MOVE ARRASTADO       TO CARACTER-ARR
                IF  CWUNIX-GUI
                    SUBTRACT 1 FROM CURSOR-COL
                END-IF
                CALL "CBL_WRITE_SCR_CHATTRS" USING CURSOR-POSITION
                                                   CARACTER-ARR
                                                   ATTRIBUTE-ARR
                                                   STRING-LENGTH
                IF  CWUNIX-GUI
                    ADD 1 to CURSOR-COL
                END-IF
           END-IF

           IF   EDIT-ENTER
                IF   (LIN-ESC NOT = LIN-E)
                OR   (COL-ESC NOT = COL-E)
                     MOVE    TAMANHO               TO TEST-TAMANHO
                     PERFORM 185-VALIDAR-ENTRADA THRU 185-99-FIM
                END-IF
                IF   EDIT-ENTER
                     MOVE LIN-E     TO OBJETO-RL      (OBJETO-A)
                     MOVE COL-E     TO OBJETO-RC      (OBJETO-A)
                     MOVE TAMANHO   TO OBJETO-TAMANHO (OBJETO-A)
                     MOVE ARRASTADO TO MAPA-LIN (LIN-E) (COL-E: TAMANHO)
                     MOVE TIPO      TO OBJETO-TIPO     (OBJETO-A)
                     MOVE DATANAME  TO OBJETO-DATANAME (OBJETO-A)
                     MOVE DATANAME2 TO OBJETO-DATANAME2(OBJETO-A)
                     MOVE LIN-E     TO LIN-W2
                     MOVE COL-E     TO COL-W2
                     PERFORM TAMANHO TIMES
                             MOVE OBJETO-A TO CX-M (LIN-W2 COL-W2)
                             ADD  1        TO COL-W2
                     END-PERFORM
                     IF   LIN-E > DET
                     AND (STATUS-LINHA (LIN-E) = SPACE OR "K" or "t")
                          MOVE LIN-E TO DET
                     END-IF
ROFER                MOVE 0 TO COL-EA
                     PERFORM 110-EXIBE-TELA THRU 110-99-FIM
                ELSE
                     CALL X"E5"
                END-IF
           END-IF.

       180-99-FIM. EXIT.

       185-VALIDAR-ENTRADA.

           SET EDIT-ENTER TO TRUE

           IF   LIN-E > TOT2
           AND  TOT > 0
                MOVE 0 TO TECLA
                CALL X"E5"
           END-IF

           IF   EDIT-ENTER
                MOVE LIN-E TO LIN-W2
                MOVE COL-E TO COL-W2
                MOVE SPACE TO FLAG-ACM
                IF  (DATANAME NOT = "Texto")
                AND (DATANAME (1: 7) NOT = "Prompt-")
                AND (DATANAME (1: 8) NOT = "Relator-")
      *         AND (DATANAME (1: 3) NOT = "(=)")
                    PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > LENAME - 3
                                 OR  DATANAME (I: 3) = "[+]"
                            CONTINUE
                    END-PERFORM
                    IF   DATANAME (I: 3) = "[+]"
                         MOVE "+" TO FLAG-ACM
                    END-IF
                    IF   STATUS-LINHA (LIN-E) = "T"
                    AND (TIPO     NOT = "A")
                    AND (TECLA    NOT = 0)
                    AND (FLAG-ACM NOT = "+")
                        MOVE 0   TO TECLA
                    END-IF
                    IF   STATUS-LINHA (LIN-E) = "Q"
                    AND  LIN-E > DET
                    AND (TIPO     NOT = "A")
                    AND (TECLA    NOT = 0)
                    AND (FLAG-ACM NOT = "+")
                         PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > COLUNAS-ORDENADAS
                                      OR COLUNA-ORDENADA (I)
                                       = DATANAME
                                 CONTINUE
                         END-PERFORM
                         IF   I > NIVEL-QUEBRA (LIN-E)
                              MOVE 0   TO TECLA
                         END-IF
                    END-IF
                    MOVE DATANAME TO CAMPOS-DATANAME
                    IF  (LIN-E NOT > CAB)
                    AND (TIPO     NOT = "A")
                    AND (NOT CAMPOS-CAB)
                    AND (TECLA    NOT = 0)
                         PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > COLUNAS-ORDENADAS
                                      OR COLUNA-ORDENADA (I)
                                       = DATANAME
                                 CONTINUE
                         END-PERFORM
                         IF   I > COLUNAS-ORDENADAS
                              MOVE 0   TO TECLA
                         END-IF
                    END-IF
                END-IF
                IF   MAPA-LIN (LIN-E) (COL-E: TEST-TAMANHO) NOT = SPACES
                     MOVE 0   TO TECLA
                END-IF
           END-IF

           IF   TECLA = 0
                MOVE "N" TO MOVER-M
           END-IF.

       185-99-FIM. EXIT.

       190-FILTROS.

           CALL "CWMSGW" USING "250179" ESPACOS
           MOVE "_Filtros:"  TO CWBOXS-TITLE
           MOVE 1            TO CWBOXS-OPTION
           MOVE SPACES       TO CWBOXS-ITENS
           MOVE "~Condi‡äes" TO CWBOXS-TEXT   (1)
           MOVE "~Prompts"   TO CWBOXS-TEXT   (2)
           CALL "CWBOXS" USING PARAMETROS-CWBOXS
           CALL "CWKBDC" USING "9595"
           EVALUATE CWBOXS-OPTION-CHAR
               WHEN "C" PERFORM 191-CONDICOES THRU 191-99-FIM
               WHEN "P" PERFORM 192-PROMPTS   THRU 192-99-FIM
           END-EVALUATE
           CALL "CWKBDC" USING "9999".

       190-99-FIM. EXIT.

       191-CONDICOES.

           MOVE CWBOXF-COLOR-FRAME   TO CWBOXW-COLOR-FRAME
           MOVE CWBOXF-COLOR-BORDER  TO CWBOXW-COLOR-BORDER
           MOVE 30                   TO CWBOXS-COLOR-FRAME
           MOVE 30                   TO CWBOXS-COLOR-BORDER
           IF  CWUNIX-GUI
               MOVE 04               TO CWBOXW-LINE
               MOVE 06               TO L L9
               MOVE 19               TO L22
           ELSE
               MOVE 07               TO CWBOXW-LINE
               MOVE 09               TO L L9
               MOVE 22               TO L22
           END-IF
           MOVE 03                   TO CWBOXW-COLUMN
           MOVE 15                   TO CWBOXW-VERTICAL-LENGTH
           MOVE 73                   TO CWBOXW-HORIZONTAL-LENGTH
           SET  CWBOXW-OPEN          TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW
           MOVE SPACES TO POP
           STRING " Condi‡äes de sele‡Æo, "
                            DELIMITED BY SIZE
                  PADRAO-L  DELIMITED BY SPACE
                  " "       DELIMITED BY SIZE
                  RELATORIO DELIMITED BY SPACE
                  ": "      DELIMITED BY SIZE
                       INTO POP
           PERFORM VARYING I FROM 80 BY -1
                     UNTIL I = 1
                        OR (POP (I: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           CALL "CWMSGW" USING TOPO2    POP I
           DISPLAY "Sele‡Æo"    LINE L8 COLUMN 05 WITH HIGHLIGHT
                                               BACKGROUND-COLOR 6
                                               FOREGROUND-COLOR 7
           DISPLAY "Compara‡Æo" LINE L8 COLUMN 45 WITH HIGHLIGHT
                                               BACKGROUND-COLOR 6
                                               FOREGROUND-COLOR 7
           DISPLAY "Coluna"     LINE L8 COLUMN 14 WITH HIGHLIGHT
                                            BACKGROUND-COLOR 6
                                            FOREGROUND-COLOR 7
           MOVE    1                   TO I K CAMPO
           MOVE    200                 TO KA
           PERFORM 197-EXIBE-CONDICOES THRU 197-99-FIM

           CALL "CBL_GET_CSR_POS"  USING SAVE-CURSOR2
           PERFORM TEST AFTER UNTIL EDIT-ESC
             COMPUTE ROW-NUMBER = L - 1
             MOVE 4 TO COLUMN-NUMBER
             CALL "CBL_SET_CSR_POS"  USING SCREEN-POSITION
             PERFORM 105-LER-TECLA       THRU 105-99-FIM
             EVALUATE TRUE
                 WHEN EDIT-CURSOR-UP
                      IF  I > 1
                          SUBTRACT 1 FROM I
                          IF   L > L9
                               SUBTRACT 1 FROM L
                          END-IF
                      END-IF
                 WHEN EDIT-CURSOR-DOWN AND I < 99
                      IF    CONDICAO-DATANAME (I) NOT = SPACE
                            ADD  1 TO I
                            IF   L < L22
                                 ADD 1 TO L
                            END-IF
                      END-IF
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (CONDICAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-CONTROL-PAGE-DOWN
                      PERFORM VARYING I FROM 99 BY -1
                              UNTIL I = 1
                                 OR (CONDICAO-ATRIBUTOS (I)
                                    NOT = SPACES)
                              CONTINUE
                      END-PERFORM
                      MOVE L9 TO L
                      IF   I > 14
                           MOVE L22 TO L
                           PERFORM UNTIL L = L9
                                   SUBTRACT 1 FROM I L
                           END-PERFORM
                      END-IF
                 WHEN EDIT-CONTROL-PAGE-UP
                      MOVE    1                   TO I
                      MOVE    L9                  TO L
                      PERFORM 197-EXIBE-CONDICOES THRU 197-99-FIM
                 WHEN EDIT-PAGE-DOWN
                      PERFORM 14 TIMES
                              COMPUTE K = I + 1
                              IF   K < 100
                              AND (CONDICAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  ADD  1 TO I
                                  IF   L < L22
                                       ADD 1 TO L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (CONDICAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
             END-EVALUATE
             PERFORM 197-EXIBE-CONDICOES THRU 197-99-FIM
             IF   EDIT-ENTER
                  MOVE 1 TO CAMPO
             PERFORM UNTIL CAMPO > 4 OR I > 99
                 MOVE 0 TO ERRO
                 EVALUATE CAMPO
                     WHEN 1
                          MOVE SPACES TO CWBOXS-ITENS
                          MOVE "_Sele‡Æo:_" TO CWBOXS-TITLE
                          IF   (CONDICAO-DATANAME (I) NOT = SPACES)
                                MOVE "~Apagar" TO CWBOXS-TEXT   (1)
                          END-IF
                          MOVE "~ExclusÆo" TO CWBOXS-TEXT   (2)
                          MOVE "~InclusÆo" TO CWBOXS-TEXT   (3)
                          EVALUATE CONDICAO-TIPO (I)
                              WHEN "I"   MOVE 3 TO CWBOXS-OPTION
                              WHEN OTHER MOVE 2 TO CWBOXS-OPTION
                          END-EVALUATE
                          COMPUTE CWBOXS-LINE = L - 1
                          MOVE 03 TO CWBOXS-COLUMN
                          CALL "CWBOXS" USING PARAMETROS-CWBOXS
                          IF   CWBOXS-OPTION-CHAR NOT = SPACE
                               MOVE CWBOXS-OPTION-CHAR
                                 TO CONDICAO-TIPO (I)
                          ELSE
                               MOVE 5 TO CAMPO
                          END-IF
                          IF   CWBOXS-OPTION-CHAR = "A"
                               IF   (CONDICAO-DATANAME (I) NOT = SPACES)
                                    PERFORM 196-APAGA-CONDICAO
                                       THRU 196-99-FIM
                               END-IF
                               MOVE 5 TO CAMPO
                          ELSE
                               EVALUATE CONDICAO-TIPO (I)
                                   WHEN "I" MOVE "InclusÆo" TO TIPO-ED
                                   WHEN "E" MOVE "ExclusÆo" TO TIPO-ED
                                   WHEN OTHER
                                            MOVE SPACES     TO TIPO-ED
                               END-EVALUATE
                               CALL "CWTEXT" USING TIPO-ED
                                         LENGTH OF TIPO-ED
                               MOVE L TO LW MOVE 5 TO CW MOVE 08 TO SW
                               CALL "CWMSGW" USING W TIPO-ED
                               SET ENTER-KEY TO TRUE
                          END-IF
                     WHEN 2
                          MOVE 11 TO CWBOXF-COLUMN
                          MOVE CONDICAO-DATANAME (I) TO CWBOXF-OPTION
                          PERFORM 193-COLUNA-PROMPT-CONDICAO
                             THRU 193-99-FIM
                          IF   CWBOXF-OPTION NOT = SPACES
                               MOVE CWBOXF-OPTION
                                 TO CONDICAO-DATANAME (I)
                          END-IF
                          MOVE L TO LW MOVE 14 TO CW MOVE 30 TO SW
                          CALL "CWMSGW" USING W CONDICAO-DATANAME (I)
                          IF   CONDICAO-DATANAME (I) NOT = SPACES
                               SET ENTER-KEY TO TRUE
                          ELSE
                               SET CURSOR-UP TO TRUE
                          END-IF
                     WHEN 3
                          MOVE "123456"            TO CWBOXS-ITENS
                          MOVE "_Operando:_"       TO CWBOXS-TITLE
                          MOVE "=  Igual         " TO CWBOXS-TEXT   (1)
                          MOVE ">  Maior         " TO CWBOXS-TEXT   (2)
                          MOVE ">= Maior ou igual" TO CWBOXS-TEXT   (3)
                          MOVE "<  Menor         " TO CWBOXS-TEXT   (4)
                          MOVE "<= Menor ou igual" TO CWBOXS-TEXT   (5)
                          MOVE "<> Diferente     " TO CWBOXS-TEXT   (6)
                          EVALUATE CONDICAO-CONDICAO (I)
                               WHEN "="    MOVE 1 TO CWBOXS-OPTION
                               WHEN ">"    MOVE 2 TO CWBOXS-OPTION
                               WHEN ">="   MOVE 3 TO CWBOXS-OPTION
                               WHEN "<"    MOVE 4 TO CWBOXS-OPTION
                               WHEN "<="   MOVE 5 TO CWBOXS-OPTION
                               WHEN "<>"   MOVE 6 TO CWBOXS-OPTION
                               WHEN OTHER  MOVE 1 TO CWBOXS-OPTION
                          END-EVALUATE
                          COMPUTE CWBOXS-LINE = L - 1
                          MOVE 42 TO CWBOXS-COLUMN
                          PERFORM TEST AFTER
                                  UNTIL CONDICAO-CONDICAO (I) NOT = "  "
                                  CALL "CWBOXS" USING PARAMETROS-CWBOXS
                                  IF   CWBOXS-OPTION-CHAR NOT = " "
                                       MOVE CWBOXS-TEXT
                                            (CWBOXS-OPTION)
                                         TO CONDICAO-CONDICAO (I)
                                  END-IF
                          END-PERFORM
                          MOVE L TO LW MOVE 44 TO CW MOVE 2 TO SW
                          CALL "CWMSGW" USING W CONDICAO-CONDICAO (I)
                     WHEN 4
                          IF  CONDICAO-LITERAL (I) = "L"
                              MOVE CONDICAO-DATANAME (I)
                                TO CAMPOS-DATANAME
                              MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                              MOVE SPACES   TO CAMPOS-RELATORIO
                              IF   CAMPOS-DATANAME (1: 7) = "Prompt-"
                              OR   CAMPOS-DATANAME (1: 3) = "(=)"
                                   MOVE RELATORIO TO CAMPOS-RELATORIO
                              END-IF
                              READ CAMPOS KEY IS CAMPOS-PRONAMREL
                              IF   FS-CAMPOS = "23"
                                   MOVE RELATORIO TO CAMPOS-RELATORIO
                                   READ CAMPOS
                                        KEY IS CAMPOS-PRONAMREL
                              END-IF
                              MOVE CAMPOS-TAMANHO TO TAMANHO
                              IF   TAMANHO > 28
                                   MOVE 28 TO TAMANHO
                              END-IF
                              MOVE TAMANHO TO CONDICAO-TAMANHO (I)
                              MOVE L TO LW MOVE 46 TO CW MOVE 30 TO SW
                              CALL "CWMSGW" USING W ESPACOS
                              EVALUATE TRUE
                              WHEN CAMPOS-NUMERICO
                                OR CAMPOS-CGC-CIC
                                OR CAMPOS-VALOR
                                   CALL "CWREL7" USING "A"
                                            CONDICAO-NUMERO (I)
                                            CAMPOS-TIPO
                                            CAMPOS-TAMANHO
                                            CAMPOS-DEC
                                            L CR7 TECLA2
                                   CALL "CWREL7" USING "D"
                                             CONDICAO-NUMERO (I)
                                             CAMPOS-TIPO
                                             CAMPOS-TAMANHO
                                             CAMPOS-DEC
                                             L CR7 TECLA2
                                   INSPECT CONDICAO-CONDICAO (I)
                                           CONVERTING "0123456789"
                                                   TO SPACES
                              WHEN CAMPOS-HORA
                                   MOVE CONDICAO-OPERANDO (I) (1: 6)
                                     TO HORA                  (1: 6)
                                   DISPLAY TELA-HORA
                                   PERFORM TEST AFTER
                                          UNTIL HH < 24
                                            AND MM < 60
                                            AND SS < 60
                                           ACCEPT  TELA-HORA
                                   END-PERFORM
                                   MOVE HORA                  (1: 6)
                                     TO CONDICAO-OPERANDO (I) (1: 6)
                              WHEN CAMPOS-DATA
                                 PERFORM 151-EDIT-DATE THRU 151-99-FIM
                                 MOVE SPACES TO MASCARA
                                 STRING "{" DELIMITED BY SIZE
                                        CAMPOS-MASCARA
                                            DELIMITED BY SPACE
                                        "}" DELIMITED BY SIZE
                                        INTO MASCARA
                                MOVE L TO LW MOVE 59 TO CW MOVE 12 TO SW
                                 CALL "CWMSGW" USING W MASCARA
                                 EVALUATE TRUE
                                     WHEN CAMPOS-MASCARA = "DD/MM"
                                                          OR "MM/AA"
                                          MOVE CONDICAO-OPERANDO (I)
                                               (1: 4) TO DT4 (1: 4)
                                          ACCEPT DT-4
                                          MOVE DT4 (1: 4)
                                            TO CONDICAO-OPERANDO (I)
                                               (1: 4)
                                     WHEN CAMPOS-MASCARA = "DD/MM/AA"
                                          MOVE CONDICAO-OPERANDO (I)
                                               (1: 6) TO DT6 (1: 6)
                                          ACCEPT DT-6
                                          MOVE DT6 (1: 6)
                                            TO CONDICAO-OPERANDO (I)
                                               (1: 6)
                                     WHEN CAMPOS-MASCARA = "MM/AAAA"
                                          MOVE CONDICAO-OPERANDO (I)
                                               (1: 6) TO DT6 (1: 6)
                                          ACCEPT DT-6X
                                          MOVE DT6 (1: 6)
                                            TO CONDICAO-OPERANDO (I)
                                               (1: 6)
                                    WHEN CAMPOS-MASCARA = "DD/MM/AAAA"
                                          MOVE CONDICAO-OPERANDO (I)
                                               (1: 8) TO DT8 (1: 8)
                                          ACCEPT DT-8
                                          MOVE DT8 (1: 8)
                                            TO CONDICAO-OPERANDO (I)
                                               (1: 8)
                                 END-EVALUATE
                              WHEN OTHER
                              MOVE SPACES TO POP
                              STRING '"'
                                     CONDICAO-OPERANDO (I) (1: TAMANHO)
                                     '"' DELIMITED BY SIZE
                                INTO POP
                              MOVE L TO LW MOVE 46 TO CW
                              COMPUTE SW = TAMANHO + 2
                              CALL "CWMSGW" USING W POP
                              ACCEPT CONDICAO-OPERANDO (I)
                                     LINE L COLUMN 47 WITH SIZE TAMANHO
                                                           PROMPT UPDATE
                                     FOREGROUND-COLOR 6
                                     BACKGROUND-COLOR 1 HIGHLIGHT
                              ACCEPT TECLA FROM ESCAPE KEY
                              END-EVALUATE
                          END-IF
                          IF ( CONDICAO-OPERANDO (I) = SPACES
                          OR  (CONDICAO-LITERAL (I) NOT = "L"))
                          AND (NOT CURSOR-UP)
                          AND (NOT ESC)
                               MOVE 47 TO CWBOXF-COLUMN
                               MOVE CONDICAO-OPERANDO (I)
                                 TO CWBOXF-OPTION
                               PERFORM 193-COLUNA-PROMPT-CONDICAO
                                  THRU 193-99-FIM
                               IF   CWBOXF-OPTION NOT = SPACES
                                    MOVE CWBOXF-OPTION
                                      TO CONDICAO-OPERANDO (I)
                               END-IF
                               MOVE L TO LW MOVE 46 TO CW MOVE 30 TO SW
                               CALL "CWMSGW" USING W
                                                   CONDICAO-OPERANDO (I)
                               IF   CONDICAO-OPERANDO (I) = SPACES
                                    MOVE 1 TO ERRO
                                    MOVE "L" TO CONDICAO-LITERAL (I)
                               ELSE
                                    MOVE SPACES TO CONDICAO-LITERAL (I)
                               END-IF
                               IF   CONDICAO-OPERANDO (I)
                                  = CONDICAO-DATANAME (I)
                               OR ( CONDICAO-OPERANDO (I) (1: 7)
                                = "Prompt-"
                               AND  CONDICAO-DATANAME (I) (1: 7)
                                = "Prompt-")
                                  MOVE SPACES TO CONDICAO-OPERANDO (I)
                                  MOVE 1      TO ERRO
                               END-IF
                          END-IF
                  END-EVALUATE
                  EVALUATE TRUE
                      WHEN ESC
                       AND (CONDICAO-ATRIBUTOS (I) = SPACES
                        OR  ERRO = 0)
                            MOVE 5 TO CAMPO
                      WHEN (ENTER-KEY OR CURSOR-DOWN)
                       AND ERRO = 0
                           ADD 1 TO CAMPO
                      WHEN CURSOR-UP
                           IF   CAMPO > 1
                                SUBTRACT 1 FROM CAMPO
                           END-IF
                  END-EVALUATE
             END-PERFORM
             END-IF
           END-PERFORM
           SET  CWBOXW-CLOSE         TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW

           MOVE CWBOXW-COLOR-FRAME   TO CWBOXS-COLOR-FRAME
           MOVE CWBOXW-COLOR-BORDER  TO CWBOXS-COLOR-BORDER
           MOVE 0                    TO TECLA
           CALL "CBL_SET_CSR_POS"  USING SAVE-CURSOR2
           MOVE "N" TO FIM.

       191-99-FIM. EXIT.

       192-PROMPTS.

           MOVE CWBOXF-COLOR-FRAME   TO CWBOXW-COLOR-FRAME
           MOVE CWBOXF-COLOR-BORDER  TO CWBOXW-COLOR-BORDER
           IF  CWUNIX-GUI
               MOVE 04               TO CWBOXW-LINE
               MOVE 06               TO L L9
               MOVE 19               TO L22
           ELSE
               MOVE 07               TO CWBOXW-LINE
               MOVE 09               TO L L9
               MOVE 22               TO L22
           END-IF
           MOVE 03                   TO CWBOXW-COLUMN
           MOVE 15                   TO CWBOXW-VERTICAL-LENGTH
           MOVE 73                   TO CWBOXW-HORIZONTAL-LENGTH
           SET  CWBOXW-OPEN          TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW
           MOVE SPACES               TO POP
           STRING " Prompts de sele‡Æo, "
                            DELIMITED BY SIZE
                  PADRAO-L  DELIMITED BY SPACE
                  " "       DELIMITED BY SIZE
                  RELATORIO DELIMITED BY SPACE
                  ": "      DELIMITED BY SIZE
                       INTO POP
           PERFORM VARYING I FROM 80 BY -1
                     UNTIL I = 1
                        OR (POP (I: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           CALL "CWMSGW" USING TOPO2    POP I
           DISPLAY "Coluna de compara‡Æo"
                              LINE L8 COLUMN 46 WITH HIGHLIGHT
                                                     BACKGROUND-COLOR 6
                                                     FOREGROUND-COLOR 7
           DISPLAY "Nome"     LINE L8 COLUMN 05 WITH HIGHLIGHT
                                      BACKGROUND-COLOR 6
                                      FOREGROUND-COLOR 7
           DISPLAY "Mensagem" LINE L8 COLUMN 15 WITH HIGHLIGHT
                                      BACKGROUND-COLOR 6
                                      FOREGROUND-COLOR 7

           MOVE    1                   TO I
           PERFORM 195-EXIBE-PROMPTS THRU 195-99-FIM
           MOVE    1                   TO I K CAMPO

           CALL "CBL_GET_CSR_POS"  USING SAVE-CURSOR2
           PERFORM TEST AFTER UNTIL EDIT-ESC
             COMPUTE ROW-NUMBER = L - 1
             MOVE 4 TO COLUMN-NUMBER
             CALL "CBL_SET_CSR_POS"  USING SCREEN-POSITION
             PERFORM 105-LER-TECLA       THRU 105-99-FIM
             EVALUATE TRUE
                 WHEN EDIT-CURSOR-UP
                      IF  I > 1
                          SUBTRACT 1 FROM I
                          IF   L > L9
                               SUBTRACT 1 FROM L
                          END-IF
                      END-IF
                 WHEN EDIT-CURSOR-DOWN AND I < 99
                      IF    PROMPT-DATANAME (I) NOT = SPACE
                            ADD  1 TO I
                            IF   L < L22
                                 ADD 1 TO L
                            END-IF
                      END-IF
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (PROMPT-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-CONTROL-PAGE-DOWN
                      PERFORM VARYING I FROM 99 BY -1
                              UNTIL I = 1
                                 OR (PROMPT-ATRIBUTOS (I)
                                    NOT = SPACES)
                              CONTINUE
                      END-PERFORM
                      MOVE L9 TO L
                      IF   I > 14
                           MOVE L22 TO L
                           PERFORM UNTIL L = L9
                                   SUBTRACT 1 FROM I L
                           END-PERFORM
                      END-IF
                 WHEN EDIT-CONTROL-PAGE-UP
                      MOVE    1                   TO I
                      MOVE    L9                  TO L
                      PERFORM 195-EXIBE-PROMPTS THRU 195-99-FIM
                 WHEN EDIT-PAGE-DOWN
                      PERFORM 14 TIMES
                              COMPUTE K = I + 1
                              IF   K < 100
                              AND (PROMPT-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  ADD  1 TO I
                                  IF   L < L22
                                       ADD 1 TO L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (PROMPT-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
             END-EVALUATE
             PERFORM 195-EXIBE-PROMPTS THRU 195-99-FIM
             IF   EDIT-DEL
             AND (PROMPT-DATANAME (I) NOT = SPACES)
                  MOVE SPACES TO CWSEND-MSG
                  STRING "Apagar Prompt-" DELIMITED BY SIZE
                          PROMPT-DATANAME (I) DELIMITED BY SPACE
                          " ? "               DELIMITED BY SIZE
                      INTO CWSEND-MSG
                  MOVE SPACES          TO CWSEND-SCREENS
                  MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
                  MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)
                  move 2             TO CWSEND-OPTION
                  CALL "CWSEND" USING PARAMETROS-CWSEND
                  IF   CWSEND-OPTION = 1
                       move spaces to PROMPT-DATANAME (I)
                       PERFORM 194-APAGA-PROMPT THRU 194-99-FIM
                  END-IF
             END-IF
             IF   EDIT-ENTER
                  MOVE 1 TO CAMPO
               PERFORM UNTIL CAMPO > 3 OR I > 99
                   MOVE 0 TO ERRO
                   EVALUATE CAMPO
                       WHEN 1
                            MOVE   "Prompt-"           TO NAME-OLD
                            MOVE   PROMPT-DATANAME (I) TO NAME-OLD (8:)
                            ACCEPT PROMPT-DATANAME (I) LINE L COLUMN 05
                                   WITH PROMPT UPDATE
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            ACCEPT TECLA FROM ESCAPE KEY
                            INSPECT PROMPT-DATANAME (I)
                                    CONVERTING MINUSCULAS TO MAIUSCULAS
                            INSPECT PROMPT-DATANAME (I)
                                    CONVERTING ACENTOS TO SEM-ACENTOS
                            MOVE L TO LW MOVE 05 TO CW MOVE 09 TO SW
                            CALL "CWMSGW" USING W PROMPT-DATANAME (I)
                            IF   PROMPT-DATANAME (I) = SPACES
                                 IF   (PROMPT-MENSAGEM (I) NOT = SPACES)
                                      PERFORM 194-APAGA-PROMPT
                                         THRU 194-99-FIM
                                      SET ENTER-KEY TO TRUE
                                      MOVE 3 TO CAMPO
                                 ELSE
                                      MOVE 1 TO ERRO
                                 END-IF
                            END-IF
                       WHEN 2
                            ACCEPT PROMPT-MENSAGEM (I) LINE L COLUMN 15
                                   WITH PROMPT UPDATE
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            IF   PROMPT-MENSAGEM (I) = SPACES
                                 MOVE 1 TO ERRO
                            END-IF
                       WHEN 3
                            MOVE 44 TO CWBOXF-COLUMN
                            MOVE PROMPT-COLUNA (I) TO CWBOXF-OPTION
                            PERFORM 193-COLUNA-PROMPT-CONDICAO
                               THRU 193-99-FIM
                            IF   CWBOXF-OPTION NOT = SPACES
                                 MOVE CWBOXF-OPTION TO PROMPT-COLUNA (I)
                                 SET ENTER-KEY TO TRUE
                            ELSE
                                 SET ESC TO TRUE
                            END-IF
                            MOVE L TO LW MOVE 46 TO CW MOVE 30 TO SW
                            CALL "CWMSGW" USING W PROMPT-COLUNA (I)
                            IF   PROMPT-COLUNA (I) = SPACES
                                 MOVE 1 TO ERRO
                            END-IF
                    END-EVALUATE
                    IF   ERRO = 0
                    AND  CAMPO = 1
                    AND (PROMPT-DATANAME (I) NOT = SPACES)
                         PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 99
                            IF  (Y NOT = I)
                            AND (PROMPT-DATANAME (Y) =
                                 PROMPT-DATANAME (I))
                                 MOVE 1   TO ERRO
                                 MOVE "?" TO PROMPT-DATANAME (I)
                            END-IF
                         END-PERFORM
                    END-IF
                    EVALUATE TRUE
                        WHEN ESC
                         AND (PROMPT-ATRIBUTOS (I) = SPACES
                          OR  ERRO = 0)
                              MOVE 4 TO CAMPO
                        WHEN (ENTER-KEY OR CURSOR-DOWN)
                         AND ERRO = 0
                             ADD 1 TO CAMPO
                        WHEN CURSOR-UP
                             IF   CAMPO > 1
                                  SUBTRACT 1 FROM CAMPO
                             END-IF
                    END-EVALUATE
             END-PERFORM
             END-IF
           END-PERFORM

           PERFORM 850-ATUALIZA-PROMPTS THRU 850-99-FIM
           SET  CWBOXW-CLOSE         TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW
           MOVE 0                    TO TECLA
           MOVE SPACES               TO FIM.

       192-99-FIM. EXIT.

       193-COLUNA-PROMPT-CONDICAO.

           MOVE 30                   TO CWBOXF-COLOR-FRAME
           MOVE 30                   TO CWBOXF-COLOR-BORDER
           COMPUTE                      CWBOXF-LINE = L - 1
           MOVE 07                   TO CWBOXF-VERTICAL-LENGTH
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = MAIOR-NOME + 2
           SET  CWBOXF-SHOW          TO TRUE
           MOVE 2                    TO CWBOXF-RETURN CWBOXF-ORDER
           PERFORM UNTIL CWBOXF-LINE + CWBOXF-VERTICAL-LENGTH < 23
                  SUBTRACT 1 FROM CWBOXF-LINE
           END-PERFORM
           PERFORM UNTIL CWBOXF-COLUMN + CWBOXF-HORIZONTAL-LENGTH < 78
                  SUBTRACT 1 FROM CWBOXF-COLUMN
           END-PERFORM
           IF   WORK-OPERACOES = 1
                MOVE "V"                   TO CWBOXF-WORK-AREA (1: 1)
                PERFORM 870-ATUALIZA-OPERACOES THRU 870-99-FIM
           ELSE
                MOVE "I"             TO CWBOXF-WORK-AREA (1: 1)
           END-IF
           MOVE "CWREL4"             TO CWBOXF-PROGRAM
           PERFORM 143-HELP THRU 143-99-FIM
           MOVE CWBOXW-COLOR-FRAME   TO CWBOXF-COLOR-FRAME
           MOVE CWBOXW-COLOR-BORDER  TO CWBOXF-COLOR-BORDER.

       193-99-FIM. EXIT.

       194-APAGA-PROMPT.

           PERFORM VARYING OBJETO-Y
                     FROM 1 BY 1
                     UNTIL OBJETO-Y > OBJETO-R
                   IF  NAME-OLD = OBJETO-DATANAME(OBJETO-Y)
                       MOVE LOW-VALUES TO OBJETO-ATTR (OBJETO-Y)
                   END-IF
           END-PERFORM
           PERFORM VARYING K FROM I BY 1 UNTIL K > 98
                   COMPUTE Y = K + 1
                   MOVE PROMPT-ATRIBUTOS (Y) TO PROMPT-ATRIBUTOS (K)
                   IF   Y = 99
                        MOVE SPACES TO PROMPT-ATRIBUTOS (Y)
                   END-IF
           END-PERFORM
           PERFORM 195-EXIBE-PROMPTS THRU 195-99-FIM.

       194-99-FIM. EXIT.

       195-EXIBE-PROMPTS.

           COMPUTE K = I - (L - L9)
           MOVE L9 TO LL
           PERFORM 14 TIMES
                   MOVE SPACES TO LL-CARACTERS
                   IF   K = I
                        MOVE ALL-X1E TO LL-ATTR
                   ELSE
                        MOVE ALL-X6E TO LL-ATTR
                   END-IF
                   IF   K < 100
                        STRING  " "                  DELIMITED BY SIZE
                                PROMPT-DATANAME (K)  DELIMITED BY SIZE
                                " "                  DELIMITED BY SIZE
                                PROMPT-MENSAGEM (K)  DELIMITED BY SIZE
                                " "                  DELIMITED BY SIZE
                                PROMPT-COLUNA   (K)  DELIMITED BY SIZE
                         INTO LL-CARACTERS
                   END-IF
                   COMPUTE ROW-NUMBER = LL - 1
                   MOVE 3 TO COLUMN-NUMBER
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                      LL-CARACTERS
                                                      LL-ATTR
                                                      X"0049"
                   ADD 1 TO LL K
           END-PERFORM.

       195-99-FIM. EXIT.

       196-APAGA-CONDICAO.

           MOVE "Apagar condi‡Æo" TO CWSEND-MSG
           MOVE SPACES          TO CWSEND-SCREENS
           MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
           MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)
           move 2             TO CWSEND-OPTION
           CALL "CWSEND" USING PARAMETROS-CWSEND
           IF   CWSEND-OPTION NOT = 1
                GO TO 196-99-FIM
           END-IF

           PERFORM VARYING K FROM I BY 1 UNTIL K > 98
                   COMPUTE Y = K + 1
                   MOVE CONDICAO-ATRIBUTOS (Y) TO CONDICAO-ATRIBUTOS (K)
                   IF   Y = 99
                        MOVE SPACES TO CONDICAO-ATRIBUTOS (Y)
                   END-IF
           END-PERFORM
           MOVE 200 TO KA
           PERFORM 197-EXIBE-CONDICOES THRU 197-99-FIM.

       196-99-FIM. EXIT.

       197-EXIBE-CONDICOES.

           COMPUTE K = I - (L - L9)
           IF  K = KA
               MOVE 1 TO KB
           ELSE
               MOVE 0 TO KB
           END-IF
           MOVE K TO KA
           MOVE L9 TO LL
           PERFORM 14 TIMES
                   IF   KB = 0
                   IF   K < 100
                   AND (CONDICAO-DATANAME (K) NOT = SPACES)
                        EVALUATE CONDICAO-TIPO (K)
                            WHEN "I"   MOVE "InclusÆo" TO TIPO-ED
                            WHEN "E"   MOVE "ExclusÆo" TO TIPO-ED
                            WHEN OTHER MOVE SPACES     TO TIPO-ED
                        END-EVALUATE
                        CALL "CWTEXT" USING TIPO-ED
                                  LENGTH OF TIPO-ED
                        MOVE TIPO-ED               TO LL-CARACTERS (02:)
                        MOVE CONDICAO-DATANAME (K) TO LL-CARACTERS (11:)
                        MOVE CONDICAO-CONDICAO (K) TO LL-CARACTERS (41:)
                        IF  CONDICAO-LITERAL (K) = "L"
                            MOVE CONDICAO-DATANAME (K)
                              TO CAMPOS-DATANAME
                            MOVE CONDICAO-TAMANHO (K) TO TAMANHO
                            MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                            MOVE SPACES            TO CAMPOS-RELATORIO
                            IF   CAMPOS-DATANAME (1: 7) = "Prompt-"
                            OR   CAMPOS-DATANAME (1: 3) = "(=)"
                                 MOVE RELATORIO TO CAMPOS-RELATORIO
                            END-IF
                            READ CAMPOS KEY IS CAMPOS-PRONAMREL
                            IF   CAMPOS-DATA
                                 PERFORM 151-EDIT-DATE THRU 151-99-FIM
                            END-IF
                            IF   NOT CAMPOS-DATA
                            OR  (CAMPOS-MASCARA = SPACES)
                                MOVE SPACES TO LL-CARACTERS (43:)
                                STRING '"' DELIMITED BY SIZE
                                   CONDICAO-OPERANDO (K) (1: TAMANHO)
                                        DELIMITED BY SIZE
                                   '"'  DELIMITED BY SIZE
                                 INTO LL-CARACTERS (43:)
                            ELSE
                                 MOVE CAMPOS-MASCARA TO MASCARA
                                 MOVE 0                TO E2
                                 PERFORM VARYING E
                                         FROM 1 BY 1 UNTIL E > 30
                                         IF   MASCARA (E: 1) NOT = "/"
                                              ADD 1 TO E2
                                              MOVE CONDICAO-OPERANDO (K)
                                                   (E2: 1)
                                                TO MASCARA (E: 1)
                                         END-IF
                                 END-PERFORM
                                 MOVE SPACES  TO LL-CARACTERS (56:)
                                 MOVE MASCARA TO LL-CARACTERS (43:)
                                 STRING "[" DELIMITED BY SIZE
                                    CAMPOS-MASCARA DELIMITED BY SPACE
                                    "]" DELIMITED BY SIZE
                                       INTO LL-CARACTERS (56:)
                            END-IF
                        ELSE
                             MOVE CONDICAO-OPERANDO (K)
                               TO LL-CARACTERS (43:)
                        END-IF
                   ELSE
                       MOVE SPACES TO LL-CARACTERS
                   END-IF
                   END-IF
                   IF   K = I
                        MOVE ALL-X1E TO LL-ATTR
                   ELSE
                        MOVE ALL-X6E TO LL-ATTR
                   END-IF
                   COMPUTE ROW-NUMBER = LL - 1
                   MOVE 3 TO COLUMN-NUMBER
                   IF   KB = 0
                        CALL "CBL_WRITE_SCR_CHATTRS"
                              USING SCREEN-POSITION
                                    LL-CARACTERS
                                    LL-ATTR
                                    X"0049"
                        IF   CONDICAO-LITERAL (K) = "L"
                        IF  (CAMPOS-NUMERICO
                         OR  CAMPOS-CGC-CIC
                         OR  CAMPOS-VALOR)
                             MOVE LL TO LW MOVE CR7 TO CW MOVE 30 TO SW
                             CALL "CWMSGW" USING W ESPACOS
                             CALL "CWREL7" USING "D"
                                      CONDICAO-NUMERO  (K)
                                      CAMPOS-TIPO
                                      CAMPOS-TAMANHO
                                      CAMPOS-DEC
                                      LW CR7 TECLA2
                        ELSE
                              IF   CAMPOS-HORA
                                   MOVE CONDICAO-OPERANDO (K) (1: 6)
                                     TO HORA                  (1: 6)
                                   DISPLAY TELA-HORA
                               END-IF
                        END-IF
                        END-IF
                   END-IF
                   CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                                    LL-ATTR
                                                    X"0049"
                   ADD 1 TO LL K
           END-PERFORM.

       197-99-FIM. EXIT.

       198-OPERACOES.

           MOVE CWBOXF-COLOR-FRAME   TO CWBOXW-COLOR-FRAME
           MOVE CWBOXF-COLOR-BORDER  TO CWBOXW-COLOR-BORDER
           IF  CWUNIX-GUI
               MOVE 04               TO CWBOXW-LINE
               MOVE 06               TO L L9
               MOVE 19               TO L22
           ELSE
               MOVE 07               TO CWBOXW-LINE
               MOVE 09               TO L L9
               MOVE 22               TO L22
           END-IF
           MOVE 03                   TO CWBOXW-COLUMN
           MOVE 15                   TO CWBOXW-VERTICAL-LENGTH
           MOVE 74                   TO CWBOXW-HORIZONTAL-LENGTH
           MOVE 1                    TO WORK-OPERACOES
           SET  CWBOXW-OPEN          TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW
           MOVE SPACES               TO POP
           STRING " Opera‡äes, " DELIMITED BY SIZE
                  PADRAO-L  DELIMITED BY SPACE
                        ' ' DELIMITED BY SIZE
                  RELATORIO DELIMITED BY SPACE
                  ": "      DELIMITED BY SIZE
                       INTO POP
           PERFORM VARYING I FROM 80 BY -1
                     UNTIL I = 1
                        OR (POP (I: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           CALL "CWMSGW" USING TOPO2    POP I
           DISPLAY "Operando 2" LINE L8 COLUMN 48 WITH HIGHLIGHT
                                                     BACKGROUND-COLOR 6
                                                     FOREGROUND-COLOR 7
           DISPLAY "Nome"       LINE L8 COLUMN 05 WITH HIGHLIGHT
                                      BACKGROUND-COLOR 6
                                      FOREGROUND-COLOR 7
           DISPLAY "Operando 1" LINE L8 COLUMN 15
                                      WITH HIGHLIGHT
                                      BACKGROUND-COLOR 6
                                      FOREGROUND-COLOR 7

           MOVE    1                   TO I
           PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM
           MOVE    1                   TO I K CAMPO

           CALL "CBL_GET_CSR_POS"  USING SAVE-CURSOR2
           PERFORM TEST AFTER UNTIL EDIT-ESC
             COMPUTE ROW-NUMBER = L - 1
             MOVE 4 TO COLUMN-NUMBER
             CALL "CBL_SET_CSR_POS"  USING SCREEN-POSITION
             PERFORM 105-LER-TECLA       THRU 105-99-FIM
             EVALUATE TRUE
                 WHEN EDIT-CURSOR-UP
                      IF  I > 1
                          SUBTRACT 1 FROM I
                          IF   L > L9
                               SUBTRACT 1 FROM L
                          END-IF
                      END-IF
                 WHEN EDIT-CURSOR-DOWN AND I < 99
                      IF    OPERACAO-DATANAME (I) NOT = SPACE
                            ADD  1 TO I
                            IF   L < L22
                                 ADD 1 TO L
                            END-IF
                      END-IF
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (OPERACAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-CONTROL-PAGE-DOWN
                      PERFORM VARYING I FROM 99 BY -1
                              UNTIL I = 1
                                 OR (OPERACAO-ATRIBUTOS (I)
                                    NOT = SPACES)
                              CONTINUE
                      END-PERFORM
                      MOVE L9 TO L
                      IF   I > 14
                           MOVE L22 TO L
                           PERFORM UNTIL L = L9
                                   SUBTRACT 1 FROM I L
                           END-PERFORM
                      END-IF
                 WHEN EDIT-CONTROL-PAGE-UP
                      MOVE    1                   TO I
                      MOVE    L9                  TO L
                      PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM
                 WHEN EDIT-PAGE-DOWN
                      PERFORM 14 TIMES
                              COMPUTE K = I + 1
                              IF   K < 100
                              AND (OPERACAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  ADD  1 TO I
                                  IF   L < L22
                                       ADD 1 TO L
                                  END-IF
                              END-IF
                      END-PERFORM
                 WHEN EDIT-PAGE-UP
                      PERFORM 14 TIMES
                              COMPUTE K = I - 1
                              IF   I > 1
                              AND (OPERACAO-ATRIBUTOS (K)
                                  NOT = SPACES)
                                  SUBTRACT 1 FROM I
                                  IF   L > L9
                                       SUBTRACT 1 FROM L
                                  END-IF
                              END-IF
                      END-PERFORM
             END-EVALUATE
             PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM
             IF   EDIT-DEL
             AND (OPERACAO-DATANAME (I) NOT = SPACES)
                  MOVE SPACES TO CWSEND-MSG
                  STRING "Apagar (=)" DELIMITED BY SIZE
                          OPERACAO-DATANAME (I) DELIMITED BY SPACE
                          " ? "               DELIMITED BY SIZE
                      INTO CWSEND-MSG
                  MOVE SPACES          TO CWSEND-SCREENS
                  MOVE "_~Sim_"      TO CWSEND-SCREEN (1)
                  MOVE "_~NÆo_"      TO CWSEND-SCREEN (2)
                  move 2             TO CWSEND-OPTION
                  CALL "CWSEND" USING PARAMETROS-CWSEND
                  IF   CWSEND-OPTION = 1
                       MOVE "(=)"               TO NAME-DELETED
                                                   NAME-OLD
                       MOVE OPERACAO-DATANAME(I)TO NAME-DELETED(4:)
                                                   NAME-OLD(4:)
                       move spaces to OPERACAO-DATANAME (I)
                       PERFORM 200-APAGA-OPERACAO THRU 200-99-FIM
                  END-IF
             END-IF
             IF   EDIT-ENTER
                  MOVE 1 TO CAMPO
               PERFORM UNTIL CAMPO > 6 OR I > 99
                   MOVE 0 TO ERRO TECLA
                   MOVE SPACES TO CWSEND-MSG
                   EVALUATE CAMPO
                       WHEN 1
                            PERFORM 220-CLOSE-MASK THRU 220-99-FIM
                            MOVE "(=)"               TO NAME-DELETED
                                                        NAME-OLD
                            MOVE OPERACAO-DATANAME(I)TO NAME-DELETED(4:)
                                                        NAME-OLD(4:)
                            ACCEPT OPERACAO-DATANAME (I)
                                   LINE L COLUMN 05
                                   WITH PROMPT UPDATE
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            ACCEPT TECLA FROM ESCAPE KEY
                            INSPECT OPERACAO-DATANAME (I)
                                    CONVERTING MINUSCULAS TO MAIUSCULAS
                            INSPECT OPERACAO-DATANAME (I)
                                    CONVERTING ACENTOS TO SEM-ACENTOS
                            MOVE L TO LW MOVE 05 TO CW MOVE 09 TO SW
                            CALL "CWMSGW" USING W OPERACAO-DATANAME (I)
                            IF   OPERACAO-DATANAME (I) = SPACES
                                 IF   OPERACAO-ATRIBUTOS(I) NOT = SPACES
                                      PERFORM 200-APAGA-OPERACAO
                                         THRU 200-99-FIM
                                      SET ENTER-KEY TO TRUE
                                      MOVE 7 TO CAMPO
                                 ELSE
                                      MOVE 1 TO ERRO
                                 END-IF
                            ELSE
                                IF OPERACAO-DATANAME (I)
                                    NOT = NAME-OLD (4:)
                                AND NAME-OLD NOT = "(=)"
                                    PERFORM VARYING Y FROM I BY 1
                                                    UNTIL Y > 100
                                    IF OPERACAO-OPERANDO1(Y) = NAME-OLD
                                       MOVE " " TO OPERACAO-OPERANDO1(Y)
                                       STRING "(=)" OPERACAO-DATANAME(I)
                                                DELIMITED BY SIZE
                                              INTO OPERACAO-OPERANDO1(Y)
                                    END-IF
                                    IF OPERACAO-OPERANDO2(Y) = NAME-OLD
                                       MOVE " " TO OPERACAO-OPERANDO2(Y)
                                       STRING "(=)" OPERACAO-DATANAME(I)
                                                DELIMITED BY SIZE
                                              INTO OPERACAO-OPERANDO2(Y)
                                    END-IF
                                    END-PERFORM
                                    PERFORM VARYING OBJETO-S FROM 1 BY 1
                                              UNTIL OBJETO-S > OBJETO-R
                                     IF  NAME-OLD =
                                         OBJETO-DATANAME (OBJETO-S)
                                         MOVE " "
                                           TO OBJETO-DATANAME (OBJETO-S)
                                         STRING "(=)"
                                                OPERACAO-DATANAME(I)
                                                DELIMITED BY SIZE
                                        INTO OBJETO-DATANAME (OBJETO-S)
                                     END-IF
                                    END-PERFORM
                                END-IF
                            END-IF
                       WHEN 2
                            IF  OPERACAO-INT (I) NOT NUMERIC
                                MOVE 0 TO OPERACAO-INT (I)
                            END-IF
                            IF  OPERACAO-DEC (I) NOT NUMERIC
                                MOVE 0 TO OPERACAO-DEC (I)
                            END-IF
                            PERFORM 210-OPEN-MASK THRU 210-99-FIM
                            DISPLAY OPERACAO-INT (I)
                                    LINE CWBOXW-LINE COLUMN 27
                                        WITH  HIGHLIGHT
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1
                            DISPLAY OPERACAO-DEC (I)
                                    LINE CWBOXW-LINE COLUMN 40
                                        WITH  HIGHLIGHT
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1
                            ACCEPT OPERACAO-INT (I)
                              LINE CWBOXW-LINE COLUMN 27
                                   WITH PROMPT UPDATE
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            ACCEPT TECLA FROM ESCAPE KEY
                            EVALUATE TRUE
                                WHEN OPERACAO-INT(I) > 12
                                     MOVE 1 TO ERRO
                                     MOVE "Inteiros > 12" TO CWSEND-MSG
                                WHEN OPERACAO-DEC (I) > 6
                                     MOVE 1 TO ERRO
                                     MOVE "Decimais > 6" TO CWSEND-MSG
                                WHEN OPERACAO-INT (I) = 0
                                AND  OPERACAO-DEC (I) = 0
                                AND (CURSOR-UP OR ESC)
                                     MOVE 1 TO ERRO
                                     MOVE "Inteiros + Decimais = 0"
                                        TO CWSEND-MSG
                            END-EVALUATE
                       WHEN 3
                            PERFORM 210-OPEN-MASK THRU 210-99-FIM
                            ACCEPT OPERACAO-DEC (I)
                              LINE CWBOXW-LINE COLUMN 40
                                   WITH PROMPT UPDATE
                                        FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            ACCEPT TECLA FROM ESCAPE KEY
                            EVALUATE TRUE
                                WHEN OPERACAO-DEC (I) > 6
                                     MOVE 1 TO ERRO
                                     MOVE "Decimais > 6" TO CWSEND-MSG
                                WHEN OPERACAO-INT (I) = 0
                                AND  OPERACAO-DEC (I) = 0
                                AND (CURSOR-DOWN OR ESC)
                                     MOVE 1 TO ERRO
                                     MOVE "Inteiros + Decimais = 0"
                                        TO CWSEND-MSG
                            END-EVALUATE
                       WHEN 4
                            PERFORM 220-CLOSE-MASK THRU 220-99-FIM
                            PERFORM TEST AFTER UNTIL NOT ESC
                            SET ENTER-KEY TO TRUE
                            MOVE 15 TO CWBOXF-COLUMN
                            IF   OPERACAO-OP-1LIT (I) = "$"
                                 MOVE OPERACAO-OPERANDO1 (I)
                                   TO WORK-NUMBER
                                PERFORM 230-ACCEPT-WORK-NUMBER
                                   THRU 230-99-FIM
                                IF   WORK-NUMBER = SPACES
                                     MOVE SPACES TO OPERACAO-OP-1LIT (I)
                                                  OPERACAO-OPERANDO1 (I)
                                ELSE
                                     MOVE WORK-NUMBER
                                       TO OPERACAO-OPERANDO1 (I)
                                END-IF
                            END-IF
                            IF   OPERACAO-OP-1LIT (I) NOT = "$"
                                 MOVE OPERACAO-OPERANDO1 (I)
                                   TO CWBOXF-OPTION
                                 PERFORM 193-COLUNA-PROMPT-CONDICAO
                                 THRU    193-99-FIM
                                 IF   CWBOXF-OPTION NOT = SPACES
                                      MOVE CWBOXF-OPTION
                                        TO OPERACAO-OPERANDO1 (I)
                                      SET ENTER-KEY TO TRUE
                                 ELSE
                                      SET ESC TO TRUE
                                      MOVE " " TO OPERACAO-OPERANDO1 (I)
                                      MOVE "$" TO OPERACAO-OP-1LIT   (I)
                                 END-IF
                            END-IF
                            END-PERFORM
                            MOVE L TO LW MOVE 15 TO CW MOVE 30 TO SW
                            CALL "CWMSGW" USING W OPERACAO-OPERANDO1 (I)
                            IF   OPERACAO-OPERANDO1 (I) = SPACES
                                 MOVE 1 TO ERRO
                            END-IF
                       WHEN 5
                            PERFORM 220-CLOSE-MASK THRU 220-99-FIM
                            MOVE 001 TO CWBOXS-COLOR-FRAME
                            MOVE 002 TO CWBOXS-COLOR-BORDER
                            MOVE 046 TO CWBOXS-COLOR-BARR-MENU
                            EVALUATE OPERACAO-OPERADOR (I)
                                  WHEN "-"   MOVE 2 TO CWBOXS-OPTION
                                  WHEN "*"   MOVE 3 TO CWBOXS-OPTION
                                  WHEN "/"   MOVE 4 TO CWBOXS-OPTION
                                  WHEN "P"   MOVE 5 TO CWBOXS-OPTION
                                  WHEN "R"   MOVE 6 TO CWBOXS-OPTION
                                  WHEN OTHER MOVE 1 TO CWBOXS-OPTION
                            END-EVALUATE
                            COMPUTE CWBOXS-LINE = L - 2
                            PERFORM UNTIL (CWBOXS-LINE + 6) < L22
                                    SUBTRACT 1 FROM CWBOXS-LINE
                            END-PERFORM
                            MOVE 45 TO CWBOXS-COLUMN
                            MOVE SPACES TO CWBOXS-TITLE
                                           CWBOXS-ITENS
                            MOVE "~+ Soma"       TO CWBOXS-TEXT (1)
                            MOVE "~- Subtrai"    TO CWBOXS-TEXT (2)
                            MOVE "~* Multiplica" TO CWBOXS-TEXT (3)
                            MOVE "~/ Divide"     TO CWBOXS-TEXT (4)
                            MOVE "~Potˆncia"     TO CWBOXS-TEXT (5)
                            MOVE "~Raiz"         TO CWBOXS-TEXT (6)
                            CALL "CWBOXS" USING PARAMETROS-CWBOXS
                            MOVE 110 TO CWBOXS-COLOR-FRAME
                                        CWBOXS-COLOR-BORDER
                            MOVE 112 TO CWBOXS-COLOR-BARR-MENU
                            MOVE CWBOXS-OPTION-CHAR
                              TO OPERACAO-OPERADOR (I)
                            DISPLAY OPERACAO-OPERADOR (I)
                                   LINE L COLUMN 46
                                   WITH FOREGROUND-COLOR 6
                                        BACKGROUND-COLOR 1 HIGHLIGHT
                            IF   OPERACAO-OPERADOR (I) = SPACES
                                 MOVE 1 TO ERRO
                            END-IF
                       WHEN 6
                            PERFORM 220-CLOSE-MASK THRU 220-99-FIM
                            MOVE 48 TO CWBOXF-COLUMN
                            PERFORM TEST AFTER UNTIL NOT ESC
                            SET ENTER-KEY TO TRUE
                            IF   OPERACAO-OP-2LIT (I) = "$"
                                 MOVE OPERACAO-OPERANDO2 (I)
                                   TO WORK-NUMBER
                                PERFORM 230-ACCEPT-WORK-NUMBER
                                   THRU 230-99-FIM
                                IF   WORK-NUMBER = SPACES
                                     MOVE SPACES TO OPERACAO-OP-2LIT (I)
                                                  OPERACAO-OPERANDO2 (I)
                                ELSE
                                     MOVE WORK-NUMBER
                                       TO OPERACAO-OPERANDO2 (I)
                                END-IF
                            END-IF
                            IF   OPERACAO-OP-2LIT (I) NOT = "$"
                                 MOVE OPERACAO-OPERANDO2 (I)
                                   TO CWBOXF-OPTION
                                 PERFORM 193-COLUNA-PROMPT-CONDICAO
                                 THRU    193-99-FIM
                                 IF   CWBOXF-OPTION NOT = SPACES
                                      MOVE CWBOXF-OPTION
                                        TO OPERACAO-OPERANDO2 (I)
                                      SET ENTER-KEY TO TRUE
                                 ELSE
                                      SET ESC TO TRUE
                                      MOVE " " TO OPERACAO-OPERANDO2 (I)
                                      MOVE "$" TO OPERACAO-OP-2LIT   (I)
                                 END-IF
                            END-IF
                            END-PERFORM
                            MOVE L TO LW MOVE 48 TO CW MOVE 30 TO SW
                            CALL "CWMSGW" USING W OPERACAO-OPERANDO2 (I)
                            IF   OPERACAO-OPERANDO2 (I) = SPACES
                                 MOVE 1 TO ERRO
                            END-IF
                    END-EVALUATE
                    IF   ERRO = 0
                    AND  CAMPO = 1
                    AND (OPERACAO-DATANAME (I) NOT = SPACES)
                         PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 99
                            IF  (Y NOT = I)
                            AND (OPERACAO-DATANAME (Y) =
                                 OPERACAO-DATANAME (I))
                                 MOVE 1   TO ERRO
                                 MOVE "?" TO PROMPT-DATANAME (I)
                            END-IF
                         END-PERFORM
                    END-IF
                    EVALUATE TRUE
                        WHEN ESC
                         AND (OPERACAO-ATRIBUTOS (I) = SPACES
                          OR  ERRO = 0)
                              MOVE 7 TO CAMPO
                        WHEN (ENTER-KEY OR CURSOR-DOWN)
                         AND ERRO = 0
                         AND (OPERACAO-DATANAME (I) NOT = SPACES)
                             ADD 1 TO CAMPO
                        WHEN CURSOR-UP
                             IF   CAMPO > 1
                                  IF  (ERRO = 1 AND CAMPO = 2)
                                      MOVE SPACES TO CWSEND-SCREENS
                                      CALL "CWSEND"
                                            USING PARAMETROS-CWSEND
                                  ELSE
                                       SUBTRACT 1 FROM CAMPO
                                  END-IF
                             END-IF
                       WHEN ERRO = 1
                            MOVE SPACES TO CWSEND-SCREENS
                            CALL "CWSEND" USING PARAMETROS-CWSEND
                    END-EVALUATE
             END-PERFORM
             PERFORM 220-CLOSE-MASK      THRU 220-99-FIM
             PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM
             END-IF
           END-PERFORM

           MOVE 0                    TO WORK-OPERACOES
           PERFORM 870-ATUALIZA-OPERACOES THRU 870-99-FIM
           PERFORM 220-CLOSE-MASK THRU 220-99-FIM
           SET  CWBOXW-CLOSE         TO TRUE
           CALL "CWBOXW"          USING PARAMETROS-CWBOXW
           MOVE 0                    TO TECLA
           MOVE SPACES               TO FIM.

       198-99-FIM. EXIT.

       199-EXIBE-OPERACOES.

           COMPUTE K = I - (L - L9)
           MOVE L9 TO LL
           PERFORM 14 TIMES
                   MOVE SPACES TO LL-CARACTERS
                   IF   K = I
                        MOVE ALL-X1E TO LL-ATTR
                   ELSE
                        MOVE ALL-X6E TO LL-ATTR
                   END-IF
                   IF   K < 100
                        STRING  " " OPERACAO-DATANAME  (K)
                                " " OPERACAO-OPERANDO1 (K)
                                " " OPERACAO-OPERADOR  (K)
                                " " OPERACAO-OPERANDO2 (K)
                                    DELIMITED BY SIZE
                         INTO LL-CARACTERS
                   COMPUTE ROW-NUMBER = LL - 1
                   MOVE 3 TO COLUMN-NUMBER
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                      LL-CARACTERS
                                                      LL-ATTR
                                                      X"004A"
                   ADD 1 TO LL K
           END-PERFORM.

       199-99-FIM. EXIT.

       200-APAGA-OPERACAO.

           PERFORM VARYING OBJETO-Y
                     FROM 1 BY 1
                     UNTIL OBJETO-Y > OBJETO-R
                   IF  NAME-OLD = OBJETO-DATANAME(OBJETO-Y)
                       MOVE LOW-VALUES TO OBJETO-ATTR (OBJETO-Y)
                   END-IF
           END-PERFORM
           PERFORM VARYING K FROM I BY 1 UNTIL K > 98
                   COMPUTE Y = K + 1
                   MOVE OPERACAO-ATRIBUTOS (Y) TO OPERACAO-ATRIBUTOS (K)
                   IF   Y = 99
                        MOVE SPACES TO OPERACAO-ATRIBUTOS (Y)
                   END-IF
           END-PERFORM.

       200-LIMPA.

           IF  I-SAVE = 0
               MOVE I TO I-SAVE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 98
                            OR OPERACAO-OPERANDO1 (I) = NAME-DELETED
                            OR OPERACAO-OPERANDO2 (I) = NAME-DELETED
                       CONTINUE
               END-PERFORM
               IF   I NOT > 98
                    MOVE "(=)"                 TO NAME-OLD
                    MOVE OPERACAO-DATANAME (I) TO NAME-OLD (4:)
                    GO TO 200-APAGA-OPERACAO
               ELSE
                    MOVE I-SAVE TO I
               END-IF
               MOVE 0 TO I-SAVE
           ELSE
               MOVE I-SAVE TO I
               MOVE 0      TO I-SAVE
               GO TO 200-LIMPA
           END-IF
           PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM.

       200-99-FIM. EXIT.

       210-OPEN-MASK.

           IF  OPEN-MASK = 0
               MOVE     1   TO OPEN-MASK
               MOVE     L   TO CWBOXW-LINE
               SUBTRACT 1 FROM CWBOXW-LINE
               MOVE 031     TO CWBOXW-COLOR-FRAME
                               CWBOXW-COLOR-BORDER
               MOVE 15      TO CWBOXW-COLUMN
               MOVE 01      TO CWBOXW-VERTICAL-LENGTH
               MOVE 26      TO CWBOXW-HORIZONTAL-LENGTH
               IF    CWBOXW-LINE > 21
                     MOVE 21 TO CWBOXW-LINE
               END-IF
               SET  CWBOXW-OPEN         TO TRUE
               CALL "CWBOXW"         USING PARAMETROS-CWBOXW
               MOVE CWBOXF-COLOR-FRAME  TO CWBOXW-COLOR-FRAME
               MOVE CWBOXF-COLOR-BORDER TO CWBOXW-COLOR-BORDER
               ADD  1                   TO CWBOXW-LINE
               MOVE CWBOXW-LINE         TO LW
               MOVE 17                  TO CW
               MOVE 24                  TO SW
               CALL "CWMSGW" USING W "Inteiros:    Decimais:  "
           END-IF.

       210-99-FIM. EXIT.

       220-CLOSE-MASK.

           IF   OPEN-MASK = 1
                SET  CWBOXW-CLOSE   TO TRUE
                CALL "CWBOXW" USING PARAMETROS-CWBOXW
                MOVE 0 TO OPEN-MASK
                PERFORM 199-EXIBE-OPERACOES THRU 199-99-FIM
           END-IF.

       220-99-FIM. EXIT.

       230-ACCEPT-WORK-NUMBER.

           MOVE SPACES TO CWSEND-SCREENS WORK-SINAL
           ACCEPT WORK-NUMBER
                  LINE L COLUMN CWBOXF-COLUMN
              WITH PROMPT UPDATE
                   FOREGROUND-COLOR 6
                   BACKGROUND-COLOR 1 HIGHLIGHT
           MOVE 0 TO WN2 ERRO
           MOVE SPACES TO WORK-NUMBER2
           PERFORM VARYING WN FROM 1 BY 1 UNTIL WN > 30
                   IF   WORK-NUMBER (WN: 1) = "."
                   AND  ERRO = 0
                        MOVE 2 TO ERRO
                   END-IF
                   IF   WORK-NUMBER (WN: 1) = "."
                   AND  ERRO = 3
                        MOVE 4 TO ERRO
                   END-IF
                   IF   WORK-NUMBER (WN: 1) NUMERIC
                   OR   WORK-NUMBER (WN: 1) = ","
                        IF   WORK-NUMBER (WN: 1) = ","
                             IF   ERRO = 2
                                  MOVE 0 TO ERRO
                             ELSE
                                  IF  ERRO = 0
                                      MOVE 3 TO ERRO
                                  END-IF
                             END-IF
                        END-IF
                        ADD 1 TO WN2
                        MOVE WORK-NUMBER (WN: 1)
                          TO WORK-NUMBER2 (WN2: 1)
                   ELSE
                        IF  (WORK-NUMBER (WN: 1) NOT = SPACE)
                        AND (WORK-NUMBER (WN: 1) NOT = ".")
                        AND (WORK-NUMBER (WN: 1) NOT = "+")
                        AND (WORK-NUMBER (WN: 1) NOT = "-")
                            MOVE 1 TO ERRO
                        ELSE
                           IF  WORK-NUMBER (WN: 1) = "-" OR "+"
                               IF   WORK-SINAL NOT = SPACE
                                    MOVE 5 TO ERRO
                               ELSE
                                    MOVE WORK-NUMBER (WN: 1)
                                      TO WORK-SINAL
                               END-IF
                           END-IF
                        END-IF
                   END-IF
           END-PERFORM

           IF  ERRO = 4
               MOVE "Utilize a v¡rgula decimal" TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               GO TO 230-ACCEPT-WORK-NUMBER
           END-IF

           IF  ERRO = 5
               MOVE "Sinal incompreens¡vel" TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               GO TO 230-ACCEPT-WORK-NUMBER
           END-IF

           IF  ERRO = 2
               MOVE "O valor nÆo deve ser editado" TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               GO TO 230-ACCEPT-WORK-NUMBER
           END-IF

           IF  ERRO = 1
               MOVE "Dado nÆo num‚rico" TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               GO TO 230-ACCEPT-WORK-NUMBER
           ELSE
               MOVE 0 TO ERRO
               IF  WORK-SINAL = "-"
                   ADD 1    TO WN2
                   MOVE "-" TO WORK-NUMBER2 (WN2: 1)
               END-IF
               MOVE WORK-NUMBER2 TO WORK-NUMBER
               DISPLAY WORK-NUMBER
                      LINE L COLUMN CWBOXF-COLUMN
                           WITH HIGHLIGHT
                                FOREGROUND-COLOR 6
                                BACKGROUND-COLOR 1
           END-IF.

       230-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   TMP (1: 2) = "ON"
                MOVE 1 TO MULTI-USER
           END-IF
           CALL "CWMOLD" USING CORES MOLDURAS
           CALL "UNIX" USING ALL-X02 X"004E" CORES
           CALL "UNIX" USING ALL-X0A X"004E" CORES
           CALL "UNIX" USING ALL-X0B X"004E" CORES
           CALL "UNIX" USING ALL-X0C X"004E" CORES
           CALL "UNIX" USING ALL-X0D X"004E" CORES
           CALL "UNIX" USING ALL-X0E X"004E" CORES
           CALL "UNIX" USING ALL-X1E X"004E" CORES
           CALL "UNIX" USING ALL-X3E X"004E" CORES
           CALL "UNIX" USING ALL-X4B X"004E" CORES
           CALL "UNIX" USING ALL-X4E X"004E" CORES
           CALL "UNIX" USING ALL-X5E X"004E" CORES
           CALL "UNIX" USING ALL-X6E X"004E" CORES
           CALL "UNIX" USING ALL-XB0 X"004E" CORES
           CALL "UNIX" USING ALL-XB2 X"004E" CORES
           CALL "UNIX" USING OBS-ATTR X"0050" CORES
           CALL "UNIX" USING ATTRIBUTE-ARR X"004E" CORES
           CALL X"91" USING X91-RESULT X91-F16 X91-PARAMETER

           IF   X91-PARAMETER < 1
                MOVE "Rodar sob o CWMENU do COBOLware"
                  TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                GOBACK
           END-IF

           DISPLAY "RELATOR-RESTRICT" UPON ENVIRONMENT-NAME
           ACCEPT RELATOR-RESTRICT    FROM ENVIRONMENT-VALUE
           IF   RELATOR-RESTRICT = SPACES
                DISPLAY "RELATOR_RESTRICT" UPON ENVIRONMENT-NAME
                ACCEPT RELATOR-RESTRICT    FROM ENVIRONMENT-VALUE
           END-IF
           INSPECT RELATOR-RESTRICT CONVERTING "of" TO "OF"

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "RL"     TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "10"
                     MOVE CWCONF-RELATOR TO RELDIR
                END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           DISPLAY "FSSERVER" UPON ENVIRONMENT-NAME
           ACCEPT  FSSERVER  FROM ENVIRONMENT-VALUE
           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                  SIZE-OLD-DIR
           IF  CWUNIX-ON
               MOVE OLD-DIRECTORY TO CURDIR
           ELSE
               MOVE SPACES TO CURDIR
               CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                           RETURN-STATUS
               STRING OLD-DRIVE DELIMITED BY SPACE
                      ":\"           DELIMITED BY SPACE
                      OLD-DIRECTORY  DELIMITED BY SPACE
                                INTO CURDIR
           END-IF

           IF  CWUNIX-ON
           OR  CWUNIX-GUI
      *        MOVE "v^<>" TO SETAS
               MOVE 1 TO MULTI-USER
               IF  CWUNIX-GUI
                   MOVE SPACES TO ALL-XB0
                                  ALL-XB2
                   MOVE 20 TO ALTURA
                   MOVE 16 TO ALTURA2
                   MOVE 18 TO ALTURA2
                   MOVE 20 TO BOT
                   MOVE 2  TO TOPO
                              MARGIN
                   MOVE 80 TO BOTM
                   MOVE ALL X"70" TO ALL-X02
                   MOVE 4   TO SETE
                   MOVE 19  TO QUINZE
                   MOVE 5   TO L8
               END-IF
           END-IF
           IF   FSSERVER = SPACES
                IF RELDIR = SPACES
                   MOVE SPACES TO RELDIR
                   STRING CURDIR DELIMITED BY SPACE
                          "\relator"     DELIMITED BY SIZE
                                    INTO RELDIR
                   MOVE RELDIR TO CURDIR
                ELSE
                   IF (RELDIR(1:1) = '/' OR '\' OR '.')
                   OR RELDIR(2:1) = ':'
                      MOVE RELDIR TO CURDIR
                   ELSE
                     MOVE SPACES TO ELEMENTO
                      STRING CURDIR DELIMITED BY SPACE
                             "\"     DELIMITED BY SIZE
                             RELDIR DELIMITED BY SPACE
                                       INTO ELEMENTO
                      MOVE ELEMENTO TO RELDIR CURDIR
                    END-IF
                END-IF
                MOVE 046 TO CWPATH-COLOR-FRAME
                            CWPATH-COLOR-BORDER
                MOVE 126 TO CWPATH-COLOR-BARR-MENU
           END-IF
           IF RELDIR = SPACES
              MOVE 'relator' TO RELDIR
           END-IF
           CALL "FS_CREATE_DIR" USING RELDIR
           MOVE SPACES TO LB-CAMPOS
           STRING RELDIR  DELIMITED BY SPACE
                  "\COLUNAS" DELIMITED BY SIZE
                    INTO LB-CAMPOS
           INSPECT LB-CAMPOS CONVERTING "\" TO "/"
           MOVE SPACES TO LB-FORMATOS
           STRING RELDIR  DELIMITED BY SPACE
                  "\FORMATOS" DELIMITED BY SIZE
                    INTO LB-FORMATOS
           INSPECT LB-FORMATOS CONVERTING "\" TO "/"
           CALL "CWGETU"         USING OPERADOR TASK PROGRAMA "?"
           DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           IF   TMP = SPACES
                DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                ACCEPT  TMP       FROM ENVIRONMENT-VALUE
           END-IF

           MOVE TASK (2: 5) TO TMP-LB (4: 5)
           IF   TMP = SPACE
                MOVE "relator" TO TMP
           END-IF
           MOVE SPACES TO LB-CABS
           STRING TMP    DELIMITED BY SPACE
                  "\"    DELIMITED BY SIZE
                  TMP-LB DELIMITED BY SPACE
             INTO LB-CABS

           INSPECT LB-CABS CONVERTING "\" TO "/"

           MOVE 08             TO CWBOXF-LINE
           MOVE 04             TO CWBOXF-COLUMN
           MOVE "CWREL6"       TO CWBOXF-PROGRAM
           MOVE SPACES         TO CWBOXF-OPTION
           MOVE 1              TO CWBOXF-ORDER
           MOVE 10             TO CWBOXF-VERTICAL-LENGTH
           MOVE 41             TO CWBOXF-HORIZONTAL-LENGTH
           MOVE 08             TO CWBOXF-STRING-1-LENGTH
           MOVE 39             TO CWBOXF-STRING-2-LENGTH
           MOVE LENGTH OF CAMPOS-DATANAME TO LENAME
           MOVE "_Relat¢rios_" TO CWBOXF-TITLE.

       RETRY.

           MOVE SPACES         TO CWBOXF-OPTION
           CALL "CWBOXF"    USING PARAMETROS-CWBOXF

           IF   CWBOXF-OPTION = SPACES
                GOBACK
           END-IF

           OPEN I-O FORMATOS

           MOVE SPACES        TO FORMATOS-REG
           INITIALIZE            FORMATOS-REG
           MOVE "1"           TO FORMATOS-TIPO-REG
           MOVE CWBOXF-OPTION TO FORMATOS-REPORT RELATORIO
           IF   RELATORIO = ALL X"00"
                MOVE 1 TO INCLUINDO
                MOVE SPACES TO RELATORIO
      *         EXEC COBOLware OBJECT PUSH-BUTTON SMALL
      *                   LINE 23 COLUMN 03 WIDTH 13
      *                   CAPTION "<Esc>-Sa¡da "
      *                   KEY ESC (TAB-OFF)
      *         END-EXEC
                PERFORM 830-ACEITA-TELA THRU 830-99-FIM
                EXEC COBOLware OBJECT DROP END-EXEC
                IF   ARQUIVO
                     MOVE NOME-ARQUIVO TO FORMATOS-ARQUIVO
                     MOVE DESTINO      TO FORMATOS-DESTINO
                     MOVE DELIM        TO FORMATOS-DELIM
                ELSE
                     MOVE SIZE-PAGE    TO FORMATOS-SIZE-PAGE
                END-IF
                IF   INCLUINDO = 3
                     PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                     REWRITE FORMATOS-REG
                ELSE
                     MOVE "1"       TO FORMATOS-TIPO-REG
                     MOVE RELATORIO TO FORMATOS-REPORT
                     MOVE 0         TO FORMATOS-OBJETOS
                     PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                     WRITE FORMATOS-REG
                END-IF
           ELSE
                MOVE 0 TO INCLUINDO
           END-IF

           READ FORMATOS WITH LOCK
           IF   FS-FORMATOS > "09"
                MOVE SPACES TO CWSEND-SCREENS
                MOVE "Relat¢rio em manuten‡Æo" TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                GO TO RETRY
           END-IF

           MOVE FORMATOS-TAMANHO   TO CAB
           MOVE FORMATOS-PADRAO    TO PADRAO
           IF ARQUIVO
              MOVE FORMATOS-ARQUIVO TO NOME-ARQUIVO
              MOVE FORMATOS-DESTINO TO DESTINO
              MOVE FORMATOS-DELIM   TO DELIM
           ELSE
              MOVE FORMATOS-SIZE-PAGE TO SIZE-PAGE
           END-IF
           MOVE "CWREL4"          TO CWBOXF-PROGRAM
           MOVE 0                 TO CWBOXF-STRING-1-LENGTH
           MOVE 30                TO CWBOXF-STRING-2-LENGTH
           MOVE 32                TO CWBOXF-HORIZONTAL-LENGTH
           IF   INCLUINDO = 0
      *         EXEC COBOLware OBJECT PUSH-BUTTON SMALL
      *              LINE 23 COLUMN 03 WIDTH 11
      *              CAPTION "<Esc>-Sa¡da"
      *              KEY ESC TAB-OFF
      *         END-EXEC
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 23 COLUMN 15 WIDTH 11
                          CAPTION "f2-~Exportar"
                          KEY F2 TAB-OFF
                END-EXEC
                PERFORM 830-ACEITA-TELA    THRU 830-99-FIM
                EXEC COBOLware OBJECT DROP END-EXEC
                IF   ARQUIVO
                     MOVE NOME-ARQUIVO TO FORMATOS-ARQUIVO
                     MOVE DESTINO      TO FORMATOS-DESTINO
                     MOVE DELIM        TO FORMATOS-DELIM
                ELSE
                     MOVE SIZE-PAGE    TO FORMATOS-SIZE-PAGE
                END-IF
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                REWRITE FORMATOS-REG
           END-IF

           IF   CWUNIX-GUI
                DISPLAY (1, 1) ERASE
           END-IF
           IF   F2
                MOVE SPACES TO CWPATH-FILE
                               LB-TEXTO
                STRING CURDIR     DELIMITED BY SPACE
                       "\"        DELIMITED BY SIZE
                       RELATORIO  DELIMITED BY SPACE
                       ".rep"     DELIMITED BY SIZE
                       INTO LB-TEXTO
                IF   CWUNIX-ON
                     INSPECT LB-TEXTO CONVERTING "\" TO "/"
                END-IF
                MOVE "Arquivo     : " TO POP
                MOVE LB-TEXTO         TO POP (15: )
                CALL "CWMSGW" USING "180364" POP
                PERFORM TEST AFTER UNTIL FS-TEXTO = "00"
                   SET CWPATH-WITH-DIR     TO TRUE
                   SET CWPATH-WITH-DRIVES  TO TRUE
                   SET CWPATH-WITH-NEWDIR  TO TRUE
                   SET CWPATH-WITH-NEWFILE TO TRUE
                   MOVE "_Exportar_para:_"   TO CWPATH-TITLE
                   MOVE SPACES               TO CWPATH-DEFAULT
                   STRING RELATORIO DELIMITED BY SPACE
                             ".rep" DELIMITED BY SIZE
                               INTO CWPATH-DEFAULT
                   MOVE SPACES TO CWPATH-PATH
                   STRING CURDIR     DELIMITED BY SPACE
                          "\*.rep"   DELIMITED BY SIZE
                          INTO CWPATH-PATH
                   CALL "CWPATH" USING PARAMETROS-CWPATH
                   IF   CWPATH-FILE = SPACES
                        close formatos
                        goback
                        CALL "CWMSGW" USING "230313" "Exportando..."
                        ACCEPT LB-TEXTO LINE 18 COLUMN 17 WITH UPDATE
                   ELSE
                        MOVE CWPATH-FILE TO LB-TEXTO
                        CALL "CWMSGW" USING "181750" LB-TEXTO
                   END-IF
                   IF   CWUNIX-ON
                        INSPECT LB-TEXTO CONVERTING "\" TO "/"
                   END-IF
                   CALL "CWBINF" USING 'I' FS-TEXTO LB-TEXTO
                   IF   FS-TEXTO = "30" OR "35"
                        CALL "CWBINF" USING 'O' FS-TEXTO LB-TEXTO
                        IF   FS-TEXTO NOT = "00"
                             CALL "CWISAM" USING ER-TEXTO
                        END-IF
                   ELSE
                        IF   FS-TEXTO < "10"
                             MOVE SPACES TO CWSEND-MSG
                             STRING "Arquivo: "     DELIMITED BY SIZE
                                     LB-TEXTO       DELIMITED BY SPACE
                                     " j  existe !" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                             MOVE SPACES      TO CWSEND-SCREENS
                             MOVE "~Sobrepor" TO CWSEND-SCREEN  (1)
                             MOVE "~Outro"    TO CWSEND-SCREEN  (2)
                             MOVE "~Cancelar" TO CWSEND-SCREEN  (3)
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             CALL "CWBINF" USING 'C' FS-TEXTO
                             EVALUATE CWSEND-OPTION
                                      WHEN 1 CALL "CWBINF" USING 'O'
                                                  FS-TEXTO LB-TEXTO
                                      WHEN 2 MOVE "99" TO FS-TEXTO
                                      WHEN 3 CLOSE FORMATOS
                                             GOBACK
                              END-EVALUATE
                   END-IF
                END-PERFORM
                PERFORM 820-EXPORTAR THRU 820-99-FIM
           END-IF
           MOVE FORMATOS-PROVEDOR TO CWBOXF-WORK-AREA (02: 8)
           MOVE RELATORIO         TO CWBOXF-WORK-AREA (10: 7)
           MOVE FORMATOS-OBJETOS  TO OBJETO-L
           IF  NOT ARQUIVO
               MOVE FORMATOS-TAMANHO  TO CAB
               MOVE FORMATOS-MASCARA  TO MAPA-STATUS-LINHA (1: 100)
               MOVE FORMATOS-MASCARA(101: 50) TO MAPA-FLAGS
               MOVE FORMATOS-FLAGS    TO MAPA-FLAGS (51: 50)
           END-IF

           DISPLAY RODAPE
           MOVE SPACES TO PERC-TIT
           STRING "Carregando " DELIMITED BY SIZE
                  RELATORIO     DELIMITED BY SPACE
                  "..."         DELIMITED BY SIZE
                           INTO PERC-TIT

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
                        IF   F2
                             CALL "CWMSGW" USING "230308" "Exportar"
                             PERFORM 820-EXPORTAR THRU 820-99-FIM
                        END-IF
                        IF   FORMATOS-TIPO = "O" OR "Q" OR "P"
                                                 OR "S" OR "C" OR '3'
                             EVALUATE FORMATOS-TIPO
                                  WHEN "3"
                                       ADD 1 TO tags
                                       MOVE FORMATOS-STRING
                                         TO TAG (tags) (6:)
                                       MOVE '0'TO TAG-FLAG (tags)
                                  WHEN "O"
                                       MOVE FORMATOS-STRING
                                         TO MAPA-ORDENACAO
                                  WHEN "Q"
                                       MOVE FORMATOS-COLUNA
                                         TO NIVEL-QUEBRA(FORMATOS-LINHA)
                                  WHEN "P"
                                       MOVE FORMATOS-MASCARA
                                         TO PROMPT-ATRIBUTOS
                                            (FORMATOS-LINHA)
                                  WHEN "S"
                                       MOVE FORMATOS-MASCARA
                                         TO CONDICAO-ATRIBUTOS
                                            (FORMATOS-LINHA)
                                  WHEN "C"
                                       MOVE FORMATOS-MASCARA
                                         TO OPERACAO-ATRIBUTOS
                                            (FORMATOS-LINHA)
                             END-EVALUATE
                        ELSE
                             EVALUATE TRUE
                               WHEN FORMATOS-ALFANUMERICO
                                    MOVE ALL "X" TO ELEMENTO
                               WHEN FORMATOS-EDITADO
                                    MOVE FORMATOS-MASCARA TO ELEMENTO
                               WHEN FORMATOS-LITERAL
                                    MOVE FORMATOS-STRING  TO ELEMENTO
                                    INSPECT ELEMENTO
                                            (1: FORMATOS-TAMANHO)
                                    CONVERTING SPACE TO X"00"
                               WHEN FORMATOS-NUMERICO
                                    MOVE ALL "9" TO ELEMENTO
                               WHEN FORMATOS-ESTILO
                                    MOVE X"FE"   TO ELEMENTO
                             END-EVALUATE
                             ADD  1     TO OBJETO-R OBJETO-V
                             EXEC COBOLware PROCESS SHOW
                                  HEADER PERC-TIT
                                  LINE 10 COLUMN 10
                                  EMPTY OBJETO-L
                                  FULL OBJETO-V
                             END-EXEC
                             MOVE ELEMENTO
                               TO MAPA-LIN (FORMATOS-LINHA)
                                  (FORMATOS-COLUNA: FORMATOS-TAMANHO)
                             IF   FORMATOS-LINHA > DET
                             AND (STATUS-LINHA (FORMATOS-LINHA)
                                 = SPACE OR "K" or "t")
                                  MOVE FORMATOS-LINHA TO DET
                             END-IF
                             MOVE FORMATOS-LINHA
                               TO OBJETO-RL(OBJETO-R)
                                  LIN-W
                             MOVE FORMATOS-COLUNA
                               TO OBJETO-RC(OBJETO-R)
                                  COL-W
                             MOVE FORMATOS-TIPO
                               TO OBJETO-TIPO (OBJETO-R)
                             MOVE FORMATOS-TAMANHO TO OBJETO-TAMANHO
                                                     (OBJETO-R)
                             IF   FORMATOS-LITERAL
                                  MOVE "Texto"
                                    TO OBJETO-DATANAME (OBJETO-R)
                             ELSE
                                  MOVE FORMATOS-DATANAME
                                    TO OBJETO-DATANAME (OBJETO-R)
                                  IF FORMATOS-RESULTADO
                                     MOVE FORMATOS-DATANAME2
                                       TO OBJETO-DATANAME2 (OBJETO-R)
                                  END-IF
                             END-IF
                             PERFORM FORMATOS-TAMANHO TIMES
                                MOVE OBJETO-R TO CX-M (LIN-W COL-W)
                                ADD  1        TO COL-W
                             END-PERFORM
                        END-IF
                   END-IF
           END-PERFORM
           EXEC COBOLware PROCESS CLOSE END-EXEC
           DISPLAY RODAPE
           IF   F2
                CLOSE FORMATOS
                CALL "CWBINF" USING 'C' FS-TEXTO
                GOBACK
           END-IF
           MOVE SPACES        TO FORMATOS-REG
           INITIALIZE            FORMATOS-REG
           MOVE "1"           TO FORMATOS-TIPO-REG
           MOVE RELATORIO     TO FORMATOS-REPORT
           READ FORMATOS WITH LOCK
           MOVE FORMATOS-TAMANHO     TO CAB
           IF   ARQUIVO
                MOVE FORMATOS-ARQUIVO     TO NOME-ARQUIVO
                MOVE FORMATOS-DESTINO     TO DESTINO
                MOVE FORMATOS-DELIM       TO DELIM
           ELSE
                MOVE FORMATOS-SIZE-PAGE   TO SIZE-PAGE
           END-IF
           IF  CWUNIX-ON
           OR  CWUNIX-GUI
               CONTINUE
           ELSE
               CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                           MOUSE-BUTTONS
               CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                   MOUSE-POSITION
           END-IF
           MOVE MOUSE-POSITION   TO MOUSE-POSITION-A
           OPEN I-O CAMPOS
           MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
           MOVE 0                 TO CAMPOS-COLUNA
           READ CAMPOS
           MOVE CAMPOS-TAMANHO    TO MAIOR-NOME
           PERFORM 850-ATUALIZA-PROMPTS THRU 850-99-FIM

           MOVE ALL "." TO MAPA-LIN (100)
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 500
                   IF   C (3: 1) = "5"
                        MOVE "+" TO MAPA-LIN (100) (C: 1)
                   END-IF
                   IF   C (3: 1) = "0"
                        COMPUTE C0 = C / 10
                        MOVE C0 (2: 1) TO MAPA-LIN (100) (C: 1)
                        IF   C0 (1: 1) NOT = "0"
                             SUBTRACT 1 FROM C
                             MOVE C0 (1: 1) TO MAPA-LIN (100) (C: 1)
                             ADD      1   TO C
                        END-IF
                   END-IF
           END-PERFORM

           IF   ARQUIVO
                MOVE 'arquivo' TO PADRAO-L
                PERFORM 870-ATUALIZA-OPERACOES THRU 870-99-FIM
                EXIT PARAGRAPH
           END-IF

           IF   NOT CWUNIX-GUI
                CALL "CWMSGW" USING "070101" SETA-UP
                CALL "CWMSGW" USING "078001" SETA-UP
                CALL "CWMSGW" USING "210101" SETA-DOWN
                CALL "CWMSGW" USING "218001" SETA-DOWN
                CALL "CWMSGW" USING "220201" SETA-LEFT
                CALL "CWMSGW" USING "060201" SETA-LEFT
                CALL "CWMSGW" USING "227901" SETA-RIGHT
                CALL "CWMSGW" USING "067901" SETA-RIGHT
                CALL "CWMSGW" USING "231607" RELATORIO
                CALL "CWMSGW" USING "232409" PADRAO-TXT (PADRAO)
                CALL "CWMSGW" USING "230365" LEGENDA
                MOVE ALL-X3E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1621" ATT X"000B"
                MOVE ALL-X1E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"162C" ATT X"0009"
                MOVE ALL-X4E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1635" ATT X"0008"
                MOVE ALL-X5E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"163D" ATT X"0007"
           ELSE
                CALL "CWKBDC" USING "9999"
                CALL "CWMSGW" USING "230107" RELATORIO
                CALL "CWMSGW" USING "230909" PADRAO-TXT (PADRAO)
                CALL "CWMSGW" USING "234734"
                     "Cabe‡alho  Detalhe  Quebra  Total "
                MOVE ALL-X3E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"162D" ATT X"000B"
                MOVE ALL-X1E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1638" ATT X"0009"
                MOVE ALL-X4E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1641" ATT X"0008"
                MOVE ALL-X5E TO ATT
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1649" ATT X"0008"
                MOVE ALL X"0E" TO OBS-ATTR
                CALL "CBL_WRITE_SCR_ATTRS" USING X"1700"
                                                 OBS-ATTR X"0050"
           END-IF

           MOVE 1 TO LIN-I LIN-T COL-I COL-T
           PERFORM 110-EXIBE-TELA THRU 110-99-FIM
           MOVE    TOPO             TO LIN-S
           MOVE    5                TO COL-S
           PERFORM 130-MONITOR    THRU 130-99-FIM
           PERFORM 870-ATUALIZA-OPERACOES THRU 870-99-FIM.

       800-99-FIM. EXIT.

       810-AJUSTA-TAMANHO.

           PERFORM VARYING SZ-FORMATOS
                     FROM LENGTH FORMATOS-REG BY -1
                    UNTIL FORMATOS-REG (SZ-FORMATOS: 1) NOT  = SPACE
                   CONTINUE
           END-PERFORM.

       810-99-FIM. EXIT.

       820-EXPORTAR.

           MOVE SPACES           TO TEXTO-REG
           IF   FORMATOS-TIPO-REG = "1"
                MOVE FORMATOS-OBJETOS    TO TEXTO-OBJETOS
                MOVE FORMATOS-PADRAO     TO TEXTO-PADRAO
                MOVE FORMATOS-PROVEDOR   TO TEXTO-PROVEDOR
                MOVE FORMATOS-TAMANHO    TO TEXTO-CAB
                IF   NOT ARQUIVO
                     MOVE SIZE-PAGE      TO TEXTO-SIZE-PAGE
                END-IF
           ELSE
                MOVE FORMATOS-OBJETO  TO TEXTO-OBJETO
                MOVE FORMATOS-TIPO    TO TEXTO-TIPO
                IF  NOT FORMATOS-EXPORT
                    MOVE FORMATOS-LINHA   TO TEXTO-LINHA
                    MOVE FORMATOS-COLUNA  TO TEXTO-COLUNA
                    MOVE FORMATOS-TAMANHO TO TEXTO-TAMANHO
                END-IF
           END-IF
           MOVE FORMATOS-STRING   TO TEXTO-STRING
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF TEXTO-REG
                                           OR  TEXTO-REG (I:) = SPACES
                   CALL "CWBINF" USING 'W' FS-TEXTO TEXTO-REG (I:1)
           END-PERFORM
           IF NOT CWUNIX-ON
              CALL "CWBINF" USING 'W' FS-TEXTO X'0D'
           END-IF
           CALL "CWBINF" USING 'W' FS-TEXTO X'0A'.

       820-99-FIM. EXIT.

       830-ACEITA-TELA.

           PERFORM 835-OBJETOS THRU 835-99-FIM
           DISPLAY CTAC-LIT-CWREL1
                   CTAC-VAR-CWREL1

           IF   INCLUINDO = 1
                MOVE 1 TO FORMATOS-PADRAO
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 23 COLUMN 15 WIDTH 10
                          CAPTION "f1-~Importar"
                          KEY F1 TAB-OFF
                END-EXEC
      *         ACCEPT  CTAC-VAR-CWREL1
                PERFORM TEST AFTER UNTIL (RELATORIO NOT = SPACES)
                             OR F1
                             OR ESC
                   ACCEPT T-RELATORIO
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   ESC
                        MOVE SPACES TO RELATORIO
                        EXIT PERFORM
                   ELSE
                       IF RELATORIO = SPACES
                          MOVE "Falta o c¢digo do relat¢rio"
                            TO CWSEND-MSG
                          CALL "CWSEND"
                           USING PARAMETROS-CWSEND
                          EXIT PERFORM CYCLE
                       END-IF
                   END-IF
                   IF   F1
                        IF   RELATORIO = SPACES OR ALL X"00"
                             MOVE 0 TO TECLA
                             EXIT PERFORM CYCLE
                        END-IF
                   END-IF
                   INSPECT RELATORIO CONVERTING MINUSCULAS
                                             TO MAIUSCULAS
                   INSPECT RELATORIO CONVERTING ACENTOS
                                             TO SEM-ACENTOS
                   DISPLAY SPACES LINE 23 COLUMN 03 WITH SIZE 27
      *            INITIALIZE            FORMATOS-REG
                   MOVE "1"       TO FORMATOS-TIPO-REG
                   MOVE RELATORIO TO FORMATOS-REPORT
                   READ FORMATOS
                   IF   FS-FORMATOS < "10"
                        PERFORM VARYING I FROM 7 BY -1 UNTIL I = 0
                                OR (RELATORIO (I:1) NOT = SPACE)
                                CONTINUE
                        END-PERFORM
                        MOVE SPACES TO CWSEND-MSG
                        STRING 'C¢digo "'
                               RELATORIO (1: I)
                               ' j  utilizado' DELIMITED BY SIZE
                          INTO CWSEND-MSG
                        CALL "CWSEND"  USING PARAMETROS-CWSEND
                        MOVE SPACES TO RELATORIO
                   ELSE
                        IF  F1
                            PERFORM 840-IMPORTAR THRU 840-99-FIM
                            IF INCLUINDO = 3
                               MOVE 2 TO CAMPO
                            END-IF
                        ELSE
      *                     PERFORM 835-OBJETOS THRU 835-99-FIM
                            PERFORM TEST AFTER
                                  UNTIL (FORMATOS-PROVEDOR NOT = SPACES)
                                       OR ESC
                                     ACCEPT T-RESTO
                                     ACCEPT TECLA FROM ESCAPE KEY
                                     IF (NOT ESC)
                                     AND FORMATOS-PROVEDOR = SPACES
                                         EXEC COBOLware Send
                                         SCREENS SPACES
                                      Message 'Provedor ‚ indispens vel'
                                         END-EXEC
                                     END-IF
                            END-PERFORM
                        END-IF
                   END-IF
                END-PERFORM
                IF  NOT ESC
                    EXEC COBOLware OBJECT DROP END-EXEC
                    PERFORM 835-OBJETOS THRU 835-99-FIM
                ELSE
                   DISPLAY T-RESTO
                END-IF
           ELSE
               DISPLAY T-RESTO
           END-IF
           IF ESC
              CLOSE FORMATOS
              GOBACK
           END-IF
           MOVE FORMATOS-PADRAO TO PADRAO
           IF ARQUIVO
              DISPLAY D-ARQUIVO
              ACCEPT  A-ARQUIVO
           ELSE
              DISPLAY D-RELATORIO
              ACCEPT  A-RELATORIO
           END-IF
           MOVE 1               TO CAMPO
           EXEC COBOLware OBJECT DROP END-EXEC
           ACCEPT TECLA FROM ESCAPE KEY
           IF   ESC
                CLOSE FORMATOS
                GOBACK
           END-IF

           MOVE SPACES TO LB-HELP
           STRING RELDIR     DELIMITED BY SPACE
                  "\"        DELIMITED BY SIZE
                  FORMATOS-PROVEDOR DELIMITED BY SPACE
                  ".HLP"     DELIMITED BY SIZE
                  INTO LB-HELP

           IF   CWUNIX-ON
                INSPECT LB-HELP CONVERTING "\" TO "/"
           END-IF

           OPEN INPUT HELP
           IF   FS-HELP < "10" OR FS-HELP = "9A"
                CLOSE HELP
           ELSE
                MOVE SPACES    TO LB-HELP
           END-IF.

       830-99-FIM. EXIT.

       835-OBJETOS.

           EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                CAPTION "Formato"
                LINE 10 COLUMN 16 HEIGHT 3 WIDTH 9
                ORDER 2 RETURN 1
                OPTION FORMATOS-PADRAO
                STRING-2-LENGTH 9
                STRING-1(1) "1" STRING-2(1) "COBOLware"
                STRING-1(2) "2" STRING-2(2) "Livre"
                STRING-1(3) "3" STRING-2(3) "Arquivo"
           END-EXEC
           EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                     CAPTION "~Provedores"
                     LINE 12 COLUMN 16 HEIGHT 09 WIDTH 38
                     PROGRAM "CWREL0"
                     OPTION FORMATOS-PROVEDOR ORDER 2 RETURN 1
                     STRING-1-LENGTH 08
                     STRING-2-LENGTH 30
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 23 COLUMN 03 WIDTH 11
                     CAPTION "[esc]-~Sa¡da"
                     KEY ESC TAB-OFF
           END-EXEC

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 23 COLUMN 27 WIDTH 4
                     CAPTION " ~OK" KEY F5 TAB-OFF
           END-EXEC.

       835-99-FIM. EXIT.


       840-IMPORTAR.

           DISPLAY RELATORIO LINE 23 COLUMN 18
           MOVE SPACES TO LB-TEXTO
           STRING CURDIR     DELIMITED BY SPACE
                  "\"        DELIMITED BY SIZE
                  RELATORIO  DELIMITED BY SPACE
                  ".rep"     DELIMITED BY SIZE
                  INTO LB-TEXTO

           IF   CWUNIX-ON
                INSPECT LB-TEXTO CONVERTING "\" TO "/"
           END-IF

           CALL "CWMSGW" USING "230327" "[esc]-Sa¡da                "
           CALL "CWMSGW" USING "180314" "Arquivo     : "
           CALL "CWMSGW" USING "182850" LB-TEXTO
           PERFORM TEST AFTER UNTIL FS-TEXTO = "00"
                                OR ESC
              SET CWPATH-WITH-DIR        TO TRUE
              SET CWPATH-WITH-DRIVES     TO TRUE
              SET CWPATH-WITHOUT-NEWDIR  TO TRUE
              SET CWPATH-WITHOUT-NEWFILE TO TRUE
              MOVE "_Importar_de:_"      TO CWPATH-TITLE
              MOVE SPACES                TO CWPATH-DEFAULT
              STRING RELATORIO DELIMITED BY SPACE
                        ".rep" DELIMITED BY SIZE
                INTO CWPATH-DEFAULT
              MOVE SPACES TO CWPATH-PATH
              STRING CURDIR     DELIMITED BY SPACE
                     "\*.rep"   DELIMITED BY SIZE
                     INTO CWPATH-PATH
              CALL "CWPATH" USING PARAMETROS-CWPATH
              CANCEL "CWPATH"
              IF   CWPATH-FILE = SPACES
                   close formatos
                   goback
                   DISPLAY "Importando..." AT 2303
                   ACCEPT LB-TEXTO LINE 18 COLUMN 17 WITH UPDATE
              ELSE
                   MOVE CWPATH-FILE TO LB-TEXTO
                   DISPLAY LB-TEXTO LINE 18 COLUMN 17 WITH SIZE 50
              END-IF
              IF   CWUNIX-ON
                   INSPECT LB-TEXTO CONVERTING "\" TO "/"
              END-IF
              ACCEPT TECLA FROM ESCAPE KEY
              IF  ESC
                  CLOSE FORMATOS
                  GOBACK
              END-IF
              CALL "CWBINF" USING 'I' FS-TEXTO LB-TEXTO
              IF   FS-TEXTO = "30" OR "35"
                   MOVE SPACES TO CWSEND-MSG
                   STRING "Arquivo: "      DELIMITED BY SIZE
                           LB-TEXTO        DELIMITED BY SPACE
                           " nÆo existe !" DELIMITED BY SIZE
                                      INTO CWSEND-MSG
                   CALL "CWSEND" USING PARAMETROS-CWSEND
              END-IF
           END-PERFORM

           DISPLAY RODAPE
           MOVE SPACES TO PERC-TIT
           STRING "Importando " DELIMITED BY SIZE
                  RELATORIO     DELIMITED BY SPACE
                  "..."         DELIMITED BY SIZE
                           INTO PERC-TIT
           PERFORM 845-LER-TEXTO THRU 845-99-FIM
           MOVE SPACES           TO FORMATOS-REG
           INITIALIZE        FORMATOS-REG
           MOVE "1"              TO FORMATOS-TIPO-REG
           MOVE RELATORIO        TO FORMATOS-REPORT
           MOVE TEXTO-OBJETOS    TO FORMATOS-OBJETOS  OBJETO-L
           MOVE TEXTO-PADRAO     TO FORMATOS-PADRAO   PADRAO
           MOVE TEXTO-STRING     TO FORMATOS-STRING
           MOVE TEXTO-PROVEDOR   TO FORMATOS-PROVEDOR
           MOVE TEXTO-CAB        TO FORMATOS-TAMANHO
           MOVE 3                TO INCLUINDO
           CALL "CWMSGW" USING "180370" ESPACOS
           PERFORM 835-OBJETOS THRU 835-99-FIM
           DISPLAY CTAC-VAR-CWREL1
           IF ARQUIVO
              MOVE FORMATOS-ARQUIVO TO NOME-ARQUIVO
              MOVE FORMATOS-DESTINO TO DESTINO
              MOVE FORMATOS-DELIM   TO DELIM
              DISPLAY D-ARQUIVO
           ELSE
              MOVE TEXTO-SIZE-PAGE TO FORMATOS-SIZE-PAGE SIZE-PAGE
              DISPLAY D-RELATORIO
           END-IF
           PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
           WRITE FORMATOS-REG
           MOVE 0 TO SEQ
           PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                   MOVE SPACES TO TEXTO-REG
                   PERFORM 845-LER-TEXTO THRU 845-99-FIM
                        IF   FS-TEXTO < '10'
                             MOVE "2"           TO FORMATOS-REG
                             MOVE RELATORIO     TO FORMATOS-REPORT
                             MOVE TEXTO-OBJETO  TO FORMATOS-OBJETO
                             MOVE TEXTO-TIPO    TO FORMATOS-TIPO
                             IF  NOT FORMATOS-EXPORT
                                 MOVE TEXTO-LINHA   TO FORMATOS-LINHA
                                 MOVE TEXTO-COLUNA  TO FORMATOS-COLUNA
                                 MOVE TEXTO-TAMANHO TO FORMATOS-TAMANHO
                             END-IF
                             MOVE TEXTO-STRING  TO FORMATOS-STRING
                             PERFORM 810-AJUSTA-TAMANHO
                                THRU 810-99-FIM
                             WRITE FORMATOS-REG
      *                      UNLOCK FORMATOS
                        END-IF
                        EXEC COBOLware PROCESS SHOW
                             HEADER PERC-TIT
                             LINE 10 COLUMN 10
                             EMPTY OBJETO-L
                             FULL  FORMATOS-OBJETO
                        END-EXEC
           END-PERFORM

           EXEC COBOLware PROCESS CLOSE END-EXEC
           CALL "CWBINF" USING 'C' FS-TEXTO
           DISPLAY RODAPE
           MOVE SPACES           TO FORMATOS-REG
           INITIALIZE        FORMATOS-REG
           MOVE "1"            TO FORMATOS-TIPO-REG
           MOVE RELATORIO      TO FORMATOS-REPORT
           READ FORMATOS.

       840-99-FIM. EXIT.

       845-LER-TEXTO.

           MOVE SPACES TO TEXTO-REG
           MOVE 0      TO I
           PERFORM UNTIL FS-TEXTO > '09'
                   CALL "CWBINF" USING 'R' FS-TEXTO TEXTO-BYTE
                   IF  FS-TEXTO < '10'
                   AND(TEXTO-BYTE NOT = X'0D')
                       IF TEXTO-BYTE = X'0A'
                          EXIT PERFORM
                       END-IF
                       IF  I < LENGTH OF TEXTO-REG
                           ADD 1 TO I
                           MOVE TEXTO-BYTE TO TEXTO-REG (I: 1)
                       END-IF
                   END-IF
           END-PERFORM.

       845-99-FIM. EXIT.

       850-ATUALIZA-PROMPTS.

           MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
           MOVE "Prompt-"  TO CAMPOS-DATANAME
           MOVE RELATORIO  TO CAMPOS-RELATORIO
           START CAMPOS KEY NOT LESS CAMPOS-PRONAMREL
           PERFORM TEST AFTER
                   UNTIL FS-CAMPOS > "09"
                      OR (CAMPOS-DATANAME (1: 7) NOT = "Prompt-")
                      OR (CAMPOS-PROVEDOR NOT = FORMATOS-PROVEDOR)
                   READ CAMPOS NEXT RECORD
                    NOT AT END
                        IF   CAMPOS-DATANAME (1: 7) = "Prompt-"
                        AND  CAMPOS-PROVEDOR       = FORMATOS-PROVEDOR
                        AND  CAMPOS-RELATORIO      = RELATORIO
                             DELETE CAMPOS RECORD
                        END-IF
                   END-READ
           END-PERFORM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99 *>nena
                   IF   PROMPT-DATANAME (I) NOT = SPACES
                        MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                        MOVE 9999     TO CAMPOS-COLUNA
                        START CAMPOS KEY NOT > CAMPOS-CHAVE
                        READ CAMPOS PREVIOUS RECORD
                        MOVE CAMPOS-COLUNA      TO COLUNAS
                        IF   PROMPT-COLUNA (I) (1: 3) = "(=)"
                             PERFORM VARYING O FROM 1 BY 1 UNTIL O > 100
                                     OR OPERACAO-DATANAME (O) = SPACE
                                     OR OPERACAO-DATANAME (O)
                                      = PROMPT-COLUNA (I) (4: 9)
                             END-PERFORM
                             IF   OPERACAO-DATANAME (O)
                                = PROMPT-COLUNA (I) (4: 9)
                                COMPUTE CAMPOS-TAMANHO = OPERACAO-INT(O)
                                                       + OPERACAO-DEC(O)
                                MOVE OPERACAO-DEC (O) TO CAMPOS-DEC
                                MOVE "V"              TO CAMPOS-TIPO
                             END-IF
                        ELSE
                             MOVE PROMPT-COLUNA (I) TO CAMPOS-DATANAME
                             MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                             MOVE SPACES            TO CAMPOS-RELATORIO
                             READ CAMPOS KEY IS CAMPOS-PRONAMREL
                        END-IF
                        MOVE "Prompt-"           TO CAMPOS-DATANAME
                        MOVE PROMPT-DATANAME (I) TO CAMPOS-DATANAME
                                                    (8: )
                        MOVE RELATORIO          TO CAMPOS-RELATORIO
                        PERFORM 860-GRAVA-COLUNA THRU 860-99-FIM
                   END-IF
           END-PERFORM.

       850-99-FIM. EXIT.

       860-GRAVA-COLUNA.

           ADD   1                 TO COLUNAS
           MOVE  COLUNAS           TO CAMPOS-COLUNA
           MOVE  FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
           MOVE  RELATORIO         TO CAMPOS-RELATORIO
           MOVE  SPACES            TO CAMPOS-MASCARA
           IF   CAMPOS-DATANAME (1:3) = "(=)"
                MOVE CAMPOS-COLUNA TO OPERACAO-COLUNA (Y)
           END-IF
           WRITE CAMPOS-REG
           PERFORM VARYING MAX-NAME
                      FROM LENAME BY -1
                     UNTIL MAX-NAME = MAIOR-NOME
                        OR MAX-NAME = 1
                        OR CAMPOS-DATANAME(MAX-NAME: 1)
                           NOT = SPACE
                   CONTINUE
           END-PERFORM
           IF  MAX-NAME > MAIOR-NOME
               MOVE MAX-NAME TO MAIOR-NOME
           END-IF.

       860-99-FIM. EXIT.

       870-ATUALIZA-OPERACOES.

           MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
           MOVE "(=)"    TO CAMPOS-DATANAME
           MOVE SPACES   TO CAMPOS-RELATORIO
           START CAMPOS       KEY NOT LESS CAMPOS-PRONAMREL
           PERFORM TEST AFTER
                   UNTIL FS-CAMPOS > "09"
                      OR (CAMPOS-DATANAME (1: 3) NOT = "(=)")
                      OR (CAMPOS-PROVEDOR NOT = FORMATOS-PROVEDOR)
                   READ CAMPOS NEXT RECORD
                    NOT AT END
                        IF   CAMPOS-DATANAME (1: 3) = "(=)"
                        AND  CAMPOS-PROVEDOR  = FORMATOS-PROVEDOR
                        AND  CAMPOS-RELATORIO = RELATORIO
                             DELETE CAMPOS RECORD
                        END-IF
                   END-READ
           END-PERFORM
           IF   WORK-OPERACOES = 1
                MOVE I TO FIM-OP
           ELSE
                MOVE 100 TO FIM-OP
                MOVE 0   TO NUM-OP
           END-IF
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = FIM-OP
                   IF   OPERACAO-DATANAME (Y) NOT = SPACES
                        MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                        MOVE 9999          TO CAMPOS-COLUNA
                        START CAMPOS KEY NOT > CAMPOS-CHAVE
                        READ CAMPOS PREVIOUS RECORD
                        MOVE CAMPOS-COLUNA TO COLUNAS
                        MOVE 0             TO CAMPOS-POSIT
                        IF   WORK-OPERACOES = 0
                             ADD  1      TO NUM-OP
                             MOVE NUM-OP TO CAMPOS-POSIT
                        END-IF
                        COMPUTE CAMPOS-TAMANHO = OPERACAO-INT (Y) +
                                                 OPERACAO-DEC (Y)
                        MOVE OPERACAO-DEC (Y)      TO CAMPOS-DEC
                        MOVE "V"                   TO CAMPOS-TIPO
                        MOVE FORMATOS-PROVEDOR     TO CAMPOS-PROVEDOR
                        MOVE "(=)"                 TO CAMPOS-DATANAME
                        MOVE OPERACAO-DATANAME (Y) TO CAMPOS-DATANAME
                                                    (4: )
                        COMPUTE OPERACAO-COLUNA (Y) = COLUNAS + 1
                        MOVE SPACE                 TO CAMPOS-ACUMULADOR
                        MOVE RELATORIO             TO CAMPOS-RELATORIO
                        PERFORM 860-GRAVA-COLUNA THRU 860-99-FIM
                        MOVE "+"                   TO CAMPOS-ACUMULADOR
                        MOVE CAMPOS-DATANAME       TO DATANAME
                        MOVE SPACES                TO CAMPOS-DATANAME
                        IF   WORK-OPERACOES = 0
                             ADD    4   TO CAMPOS-TAMANHO
                             IF  CAMPOS-TAMANHO > 18
                                 MOVE 18 TO CAMPOS-TAMANHO
                             END-IF
                             STRING DATANAME DELIMITED BY SPACE
                                       "[+]" DELIMITED BY SIZE
                                        INTO CAMPOS-DATANAME
                             MOVE    0                  TO CAMPOS-POSIT
                             PERFORM 860-GRAVA-COLUNA THRU 860-99-FIM
                        END-IF
                   END-IF
           END-PERFORM.

       870-99-FIM. EXIT.

       900-FINAIS.

           IF  DELETAR-CABS = 1
               DELETE FILE CABS
           END-IF

           CANCEL "CWREL7"
           MOVE 1 TO OBJETO-M

           IF   CWSEND-OPTION = 2
                MOVE SPACES TO CWSEND-MSG
                PERFORM VARYING I FROM LENGTH RELATORIO BY -1
                                   UNTIL RELATORIO (I: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                MOVE 2           TO CWSEND-OPTION
                MOVE SPACES      TO CWSEND-SCREENS
                MOVE "__~Sim__"  TO CWSEND-SCREEN (1)
                MOVE "__~NÆo__"  TO CWSEND-SCREEN (2)
                STRING "Apagar relat¢rio " DELIMITED BY SIZE
                       RELATORIO (1: I)    DELIMITED BY SIZE
      *                X"003F00"           DELIMITED BY SIZE
                       INTO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 1
                     MOVE SPACES TO CWSEND-MSG
                     STRING "Confirme: "          DELIMITED BY SIZE
                                RELATORIO (1: I)  DELIMITED BY SIZE
                                " ser  apagado !" DELIMITED BY SIZE
                                INTO CWSEND-MSG
                     MOVE 2            TO CWSEND-OPTION
                     MOVE SPACES       TO CWSEND-SCREENS
                     MOVE "~Confirmar" TO CWSEND-SCREEN (1)
                     MOVE "~Abandona"  TO CWSEND-SCREEN (2)
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     IF   CWSEND-OPTION = 1
                          MOVE 9 TO CWSEND-OPTION
                     END-IF
                END-IF
           END-IF
           IF   CWSEND-OPTION = 1 OR 9
                MOVE CAB               TO FORMATOS-TAMANHO
                MOVE MAPA-STATUS-LINHA (1:100) TO FORMATOS-MASCARA
      *         MOVE MAPA-FLAGS (90: 11) TO FORMATOS-FLAGS
                MOVE OBJETO-R          TO FORMATOS-OBJETOS
                ADD  TAGS              TO FORMATOS-OBJETOS
                IF   ARQUIVO
                     MOVE NOME-ARQUIVO TO FORMATOS-ARQUIVO
                     MOVE DESTINO      TO FORMATOS-DESTINO
                     MOVE DELIM        TO FORMATOS-DELIM
                ELSE
                     MOVE  MAPA-FLAGS     TO FORMATOS-MASCARA(101: 50)
                     MOVE  MAPA-FLAGS (51: 50) TO FORMATOS-FLAGS
                     MOVE SIZE-PAGE    TO FORMATOS-SIZE-PAGE
                END-IF
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                REWRITE FORMATOS-REG
                MOVE "2"           TO FORMATOS-TIPO-REG
                MOVE "S"           TO VAZIO
                MOVE FORMATOS-PROVEDOR TO CAMPOS-PROVEDOR
                MOVE SPACES    TO CAMPOS-DATANAME
                MOVE RELATORIO TO CAMPOS-RELATORIO
                START CAMPOS KEY NOT LESS  CAMPOS-PRONAMREL
                PERFORM TEST AFTER
                       UNTIL FS-CAMPOS > "09"
                          OR (CAMPOS-PROVEDOR NOT = FORMATOS-PROVEDOR)
                        CALL "CWATCH"
                        READ CAMPOS NEXT RECORD
                        IF   FS-CAMPOS < "10"
                        AND  CAMPOS-RELATORIO = RELATORIO
                             DELETE CAMPOS RECORD
                        END-IF
                END-PERFORM
                DISPLAY RODAPE
                MOVE SPACES TO PERC-TIT
                STRING "Apagando " DELIMITED BY SIZE
                       RELATORIO   DELIMITED BY SPACE
                       "..."       DELIMITED BY SIZE
                           INTO PERC-TIT
                START FORMATOS KEY NOT LESS FORMATOS-CHAVE
                MOVE 0        TO OBJETO-S
                MOVE OBJETO-V TO OBJETO-S
                PERFORM TEST AFTER
                             UNTIL FS-FORMATOS > "09"
                                OR (FORMATOS-REPORT   NOT = RELATORIO)
                                OR (FORMATOS-TIPO-REG NOT = "2")
                        CALL "CWATCH"
                        READ FORMATOS NEXT RECORD
                        IF   FS-FORMATOS < "10"
                        AND  FORMATOS-REPORT = RELATORIO
                        AND  FORMATOS-TIPO-REG = "2"
                             DELETE FORMATOS RECORD
                             SUBTRACT 1     FROM OBJETO-S
                             EXEC COBOLware PROCESS SHOW
                                  HEADER PERC-TIT
                                  LINE 10 COLUMN 10
                                  EMPTY OBJETO-V
                                  FULL  OBJETO-S
                             END-EXEC
                        END-IF
                END-PERFORM
                IF   CWSEND-OPTION NOT = 9
                     MOVE SPACES TO PERC-TIT
                     STRING "Salvando " DELIMITED BY SIZE
                            RELATORIO   DELIMITED BY SPACE
                            "..."       DELIMITED BY SIZE
                                INTO PERC-TIT
                END-IF
                SORT OBJETO-ATTR ON ASCENDING KEY OBJETO-RL
                                                  OBJETO-RC
                PERFORM VARYING OBJETO-S FROM 1 BY 1
                          UNTIL OBJETO-S > OBJETO-R
                             OR CWSEND-OPTION = 9
                    EXEC COBOLware PROCESS SHOW
                         HEADER PERC-TIT
                         LINE 10 COLUMN 10
                         EMPTY OBJETO-R
                         FULL  OBJETO-S
                    END-EXEC
                    IF OBJETO-TAMANHO (OBJETO-S) NOT = 0
                       ADD  1                        TO OBJETO-M
                       MOVE "2"                      TO FORMATOS-REG
                       MOVE RELATORIO                TO FORMATOS-REPORT
                       MOVE OBJETO-M                 TO FORMATOS-OBJETO
                       MOVE OBJETO-RL     (OBJETO-S) TO FORMATOS-LINHA
                       MOVE OBJETO-RC     (OBJETO-S) TO FORMATOS-COLUNA
                       MOVE OBJETO-TAMANHO(OBJETO-S) TO FORMATOS-TAMANHO
                       MOVE OBJETO-TIPO   (OBJETO-S) TO FORMATOS-TIPO
                       IF  NOT FORMATOS-LITERAL
                           MOVE OBJETO-DATANAME (OBJETO-S)
                             TO FORMATOS-DATANAME
                           IF  FORMATOS-EDITADO
                               MOVE MAPA-LIN (FORMATOS-LINHA)
                                    (FORMATOS-COLUNA: FORMATOS-TAMANHO)
                                 TO FORMATOS-MASCARA
                           END-IF
                           IF FORMATOS-RESULTADO
                              MOVE OBJETO-DATANAME2 (OBJETO-S)
                                TO FORMATOS-DATANAME2
                           END-IF
                       ELSE
                           MOVE MAPA-LIN (FORMATOS-LINHA)
                                (FORMATOS-COLUNA: FORMATOS-TAMANHO)
                             TO FORMATOS-STRING
                           INSPECT FORMATOS-STRING
                                   CONVERTING X"00" TO SPACE
                           PERFORM VARYING FORMATOS-TAMANHO
                                      FROM FORMATOS-TAMANHO BY -1
                                     UNTIL FORMATOS-TAMANHO = 1
                                        OR FORMATOS-STRING
                                       (FORMATOS-TAMANHO: 1) NOT = SPACE
                                   CONTINUE
                           END-PERFORM
                       END-IF
      *                IF  (FORMATOS-LITERAL
      *                AND  FORMATOS-STRING = SPACES)
                       IF  (FORMATOS-EDITADO
                       AND  FORMATOS-MASCARA = SPACES)
                       OR   FORMATOS-TAMANHO = 0
                            CONTINUE
                       ELSE
                            MOVE "N"           TO VAZIO
                            PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                            WRITE FORMATOS-REG
                       END-IF
                    END-IF
                END-PERFORM
                PERFORM 910-VERIFICA-QUEBRA  THRU 910-99-FIM
                        VARYING OBJETO-S FROM 1 BY 1
                          UNTIL OBJETO-S > 100
                             OR CWSEND-OPTION = 9
                PERFORM 920-VERIFICA-PROMPTS THRU 920-99-FIM
                        VARYING I FROM 1 BY 1
                          UNTIL I > 99
                             OR CWSEND-OPTION = 9
                PERFORM 930-VERIFICA-CONDICOES THRU 930-99-FIM
                        VARYING I FROM 1 BY 1
                          UNTIL I > 99
                             OR CWSEND-OPTION = 9
                PERFORM 940-VERIFICA-OPERACOES THRU 940-99-FIM
                        VARYING I FROM 1 BY 1
                          UNTIL I > 99
                             OR CWSEND-OPTION = 9
                IF ARQUIVO
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > tags
                             OR CWSEND-OPTION = 9
                           MOVE "N"               TO VAZIO
                           MOVE "2"               TO FORMATOS-REG
                           MOVE RELATORIO         TO FORMATOS-REPORT
                           ADD  1                 TO OBJETO-M
                           MOVE OBJETO-M          TO FORMATOS-OBJETO
                           MOVE TAG (I) (6:)      TO FORMATOS-STRING
                           MOVE "3"               TO FORMATOS-TIPO
                           PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                           WRITE FORMATOS-REG
                   END-PERFORM
                END-IF
                IF   VAZIO = "S"
                     MOVE SPACES    TO FORMATOS-REG
                     INITIALIZE        FORMATOS-REG
                     MOVE "1"       TO FORMATOS-TIPO-REG
                     MOVE RELATORIO TO FORMATOS-REPORT
                     READ FORMATOS
                     IF   FS-FORMATOS < "10"
                          DELETE FORMATOS RECORD
                     END-IF
                END-IF
                EXEC COBOLware PROCESS CLOSE END-EXEC
           END-IF

           IF   CWUNIX-GUI
                CALL "CWKBDC" USING "9898"
           END-IF

           CANCEL "CWREL9"
           CLOSE FORMATOS CAMPOS.

       900-99-FIM. EXIT.

       910-VERIFICA-QUEBRA.

           IF   OBJETO-S = 1
           AND (COLUNAS-ORDENADAS NOT = 0)
                MOVE SPACES                  TO FORMATOS-REG
                INITIALIZE                      FORMATOS-REG
                MOVE "2"                     TO FORMATOS-TIPO-REG
                MOVE RELATORIO               TO FORMATOS-REPORT
                MOVE 1                       TO FORMATOS-OBJETO
                MOVE MAPA-ORDENACAO          TO FORMATOS-STRING
                MOVE "O"                     TO FORMATOS-TIPO
                MOVE "N"                     TO VAZIO
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                WRITE FORMATOS-REG
           END-IF

           IF  (NIVEL-QUEBRA (OBJETO-S) NOT = 0)
           AND (NOT ARQUIVO)
                ADD  1                       TO OBJETO-M
                MOVE SPACES                  TO FORMATOS-REG
                INITIALIZE                      FORMATOS-REG
                MOVE "2"                     TO FORMATOS-TIPO-REG
                MOVE RELATORIO               TO FORMATOS-REPORT
                MOVE OBJETO-M                TO FORMATOS-OBJETO
                MOVE OBJETO-S                TO FORMATOS-LINHA
                MOVE NIVEL-QUEBRA (OBJETO-S) TO FORMATOS-COLUNA
                MOVE "Q"                     TO FORMATOS-TIPO
                MOVE "N"                     TO VAZIO
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                WRITE FORMATOS-REG
           END-IF.

       910-99-FIM. EXIT.

       920-VERIFICA-PROMPTS.

           IF   PROMPT-DATANAME (I) NOT = SPACES
                ADD  1                       TO OBJETO-M
                MOVE SPACES                  TO FORMATOS-REG
                INITIALIZE                      FORMATOS-REG
                MOVE "2"                     TO FORMATOS-TIPO-REG
                MOVE RELATORIO               TO FORMATOS-REPORT
                MOVE OBJETO-M                TO FORMATOS-OBJETO
                MOVE I                       TO FORMATOS-LINHA
                MOVE PROMPT-ATRIBUTOS (I)    TO FORMATOS-MASCARA
                MOVE "P"                     TO FORMATOS-TIPO
                MOVE "N"                     TO VAZIO
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                WRITE FORMATOS-REG
           END-IF.

       920-99-FIM. EXIT.

       930-VERIFICA-CONDICOES.

           IF   CONDICAO-DATANAME (I) NOT = SPACES
                ADD  1                       TO OBJETO-M
                MOVE SPACES                  TO FORMATOS-REG
                INITIALIZE                      FORMATOS-REG
                MOVE "2"                     TO FORMATOS-TIPO-REG
                MOVE RELATORIO               TO FORMATOS-REPORT
                MOVE OBJETO-M                TO FORMATOS-OBJETO
                MOVE I                       TO FORMATOS-LINHA
                MOVE CONDICAO-ATRIBUTOS (I)  TO FORMATOS-MASCARA
                MOVE "S"                     TO FORMATOS-TIPO
                MOVE "N"                     TO VAZIO
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                WRITE FORMATOS-REG
           END-IF.

       930-99-FIM. EXIT.

       940-VERIFICA-OPERACOES.

           IF   OPERACAO-DATANAME (I) NOT = SPACES
                ADD  1                       TO OBJETO-M
                MOVE SPACES                  TO FORMATOS-REG
                INITIALIZE                      FORMATOS-REG
                MOVE "2"                     TO FORMATOS-TIPO-REG
                MOVE RELATORIO               TO FORMATOS-REPORT
                MOVE OBJETO-M                TO FORMATOS-OBJETO
                MOVE I                       TO FORMATOS-LINHA
                MOVE OPERACAO-ATRIBUTOS (I)  TO FORMATOS-MASCARA
                MOVE "C"                     TO FORMATOS-TIPO
                MOVE "N"                     TO VAZIO
                PERFORM 810-AJUSTA-TAMANHO THRU 810-99-FIM
                WRITE FORMATOS-REG
           END-IF.

       940-99-FIM. EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UNIX.

       WORKING-STORAGE SECTION.
       01  AREAS-DE-TRABALHO-1.
           05 C                       PIC  9(003) VALUE 0.
           05 I                COMP-X PIC  9(004) VALUE 0.

       LINKAGE SECTION.

       01  ATTRIBUTE-BUFFER    PIC  X(078).
       01  REDEFINES ATTRIBUTE-BUFFER.
           05 COR-PC OCCURS 78 PIC  9(002) COMP-X.
       01  STRING-LENGTH       PIC  9(004) COMP-X.
       01  TABELA-CORES.
           10 COR              PIC X(001) OCCURS 256.

       PROCEDURE DIVISION USING ATTRIBUTE-BUFFER
                                STRING-LENGTH
                                TABELA-CORES.

       INICIO.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH
                   COMPUTE C = COR-PC (I) + 1
                   MOVE COR (C) TO ATTRIBUTE-BUFFER (I: 1)
           END-PERFORM.

       END PROGRAM UNIX.
       END PROGRAM CWREL1.
