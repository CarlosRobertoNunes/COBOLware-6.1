       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/04/1996.
       SECURITY.      *************************************************
                      *                                               *
                      * Configura tabela de cores, molduras e acentos *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. CRT STATUS    IS CRT-STATUS
                      DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BOXCOLOR ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  RELATIVE KEY  IS RK-BOXCOLOR
                  ACCESS MODE   IS RANDOM
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-BOXCOLOR.

       DATA DIVISION.
       FILE SECTION.

       FD  BOXCOLOR
           VALUE OF FILE-ID LB-BOXCOLOR.

       01  BOXCOLOR-REG PIC X(256).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CONTA-LOOP              PIC  9(003) VALUE 0.
           05 CRT-STATUS.
              10 KEY-1                PIC 9(001).
              10 KEY-2 COMP-X         PIC 9(002).
              10 KEY-3 COMP-X         PIC 9(002).
           05 ER-BOXCOLOR.
              10 FS-BOXCOLOR          PIC X(002) VALUE "00".
              10 LB-BOXCOLOR          PIC X(255) VALUE SPACES.
           05 COBWARE                 PIC X(030) VALUE SPACES.
           05 TERM                    PIC X(030) VALUE SPACES.
           05 KEY-STATUS              PIC 9(002) COMP-X VALUE 0.
           05 BYTE-ANTES              PIC X(001).
           05 BYTE-DEPOIS             PIC X(001).
           05 SCREEN-POSITION-A       PIC X(002).
           05 PROIBIDOS VALUE SPACES.
              10 PROIBIDO             PIC X(001)  OCCURS 255.
           05 SALVA-TELA.
              10 SCREEN-POSITION-2.
                 15                   PIC 9(002) COMP-X VALUE 0.
                 15                   PIC 9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER-2    PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER-2   PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH-2      PIC 9(004) COMP-X VALUE 2000.
           05 SALVA-TELA-3.
              10 SCREEN-POSITION-3.
                 15 L3                PIC 9(002) COMP-X VALUE 0.
                 15 C3                PIC 9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER-3    PIC X(005) VALUE SPACES.
              10 REDEFINES CARACTER-BUFFER-3.
                 15 GIRO              PIC X(001).
                 15                   PIC X(004).
              10 ATTRIBUTE-BUFFER-3   PIC X(005) VALUE SPACES.
           05 SALVA-CHAR-1            PIC X(0001) VALUE SPACE.
           05 SALVA-CHAR-2            PIC X(0001) VALUE SPACE.
           05 M                       PIC 9(001) VALUE 0.
           05 K                       PIC 9(002) VALUE 0.
           05 AX                      PIC X(001) VALUE SPACE.
      *    05 VALUE "--  <>  []  {}  ()  ||  )(  }{  ><  ][  ".
           05 VALUE "--<>||><".
              10 KS OCCURS 4.
                 15 K1 PIC X.
                 15 K2 PIC X.
           05 TIPO-MOLDURA            PIC 9(001) VALUE 0.
           05 TIPO-CARACTER           PIC 9(001) VALUE 0.
             05 RESPOSTA                PIC X(001) VALUE SPACE.
           05 CARACTER                PIC X(001) VALUE SPACE.
           05 COR-A                   PIC 9(003) VALUE 0.
           05 COR-C REDEFINES COR-A.
              10 FILLER               PIC X(001).
              10 COR-B                PIC 9(002).
           05 TECLA                   PIC 9(003) VALUE 0. COPY CWEDIT.
           05 OPCAO                   PIC 9(001) VALUE 0.
           05 TRAVADO                 PIC X(002) VALUE "9D".
           05 FL-SAVE1                PIC X(001) VALUE "0".
           05 FL-SAVE2                PIC X(001) VALUE "0".
           05 FL-SAVE3                PIC X(001) VALUE "0".
           05 RK-BOXCOLOR             PIC 9(002) VALUE 1.
           05 I                       PIC 9(003) VALUE 0.
           05 I-999                   PIC 9(003) VALUE 0.
           05 X.
              10 I-X                  PIC 9(02) COMP-X.
           05 L                       PIC 9(02) VALUE 1.
           05 C                       PIC 9(02) VALUE 16.
           05 CURPOS.
              10 L2                   PIC 9(02) VALUE 0.
              10 C2                   PIC 9(02) VALUE 0.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC 9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC 9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(003) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(003) VALUE SPACES.
              10 STRING-LENGTH        PIC 9(004) COMP-X VALUE 3.
           05 Y                       PIC 9(003) VALUE 0.
           05 ACENTOS                            VALUE SPACES.
              10 AC OCCURS 51         PIC X(001).
           05 ACENTOS-SAVE            PIC X(051) VALUE SPACES.
           05 CORES VALUE SPACES.
              10 COR OCCURS 257       PIC 9(002) COMP-X.
           05 CORES-SAVE              PIC X(256) VALUE SPACES.
           05 POSICOES VALUE ZEROS.
              10 POSICAO OCCURS 256.
                 15 L-P PIC 9(2).
                 15 C-P PIC 9(2).
           05 MOLDURAS-SAVE            PIC  X(072) VALUE SPACES.
           05 MOLDURAS.
              10 FILLER PIC X(8) VALUE X"C9CDBBBACCB9C8BC".
              10 FILLER PIC X(8) VALUE X"DAC4BFB3C3B4C0D9".
              10 FILLER PIC X(8) VALUE X"D5CDB8B3C6B5D4BE".
              10 FILLER PIC X(8) VALUE X"D6C4B7BAC7B6D3BD".
              10 FILLER PIC X(8) VALUE ALL X"B0".
              10 FILLER PIC X(8) VALUE ALL X"B1".
              10 FILLER PIC X(8) VALUE ALL X"B2".
              10 FILLER PIC X(8) VALUE "---|----".
              10 FILLER PIC X(8) VALUE "        ".
           05 FILLER REDEFINES MOLDURAS.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 DESENHO VALUE SPACES.
              10 MOLDURA OCCURS 8 TIMES.
                 15 M-TOPO PIC X(013).
                 15 M-LADO PIC X(001).
                 15 M-MEIO PIC X(013).
                 15 M-BASE PIC X(013).
           05 REDEFINES DESENHO.
              10 M0-TOPO PIC X(013).
              10 M0-LADO PIC X(001).
              10 M0-MEIO PIC X(013).
              10 M0-BASE PIC X(013).
              10 M1-TOPO PIC X(013).
              10 M1-LADO PIC X(001).
              10 M1-MEIO PIC X(013).
              10 M1-BASE PIC X(013).
              10 M2-TOPO PIC X(013).
              10 M2-LADO PIC X(001).
              10 M2-MEIO PIC X(013).
              10 M2-BASE PIC X(013).
              10 M3-TOPO PIC X(013).
              10 M3-LADO PIC X(001).
              10 M3-MEIO PIC X(013).
              10 M3-BASE PIC X(013).
              10 M4-TOPO PIC X(013).
              10 M4-LADO PIC X(001).
              10 M4-MEIO PIC X(013).
              10 M4-BASE PIC X(013).
              10 M5-TOPO PIC X(013).
              10 M5-LADO PIC X(001).
              10 M5-MEIO PIC X(013).
              10 M5-BASE PIC X(013).
              10 M6-TOPO PIC X(013).
              10 M6-LADO PIC X(001).
              10 M6-MEIO PIC X(013).
              10 M6-BASE PIC X(013).
              10 M7-TOPO PIC X(013).
              10 M7-LADO PIC X(001).
              10 M7-MEIO PIC X(013).
              10 M7-BASE PIC X(013).
              10 M8-TOPO PIC X(013).
              10 M8-LADO PIC X(001).
              10 M8-MEIO PIC X(013).
              10 M8-BASE PIC X(013).
       COPY CWUNIX.
      *01  CWUNIX-STATUS PIC X(001).
      *    88 CWUNIX-ON  VALUE "1".
      *    88 CWUNIX-OFF VALUE "0".

       SCREEN SECTION.

       01  CTAC-LIT-MFX.
           03 FOREGROUND-COLOR 1 HIGH.
           05 LINE 01 COLUMN 06 VALUE "Tipo/Fundo Preto   M".
           05 LINE 01 COLUMN 26 VALUE "arinho Verde   Azul ".
           05 LINE 01 COLUMN 46 VALUE "   Rubro   Rosa    A".
           05 LINE 01 COLUMN 66 VALUE "marelo Branco".
           05 LINE 02 COLUMN 01 VALUE "Low  Preto".
           05 LINE 03 COLUMN 06 VALUE "Marinho".
           05 LINE 04 COLUMN 06 VALUE "Verde".
           05 LINE 05 COLUMN 06 VALUE "Azul".
           05 LINE 06 COLUMN 06 VALUE "Rubro".
           05 LINE 07 COLUMN 06 VALUE "Rosa".
           05 LINE 08 COLUMN 06 VALUE "Amarelo".
           05 LINE 09 COLUMN 06 VALUE "Branco".
           05 LINE 10 COLUMN 01 VALUE "High Preto".
           05 LINE 11 COLUMN 06 VALUE "Marinho".
           05 LINE 12 COLUMN 06 VALUE "Verde".
           05 LINE 13 COLUMN 06 VALUE "Azul".
           05 LINE 14 COLUMN 06 VALUE "Vermelho".
           05 LINE 15 COLUMN 06 VALUE "Rosa".
           05 LINE 16 COLUMN 06 VALUE "Amarelo".
           05 LINE 17 COLUMN 06 VALUE "Branco".
           05 LINE 23 COLUMN 03 VALUE "COBOLware 6.1 Config".
           05 LINE 23 COLUMN 23 VALUE "uracao de cores".
           05 LINE 24 COLUMN 03 VALUE "Copyright (C) 1984-2020 C".
           05 LINE 24 COLUMN 28 VALUE "OBOLware Services Ltda.".
           05 LINE 24 COLUMN 67 VALUE "[esc] Encerra".
           05 FOREGROUND-COLOR 4 HIGH.
              06 LINE 23 COLUMN 08 "ware".
              06 LINE 24 COLUMN 32 "ware".
           05 FOREGROUND-COLOR 6 HIGH.
              06 LINE 19 COLUMN 03 VALUE "Posicione o cursor n".
              06 LINE 19 COLUMN 23 VALUE "o numero da combinac".
              06 LINE 19 COLUMN 43 VALUE "ao de cor a alterar ".
              06 LINE 19 COLUMN 63 VALUE "e digite o novo".
              06 LINE 20 COLUMN 03 VALUE "codigo ou tecle PgUp".
              06 LINE 20 COLUMN 23 VALUE " PgDn (Segunda colun".
              06 LINE 20 COLUMN 43 VALUE "a da cor deve ser pi".
              06 LINE 20 COLUMN 63 VALUE "scante em 16-bit".
              06 LINE 21 COLUMN 03 VALUE "e em 32-bit mais intenso.".

       01  CTAC-LIT-MFY.
           03 FOREGROUND-COLOR 1 HIGH.
           05 LINE 01 COLUMN 04 VALUE "0 1 2 3 4 5 6 7 8 9 ".
           05 LINE 01 COLUMN 24 VALUE "A B C D E F".
           05 LINE 02 COLUMN 01 VALUE " 0".
           05 LINE 03 COLUMN 01 VALUE " 1".
           05 LINE 04 COLUMN 01 VALUE " 2".
           05 LINE 04 COLUMN 43 VALUE "0".
           05 LINE 04 COLUMN 58 VALUE "1".
           05 LINE 04 COLUMN 73 VALUE "2".
           05 LINE 05 COLUMN 01 VALUE " 3".
           05 LINE 06 COLUMN 01 VALUE " 4".
           05 LINE 07 COLUMN 01 VALUE " 5".
           05 LINE 08 COLUMN 01 VALUE " 6".
           05 LINE 09 COLUMN 01 VALUE " 7".
           05 LINE 09 COLUMN 43 VALUE "3".
           05 LINE 09 COLUMN 58 VALUE "4".
           05 LINE 09 COLUMN 73 VALUE "5".
           05 LINE 10 COLUMN 01 VALUE " 8".
           05 LINE 11 COLUMN 01 VALUE " 9".
           05 LINE 12 COLUMN 01 VALUE " A".
           05 LINE 13 COLUMN 01 VALUE " B".
           05 LINE 14 COLUMN 01 VALUE " C".
           05 LINE 14 COLUMN 43 VALUE "6".
           05 LINE 14 COLUMN 58 VALUE "7".
           05 LINE 15 COLUMN 01 VALUE " D".
           05 LINE 16 COLUMN 01 VALUE " E".
           05 LINE 17 COLUMN 01 VALUE " F".
           05 LINE 23 COLUMN 03 VALUE "COBOLware 6.0 Config".
           05 LINE 23 COLUMN 23 VALUE "uracao de molduras".
           05 LINE 24 COLUMN 03 VALUE "Copyright (C) 1984-2015 C".
           05 LINE 24 COLUMN 28 VALUE "OBOLware Services Ltda.".
           05 LINE 24 COLUMN 67 VALUE "[esc] Encerra".
           05 FOREGROUND-COLOR 4 HIGH.
              06 LINE 23 COLUMN 08 "ware".
              06 LINE 24 COLUMN 32 "ware".
           05 FOREGROUND-COLOR 3 HIGH.
              06 LINE 19 COLUMN 04 VALUE "1 Canto superior esq".
              06 LINE 19 COLUMN 24 VALUE "uerdo".
              06 LINE 19 COLUMN 30 VALUE "4 Fio Vertical".
              06 LINE 19 COLUMN 50 VALUE "7 Canto inferior esq".
              06 LINE 19 COLUMN 70 VALUE "uerdo".
              06 LINE 20 COLUMN 04 VALUE "2 Fio Horizontal".
              06 LINE 20 COLUMN 30 VALUE "5 Conector esquerdo".
              06 LINE 20 COLUMN 50 VALUE "8 Canto inferior dir".
              06 LINE 20 COLUMN 70 VALUE "eito".
              06 LINE 21 COLUMN 04 VALUE "3 Canto superior dir".
              06 LINE 21 COLUMN 24 VALUE "eito".
              06 LINE 21 COLUMN 30 VALUE "6 Conector direito".
           05 FOREGROUND-COLOR 6 HIGH.
              06 LINE 19 COLUMN 04 VALUE "1".
              06 LINE 20 COLUMN 04 VALUE "2".
              06 LINE 21 COLUMN 04 VALUE "3".
              06 LINE 19 COLUMN 30 VALUE "4".
              06 LINE 20 COLUMN 30 VALUE "5".
              06 LINE 21 COLUMN 30 VALUE "6".
              06 LINE 19 COLUMN 50 VALUE "7".
              06 LINE 20 COLUMN 50 VALUE "8".
              06 LINE 21 COLUMN 50 VALUE "Caractere a alterar".
              06 T02
                 LINE 21 COLUMN 70 PIC 9(001) USING TIPO-CARACTER AUTO.
           05 FOREGROUND-COLOR 1 HIGH.
              06 LINE 12 COLUMN 68 VALUE "Alterar Tipo".
              06 T01
                 LINE 14 COLUMN 73 PIC 9(001) USING TIPO-MOLDURA AUTO.

       01  CTAC-VAR-MFY.
           02 FOREGROUND-COLOR 3 HIGH.
           03 M0.
              05 LINE 01 COLUMN 37 PIC X(013) FROM M0-TOPO.
              05 LINE 02 COLUMN 37 PIC X(001) FROM M0-LADO.
              05 LINE 02 COLUMN 49 PIC X(001) FROM M0-LADO.
              05 LINE 03 COLUMN 37 PIC X(013) FROM M0-MEIO.
              05 LINE 04 COLUMN 37 PIC X(001) FROM M0-LADO.
              05 LINE 04 COLUMN 49 PIC X(001) FROM M0-LADO.
              05 LINE 05 COLUMN 37 PIC X(013) FROM M0-BASE.
           03 M1.
              05 LINE 01 COLUMN 52 PIC X(013) FROM M1-TOPO.
              05 LINE 02 COLUMN 52 PIC X(001) FROM M1-LADO.
              05 LINE 02 COLUMN 64 PIC X(001) FROM M1-LADO.
              05 LINE 03 COLUMN 52 PIC X(013) FROM M1-MEIO.
              05 LINE 04 COLUMN 52 PIC X(001) FROM M1-LADO.
              05 LINE 04 COLUMN 64 PIC X(001) FROM M1-LADO.
              05 LINE 05 COLUMN 52 PIC X(013) FROM M1-BASE.
           03 M2.
              05 LINE 01 COLUMN 67 PIC X(013) FROM M2-TOPO.
              05 LINE 02 COLUMN 67 PIC X(001) FROM M2-LADO.
              05 LINE 02 COLUMN 79 PIC X(001) FROM M2-LADO.
              05 LINE 03 COLUMN 67 PIC X(013) FROM M2-MEIO.
              05 LINE 04 COLUMN 67 PIC X(001) FROM M2-LADO.
              05 LINE 04 COLUMN 79 PIC X(001) FROM M2-LADO.
              05 LINE 05 COLUMN 67 PIC X(013) FROM M2-BASE.
           03 M3.
              05 LINE 06 COLUMN 37 PIC X(013) FROM M3-TOPO.
              05 LINE 07 COLUMN 37 PIC X(001) FROM M3-LADO.
              05 LINE 07 COLUMN 49 PIC X(001) FROM M3-LADO.
              05 LINE 08 COLUMN 37 PIC X(013) FROM M3-MEIO.
              05 LINE 09 COLUMN 37 PIC X(001) FROM M3-LADO.
              05 LINE 09 COLUMN 49 PIC X(001) FROM M3-LADO.
              05 LINE 10 COLUMN 37 PIC X(013) FROM M3-BASE.
           03 M4.
              05 LINE 06 COLUMN 52 PIC X(013) FROM M4-TOPO.
              05 LINE 07 COLUMN 52 PIC X(001) FROM M4-LADO.
              05 LINE 07 COLUMN 64 PIC X(001) FROM M4-LADO.
              05 LINE 08 COLUMN 52 PIC X(013) FROM M4-MEIO.
              05 LINE 09 COLUMN 52 PIC X(001) FROM M4-LADO.
              05 LINE 09 COLUMN 64 PIC X(001) FROM M4-LADO.
              05 LINE 10 COLUMN 52 PIC X(013) FROM M4-BASE.
           03 M5.
              05 LINE 06 COLUMN 67 PIC X(013) FROM M5-TOPO.
              05 LINE 07 COLUMN 67 PIC X(001) FROM M5-LADO.
              05 LINE 07 COLUMN 79 PIC X(001) FROM M5-LADO.
              05 LINE 08 COLUMN 67 PIC X(013) FROM M5-MEIO.
              05 LINE 09 COLUMN 67 PIC X(001) FROM M5-LADO.
              05 LINE 09 COLUMN 79 PIC X(001) FROM M5-LADO.
              05 LINE 10 COLUMN 67 PIC X(013) FROM M5-BASE.
           03 M6.
              05 LINE 11 COLUMN 37 PIC X(013) FROM M6-TOPO.
              05 LINE 12 COLUMN 37 PIC X(001) FROM M6-LADO.
              05 LINE 12 COLUMN 49 PIC X(001) FROM M6-LADO.
              05 LINE 13 COLUMN 37 PIC X(013) FROM M6-MEIO.
              05 LINE 14 COLUMN 37 PIC X(001) FROM M6-LADO.
              05 LINE 14 COLUMN 49 PIC X(001) FROM M6-LADO.
              05 LINE 15 COLUMN 37 PIC X(013) FROM M6-BASE.
           03 M7.
              05 LINE 11 COLUMN 52 PIC X(013) FROM M7-TOPO.
              05 LINE 12 COLUMN 52 PIC X(001) FROM M7-LADO.
              05 LINE 12 COLUMN 64 PIC X(001) FROM M7-LADO.
              05 LINE 13 COLUMN 52 PIC X(013) FROM M7-MEIO.
              05 LINE 14 COLUMN 52 PIC X(001) FROM M7-LADO.
              05 LINE 14 COLUMN 64 PIC X(001) FROM M7-LADO.
              05 LINE 15 COLUMN 52 PIC X(013) FROM M7-BASE.

       01  COR-3.
           05 LINE 1 COLUMN 1 PIC 9(003) USING COR-A AUTO.

       01  COR-2.
           05 LINE 1 COLUMN 1 PIC 9(002) USING COR-B AUTO.

       01  CTAC-LIT-MFA.
           03 BACKGROUND-COLOR 1 HIGH.
           05 LINE 03 COLUMN 15 VALUE "                 ".
           05 LINE 03 COLUMN 49 VALUE "                 ".
           05 LINE 04 COLUMN 15 VALUE " ".
           05 LINE 04 COLUMN 31 VALUE " ".
           05 LINE 04 COLUMN 49 VALUE " ".
           05 LINE 04 COLUMN 65 VALUE " ".
           05 LINE 05 COLUMN 15 VALUE "                    ".
           05 LINE 05 COLUMN 35 VALUE "                    ".
           05 LINE 05 COLUMN 55 VALUE "           ".
           05 LINE 06 COLUMN 15 VALUE " ".
           05 LINE 06 COLUMN 17 VALUE " ".
           05 LINE 06 COLUMN 19 VALUE " ".
           05 LINE 06 COLUMN 21 VALUE " ".
           05 LINE 06 COLUMN 23 VALUE " ".
           05 LINE 06 COLUMN 25 VALUE " ".
           05 LINE 06 COLUMN 27 VALUE " ".
           05 LINE 06 COLUMN 29 VALUE " ".
           05 LINE 06 COLUMN 31 VALUE " ".
           05 LINE 06 COLUMN 49 VALUE " ".
           05 LINE 06 COLUMN 51 VALUE " ".
           05 LINE 06 COLUMN 53 VALUE " ".
           05 LINE 06 COLUMN 55 VALUE " ".
           05 LINE 06 COLUMN 57 VALUE " ".
           05 LINE 06 COLUMN 59 VALUE " ".
           05 LINE 06 COLUMN 61 VALUE " ".
           05 LINE 06 COLUMN 63 VALUE " ".
           05 LINE 06 COLUMN 65 VALUE " ".
           05 LINE 07 COLUMN 15 VALUE "                    ".
           05 LINE 07 COLUMN 35 VALUE "                    ".
           05 LINE 07 COLUMN 55 VALUE "           ".
           05 LINE 08 COLUMN 15 VALUE " ".
           05 LINE 08 COLUMN 17 VALUE " ".
           05 LINE 08 COLUMN 19 VALUE " ".
           05 LINE 08 COLUMN 21 VALUE " ".
           05 LINE 08 COLUMN 23 VALUE " ".
           05 LINE 08 COLUMN 25 VALUE " ".
           05 LINE 08 COLUMN 27 VALUE " ".
           05 LINE 08 COLUMN 29 VALUE " ".
           05 LINE 08 COLUMN 31 VALUE " ".
           05 LINE 08 COLUMN 49 VALUE " ".
           05 LINE 08 COLUMN 51 VALUE " ".
           05 LINE 08 COLUMN 53 VALUE " ".
           05 LINE 08 COLUMN 55 VALUE " ".
           05 LINE 08 COLUMN 57 VALUE " ".
           05 LINE 08 COLUMN 59 VALUE " ".
           05 LINE 08 COLUMN 61 VALUE " ".
           05 LINE 08 COLUMN 63 VALUE " ".
           05 LINE 08 COLUMN 65 VALUE " ".
           05 LINE 09 COLUMN 15 VALUE "                    ".
           05 LINE 09 COLUMN 35 VALUE "                    ".
           05 LINE 09 COLUMN 55 VALUE "           ".
           05 LINE 10 COLUMN 15 VALUE " ".
           05 LINE 10 COLUMN 17 VALUE " ".
           05 LINE 10 COLUMN 19 VALUE " ".
           05 LINE 10 COLUMN 21 VALUE " ".
           05 LINE 10 COLUMN 23 VALUE " ".
           05 LINE 10 COLUMN 25 VALUE " ".
           05 LINE 10 COLUMN 27 VALUE " ".
           05 LINE 10 COLUMN 29 VALUE " ".
           05 LINE 10 COLUMN 31 VALUE " ".
           05 LINE 10 COLUMN 49 VALUE " ".
           05 LINE 10 COLUMN 51 VALUE " ".
           05 LINE 10 COLUMN 53 VALUE " ".
           05 LINE 10 COLUMN 55 VALUE " ".
           05 LINE 10 COLUMN 57 VALUE " ".
           05 LINE 10 COLUMN 59 VALUE " ".
           05 LINE 10 COLUMN 61 VALUE " ".
           05 LINE 10 COLUMN 63 VALUE " ".
           05 LINE 10 COLUMN 65 VALUE " ".
           05 LINE 11 COLUMN 15 VALUE "                    ".
           05 LINE 11 COLUMN 35 VALUE "                    ".
           05 LINE 11 COLUMN 55 VALUE "           ".
           05 LINE 12 COLUMN 15 VALUE " ".
           05 LINE 12 COLUMN 17 VALUE " ".
           05 LINE 12 COLUMN 19 VALUE " ".
           05 LINE 12 COLUMN 21 VALUE " ".
           05 LINE 12 COLUMN 23 VALUE " ".
           05 LINE 12 COLUMN 25 VALUE " ".
           05 LINE 12 COLUMN 27 VALUE " ".
           05 LINE 12 COLUMN 29 VALUE " ".
           05 LINE 12 COLUMN 31 VALUE " ".
           05 LINE 12 COLUMN 49 VALUE " ".
           05 LINE 12 COLUMN 51 VALUE " ".
           05 LINE 12 COLUMN 53 VALUE " ".
           05 LINE 12 COLUMN 55 VALUE " ".
           05 LINE 12 COLUMN 57 VALUE " ".
           05 LINE 12 COLUMN 59 VALUE " ".
           05 LINE 12 COLUMN 61 VALUE " ".
           05 LINE 12 COLUMN 63 VALUE " ".
           05 LINE 12 COLUMN 65 VALUE " ".
           05 LINE 13 COLUMN 15 VALUE "                    ".
           05 LINE 13 COLUMN 35 VALUE "                    ".
           05 LINE 13 COLUMN 55 VALUE "           ".
           05 LINE 14 COLUMN 15 VALUE " ".
           05 LINE 14 COLUMN 17 VALUE " ".
           05 LINE 14 COLUMN 19 VALUE " ".
           05 LINE 14 COLUMN 21 VALUE " ".
           05 LINE 14 COLUMN 23 VALUE " ".
           05 LINE 14 COLUMN 25 VALUE " ".
           05 LINE 14 COLUMN 27 VALUE " ".
           05 LINE 14 COLUMN 29 VALUE " ".
           05 LINE 14 COLUMN 31 VALUE " ".
           05 LINE 14 COLUMN 49 VALUE " ".
           05 LINE 14 COLUMN 51 VALUE " ".
           05 LINE 14 COLUMN 53 VALUE " ".
           05 LINE 14 COLUMN 55 VALUE " ".
           05 LINE 14 COLUMN 57 VALUE " ".
           05 LINE 14 COLUMN 59 VALUE " ".
           05 LINE 14 COLUMN 61 VALUE " ".
           05 LINE 14 COLUMN 63 VALUE " ".
           05 LINE 14 COLUMN 65 VALUE " ".
           05 LINE 15 COLUMN 15 VALUE "                    ".
           05 LINE 15 COLUMN 35 VALUE "                    ".
           05 LINE 15 COLUMN 55 VALUE "           ".
           05 LINE 16 COLUMN 15 VALUE " ".
           05 LINE 16 COLUMN 17 VALUE " ".
           05 LINE 16 COLUMN 19 VALUE " ".
           05 LINE 16 COLUMN 21 VALUE " ".
           05 LINE 16 COLUMN 23 VALUE " ".
           05 LINE 16 COLUMN 25 VALUE " ".
           05 LINE 16 COLUMN 27 VALUE " ".
           05 LINE 16 COLUMN 29 VALUE " ".
           05 LINE 16 COLUMN 31 VALUE " ".
           05 LINE 16 COLUMN 49 VALUE " ".
           05 LINE 16 COLUMN 51 VALUE " ".
           05 LINE 16 COLUMN 53 VALUE " ".
           05 LINE 16 COLUMN 55 VALUE " ".
           05 LINE 16 COLUMN 57 VALUE " ".
           05 LINE 16 COLUMN 59 VALUE " ".
           05 LINE 16 COLUMN 61 VALUE " ".
           05 LINE 16 COLUMN 63 VALUE " ".
           05 LINE 16 COLUMN 65 VALUE " ".
           05 LINE 17 COLUMN 15 VALUE "                    ".
           05 LINE 17 COLUMN 35 VALUE "                    ".
           05 LINE 17 COLUMN 55 VALUE "           ".
           05 LINE 18 COLUMN 15 VALUE " ".
           05 LINE 18 COLUMN 17 VALUE " ".
           05 LINE 18 COLUMN 19 VALUE " ".
           05 LINE 18 COLUMN 21 VALUE " ".
           05 LINE 18 COLUMN 23 VALUE " ".
           05 LINE 18 COLUMN 25 VALUE " ".
           05 LINE 18 COLUMN 27 VALUE " ".
           05 LINE 18 COLUMN 29 VALUE " ".
           05 LINE 18 COLUMN 31 VALUE " ".
           05 LINE 18 COLUMN 49 VALUE " ".
           05 LINE 18 COLUMN 51 VALUE " ".
           05 LINE 18 COLUMN 53 VALUE " ".
           05 LINE 18 COLUMN 55 VALUE " ".
           05 LINE 18 COLUMN 57 VALUE " ".
           05 LINE 18 COLUMN 59 VALUE " ".
           05 LINE 18 COLUMN 61 VALUE " ".
           05 LINE 18 COLUMN 63 VALUE " ".
           05 LINE 18 COLUMN 65 VALUE " ".
           05 LINE 19 COLUMN 15 VALUE "                    ".
           05 LINE 19 COLUMN 35 VALUE "                    ".
           05 LINE 19 COLUMN 55 VALUE "           ".
           03 FOREGROUND-COLOR 2 HIGH.
           05 LINE 04 COLUMN 18 VALUE "Maiusculas".
           05 LINE 04 COLUMN 52 VALUE "Minusculas".
           05 LINE 06 COLUMN 16 VALUE "A".
           05 LINE 06 COLUMN 18 VALUE "E".
           05 LINE 06 COLUMN 20 VALUE "I".
           05 LINE 06 COLUMN 22 VALUE "O".
           05 LINE 06 COLUMN 24 VALUE "U".
           05 LINE 06 COLUMN 26 VALUE "C".
           05 LINE 06 COLUMN 28 VALUE "Y".
           05 LINE 06 COLUMN 30 VALUE "N".
           05 LINE 06 COLUMN 33 VALUE "Tipo de acento".
           05 LINE 06 COLUMN 50 VALUE "a".
           05 LINE 06 COLUMN 52 VALUE "e".
           05 LINE 06 COLUMN 54 VALUE "i".
           05 LINE 06 COLUMN 56 VALUE "o".
           05 LINE 06 COLUMN 58 VALUE "u".
           05 LINE 06 COLUMN 60 VALUE "c".
           05 LINE 06 COLUMN 62 VALUE "y".
           05 LINE 06 COLUMN 64 VALUE "n".
           05 LINE 08 COLUMN 33 VALUE "Agudo".
           05 LINE 08 COLUMN 45 VALUE "[']".
           05 LINE 10 COLUMN 33 VALUE "Circunflexo".
           05 LINE 10 COLUMN 45 VALUE "[^]".
           05 LINE 12 COLUMN 33 VALUE "Crase".
           05 LINE 12 COLUMN 45 VALUE "[`]".
           05 LINE 14 COLUMN 33 VALUE "Til".
           05 LINE 14 COLUMN 45 VALUE "[~]".
           05 LINE 16 COLUMN 33 VALUE "Trema".
           05 LINE 16 COLUMN 45 VALUE "[ù]".
           05 LINE 18 COLUMN 33 VALUE "Cedilha".
           03 FOREGROUND-COLOR 1 HIGH.
           05 LINE 23 COLUMN 03 VALUE "COBOLware 6.0 Config".
           05 LINE 23 COLUMN 23 VALUE "uracao de acentos".
           05 LINE 24 COLUMN 03 VALUE "Copyright (C) 1984-2015 C".
           05 LINE 24 COLUMN 28 VALUE "OBOLware Services Ltda.".
           05 LINE 24 COLUMN 67 VALUE "[esc] Encerra".
           05 FOREGROUND-COLOR 4 HIGH.
              06 LINE 23 COLUMN 08 "ware".
              06 LINE 24 COLUMN 32 "ware".

       01  CTAC-VAR-MFA AUTO.
           02 FOREGROUND-COLOR 6 HIGH.
           05 LINE 08 COLUMN 16 PIC X(001) USING AC(01).
           05 LINE 08 COLUMN 18 PIC X(001) USING AC(02).
           05 LINE 08 COLUMN 20 PIC X(001) USING AC(03).
           05 LINE 08 COLUMN 22 PIC X(001) USING AC(04).
           05 LINE 08 COLUMN 24 PIC X(001) USING AC(05).
           05 LINE 08 COLUMN 28 PIC X(001) USING AC(06).
           05 LINE 08 COLUMN 50 PIC X(001) USING AC(07).
           05 LINE 08 COLUMN 52 PIC X(001) USING AC(08).
           05 LINE 08 COLUMN 54 PIC X(001) USING AC(09).
           05 LINE 08 COLUMN 56 PIC X(001) USING AC(10).
           05 LINE 08 COLUMN 58 PIC X(001) USING AC(11).
           05 LINE 08 COLUMN 62 PIC X(001) USING AC(12).
           05 LINE 10 COLUMN 16 PIC X(001) USING AC(13).
           05 LINE 10 COLUMN 18 PIC X(001) USING AC(14).
           05 LINE 10 COLUMN 20 PIC X(001) USING AC(15).
           05 LINE 10 COLUMN 22 PIC X(001) USING AC(16).
           05 LINE 10 COLUMN 24 PIC X(001) USING AC(17).
           05 LINE 10 COLUMN 50 PIC X(001) USING AC(18).
           05 LINE 10 COLUMN 52 PIC X(001) USING AC(19).
           05 LINE 10 COLUMN 54 PIC X(001) USING AC(20).
           05 LINE 10 COLUMN 56 PIC X(001) USING AC(21).
           05 LINE 10 COLUMN 58 PIC X(001) USING AC(22).
           05 LINE 12 COLUMN 16 PIC X(001) USING AC(23).
           05 LINE 12 COLUMN 18 PIC X(001) USING AC(24).
           05 LINE 12 COLUMN 20 PIC X(001) USING AC(25).
           05 LINE 12 COLUMN 22 PIC X(001) USING AC(26).
           05 LINE 12 COLUMN 24 PIC X(001) USING AC(27).
           05 LINE 12 COLUMN 50 PIC X(001) USING AC(28).
           05 LINE 12 COLUMN 52 PIC X(001) USING AC(29).
           05 LINE 12 COLUMN 54 PIC X(001) USING AC(30).
           05 LINE 12 COLUMN 56 PIC X(001) USING AC(31).
           05 LINE 12 COLUMN 58 PIC X(001) USING AC(32).
           05 LINE 14 COLUMN 16 PIC X(001) USING AC(33).
           05 LINE 14 COLUMN 22 PIC X(001) USING AC(34).
           05 LINE 14 COLUMN 30 PIC X(001) USING AC(35).
           05 LINE 14 COLUMN 50 PIC X(001) USING AC(36).
           05 LINE 14 COLUMN 56 PIC X(001) USING AC(37).
           05 LINE 14 COLUMN 64 PIC X(001) USING AC(38).
           05 LINE 16 COLUMN 16 PIC X(001) USING AC(39).
           05 LINE 16 COLUMN 18 PIC X(001) USING AC(51).
           05 LINE 16 COLUMN 20 PIC X(001) USING AC(40).
           05 LINE 16 COLUMN 22 PIC X(001) USING AC(41).
           05 LINE 16 COLUMN 24 PIC X(001) USING AC(42).
           05 LINE 16 COLUMN 50 PIC X(001) USING AC(43).
           05 LINE 16 COLUMN 52 PIC X(001) USING AC(44).
           05 LINE 16 COLUMN 54 PIC X(001) USING AC(45).
           05 LINE 16 COLUMN 56 PIC X(001) USING AC(46).
           05 LINE 16 COLUMN 58 PIC X(001) USING AC(47).
           05 LINE 16 COLUMN 62 PIC X(001) USING AC(48).
           05 LINE 18 COLUMN 26 PIC X(001) USING AC(49).
           05 LINE 18 COLUMN 60 PIC X(001) USING AC(50).

       PROCEDURE DIVISION.
       LOAD.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
           ACCEPT  COBWARE   FROM ENVIRONMENT-VALUE
           DISPLAY "TERM"      UPON ENVIRONMENT-NAME
           ACCEPT  TERM        FROM ENVIRONMENT-VALUE
           IF   TERM = SPACES
                MOVE "dos" TO TERM
                IF  CWUNIX-WINDOWS
                    MOVE "win" TO TERM
                END-IF
           END-IF
           STRING COBWARE    DELIMITED BY SPACE
                  "/boxcolor." DELIMITED BY SIZE
                  TERM         DELIMITED BY SPACE
                         INTO LB-BOXCOLOR
           OPEN INPUT BOXCOLOR
           IF    FS-BOXCOLOR < "10"
                 MOVE 1 TO RK-BOXCOLOR
                 READ BOXCOLOR INTO CORES
                 IF  FS-BOXCOLOR < "10"
                     MOVE "1" TO FL-SAVE1
                 END-IF
                 MOVE 2 TO RK-BOXCOLOR
                 READ BOXCOLOR INTO MOLDURAS
                 IF  FS-BOXCOLOR < "10"
                     MOVE "1" TO FL-SAVE2
                 END-IF
                 MOVE 3 TO RK-BOXCOLOR
                 READ BOXCOLOR INTO ACENTOS
                 IF  FS-BOXCOLOR < "10"
                     MOVE "1" TO FL-SAVE3
                 END-IF
                 CLOSE BOXCOLOR
           ELSE
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 255
                         COMPUTE Y = I + 1
                         MOVE I TO COR (Y)
                 END-PERFORM
           END-IF
           IF   FL-SAVE3 = 0
                IF   CWUNIX-ON
                     MOVE ACENTOS-OFF TO ACENTOS
                ELSE
                     MOVE ACENTOS-437 TO ACENTOS
                END-IF
           END-IF
           PERFORM MONTA-MOLDURAS
                   VARYING M FROM 1 BY 1
                           UNTIL M > 8
           MOVE ACENTOS  TO ACENTOS-SAVE
           MOVE CORES    TO CORES-SAVE
           MOVE MOLDURAS TO MOLDURAS-SAVE.

       000-INICIO.

           DISPLAY (1, 1) ERASE
           DISPLAY "Configuracao de acentos, cores e molduras"
           DISPLAY " "
           DISPLAY "COBOLware 6.0"
           DISPLAY "Copyright (C) 1984-2015 COBOLware Services Ltda."
           DISPLAY " "
           DISPLAY "Opcoes disponiveis: "
           DISPLAY " "
           DISPLAY "1 - Acentos"
           DISPLAY "2 - Cores"
           DISPLAY "3 - Molduras"
           DISPLAY "4 - Salvar"
           DISPLAY "5 - Fim"
           DISPLAY " "
           DISPLAY "Selecione opcao desejada: "
           ACCEPT (14, 27) OPCAO WITH AUTO-SKIP

           IF   OPCAO = 0
                MOVE 5 TO OPCAO
           END-IF
           EVALUATE OPCAO
                    WHEN 5 IF   (ACENTOS NOT = ACENTOS-SAVE)
                           OR   (CORES NOT = CORES-SAVE)
                           OR   (MOLDURAS NOT = MOLDURAS-SAVE)
                                DISPLAY " "
                                DISPLAY "Abandonar sem salvar ? S/<N> "
                                        WITH NO ADVANCING
                                MOVE "N" TO RESPOSTA
                                ACCEPT RESPOSTA
                                IF   RESPOSTA = "S" OR "s"
                                     GOBACK
                                END-IF
                           ELSE
                                GOBACK
                           END-IF
                    WHEN 4 OPEN OUTPUT BOXCOLOR
                           MOVE CORES TO CORES-SAVE
                           MOVE 1 TO RK-BOXCOLOR
                           WRITE BOXCOLOR-REG FROM CORES
                           MOVE 2 TO RK-BOXCOLOR
                           MOVE MOLDURAS TO MOLDURAS-SAVE
                           WRITE BOXCOLOR-REG FROM MOLDURAS
                           MOVE 3 TO RK-BOXCOLOR
                           MOVE ACENTOS TO ACENTOS-SAVE
                           WRITE BOXCOLOR-REG FROM ACENTOS
                           CANCEL "CWMOLD"
                           CANCEL "CWTEXT"
                           CLOSE BOXCOLOR
                    WHEN 3 PERFORM P-MOLDURAS THRU FIM-MOLDURAS
                    WHEN 2 PERFORM P-CORES    THRU FIM-CORES
                    WHEN 1 DISPLAY (1, 1) ERASE "F1 - Sem acentos"
                           DISPLAY (2, 1) "F2 - PC 437"
                           DISPLAY (3, 1) "F3 - PC 850"
                           DISPLAY CTAC-LIT-MFA
                                   CTAC-VAR-MFA
                           PERFORM TEST AFTER
                            UNTIL NOT (KEY-1 = 1
                                   AND KEY-2 = 1)
                              AND (NOT (KEY-1 = 1
                                    AND KEY-2 = 2))
                              AND (NOT (KEY-1 = 1
                                    AND KEY-2 = 3))
                                   ACCEPT CTAC-VAR-MFA
                                   IF  KEY-1 = 1
                                   AND KEY-2 = 1
                                       MOVE ACENTOS-OFF TO ACENTOS
                                       DISPLAY CTAC-VAR-MFA
                                   END-IF
                                   IF  KEY-1 = 1
                                   AND KEY-2 = 2
                                       MOVE ACENTOS-437 TO ACENTOS
                                       DISPLAY CTAC-VAR-MFA
                                   END-IF
                                   IF  KEY-1 = 1
                                   AND KEY-2 = 3
                                       MOVE ACENTOS-850 TO ACENTOS
                                       DISPLAY CTAC-VAR-MFA
                                   END-IF
                           END-PERFORM
           END-EVALUATE
           GO TO 000-INICIO.

       P-CORES.

           MOVE  0 TO I
           MOVE  1 TO L
           MOVE 16 TO C
           DISPLAY (1, 1) ERASE
           DISPLAY CTAC-LIT-MFX.

       PRINT-COLOR.

           COMPUTE Y = I + 1

           IF   FL-SAVE1 = "0"
                MOVE I       TO CARACTER-BUFFER
                                 I-X
           ELSE
                MOVE COR (Y) TO I-999
                                I-X
                MOVE I-999 TO CARACTER-BUFFER
           END-IF

           MOVE X                          TO ATTRIBUTE-BUFFER (1: 1)
                                              ATTRIBUTE-BUFFER (2: 1)
                                              ATTRIBUTE-BUFFER (3: 1)
           MOVE L                          TO ROW-NUMBER     L-P (Y)
           MOVE C                          TO COLUMN-NUMBER  C-P (Y)
           MOVE I-X                        TO COR (Y)
           MOVE 3                          TO STRING-LENGTH
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH
           ADD 1 TO I

           IF   I > 255
                IF   FL-SAVE1 = "0"
                     MOVE CORES TO CORES-SAVE
                END-IF
                GO TO MUDA-CORES
           END-IF

           ADD 1 TO L

           IF   L > 16
                MOVE 1 TO L
                ADD  8 TO C
           END-IF

           IF   I = 128
                MOVE  1 TO L
                MOVE 20 TO C
           END-IF

           GO TO PRINT-COLOR.

       MUDA-CORES.

           MOVE  1                   TO L
           MOVE 16                   TO C.

       KBD.

           MOVE L TO ROW-NUMBER
           MOVE C TO COLUMN-NUMBER
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 255
                      OR (L-P (I) = L AND C-P (I) = C)
                   CONTINUE
           END-PERFORM
           SUBTRACT 1 FROM I
           COMPUTE L2 = L + 1
           COMPUTE C2 = C + 1
           COMPUTE Y = I + 1
           MOVE COR (Y)                    TO I-999
                                              I-X
           MOVE I-999                      TO CARACTER-BUFFER
      *    DISPLAY (L2, C2) I-999
           SUBTRACT 1 FROM C2
           MOVE X                          TO ATTRIBUTE-BUFFER (1: 1)
                                              ATTRIBUTE-BUFFER (2: 1)
                                              ATTRIBUTE-BUFFER (3: 1)
           MOVE L                          TO ROW-NUMBER     L-P (Y)
           MOVE C                          TO COLUMN-NUMBER  C-P (Y)
           MOVE I-X                        TO COR (Y)
           MOVE 3                          TO STRING-LENGTH
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH
      *    DISPLAY (L2, C2) ">" WITH BLINK
           MOVE 98 TO TECLA

           COMPUTE L3 = L2 - 1
           COMPUTE C3 = C2 - 1
           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION-3
                                             CARACTER-BUFFER-3
                                             ATTRIBUTE-BUFFER-3
                                             X"0005"
           MOVE ATTRIBUTE-BUFFER-3 (1: 1) TO AX
           MOVE 0 TO CONTA-LOOP KEY-STATUS
==>        PERFORM TEST AFTER UNTIL KEY-STATUS = 1
              CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
              ADD 1 TO CONTA-LOOP
              IF   CONTA-LOOP = 1
              OR   CWUNIX-WINDOWS
                   ADD 1 TO K
                   IF  K = 5
                       MOVE 1 TO K
                   END-IF
                   MOVE K1 (K) TO CARACTER-BUFFER-3 (1: 1)
                   MOVE K2 (K) TO CARACTER-BUFFER-3 (5: 1)
                   MOVE X"0E" TO ATTRIBUTE-BUFFER-3 (1: 1)
                                 ATTRIBUTE-BUFFER-3 (5: 1)
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-3
                                                      CARACTER-BUFFER-3
                                                      ATTRIBUTE-BUFFER-3
                                                      X"0005"
                   IF   CWUNIX-ON
                        DISPLAY (L2, C2) GIRO WITH
                            FOREGROUND-COLOR 6 HIGH
                   END-IF
              END-IF
           END-PERFORM

           CALL "CWKBDC" USING CURPOS CARACTER TECLA
      *    DISPLAY (L2, C2) " "

           MOVE " " TO CARACTER-BUFFER-3 (1: 1)
           MOVE " " TO CARACTER-BUFFER-3 (5: 1)
           MOVE AX  TO ATTRIBUTE-BUFFER-3 (1: 1)
                       ATTRIBUTE-BUFFER-3 (5: 1)
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-3
                                              CARACTER-BUFFER-3
                                              ATTRIBUTE-BUFFER-3
                                              X"0005"

           EVALUATE TRUE
                    WHEN EDIT-ESC
                         GO TO FIM-CORES
                    WHEN EDIT-CURSOR-RIGHT
                         ADD 4 TO C
                         IF  C > 76
                             MOVE 16 TO C
                             ADD  1  TO L
                             IF   L > 16
                                  MOVE 1 TO L
                             END-IF
                         END-IF
                    WHEN EDIT-CURSOR-LEFT
                         SUBTRACT 8 FROM C
                         IF  C < 16
                             MOVE 76 TO C
                             SUBTRACT  1  FROM L
                             IF   L < 1
                                  MOVE 16 TO L
                             END-IF
                         END-IF
                    WHEN EDIT-CURSOR-DOWN
                         ADD 1 TO L
                         IF   L > 16
                              MOVE 1 TO L
                         END-IF
                    WHEN EDIT-CURSOR-UP
                         SUBTRACT 1 FROM L
                         IF   L < 1
                              MOVE 16 TO L
                         END-IF
                    WHEN EDIT-PAGE-UP
                         SUBTRACT 1 FROM COR (Y)
                    WHEN EDIT-PAGE-DOWN
                         ADD 1 TO COR (Y)
                    WHEN OTHER
                         ADD 1 TO C2
                         MOVE COR (Y) TO COR-A
                         IF   CARACTER IS NUMERIC
                              MOVE CARACTER TO COR-A (1: 1)
                              DISPLAY COR-3 LINE L2 COLUMN C2
                              ADD 1 TO C2
                              DISPLAY COR-2 LINE L2 COLUMN C2
                              ACCEPT COR-2 LINE L2 COLUMN C2
                         ELSE
                              DISPLAY COR-3 LINE L2 COLUMN C2
                              ACCEPT COR-3 LINE L2 COLUMN C2
                         END-IF
                         MOVE COR-A TO COR (Y)
           END-EVALUATE
           GO TO KBD.

       FIM-CORES.

       P-MOLDURAS.

           MOVE SPACES TO PROIBIDOS
           MOVE 0 TO I
           MOVE 1 TO L
           MOVE 3 TO C
           MOVE 0                          TO ROW-NUMBER
                                              COLUMN-NUMBER
           DISPLAY (1, 1) ERASE
           MOVE 1                          TO STRING-LENGTH
           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH
           DISPLAY CTAC-LIT-MFY
                   CTAC-VAR-MFY.

       PRINT-CHAR.

           COMPUTE Y = I + 1
           MOVE I                          TO I-X
           MOVE X                          TO CARACTER-BUFFER
           MOVE L                          TO ROW-NUMBER     L-P (Y)
           MOVE C                          TO COLUMN-NUMBER  C-P (Y)
           MOVE 1                          TO STRING-LENGTH
           CALL "CBL_READ_SCR_CHATTRS"  USING SCREEN-POSITION-2
                                              CARACTER-BUFFER-2
                                              ATTRIBUTE-BUFFER-2
                                              STRING-LENGTH-2
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              X"0A"
                                              STRING-LENGTH
           IF  I > 0
               CALL "CBL_READ_SCR_CHATTRS"  USING SCREEN-POSITION-A
                                                  BYTE-DEPOIS
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           ELSE
               MOVE CARACTER-BUFFER              TO BYTE-DEPOIS
                                                    BYTE-ANTES
           END-IF
           IF   BYTE-DEPOIS NOT = BYTE-ANTES
           OR  (BYTE-DEPOIS = SPACES AND (I - 1 NOT = 32))
                MOVE "1"                         TO PROIBIDO (Y)
                CALL "CBL_WRITE_SCR_CHATTRS"  USING SCREEN-POSITION-2
                                                    CARACTER-BUFFER-2
                                                    ATTRIBUTE-BUFFER-2
                                                    STRING-LENGTH-2
                MOVE SPACES TO                     CARACTER-BUFFER
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
           END-IF
           MOVE SCREEN-POSITION TO SCREEN-POSITION-A
           MOVE CARACTER-BUFFER TO BYTE-ANTES

           ADD 1 TO I

           IF   I > 255
                GO TO MUDA-MOLDURAS
           END-IF

           ADD 1 TO L

           IF   L > 16
                MOVE 1 TO L
                ADD  2 TO C
           END-IF

           GO TO PRINT-CHAR.

       MUDA-MOLDURAS.

           MOVE  1                   TO L
           MOVE  3                   TO C
           PERFORM TEST AFTER
                   UNTIL TIPO-MOLDURA < 9
                   ACCEPT T01
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF  TECLA = 1
                       GO TO FIM-MOLDURAS
                   END-IF
           END-PERFORM
           MOVE TIPO-MOLDURA TO M
           ADD 1 TO M.

       MUDA-CARACTER.

           PERFORM TEST AFTER
                   UNTIL TIPO-CARACTER < 9
                     AND TIPO-CARACTER > 0
                   ACCEPT T02
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF  TECLA = 1
                       GO TO FIM-MOLDURAS
                   END-IF
           END-PERFORM
           DISPLAY (17, 37) "Posicione o cursor no caracter desejado"
                            WITH FOREGROUND-COLOR 6 HIGH
           MOVE BASE-MOLDURA (M) (TIPO-CARACTER: 1) TO X
           MOVE I-X TO Y
           ADD  1   TO Y
           MOVE L-P (Y) TO L
           MOVE C-P (Y) TO C.

       KBD2.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 255
                      OR (L-P (I) = L AND C-P (I) = C)
                   CONTINUE
           END-PERFORM
           IF   PROIBIDO (I) = "1"
                GO TO SALTA-PROIBIDO
           END-IF
           SUBTRACT 1 FROM I
           MOVE I TO I-X
           COMPUTE L2 = L + 1
           COMPUTE C2 = C
      *    DISPLAY (L2, C2) ">" WITH BLINK
           MOVE X TO BASE-MOLDURA (M) (TIPO-CARACTER: 1)
           PERFORM MONTA-MOLDURAS
           EVALUATE TIPO-MOLDURA
                    WHEN 0 DISPLAY M0
                    WHEN 1 DISPLAY M1
                    WHEN 2 DISPLAY M2
                    WHEN 3 DISPLAY M3
                    WHEN 4 DISPLAY M4
                    WHEN 5 DISPLAY M5
                    WHEN 6 DISPLAY M6
                    WHEN 7 DISPLAY M7
           END-EVALUATE
           MOVE 98 TO TECLA

           COMPUTE L3 = L2 - 1
           COMPUTE C3 = C2 - 1
           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION-3
                                             CARACTER-BUFFER-3
                                             ATTRIBUTE-BUFFER-3
                                             X"0003"
           MOVE 0 TO CONTA-LOOP KEY-STATUS
           PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                   CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
              ADD 1 TO CONTA-LOOP
              IF   CONTA-LOOP = 1
              OR   CWUNIX-WINDOWS
                   ADD 1 TO K
                   IF  K = 5
                       MOVE 1 TO K
                       MOVE SPACE TO CARACTER-BUFFER-3 (2: 1)
                   ELSE
                       MOVE X     TO CARACTER-BUFFER-3 (2: 1)
                   END-IF
                   MOVE K1 (K) TO CARACTER-BUFFER-3 (1: 1)
                   MOVE K2 (K) TO CARACTER-BUFFER-3 (3: 1)
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-3
                                                      CARACTER-BUFFER-3
                                                      X"0E0E0E"
                                                      X"0003"
                   IF   CWUNIX-ON
                        DISPLAY (L2, C2) GIRO WITH
                            FOREGROUND-COLOR 6 HIGH
                   END-IF
              END-IF
           END-PERFORM

           CALL "CWKBDC" USING CURPOS CARACTER TECLA
           MOVE " " TO CARACTER-BUFFER-3 (1: 1)
           MOVE X   TO CARACTER-BUFFER-3 (2: 1)
           MOVE " " TO CARACTER-BUFFER-3 (3: 1)
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-3
                                              CARACTER-BUFFER-3
                                              X"0A0A0A"
                                              X"0003".
      *    DISPLAY (L2, C2) " ".

       SALTA-PROIBIDO.

           EVALUATE TRUE
                    WHEN EDIT-ESC
                         DISPLAY (17, 37)
                         "                                       "
                         GO TO MUDA-CARACTER
                    WHEN EDIT-CURSOR-RIGHT
                         ADD 2 TO C
                         IF  C > 33
                             MOVE 3  TO C
                             ADD  1  TO L
                             IF   L > 16
                                  MOVE 1 TO L
                             END-IF
                         END-IF
                    WHEN EDIT-CURSOR-LEFT
                         SUBTRACT 2 FROM C
                         IF  C < 3
                             MOVE 33 TO C
                             SUBTRACT  1  FROM L
                             IF   L < 1
                                  MOVE 16 TO L
                             END-IF
                         END-IF
                    WHEN EDIT-CURSOR-DOWN
                         ADD 1 TO L
                         IF   L > 16
                              MOVE 1 TO L
                         END-IF
                    WHEN EDIT-CURSOR-UP
                         SUBTRACT 1 FROM L
                         IF   L < 1
                              MOVE 16 TO L
                         END-IF
           END-EVALUATE
           GO TO KBD2.

       FIM-MOLDURAS. EXIT.

       MONTA-MOLDURAS.

           MOVE BASE-MOLDURA (M) (1: 1) TO M-TOPO (M)  (1: 1)
           MOVE BASE-MOLDURA (M) (2: 1) TO M-TOPO (M)  (2: 1)
                                           M-TOPO (M)  (3: 1)
                                           M-TOPO (M)  (4: 1)
                                           M-TOPO (M)  (5: 1)
                                           M-TOPO (M)  (6: 1)
                                           M-TOPO (M)  (7: 1)
                                           M-TOPO (M)  (8: 1)
                                           M-TOPO (M)  (9: 1)
                                           M-TOPO (M) (10: 1)
                                           M-TOPO (M) (11: 1)
                                           M-TOPO (M) (12: 1)
           MOVE BASE-MOLDURA (M) (3: 1) TO M-TOPO (M) (13: 1)
           MOVE BASE-MOLDURA (M) (4: 1) TO M-LADO (M)
           MOVE BASE-MOLDURA (M) (5: 1) TO M-MEIO (M)  (1: 1)
           MOVE BASE-MOLDURA (M) (2: 1) TO M-MEIO (M)  (2: 1)
                                           M-MEIO (M)  (3: 1)
                                           M-MEIO (M)  (4: 1)
                                           M-MEIO (M)  (5: 1)
                                           M-MEIO (M)  (6: 1)
                                           M-MEIO (M)  (7: 1)
                                           M-MEIO (M)  (8: 1)
                                           M-MEIO (M)  (9: 1)
                                           M-MEIO (M) (10: 1)
                                           M-MEIO (M) (11: 1)
                                           M-MEIO (M) (12: 1)
           MOVE BASE-MOLDURA (M) (6: 1) TO M-MEIO (M) (13: 1)
           MOVE BASE-MOLDURA (M) (7: 1) TO M-BASE (M)  (1: 1)
           MOVE BASE-MOLDURA (M) (2: 1) TO M-BASE (M)  (2: 1)
                                           M-BASE (M)  (3: 1)
                                           M-BASE (M)  (4: 1)
                                           M-BASE (M)  (5: 1)
                                           M-BASE (M)  (6: 1)
                                           M-BASE (M)  (7: 1)
                                           M-BASE (M)  (8: 1)
                                           M-BASE (M)  (9: 1)
                                           M-BASE (M) (10: 1)
                                           M-BASE (M) (11: 1)
                                           M-BASE (M) (12: 1)
           MOVE BASE-MOLDURA (M) (8: 1) TO M-BASE (M) (13: 1).

       FIM-MONTA-MOLDURAS. EXIT.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENG.
