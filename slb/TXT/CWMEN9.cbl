       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN9 INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/07/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manipular relatorios                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWDIRS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWDIRS-SPOOL
                  ALTERNATE RECORD KEY IS CWDIRS-FOLHAS  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-NOTA    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-USUARIO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-CODIGO  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-EMISSAO WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CWDIRS.

           SELECT PRINTS ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  FILE STATUS   IS FS-PRINTS.

           SELECT TEXTO ASSIGN TO DISK
                  LOCK MODE    IS EXCLUSIVE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-TEXTO.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OPTIONAL REPKEY ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS REPKEY-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-REPKEY.

       DATA DIVISION.
       FILE SECTION.

       COPY CWDIRS.

       FD  PRINTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PRINTS.

       01  PRINTS-REG                  PIC X(001).

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG PIC X(503).

       FD  REPKEY
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-REPKEY.

       01  REPKEY-REG.
           05 REPKEY-CHAVE             PIC  9(006).
           05 REPKEY-CW                PIC  X(002).
           05 REPKEY-NUMERO            PIC  9(006).
           05 REPKEY-EXT               PIC  X(003).
           05 REPKEY-COMANDO           PIC  X(007).

       WORKING-STORAGE SECTION.

       01  PRNTER-REG.
           05 PRNTER-TEXTO                PIC X(550) VALUE SPACES.

       01  PRNTER-SAVE                    PIC X(550) VALUE SPACES.
       01  BIN-REG                        PIC X(550) VALUE SPACES.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 cwcode-tamanho   comp-x PIC  9(002) VALUE 0.
           05 EXPORTANDO          PIC  9(001) VALUE ZERO.
           05 AUTOV               PIC  9(001) VALUE ZERO.
           05 PATH-IN             PIC  X(001) VALUE space.
           05 DELIM               PIC  X(001) VALUE '|'.
           05 CWSPOOLHANDLER      PIC  X(255) VALUE spaces.
           05 testa-fim           PIC  X(001) VALUE space.
           05                     PIC  X(001) VALUE '0'.
              88 SPOOL-TEST VALUE '1'.
              88 SPOOL-OK   VALUE '2'.
              88 SPOOL-UNIX VALUE '3'.
              88 SPOOL-DOS  VALUE '4'.
           05 file-handle              PIC  X(004) VALUE high-values.
           05 file-offset              pic  x(8) comp-x.
           05 byte-count               pic  x(4) comp-x.
           05 texto-handle             PIC  X(004) VALUE high-values.
           05 texto-offset             pic  x(8) comp-x.
           05 texto-count              pic  x(4) comp-x VALUE 1.
           05 SZ                       PIC  9(003) VALUE 0.
           05 NIVEL                    PIC  9(001) VALUE 0.
           05 USERID                   PIC  X(010) VALUE SPACES.
           05 SPOOLUSER                PIC  X(003) VALUE SPACES.
           05 SPOOLVIEW                PIC  X(050) VALUE SPACES.
           05 SPOOLVIEW-LEN            PIC  9(002) VALUE 0.
           05 DOLAR-VIEW               PIC  9(001) VALUE 0.
           05 igual                    PIC  9(003) VALUE ZERO.
           05 L                        PIC  9(003) VALUE 0.
           05 p1                       PIC  9(003) VALUE 0.
           05 p2                       PIC  9(003) VALUE 0.
           05 p3                       PIC  9(003) VALUE 0.
           05 p4                       PIC  9(003) VALUE 0.
           05 POSTER-BUFFER.
              10 POSTER-LINE           PIC  X(050) OCCURS 3.
           05 PS                       PIC  9(002) VALUE 0.
           05 CWPRINTFONT              PIC  X(050) VALUE SPACES.
           05 GRUPO-ID                 PIC  X(005) VALUE SPACES.
           05 X91-SRCOUT-RESULT        PIC  9(002) COMP-X.
           05 X91-SRCOUT-FUNCTION      PIC  9(002) COMP-X VALUE 47.
           05 NEW                      PIC  X(255) VALUE SPACES.
           05 EXTW                     PIC  X(004) VALUE SPACES.
           05 AFTER-0                  PIC  9(001) VALUE 0.
           05 EMAIL-TEXT               PIC  X(255) VALUE SPACES.
           05 ARROBA                   PIC  9(002) VALUE 0.
      *    05 PONTOMAIL                PIC  9(002) VALUE 0.
           05 E-MAIL                   PIC  X(050) VALUE SPACES.
           05 TITULO-W                 PIC  X(174) VALUE SPACES.
           05 CWSPLTXT                 PIC  X(003) VALUE SPACES.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 M                    PIC  9(002) VALUE 0.
           05 MC                   PIC  9(002) VALUE 0.
           05 TESTE-78             PIC  X(078) VALUE SPACES.
           05 FIRST-FLAG           PIC  9(001) VALUE 0.
           05 ORIENTACAO           PIC  X(001) VALUE "R".
           05 POSTER-C             PIC  X(050) VALUE SPACES.
           05 WINPRINT                         VALUE SPACES.
              10                   PIC  X(008).
              88 POSTER VALUE "WINDOWS " "WINDOWS:"
                              "USBTEXT " "USBTEXT:"
                              "WINVIEW ".
              10                   PIC  X(042).
           05 WINDOWS-PRINTER      PIC  X(042) VALUE SPACES.
           05 COBWARE              PIC  X(050) VALUE SPACES.
           05 COBOLWARE-DLL        PIC  X(050) VALUE SPACES.
           05 CANCELAR             PIC  X(070) VALUE "[esc]-Cancelar".
           05 SC                   PIC  X(004) VALUE SPACES.
           05 SALVA-70             PIC  X(070) VALUE SPACES.
           05 DELETE-PRINTER       PIC  X(001) VALUE SPACE.
           05 RETURN-STATUS        PIC  9(002) COMP-5 VALUE 0.
           05 SIZE-OLD-DIR         PIC  9(002) COMP-5 VALUE 50.
           05 OLD-DRIVE            PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY        PIC  X(050) VALUE SPACES.
           05 PATH-SPOOL           PIC  X(050) VALUE SPACES.
           05 POP                  PIC  X(080) VALUE SPACES.
           05 ER-TEXTO.
              10 FS-TEXTO              PIC  X(002) VALUE "00".
              10 LB-TEXTO              PIC  X(255) VALUE SPACES.
           05 SETAS-SORT.
              10 SETA-DOWN             PIC  X(001) VALUE X"19".
              10 SETA-UP               PIC  X(001) VALUE X"18".
           05 SETAS-DOS                PIC  X(004) VALUE X"18191B1A".
           05 SETAS-UNIX               PIC  X(004) VALUE "^v<>".
           05 SAVE-SPOOL-OPTIONS       PIC  X(004) VALUE "0000".
           05 SPOOL-OPTIONS.
              10 ZEBRADO               PIC  9(001) VALUE 0.
              10 FULL-SCREEN           PIC  9(001) VALUE 0.
              10 COLUNA-SORT           PIC  9(001) VALUE 0.
              10 OPCAO-SORT            PIC  9(001) VALUE 0.
           05 REORDENAR                PIC  9(001) VALUE 0.
           05 EXPORTOU                 PIC  9(001) VALUE 0.
           05 FULL-LIMITE              PIC  9(002) VALUE 15.
           05 LINE-RODAPE              PIC  9(002) VALUE 23.
           05 LINE-LISTAR              PIC  9(002) VALUE 23.
           05 ZEBRA                    PIC  X(080) VALUE ALL X"70".
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 VEZ-NOTA                 PIC  9(001) VALUE 0.
           05 LINE-NOTA                PIC  9(002) VALUE 0.
           05 POS-W                    PIC  X(006) VALUE SPACES.
           05 SAVE-NOTA                PIC  X(020) VALUE SPACES.
           05 SAVE-NOTA2               PIC  X(020) VALUE SPACES.
           05 SAVE-RELATORIO           PIC  X(082) VALUE SPACES.
           05 PRE-RELATORIO            PIC  X(082) VALUE SPACES.
           05                          PIC  X(001) VALUE SPACE.
              88 SINGLE-REPORT                     VALUE "1".
              88 MULTI-REPORTS                     VALUE "0".
           05                          PIC  X(001) VALUE SPACE.
              88 IMPRIMIU                          VALUE "1".
              88 NAO-IMPRIMIU                      VALUE "0".
           05 TAMANHO                  PIC  9(003) VALUE 0.
           05 PRINTS-WORK              PIC  X(012) VALUE SPACES.
           05 SPOOL-JOB                PIC  X(255) VALUE SPACES.
           05 SPOOL-DEV                PIC  X(001) VALUE SPACE.
           05 SPOOL-WORK               PIC  X(050) VALUE SPACE.
           05 SPOOL-REMOTO             PIC  X(050) VALUE SPACE.
           05 SPOOL-CMD                PIC  X(050) VALUE SPACE.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 CARACTER-BUFFER2     PIC X(2000) VALUE SPACES.
              10 CARACTER-BUFFER3     PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER2    PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER3    PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
              10 STRING-LENGTH2       PIC  9(004) COMP-X VALUE 78.
              10 STRING-START         PIC  9(004) COMP-X VALUE 1.
           05 SPOOL-COMMAND            PIC  X(255) VALUE SPACES.
           05 FUNCAO-35         COMP-X PIC  9(002) VALUE 35.
           05 G                   COMP PIC  9(006) VALUE ZERO.
           05 GRUPO-TESTE                          VALUE SPACES.
              10 GRUPO-BYTE OCCURS 8   PIC  X(001).
           05 GRUPO-PRT                PIC  X(008) VALUE SPACES.
           05 GRUPO                    PIC  X(008) VALUE SPACES.
           05 FLAG-GRUPO               PIC  9(001) VALUE 0.
              88 GRUPO-ERRADO                      VALUE 1.
           05 CURPOS.
              10 CURPOS-LIN            PIC  9(002) VALUE ZERO.
              10 CURPOS-COL            PIC  9(002) VALUE ZERO.
           05 TMP                      PIC  X(050) VALUE SPACES.
           05 TMP-LB                   PIC  X(012) VALUE "CW400000.TMP".
           05 RESULTADO         COMP-X PIC  9(002) VALUE ZERO.
           05 FUNCAO-47         COMP-X PIC  9(002) VALUE 47.
           05 PRINTS-WS                PIC  X(550) VALUE SPACES.
           05 PRINTS-WS2               PIC  X(550) VALUE SPACES.
           05 PRINTS-WS3               PIC  X(550) VALUE SPACES.
           05 PRINT-X                  PIC  9(002) COMP-X.
           05 E1                       PIC  9(003) VALUE 0.
           05 E2                       PIC  9(002) VALUE 0.
           05 SP-X                     PIC  X(001) VALUE SPACE.
           05 ASCII-I                  PIC  X(050) VALUE LOW-VALUES.
           05 ASCII-F                  PIC  X(050) VALUE LOW-VALUES.
           05 OLD-CWDIRS               PIC  X(050) VALUE SPACES.
           05 SAVE-CWDIRS              PIC  X(050) VALUE SPACES.
           05 VEZ-CWDIRS               PIC  9(001) VALUE 1.
           05 SALVA-LB-PRNTER          PIC  X(255) VALUE SPACES.
           05 SALTO                    PIC  9(002) VALUE 0.
      *    05 LINHAS                   PIC  9(002) VALUE 1.
           05 EJECT-MODE               PIC  X(002) VALUE "??".
           05 EM-LINHA                 PIC  X(001) VALUE "S".
           05 PEDE-IMPRESSORA          PIC  9(002) COMP-X VALUE 0.
           05 IMPRESSORA               PIC  X(030) VALUE SPACES.
           05 CURSOR-POSITION.
              15 ROW-CURSOR            PIC  9(002) COMP-X VALUE 00.
              15 COLUMN-CURSOR         PIC  9(002) COMP-X VALUE 00.
           05 SCREEN-POSITION.
              10 ROW-NUMBER            PIC  9(002) COMP-X VALUE 08.
              10 COLUMN-NUMBER         PIC  9(002) COMP-X VALUE 01.
           05 SCREEN-POSITION2.
              10 ROW-NUMBER2           PIC  9(002) COMP-X VALUE 08.
              10 COLUMN-NUMBER2        PIC  9(002) COMP-X VALUE 01.
           05 ATT-T                    PIC  X(001) VALUE X"07".
           05 ATT-T2                   PIC  X(080) VALUE SPACES.
           05 SIZE-ATT-T               PIC  9(004) COMP-X VALUE 1.
           05 CWPURGEVIEW              PIC  X(003) VALUE 'OFF'.
           05 CWPURGEPRINT             PIC  X(003) VALUE 'OFF'.
           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 REVERTE-VIDEO            PIC  X(001) VALUE SPACE.
           05 VERMELHO                 PIC  X(001) VALUE SPACE.
           05 VERDE                    PIC  X(001) VALUE SPACE.
           05 X0E                      PIC  X(001) VALUE SPACE.
           05 ZEBRA-1                  PIC  X(080) VALUE SPACES.
           05 ZEBRA-2                  PIC  X(080) VALUE SPACES.
           05 ZEBRA-3                  PIC  X(080) VALUE SPACES.
           05 ZEBRA-4                  PIC  X(080) VALUE SPACES.

           05 SIZE-BARR-MENU           PIC  9(004) COMP-X VALUE 78.
           05 ERASE-CURSOR.
              15                       PIC  9(002) COMP-X VALUE 255.
              15                       PIC  9(002) COMP-X VALUE 255.
           05 CX                       PIC  9(002) COMP-X VALUE 0.
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 CHAR                     PIC  X(001) VALUE SPACE.
           05 CHAR-X REDEFINES CHAR    PIC  9(002) COMP-X.
           05 PRINTER-NO               PIC  9(002) COMP-X VALUE 0.
           05 PRINTER-STATUS           PIC  9(002) COMP-X VALUE 0.
           05 V1                       PIC  9(002) VALUE 0.
           05 V2                       PIC  9(002) VALUE 0.
           05 width                    PIC  9(003) VALUE 0.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWEDIT.
           05 FIM-PROCURA              PIC  9(001) VALUE ZERO.
           05 SALVA-RT                 PIC  X(001) VALUE SPACE.
           05 TESTE-PROCURA            PIC  9(001) VALUE ZERO.
           05 PONTO                    PIC  9(010) VALUE ZERO.
           05 P-I-Z                    PIC  Z(004) VALUE ZERO.
           05 P-F-Z                    PIC  Z(004) VALUE ZERO.
           05 P-I                      PIC  9(004) VALUE ZERO.
           05 P-F                      PIC  9(004) VALUE ZERO.
           05 N-L                      PIC  9(010) VALUE ZERO.
           05 PAGINA-LIVRE             PIC  9(004) VALUE ZERO.
           05 PG                       PIC  9(004) VALUE ZERO.
           05 REDEFINES PG.
              10 PG-1                  PIC  X(001).
              10 PG-2                  PIC  X(001).
              10 PG-3                  PIC  X(001).
              10 PG-4                  PIC  X(001).
           05 LP                       PIC  9(010) VALUE ZERO.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
           05 OBS.
              10 OBS-CODIGO            PIC  X(007) VALUE SPACES.
              10 FILLER                PIC  X(001) VALUE SPACE.
              10 OBS-DATA              PIC  99/99/9999.
              10 FILLER                PIC  X(001) VALUE SPACE.
              10 OBS-HH                PIC  9(002).
              10 FILLER                PIC  X(001) VALUE ":".
              10 OBS-MM                PIC  9(002).
              10 FILLER                PIC  X(005) VALUE " via ".
              10 OBS-VIA               PIC  Z.ZZZ.ZZZ.
              10 FILLER                PIC  X(004) VALUE " de ".
              10 OBS-VIAS              PIC  Z.ZZZ.ZZZ.
           05 REDEFINES OBS.
              10 BYTE-OBS OCCURS 68    PIC  X(001).
           05 OBS-PRINTS               PIC  X(070) VALUE SPACES.
           05 CADEIA2                  PIC  X(062) VALUE ALL "?".
           05 CADEIA                   PIC  X(062) VALUE SPACES.
           05 REDEFINES CADEIA.
              10 BYTE-S OCCURS 62      PIC  9(002) COMP-X.
           05 IGUAIS                   PIC  9(006) VALUE ZERO.
           05 TC                       PIC  9(006) VALUE ZERO.
           05 VEZ                      PIC  9(001) VALUE ZERO.
           05 LISTE                    PIC  9(001) VALUE ZERO.
           05 VEZES                    PIC  9(010) VALUE ZERO.
           05 COMANDO-A                PIC  X(007) VALUE SPACES.
           05 MODO                     PIC  X(070) VALUE SPACES.
           05 COMANDO-E VALUE SPACES.
              10 BYTE-C OCCURS 7       PIC  X(001).
           05 REDEFINES COMANDO-E.
              10 BYTE-X OCCURS 7       PIC  9(002) COMP-X.
           05 COMANDO-T VALUE SPACES.
              10 BYTE-T OCCURS 7       PIC  X(001).
           05 REDEFINES COMANDO-T      PIC  X(007).
              88 COMANDO-OK                        VALUE
                 "EXPORTA" "EXPORT" "EXPOR" "EXPO" "EXP" "EX" "E"
                 "IMPORTA" "IMPORT" "IMPOR" "IMPO" "IMP" "IM" "I"
                 "NOTA"   "NOT"     "NO"    "N"
                 "ERASE"  "ERAS"    "ERA"   "ER"
                 "ERASE*" "ERAS*"   "ERA*"  "ER*" "E*"
                 "APAGAR" "APAGA"   "APAG"  "APA" "AP" "A*"
                 "APAGAR*" "APAGA*" "APAG*" "APA*" "AP*"
                 "APAGE" "APAGE*"
                 "DELETE"  "DELET"  "DELE"  "DEL"  "DE"
                 "DELETE*" "DELET*" "DELE*" "DEL*" "DE*" "D*"
                 "DROP"
                 "PURGE"  "PURG"  "PUR"  "PU"
                 "PURGE*" "PURG*" "PUR*" "PU*"
                 "PRINT"  "PRIN"  "PRI"  "PR"
                 "PRINTD" "PRIND" "PRID" "PRD" "PD"
                 "LISTE"  "LIST"  "LI"
                 "VIEW"   "VIE"   "VI"
                 "VER"    "VE"    "V"
                 "=" "*"
                 "BROWSE" "BROWS" "BROW" "BRO" "BR" " ".
              88 NOTAS    VALUE "NOTA" "NOTE" "NOT"     "NO"    "N".
              88 EXPORTA  VALUE
                 "EXPORTA" "EXPORT" "EXPOR" "EXPO" "EXP" "EX" "E".
              88 IMPORTA  VALUE
                 "IMPORTA" "IMPORT" "IMPOR" "IMPO" "IMP" "IM" "I".
              88 VER      VALUE
                 "VIEW"   "VIE"   "VI"
                 "VER"    "VE"    "V"
                 "BROWSE" "BROWS" "BROW" "BRO" "BR" " ".
              88 LISTAR   VALUE
                 "PRINT"  "PRIN"  "PRI"  "PR"
                 "PRINTD" "PRIND" "PRID" "PRD" "PD"
                 "LISTE"  "LIST"  "LI".
              88 REMOVER  VALUE
                 "ERASE"  "ERAS"  "ERA"  "ER"
                 "APAGAR" "APAGA" "APAG" "APA" "AP"
                 "APAGE"
                 "DELETE" "DELET" "DELE" "DEL" "DE"
                 "PURGE"  "PURG"  "PUR"  "PU"
                 "DROP".
              88 REMOVEALL VALUE
                 "ERASE*" "ERAS*"   "ERA*"  "ER*" "E*"
                 "APAGAR*" "APAGA*" "APAG*" "APA*" "AP*"
                 "APAGE*"
                 "DELETE*" "DELET*" "DELE*" "DEL*" "DE*" "D*"
                 "PURGE*" "PURG*" "PUR*" "PU*".
              88 PRINTDEL VALUE
                 "PRINTD" "PRIND" "PRID" "PRD" "PD".
           05 CMD                      PIC  X(001) VALUE SPACE.
              88 CMD-MODOS VALUE "F" "f" "Z" "z" "O" "o" "I" "i".
           05 ER-PRNTER.
              10 FS-PRNTER             PIC  X(002) VALUE "00".
              10 LB-PRNTER             PIC  X(255) VALUE "????".
           05 ER-CWDIRS.
              10 FS-CWDIRS             PIC  X(002) VALUE "00".
              10 LB-CWDIRS             PIC  X(255) VALUE "CWDIRS".
           05 LB-PRINTS-TEST           PIC  X(006) VALUE SPACES.
              88 DOS-DEVICE VALUE "PRN"  "PRN:" "PRN."
                                  "LPT1" "COM1" "LPT1:" "COM1:"
                                  "LPT2" "COM2" "LPT2:" "COM2:"
                                  "LPT3" "COM3" "LPT3:" "COM3:"
                                  "LPT4" "COM4" "LPT4:" "COM4:"
                                  "LPT5" "COM5" "LPT5:" "COM5:"
                                  "LPT6" "COM6" "LPT6:" "COM6:"
                                  "LPT7" "COM7" "LPT7:" "COM7:"
                                  "LPT8" "COM8" "LPT8:" "COM8:"
                                  "LPT9" "COM9" "LPT9:" "COM9:".
           05 ER-PRINTS.
              10 FS-PRINTS             PIC  X(002) VALUE "00".
              10 LB-PRINTS.
                 15 FILLER             PIC  X(005) VALUE SPACES.
                    88 IMP-0 VALUE "PRN:" "LPT1:" "COM1:".
                    88 IMP-1 VALUE        "LPT2:" "COM2:".
                    88 IMP-2 VALUE        "LPT3:".
                 15 FILLER             PIC  X(250) VALUE SPACES.
           05 PRONTO                   PIC  9(002) VALUE 1.
           05 VIA                      PIC  9(007) VALUE ZERO.
           05 VIAS                     PIC  9(007) VALUE ZERO.
           05 REDEFINES VIAS.
              10 BYTE-VIA OCCURS 7     PIC  X(001).
           05 TR                       PIC  9(004) VALUE ZERO.
           05 T                        PIC  9(004) VALUE ZERO.
           05 LINHA                    PIC  9(004) VALUE ZERO.
           05 PX-PLUS                  PIC  9(004) VALUE ZERO.
           05 C                   COMP PIC  9(010) VALUE ZERO.
           05 LIMITE              COMP PIC  9(004) VALUE ZERO.
           05 LIMITE-t            COMP PIC  9(004) VALUE ZERO.
           05 CI                  COMP PIC  9(004) VALUE ZERO.
           05 COLUNA              COMP PIC  9(004) VALUE 1.
           05 CO                  COMP PIC  9(004) VALUE ZERO.
           05 CO2                 COMP PIC  9(004) VALUE ZERO.
           05 RK-PRNTER                PIC  9(010) VALUE ZERO.
           05 SIZE-REPKEY         COMP PIC  9(004) VALUE ZERO.
           05 PX                  COMP PIC  9(010) VALUE ZERO.
           05 P                   COMP PIC  9(010) VALUE ZERO.
           05 Y                   COMP PIC  9(006) VALUE ZERO.
           05 I                   COMP PIC  9(006) VALUE ZERO.
           05 W                   COMP PIC  9(006) VALUE ZERO.
           05 E                   COMP PIC  9(006) VALUE ZERO.
           05 S1                       PIC  9(003) VALUE ZERO.
           05 S2                       PIC  9(003) VALUE ZERO.
           05 S3                       PIC  9(003) VALUE ZERO.
           05 S4                       PIC  9(003) VALUE ZERO.
           05 R                   COMP PIC  9(006) VALUE ZERO.
           05 R2                  COMP PIC  9(006) VALUE ZERO.
           05 D2                  COMP PIC  9(006) VALUE ZERO.
           05 A                   COMP PIC  9(006) VALUE ZERO.
           05 A3                  COMP PIC  9(006) VALUE ZERO.
           05 I-SPOOL             COMP PIC  9(006) VALUE ZERO.
           05 X                   COMP PIC  9(006) VALUE ZERO.
           05 PONTEIRO            COMP PIC  9(010) VALUE 1.
           05 SALVA-PONTEIRO      COMP PIC  9(010) VALUE 1.
           05 SALVA-PONTEIRO-2    COMP PIC  9(010) VALUE 1.
           05 PRONTO-A            COMP PIC  9(006) VALUE 1.
           05 PRONTO-S            COMP PIC  9(006) VALUE 1.
           05 TEXTO-TELA                           VALUE SPACES.
              10 TEXTO2 OCCURS 24      PIC  X(080).
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 CWMENU                   PIC  X(001) VALUE "?".
           05 ER-REPKEY.
              10 FS-REPKEY             PIC  X(002) VALUE "00".
              10 LB-REPKEY             PIC  X(255) VALUE SPACES.
           05 GET-OS.
              10 PARAMETER-SIZE      PIC 9(004) COMP-X VALUE 14.
              10 P-OS-TYPE           PIC 9(002) COMP-X.
                 88 DOS VALUE 1 2 131.
              10 P-OS-VERSION        PIC 9(008) COMP-X.
              10 P-DBCS-SUPPORT      PIC 9(002) COMP-X.
              10 P-CHAR-CODING       PIC 9(002) COMP-X.
              10 P-COUNTRY-ID        PIC 9(004) COMP-X.
              10 P-CODE-PAGE         PIC 9(004) COMP-X.
              10 P-PROCESS-TYPE      PIC 9(002) COMP-X.
           05  ESTILO PIC X(30) VALUE SPACES.
           05  ESTILOS VALUE ALL "0".
               10 OCCURS 16.
                  15 OCCURS 16.
                     20 ASC PIC 9(003).
           05 FORMATO                        PIC  X(001) VALUE SPACE.
              88 FORMATO-DOS  VALUE "D".
              88 FORMATO-UNIX VALUE "U".
           05 CODEPAGE                       PIC  X(001) VALUE SPACE.
              88 CODEPAGE-001     VALUE "0".
              88 CODEPAGE-OFF     VALUE "O".
              88 CODEPAGE-WINDOWS VALUE "W".
           05 BARRA-DIR       PIC X VALUE SPACE.
           05 TEMP            PIC X(50) VALUE SPACES.
           05 SEM-PATH        PIC X(50) VALUE SPACES.
           05 MP-I            PIC 99 VALUE 0.
           05 MP-Y            PIC 99 VALUE 0.
           05 PATH            PIC X(255).
           05 PATH-MD         PIC X(255) VALUE SPACES.

       01  AREAS-DE-TRABALHO-2.
           05 TOPO PIC X(077) VALUE "Comando C¢digo  Tipo    Data     Ho
      -       "ra P ginas Nota                 Usu rio".
           05 MSG-1 PIC X(11) VALUE "[esc]-Sa¡da".

       01  XML-REG.
           05 Codigo       PIC X(007) VALUE SPACES.
           05 Titulo       PIC X(077) VALUE SPACES.
           05 Controle     PIC X(007) VALUE SPACES.
           05 Paginas      PIC 9(006) VALUE 0.
           05 Linhas       PIC 9(006) VALUE 0.
           05 Largura      PIC 9(003) VALUE 0.
           05 Geracao      PIC X(010) VALUE SPACES.
           05 Hora         PIC X(008) VALUE SPACES.
           05 Proprietario PIC X(010) VALUE SPACES.
           05 Observacao   PIC X(020) VALUE SPACES.
           05 Arquivo      PIC X(255) VALUE SPACES.

       01  DIRETORIO-TELA VALUE SPACES.
           05 RELATORIO OCCURS 12.
              10 FILLER                         PIC  X(001).
              10 COMANDO.
                 15 BYTE-COMANDO OCCURS 7       PIC  X(001).
              10 IMPRESSO                       PIC  X(001).
              10 CWDIRSS                        PIC  X(007).
              10 FILLER                         PIC  X(001).
              10 TIPO.
                 15 TIPO-LEN                    PIC  Z(003).
                 15 TIPO-FLAG                   PIC  X(001).
              10 FILLER                         PIC  X(001).
              10 EMISSAO.
                 15 DATAS                       PIC  99/99/9999B.
                 15 HH                          PIC  9(002).
                 15 PT                          PIC  X(001).
                 15 MM                          PIC  9(002).
              10 FILLER                         PIC  X(001).
              10 PAGINA                         PIC  ZZZ.ZZZ.
              10 FILLER                         PIC  X(001).
              10 NOTA                           PIC  X(020).
              10 FILLER                         PIC  X(001).
              10 USUARIO                        PIC  X(010).
              10 CHAVE-REPKEY                   PIC  9(004).
              10 NUMERO BLANK ZERO              PIC  9(006).
              10 EXT                            PIC  X(003).
              10 TITULO-T                       PIC  X(080).
              10 CW                             PIC  X(002).

       01  SETAS.
           05 PIC X(01) VALUE SPACE.
           05 PIC X(06) VALUE "Lista ".
           05 PIC X(01) VALUE X"18".
           05 PIC X(01) VALUE SPACE.
           05 PIC X(01) VALUE X"19".
           05 PIC X(01) VALUE SPACE.
           05 PIC X(01) VALUE X"1B".
           05 PIC X(01) VALUE SPACE.
           05 PIC X(01) VALUE X"1A".
           05 PIC X(01) VALUE SPACE.
           05 PIC X(05) VALUE "[Tab]".
           05 PIC X(01) VALUE SPACE.
           05 PIC X(05) VALUE "Enter".
           05 PIC X(01) VALUE " ".
           05 PIC X(42) VALUE
              "PgUp PgDn Home End Full Pesquisa Zebra Esc".

       01  SETAS-MOUSE.
           05 PIC X(01) VALUE X"00".
           05 PIC X(05) VALUE "LLLLL".
           05 PIC X(01) VALUE X"00".
           05 PIC X(01) VALUE ALL X"AC".
           05 PIC X(01) VALUE X"00".
           05 PIC X(01) VALUE ALL X"B4".
           05 PIC X(01) VALUE X"00".
           05 PIC X(01) VALUE ALL X"AF".
           05 PIC X(01) VALUE X"00".
           05 PIC X(01) VALUE ALL X"B1".
           05 PIC X(01) VALUE X"00".
           05 PIC X(05) VALUE ALL X"D7".
           05 PIC X(01) VALUE X"00".
           05 PIC X(05) VALUE ALL X"D8".
           05 PIC X(01) VALUE X"00".
           05 PIC X(04) VALUE ALL X"AD".
           05 PIC X(01) VALUE X"00".
           05 PIC X(04) VALUE ALL X"B5".
           05 PIC X(01) VALUE X"00".
           05 PIC X(04) VALUE ALL X"AB".
           05 PIC X(01) VALUE X"00".
           05 PIC X(03) VALUE ALL X"B3".
           05 PIC X(01) VALUE X"00".
           05 PIC X(04) VALUE "FFFF".
           05 PIC X(01) VALUE X"00".
           05 PIC X(08) VALUE "PPPPPPPP".
           05 PIC X(01) VALUE X"00".
           05 PIC X(06) VALUE "ZZZZZ ".
           05 PIC X(04) VALUE X"7F7F7F".

       COPY CWMOUS.
       COPY CWBOXC.
       COPY CWBOXS.
       COPY CWSEND.
       COPY CWUNIX.
       COPY CWTIME.
       COPY CWPATH.
       COPY CWPERC.
       COPY CWBOXW.
       COPY CWEXEC.
       COPY CWGETS.
       COPY CWCONF.

       LINKAGE SECTION.

       01  LK-SPOOL.
              10 spool-CW             PIC  X(002).
              10 spool-NUMERO         PIC  9(006).
              10 spool-PONTO          PIC  X(001).
              10 spool-EXT            PIC  X(003).

       PROCEDURE DIVISION USING LK-SPOOL.

       000-INICIO.

           EXEC COBOLware GetSystem
                LEVEL;NIVEL
                USER;USERID
           END-EXEC
           CALL "CWUNIX"         USING PARAMETROS-CWUNIX
           DISPLAY "CWSPOOLHANDLER" UPON ENVIRONMENT-NAME
           ACCEPT SPOOL-JOB FROM ENVIRONMENT-VALUE
           IF  SPOOL-JOB NOT = SPACES
               IF   NOT CWUNIX-ON
                    CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                RETURN-STATUS
               END-IF
               CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                           SIZE-OLD-DIR
               PERFORM VARYING SPOOLVIEW-LEN FROM LENGTH OF SPOOL-JOB
                        BY -1
                       UNTIL SPOOL-JOB (SPOOLVIEW-LEN: 1) NOT = SPACE
                       CONTINUE
               END-PERFORM
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > SPOOLVIEW-LEN
                       ADD 1 TO Y
                       IF SPOOL-JOB (I:1) = '?' OR '!'
                          MOVE SPOOL-JOB (I:1) TO PATH-IN
                          IF LB-TEXTO = SPACES
                             ADD  1 TO I
                             MOVE 0 TO L
                             PERFORM VARYING I FROM I BY 1
                                     UNTIL I > SPOOLVIEW-LEN
                                        OR SPOOL-JOB (I:1) = SPACE
                                     ADD 1 TO L
                                     MOVE SPOOL-JOB (I:1)
                                       TO LB-TEXTO (L: 1)
                             END-PERFORM
                          END-IF
                          IF LB-TEXTO = SPACES
                             MOVE 'cwdir###.xml' TO LB-TEXTO
                          END-IF
                          CALL 'CWFILE' USING LB-TEXTO
                          MOVE LB-TEXTO TO CWSPOOLHANDLER (Y:)
                          PERFORM VARYING Y
                                     FROM LENGTH OF CWSPOOLHANDLER
                                   BY -1
                                  UNTIL CWSPOOLHANDLER(Y: 1) NOT = SPACE
                                  CONTINUE
                          END-PERFORM
                          ADD 1 TO Y
                       ELSE
                          IF SPOOL-JOB (I:1) = '$'
                             ADD  1              TO I
                             MOVE SPOOL-JOB(I:1) TO DELIM
                          ELSE
                             MOVE SPOOL-JOB(I:1) TO CWSPOOLHANDLER(Y:1)
                          END-IF
                       END-IF
               END-PERFORM
               MOVE SPACES TO SPOOL-JOB
           END-IF
           DISPLAY "CWSPOOLUSER" UPON ENVIRONMENT-NAME
           ACCEPT SPOOLUSER      FROM ENVIRONMENT-VALUE
           INSPECT  SPOOLUSER  CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWSPOOLVIEW" UPON ENVIRONMENT-NAME
           ACCEPT SPOOLVIEW      FROM ENVIRONMENT-VALUE
           IF  SPOOLVIEW NOT = SPACES
               PERFORM VARYING SPOOLVIEW-LEN FROM LENGTH OF SPOOLVIEW
                        BY -1
                       UNTIL SPOOLVIEW (SPOOLVIEW-LEN: 1) NOT = SPACE
                       CONTINUE
               END-PERFORM
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > SPOOLVIEW-LEN
                       IF SPOOLVIEW (I:1) = '$'
                          MOVE 1 TO DOLAR-VIEW
                          EXIT PERFORM
                       END-IF
               END-PERFORM
           END-IF
           DISPLAY "CWPRINTFONT" UPON ENVIRONMENT-NAME
           ACCEPT CWPRINTFONT  FROM ENVIRONMENT-VALUE
           DISPLAY "CWLINES" UPON ENVIRONMENT-NAME
           DISPLAY "CWSPLTXT" UPON ENVIRONMENT-NAME
           ACCEPT CWSPLTXT    FROM ENVIRONMENT-VALUE
           IF   CWSPLTXT = SPACES OR LOW-VALUES
                MOVE "txt" TO CWSPLTXT
           END-IF
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER NOT = 0
                IF   spool-NUMERO NUMERIC
                AND  spool-NUMERO > 0
                     MOVE 1 TO AUTOV
                     CALL "CBL_READ_SCR_CHATTRS" USING X"0000"
                                             CARACTER-BUFFER3
                                             ATTRIBUTE-BUFFER3
                                             STRING-LENGTH
                     DISPLAY "CWPURGEVIEW" UPON ENVIRONMENT-NAME
                     ACCEPT   CWPURGEVIEW  FROM ENVIRONMENT-VALUE
                     INSPECT  CWPURGEVIEW  CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                END-IF
           END-IF
           DISPLAY "CWPURGEPRINT" UPON ENVIRONMENT-NAME
           ACCEPT   CWPURGEPRINT  FROM ENVIRONMENT-VALUE
           INSPECT  CWPURGEPRINT  CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
           DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
           ACCEPT COBWARE FROM ENVIRONMENT-VALUE
           CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                           ATT-T
                                           SIZE-ATT-T
           INSPECT ATT-T2 CONVERTING SPACE TO ATT-T
           CALL "CBL_READ_SCR_ATTRS" USING X"1602" SALVA-70 X"0046"

           CALL "CWGETS"         USING PARAMETROS-CWGETS
           IF   CWUNIX-ON
                INSPECT SETAS-SORT CONVERTING SETAS-DOS TO SETAS-UNIX
                INSPECT SETAS      CONVERTING SETAS-DOS TO SETAS-UNIX
                MOVE "/" TO BARRA-DIR
           ELSE
                MOVE "\" TO BARRA-DIR
           END-IF
           CALL "CWMOLD"         USING TABELA-CORES TABELA-MOLDURA
txt   *    CALL "CWTEXT"         USING AREAS-DE-TRABALHO-2
txt   *                      LENGTH OF AREAS-DE-TRABALHO-2
           MOVE COR (113)           TO REVERTE-VIDEO
           MOVE COR (015)           TO X0E
           MOVE COR (079)           TO VERMELHO
           MOVE COR (047)           TO VERDE
           INSPECT ZEBRA-1 CONVERTING SPACE TO COR (49)
           INSPECT ZEBRA-2 CONVERTING SPACE TO REVERTE-VIDEO
           INSPECT ZEBRA-3 CONVERTING SPACE TO COR (31)
           INSPECT ZEBRA-4 CONVERTING SPACE TO VERMELHO
           DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           IF   TMP = SPACES
                DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                ACCEPT  TMP       FROM ENVIRONMENT-VALUE
           END-IF

           CALL "CBL_GET_OS_INFO" USING GET-OS
                 ON EXCEPTION
                     SET DOS TO TRUE.

           MOVE "?"               TO CWMENU
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     CWMENU

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "PS"     TO CWCONF-REGPS
           MOVE OPERADOR TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE CWCONF-GRUPO         TO GRUPO
           MOVE CWCONF-SPOOL-OPTIONS TO SPOOL-OPTIONS
                                        SAVE-SPOOL-OPTIONS
           IF   FULL-SCREEN = 1
                MOVE 24 TO FULL-LIMITE
                MOVE 80 TO STRING-LENGTH2
           END-IF
           MOVE 1            TO I I-SPOOL

           IF   CWCONF-PATH-SPOOL = SPACES
                MOVE "SPOOL" TO  CWCONF-PATH-SPOOL
           ELSE
                IF   CWCONF-PATH-SPOOL (1: 1) = "\"
                AND  CWSPOOLHANDLER = SPACES
                AND  CWUNIX-OFF
                     CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                 RETURN-STATUS
                     MOVE SPACES TO PATH-SPOOL
                     STRING OLD-DRIVE ":" CWCONF-PATH-SPOOL
                                          DELIMITED BY SPACE
                            INTO PATH-SPOOL
                     MOVE PATH-SPOOL TO CWCONF-PATH-SPOOL
                END-IF
           END-IF

           IF   CWSPOOLHANDLER NOT = SPACES
           AND  PATH-IN = '?'
                IF   NOT CWUNIX-ON
                     IF CWCONF-PATH-SPOOL (2:1) = ':'
                        NEXT SENTENCE
                     END-IF
                     CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                 RETURN-STATUS
                ELSE
                     IF CWCONF-PATH-SPOOL (1:1) = '/'
                        NEXT SENTENCE
                     END-IF
                END-IF
                CALL "CBL_READ_DIR"  USING OLD-DIRECTORY
                MOVE SPACES TO SPOOL-JOB
                IF   CWUNIX-OFF
                     IF OLD-DIRECTORY (1:1) = '\'
                        STRING OLD-DRIVE ':'  DELIMITED BY SIZE
                               OLD-DIRECTORY  DELIMITED BY SPACE
                               '\'            DELIMITED BY SIZE
                               CWCONF-PATH-SPOOL   DELIMITED BY SPACE
                          INTO SPOOL-JOB
                     ELSE
                        STRING OLD-DRIVE ':\' DELIMITED BY SIZE
                               OLD-DIRECTORY  DELIMITED BY SPACE
                               '\'            DELIMITED BY SIZE
                               CWCONF-PATH-SPOOL   DELIMITED BY SPACE
                          INTO SPOOL-JOB
                     END-IF
                ELSE
                     STRING OLD-DIRECTORY  DELIMITED BY SPACE
                            '/'            DELIMITED BY SIZE
                            CWCONF-PATH-SPOOL    DELIMITED BY SPACE
                       INTO SPOOL-JOB
                END-IF
                MOVE SPOOL-JOB TO CWCONF-PATH-SPOOL
                MOVE SPACES TO SPOOL-JOB
           END-IF.

           MOVE CWCONF-PATH-SPOOL TO LB-CWDIRS PATH-SPOOL
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL LB-CWDIRS(I:1) = SPACES
                      OR I > 30
                   IF   LB-CWDIRS(I:1) = "\" OR "/"
                        MOVE BARRA-DIR TO LB-CWDIRS(I:1)
                   END-IF
           END-PERFORM
           SUBTRACT 1 FROM I
           IF   LB-CWDIRS(I:1) = "\" OR "/"
                SUBTRACT 1 FROM I
           END-IF
           ADD  1         TO I
           MOVE BARRA-DIR TO LB-CWDIRS (I: )
           ADD  1         TO I
           MOVE I         TO I-SPOOL

           MOVE SPACES        TO GRUPO-ID
           MOVE "GU"          TO CWCONF-TIPO
           MOVE CWCONF-GRUPO  TO CWCONF-ELEMENTO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF < "10"
           AND CWCONF-GRUPO-ID NUMERIC
               MOVE CWCONF-GRUPO-ID      TO GRUPO-ID
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE TASK (2: 5)     TO TMP-LB     (4: 5)
           IF   TMP NOT = SPACE
                MOVE SPACES TO LB-REPKEY
                STRING TMP       DELIMITED BY SPACE
                       BARRA-DIR DELIMITED BY SIZE
                       TMP-LB    DELIMITED BY SPACE
                  INTO LB-REPKEY
           ELSE
                MOVE TMP-LB TO LB-REPKEY
           END-IF
           MOVE "CWDIRS"     TO LB-CWDIRS     (I: )
           MOVE LB-CWDIRS    TO OLD-CWDIRS
           OPEN INPUT CWDIRS
  2747     IF   FS-CWDIRS = X"3909"
  2748          CALL "CBL_CREATE_DIR" USING PATH-SPOOL
                IF  RETURN-CODE = 0
                AND CWUNIX-ON
                    MOVE SPACES TO CWEXEC-COMANDO
                    STRING 'chmod 777 ' DELIMITED BY SIZE
                         PATH-MD DELIMITED BY SPACE
                         x'00' delimited by size
                     INTO CWEXEC-COMANDO
                    CALL 'system' USING CWEXEC-COMANDO
                END-IF
  2749     ELSE
  2750          IF   FS-CWDIRS (1: 1) = "9"
  2751               CALL "CWISAM" USING ER-CWDIRS
  2752          END-IF
  2753     END-IF
           READ CWDIRS NEXT RECORD IGNORE LOCK
           IF   FS-CWDIRS > "09"
                CLOSE CWDIRS
                PERFORM cwimpr
                MOVE SPACES                       TO CWSEND-SCREENS
                MOVE "Sem relat¢rios dispon¡veis" TO CWSEND-MSG
                MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                MOVE "~Importar"  TO CWSEND-SCREEN (2)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 2
                     OPEN OUTPUT CWDIRS
                     CLOSE CWDIRS
                     OPEN I-O CWDIRS
                     PERFORM 113-IMPORTA THRU 113-99-FIM
                END-IF
                IF   RK-PRNTER = 0
                     CLOSE CWDIRS
                     GOBACK
                END-IF
           END-IF

           CLOSE CWDIRS
           OPEN I-O CWDIRS
           PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
           IF CWSPOOLHANDLER NOT = SPACES
              CLOSE CWDIRs
              IF LB-TEXTO NOT = SPACES
                 EXEC COBOLware Save File LB-TEXTO CLOSE END-EXEC
              END-IF
              set CWEXEC-NOWARNING  to true
              set CWEXEC-ASSYNCRONE to true
              MOVE CWSPOOLHANDLER TO CWEXEC-COMMAND
              CALL "CWEXE2" USING  PARAMETROS-CWEXEC
              GOBACK
           END-IF

           CALL "CWMSGW" USING "080377" TOPO
           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1801"
                                             X0E
                                             X"004E"

           MOVE    1           TO PONTEIRO
           PERFORM 110-EXIBE THRU 110-99-FIM

           PERFORM UNTIL EDIT-ESC
             MOVE COMANDO (PRONTO) TO COMANDO-A
             PERFORM 310-PREPARA-RODAPE THRU 310-99-FIM
             PERFORM TEST AFTER UNTIL COMANDO-OK
                                   OR EDIT-ESC
                MOVE    0 TO TECLA
                MOVE    1 TO PX
                PERFORM TEST AFTER UNTIL (TECLA NOT = 0)
                                     AND (NOT EDIT-CURSOR-LEFT)
                                     AND (NOT EDIT-CURSOR-RIGHT)
                                     AND (NOT EDIT-CONTROL-END)
                                     AND (NOT EDIT-DEL)
                   MOVE    0 TO TECLA
                   IF   ROW-NUMBER NOT = PRONTO + 8
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                    SCREEN-POSITION
                                                    ATT-T
                                                    SIZE-BARR-MENU
                   END-IF
                   COMPUTE ROW-NUMBER = PRONTO + 8
                   IF   NOTA (PRONTO) = "Gerando..."
                   OR   "Terminado anormal"
                   OR   "Interrompido"
                   OR   USUARIO (PRONTO) = "Importando..."
                        IF   NOTA (PRONTO) = "Gerando..."
                        OR   USUARIO (PRONTO) = "Importando..."
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                    SCREEN-POSITION
                                                    VERDE
                                                    SIZE-BARR-MENU
                        ELSE
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                    SCREEN-POSITION
                                                    VERMELHO
                                                    SIZE-BARR-MENU
                        END-IF
                   ELSE
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                    SCREEN-POSITION
                                                    REVERTE-VIDEO
                                                    SIZE-BARR-MENU
                   END-IF
                   IF TITULO-T (PRONTO) = ALL '0'
                      MOVE SPACES TO TITULO-T (PRONTO)
                   END-IF
                   CALL "CBL_WRITE_SCR_CHARS" USING X"1801"
                                                    TITULO-T (PRONTO)
                                                    X"004E"
                   CALL "CBL_WRITE_SCR_N_ATTR" USING X"1801"
                                                     X0E
                                                     X"004E"
                   COMPUTE LINHA = PRONTO + 9
                   COMPUTE ROW-CURSOR = LINHA - 1
                   IF   PX = 1
                        MOVE 02 TO COLUMN-CURSOR
                   ELSE
                        COMPUTE PX-PLUS = PX + 1
                        COMPUTE COLUMN-CURSOR = PX-PLUS - 1
                        CALL "CBL_WRITE_SCR_CHARS" USING CURSOR-POSITION
                                            BYTE-COMANDO (PRONTO PX - 1)
                                                         X"0001"
                        ADD 1 TO COLUMN-CURSOR
                   END-IF
                   CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
==>                IF   spool-NUMERO NUMERIC
                   AND  spool-NUMERO > 0
                        MOVE 1 TO CWMOUS-KEY
                   ELSE
                        CALL "CWMOUS"  USING PARAMETROS-CWMOUS
                   END-IF
                   IF CWMOUS-KEY = 255
                      IF   CWMOUS-TIMEOUT-ON
                           SET EDIT-ESC TO TRUE
                           MOVE 0 TO KEY-STATUS
                      ELSE
                           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                      END-IF
                      IF KEY-STATUS = 1
                      OR CWUNIX-ON
                         MOVE LINHA   TO CURPOS-LIN
                         MOVE PX      TO CURPOS-COL
                         ADD  2       TO CURPOS-COL
                         CALL "CWKBDC" USING CURPOS CHAR TECLA
                         IF   EDIT-ALT-F4
                              STOP RUN
                         END-IF
                         IF   EDIT-ESC
                              MOVE SPACES TO COMANDO (PRONTO)
                              EXIT PERFORM
                         END-IF
                         EVALUATE TRUE
                          WHEN EDIT-BACKSPACE
                               MOVE 0   TO TECLA
                               MOVE " " TO BYTE-COMANDO (PRONTO PX)
                               COMPUTE PX-PLUS = PX + 2
                               COMPUTE COLUMN-CURSOR = PX-PLUS - 1
                               CALL "CBL_WRITE_SCR_CHARS"
                                      USING CURSOR-POSITION
                                            BYTE-COMANDO (PRONTO PX)
                                                         X"0001"
                               ADD 1 TO COLUMN-CURSOR
                               CALL "CBL_SET_CSR_POS"
                                    USING CURSOR-POSITION
                               IF   PX > 1
                                    SUBTRACT 1 FROM PX
                               ELSE
                                    MOVE 1  TO PX
                                    SET EDIT-CURSOR-UP TO TRUE
                               END-IF
                          WHEN TECLA = 0
                               MOVE CHAR TO BYTE-COMANDO (PRONTO PX)
                               EVALUATE TRUE
                                   WHEN (CHAR = "O" OR "o")
                                    AND  PX = 1
                                         PERFORM 112-ORDEM
                                            THRU 112-99-FIM
                                   WHEN (CHAR = "I" OR "i")
                                    AND  PX = 1
                                         PERFORM 113-IMPORTA
                                            THRU 113-99-FIM
                                   WHEN OTHER
                                    COMPUTE PX-PLUS = PX + 2
                                    COMPUTE COLUMN-CURSOR = PX-PLUS - 1
                                    CALL "CBL_WRITE_SCR_CHARS"
                                          USING CURSOR-POSITION
                                            BYTE-COMANDO (PRONTO PX)
                                                         X"0001"
                                    ADD 1 TO COLUMN-CURSOR
                                    CALL "CBL_SET_CSR_POS"
                                    USING CURSOR-POSITION
                                    IF   PX < 7
                                         ADD 1 TO PX
                                    ELSE
                                         MOVE 1  TO PX
                                         SET EDIT-CURSOR-DOWN TO TRUE
                               END-EVALUATE
                         END-EVALUATE
                      END-IF
                   ELSE
                      IF CWMOUS-KEY > 100
                         COMPUTE TECLA = CWMOUS-KEY - 100
                         IF   EDIT-ESC
                              MOVE SPACES TO COMANDO (PRONTO)
                              EXIT PERFORM
                         END-IF
                         IF   EDIT-ALT-F4
                              STOP RUN
                         END-IF
                      ELSE
                         EVALUATE CWMOUS-KEY
                            WHEN 69 IF   COMANDO (PRONTO) = "Exporta"
                                    OR   "EXPORTA"
                                         MOVE SPACES TO COMANDO (PRONTO)
                                    ELSE
                                       MOVE "Exporta" TO COMANDO(PRONTO)
                                    END-IF
                            WHEN 78 IF   COMANDO (PRONTO) = "Nota"
                                    OR   "NOTA"
                                         MOVE SPACES TO COMANDO (PRONTO)
                                    ELSE
                                         MOVE "Nota" TO COMANDO (PRONTO)
                                    END-IF
                            WHEN 79 PERFORM 112-ORDEM THRU 112-99-FIM
                            WHEN 73 PERFORM 113-IMPORTA THRU 113-99-FIM
                            WHEN 68 IF   COMANDO (PRONTO) = "Del"
                                    OR   "DEL"
                                         MOVE SPACES TO COMANDO (PRONTO)
                                    ELSE
                                         MOVE "Del"  TO COMANDO (PRONTO)
                                    END-IF
                            WHEN 80 IF   COMANDO (PRONTO) = "Print"
                                    OR   "PRINT"
                                         MOVE SPACES  TO COMANDO(PRONTO)
                                    ELSE
                                         MOVE "Print" TO COMANDO(PRONTO)
                                    END-IF
                            WHEN 86 IF   COMANDO (PRONTO) = "Ver"
                                    OR   "VER"
                                         MOVE SPACES  TO COMANDO(PRONTO)
                                    ELSE
                                         MOVE "Ver"   TO COMANDO(PRONTO)
                                    END-IF
                         END-EVALUATE
                         MOVE 2 TO COLUMN-CURSOR
                         CALL "CBL_WRITE_SCR_CHARS"
                                          USING CURSOR-POSITION
                                                COMANDO (PRONTO)
                                                X"0007"
                         ADD 1 TO COLUMN-CURSOR
                         CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                         MOVE 0       TO TECLA
                      END-IF
                   END-IF
                   MOVE 2 TO COLUMN-CURSOR
                   IF  CWMOUS-KEY > 0
                   AND CWMOUS-KEY < 12
                       MOVE CWMOUS-KEY TO PRONTO
                       MOVE "Ver"      TO COMANDO(PRONTO)
                       SET EDIT-ENTER  TO TRUE
                   END-IF
                   EVALUATE TRUE
                            WHEN EDIT-CURSOR-RIGHT
                                 IF   PX < 7
                                      ADD 1 TO PX
                                 ELSE
                                      CALL X"E5"
                                 END-IF
                            WHEN EDIT-CURSOR-LEFT
                                 IF   PX > 1
                                      SUBTRACT 1 FROM PX
                                 ELSE
                                      CALL X"E5"
                                 END-IF
                            WHEN EDIT-CONTROL-END
                                 MOVE SPACES TO COMANDO (PRONTO)
                                 MOVE ZERO   TO TECLA
                            WHEN EDIT-DEL
                                 MOVE COMANDO (PRONTO) (PX + 1:)
                                   TO COMANDO-E
                                 MOVE COMANDO-E TO COMANDO (PRONTO)
                                                   (PX: )
                                 MOVE ZERO   TO TECLA
                   END-EVALUATE
                END-PERFORM
                INSPECT COMANDO (PRONTO) CONVERTING MINUSCULAS
                                                 TO MAIUSCULAS
                CALL "CBL_WRITE_SCR_CHARS" USING CURSOR-POSITION
                                                 COMANDO (PRONTO)
                                                 X"0007"
                IF ((NOTA (PRONTO) = "Gerando...")
                OR   USUARIO (PRONTO) = "Importando...")
                AND (COMANDO (PRONTO) NOT = SPACES)
                    MOVE SPACES TO COMANDO (PRONTO)
                END-IF
                MOVE COMANDO (PRONTO) TO COMANDO-E
                MOVE SPACES           TO COMANDO-T
                MOVE ZERO             TO T
                PERFORM VARYING P FROM 1 BY 1 UNTIL P > 7
                    IF   BYTE-C (P) NOT NUMERIC
                    AND  BYTE-C (P) NOT = " "
                         ADD  1          TO T
                         MOVE BYTE-C (P) TO BYTE-T (T)
                    END-IF
                END-PERFORM
                IF   NOT COMANDO-OK
                     MOVE "?"       TO BYTE-C (1)
                     MOVE COMANDO-E TO COMANDO (PRONTO)
                     CALL "CBL_WRITE_SCR_CHARS" USING CURSOR-POSITION
                                                      COMANDO (PRONTO)
                                                      X"0007"
                ELSE
                     IF   NOT LISTAR
                          MOVE COMANDO-T TO COMANDO (PRONTO)
                          IF   NOTAS
                               MOVE NOTA (PRONTO) TO SAVE-NOTA
                               MOVE LINHA         TO LINE-NOTA
                               MOVE RELATORIO (PRONTO) TO SAVE-RELATORIO
                          END-IF
                     END-IF
                END-IF
             END-PERFORM

             IF   COMANDO (PRONTO) NOT = COMANDO-A
                  MOVE CW     (PRONTO) TO CWDIRS-CW
                  MOVE NUMERO (PRONTO) TO CWDIRS-NUMERO
                  MOVE "."             TO CWDIRS-PONTO
                  MOVE EXT    (PRONTO) TO CWDIRS-EXT
                  READ CWDIRS IGNORE LOCK KEY IS CWDIRS-SPOOL
                  MOVE CHAVE-REPKEY (PRONTO) TO REPKEY-CHAVE
                  READ REPKEY
                  IF   FS-CWDIRS < "10"
                  AND  COMANDO (PRONTO) NOT = REPKEY-COMANDO
                       MOVE COMANDO (PRONTO) TO REPKEY-COMANDO
                       REWRITE REPKEY-REG
                  END-IF
             END-IF

             CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                               ATT-T
                                               SIZE-BARR-MENU
             IF   EDIT-ENTER
                  PERFORM 120-EXEC THRU 120-99-FIM
==>               IF   SPOOL-NUMERO NUMERIC
                  AND  SPOOL-NUMERO > 0
                       SET EDIT-ESC TO TRUE
                  END-IF
             ELSE
             IF   EDIT-CURSOR-DOWN
             AND  PRONTO < 11
             AND (PONTEIRO - 1 + PRONTO) < SIZE-REPKEY
                 MOVE PRONTO TO PRONTO-A
                 ADD  1      TO PRONTO
                 PERFORM VARYING PRONTO FROM PRONTO BY 1
                         UNTIL NUMERO (PRONTO) NOT = SPACES
                            OR PRONTO = 11
                                   CONTINUE
                 END-PERFORM
                 IF   PRONTO = 11
                 AND  NUMERO (11) = SPACES
                      MOVE PRONTO-A TO PRONTO
                      IF   PONTEIRO < SIZE-REPKEY
                           ADD 1 TO PONTEIRO
                           PERFORM 110-EXIBE THRU 110-99-FIM
                      END-IF
                 END-IF
             ELSE
             IF   EDIT-CURSOR-UP
             AND  PRONTO > 1
             AND  PONTEIRO = 1
                  MOVE PRONTO TO PRONTO-A
                  SUBTRACT 1 FROM PRONTO
                  PERFORM VARYING PRONTO
                          FROM PRONTO BY -1
                          UNTIL NUMERO (PRONTO) NOT = SPACES
                             OR PRONTO = 1
                             CONTINUE
                  END-PERFORM
                  IF  NUMERO (PRONTO) = SPACES
                      MOVE PRONTO-A TO PRONTO
                  END-IF
             ELSE
             IF   EDIT-CURSOR-DOWN
             AND (PONTEIRO + 10) < SIZE-REPKEY
             AND  PRONTO   < SIZE-REPKEY
                  IF   PRONTO = 11
                       ADD 1 TO PONTEIRO
                       PERFORM 110-EXIBE THRU 110-99-FIM
                  ELSE
                       MOVE PRONTO TO PRONTO-A
                       ADD  1      TO PRONTO
                       PERFORM VARYING PRONTO FROM PRONTO BY 1
                               UNTIL NUMERO (PRONTO) NOT = SPACES
                                  OR PRONTO = 11
                       END-PERFORM
                       IF   PRONTO = 11
                       AND  NUMERO (11) = SPACES
                            MOVE PRONTO-A TO PRONTO
                            IF   PONTEIRO < SIZE-REPKEY
                                 ADD 1 TO PONTEIRO
                                 PERFORM 110-EXIBE THRU 110-99-FIM
                            END-IF
                       END-IF
                  END-IF
             ELSE
             IF   EDIT-CURSOR-UP
                  IF   PONTEIRO > 1
                  OR   PRONTO   > 1
                       IF   PRONTO = 1
                            SUBTRACT 1 FROM PONTEIRO
                            PERFORM 110-EXIBE THRU 110-99-FIM
                       ELSE
                            MOVE PRONTO TO PRONTO-A
                            SUBTRACT 1 FROM PRONTO
                            PERFORM VARYING PRONTO
                                    FROM PRONTO BY -1
                                    UNTIL NUMERO (PRONTO) NOT = SPACES
                                       OR PRONTO = 1
                            END-PERFORM
                            IF   PRONTO = 1
                            AND  NUMERO (1) = SPACES
                                 MOVE PRONTO-A TO PRONTO
                                 IF   PONTEIRO > 1
                                      SUBTRACT 1 FROM PONTEIRO
                                      PERFORM 110-EXIBE
                                         THRU 110-99-FIM
                                 END-IF
                            END-IF
                       END-IF
                  END-IF
             ELSE
             IF   EDIT-PAGE-DOWN
JO           AND (SIZE-REPKEY NOT < 11)
                  IF   (PONTEIRO + 10) < SIZE-REPKEY
                       ADD     10          TO PONTEIRO
                       MOVE    99          TO PRONTO
                       PERFORM 110-EXIBE THRU 110-99-FIM
                  ELSE
                       IF   PONTEIRO NOT = (SIZE-REPKEY - 10)
                            COMPUTE PONTEIRO = SIZE-REPKEY - 10
                            MOVE    99          TO PRONTO
                            PERFORM 110-EXIBE THRU 110-99-FIM
                       END-IF
                  END-IF
             ELSE
             IF   EDIT-PAGE-UP
JO           AND (SIZE-REPKEY NOT < 11)
                  IF   PONTEIRO > 10
                       SUBTRACT 10        FROM PONTEIRO
                       MOVE     1           TO PRONTO
                       PERFORM  110-EXIBE THRU 110-99-FIM
                  ELSE
                       IF   PONTEIRO NOT = 1
                            MOVE 1 TO PONTEIRO
                                      PRONTO
                            PERFORM 110-EXIBE THRU 110-99-FIM
                       END-IF
                  END-IF
             ELSE
             IF  (EDIT-HOME OR EDIT-CONTROL-PAGE-UP)
             AND  SIZE-REPKEY > 11
             AND  PONTEIRO NOT = 1
                  MOVE 1 TO PONTEIRO
                            PRONTO
                  PERFORM 110-EXIBE THRU 110-99-FIM
             ELSE
             IF  (EDIT-END OR EDIT-CONTROL-PAGE-DOWN)
             AND  SIZE-REPKEY > 11
             AND  PONTEIRO NOT = (SIZE-REPKEY - 10)
                  COMPUTE PONTEIRO = SIZE-REPKEY - 10
                  PERFORM 110-EXIBE THRU 110-99-FIM
                  MOVE    11 TO PRONTO
             END-IF
           END-PERFORM.

           CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                             ATT-T
                                             SIZE-BARR-MENU

           CLOSE CWDIRS
                 REPKEY DELETE FILE REPKEY

           IF  SPOOL-OPTIONS NOT = SAVE-SPOOL-OPTIONS
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                       MOVE "PS"     TO CWCONF-REGPS
                       MOVE OPERADOR TO CWCONF-NOME
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-EQUAL TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF   FS-CWCONF = "9D"
                            CALL "CWCONF" USING "ISAM"
                       END-IF
               END-PERFORM
               MOVE SPOOL-OPTIONS TO CWCONF-SPOOL-OPTIONS
               SET CWSQLC-REWRITE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           CLOSE REPKEY DELETE FILE REPKEY
           CALL "CWMSGW" USING "250270" ESPACOS
           PERFORM cwimpr
           GOBACK.

       cwimpr.

         If   SPOOL-NUMERO numeric
         and  SPOOL-NUMERO > 0
              CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                             CARACTER-BUFFER3
                                             ATTRIBUTE-BUFFER3
                                             STRING-LENGTH
         end-if.

       fim-cwimpr. exit.

       110-EXIBE.

           MOVE PONTEIRO TO C
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 11
                   MOVE C         TO REPKEY-CHAVE
                   IF   REPKEY-CHAVE > SIZE-REPKEY
                        MOVE "23" TO FS-CWDIRS
                   ELSE
                        READ REPKEY
                        MOVE REPKEY-CW       TO CWDIRS-CW
                                                CW (Y)
                        MOVE REPKEY-NUMERO   TO CWDIRS-NUMERO
==>                                             NUMERO (Y)
                        MOVE REPKEY-EXT      TO EXT (Y)
                        MOVE CW     (Y)  TO CWDIRS-CW
                        MOVE NUMERO (Y)  TO CWDIRS-NUMERO
                        MOVE "."         TO CWDIRS-PONTO
                        MOVE EXT    (Y)  TO CWDIRS-EXT
                        READ CWDIRS IGNORE LOCK
                   END-IF
                   IF   FS-CWDIRS < "10"
                   AND (CWDIRS-NUMERO NOT = 0)
                   AND (CWDIRS-NUMERO NUMERIC)
                   AND (CWDIRS-COMANDO NOT = "Erased")
                   AND (REPKEY-COMANDO NOT = "Erased")
                        MOVE REPKEY-COMANDO  TO COMANDO  (Y)
                        IF  (CWDIRS-IMPRESSO NOT = "*")
                        AND (CWDIRS-IMPRESSO NOT = "+")
                             MOVE SPACE TO CWDIRS-IMPRESSO
                        END-IF
                        MOVE CWDIRS-IMPRESSO TO IMPRESSO (Y)
                        MOVE CWDIRS-CODIGO   TO CWDIRSS  (Y)
                        MOVE CWDIRS-TITULO   TO TITULO-T   (Y)
                        MOVE CWDIRS-WIDTH    TO TIPO-LEN (Y)
                        IF ((CWDIRS-PROGRAMA NOT = LOW-VALUES)
                        AND (CWDIRS-PROGRAMA NOT = SPACES))
                             MOVE 'c'   TO TIPO-FLAG (Y)
                        ELSE
                             MOVE SPACE TO TIPO-FLAG (Y)
                        END-IF
                        MOVE CWDIRS-FOLHAS     TO PAGINA  (Y)
                        MOVE CWDIRS-DATA       TO CWTIME-DATE
                        SET  CWTIME-REVERSED   TO TRUE
                        SET  CWTIME-REVERSE    TO TRUE
                        CALL "CWTIME" USING PARAMETROS-CWTIME
                        MOVE CWTIME-DATE-FINAL TO DATAS   (Y)
                        MOVE CWDIRS-HORA       TO HH      (Y)
                        MOVE ":"               TO PT      (Y)
                        MOVE CWDIRS-MINUTO     TO MM      (Y)
                        MOVE CWDIRS-USUARIO    TO USUARIO      (Y)
                        MOVE CWDIRS-NOTA       TO NOTA         (Y)
                        MOVE CWDIRS-NUMERO     TO NUMERO       (Y)
                        MOVE CWDIRS-EXT        TO EXT          (Y)
                        MOVE REPKEY-CHAVE      TO CHAVE-REPKEY (Y)
                        MOVE CWDIRS-CW         TO CW           (Y)
                   ELSE
                        INITIALIZE RELATORIO (Y)
                        MOVE SPACES TO EMISSAO (Y)
                        IF   FS-CWDIRS = "9D"
                             MOVE "Em uso" to CWDIRSS (Y)
                        END-IF
                   END-IF
                   ADD 1 TO C
           END-PERFORM

           PERFORM VARYING T FROM 1 BY 1 UNTIL T > 11
                   COMPUTE ROW-NUMBER = T + 8
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                      RELATORIO (T)
                                                      ATT-T2
                                                      X"004E"
           END-PERFORM

           IF   PRONTO = 99
                PERFORM VARYING PRONTO FROM 11 BY -1
                         UNTIL NUMERO (PRONTO) NOT = SPACES
                            OR PRONTO = 1
                        CONTINUE
                END-PERFORM
           ELSE
                PERFORM VARYING PRONTO FROM PRONTO BY -1
                         UNTIL NUMERO (PRONTO) NOT = SPACES
                            OR PRONTO = 1
                        CONTINUE
                END-PERFORM
                PERFORM VARYING PRONTO FROM PRONTO BY 1
                         UNTIL NUMERO (PRONTO) NOT = SPACES
                            OR PRONTO = 11
                        CONTINUE
                END-PERFORM.

       110-99-FIM. EXIT.

       112-ORDEM.

           MOVE 09             TO CWBOXS-LINE
           MOVE 23             TO CWBOXS-COLUMN
           MOVE COLUNA-SORT    TO CWBOXS-OPTION
           MOVE "Ordenar_por:" TO CWBOXS-TITLE
           MOVE SPACES         TO CWBOXS-ITENS
           MOVE "C¢digo"       TO CWBOXS-TEXT (1) CWBOXS-CHAR (1)
           MOVE "Data/hora"    TO CWBOXS-TEXT (2) CWBOXS-CHAR (2)
           MOVE "P ginas"      TO CWBOXS-TEXT (3) CWBOXS-CHAR (3)
           MOVE "Nota"         TO CWBOXS-TEXT (4) CWBOXS-CHAR (4)
           MOVE "Usu rio"      TO CWBOXS-TEXT (5) CWBOXS-CHAR (5)
           CALL "CWBOXS"    USING PARAMETROS-CWBOXS
           IF   CWBOXS-OPTION  = 0
                GO TO 112-99-FIM
           END-IF
           MOVE CWBOXS-OPTION  TO COLUNA-SORT
           MOVE OPCAO-SORT     TO CWBOXS-OPTION
           MOVE SPACES         TO CWBOXS-TITLE
           MOVE "Ordena‡Æo:_"  TO CWBOXS-TITLE
           MOVE CWBOXS-TEXT (COLUNA-SORT) TO CWBOXS-TITLE (12:)
           MOVE SPACES         TO CWBOXS-ITENS
           MOVE "Ascendente"   TO CWBOXS-TEXT (1) CWBOXS-CHAR (1)
           MOVE "Descendente"  TO CWBOXS-TEXT (2) CWBOXS-CHAR (2)
           CALL "CWBOXS"    USING PARAMETROS-CWBOXS
           IF   CWBOXS-OPTION  = 0
                GO TO 112-99-FIM
           END-IF
           MOVE CWBOXS-OPTION  TO OPCAO-SORT
           PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
           CALL "CWMSGW" USING "080377" TOPO
           MOVE    1           TO PONTEIRO
           PERFORM 110-EXIBE THRU 110-99-FIM.

       112-99-FIM. EXIT.

       113-IMPORTA.

           CALL "CWMSGW" USING "230270" ESPACOS
           MOVE 110 TO CWPATH-COLOR-FRAME
                       CWPATH-COLOR-BORDER
           MOVE 112 TO CWPATH-COLOR-BARR-MENU
           SET CWPATH-WITH-DIR        TO TRUE
           SET CWPATH-WITH-DRIVES     TO TRUE
           SET CWPATH-WITHOUT-NEWDIR  TO TRUE
           SET CWPATH-WITHOUT-NEWFILE TO TRUE
           MOVE "_Importar_de:"     TO CWPATH-TITLE
           MOVE SPACES              TO CWPATH-DEFAULT
                                       CWPATH-PATH
           IF   PATH-SPOOL (2: 1) = ":"
                STRING PATH-SPOOL DELIMITED BY SPACE
                       "\*.cw6"   DELIMITED BY SIZE
                       INTO CWPATH-PATH
           ELSE
                IF   CWUNIX-OFF
                     CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                 RETURN-STATUS
                END-IF
                CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
                IF   CWUNIX-OFF
                     STRING OLD-DRIVE      DELIMITED BY SPACE
                            ":\"           DELIMITED BY SPACE
                            OLD-DIRECTORY  DELIMITED BY SPACE
                            "\"            DELIMITED BY SIZE
                            PATH-SPOOL     DELIMITED BY SPACE
                            "\*.cw6"       DELIMITED BY SIZE
                     INTO CWPATH-PATH
                ELSE
                     STRING OLD-DIRECTORY  DELIMITED BY SPACE
                            "/"            DELIMITED BY SIZE
                            PATH-SPOOL     DELIMITED BY SPACE
                            "/*.cw6"       DELIMITED BY SIZE
                     INTO CWPATH-PATH
                END-IF
           END-IF
           CALL "CWPATH" USING PARAMETROS-CWPATH
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230370" MODO
           ELSE
                CALL "CWMSGW" USING "250370" MODO
           END-IF
           IF   CWPATH-FILE NOT = SPACES
                MOVE CWPATH-FILE TO LB-TEXTO
                OPEN INPUT TEXTO
                IF   FS-TEXTO = "30" OR "35"
                     MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                     STRING "Arquivo: " DELIMITED BY SIZE
                              LB-TEXTO  DELIMITED BY SPACE
                        " nÆo existe !" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     GO TO 113-99-FIM
                ELSE
                     CALL X"91" USING X91-SRCOUT-RESULT
                                      X91-SRCOUT-FUNCTION
                                      TEXTO
                     PERFORM 252-ABRE-PERC    THRU 252-99-FIM
                     IF   LINE-RODAPE = 23
                          CALL "CWMSGW" USING "230370" CANCELAR
                     ELSE
                          CALL "CWMSGW" USING "250370" CANCELAR
                     END-IF
                     MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
                     MOVE 1           TO CWMOUS-MODE
                     MOVE ALL X'01'   TO CWMOUS-LINE (23) (03: 14)
                     PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                             READ TEXTO IGNORE LOCK
                             NOT AT END
                                 PERFORM 114-GRAVA-SPOOL
                                    THRU 114-99-FIM
                             END-READ
                             IF CWDIRS-EXT = 'spl'
                                EXIT PERFORM
                             END-IF
                     END-PERFORM
                     CLOSE TEXTO
                     IF CWDIRS-EXT = 'spl'
                        move high-values to texto-handle
                        call "CBL_OPEN_FILE" using LB-TEXTO
                                                   X"01" X"03" X"00"
                                                   texto-handle
                        MOVE 0 TO texto-offset
                        PERFORM TEST AFTER UNTIL TEXTO-REG(1:1) = X'0A'
                                  OR RETURN-CODE NOT = 0
                           call "CBL_READ_FILE" using texto-handle
                                                texto-offset
                                                texto-count
                                                X'00'
                                                TEXTO-REG(1:1)
                           ADD 1 TO texto-offset
                        END-PERFORM
                        PERFORM TEST AFTER UNTIL RETURN-CODE NOT = 0
                                call "CBL_READ_FILE" using texto-handle
                                                     texto-offset
                                                     byte-count
                                                     X'00'
                                              TEXTO-REG(1:byte-count)
                                ADD byte-count TO texto-offset
                                IF RETURN-CODE = 0
                                   PERFORM 114-GRAVA-SPOOL
                                      THRU 114-99-FIM
                                END-IF
                        END-PERFORM
                        call "CBL_CLOSE_FILE" using texto-handle
                     END-IF
                     PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                     MOVE 0 TO CWMOUS-MODE
                     IF   RESPOSTA = "S" OR "s"
                          CALL "CBL_DELETE_FILE" USING LB-PRNTER
                          IF RETURN-CODE = 0
                             DELETE CWDIRS RECORD
                          END-IF
                     END-IF
                     UNLOCK CWDIRS
                     SET CWBOXW-CLOSE TO TRUE
                     CALL "CWBOXW" USING PARAMETROS-CWBOXW
                END-IF
                PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
                CALL "CWMSGW" USING "080377" TOPO
                MOVE    1           TO PONTEIRO
                PERFORM 110-EXIBE THRU 110-99-FIM
           END-IF.

       113-99-FIM. EXIT.

       114-GRAVA-SPOOL.

           IF  (TEXTO-REG (1: 2) = "CW" OR TEXTO-REG (1: 1) = "R")
           AND  TEXTO-REG (9: 4) = ".SPL" OR ".spl"
                PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                PERFORM TEST AFTER UNTIL FS-CWDIRS < "10"
                        MOVE "C0999999"     TO CWDIRS-SPOOL
                        START CWDIRS KEY NOT GREATER CWDIRS-SPOOL
                        READ CWDIRS PREVIOUS RECORD IGNORE LOCK
                        MOVE "."               TO CWDIRS-PONTO
                        MOVE TEXTO-REG (10: 3) TO CWDIRS-EXT
                        ADD  1 TO CWDIRS-NUMERO
                        IF   CWDIRS-NUMERO = 0
                             MOVE 1 TO CWDIRS-NUMERO
                        END-IF
                        MOVE TEXTO-REG (13: ) TO CWDIRS-REG (13: )
                        MOVE "Importando..."  TO CWDIRS-USUARIO
                        MOVE ZERO             TO RK-PRNTER
                                                 CWDIRS-FOLHAS
                                                 CWDIRS-LINHAS
                        IF CWDIRS-WIDTH NOT NUMERIC
                        OR CWDIRS-WIDTH = 0
                           EVALUATE CWDIRS-TIPO
                               WHEN '1'
                                    MOVE 132 TO CWDIRS-WIDTH
                               WHEN '2'
                                    MOVE  80 TO CWDIRS-WIDTH
                               WHEN '2'
                                    MOVE 220 TO CWDIRS-WIDTH
                               WHEN OTHER
                                    MOVE CWDIRS-TIPO TO RESULTADO(1:1)
                                    MOVE RESULTADO   TO CWDIRS-WIDTH
                                    MOVE 0           TO RESULTADO
                           END-EVALUATE
                        END-IF
                        MOVE CWDIRS-WIDTH TO WIDTH
                        WRITE CWDIRS-REG
                END-PERFORM
                READ CWDIRS WITH LOCK
                MOVE TEXTO-REG (13: ) TO CWDIRS-REG (13: )
                MOVE "Importado"      TO CWDIRS-USUARIO
                MOVE SPACES           TO CWDIRS-COMANDO
                                         CWDIRS-IMPRESSO
                PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
                MOVE CWDIRS-LINHAS    TO CWPERC-EMPTY
                move 0 to file-offset
                call "CBL_CREATE_FILE" using LB-PRNTER
                                           X"02" X"00" X"00"
                                           file-handle
                compute byte-count = width + 5
                MOVE CWDIRS-DATA       TO CWTIME-DATE
                SET  CWTIME-REVERSED   TO TRUE
                SET  CWTIME-REVERSE    TO TRUE
                CALL "CWTIME" USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL TO OBS-DATA
                CALL "CWMSGW" USING "101215" "Importando de:"
                CALL "CWMSGW" USING "111250" LB-TEXTO
                CALL "CBL_SET_CSR_POS" USING X"0B0A"
                CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR
                MOVE SPACES TO CWPERC-MSG
                STRING CWDIRS-CODIGO DELIMITED BY SPACE
                       " "           DELIMITED BY SIZE
                       OBS-DATA      DELIMITED BY SIZE
                       " "           DELIMITED BY SIZE
                       CWDIRS-HORA   DELIMITED BY SIZE
                       ":"           DELIMITED BY SIZE
                       CWDIRS-MINUTO DELIMITED BY SIZE
                                INTO CWPERC-MSG
           ELSE
                ADD 1 TO RK-PRNTER
                MOVE TEXTO-REG TO PRNTER-REG
                IF CWDIRS-EXT NOT = 'spl'
                   MOVE X'0D0A'   TO PRNTER-REG (byte-count - 1: 2)
                END-IF
                call "CBL_WRITE_FILE" using file-handle
                                 file-offset
                                 byte-count
                                 X"00"
                                 PRNTER-REG
                add byte-count   to file-offset
                MOVE RK-PRNTER   TO CWPERC-FULL
                CALL "CWPERC" USING PARAMETROS-CWPERC
                CALL "CWMOUS" USING PARAMETROS-CWMOUS
                IF   CWMOUS-KEY = 1
                     MOVE 27 TO TECLA
                ELSE
                     CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                END-IF
                IF   KEY-STATUS = 1
                OR   CWMOUS-KEY = 1
                     IF   CWMOUS-KEY NOT = 1
                          MOVE 23         TO CURPOS-LIN
                          MOVE 03         TO CURPOS-COL
                          CALL "CWKBDC" USING CURPOS CHAR TECLA
                     END-IF
                     IF   TECLA = 27
                          MOVE 0            TO TECLA
                          MOVE SPACES       TO CWSEND-SCREENS RESPOSTA
                          MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                          MOVE "~Continuar" TO CWSEND-SCREEN (2)
                          MOVE "Cancelar importa‡Æo ?" TO CWSEND-MSG
                          CALL "CWSEND" USING PARAMETROS-CWSEND
                          IF   CWSEND-OPTION = 1
                               MOVE "S" TO RESPOSTA
                          END-IF
                     END-IF
                END-IF
                IF   CWPERC-FULL = CWPERC-EMPTY
                     REWRITE CWDIRS-REG
                     UNLOCK CWDIRS
                ELSE
                     IF   RESPOSTA = "S" OR "s"
                          MOVE "10" TO FS-TEXTO
                     END-IF
                END-IF
           END-IF.

       114-99-FIM. EXIT.

       115-PREPARA-LISTA.

           IF AUTOV = 1
              MOVE 2 TO OPCAO-SORT
              MOVE 6 TO COLUNA-SORT
           END-IF
           INSPECT TOPO CONVERTING SETAS-SORT TO SPACE

           IF   OPCAO-SORT = 1
                MOVE LOW-VALUES  TO CWDIRS-REG
           ELSE
                MOVE HIGH-VALUES TO CWDIRS-REG
           END-IF

           PERFORM 116-RESTART-CWDIRS THRU 116-99-FIM

           IF   FS-CWDIRS NOT = "00"
                CALL "CWISAM" USING ER-CWDIRS
                GOBACK
           END-IF

           CLOSE REPKEY DELETE FILE REPKEY
           OPEN I-O REPKEY
           MOVE 0 TO REPKEY-CHAVE
           IF   FS-REPKEY > "09"
                CALL "CWISAM" USING ER-REPKEY
                GOBACK
           END-IF

           PERFORM UNTIL FS-CWDIRS > "09"
                   PERFORM 117-LER-CWDIRS THRU 117-99-FIM
                   IF   FS-CWDIRS < "10"
                   AND  CWDIRS-NUMERO NOT = 0
                        IF   FS-CWDIRS NOT = "9D"
                             MOVE CWDIRS-SPOOL TO LB-PRNTER
                             PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
                             SET SPOOL-TEST TO TRUE
                             PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
                             SET SPOOL-OK TO TRUE
                             IF FS-PRNTER < "10"
                             OR FS-PRNTER = "9A"
                                IF CWDIRS-COMANDO NOT = "Erased"
                                   ADD  1             TO REPKEY-CHAVE
                                   MOVE CWDIRS-CW     TO REPKEY-CW
                                   MOVE CWDIRS-NUMERO TO REPKEY-NUMERO
                                   MOVE CWDIRS-EXT    TO REPKEY-EXT
                                   MOVE SPACES        TO REPKEY-COMANDO
                                   WRITE REPKEY-REG
                                   READ CWDIRS
                                   IF   FS-CWDIRS < "10"
                                   AND (CWDIRS-NOTA = "Gerando..."
                                   OR CWDIRS-USUARIO = "Importando...")
                                        MOVE "Terminado anormal"
                                          TO CWDIRS-NOTA
                                        PERFORM 118-CONTA-FOLHAS
                                           THRU 118-99-FIM
                                        REWRITE CWDIRS-REG
                                   END-IF
                                   PERFORM 116-RESTART-CWDIRS
                                      THRU 116-99-FIM
                                   MOVE 0 TO CWDIRS-NUMERO
                                   PERFORM 117-LER-CWDIRS
                                      THRU 117-99-FIM
                                     UNTIL(CWDIRS-NUMERO = REPKEY-NUMERO
                                       AND CWDIRS-CW     = REPKEY-CW)
                                        OR FS-CWDIRS > "09"
                                   IF (CWSPOOLHANDLER NOT = SPACES)
                                   AND(LB-TEXTO NOT = SPACES)
                                   AND FS-CWDIRS < "10"
                                       INSPECT CWDIRS-PROGRAMA
                                         CONVERTING LOW-VALUES TO SPACE
                                       PERFORM 150-GRAVA-XML
                                          THRU 150-99-FIM
                                   END-IF
                                END-IF
                             ELSE
                                IF FS-PRNTER = "30" OR "35"
                                   DELETE CWDIRS RECORD
                                END-IF
                             END-IF
                        END-IF
                        UNLOCK CWDIRS
                   END-IF
                   IF   FS-CWDIRS = "9D"
                        MOVE "00" TO FS-CWDIRS
                   END-IF
           END-PERFORM
           MOVE REPKEY-CHAVE TO SIZE-REPKEY.

       115-99-FIM. EXIT.

       116-RESTART-CWDIRS.

           IF   OPCAO-SORT = 1
                EVALUATE COLUNA-SORT
                    WHEN 1     START CWDIRS KEY NOT < CWDIRS-CODIGO
                               MOVE SETA-DOWN TO TOPO (15: 1)
                    WHEN 3     START CWDIRS KEY NOT < CWDIRS-FOLHAS
                               MOVE SETA-DOWN TO TOPO (46: 1)
                    WHEN 4     START CWDIRS KEY NOT < CWDIRS-NOTA
                               MOVE SETA-DOWN TO TOPO (51: 1)
                    WHEN 5     START CWDIRS KEY NOT < CWDIRS-USUARIO
                               MOVE SETA-DOWN TO TOPO (74: 1)
                    WHEN OTHER START CWDIRS KEY NOT < CWDIRS-EMISSAO
                               MOVE SETA-DOWN TO TOPO (29: 1)
                END-EVALUATE
           ELSE
                EVALUATE COLUNA-SORT
                    WHEN 1     START CWDIRS KEY NOT > CWDIRS-CODIGO
                               MOVE SETA-UP   TO TOPO (15: 1)
                    WHEN 3     START CWDIRS KEY NOT > CWDIRS-FOLHAS
                               MOVE SETA-UP   TO TOPO (46: 1)
                    WHEN 4     START CWDIRS KEY NOT > CWDIRS-NOTA
                               MOVE SETA-UP   TO TOPO (51: 1)
                    WHEN 5     START CWDIRS KEY NOT > CWDIRS-USUARIO
                               MOVE SETA-UP   TO TOPO (74: 1)
                    WHEN OTHER START CWDIRS KEY NOT > CWDIRS-EMISSAO
                               MOVE SETA-UP   TO TOPO (29: 1)
                END-EVALUATE
           END-IF.

       116-99-FIM. EXIT.

       117-LER-CWDIRS.

           IF   OPCAO-SORT = 1
                READ CWDIRS NEXT RECORD IGNORE LOCK
           ELSE
                READ CWDIRS PREVIOUS RECORD IGNORE LOCK
           END-IF

           IF  FS-CWDIRS < "10"
               IF  SPOOL-NUMERO NUMERIC
               AND SPOOL-NUMERO > 0
               AND (LK-SPOOL NOT = CWDIRS-SPOOL)
                   GO TO 117-LER-CWDIRS
               END-IF
               IF (CWDIRS-CW NOT = 'CW')
               AND CWDIRS-CW (2:1) > NIVEL
                   GO TO 117-LER-CWDIRS
               END-IF
               IF SPOOLUSER = 'ON'
               AND(CWDIRS-CW NOT = 'CW')
               AND CWDIRS-CW (2:1) = NIVEL
               AND (CWDIRS-USUARIO NOT = USERID)
                   GO TO 117-LER-CWDIRS
               END-IF
           END-IF.

       117-99-FIM. EXIT.

       118-CONTA-FOLHAS.

           CALL "CWMSGW" USING "230326" "Verificando fim anormal..."
           PERFORM VARYING RK-PRNTER FROM 1 BY 1
                           UNTIL FS-PRNTER NOT = "00"
                   PERFORM 335-READ-PRNTE THRU 335-99-FIM
                   CALL 'CWATCH'
                   IF   FS-PRNTER < "10"
                        IF   PRINTS-WS (1: 1) = X"0C"
                        OR   PRINTS-WS (2: 1) = X"0C"
                        OR   PRINTS-WS (3: 1) = X"0C"
                             ADD 1 TO CWDIRS-FOLHAS
                             IF   CWDIRS-FOLHAS = 1
                             AND  RK-PRNTER > 1
                                  MOVE 2 TO CWDIRS-FOLHAS
                             END-IF
                        END-IF
                        MOVE RK-PRNTER TO CWDIRS-LINHAS
                   END-IF
                   ADD 1 TO RK-PRNTER
           END-PERFORM
           CALL "CWMSGW" USING "230326" ESPACOS.

       118-99-FIM. EXIT.

       120-EXEC.

           CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR
           MOVE 0 TO REORDENAR EXPORTOU
           CALL "CBL_READ_SCR_CHATTRS" USING X"0000"
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH
           CALL "CWMSGW" USING "250179" ESPACOS
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230370" ESPACOS
           ELSE
                CALL "CWMSGW" USING "250370" ESPACOS
           END-IF
           IF   FULL-SCREEN = 1
                MOVE 25 TO LINE-RODAPE
           ELSE
                MOVE 23 TO LINE-RODAPE
           END-IF
           MOVE PONTEIRO TO SALVA-PONTEIRO
           MOVE 1        TO VEZ
           MOVE 1        TO VEZ-NOTA
           PERFORM VARYING P FROM 1 BY 1 UNTIL P GREATER SIZE-REPKEY
                   MOVE P          TO REPKEY-CHAVE
                   IF   REPKEY-CHAVE > SIZE-REPKEY
                        MOVE "23" TO FS-CWDIRS
                   ELSE
                        READ REPKEY
                        MOVE REPKEY-CW     TO CWDIRS-CW
                        MOVE REPKEY-NUMERO TO CWDIRS-NUMERO
                        MOVE REPKEY-EXT    TO CWDIRS-EXT
                        READ CWDIRS IGNORE LOCK
                   END-IF
                   IF   FS-CWDIRS < "10"
                   AND (REPKEY-COMANDO NOT = SPACES)
                   AND (REPKEY-COMANDO NOT = "*")
                   AND (REPKEY-COMANDO NOT = "Erased")
                   AND (CWDIRS-NUMERO  NOT = 0)
                        IF   REPKEY-COMANDO NOT = "="
                             MOVE REPKEY-COMANDO TO COMANDO-T
                        END-IF
                        EVALUATE TRUE
                        WHEN VER
                        AND (FS-CWDIRS NOT = "9D")
                             MOVE    1               TO COLUNA
                             PERFORM 200-VISUALIZA THRU 200-99-FIM
                             IF  CWPURGEVIEW = 'ON'
                             OR (CWPURGEPRINT = 'ON' AND IMPRIMIU)
                                 PERFORM 250-REMOVER   THRU 250-99-FIM
                             ELSE
                                 MOVE "*"            TO REPKEY-COMANDO
                                 REWRITE                REPKEY-REG
                             END-IF
                        WHEN REMOVER
                             PERFORM 250-REMOVER   THRU 250-99-FIM
                        WHEN EXPORTA
                             MOVE    1               TO EXPORTANDO
                             PERFORM 251-EXPORTA   THRU 251-99-FIM
                             MOVE    0               TO EXPORTANDO
                             MOVE "*"                TO REPKEY-COMANDO
                             MOVE "N"                TO RESPOSTA
                             REWRITE                    REPKEY-REG
                        WHEN NOTAS
                             PERFORM 255-NOTAS     THRU 255-99-FIM
                        WHEN REMOVEALL
                             MOVE SPACES     TO CWSEND-SCREENS RESPOSTA
                             MOVE "  ~Sim__" TO CWSEND-SCREEN (1)
                             MOVE "  ~NÆo__" TO CWSEND-SCREEN (2)
                             MOVE "Remover TODOS ?" TO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             MOVE 0 TO REPKEY-CHAVE
                             IF   CWSEND-OPTION = 1
                                  PERFORM UNTIL FS-REPKEY > "09"
                                  ADD 1 TO REPKEY-CHAVE
                                  READ REPKEY
                                  IF   FS-REPKEY < "10"
                                       MOVE REPKEY-CW
                                         TO CWDIRS-CW
                                       MOVE REPKEY-NUMERO
                                         TO CWDIRS-NUMERO
                                       MOVE REPKEY-EXT
                                         TO CWDIRS-EXT
                                       MOVE "DELETE"
                                         TO REPKEY-COMANDO
                                       PERFORM TEST AFTER
                                         UNTIL FS-REPKEY NOT = "9D"
                                               READ CWDIRS
                                               IF FS-CWDIRS < "10"
                                                  PERFORM 250-REMOVER
                                                  THRU    250-99-FIM
                                               END-IF
                                      END-PERFORM
                                      MOVE "00" TO FS-REPKEY
                                  END-IF
                                  END-PERFORM
                             END-IF
                        WHEN COMANDO-T NOT = "="
                             MOVE    1                 TO I
                                                          PONTEIRO
                             MOVE    CWDIRS-LINHAS     TO C
                             MOVE    CWDIRS-SPOOL      TO LB-PRNTER
                             MOVE    1                 TO COLUNA
                             PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
                             PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
                             IF   FS-PRNTER > "09"
                                  CALL "CWISAM" USING ER-PRNTER
                             END-IF
                             PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                             SET NAO-IMPRIMIU TO TRUE
                             MOVE 9999        TO P-F
                             MOVE "*"         TO CWDIRS-IMPRESSO
                             MOVE 23          TO LINE-LISTAR
                             PERFORM 260-LISTAR THRU 260-99-FIM
                             IF   IMPRIMIU
                                  REWRITE CWDIRS-REG
                             END-IF
                             IF   IMPRIMIU
                             AND (PRINTDEL OR CWPURGEPRINT = 'ON')
                                  PERFORM 250-REMOVER THRU 250-99-FIM
                             ELSE
                                  MOVE   "*" TO REPKEY-COMANDO
                                  REWRITE       REPKEY-REG
                             END-IF
                        END-EVALUATE
                   END-IF
                   UNLOCK CWDIRS
           END-PERFORM.

           MOVE 23             TO LINE-RODAPE
           MOVE SPACES         TO TEXTO-TELA
           MOVE SALVA-PONTEIRO TO PONTEIRO
           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH

           IF   REORDENAR = 1
                PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
           END-IF
           PERFORM 290-EXIBE-TEXTO THRU 290-99-FIM
           CALL "CWMSGW" USING "080377" TOPO
           CALL "CWMSGW" USING "230370" MODO
           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1801"
                                             X0E
                                             X"004E"
           PERFORM 110-EXIBE THRU 110-99-FIM.

       120-99-FIM. EXIT.

       125-PR.

           MOVE    1                 TO I
                                        PONTEIRO
           MOVE    CWDIRS-LINHAS     TO C
           MOVE    CWDIRS-SPOOL      TO LB-PRNTER
           MOVE    1                 TO COLUNA
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
           PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
           IF   FS-PRNTER > "09"
                GO TO 125-99-FIM
           END-IF
           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
           SET NAO-IMPRIMIU TO TRUE
           MOVE 9999        TO P-F
           MOVE "*"         TO CWDIRS-IMPRESSO
           PERFORM 260-LISTAR THRU 260-99-FIM
           IF   IMPRIMIU
                REWRITE CWDIRS-REG
           END-IF
           MOVE   "*" TO REPKEY-COMANDO
           REWRITE       REPKEY-REG
           IF   IMPRIMIU
           AND (PRINTDEL OR CWPURGEPRINT = 'ON')
                PERFORM 250-REMOVER THRU 250-99-FIM
           END-IF.

       125-99-FIM. EXIT.

       150-GRAVA-XML.

           MOVE SPACES            TO XML-REG
           STRING PATH-SPOOL DELIMITED BY SPACE
                   BARRA-DIR DELIMITED BY SIZE
                CWDIRS-SPOOL DELIMITED BY SPACE
                                INTO Arquivo
      *    IF   NOT CWUNIX-ON
      *         INSPECT Arquivo converting MAIUSCULAS TO minusculas
      *    END-IF
           MOVE CWDIRS-CODIGO     TO Codigo
           MOVE CWDIRS-PROGRAMA   TO Controle
           MOVE CWDIRS-FOLHAS     TO Paginas
           MOVE CWDIRS-LINHAS     TO Linhas
           MOVE CWDIRS-DATA       TO CWTIME-DATE
           SET  CWTIME-REVERSED   TO TRUE
           SET  CWTIME-REVERSE    TO TRUE
           CALL "CWTIME" USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO DATAS   (1)
           MOVE DATAS(1) (1:)     TO Geracao
           STRING CWDIRS-HORA ':' CWDIRS-MINUTO DELIMITED BY SIZE
                                INTO Hora
           MOVE CWDIRS-USUARIO    TO Proprietario
           If CWDIRS-NOTA NOT = CWDIRS-TITULO (1: 20)
              MOVE CWDIRS-NOTA       TO Observacao
           END-IF
           MOVE CWDIRS-TITULO     TO Titulo
           MOVE CWDIRS-WIDTH      TO Largura
           EXEC COBOLware Save File LB-TEXTO
                          Codigo
                          Paginas
                          Linhas
                          Largura
                          Geracao
                          Hora
                          Proprietario
                          Titulo
                          Arquivo
                          Observacao
                          Controle
                          DELIMITER DELIM
           END-EXEC.

       150-99-FIM. EXIT.

       200-VISUALIZA.

           IF   SPOOLVIEW NOT = SPACES
                MOVE SPACE     TO LB-PRINTS
                MOVE 3         TO VEZ
                MOVE 255       TO PRINTER-NO
                IF DOLAR-VIEW = 0
                   STRING "$" SPOOLVIEW (1: SPOOLVIEW-LEN) " $"
                          DELIMITED BY SIZE
                          INTO LB-PRINTS
                ELSE
                   STRING "$" SPOOLVIEW (1: SPOOLVIEW-LEN)
                          DELIMITED BY SIZE
                          INTO LB-PRINTS
                END-IF
                MOVE LB-PRINTS      TO WINPRINT SPOOL-DEV
                MOVE LB-PRINTS (2:) TO SPOOL-WORK
                PERFORM 125-PR THRU 125-99-FIM
                GO TO 200-99-FIM
           END-IF

           MOVE CWDIRS-SPOOL  TO LB-PRNTER
           MOVE 1             TO I PONTEIRO
           MOVE CWDIRS-LINHAS TO C
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
           PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM

           IF   FS-PRNTER > "09"
                GO TO 200-99-FIM
           END-IF

           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230270" SETAS
           ELSE
                CALL "CWMSGW" USING "250270" SETAS
           END-IF
           PERFORM 210-EXIBE THRU 210-99-FIM

           PERFORM UNTIL EDIT-ESC
                      OR EDIT-ALT-F4
=>                 MOVE 0 TO TECLA
                   PERFORM TEST AFTER UNTIL NOT CMD-MODOS
                   MOVE  LOW-VALUES  TO PARAMETROS-CWMOUS
                   MOVE  SETAS-MOUSE TO CWMOUS-LINE (LINE-RODAPE)
                                        (2: LENGTH OF SETAS-MOUSE)
                   MOVE LINE-RODAPE  TO CURPOS-LIN
                   MOVE 03           TO CURPOS-COL
                   CALL "CWMOUS"  USING PARAMETROS-CWMOUS CURPOS
                   MOVE SPACE        TO CMD
                   IF   CWMOUS-KEY = 255
                        IF   CWMOUS-TIMEOUT-ON
                             SET EDIT-ESC TO TRUE
                             MOVE 0 TO KEY-STATUS
                        ELSE
                             CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                        END-IF
                        IF   KEY-STATUS = 1
                        OR   CWUNIX-ON
                             CALL "CWKBDC" USING CURPOS CHAR TECLA
                             IF   EDIT-ESC
                                  EXIT PERFORM
                             ELSE
                                  IF   CHAR NOT = X"00"
                                       MOVE CHAR TO CMD
                                  END-IF
                             END-IF
                        END-IF
                   ELSE
                        IF   CWMOUS-KEY > 100
                             COMPUTE TECLA = CWMOUS-KEY - 100
                        ELSE
                             MOVE CWMOUS-KEY TO CHAR-X
                             MOVE CHAR       TO CMD
                        END-IF
                        IF   EDIT-ESC
                             EXIT PERFORM
                        END-IF
                   END-IF
                   IF   (CMD = "F" OR "f") AND FULL-SCREEN = 0
                        MOVE 1  TO FULL-SCREEN
                        CALL "CBL_WRITE_SCR_N_ATTR" USING X"1800"
                                                          X"6E"
                                                          X"0050"
                   ELSE
                        IF   (CMD = "F" OR "f") AND FULL-SCREEN = 1
                             MOVE 0  TO FULL-SCREEN
                             CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                                        CARACTER-BUFFER
                                                       ATTRIBUTE-BUFFER
                                                          STRING-LENGTH
                        END-IF
                   END-IF
                   IF   (CMD = "Z" OR "z") AND ZEBRADO = 0
                        MOVE 1 TO ZEBRADO
                   ELSE
                        IF   (CMD = "Z" OR "z") AND ZEBRADO = 1
                             MOVE 0 TO ZEBRADO
                        END-IF
                   END-IF
                   IF   FULL-SCREEN = 1
                        MOVE 25 TO LINE-RODAPE
                        MOVE 24 TO FULL-LIMITE
                        MOVE 80 TO STRING-LENGTH2
      *                 compute limite-t = limite - 3
                        compute limite-t = limite - 2
                        if limite-t = 0
                           move 1 to limite-t
                        end-if
                   ELSE
                        MOVE 23 TO LINE-RODAPE
                        MOVE 15 TO FULL-LIMITE
                        MOVE 78 TO STRING-LENGTH2
                        move limite to limite-t
                   END-IF
                   IF   CMD-MODOS
                        PERFORM 210-EXIBE THRU 210-99-FIM
                        IF   LINE-RODAPE = 23
                             CALL "CWMSGW" USING "230270" SETAS
                        ELSE
                             CALL "CWMSGW" USING "250270" SETAS
                        END-IF
                   END-IF
                   END-PERFORM
                   EVALUATE TRUE
                   WHEN CMD = "L" OR "l"
                        MOVE    PONTEIRO         TO SALVA-PONTEIRO-2
                        PERFORM 220-IMPRIMIR   THRU 220-99-FIM
                        MOVE    1                TO I
                        MOVE    CWDIRS-LINHAS    TO C
                        MOVE    SALVA-PONTEIRO-2 TO PONTEIRO
                        IF   LINE-RODAPE = 23
                             CALL "CWMSGW" USING "230270" SETAS
                        ELSE
                             CALL "CWMSGW" USING "250270" SETAS
                        END-IF
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN CMD = "P" OR "p" OR "/"
                        PERFORM 230-LOCALIZA THRU 230-99-FIM
                        IF   LINE-RODAPE = 23
                             CALL "CWMSGW" USING "230270" SETAS
                        ELSE
                             CALL "CWMSGW" USING "250270" SETAS
                        END-IF
                   WHEN EDIT-CURSOR-DOWN
                   AND  PONTEIRO < C
                        ADD 1 TO PONTEIRO
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN EDIT-CURSOR-UP
                   AND  PONTEIRO > I
                        SUBTRACT 1 FROM PONTEIRO
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN EDIT-PAGE-DOWN
                        IF   PONTEIRO < (C - 14)
                             ADD FULL-LIMITE TO PONTEIRO
                             PERFORM 210-EXIBE THRU 210-99-FIM
                        ELSE
                             IF   PONTEIRO NOT = (C - 14)
                                  COMPUTE PONTEIRO = C - 14
                                  IF   PONTEIRO < 1
                                       MOVE 1 TO PONTEIRO
                                  END-IF
                                  PERFORM 210-EXIBE THRU 210-99-FIM
                             END-IF
                        END-IF
                   WHEN EDIT-PAGE-UP
                        IF   PONTEIRO > (I + FULL-LIMITE)
                             SUBTRACT FULL-LIMITE FROM PONTEIRO
                             PERFORM 210-EXIBE THRU 210-99-FIM
                        ELSE
                             IF   PONTEIRO NOT = I
                                  MOVE I TO PONTEIRO
                                  PERFORM 210-EXIBE THRU 210-99-FIM
                             END-IF
                        END-IF
                   WHEN EDIT-HOME OR EDIT-CONTROL-PAGE-UP
                        MOVE I TO PONTEIRO
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN EDIT-END OR EDIT-CONTROL-PAGE-DOWN
                        COMPUTE PONTEIRO = C - 14
                        IF   PONTEIRO < I
                             MOVE I TO PONTEIRO
                        END-IF
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN EDIT-CURSOR-RIGHT
                   AND  COLUNA < LIMITE-t
                        ADD 1 TO COLUNA
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN EDIT-CURSOR-LEFT
                   AND  COLUNA > 1
                        SUBTRACT 1 FROM COLUNA
                        PERFORM 210-EXIBE THRU 210-99-FIM
                   WHEN(EDIT-ENTER
                     OR EDIT-CONTROL-CURSOR-RIGHT)
                    AND  (NOT EDIT-TAB)
                        IF   (COLUNA + 10) < LIMITE-t
                             ADD 10 TO COLUNA
                             PERFORM 210-EXIBE THRU 210-99-FIM
                        ELSE
                             IF   COLUNA NOT = LIMITE-t
                                  MOVE LIMITE-t TO COLUNA
                                  PERFORM 210-EXIBE THRU 210-99-FIM
                             END-IF
                        END-IF
                   WHEN EDIT-CONTROL-CURSOR-LEFT
                     OR EDIT-TAB
                        IF   COLUNA > 10
                             SUBTRACT 10 FROM COLUNA
                             PERFORM 210-EXIBE THRU 210-99-FIM
                        ELSE
                             IF   COLUNA > 1
                                  MOVE 1 TO COLUNA
                                  PERFORM 210-EXIBE THRU 210-99-FIM
                             END-IF
                        END-IF
                   END-EVALUATE
           END-PERFORM.

           MOVE 23   TO LINE-RODAPE
           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
           IF   EDIT-ALT-F4
                STOP RUN
           END-IF
           MOVE ZERO TO TECLA.

       200-99-FIM. EXIT.

       210-EXIBE.

           MOVE SPACES   TO TEXTO-TELA
           MOVE PONTEIRO TO RK-PRNTER

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > FULL-LIMITE
                   PERFORM 335-READ-PRNTE THRU 335-99-FIM
                   IF   FS-PRNTER > "09"
                   OR   PRINTS-WS = SPACES
                   OR   RK-PRNTER > C
                        MOVE SPACES  TO TEXTO2 (Y)
                   ELSE
                        MOVE SPACES TO PRINTS-WS2
                        MOVE 0      TO CO2
                        PERFORM VARYING CO FROM 1 BY 1 UNTIL CO > TR
                           IF   PRINTS-WS (CO: 1) = X"00" OR X"0C"
                                CONTINUE
                           ELSE
                                IF   PRINTS-WS (CO: 2) > X"5C60"
                                AND  PRINTS-WS (CO: 2) < X"5C71"
                                     ADD 1 TO CO
                                ELSE
                                     ADD 1 TO CO2
                                     MOVE PRINTS-WS  (CO:  1)
                                       TO PRINTS-WS2 (CO2: 1)
                                END-IF
                           END-IF
                        END-PERFORM
                        MOVE PRINTS-WS2 (COLUNA: 80) TO TEXTO2 (Y)
                   END-IF
                   IF   FULL-SCREEN = 1
                        COMPUTE ROW-NUMBER = Y - 1
                        MOVE 0 TO COLUMN-NUMBER
                   ELSE
                        COMPUTE ROW-NUMBER = Y + 5
                        MOVE 1 TO COLUMN-NUMBER
                   END-IF
                   IF   ZEBRADO = 1
                        IF   RK-PRNTER (10: 1) = "0" OR "2" OR "4"
                                                  OR "6" OR "8"
                             MOVE ZEBRA-1 TO ZEBRA
                             IF   CWDIRS-NOTA = "Gerando..."
                             OR   "Terminado anormal"
                             OR   "Interrompido"
                             OR   CWDIRS-USUARIO = "Importando..."
                                  MOVE ZEBRA-3 TO ZEBRA
                             END-IF
                        ELSE
                             MOVE ZEBRA-2 TO ZEBRA
                             IF   CWDIRS-NOTA = "Gerando..."
                             OR   "Terminado anormal"
                             OR   "Interrompido"
                             OR   CWDIRS-USUARIO = "Importando..."
                                  MOVE ZEBRA-4 TO ZEBRA
                             END-IF
                        END-IF
                   ELSE
                        MOVE ATT-T2  TO ZEBRA
                   END-IF
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                      TEXTO2 (Y)
                                                      ZEBRA
                                                      STRING-LENGTH2
                   ADD  1 TO RK-PRNTER
           END-PERFORM.

       210-99-FIM. EXIT.

       220-IMPRIMIR.

           MOVE LINE-RODAPE TO LINE-LISTAR
           MOVE ZERO TO P-I
                        N-L FIRST-FLAG
           MOVE 9999 TO P-F

           IF   CWDIRS-FOLHAS NOT = 0
                MOVE 1             TO P-I P-I-Z
                MOVE CWDIRS-FOLHAS TO P-F P-F-Z
                MOVE SPACES TO POP
                STRING "Da folha " P-I-Z " a " P-F-Z DELIMITED BY SIZE
                       INTO POP
                IF   LINE-RODAPE = 23
                     CALL "CWMSGW" USING "230370" POP
                ELSE
                     CALL "CWMSGW" USING "250370" POP
                END-IF
                PERFORM TEST AFTER
                        UNTIL (P-F NOT < P-I
                          AND  P-I NOT < 1
                          AND  P-F NOT > CWDIRS-FOLHAS)
                           OR (9999 = P-I AND P-F)
                        IF   LINE-RODAPE = 23
                             CALL "CBL_SET_CSR_POS" USING X"160B"
                        ELSE
                             CALL "CBL_SET_CSR_POS" USING X"180B"
                        END-IF
                        MOVE LINE-RODAPE TO CWMOUS-CURSOR-LIN
                        MOVE 12          TO CWMOUS-CURSOR-COL
                        PERFORM 320-STOP-EXIT THRU 320-99-FIM
                        IF  (CWMOUS-KEY = 0 OR 255)
                             ACCEPT (LINE-RODAPE, 12) P-I
                                     WITH UPDATE PROMPT
                             PERFORM ATTR-70
                             ACCEPT TECLA FROM ESCAPE KEY
                             IF   TECLA = 1
                                  MOVE 9999 TO P-I P-F
                             END-IF
                             IF   NOT (P-I = 9999 AND P-F = 9999)
                              IF   LINE-RODAPE = 23
                                   CALL "CBL_SET_CSR_POS" USING X"1612"
                              ELSE
                                   CALL "CBL_SET_CSR_POS" USING X"1812"
                              END-IF
                                  PERFORM 320-STOP-EXIT THRU 320-99-FIM
                                  IF   CWMOUS-KEY = 0 OR 255
                                       MOVE LINE-RODAPE
                                         TO CWMOUS-CURSOR-LIN
                                       MOVE 19
                                         TO CWMOUS-CURSOR-COL
                                       ACCEPT  (LINE-RODAPE, 19) P-F
                                               WITH UPDATE PROMPT
                                       ON   ESCAPE
                                            MOVE 9999 TO P-I P-F
                                       END-ACCEPT
                                       PERFORM ATTR-70
                                  ELSE
                                       IF   CWMOUS-KEY = 1
                                            MOVE 9999 TO P-I P-F
                                       END-IF
                                  END-IF
                             END-IF
                        ELSE
                             IF   CWMOUS-KEY = 1
                                  MOVE 9999 TO P-I P-F
                             END-IF
                        END-IF
                        IF   P-I < 1
                        AND  NOT (9999 = P-I AND P-F)
                             MOVE 1 TO P-I
                        END-IF
                        IF   P-F > CWDIRS-FOLHAS
                        AND  NOT (9999 = P-I AND P-F)
                             MOVE CWDIRS-FOLHAS TO P-F
                        END-IF
                        IF   P-I > 1
                             MOVE 1 TO FIRST-FLAG
                        END-IF
                END-PERFORM
                CALL "CBL_SET_CSR_POS" USING X"0000"
                IF   (9999 = P-I AND P-F)
                     GO TO FECHAR-RT
                END-IF
                MOVE 1             TO RK-PRNTER
                MOVE 0             TO PAGINA-LIVRE
                IF   P-I NOT = 1
                     MOVE    P-I        TO PG
                     MOVE    0          TO FIM-PROCURA
                     MOVE    0          TO TESTE-PROCURA
                     PERFORM 225-ACHA THRU 225-99-FIM
                             UNTIL FIM-PROCURA = 1
                     MOVE    PONTO      TO I RK-PRNTER
                ELSE
                     MOVE 1             TO I
                     MOVE 9999          TO PG
                     MOVE 1             TO PAGINA-LIVRE
                END-IF
                MOVE CWDIRS-LINHAS TO C
           ELSE
                IF    CWDIRS-FOLHAS = 0
                      COMPUTE VEZES = CWDIRS-LINHAS - PONTEIRO + 1
                      MOVE    PONTEIRO TO PONTO
                ELSE
                      COMPUTE VEZES = CWDIRS-FOLHAS - PONTEIRO + 1
                END-IF
                PERFORM TEST AFTER UNTIL N-L NOT GREATER VEZES
                        MOVE VEZES TO N-L
                        MOVE SPACE TO RESPOSTA
                        IF   LINE-RODAPE = 23
==>                          CALL "CWMSGW" USING "230370" ESPACOS
                             CALL "CWMSGW" USING "230307" "Linhas:"
                             CALL "CWMSGW" USING "231110" N-L
                             CALL "CBL_SET_CSR_POS" USING X"160A"
                             MOVE "2311" TO CWMOUS-CURSOR-POSITION
                        ELSE
                             CALL "CWMSGW" USING "250370" ESPACOS
                             CALL "CWMSGW" USING "250307" "Linhas:"
                             CALL "CWMSGW" USING "251110" N-L
                             CALL "CBL_SET_CSR_POS" USING X"180A"
                             MOVE "2511" TO CWMOUS-CURSOR-POSITION
                        END-IF
                        MOVE    2                TO PEDE-IMPRESSORA
                        PERFORM 300-INTERROMPE THRU 300-99-FIM
                        MOVE    0                TO PEDE-IMPRESSORA
                        IF   CWMOUS-KEY = 1
                             MOVE 9999999999 TO N-L
                             EXIT PERFORM
                        END-IF
                        IF   CWMOUS-KEY NOT = 13
                             IF   RESPOSTA = "S" OR "s"
                                  MOVE 9999999999 TO N-L
                                  EXIT PERFORM
                             ELSE
                                  IF  RESPOSTA = SPACE
                                      ACCEPT  (LINE-RODAPE, 11) N-L
                                             WITH UPDATE PROMPT
                                      ON   ESCAPE
                                           MOVE 9999999999 TO N-L
                                           PERFORM ATTR-70
                                           EXIT PERFORM
                                      END-ACCEPT
                                      PERFORM ATTR-70
                                      IF   N-L > VEZES
                                           CALL X"E5"
                                      END-IF
                                  END-IF
                             END-IF
                        ELSE
                             MOVE VEZES TO N-L
                        END-IF
                END-PERFORM
                MOVE PONTEIRO      TO I
                COMPUTE C = PONTEIRO + N-L - 1
           END-IF.

       FECHAR-RT.

           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM

           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230370" ESPACOS
           ELSE
                CALL "CWMSGW" USING "250370" ESPACOS
           END-IF
           MOVE PONTEIRO             TO SALVA-PONTEIRO-2
           MOVE I                    TO PONTEIRO
           MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
           MOVE 1           TO CWMOUS-MODE
           MOVE ALL X'01'   TO CWMOUS-LINE (23) (61: 11)
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "236111" "[esc]-Parar"
                MOVE ALL X'01'   TO CWMOUS-LINE (23) (61: 11)
           ELSE
                CALL "CWMSGW" USING "256111" "[esc]-Parar"
                MOVE ALL X'01'   TO CWMOUS-LINE (25) (61: 11)
           END-IF

           PERFORM 260-LISTAR      THRU 260-99-FIM
           MOVE 23                   TO LINE-LISTAR
           MOVE    SALVA-PONTEIRO-2  TO PONTEIRO
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
           PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM.

       220-99-FIM. EXIT.

       222-MAKE-PATH.

           IF   PATH = SPACES
                GO TO 220-99-FIM
           END-IF

           MOVE SPACES TO PATH-MD
           PERFORM VARYING MP-I FROM LENGTH PATH BY -1
                             UNTIL MP-I = 0
                                OR (PATH(MP-I:1) NOT = " ")
                   CONTINUE
           END-PERFORM

           MOVE MP-I TO MP-Y
           ADD  1    TO MP-Y
           PERFORM VARYING MP-I FROM 1 BY 1 UNTIL MP-I > MP-Y
                   IF   PATH(MP-I:1) = "\" OR "/" OR " "
                        IF   CWUNIX-ON
                             MOVE "/" TO PATH(MP-I:1)
                        ELSE
                             MOVE "\" TO PATH(MP-I:1)
                        END-IF
                        CALL "CBL_CREATE_DIR" USING PATH-MD
                   END-IF
                   MOVE PATH(MP-I:1) TO PATH-MD(MP-I:1)
           END-PERFORM.

       222-99-FIM. EXIT.

       225-ACHA.

           PERFORM 335-READ-PRNTE THRU 335-99-FIM

           IF   RK-PRNTER = 1
           AND (PRINTS-WS (1: 1) NOT = X"0C")
           AND (PRINTS-WS (2: 1) NOT = X"0C")
           AND (PRINTS-WS (3: 1) NOT = X"0C")
                MOVE 1 TO PAGINA-LIVRE
           END-IF

           IF   FS-PRNTER > "09"
                COMPUTE PONTO = RK-PRNTER - 1
                MOVE 1      TO FIM-PROCURA
                MOVE SPACES TO PRINTS-WS
                               PRNTER-REG
           ELSE
                MOVE PRINTS-WS TO PRNTER-REG
           END-IF

           IF   PRNTER-REG (1: 1) = X"0C"
                ADD 1 TO PAGINA-LIVRE
                IF   PAGINA-LIVRE = PG
                     MOVE 1         TO FIM-PROCURA
                     MOVE RK-PRNTER TO PONTO
                END-IF
           ELSE
                IF   PRNTER-REG (3: 1) = X"0C" OR X"00"
                     IF   PRNTER-REG (p1: 1) = PG-1
                     AND  PRNTER-REG (p2: 1) = PG-2
                     AND  PRNTER-REG (p3: 1) = PG-3
                     AND  PRNTER-REG (p4: 1) = PG-4
                          MOVE 1         TO FIM-PROCURA
                          MOVE RK-PRNTER TO PONTO
                     END-IF
                END-IF
           END-IF

           ADD 1 TO RK-PRNTER

           IF   1 = FIM-PROCURA AND TESTE-PROCURA
                PERFORM TEST AFTER
                        UNTIL PRNTER-REG (3: 1) = X"0C" OR X"00"
                           OR PRNTER-REG (1: 1) = X"0C" OR X"00"
                                OR FS-PRNTER > "09"
                        PERFORM 335-READ-PRNTE THRU 335-99-FIM
                        IF   FS-PRNTER < "10"
                             MOVE RK-PRNTER TO PONTO
                             MOVE PRINTS-WS TO PRNTER-REG
                        ELSE
                             MOVE SPACES TO PRNTER-REG
                                            PRINTS-WS
                        END-IF
                        ADD 1 TO RK-PRNTER
                END-PERFORM
                SUBTRACT 1 FROM PONTO.

       225-99-FIM. EXIT.

       230-LOCALIZA.

           move LINE-RODAPE to line-listar
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230315" "String:        "
                CALL "CWMSGW" USING "231656" CADEIA
                MOVE "2311" TO CWMOUS-CURSOR-POSITION
           ELSE
                CALL "CWMSGW" USING "250315" "String:        "
                CALL "CWMSGW" USING "251662" CADEIA
                MOVE "2511" TO CWMOUS-CURSOR-POSITION
           END-IF
           MOVE    SPACE            TO RESPOSTA
           MOVE    3                TO PEDE-IMPRESSORA
           PERFORM 300-INTERROMPE THRU 300-99-FIM
           MOVE    0                TO PEDE-IMPRESSORA

           IF   CWMOUS-KEY = 1
           OR   RESPOSTA = "S" OR "s"
                GO TO 230-99-FIM
           ELSE
                IF   CWMOUS-KEY NOT = 13
                     ACCEPT (LINE-RODAPE, 11) CADEIA WITH UPDATE
                     PERFORM ATTR-70
                END-IF
           END-IF

           IF   CADEIA NOT = SPACES
                MOVE ZERO TO IGUAIS
                PERFORM VARYING TC FROM 62 BY -1
                        UNTIL BYTE-S (TC) NOT = 32
                END-PERFORM
                PERFORM VARYING T FROM 1 BY 1
                        UNTIL T > TC
                        IF   BYTE-S (T) GREATER 96
                        AND  LESS 123
                             SUBTRACT 32 FROM BYTE-S (T)
                        END-IF
                END-PERFORM
                COMPUTE RK-PRNTER = PONTEIRO + 1
                MOVE    0        TO CWMOUS-MODE
                PERFORM 240-BUSCA THRU 240-99-FIM
                        VARYING RK-PRNTER FROM RK-PRNTER BY 1
                                UNTIL IGUAIS = TC
                                   OR RK-PRNTER > C
                IF   LINE-RODAPE = 23
                     CALL "CWMSGW" USING "236111" ESPACOS
                ELSE
                     CALL "CWMSGW" USING "256111" ESPACOS
                END-IF
                IF   IGUAIS = TC
                     COMPUTE PONTEIRO = RK-PRNTER - 1
                     COMPUTE COLUNA   = X - 1
                     IF   PRNTER-REG (3: 1) = X"0C" OR X"00"
                     AND  COLUNA > 3
                          SUBTRACT 3 FROM COLUNA
                     END-IF
                     IF   PRNTER-REG (1: 1) = X"0C" OR X"00"
                     AND  COLUNA > 1
                          SUBTRACT 1 FROM COLUNA
                     END-IF
                     PERFORM 210-EXIBE THRU 210-99-FIM.

       230-99-FIM. EXIT.

       240-BUSCA.

           IF   CWMOUS-MODE = 0
                IF   LINE-RODAPE = 23
                     CALL "CWMSGW" USING "236111" "[esc]-Parar"
                ELSE
                     CALL "CWMSGW" USING "256111" "[esc]-Parar"
                END-IF
                MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
                MOVE 1           TO CWMOUS-MODE
                MOVE ALL X'01'   TO CWMOUS-LINE (23) (61: 11).

           PERFORM 335-READ-PRNTE THRU 335-99-FIM
           IF   FS-PRNTER > "09"
                MOVE SPACES TO PRINTS-WS
                               PRNTER-REG
           ELSE
                MOVE PRINTS-WS TO PRNTER-REG
           END-IF

           MOVE ZERO TO IGUAIS
           INSPECT PRNTER-REG CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE    PRNTER-REG TO PRNTER-SAVE
           INSPECT PRNTER-SAVE REPLACING FIRST CADEIA (1: TC)
                                           BY CADEIA2 (1: TC)
           IF PRNTER-REG NOT = PRNTER-SAVE
              PERFORM VARYING X FROM 1 BY 1 UNTIL (X + TC) > TR
                                               OR IGUAIS = TC
                      IF   CADEIA (1: TC) = PRNTER-REG (X: TC)
                           MOVE TC TO IGUAIS
                      END-IF
              END-PERFORM
           END-IF

           CALL "CWMOUS" USING PARAMETROS-CWMOUS

           IF   CWMOUS-KEY = 1
                MOVE 27 TO TECLA
           ELSE
                CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
           END-IF

           IF   KEY-STATUS   = 1
           OR   CWMOUS-KEY = 1
                IF   CWMOUS-KEY NOT = 1
                     MOVE LINE-RODAPE  TO CURPOS-LIN
                     MOVE 03           TO CURPOS-COL
                     CALL "CWKBDC" USING CURPOS CHAR TECLA
                END-IF
                IF   TECLA = 27
                     MOVE 0           TO TECLA
                     MOVE SPACES      TO CWSEND-SCREENS RESPOSTA
                     MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                     MOVE "~Continuar" TO CWSEND-SCREEN (2)
                     MOVE "Interromper pesquisa ?" TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     IF   CWSEND-OPTION = 1
                          MOVE "S" TO RESPOSTA
                     END-IF
                     IF    RESPOSTA = "S" OR "s"
                           COMPUTE RK-PRNTER = C + 1
                     ELSE
                           IF   LINE-RODAPE = 23
                                CALL "CWMSGW" USING "230307" "String: "
                                CALL "CWMSGW" USING "231162" CADEIA
                             CALL "CWMSGW" USING "236111" "[esc]-Parar"
                           ELSE
                                CALL "CWMSGW" USING "250307" "String: "
                                CALL "CWMSGW" USING "251162" CADEIA
                             CALL "CWMSGW" USING "256111" "[esc]-Parar"
                           END-IF
                     END-IF
                END-IF
           END-IF.

       240-99-FIM. EXIT.

       250-REMOVER.

           MOVE CWDIRS-SPOOL TO LB-PRNTER
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM

           IF   CWDIRS-NUMERO NOT = 0
                MOVE "Erased" TO CWDIRS-COMANDO
                INITIALIZE REPKEY-NUMERO
                MOVE "Erased" TO REPKEY-COMANDO
                CALL "CBL_DELETE_FILE" USING LB-PRNTER
                IF   RETURN-CODE = 0
                     DELETE CWDIRS RECORD
                     REWRITE REPKEY-REG
                ELSE
                     MOVE 0 TO RETURN-CODE
                END-IF
           END-IF.

       250-99-FIM. EXIT.

       251-EXPORTA.

           IF   EXPORTOU = 0
                MOVE 046 TO CWPATH-COLOR-FRAME
                            CWPATH-COLOR-BORDER
                MOVE 126 TO CWPATH-COLOR-BARR-MENU
                SET CWPATH-WITH-DIR     TO TRUE
                SET CWPATH-WITH-DRIVES  TO TRUE
                SET CWPATH-WITH-NEWDIR  TO TRUE
                SET CWPATH-WITH-NEWFILE TO TRUE
                MOVE "_Exportar_para:"    TO CWPATH-TITLE
                MOVE "REPORTS.cw6"        TO CWPATH-DEFAULT
                MOVE SPACES               TO CWPATH-FILE
                                             CWPATH-PATH
                IF  PATH-SPOOL (2: 1) = ":"
                    CALL "CBL_CREATE_DIR" USING PATH-SPOOL
                    STRING PATH-SPOOL DELIMITED BY SPACE
                           "\*.cw6"   DELIMITED BY SIZE
                           INTO CWPATH-PATH
                ELSE
                    IF   CWUNIX-OFF
                         CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                     RETURN-STATUS
                    END-IF
                    CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                           SIZE-OLD-DIR
                    IF   CWUNIX-OFF
                         STRING OLD-DRIVE DELIMITED BY SPACE
                                ":\"           DELIMITED BY SPACE
                                OLD-DIRECTORY  DELIMITED BY SPACE
                                "\"            DELIMITED BY SIZE
                                PATH-SPOOL     DELIMITED BY SPACE
                                "\*.cw6"       DELIMITED BY SIZE
                         INTO CWPATH-PATH
                    ELSE
                         STRING OLD-DIRECTORY  DELIMITED BY SPACE
                                "/"            DELIMITED BY SIZE
                                PATH-SPOOL     DELIMITED BY SPACE
                                "/*.cw6"       DELIMITED BY SIZE
                         INTO CWPATH-PATH
                    END-IF
                END-IF
                CALL "CWPATH" USING PARAMETROS-CWPATH
                IF   CWPATH-FILE = SPACES
                     MOVE 2 TO EXPORTOU
                ELSE
                     MOVE CWPATH-FILE TO LB-TEXTO
                     OPEN INPUT TEXTO
                     IF   FS-TEXTO = "30" OR "35"
                          OPEN OUTPUT TEXTO
                          IF   FS-TEXTO NOT = "00"
                               CALL "CWISAM" USING ER-TEXTO
                               MOVE 2 TO EXPORTOU
                          ELSE
                               MOVE 1 TO EXPORTOU
                          END-IF
                     ELSE
                          IF   FS-TEXTO < "10"
                               MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                               STRING "Arquivo: " DELIMITED BY SIZE
                                  LB-TEXTO  DELIMITED BY SPACE
                                  " j  existe !" DELIMITED BY SIZE
                                     INTO CWSEND-MSG
                               MOVE "~Sobrepor " TO CWSEND-SCREEN  (1)
      *                        MOVE "~Extender " TO CWSEND-SCREEN  (2)
                               MOVE "  ~Outro"   TO CWSEND-SCREEN  (2)
                               MOVE "~Cancelar"  TO CWSEND-SCREEN  (3)
                               CALL "CWSEND" USING PARAMETROS-CWSEND
                               CLOSE TEXTO
                               EVALUATE CWSEND-OPTION
                                        WHEN 1 OPEN OUTPUT TEXTO
                                               MOVE 1 TO EXPORTOU
      *                                 WHEN 2 OPEN EXTEND TEXTO
      *                                        MOVE 1 TO EXPORTOU
                                        WHEN 2 GO TO 251-EXPORTA
                                        WHEN OTHER
                                             MOVE 2 TO EXPORTOU
                                END-EVALUATE
                          END-IF
                     END-IF
                END-IF
           END-IF

           IF   EXPORTOU NOT = 2
                MOVE CWDIRS-SPOOL TO LB-PRNTER
                PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
                PERFORM 252-ABRE-PERC    THRU 252-99-FIM
                MOVE CWDIRS-DATA       TO CWTIME-DATE
                SET  CWTIME-REVERSED   TO TRUE
                SET  CWTIME-REVERSE    TO TRUE
                CALL "CWTIME" USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL TO OBS-DATA
                IF   LINE-RODAPE = 23
                     CALL "CWMSGW" USING "230370" CANCELAR
                ELSE
                     CALL "CWMSGW" USING "250370" CANCELAR
                END-IF
                CALL "CWMSGW" USING "101216" "Exportando para:"
                CALL "CWMSGW" USING "111250" LB-TEXTO
                CALL "CBL_SET_CSR_POS" USING X"0B0A"
                CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR
                MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
                MOVE 1           TO CWMOUS-MODE
                MOVE ALL X'01'   TO CWMOUS-LINE (23) (03: 14)
                STRING CWDIRS-CODIGO DELIMITED BY SPACE
                       " "           DELIMITED BY SIZE
                       OBS-DATA      DELIMITED BY SIZE
                       " "           DELIMITED BY SIZE
                       CWDIRS-HORA   DELIMITED BY SIZE
                       ":"           DELIMITED BY SIZE
                       CWDIRS-MINUTO DELIMITED BY SIZE
                             INTO CWPERC-MSG
                CLOSE TEXTO
                IF CWDIRS-EXT = 'spl'
                   move low-values to texto-handle
                   call "CBL_CREATE_FILE" using LB-TEXTO
                                           X"02" X"00" X"00"
                                           texto-handle
                ELSE
                   OPEN EXTEND TEXTO
                   CALL X"91" USING X91-SRCOUT-RESULT
                                    X91-SRCOUT-FUNCTION
                                    TEXTO
                END-IF
                MOVE CWDIRS-REG TO TEXTO-REG
                MOVE "CW"       TO TEXTO-REG(1:2)
                PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
                IF CWDIRS-EXT = 'spl'
                   MOVE 0 TO TEXTO-offset
                   move length cwdirs-reg to texto-count
                   add  1                 to texto-count
                   move x'0A'             to texto-reg (texto-count:1)
                   call "CBL_WRITE_FILE" using texto-handle
                                    TEXTO-offset
                                    TEXTO-count
                                    X"00"
                                    TEXTO-REG
                   add texto-count to texto-offset
                ELSE
                   WRITE TEXTO-REG
                END-IF
                MOVE CWDIRS-LINHAS  TO CWPERC-EMPTY
                PERFORM VARYING RK-PRNTER FROM 1 BY 1
                                UNTIL RK-PRNTER > CWDIRS-LINHAS
                                   OR RESPOSTA = "S"
                 PERFORM 335-READ-PRNTE THRU 335-99-FIM
                 MOVE PRINTS-WS   TO TEXTO-REG
                 MOVE RK-PRNTER   TO CWPERC-FULL
                 CALL "CWPERC" USING PARAMETROS-CWPERC
                 IF CWDIRS-EXT = 'spl'
                    call "CBL_WRITE_FILE" using TEXTO-handle
                                                TEXTO-offset
                                                byte-count
                                                X"00"
                                                BIN-REG
                    ADD BYTE-COUNT TO TEXTO-OFFSET
                 ELSE
                    WRITE TEXTO-REG
                 END-IF
                 CALL "CWMOUS" USING PARAMETROS-CWMOUS
                 IF   CWMOUS-KEY = 1
                      MOVE 27 TO TECLA
                 ELSE
                      CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                 END-IF
                 IF   KEY-STATUS = 1
                 OR   CWMOUS-KEY = 1
                      IF   CWMOUS-KEY NOT = 1
                           MOVE 23         TO CURPOS-LIN
                           MOVE 03         TO CURPOS-COL
                           CALL "CWKBDC" USING CURPOS CHAR TECLA
                      END-IF
                      IF   TECLA = 27
                           MOVE 0           TO TECLA
                           MOVE SPACES      TO CWSEND-SCREENS RESPOSTA
                           MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                           MOVE "~Continuar" TO CWSEND-SCREEN (2)
                           MOVE "Cancelar exporta‡Æo ?" TO CWSEND-MSG
                           CALL "CWSEND" USING PARAMETROS-CWSEND
                           IF   CWSEND-OPTION = 1
                                MOVE "S" TO RESPOSTA
                                MOVE 2   TO EXPORTOU
                           END-IF
                      END-IF
                 END-IF
                END-PERFORM
                MOVE 0 TO CWMOUS-MODE
                IF CWDIRS-EXT = 'spl'
                   call "CBL_CLOSE_FILE" using texto-handle
                ELSE
                   CLOSE TEXTO
                END-IF
                PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                IF   RESPOSTA = "S"
                     DELETE FILE TEXTO
                END-IF
               SET CWBOXW-CLOSE TO TRUE
               CALL "CWBOXW" USING PARAMETROS-CWBOXW
               MOVE "*"         TO REPKEY-COMANDO
               REWRITE             REPKEY-REG
           END-IF.

       251-99-FIM. EXIT.

       252-ABRE-PERC.

           MOVE 10  TO CWBOXW-LINE
           MOVE 12  TO CWPERC-LINE
           MOVE 10  TO CWBOXW-COLUMN
           MOVE 12  TO CWPERC-COLUMN
           MOVE 02  TO CWBOXW-VERTICAL-LENGTH
           MOVE 52  TO CWBOXW-HORIZONTAL-LENGTH
           MOVE 110 TO CWBOXW-COLOR-FRAME
                       CWBOXW-COLOR-BORDER
           SET CWBOXW-OPEN TO TRUE
           CALL "CWBOXW" USING PARAMETROS-CWBOXW.

       252-99-FIM. EXIT.

       255-NOTAS.

           IF   SAVE-NOTA = "Gerando..."
           OR   "Terminado anormal"
           OR   "Interrompido"
                GO TO 255-99-FIM
           END-IF
           IF   VEZ-NOTA = 1
                COMPUTE ROW-NUMBER = LINE-NOTA - 1
                CALL "CBL_READ_SCR_CHARS" USING SCREEN-POSITION
                                                PRE-RELATORIO
                                                X"004E"
                IF   PRE-RELATORIO (9: 70) NOT = SAVE-RELATORIO (9: 70)
                     PERFORM VARYING ROW-NUMBER FROM 9 BY 1
                               UNTIL ROW-NUMBER > 19
                               CALL "CBL_WRITE_SCR_CHARS"
                                    USING SCREEN-POSITION
                                          ESPACOS
                                          X"004E"
                     END-PERFORM
                     COMPUTE ROW-NUMBER = LINE-NOTA - 1
                     CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                      SAVE-RELATORIO
                                                      X"004E"
                END-IF
                CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                  REVERTE-VIDEO
                                                  SIZE-BARR-MENU
                MOVE "Informe a nova nota" TO POP
                IF   LINE-RODAPE = 23
                     CALL "CWMSGW" USING "230370" POP
                ELSE
                     CALL "CWMSGW" USING "250370" POP
                END-IF
                MOVE SPACES TO POS-W
                STRING LINE-NOTA "4920" DELIMITED BY SIZE INTO POS-W
                CALL "CWMSGW" USING POS-W SAVE-NOTA
                PERFORM TEST AFTER
                             UNTIL NOT (SAVE-NOTA = "Gerando..."
                                      OR   "Terminado anormal"
                                      OR   "Interrompido")
                        ACCEPT (LINE-NOTA, 49) SAVE-NOTA WITH UPDATE
                        COMPUTE ROW-NUMBER = LINE-NOTA - 1
                        MOVE 48 TO COLUMN-NUMBER
                        CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                                         ATT-T
                                                         SIZE-ATT-T
                END-PERFORM
                MOVE   2 TO VEZ-NOTA
                CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                  ATT-T
                                                  SIZE-BARR-MENU
           END-IF
           IF   CWDIRS-NOTA = "Gerando..."
           OR   "Terminado anormal"
           OR   "Interrompido"
                GO TO 255-99-FIM
           END-IF

           IF   SAVE-NOTA NOT = CWDIRS-NOTA
                MOVE SAVE-NOTA TO CWDIRS-NOTA
                IF   COLUNA-SORT = 4
                     MOVE 1 TO REORDENAR
                END-IF
                MOVE "*" TO REPKEY-COMANDO
                REWRITE CWDIRS-REG
           END-IF.

       255-99-FIM. EXIT.

       260-LISTAR.

           set CWEXEC-NOWARNING  to true
           set CWEXEC-ASSYNCRONE to true

           IF   LINE-LISTAR = 23
                CALL "CWMSGW" USING "230370" ESPACOS
           ELSE
                CALL "CWMSGW" USING "250370" ESPACOS
           END-IF
           IF   LB-PRINTS = SPACES
                MOVE 1 TO VEZ
           END-IF

           IF   (9999 = P-I AND P-F)
                MOVE 0 TO LISTE
                GO TO 260-99-FIM
           ELSE
                MOVE 1 TO LISTE
                IF   VEZ = 1
                     MOVE 2 TO VEZ
                     PERFORM 340-SET-PRINTER THRU 340-99-FIM
                     MOVE 255 TO PRINTER-NO
                     IF IMP-0 MOVE 0 TO PRINTER-NO END-IF
                     IF IMP-1 MOVE 1 TO PRINTER-NO END-IF
                     IF IMP-2 MOVE 2 TO PRINTER-NO END-IF
                END-IF
           END-IF
Pia   *
Pia   *    IF   SPOOL-DEV = "$"
Pia   *    AND (P-I NOT = 1
Pia   *    OR   P-F NOT = CWDIRS-FOLHAS)
Pia   *    AND  VER
Pia   *         MOVE SPACES                         TO CWSEND-SCREENS
Pia   *         MOVE "ImpressÆo parcial imposs¡vel" TO CWSEND-MSG
Pia   *         CALL "CWSEND" USING PARAMETROS-CWSEND
Pia   *         GO TO 260-99-FIM
Pia   *    END-IF

           IF   LISTE  = 0
                GO TO 260-99-FIM
           END-IF

           MOVE CWDIRS-SPOOL TO LB-PRNTER
           MOVE 8            TO T
           MOVE ZERO         TO VIAS
                                VIA

           PERFORM VARYING X FROM 7 BY -1 UNTIL X = 0
                   IF   BYTE-T (X) NUMERIC
                        SUBTRACT 1 FROM T
                        MOVE BYTE-T (X) TO BYTE-VIA (T)
                        MOVE SPACE      TO BYTE-T (X)
                   END-IF
           END-PERFORM

           IF   VIAS = 0
                MOVE 1 TO VIAS.

           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
           IF   SPOOL-DEV = "$"
           AND  CWDIRS-ESTILO = SPACE
                PERFORM 261-LABEL-SPOOL THRU 261-99-FIM
                SET IMPRIMIU TO TRUE
                GO TO 260-99-FIM
           END-IF

           IF   SPOOL-DEV = "$"
      *         COMPUTE VIA = VIAS - 1
                MOVE "sp000000.txt" TO PRINTS-WORK
                MOVE CWSPLTXT       TO PRINTS-WORK (10: 3)
                IF   CWUNIX-OFF
                     IF   WINPRINT (8: 1) = ":"
                          MOVE WINPRINT (9: ) TO WINDOWS-PRINTER
                          MOVE SPACES TO WINPRINT (9:)
                          PERFORM POSTER-CHECK THRU FIM-POSTER-CHECK
                     END-IF
                     INSPECT WINPRINT CONVERTING MINUSCULAS
                                              TO MAIUSCULAS
                     IF   POSTER
                          MOVE "wp"  TO PRINTS-WORK (01: 2)
                          MOVE "spl" TO PRINTS-WORK (10: 3)
                     END-IF
                END-IF
                CALL "CWTASK" USING "3" PRINTS-WORK (3: 6)
                MOVE  PRINTS-WORK    TO LB-PRINTS
==>             MOVE SPACES TO TEMP
==>             DISPLAY "CWSPLTMP" UPON ENVIRONMENT-NAME
==>             ACCEPT   TEMP  FROM ENVIRONMENT-VALUE
==>             IF   TEMP NOT = SPACES
==>                  MOVE TEMP TO PATH
==>                  PERFORM 222-MAKE-PATH THRU 222-99-FIM
==>                  PERFORM VARYING S1 FROM LENGTH OF LB-PRINTS
==>                               BY -1
==>                          UNTIL S1 = 0
==>                                OR LB-PRINTS (S1: 1) = '\'
==>                                OR LB-PRINTS (S1: 1) = '/'
==>                          CONTINUE
==>                  END-PERFORM
==>                  ADD  1      TO S1
==>                  MOVE LB-PRINTS (S1:) TO SEM-PATH
==>                  MOVE SPACES TO LB-PRINTS
==>                  STRING TEMP      DELIMITED BY SPACE
==>                         BARRA-DIR DELIMITED BY SIZE
==>                         SEM-PATH  DELIMITED BY SPACE
==>                         INTO LB-PRINTS
==>             END-IF
           ELSE
                CALL "CWLABE" USING LB-PRINTS NEW EXTW
                CANCEL "CWLABE"
                IF  (EXTW NOT = SPACES)
                AND NEW = SPACES
                    GO TO 260-99-FIM
                END-IF
           END-IF

           PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
           OPEN OUTPUT PRINTS
           MOVE ALL "." TO CWPERC-MSG
           PERFORM 252-ABRE-PERC    THRU 252-99-FIM
           CALL "CWMSGW" USING "111214" "Imprimindo em "
           CALL "CWMSGW" USING "112630" IMPRESSORA
           if   ponto = 0
                move 1 to ponto
           end-if
           PERFORM UNTIL VIA = VIAS
                   IF   CWDIRS-PROGRAMA = SPACES
                        MOVE LOW-VALUES TO CWDIRS-PROGRAMA
                   END-IF
                   ADD  1              TO VIA
                   MOVE CWDIRS-CODIGO  TO OBS-CODIGO
                   MOVE CWDIRS-DATA       TO CWTIME-DATE
                   SET  CWTIME-REVERSED   TO TRUE
                   SET  CWTIME-REVERSE    TO TRUE
                   CALL "CWTIME" USING PARAMETROS-CWTIME
                   MOVE CWTIME-DATE-FINAL TO OBS-DATA
                   MOVE CWDIRS-HORA       TO OBS-HH
                   MOVE CWDIRS-MINUTO     TO OBS-MM
                   MOVE VIA            TO OBS-VIA
                   MOVE VIAS           TO OBS-VIAS
                   MOVE 0              TO T
                                          LP
                   MOVE OBS            TO OBS-PRINTS
                   EXEC COBOLware PAck String OBS-PRINTS END-EXEC
                   MOVE OBS-PRINTS TO CWPERC-MSG
                   MOVE 0 TO SALTO
                   COMPUTE PAGINA-LIVRE = P-I - 1
                   PERFORM VARYING RK-PRNTER FROM ponto
                                               BY 1
                                          UNTIL RK-PRNTER > C
                                             OR PAGINA-LIVRE > P-F
                     PERFORM 280-LIGADA THRU 280-99-FIM
                     IF   RK-PRNTER = 1
                     AND (PRINTS-WS (1: 1) NOT = X"0C")
                     AND (PRINTS-WS (2: 1) NOT = X"0C")
                     AND (PRINTS-WS (3: 1) NOT = X"0C")
                          MOVE 1 TO PAGINA-LIVRE
                     END-IF
                     IF   testa-fim = '9'
                          EXIT PERFORM
                     ELSE
                          PERFORM 265-GRAVA-LINHA THRU 265-99-FIM
                     END-IF
                     ADD   1          TO LP
                     MOVE LP          TO CWPERC-FULL
                     MOVE C           TO CWPERC-EMPTY
                     CALL "CWPERC" USING PARAMETROS-CWPERC
                     MOVE "N" TO RESPOSTA
                     MOVE 0   TO CWMOUS-MODE
                     PERFORM 300-INTERROMPE THRU 300-99-FIM
                     IF   RESPOSTA = "S" OR "s"
                          COMPUTE RK-PRNTER = C + 1
                          EXIT PERFORM
                     END-IF
                   END-PERFORM
           END-PERFORM

           CALL "CWMSGW" USING "057108" "        "
           SET CWBOXW-CLOSE TO TRUE
           CALL "CWBOXW" USING PARAMETROS-CWBOXW

           IF   testa-fim = '9'
                COMPUTE RK-PRNTER = C + 1
                MOVE SALVA-RT TO testa-fim
           END-IF

           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
           CLOSE PRINTS

           IF  (NEW NOT = SPACE)
           AND (EXTW NOT = SPACES)
                MOVE SPACES TO LB-TEXTO CWEXEC-COMANDO
                STRING LB-REPKEY DELIMITED BY "."
                       ".ini"    DELIMITED BY SIZE
                       INTO LB-TEXTO
                STRING  COBWARE DELIMITED BY SPACE
                 "\POSTER.EXE " DELIMITED BY SIZE
                       LB-TEXTO DELIMITED BY SPACE
                             INTO CWEXEC-COMANDO
                OPEN OUTPUT TEXTO
                WRITE TEXTO-REG FROM "[Config]"
                MOVE "Infile=" TO TEXTO-REG
                MOVE LB-PRINTS TO TEXTO-REG(8:)
                WRITE TEXTO-REG
                MOVE "Outfile=" TO TEXTO-REG
                MOVE NEW        TO TEXTO-REG(9:)
                WRITE TEXTO-REG
                WRITE TEXTO-REG FROM "Quiet=Yes"
                MOVE "OutputFormat=" TO TEXTO-REG
                INSPECT EXTW CONVERTING MINUSCULAS TO MAIUSCULAS
                IF EXTW = "HTM"
                   MOVE "HTML" TO EXTW
                END-IF
                MOVE EXTW            TO TEXTO-REG (14: )
                WRITE TEXTO-REG
                MOVE SPACES TO TEXTO-REG
                STRING "Title="        DELIMITED BY SIZE
                    CWDIRS-CODIGO      DELIMITED BY SPACE
                             ": "      DELIMITED BY SIZE
                         CWDIRS-TITULO DELIMITED BY SIZE
                         INTO TEXTO-REG
                WRITE TEXTO-REG
                CLOSE TEXTO
                CALL "CWEXE2" USING  PARAMETROS-CWEXEC
           END-IF

           IF   SPOOL-DEV NOT = "$"
           AND  DELETE-PRINTER = "S"
                DELETE FILE PRINTS
           END-IF

           IF   SPOOL-DEV = "$"
                MOVE LB-PRINTS  TO LB-PRNTER
                PERFORM 261-LABEL-SPOOL THRU 261-99-FIM
           END-IF

           IF   E-MAIL NOT = SPACES
                IF   CWDIRS-NOTA = SPACES
                     MOVE "Spool COBOLware 6.1" TO CWDIRS-NOTA
                END-IF
                MOVE SPACES TO EMAIL-TEXT
                DISPLAY 'CWSPOOLTEXTMAIL' UPON ENVIRONMENT-NAME
                ACCEPT EMAIL-TEXT FROM ENVIRONMENT-VALUE
                IF EMAIL-TEXT = SPACES
                IF CWUNIX-OFF
                STRING
            "Em anexo relat¢rio gerado por sistema no formato PDF^"
                      CWGETS-SISTEMA-RELATORIOS  "^"
                      CWGETS-EMPRESA-RELATORIOS "^^"
                      "Para visualizar e imprimir o anexo, pode "
                      "ser usado o Adobe Reader em: "
                  "http://www.brasil.adobe.com/products/acrobat"
                  "/readermain.html"
                      DELIMITED BY SIZE
                      INTO EMAIL-TEXT
                ELSE
                     STRING
                     "Em anexo relat¢rio gerado por sistema^"
                      CWGETS-SISTEMA-RELATORIOS  "^"
                      CWGETS-EMPRESA-RELATORIOS "^^"
                      DELIMITED BY SIZE
                      INTO EMAIL-TEXT
                END-IF
                END-IF
                IF  CWDIRS-TITULO = SPACES
                    MOVE "Relat¢rio em anexo" TO CWDIRS-TITULO
                END-IF
                EXEC COBOLware Mail
                     TO E-MAIL
                     REPORT LB-PRINTS
                     Subject CWDIRS-TITULO
                        Text EMAIL-TEXT
                END-EXEC
                MOVE SPACES TO E-MAIL
           END-IF

           SET IMPRIMIU TO TRUE.

       260-99-FIM. EXIT.

       261-LABEL-SPOOL.

           MOVE SPOOL-WORK TO LB-PRINTS

           MOVE SPACES     TO SPOOL-JOB
           MOVE 0          TO S1 S3 S4

           PERFORM VARYING S4
                      FROM LENGTH LB-PRINTS BY -1
                      UNTIL LB-PRINTS(S4:1) NOT = SPACE
                         OR S4 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S2
                      FROM LENGTH LB-PRNTER BY -1
                      UNTIL LB-PRNTER(S2:1) NOT = SPACE
                         OR S2 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S1
                      FROM 1 BY 1 UNTIL S1 > S4
                   ADD 1 TO S3
                   EVALUATE LB-PRINTS(S1:1)
                       WHEN "$"
                            MOVE LB-PRNTER TO SPOOL-JOB (S3: )
                            IF   CWUNIX-OFF
                                 INSPECT SPOOL-JOB (S3: ) CONVERTING
                                         "/" TO "\"
                            END-IF
                            ADD  S2        TO S3
                            SUBTRACT 1   FROM S3
                       WHEN "#"
                            MOVE 7 TO Y
                            PERFORM VARYING X FROM 1 BY 1
                                              UNTIL X = 8
                                              OR BYTE-VIA (X) NOT = "0"
                                    SUBTRACT 1 FROM Y
                            END-PERFORM
                            MOVE VIAS (X: Y) TO SPOOL-JOB (S3: )
                            ADD  Y           TO S3
                            SUBTRACT 1     FROM S3
                       WHEN "@"
                            MOVE CWDIRS-CODIGO TO SPOOL-JOB (S3: )
                            ADD  6             TO S3
                       WHEN OTHER
                            MOVE LB-PRINTS(S1:1) TO SPOOL-JOB(S3: 1)
                   END-EVALUATE
           END-PERFORM

           IF  SPOOL-REMOTO NOT = SPACES
               IF   CWUNIX-DOS16
               OR   CWUNIX-WIN16
               OR   SPOOL-REMOTO(1:1) = '"'
                    MOVE SPACES TO SPOOL-CMD CWEXEC-COMANDO
                    DISPLAY "OS" UPON ENVIRONMENT-NAME
                    ACCEPT SPOOL-CMD  FROM ENVIRONMENT-VALUE
                    IF  SPOOL-CMD NOT = "Windows_NT"
                        STRING "COPY "      DELIMITED BY SIZE
                               LB-PRNTER    DELIMITED BY SPACE
                               " "          DELIMITED BY SIZE
                               SPOOL-REMOTO(1:D2) DELIMITED BY SIZE
                               INTO CWEXEC-COMANDO
                    ELSE
                        STRING "PRINT /D:"   DELIMITED BY SIZE
                               SPOOL-REMOTO(1:D2) DELIMITED BY SIZE
                               " "          DELIMITED BY SIZE
                               LB-PRNTER    DELIMITED BY SPACE
                               INTO CWEXEC-COMANDO
                    END-IF
                    CALL "CWEXE2" USING PARAMETROS-CWEXEC
               ELSE
                    CALL "CBL_COPY_FILE" USING LB-PRNTER SPOOL-REMOTO
               END-IF
               CALL "CBL_DELETE_FILE" USING LB-PRNTER
               MOVE 0 TO RETURN-CODE
               GO TO 261-99-FIM
           END-IF

           CALL "CBL_READ_SCR_CHATTRS" USING X"0000"
                                             CARACTER-BUFFER2
                                             ATTRIBUTE-BUFFER2
                                             STRING-LENGTH
           IF   CWUNIX-OFF
                INSPECT WINPRINT CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
                IF   POSTER
                     MOVE "D" TO ORIENTACAO
                     PERFORM VARYING X FROM 9 BY 1
                             UNTIL X > LENGTH OF WINPRINT
                                OR WINPRINT (X: 1) NOT = SPACE
                                CONTINUE
                     END-PERFORM
                     IF X < LENGTH OF WINPRINT
                        MOVE WINPRINT (X: 1) TO ORIENTACAO
                     END-IF
                     PERFORM POSTER-CHECK THRU FIM-POSTER-CHECK
                     IF   ORIENTACAO = "S" OR "s"
                          MOVE 10             TO CWBOXS-LINE
                                                 CWBOXS-COLUMN
                          MOVE "Orienta‡Æo:"  TO CWBOXS-TITLE
                          MOVE SPACES         TO CWBOXS-ITENS
                          MOVE "~Paisagem"    TO CWBOXS-TEXT (1)
                          MOVE "~Retrato"     TO CWBOXS-TEXT (2)
                          MOVE 0              TO CWBOXS-OPTION
                          PERFORM UNTIL CWBOXS-OPTION <> 0
                                  CALL "CWBOXS" USING PARAMETROS-CWBOXS
                          END-PERFORM
                          MOVE CWBOXS-OPTION-CHAR TO ORIENTACAO
                     END-IF
                     MOVE SPACES        TO SPOOL-JOB
                     IF   ORIENTACAO = "D"
                          MOVE SPACES TO LB-TEXTO
                          STRING LB-REPKEY DELIMITED BY "."
                                 ".ini"    DELIMITED BY SIZE
                                 INTO LB-TEXTO
                          STRING  COBWARE DELIMITED BY SPACE
                           "\POSTER.EXE " DELIMITED BY SIZE
                                 LB-TEXTO DELIMITED BY SPACE
                                       INTO SPOOL-JOB
                          OPEN OUTPUT TEXTO
                          WRITE TEXTO-REG FROM "[Config]"
                          MOVE "Infile=" TO TEXTO-REG
                          MOVE LB-PRNTER TO TEXTO-REG(8:)
                          WRITE TEXTO-REG
                          IF   WINPRINT (1: 8) = "WINVIEW "
                               WRITE TEXTO-REG
                                FROM "OutFile=VIEW"
                               WRITE TEXTO-REG
                                FROM "PrinterMode=Graphic"
                          ELSE
                               IF   WINPRINT (1: 8) = "WINDOWS:"
                               OR   WINPRINT (1: 8) = "USBTEXT:"
                                    IF   WINDOWS-PRINTER NOT = SPACES
                                         MOVE "OutFile=Printer:"
                                           TO TEXTO-REG
                                         MOVE WINDOWS-PRINTER
                                           TO TEXTO-REG(17:)
                                         perform limpa-outfile
                                    ELSE
                                         MOVE "OutFile=Printer"
                                           TO TEXTO-REG
                                    END-IF
                                    WRITE TEXTO-REG
                                    WRITE TEXTO-REG FROM "Quiet=Yes"
                               ELSE
                                    WRITE TEXTO-REG
                                    FROM "OutFile=PRINTER"
                               END-IF
                          END-IF
                          IF  CWPRINTFONT NOT = SPACES
                              MOVE "Font="     TO TEXTO-REG
                              MOVE CWPRINTFONT TO TEXTO-REG(6:)
                              WRITE TEXTO-REG
                          END-IF
                          MOVE SPACES TO TEXTO-REG
                          STRING "Title="        DELIMITED BY SIZE
                              CWDIRS-CODIGO      DELIMITED BY SPACE
                                       ": "      DELIMITED BY SIZE
                                   CWDIRS-TITULO DELIMITED BY SIZE
                                   INTO TEXTO-REG
                          WRITE TEXTO-REG
                          IF   WINPRINT (1: 8) = "USBTEXT "
                          OR   WINPRINT (1: 8) = "USBTEXT:"
                                WRITE TEXTO-REG
                                 FROM "PrinterMode=Text"
                          END-IF
                          IF POSTER-BUFFER NOT = SPACES
                             IF POSTER-BUFFER (1:2) = X"0D0A"
                                 WRITE TEXTO-REG
                                  FROM POSTER-BUFFER(3:)
                             ELSE
                                 WRITE TEXTO-REG
                                  FROM POSTER-BUFFER
                             END-IF
                             MOVE SPACES TO POSTER-BUFFER
                          END-IF
                          CLOSE TEXTO
                     ELSE
                          MOVE CWDIRS-TITULO TO TITULO-W
                          INSPECT TITULO-W CONVERTING "><" TO "]["
                          STRING  COBWARE DELIMITED BY SPACE
                           "\WINPRINT.EXE " DELIMITED BY SIZE
                                 ORIENTACAO DELIMITED BY SIZE
                                        " " DELIMITED BY SIZE
                                  LB-PRNTER DELIMITED BY SPACE
                                       INTO SPOOL-JOB
                     END-IF
                     MOVE SPOOL-JOB   TO CWEXEC-COMANDO
                     MOVE 5           TO CWEXEC-RETORNO
                     CALL "CWEXE2" USING PARAMETROS-CWEXEC
                ELSE
                     MOVE SPOOL-JOB TO CWEXEC-COMANDO
                     CALL "CWEXE2" USING  PARAMETROS-CWEXEC
                END-IF
           ELSE
                PERFORM VARYING R
                             FROM LENGTH OF SPOOL-JOB
                               BY -1
                               UNTIL SPOOL-JOB (R: 1)
                                     NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF   SPOOL-JOB (R: 1) NOT = X"00"
                     ADD 1 TO R
                     MOVE X"00" TO SPOOL-JOB (R: 1)
                END-IF
                CALL "system" USING SPOOL-JOB
           END-IF
           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                              CARACTER-BUFFER2
                                              ATTRIBUTE-BUFFER2
                                              STRING-LENGTH.

       261-99-FIM. EXIT.

       265-GRAVA-LINHA.

           MOVE SPACES TO PRINTS-WS2
           IF   RK-PRNTER = I
           AND ((ASCII-I NOT = LOW-VALUES)
           OR   EJECT-MODE = "10" OR "11")
                IF  EJECT-MODE = "10" OR "11"
                    WRITE PRINTS-REG FROM X"0C"
                END-IF
                MOVE PRINTS-WS TO PRINTS-WS3
                MOVE SPACES    TO PRINTS-WS
                MOVE 0         TO A3
                PERFORM VARYING A FROM 1 BY 1 UNTIL A > 50
                        IF  ASCII-I (A: 1) NOT = X"00"
                            ADD  1              TO A3
                            MOVE ASCII-I (A: 1) TO PRINTS-WS (A3: 1)
                        END-IF
                END-PERFORM
                IF   A3 NOT = 0
                     PERFORM 266-GRAVA-COLUNAS THRU 266-99-FIM
                END-IF
                MOVE PRINTS-WS3 TO PRINTS-WS
                MOVE 0          TO A3
           END-IF

           PERFORM 335-READ-PRNTE THRU 335-99-FIM
           EVALUATE TRUE
               WHEN CODEPAGE-001
                    INSPECT PRINTS-WS
                            CONVERTING ACENTOS-850 TO ACENTOS-437
               WHEN CODEPAGE-OFF
                    INSPECT PRINTS-WS
                            CONVERTING ACENTOS-850 TO ACENTOS-OFF
               WHEN CODEPAGE-WINDOWS
                    INSPECT PRINTS-WS
                            CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-EVALUATE

           IF (POSTER
           OR  ARROBA > 0)
           AND RK-PRNTER = I
               IF PRINTS-WS (width: ) =  SPACES
                  MOVE X'2E' TO PRINTS-WS (width: )
               END-IF
           END-IF

           MOVE PRINTS-WS TO PRNTER-REG

           IF   PRINTS-WS = SPACES
           AND (RK-PRNTER NOT = C)
                ADD 1 TO SALTO
           ELSE
               PERFORM SALTO TIMES
                       IF  (CWUNIX-OFF AND NOT FORMATO-UNIX)
                       OR   FORMATO-DOS
                            WRITE PRINTS-REG FROM X"0D"
                       END-IF
                       WRITE PRINTS-REG FROM X"0A"
               END-PERFORM
               PERFORM 266-GRAVA-COLUNAS THRU 266-99-FIM
               MOVE    0                   TO SALTO
           END-IF

           IF   RK-PRNTER = C
           AND ((ASCII-F NOT = LOW-VALUES)
           OR  EJECT-MODE = "01" OR "11")
                MOVE PRINTS-WS TO PRINTS-WS3
                MOVE SPACES    TO PRINTS-WS
                MOVE 0         TO A3
                PERFORM VARYING A FROM 1 BY 1 UNTIL A > 50
                        IF  ASCII-F (A: 1) NOT = X"00"
                            ADD  1              TO A3
                            MOVE ASCII-F (A: 1) TO PRINTS-WS (A3: 1)
                        END-IF
                END-PERFORM
                IF   A3 NOT = 0
                     PERFORM 266-GRAVA-COLUNAS THRU 266-99-FIM
                END-IF
                MOVE PRINTS-WS3 TO PRINTS-WS
                MOVE 0          TO A3
                IF  EJECT-MODE = "01" OR "11"
                    WRITE PRINTS-REG FROM X"0C"
                END-IF
           END-IF.

       265-99-FIM. EXIT.

       266-GRAVA-COLUNAS.

           MOVE 0      TO CO
           PERFORM VARYING TAMANHO FROM TR BY -1
                             UNTIL TAMANHO = 0
                                OR PRINTS-WS (TAMANHO: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           MOVE 0 TO AFTER-0
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > TAMANHO
                   IF   PRINTS-WS (A: 1) = "\"
                   AND (NOT POSTER)
                        ADD 1 TO A
                        MOVE PRINTS-WS (A: 1) TO PRINT-X (1: 1)
                        IF   PRINT-X > 96
                        AND  PRINT-X < 113
                             COMPUTE E1 = PRINT-X - 96
                             PERFORM VARYING E2 FROM 1 BY 1
                                        UNTIL E2 > 16
                                IF   ASC (E1 E2) NOT = 0
                                     MOVE ASC (E1 E2) TO PRINT-X
                                     WRITE PRINTS-REG
                                      FROM PRINT-X (1: 1)
                                END-IF
                             END-PERFORM
                        ELSE
                             WRITE PRINTS-REG FROM "\"
                             ADD   1            TO CO
                             MOVE "\"           TO PRINTS-WS2 (CO: 1)
                             SUBTRACT 1 FROM A
                        END-IF
                   ELSE
                        IF PRINTS-WS (A: 1) NOT = X"00"
                         IF PRINTS-WS (A: 1) NOT = X"0C"
                            IF  A = TAMANHO
                            AND PRINTS-WS (A: 1) = X"0D"
                                MOVE 1 TO AFTER-0
                            ELSE
                                ADD  1                TO CO
                                MOVE PRINTS-WS (A: 1)
                                  TO PRINTS-WS2 (CO: 1)
                                IF POSTER
                                AND (PRINTS-WS(A: 1) = X"0F" OR X"12")
                                    CONTINUE
                                ELSE
                                  WRITE PRINTS-REG FROM PRINTS-WS (A: 1)
                                END-IF
                            END-IF
                         ELSE
                            ADD 1 TO PAGINA-LIVRE
                            IF  PAGINA-LIVRE > P-F
                                COMPUTE A = TAMANHO + 1
                            ELSE
                                IF   FIRST-FLAG = 0
                                     WRITE PRINTS-REG FROM X"0C"
                                     IF POSTER
                                        WRITE PRINTS-REG FROM SPACE
                                     END-IF
                                END-IF
                                MOVE 0 TO FIRST-FLAG
                            END-IF
                         END-IF
                        END-IF
                   END-IF
           END-PERFORM

           IF  (PAGINA-LIVRE NOT > P-F)
           AND  A3 = 0
                IF  AFTER-0 = 1
                    WRITE PRINTS-REG FROM X"0D"
                ELSE
                    IF  (CWUNIX-OFF AND NOT FORMATO-UNIX)
                    OR   FORMATO-DOS
                         WRITE PRINTS-REG FROM X"0D"
                    END-IF
                    WRITE PRINTS-REG FROM X"0A"
                END-IF
           END-IF.

       266-99-FIM. EXIT.

       280-LIGADA.

            MOVE "N" TO RESPOSTA

            IF  (PRINTER-NO NOT = 255)
            AND  DOS
                 PERFORM TEST AFTER UNTIL PRINTER-STATUS = 144
                         CALL "PC_TEST_PRINTER" USING PRINTER-NO
                                                      PRINTER-STATUS
                         ON   OVERFLOW
                              MOVE 144 TO PRINTER-STATUS
                         END-CALL
                         IF   PRINTER-STATUS = 200
                         OR   PRINTER-STATUS = 128
                         OR   PRINTER-STATUS = 032
                         OR   PRINTER-STATUS = 008
                         OR   PRINTER-STATUS = 048
                              MOVE "Impressora fora de linha "
                                TO CWSEND-MSG
                              MOVE SPACES TO CWSEND-SCREENS
                              MOVE "  ~Repetir" TO CWSEND-SCREEN (1)
                              MOVE "~Interromper"
                                             TO CWSEND-SCREEN (2)
                              SET CWSEND-TIMEOUT-RETRY TO TRUE
                              MOVE 1 TO CWSEND-OPTION
                              CALL "CWSEND" USING PARAMETROS-CWSEND
                              IF   CWSEND-OPTION = 2
                                   MOVE testa-fim  TO SALVA-RT
                                   MOVE '9'         TO testa-fim
                                   MOVE "+" TO CWDIRS-IMPRESSO
                                   MOVE "S" TO RESPOSTA
                                   IF   SPOOL-DEV = "$"
                                        MOVE "S" TO DELETE-PRINTER
                                   END-IF
                                   EXIT PERFORM
                              ELSE
                                   GO TO 280-LIGADA
                              END-IF
                         END-IF
                 END-PERFORM
            END-IF.

       280-99-FIM. EXIT.

       290-EXIBE-TEXTO.

           PERFORM VARYING T FROM 1 BY 1 UNTIL T > 15
                   COMPUTE ROW-NUMBER = T + 5
                   MOVE 1 TO COLUMN-NUMBER
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                              TEXTO2 (T)
                                              X"004E"
           END-PERFORM

           CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR.

       290-99-FIM. EXIT.

       300-INTERROMPE.

           MOVE SPACE TO CHAR RESPOSTA

           IF   CWMOUS-MODE = 0
           OR  (PEDE-IMPRESSORA NOT = 0)
                IF   LINE-LISTAR = 23
                     CALL "CBL_WRITE_SCR_CHARS" USING X"163C"
                                                      "[esc]-Parar"
                                                      X"000B"
                ELSE
                     CALL "CBL_WRITE_SCR_CHARS" USING X"183C"
                                                      "[esc]-Parar"
                                                      X"000B"
                END-IF
                MOVE CWMOUS-CURSOR-POSITION TO SC
                MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
                MOVE SC TO CWMOUS-CURSOR-POSITION
                MOVE ALL X'01'   TO CWMOUS-LINE (LINE-LISTAR) (61: 11)
                IF   PEDE-IMPRESSORA = 0
                     MOVE 1 TO CWMOUS-MODE
                END-IF
           END-IF

           IF   PEDE-IMPRESSORA NOT = 0
                MOVE 0 TO CWMOUS-MODE
                IF   LINE-LISTAR = 23
                     CALL "CBL_WRITE_SCR_CHARS" USING X"162F"
                                                      "<Enter>"
                                                      X"0007"
                     EVALUATE PEDE-IMPRESSORA
                         WHEN 1 CALL "CBL_SET_CSR_POS" USING X"160E"
                         WHEN 2 CALL "CBL_SET_CSR_POS" USING X"1610"
                         WHEN 3 CALL "CBL_SET_CSR_POS" USING X"1608"
                     END-EVALUATE
                ELSE
                     CALL "CBL_WRITE_SCR_CHARS" USING X"182F"
                                                      "<Enter>"
                                                      X"0007"
                     EVALUATE PEDE-IMPRESSORA
                         WHEN 1 CALL "CBL_SET_CSR_POS" USING X"180E"
                         WHEN 2 CALL "CBL_SET_CSR_POS" USING X"1810"
                         WHEN 3 CALL "CBL_SET_CSR_POS" USING X"1808"
                     END-EVALUATE
                END-IF
                PERFORM VARYING CX FROM 48 BY 1
                                    UNTIL CX > 54
                        MOVE 13 TO CWMOUS-POSIT (LINE-LISTAR CX)
                END-PERFORM.

           CALL "CWMOUS" USING PARAMETROS-CWMOUS

           IF   CWMOUS-KEY = 1
                MOVE 27 TO TECLA
           ELSE
                CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
           END-IF

           IF   PEDE-IMPRESSORA = 0
           AND (KEY-STATUS   = 1
           OR   CWMOUS-KEY = 1)
                IF   CWMOUS-KEY NOT = 1
                     MOVE LINE-LISTAR  TO CURPOS-LIN
                     MOVE 03           TO CURPOS-COL
                     CALL "CWKBDC" USING CURPOS CHAR TECLA
                END-IF
                IF   TECLA = 27
                     MOVE 0           TO TECLA
                     MOVE SPACES      TO CWSEND-SCREENS RESPOSTA
                     MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                     MOVE "~Continuar" TO CWSEND-SCREEN (2)
                     MOVE "Interromper impressÆo ?" TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     IF   CWSEND-OPTION = 1
                          MOVE "+" TO CWDIRS-IMPRESSO
                          MOVE "S" TO RESPOSTA
                          IF   SPOOL-DEV = "$"
                               MOVE "S" TO DELETE-PRINTER
                          END-IF
                     END-IF
                END-IF
           END-IF

           IF   EM-LINHA = "S"
           AND  RESPOSTA = "S" OR "s"
                MOVE CWDIRS-FOLHAS        TO RK-PRNTER
                MOVE    1                 TO SALTO
                PERFORM 265-GRAVA-LINHA THRU 265-99-FIM
           END-IF.

       300-99-FIM. EXIT.

       310-PREPARA-RODAPE.

           MOVE LOW-VALUES TO PARAMETROS-CWMOUS
           PERFORM VARYING M FROM 10 BY 1 UNTIL M > 20
                   COMPUTE MC = M - 9
                   COMPUTE ROW-NUMBER2 = M - 1
                   CALL "CBL_READ_SCR_CHARS" USING SCREEN-POSITION2
                                             TESTE-78
                                             X"004E"
                   IF   TESTE-78 NOT = SPACES
                        PERFORM VARYING C FROM 2 BY 1 UNTIL C > 79
                                MOVE MC TO CWMOUS-POSIT (M C)
                        END-PERFORM
                   END-IF
           END-PERFORM
           MOVE ALL "P"    TO CWMOUS-LINE (23) (03: 10)
           MOVE ALL "D"    TO CWMOUS-LINE (23) (14:  3)
           MOVE ALL "N"    TO CWMOUS-LINE (23) (18:  4)
           MOVE ALL "O"    TO CWMOUS-LINE (23) (23:  5)
           MOVE ALL "V"    TO CWMOUS-LINE (23) (29:  3)
           MOVE ALL X"AC"  TO CWMOUS-LINE (23) (33:  1)
           MOVE ALL X"B4"  TO CWMOUS-LINE (23) (35:  1)
           MOVE ALL "E"    TO CWMOUS-LINE (23) (37:  7)
           MOVE ALL "I"    TO CWMOUS-LINE (23) (45:  7)
           MOVE ALL X"7F"  TO CWMOUS-LINE (23) (53:  3)
           MOVE ALL X"71"  TO CWMOUS-LINE (23) (57:  5)
           MOVE "PRint vias DEl Nota Ordem Ver X X Exporta Importa Esc E
      -      "nter" TO MODO
           MOVE SETA-UP    TO MODO (31: 1)
           MOVE SETA-DOWN  TO MODO (33: 1)
           IF   SIZE-REPKEY NOT < 11
                MOVE "PgUp PgDn" TO MODO (61: )
                MOVE ALL X"AD" TO CWMOUS-LINE (23) (63: 4)
                MOVE ALL X"B5" TO CWMOUS-LINE (23) (68: 4)
           END-IF

           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "230370" MODO
           ELSE
                CALL "CWMSGW" USING "250370" MODO
           END-IF.

       310-99-FIM. EXIT.

       320-STOP-EXIT.

           MOVE 47 TO CX
           CALL "CBL_GET_CSR_POS" USING CURSOR-POSITION
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "234713" "<Enter>-Segue"
           ELSE
                CALL "CWMSGW" USING "254713" "<Enter>-Segue"
           END-IF
           MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
           PERFORM 13 TIMES
                   MOVE 2 TO CWMOUS-POSIT (LINE-RODAPE CX)
                   ADD  1 TO CX
           END-PERFORM
           MOVE 61 TO CX
           IF   LINE-RODAPE = 23
                CALL "CWMSGW" USING "236111" MSG-1
           ELSE
                CALL "CWMSGW" USING "256111" MSG-1
           END-IF
           PERFORM 11 TIMES
                   MOVE 1 TO CWMOUS-POSIT (LINE-RODAPE CX)
                   ADD  1 TO CX
           END-PERFORM
           CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
           CALL "CWMOUS" USING PARAMETROS-CWMOUS.

       320-99-FIM. EXIT.

       330-LABEL-PRNTER.

           MOVE CWDIRS-SPOOL    TO SALVA-LB-PRNTER
           MOVE LB-CWDIRS       TO LB-PRNTER
           MOVE SALVA-LB-PRNTER TO LB-PRNTER (I-SPOOL: ).

       330-99-FIM. EXIT.

       333-OPEN-PRNTE.

           move high-values to file-handle
           call "CBL_OPEN_FILE" using LB-PRNTER
                                      X"01" X"03" X"00"
                                      file-handle

           IF   file-handle = high-values
                IF RETURN-CODE = 14657
                   MOVE '9A' TO FS-PRNTER
                   IF NOT SPOOL-TEST
                      EXEC COBOLware Send
                           Message 'Relat¢rio em uso'
                      END-EXEC
                   END-IF
                ELSE
                   MOVE '30' TO FS-PRNTER
                   IF NOT SPOOL-TEST
                      EXEC COBOLware Send
                           Message 'Relat¢rio inacess¡vel'
                      END-EXEC
                   END-IF
                END-IF
                GO TO 333-99-FIM
           ELSE
                MOVE '00' TO FS-PRNTER
                MOVE 0 TO file-offset
                MOVE 1 TO byte-count
                IF SPOOL-TEST
                   MOVE '@' TO TESTA-FIM
                ELSE
                   MOVE '#' TO TESTA-FIM
                END-IF
                SET SPOOL-UNIX TO TRUE
                IF   CWDIRS-WIDTH NOT NUMERIC
                OR  (CWSPOOLHANDLER NOT = SPACES)
                     DISPLAY LB-PRNTER AT 2303
                END-IF
                if CWDIRS-EXT = "spl"
                   SET SPOOL-DOS TO TRUE
                   compute byte-count = CWDIRS-WIDTH + 5
                else
                   PERFORM UNTIL TESTA-FIM = '@'
                             AND CWDIRS-WIDTH NUMERIC
                           call "CBL_READ_FILE" using file-handle
                                                file-offset
                                                byte-count
                                                X'00'
                                                PRNTER-REG(1:1)
                           IF RETURN-CODE = 0
                              IF PRNTER-REG(1:1) = X"0D"
                                 SET SPOOL-DOS TO TRUE
                              END-IF
                              IF PRNTER-REG(1:1) = X"0A"
                                 EXIT PERFORM
                              END-IF
                           ELSE
                              MOVE 0 TO RETURN-CODE
                              EXIT PERFORM
                           END-IF
                           ADD 1 TO file-offset
                           IF file-offset > 505
                              EXIT PERFORM
                           END-IF
                   END-PERFORM
                   compute byte-count = file-offset + 1
                end-if
                COMPUTE WIDTH = byte-count - 4
                IF SPOOL-DOS
                   SUBTRACT 1 FROM WIDTH
                END-IF
                MOVE 0          TO file-offset
                IF TESTA-FIM = '@'
                   IF CWDIRS-WIDTH NUMERIC
                      MOVE CWDIRS-WIDTH TO WIDTH
                   ELSE
                      MOVE WIDTH TO CWDIRS-WIDTH
                      REWRITE CWDIRS-REG
                   END-IF
                   PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                   SET SPOOL-TEST TO TRUE
                   MOVE SPACE TO TESTA-FIM
                   GO TO 333-99-FIM
                END-IF
           END-IF

           COMPUTE TR     = WIDTH + 3
           IF WIDTH > 80
              COMPUTE LIMITE = WIDTH - 80 + 3
           ELSE
              IF WIDTH = 80
                 MOVE 3 TO LIMITE
              ELSE
                 MOVE 1 TO LIMITE
              END-IF
           END-IF
           COMPUTE P4 = WIDTH + 3
           COMPUTE P3 = WIDTH + 2
           COMPUTE P2 = WIDTH + 1
           COMPUTE P1 = WIDTH.

       333-99-FIM. EXIT.

       334-CLOSE-PRNTE.

           IF file-handle NOT = high-values
              call "CBL_CLOSE_FILE" using file-handle
              move high-values to file-handle
           END-IF.

       334-99-FIM. EXIT.

       335-READ-PRNTE.

           compute file-offset = (rk-prnter * byte-count) - byte-count
           call "CBL_READ_FILE" using      file-handle
                                file-offset
                                byte-count
                                X'00'
                                PRNTER-REG
           MOVE PRNTER-REG TO BIN-REG
           IF RETURN-CODE NOT = 0
              MOVE '10'   TO FS-PRNTER
              MOVE 0      TO RETURN-CODE
           ELSE
              if CWDIRS-EXT = "spl"
              and exportando = 0
                 if byte-count < 254
                    compute cwcode-tamanho = byte-count - 1
                    call "CWCODE" using "D" cwcode-tamanho
                                            prnter-reg (byte-count: 1)
                                       prnter-reg (1: cwcode-tamanho)
                 else
                    move 254 to cwcode-tamanho
                    call "CWCODE" using "D" cwcode-tamanho
                                       prnter-reg (byte-count - 1: 1)
                                       prnter-reg (1: cwcode-tamanho)
                    compute cwcode-tamanho = byte-count - 254 - 2
                    call "CWCODE" using "D" cwcode-tamanho
                                       prnter-reg (byte-count: 1)
                               prnter-reg (255: cwcode-tamanho)
                 end-if
              end-if
              MOVE '00'   TO FS-PRNTER
              MOVE SPACES TO PRNTER-REG (width + 4:)
           END-IF
           move PRNTER-REG TO PRINTS-WS.

       335-99-FIM. EXIT.

       340-SET-PRINTER.

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  GRUPO-ID NUMERIC
               MOVE "GI"     TO CWCONF-REGGI
               MOVE GRUPO-ID TO CWCONF-GU-ID
               SET CWSQLC-START TO TRUE
               SET CWSQLC-NOT-LESS TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF  FS-CWCONF < "10"
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-NEXT TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF  FS-CWCONF > "09"
                   OR (GRUPO-ID NOT = CWCONF-GU-ID)
                   OR ("GI"     NOT = CWCONF-TIPO)
                      MOVE SPACES TO GRUPO-ID
                   END-IF
               END-IF
           END-IF

           MOVE SPACES     TO ESTILO
                              WINPRINT POSTER-C
                              FORMATO
                              CODEPAGE
           MOVE LOW-VALUES TO ASCII-I ASCII-F
           MOVE "03" TO CWCONF-REG03
           SET CWSQLC-START TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "L" TO CWBOXC-FUNCTION
           MOVE 09  TO CWBOXC-LINE
           MOVE 23  TO CWBOXC-COLUMN
           MOVE 00  TO CWBOXC-VERTICAL-LENGTH
           MOVE 24  TO CWBOXC-HORIZONTAL-LENGTH
           MOVE  1  TO CWBOXC-ORDER
           MOVE 22  TO CWBOXC-STRING-1-LENGTH
           MOVE 1   TO CWBOXC-RETURN
           MOVE "Impressoras_dispon¡veis" TO CWBOXC-TITLE
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                           OR CWCONF-TIPO NOT = "03"
              SET CWSQLC-READ TO TRUE
              SET CWSQLC-NEXT TO TRUE
              SET CWSQLC-IGNORE-LOCK TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              IF   FS-CWCONF = "9D"
                   CALL "CWCONF" USING "ISAM"
              END-IF
              IF  GRUPO-ID NUMERIC
                  CALL "CWMENN" USING GRUPO-ID CWCONF-ARQUIVO
              END-IF
              IF   FS-CWCONF < "10"
              AND  CWCONF-TIPO = "03"
              AND (CWCONF-ARQUIVO NOT = SPACES)
              AND (CWCONF-ARQUIVO NOT = "<Spool>")
              AND (CWCONF-ARQUIVO NOT = "<Default>")
                   MOVE 0              TO FLAG-GRUPO
                   MOVE CWCONF-ARQUIVO TO GRUPO-TESTE
                   PERFORM VARYING G FROM 1 BY 1
                           UNTIL G > 8
                           IF   GRUPO-BYTE (G) = "/"
                                SUBTRACT 1 FROM G
                                MOVE CWCONF-ARQUIVO (1: G) TO GRUPO-PRT
                                ADD      1   TO G
                                IF   GRUPO NOT = GRUPO-PRT
                                     MOVE 1 TO FLAG-GRUPO
                                END-IF
                           END-IF
                   END-PERFORM
                   IF   NOT GRUPO-ERRADO
                        MOVE CWCONF-ARQUIVO    TO CWBOXC-STRING-1
                        CALL "CWBOXC"       USING PARAMETROS-CWBOXC
                        IF   CWBOXC-VERTICAL-LENGTH < 10
                             ADD 1 TO CWBOXC-VERTICAL-LENGTH
                        END-IF
                   END-IF
              END-IF
           END-PERFORM

           IF   CWBOXC-VERTICAL-LENGTH = 0
                MOVE "Implemente a tabela de impressoras" TO CWSEND-MSG
                MOVE SPACES         TO CWSEND-SCREENS
                CALL "CWSEND"        USING PARAMETROS-CWSEND
                MOVE SPACES TO CWBOXC-OPTION
                MOVE 0      TO LISTE
           ELSE
                MOVE "S"           TO CWBOXC-FUNCTION
                CALL "CWBOXC"   USING PARAMETROS-CWBOXC
                IF   CWBOXC-OPTION = SPACES
                     MOVE 0 TO LISTE
                ELSE
                     MOVE "03"          TO CWCONF-REG03
                     MOVE CWBOXC-OPTION TO CWCONF-ARQUIVO IMPRESSORA
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     PERFORM 350-CHECK-SPOOL-COMMAND THRU 350-99-FIM
                     MOVE CWCONF-ESTILO      TO ESTILO
                     MOVE CWCONF-FORMATO     TO FORMATO
                     MOVE CWCONF-CODEPAGE    TO CODEPAGE
                     IF   CWCONF-LABEL (1: 2) = "\\"
                          MOVE CWCONF-LABEL  TO SPOOL-REMOTO
                          PERFORM VARYING R2 FROM 1 BY 1
                             UNTIL CWCONF-LABEL (R2: 1) = SPACE
                                   CONTINUE
                          END-PERFORM
                          PERFORM VARYING D2 FROM LENGTH OF CWCONF-LABEL
                                      BY -1
                             UNTIL CWCONF-LABEL (D2: 1) NOT = SPACE
                                   CONTINUE
                          END-PERFORM
                          IF  R2 < D2
                              MOVE '"' TO SPOOL-REMOTO
                              MOVE CWCONF-LABEL TO SPOOL-REMOTO (2: )
                              ADD 2 TO D2
                              MOVE '"' TO SPOOL-REMOTO (D2: )
                          END-IF
                          MOVE "$!"          TO CWCONF-LABEL
                     ELSE
                          MOVE SPACES        TO SPOOL-REMOTO
                     END-IF
                     MOVE CWCONF-LABEL       TO LB-PRINTS SPOOL-DEV
                     MOVE CWCONF-LABEL (2: ) TO SPOOL-WORK
                                                WINPRINT POSTER-C
                     MOVE CWCONF-LABEL      TO LB-PRINTS
                     MOVE CWCONF-EJECT-MODE TO EJECT-MODE
                     IF   CWCONF-ASCII = X"FF"
                          MOVE CWCONF-CADEIA-ASCII-INICIAL TO ASCII-I
                          MOVE CWCONF-CADEIA-ASCII-FINAL   TO ASCII-F
                     END-IF
                END-IF
           END-IF

           MOVE "D"           TO CWBOXC-FUNCTION
           CALL "CWBOXC"   USING PARAMETROS-CWBOXC

           IF   FS-CWCONF < "10"
                MOVE "ES"   TO CWCONF-TIPO
                MOVE ESTILO TO CWCONF-ELEMENTO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF = "23"
                OR   ESTILO = SPACES
                     MOVE ZEROS TO ESTILOS
                ELSE
                     MOVE CWCONF-ESTILOS TO ESTILOS
JR==>                IF  ASC (1 1) = 92
JR==>                AND ASC (1 2) = 97
JR==>                    MOVE  92 TO ASC (13 1)
JR==>                    MOVE 109 TO ASC (13 2)
JR==>                END-IF
                END-IF
                IF  ASC (13 1) = 0
                    MOVE 12 TO ASC (13 1)
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                GO TO 340-SET-PRINTER
           END-IF.

       340-99-FIM. EXIT.

       350-CHECK-SPOOL-COMMAND.

           MOVE CWCONF-LABEL TO E-MAIL
           MOVE 0            TO ARROBA
      *    MOVE 0            TO PONTOMAIL
           PERFORM VARYING E FROM 1 BY 1 UNTIL E > LENGTH OF E-MAIL
                   IF   E-MAIL (E: 1) = "@"
                        MOVE E TO ARROBA
                   END-IF
      *            IF   E-MAIL (E: 1) = "."
      *            AND  ARROBA > 0
      *                 MOVE E TO PONTOMAIL
      *            END-IF
           END-PERFORM
      *    IF  PONTOMAIL > ARROBA
           IF  ARROBA NOT = 0
               MOVE SPACES TO CWCONF-LABEL
               STRING "em" TASK ".txt" DELIMITED BY SIZE
                          INTO CWCONF-LABEL
           ELSE
               MOVE SPACES TO E-MAIL
           END-IF
           MOVE CWCONF-LABEL TO SPOOL-COMMAND
                                LB-PRINTS-TEST
           INSPECT LB-PRINTS-TEST CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   DOS-DEVICE
                INSPECT LB-PRINTS-TEST CONVERTING ".:" TO "  "
                MOVE SPACES TO LB-PRINTS
                STRING LB-PRINTS-TEST DELIMITED BY SPACE
                       ":"            DELIMITED BY SIZE
                       INTO LB-PRINTS
                MOVE LB-PRINTS TO SPOOL-COMMAND
           END-IF
           IF   SPOOL-COMMAND(1:1)  = "("
                MOVE CWCONF-LABEL (2: ) TO SPOOL-COMMAND
                PERFORM VARYING R FROM 1 BY 1
                          UNTIL R > LENGTH SPOOL-COMMAND
                             OR SPOOL-COMMAND(R:1) = ")"
                        CONTINUE
                END-PERFORM
                MOVE R TO R2
                IF R < LENGTH SPOOL-COMMAND
                   ADD 1 TO R
                   PERFORM VARYING R FROM R BY 1
                             UNTIL R > LENGTH SPOOL-COMMAND
                                OR SPOOL-COMMAND(R:1) NOT = " "
                           CONTINUE
                   END-PERFORM
                   IF R < LENGTH SPOOL-COMMAND
                      MOVE SPOOL-COMMAND (R: ) TO CWCONF-LABEL
                      MOVE SPACES       TO SPOOL-COMMAND (R2: )
                      IF   SPOOL-COMMAND NOT = SPACES
                           MOVE SPOOL-COMMAND TO CWEXEC-COMANDO
                           CALL "CWEXE2" USING  PARAMETROS-CWEXEC
                      END-IF
                   END-IF
                END-IF
           END-IF.

       350-99-FIM. EXIT.

       ATTR-70.

           IF   LINE-RODAPE = 23
           CALL "CBL_WRITE_SCR_ATTRS" USING X"1602" SALVA-70 X"0046".

       390-99-FIM. EXIT.

       POSTER-CHECK.

           MOVE SPACES TO POSTER-BUFFER
           MOVE 0      TO PS
           PERFORM VARYING I FROM 9 BY 1 UNTIL I > LENGTH POSTER-C
                   IF POSTER-C (I: 1) = '='
                      MOVE "D" TO ORIENTACAO
                      PERFORM VARYING I FROM I BY -1
                              UNTIL I = 9 OR POSTER-C (I: 1) = SPACE
                              CONTINUE
                      END-PERFORM
                      IF I > 9
                         ADD 1 TO I
                      END-IF
                      PERFORM UNTIL I > LENGTH POSTER-C
                              ADD 1 TO PS
                              IF POSTER-C (I: 1) = ';'
                                 MOVE X'0D' TO POSTER-BUFFER(PS:1)
                                 ADD  1     TO PS
                                 MOVE X'0A' TO POSTER-BUFFER(PS:1)
                              ELSE
                                 MOVE POSTER-C (I: 1)
                                   TO POSTER-BUFFER(PS:1)
                              END-IF
                              ADD 1 TO I
                      END-PERFORM
                   END-IF
           END-PERFORM.

       FIM-POSTER-CHECK.

       limpa-outfile.
           move 0 to igual
           perform varying l from 17 by 1 until igual not = 0
                      or l > length of TEXTO-REG
                      if TEXTO-REG (l:1) = '='
                         move l to igual
                      end-if
           end-perform
           if igual not = 0
              perform varying l from igual by -1 until l < 2
                      or TEXTO-REG (l:1) = space or ":"
                      continue
              end-perform
              if l > 1
                  move spaces to texto-reg(l:)
              end-if
           end-if.

       fim-limpa-outfile. exit.
       END PROGRAM CWMEN9.

