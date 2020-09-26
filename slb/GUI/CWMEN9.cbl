       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN9 INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/07/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manipular relatorios SP2                    *
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
           05 REPKEY-SEQUENCIA         PIC  9(006).
           05 REPKEY-EXT               PIC  X(003).
           05 REPKEY-COMANDO           PIC  X(007).
           05 REPKEY-NOVA              PIC  9(001).
           05 REPKEY-NOTA              PIC  X(020).

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  PRNTER-REG.
           05 PRNTER-TEXTO                PIC X(550) VALUE SPACES.

       01  PRNTER-SAVE                    PIC X(550) VALUE SPACES.
       01  BIN-REG                        PIC X(550) VALUE SPACES.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 cwcode-tamanho   comp-x PIC  9(002) VALUE 0.
           05 PATH-IN             PIC  X(001) VALUE space.
           05 DELIM               PIC  X(001) VALUE '|'.
           05 AUTOV               PIC  9(001) VALUE 0.
           05 CWSPOOLHANDLER      PIC  X(255) VALUE spaces.
           05 erros               PIC  9(006) VALUE 0.
           05 width               PIC  9(003) VALUE 0.
           05 testa-fim           PIC  X(001) VALUE space.
           05                     PIC  X(001) VALUE '0'.
              88 SPOOL-TEST VALUE '1'.
              88 SPOOL-OK   VALUE '2'.
              88 SPOOL-UNIX VALUE '3'.
              88 SPOOL-DOS  VALUE '4'.
           05 file-handle         PIC  X(004) VALUE high-values.
           05 file-offset         pic  x(8) comp-x.
           05 byte-count          pic  x(4) comp-x.
           05 texto-handle        PIC  X(004) VALUE high-values.
           05 texto-offset        pic  x(8) comp-x.
           05 texto-count         pic  x(4) comp-x VALUE 1.
           05 NIVEL               PIC  9(001) VALUE 0.
           05 USERID              PIC  X(010) VALUE SPACES.
           05 SPOOLUSER           PIC  X(003) VALUE SPACES.
           05 igual               PIC  9(003) VALUE ZERO.
           05 L                   PIC  9(003) VALUE 0.
           05 p1                  PIC  9(003) VALUE 0.
           05 p2                  PIC  9(003) VALUE 0.
           05 p3                  PIC  9(003) VALUE 0.
           05 p4                  PIC  9(003) VALUE 0.
           05 SPOOL-COLOR         PIC  X(001) VALUE X"00".
           05 NX                  PIC  9(002) COMP-X.
           05 II                  PIC  9(004) VALUE 0.
           05 NUMERO              PIC  9(018) VALUE 0.
           05 CWNUMERO            PIC  X(018) VALUE SPACES.
           05 CWSPOOL-WIDTH       PIC  9(003) VALUE 0.
           05 CWSPOOL-HEIGHT      PIC  9(003) VALUE 0.
           05 CWSPOOL-FIXED       PIC  X(002) VALUE SPACES.
           05 CWSPOOL-BOLD        PIC  X(002) VALUE SPACES.
           05 CWSPOOL-ITALIC      PIC  X(002) VALUE SPACES.
           05 CWSPOOL-STRIKE-OUT  PIC  X(002) VALUE SPACES.
           05 CWSPOOL-UNDERLINE   PIC  X(002) VALUE SPACES.
           05 SPOOLVIEW           PIC  X(050) VALUE SPACES.
           05 SPOOLVIEW-LEN       PIC  9(002) VALUE 0.
           05 DOLAR-VIEW          PIC  9(001) VALUE 0.
           05 CWPURGEVIEW         PIC  X(003) VALUE 'OFF'.
           05 CWPURGEPRINT        PIC  X(003) VALUE 'OFF'.
           05 POSTER-BUFFER.
              10 POSTER-LINE      PIC  X(050) OCCURS 3.
           05 PS                  PIC  9(002) VALUE 0.
           05 volta-pd            PIC  9(001) VALUE 0.
           05 CWPRINTFONT         PIC  X(050) VALUE SPACES.
           05 CWLINES             PIC  X(002) VALUE SPACES.
           05 GRUPO-ID            PIC  X(005) VALUE SPACES.
           05 X91-SRCOUT-RESULT   PIC  9(002) COMP-X.
           05 X91-SRCOUT-FUNCTION PIC  9(002) COMP-X VALUE 47.
           05 SCREEN-POSITION.
              10 ROW-NUMBER       PIC 9(0002) COMP-X VALUE 0.
              10 COLUMN-NUMBER    PIC 9(0002) COMP-X VALUE 0.
           05 CHARACTER-BUFFER    PIC X(2000) VALUE SPACES.
           05 REDEFINES CHARACTER-BUFFER.
              10 RP               PIC  X(080) OCCURS 25.
           05 STRING-LENGTH       PIC 9(0004) COMP-X VALUE 2000.
           05 WAIT-SW             PIC  X(001) VALUE SPACES.
           05 CD-KEY       COMP-5 PIC S9(004) VALUE 0.
           05 BT-KEY       COMP-5 PIC S9(004) VALUE 0.
           05 RESOLUTION          PIC  X(001) VALUE SPACES.
           05 NEW                 PIC  X(050) VALUE SPACES.
           05 EXTW                PIC  X(004) VALUE SPACES.
           05 CWLITS              PIC  X(003) VALUE SPACES.
           05 CWACCENT            PIC  X(003) VALUE SPACES.
           05 AFTER-0             PIC  9(001) VALUE 0.
           05 MESTRE              PIC  X(008) VALUE SPACES.
           05 CORRENTE            PIC  X(008) VALUE SPACES.
           05 EMAIL-TEXT          PIC  X(255) VALUE SPACES.
           05 TITULO-TT OCCURS 3  PIC  X(255) VALUE SPACES.
           05 TT                  PIC  9(002) VALUE 0.
           05 J                   PIC  9(002) VALUE 0.
           05 F                   PIC  9(005) VALUE 0.
           05 POS                 PIC S9(005) VALUE 0 COMP-5.
           05 SAVE-ID             PIC S9(005) VALUE 0 COMP-5.
           05 FIRST-ID            PIC S9(005) VALUE 0 COMP-5.
           05 ARROBA              PIC  9(002) VALUE 0.
      *    05 PONTOMAIL           PIC  9(002) VALUE 0.
           05 E-MAIL              PIC  X(050) VALUE SPACES.
           05 X10                 PIC  9(002) VALUE 22.
           05 X11                 PIC  9(002) VALUE 23.
           05 NEW-OUTPUT          PIC  X(001) VALUE SPACE.
           05 NEW-CUR-COLR        PIC  X(001) VALUE SPACE.
           05 OPT-C               PIC  X(001) VALUE SPACE.
           05 PERC-PLUS           PIC  9(005) VALUE 0.
           05 PERC-MINS           PIC  9(005) VALUE 0.
           05 PERC-OPT            PIC  X(001) VALUE SPACE.
           05 PERC-A              PIC  9(005) VALUE 0.
           05 PERC                PIC  9(005) VALUE 0.
           05 PERC2               PIC  9(005) VALUE 0.
           05 PERC2-PLUS          PIC  9(005) VALUE 0.
           05 PERC2-MINS          PIC  9(005) VALUE 0.
           05 PERC-TIT            PIC  X(100) VALUE SPACES.
           05 PERC-OBS            PIC  X(100) VALUE SPACES.
           05 CPERC               PIC  9(001) VALUE 1.
           05 CPERC2              PIC  9(001) VALUE 1.
           05 TESTE-VERDE         PIC  X(001) VALUE X"00".
           05 LEN-U               PIC  9(002) VALUE 0.
           05 TECLA               PIC  9(003) VALUE 0.
           05 RECARGA             PIC  9(001) VALUE 0.
           05 SALVA-REP           PIC  9(004) VALUE 0.
           05 BARRH-ID     COMP-5 PIC S9(004) VALUE 0.
           05 BARRH-OFF           PIC  9(005) VALUE 0.
           05 VER-PERC-A          PIC  9(005) VALUE 0.
           05 VER-PERC            PIC  9(005) VALUE 0.
           05 HOR-PERC            PIC  9(005) VALUE 0.
           05 JANSIZE             PIC  9(002) VALUE 23.
           05 LER-NOTAS           PIC  9(001) VALUE 0.
           05 COL-NOTA            PIC  9(002) VALUE 47.
           05 ALTURA              PIC  9(002) VALUE 0.
           05 LARG                PIC  9(002) VALUE 0.
           05 SAVE-FIELDS         PIC  9(002) VALUE 0.
           05 JAN OCCURS 2.
              10 FIELDS           PIC  9(004) VALUE 0.
              10 BARRV-ID  COMP-5 PIC S9(004) VALUE 0.
              10 BARRV-OFF COMP-5 PIC S9(005) VALUE 0.
              10 FD-ID     COMP-5 PIC S9(004) OCCURS 2000 VALUE 0.
              10 SD-ID     COMP-5 PIC S9(004) OCCURS 23   VALUE 0.
           05 LS-ID        COMP-5 PIC S9(004) OCCURS 23   VALUE 0.
           05 CM-ID        COMP-5 PIC S9(004) OCCURS 23.
           05 NT-ID        COMP-5 PIC S9(004) OCCURS 23.
           05 LS                  PIC  9(002) VALUE 0.
           05 COMANDO-ID   COMP-5 PIC S9(004) VALUE 0.
           05 CODIGO-ID    COMP-5 PIC S9(004) VALUE 0.
           05 DATA-ID      COMP-5 PIC S9(004) VALUE 0.
           05 PAGINA-ID    COMP-5 PIC S9(004) VALUE 0.
           05 NOTA-ID      COMP-5 PIC S9(004) VALUE 0.
           05 USUARIO-ID   COMP-5 PIC S9(004) VALUE 0.
           05 CMDS.
              10 FILLER                PIC  X(007) VALUE "PRINT".
              10 FILLER                PIC  X(007) VALUE "VIEW".
              10 FILLER                PIC  X(007) VALUE "DEL".
              10 FILLER                PIC  X(007) VALUE "NOTA".
              10 FILLER                PIC  X(007) VALUE "EXPORTA".
              10 FILLER                PIC  X(007) VALUE "IMPORTA".
              10 FILLER                PIC  X(007) VALUE "SAIR".
           05 REDEFINES CMDS.
              10 CM OCCURS 7           PIC  X(007).
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
              10 SETA-DOWN             PIC  X(001) VALUE "v".
              10 SETA-UP               PIC  X(001) VALUE "^".
           05 SAVE-SPOOL-OPTIONS       PIC  X(004) VALUE "0000".
           05 SPOOL-OPTIONS.
              10 ZEBRADO               PIC  9(001) VALUE 0.
              10 FULL-SCREEN           PIC  9(001) VALUE 0.
              10 COLUNA-SORT           PIC  9(001) VALUE 0.
              10 OPCAO-SORT            PIC  9(001) VALUE 0.
           05 REORDENAR                PIC  9(001) VALUE 0.
           05 EXPORTOU                 PIC  9(001) VALUE 0.
           05 ZEBRA                    PIC  X(001) VALUE ALL X"70".
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 VEZ-NOTA                 PIC  9(001) VALUE 0.
           05 LINE-NOTA                PIC  9(002) VALUE 0.
           05 POS-W                    PIC  X(006) VALUE SPACES.
           05 SAVE-NOTA                PIC  X(020) VALUE SPACES.
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
           05 SPOOL-REMOTO             PIC  X(050) VALUE SPACE.
           05 SPOOL-CMD                PIC  X(050) VALUE SPACE.
           05 SPOOL-WORK               PIC  X(050) VALUE SPACE.
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
           05 IMPRESSORA               PIC  X(030) VALUE SPACES.

           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 REVERTE-VIDEO            PIC  X(001) VALUE SPACE.
           05 VERMELHO                 PIC  X(001) VALUE SPACE.
           05 VERDE                    PIC  X(001) VALUE SPACE.
           05 X0E                      PIC  X(001) VALUE SPACE.
           05 ZEBRA-1                  PIC  X(001) VALUE SPACES.
           05 ZEBRA-2                  PIC  X(001) VALUE X"01".
           05 ZEBRA-3                  PIC  X(001) VALUE SPACES.
           05 ZEBRA-4                  PIC  X(001) VALUE SPACES.
           05 ZEBRA-I                  PIC  X(001) VALUE SPACES.
           05 ZEBRA-P                  PIC  X(001) VALUE SPACES.

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
           05 FIM-PROCURA              PIC  9(001) VALUE ZERO.
           05 SALVA-RT                 PIC  9(001) VALUE 0.
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
               10 OBS-CODIGO           PIC  X(007) VALUE SPACES.
               10 FILLER               PIC  X(001) VALUE SPACE.
               10 OBS-DATA             PIC  99/99/9999.
               10 FILLER               PIC  X(001) VALUE SPACE.
               10 OBS-HH               PIC  9(002).
               10 FILLER               PIC  X(001) VALUE ":".
               10 OBS-MM               PIC  9(002).
               10 FILLER               PIC  X(005) VALUE SPACE.
               10 OBS-VIA              PIC  Z.ZZZ.ZZZ.
               10 FILLER               PIC  X(004) VALUE " de ".
               10 OBS-VIAS             PIC  Z.ZZZ.ZZZ.
           05 REDEFINES OBS.
              10 BYTE-OBS OCCURS 68    PIC  X(001).
           05 OBS-PRINTS               PIC  X(070).
           05 CADEIA2                  PIC  X(062) VALUE ALL "?".
           05 CADEIA                   PIC  X(062) VALUE SPACES.
           05 REDEFINES CADEIA.
              10 BYTE-S OCCURS 62      PIC  9(002) COMP-X.
           05 IGUAIS                   PIC  9(006) VALUE ZERO.
           05 TC                       PIC  9(006) VALUE ZERO.
           05 VEZ                      PIC  9(001) VALUE ZERO.
           05 LISTE                    PIC  9(001) VALUE ZERO.
           05 VEZES                    PIC  9(010) VALUE ZERO.
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
              88 CMD-MODOS VALUE "F" "f" "Z" "z" "I" "i".
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
           05 CI                  COMP PIC  9(004) VALUE ZERO.
           05 COLUNA              COMP PIC  9(004) VALUE 1.
           05 CO                  COMP PIC  9(004) VALUE ZERO.
           05 CO2                 COMP PIC  9(004) VALUE ZERO.
           05 RK-PRNTER                PIC  9(010) VALUE ZERO.
           05 SIZE-REPKEY         COMP PIC  9(004) VALUE ZERO.
           05 PX                  COMP PIC  9(010) VALUE ZERO.
           05 FAIXA                    PIC  9(004) VALUE ZERO.
           05 FAIXA-P                  PIC  9(004) VALUE ZERO.
           05 P                   COMP PIC  9(010) VALUE ZERO.
           05 PA                  COMP PIC  9(010) VALUE ZERO.
           05 W                   COMP PIC  9(010) VALUE ZERO.
           05 Y                   COMP PIC  9(006) VALUE ZERO.
           05 I                   COMP PIC  9(006) VALUE ZERO.
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
           05 PONTEIRO-A          COMP PIC  9(010) VALUE 1.
           05 PONTEIRO-B          COMP PIC  9(010) VALUE 1.
           05 SALVA-PONTEIRO      COMP PIC  9(010) VALUE 1.
           05 SALVA-PONTEIRO-2    COMP PIC  9(010) VALUE 1.
           05 PRONTO-A            COMP PIC  9(006) VALUE 1.
           05 PRONTO-S            COMP PIC  9(006) VALUE 1.
           05 TEXTO-TELA                           VALUE SPACES.
              10 OCCURS 24.
                 11 TEXTO2              PIC  X(080).
                 11 SAVE-ZEBRA          PIC  X(001).
           05 SAVE-TEXTO-TELA                      VALUE LOW-VALUES.
              10 SAVE-TEXTO2 OCCURS 24 PIC  X(080).
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 CWMENU                   PIC  X(001) VALUE "?".
           05 ER-REPKEY.
              10 FS-REPKEY             PIC  X(002) VALUE "00".
              10 LB-REPKEY             PIC  X(255) VALUE SPACES.
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
           05 BARRA-DIR PIC X VALUE '\'.
           05 TEMP PIC X(50) VALUE SPACES.
           05 SEM-PATH PIC X(50) VALUE SPACES.
           05 MP-I PIC 99 VALUE 0.
           05 MP-Y PIC 99 VALUE 0.
           05 PATH       PIC X(255).
           05 PATH-MD    PIC X(255) VALUE SPACES.

       01  OFF-SETS OCCURS 2.
           05 SIZE-FIELDS      PIC  9(004) VALUE 0.
           05 POSIT            PIC  9(001) VALUE 0.
           05 OFF-W            PIC  9(004) VALUE 0.
           05 FIELD-AREA       PIC X(2000).

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
           05 RELATORIO OCCURS 23.
              10 FILLER                         PIC  X(001).
              10 COMANDO                        PIC  X(007).
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
              10 USUARIO                        PIC  X(010).
              10 FILLER                         PIC  X(001).
              10 CHAVE-REPKEY                   PIC  9(004).
              10 SEQUENCIA BLANK ZERO           PIC  9(006).
              10 EXT                            PIC  X(003).
              10 TITULO-T                       PIC  X(080).
              10 N-OFF                          PIC  9(004).
              10 C-OFF                          PIC  9(004).
              10 CW                             PIC  X(002).

       01  SP2-TELA VALUE HIGH-VALUES.
           05 OCCURS 23.
              10 FD-OUTPUT                      PIC  X(001).
              10 FD-CUR-COLR                    PIC  X(001).
              10 FD-PROG-OFF             COMP-5 PIC S9(004).

       COPY CWBOXC.
       COPY CWBOXS.
       COPY CWSEND.
       COPY CWTIME.
       COPY CWPATH.
       COPY CWBOXW.
       COPY CWEXEC.
       COPY CWSPWS.
       COPY CWGETS.
       COPY CWCONF.
       COPY CWFONT.

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
           DISPLAY "CWSPOOLHANDLER" UPON ENVIRONMENT-NAME
           ACCEPT SPOOL-JOB FROM ENVIRONMENT-VALUE
           IF  SPOOL-JOB NOT = SPACES
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
           DISPLAY "CWLINES"     UPON ENVIRONMENT-NAME
           ACCEPT CWLINES        FROM ENVIRONMENT-VALUE
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER NOT = 0
                IF   spool-NUMERO NUMERIC
                AND  spool-NUMERO > 0
                     DISPLAY "CWPURGEVIEW" UPON ENVIRONMENT-NAME
                     ACCEPT   CWPURGEVIEW  FROM ENVIRONMENT-VALUE
                     INSPECT  CWPURGEVIEW  CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                ELSE
                     MOVE 0 TO SPOOL-NUMERO
                END-IF
           ELSE
                MOVE 0 TO SPOOL-NUMERO
           END-IF
           DISPLAY "CWPURGEPRINT" UPON ENVIRONMENT-NAME
           ACCEPT   CWPURGEPRINT  FROM ENVIRONMENT-VALUE
           INSPECT  CWPURGEPRINT  CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
           COPY CWSPPD.
           DISPLAY "CWRESOLUTION" UPON ENVIRONMENT-NAME
           ACCEPT RESOLUTION FROM ENVIRONMENT-VALUE
           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE LOW-VALUES TO SP2-SD-DATA
           MOVE "CWMENU" TO MESTRE
           DISPLAY "CWSPLTXT" UPON ENVIRONMENT-NAME
           ACCEPT CWSPLTXT    FROM ENVIRONMENT-VALUE
           IF   CWSPLTXT = SPACES OR LOW-VALUES
                MOVE "txt" TO CWSPLTXT
           END-IF
           DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
           ACCEPT COBWARE FROM ENVIRONMENT-VALUE
           CALL "CWMOLD"         USING TABELA-CORES TABELA-MOLDURA
           MOVE COR (015)           TO X0E
           MOVE COR (079)           TO VERMELHO
           MOVE COR (047)           TO VERDE
           MOVE COR (49)            TO ZEBRA-1
           MOVE COR (31)            TO ZEBRA-3
           MOVE VERMELHO            TO ZEBRA-4
           DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           IF   TMP = SPACES
                DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                ACCEPT  TMP       FROM ENVIRONMENT-VALUE
           END-IF

           CALL "CWGETS" USING PARAMETROS-CWGETS
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
           MOVE 1            TO I I-SPOOL

           IF   CWCONF-PATH-SPOOL = SPACES
                MOVE "SPOOL" TO  CWCONF-PATH-SPOOL
           ELSE
                IF   CWCONF-PATH-SPOOL (1: 1) = "\"
                AND  CWSPOOLHANDLER = SPACES
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
                IF CWCONF-PATH-SPOOL (2:1) = ':'
                   NEXT SENTENCE
                END-IF
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            RETURN-STATUS
                CALL "CBL_READ_DIR"  USING OLD-DIRECTORY
                                           SIZE-OLD-DIR
                MOVE SPACES TO SPOOL-JOB
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
                MOVE SPOOL-JOB TO CWCONF-PATH-SPOOL
                MOVE SPACES TO SPOOL-JOB
           END-IF.

           MOVE CWCONF-PATH-SPOOL TO LB-CWDIRS PATH-SPOOL
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL LB-CWDIRS(I:1) = SPACES
                      OR I > LENGTH LB-CWDIRS
                   IF   LB-CWDIRS(I:1) = "\" OR "/"
                        MOVE BARRA-DIR TO LB-CWDIRS(I:1)
                   END-IF
           END-PERFORM
           SUBTRACT 1 FROM I
           IF   LB-CWDIRS(I:1) = "/" OR "\"
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
                STRING TMP    DELIMITED BY SPACE
                       BARRA-DIR DELIMITED BY SIZE
                       TMP-LB DELIMITED BY SPACE
                  INTO LB-REPKEY
           ELSE
                MOVE TMP-LB TO LB-REPKEY
           END-IF
           MOVE "CWDIRS"     TO LB-CWDIRS     (I: )
           MOVE LB-CWDIRS    TO OLD-CWDIRS.
       pd.
           OPEN INPUT CWDIRS
  2747     IF   FS-CWDIRS = X"3909"
  2748          CALL "CBL_CREATE_DIR" USING PATH-SPOOL
  2749     ELSE
  2750          IF   FS-CWDIRS (1: 1) = "9"
  2751               CALL "CWISAM" USING ER-CWDIRS
  2752          END-IF
  2753     END-IF

           READ CWDIRS NEXT RECORD IGNORE LOCK
           IF   FS-CWDIRS > "09"
                CLOSE CWDIRS
                MOVE SPACES                       TO CWSEND-SCREENS
                MOVE "Sem relat¢rios dispon¡veis" TO CWSEND-MSG
                MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                MOVE "~Importar"  TO CWSEND-SCREEN (2)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 2
                     OPEN OUTPUT CWDIRS
                     CLOSE CWDIRS
                     OPEN I-O CWDIRS
                     PERFORM 113-IMPORTA
      *                 THRU 113-99-FIM
                     CLOSE CWDIRS
                     go to pd
                else
                     CLOSE CWDIRS
                     GOBACK
      *         END-IF
                END-IF
      *         IF   RK-PRNTER = 0
      *              CLOSE CWDIRS
      *              GOBACK
      *         END-IF
           ELSE
                CLOSE CWDIRS
                OPEN I-O CWDIRS
           END-IF

           DISPLAY "CWSPOOL-COLOR"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           MOVE 1 TO I
           IF  NUMERO NOT = 0
               MOVE NUMERO TO NX
               MOVE NX (1: 1) TO SPOOL-COLOR
           END-IF
           move spaces to CWFONT-NAME
           DISPLAY "CWSPOOL-FONT" UPON ENVIRONMENT-NAME
           ACCEPT   CWFONT-NAME  FROM ENVIRONMENT-VALUE
           IF   CWFONT-NAME NOT = SPACES
                DISPLAY "CWSPOOL-WIDTH"   UPON ENVIRONMENT-NAME
                PERFORM AJUSTA MOVE NUMERO TO CWSPOOL-WIDTH
                DISPLAY "CWSPOOL-HEIGHT "   UPON ENVIRONMENT-NAME
                PERFORM AJUSTA MOVE NUMERO TO CWSPOOL-HEIGHT
                DISPLAY "CWSPOOL-FIXED " UPON ENVIRONMENT-NAME
                ACCEPT  CWSPOOL-FIXED    FROM ENVIRONMENT-VALUE
                INSPECT CWSPOOL-FIXED
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWSPOOL-BOLD " UPON ENVIRONMENT-NAME
                ACCEPT  CWSPOOL-BOLD    FROM ENVIRONMENT-VALUE
                INSPECT CWSPOOL-BOLD CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWSPOOL-ITALIC " UPON ENVIRONMENT-NAME
                ACCEPT  CWSPOOL-ITALIC    FROM ENVIRONMENT-VALUE
                INSPECT CWSPOOL-ITALIC
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWSPOOL-STRIKE-OUT " UPON ENVIRONMENT-NAME
                ACCEPT  CWSPOOL-STRIKE-OUT    FROM ENVIRONMENT-VALUE
                INSPECT CWSPOOL-STRIKE-OUT
                         CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWSPOOL-UNDERLINE " UPON ENVIRONMENT-NAME
                ACCEPT  CWSPOOL-UNDERLINE    FROM ENVIRONMENT-VALUE
                INSPECT CWSPOOL-UNDERLINE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                IF CWSPOOL-WIDTH      NOT = 0
                   MOVE CWSPOOL-WIDTH TO CWFONT-WIDTH
                END-IF
                IF CWSPOOL-HEIGHT     NOT = 0
                   MOVE CWSPOOL-HEIGHT TO CWFONT-HEIGHT
                END-IF
                IF CWSPOOL-FIXED      = "ON"
                   SET  CWFONT-FIXED TO TRUE
                END-IF
                IF CWSPOOL-BOLD       = "ON"
                   SET  CWFONT-BOLD TO TRUE
                END-IF
                IF CWSPOOL-ITALIC     = "ON"
                   SET  CWFONT-ITALIC TO TRUE
                END-IF
                IF CWSPOOL-STRIKE-OUT = "ON"
                   SET  CWFONT-STRIKE-OUT TO TRUE
                END-IF
                IF CWSPOOL-UNDERLINE  = "ON"
                   SET  CWFONT-UNDERLINE TO TRUE
                END-IF
                INITIALIZE CWFONT-REFERENCE
                CALL "CWFONT" USING PARAMETROS-CWFONT
           ELSE
                MOVE 4 TO CWFONT-REFERENCE
           END-IF
           IF CWSPOOLHANDLER NOT = SPACES
              IF LB-TEXTO NOT = SPACES
                 PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
                 EXEC COBOLware Save File LB-TEXTO CLOSE END-EXEC
              END-IF
              CLOSE CWDIRS
              set CWEXEC-NOWARNING  to true
              set CWEXEC-ASSYNCRONE to true
              MOVE CWSPOOLHANDLER TO CWEXEC-COMMAND
              CALL "CWEXE2" USING  PARAMETROS-CWEXEC
              GOBACK
           END-IF
           IF   SPOOL-NUMERO = 0
                MOVE "CWMEN9" TO CORRENTE
                PERFORM 500-OPEN-WINDOW THRU 500-99-FIM
                PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
                PERFORM 400-SP2-TOPO      THRU 400-99-FIM
           END-IF
           CLOSE CWDIRS
           OPEN I-O CWDIRS
           IF   SPOOL-NUMERO NOT = 0
                MOVE 1         TO J AUTOV
      *         MOVE SPOOL-NUMERO     TO CWDIRS-NUMERO
                MOVE LK-SPOOL         TO CWDIRS-SPOOL
                READ CWDIRS
                PERFORM 200-VISUALIZA THRU 200-99-FIM
                CLOSE CWDIRS
                GOBACK
           END-IF
           MOVE 1 TO PONTEIRO
           MOVE 0 TO SP2-CD-KEY PONTEIRO-A
           PERFORM UNTIL SP2-CD-KEY = SP2-KEY-ESC or volta-pd = 1
             PERFORM TEST AFTER UNTIL COMANDO-OK
                                   OR SP2-CD-KEY = SP2-KEY-ESC
                IF (SP2-CD-KEY = SP2-KEY-UP
                             OR SP2-KEY-DOWN)
                AND PONTEIRO = PONTEIRO-A
                    CONTINUE
                ELSE
                    PERFORM 110-EXIBE THRU 110-99-FIM
                END-IF
                MOVE PONTEIRO TO PONTEIRO-A
                MOVE 0 TO SP2-CD-KEY
                EVALUATE LER-NOTAS
                     WHEN 0 MOVE LS-ID (PRONTO) TO SP2-CD-NEXT-FLD-ID
                     WHEN 1 MOVE NT-ID (PRONTO) TO SP2-CD-NEXT-FLD-ID
                     WHEN 2 MOVE CM-ID (PRONTO) TO SP2-CD-NEXT-FLD-ID
                END-EVALUATE
                PERFORM TEST AFTER UNTIL SP2-CD-KEY
                                       = SP2-KEY-ENTER
                                      OR SP2-KEY-ESC
                                      OR SP2-KEY-UP
                                      OR SP2-KEY-DOWN
                                      OR SP2-KEY-PGDN
                                      OR SP2-KEY-PGUP
                                      OR SP2-KEY-CTRL-PGUP
                                      OR SP2-KEY-CTRL-PGDN
                                      OR SP2-KEY-SELECT
                   MOVE CORRENTE TO SP2-CD-NEXT-PANEL
                   IF  BARRV-ID (J) NOT = 0
                       IF   CPERC = 1
                            COMPUTE PERC = PONTEIRO / SIZE-REPKEY * 100
                            MOVE BARRV-OFF(J) TO POS
                            MOVE PERC TO FIELD-AREA(J) (POS: 5)
                       ELSE
                            MOVE 1 TO CPERC
                       END-IF
                   END-IF
                   CALL SP2   USING SP2-SET-PANEL-FIELDS FIELD-AREA(J)
                   CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
avi                IF   SPOOL-NUMERO NUMERIC
                   AND  SPOOL-NUMERO > 0
                        MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                        MOVE SPACES TO COMANDO(PRONTO)
                        MOVE 0      TO LER-NOTAS
                   ELSE
      *                 CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                   CALL SP2   USING SP2-CONVERSE-PANEL SP2-CONVERSE-DATA
                        PERFORM 800-EVITA-BUG-SP2 THRU 800-99-FIM
                        IF   SP2-CD-KEY = SP2-KEY-CTRL-P
                             CALL "CBL_WRITE_SCR_CHARS"
                                   USING SCREEN-POSITION
                                         CHARACTER-BUFFER
                                         STRING-LENGTH
                             CALL "CWPRTS"
                             CANCEL "CWPRTS"
                             EXIT PERFORM CYCLE
                        END-IF
                   END-IF
      *            IF   SP2-CD-KEY = SP2-KEY-CLOSE
      *                 MOVE SP2-KEY-ESC TO SP2-CD-KEY
      *            END-IF
                   IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
                   OR   SP2-KEY-APP-CLOSE
                   or   SP2-KEY-CLOSE
                        PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
                        STOP RUN
                   END-IF
pop                IF   SP2-CD-KEY = SP2-KEY-SELECT
                   AND  (BARRV-ID (J) NOT = 0)
                        MOVE PERC         TO PERC-A
                        MOVE BARRV-OFF(J) TO POS
                        MOVE FIELD-AREA(J) (POS: 5) TO PERC
                        EVALUATE TRUE
                            WHEN (PERC = PERC-A + 1)
                                 MOVE SP2-KEY-DOWN TO SP2-CD-KEY
                            WHEN (PERC = PERC-A - 1)
                                 MOVE SP2-KEY-UP TO SP2-CD-KEY
                            WHEN PERC < 2
                                 MOVE SP2-KEY-CTRL-PGDN TO SP2-CD-KEY
                            WHEN PERC > 98
                                 MOVE SP2-KEY-CTRL-PGUP TO SP2-CD-KEY
                            WHEN PERC > PERC-A
                                 MOVE SP2-KEY-PGDN TO SP2-CD-KEY
                            WHEN PERC < PERC-A
                                 MOVE SP2-KEY-PGUP TO SP2-CD-KEY
                        END-EVALUATE
                   END-IF
                   IF  SP2-CD-KEY NOT = SP2-KEY-ESC
                       PERFORM 111-CHECK-POINT THRU 111-99-FIM
                       EVALUATE TRUE
                           WHEN SP2-CD-KEY = SP2-KEY-MENU
                                MOVE CM (SP2-CD-MENU-ID)
                                  TO COMANDO(PRONTO)
                                IF LER-NOTAS = 0
                                   MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                                END-IF
                                IF SP2-CD-MENU-ID = 7
                                   MOVE SP2-KEY-ESC TO SP2-CD-KEY
                                   EXIT PERFORM
                                END-IF
                           WHEN SP2-CD-KEY = SP2-KEY-ENTER
                            AND (COMANDO(PRONTO) = SPACES OR "*")
                            AND LER-NOTAS = 0
                                MOVE "Ver"  TO COMANDO(PRONTO)
                           WHEN SP2-CD-KEY = SP2-KEY-F2
                            AND SP2-CD-BUTTON-ID = COMANDO-ID
                            AND (LER-NOTAS NOT = 2)
                                MOVE 2 TO LER-NOTAS
                                PERFORM 430-ENABLE-NOTAS THRU 430-99-FIM
                           WHEN SP2-CD-KEY = SP2-KEY-F2
                            AND SP2-CD-BUTTON-ID NOT = 0
                                PERFORM 112-ORDEM THRU 112-99-FIM
                                PERFORM 110-EXIBE THRU 110-99-FIM
                       END-EVALUATE
                       IF   COMANDO(PRONTO) = "NOTA"
                            IF  LER-NOTAS = 0
                                MOVE 1 TO LER-NOTAS
                                PERFORM 430-ENABLE-NOTAS THRU 430-99-FIM
                            END-IF
                            MOVE 0      TO SP2-CD-KEY
                            MOVE SPACES TO COMANDO (PRONTO)
                       ELSE
                            IF  LER-NOTAS NOT = 0
                                PERFORM 440-SAVE-NOTA THRU 440-99-FIM
                                   VARYING P FROM 1
                                             BY 1 UNTIL P > ALTURA
                                IF  SP2-CD-KEY = SP2-KEY-ENTER
                                    PERFORM 431-DISABLE-NOTAS
                                       THRU 431-99-FIM
                                    MOVE 0 TO LER-NOTAS
                                ELSE
                                    IF SP2-CD-KEY = SP2-KEY-MENU
                                       MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                                    END-IF
                                END-IF
                            END-IF
                       END-IF
                   ELSE
                       PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
                   END-IF
                END-PERFORM
                MOVE PRONTO TO W
                PERFORM 450-CRITICA-COMANDO THRU 450-99-FIM
             END-PERFORM

             MOVE CW (PRONTO)        TO CWDIRS-CW
             MOVE SEQUENCIA (PRONTO) TO CWDIRS-NUMERO
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
      *      IF   FIELD-AREA (BARRV-OFF: 5) NOT = VER-PERC
      *           MOVE VER-PERC                TO VER-PERC-A
      *           MOVE FIELD-AREA (BARRV-OFF: 5) TO VER-PERC
      *           EVALUATE TRUE
      *                WHEN SP2-CD-KEY = SP2-KEY-DOWN
      *                               OR SP2-KEY-UP
      *                               OR SP2-KEY-PGDN
      *                               OR SP2-KEY-PGUP
      *                               OR SP2-KEY-CTRL-PGUP
      *                               OR SP2-KEY-CTRL-PGDN
      *                     CONTINUE
      *                WHEN VER-PERC = (VER-PERC-A + 1)
      *                     MOVE SP2-KEY-DOWN TO SP2-CD-KEY
      *           END-EVALUATE
      *      END-IF
             EVALUATE TRUE
                 WHEN SP2-CD-KEY = SP2-KEY-ENTER
                      IF   COMANDO(PRONTO) = "IMPORTA"
                           PERFORM 113-IMPORTA THRU 113-99-FIM
Nena                       MOVE SPACES TO COMANDO(PRONTO)
                           move 1 to volta-pd
                           exit perform
                      ELSE
                           PERFORM 120-EXEC THRU 120-99-FIM
                           IF   SPOOL-NUMERO NUMERIC
                           AND  SPOOL-NUMERO > 0
                                MOVE SP2-KEY-ESC TO SP2-CD-KEY
                           END-IF
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-DOWN
                 AND  PRONTO < X11
                 AND (PONTEIRO - 1 + PRONTO) < SIZE-REPKEY
                     MOVE PRONTO TO PRONTO-A
                     ADD  1      TO PRONTO
                     PERFORM VARYING PRONTO FROM PRONTO BY 1
                             UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                                OR PRONTO = X11
                                   CONTINUE
                     END-PERFORM
                     IF   PRONTO = X11
                     AND  SEQUENCIA (X11) = SPACES
                          MOVE PRONTO-A TO PRONTO
                          IF   PONTEIRO < SIZE-REPKEY
                               ADD 1 TO PONTEIRO
                          END-IF
                     END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-UP
                 AND  PRONTO > 1
                 AND  PONTEIRO = 1
                      MOVE PRONTO TO PRONTO-A
                      SUBTRACT 1 FROM PRONTO
                      PERFORM VARYING PRONTO
                              FROM PRONTO BY -1
                              UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                                 OR PRONTO = 1
                                 CONTINUE
                      END-PERFORM
                      IF  SEQUENCIA (PRONTO) = SPACES
                          MOVE PRONTO-A TO PRONTO
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-DOWN
                 AND (PONTEIRO + X10) < SIZE-REPKEY
                 AND  PRONTO   < SIZE-REPKEY
                      IF   PRONTO = X11
                           ADD 1 TO PONTEIRO
                      ELSE
                           MOVE PRONTO TO PRONTO-A
                           ADD  1      TO PRONTO
                           PERFORM VARYING PRONTO FROM PRONTO BY 1
                                   UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                                      OR PRONTO = X11
                           END-PERFORM
                           IF   PRONTO = X11
                           AND  SEQUENCIA (X11) = SPACES
                                MOVE PRONTO-A TO PRONTO
                                IF   PONTEIRO < SIZE-REPKEY
                                     ADD 1 TO PONTEIRO
                                END-IF
                           END-IF
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-UP
                      IF   PONTEIRO > 1
                      OR   PRONTO   > 1
                           IF   PRONTO = 1
                                SUBTRACT 1 FROM PONTEIRO
                           ELSE
                                MOVE PRONTO TO PRONTO-A
                                SUBTRACT 1 FROM PRONTO
                                PERFORM VARYING PRONTO
                                        FROM PRONTO BY -1
                                     UNTIL SEQUENCIA (PRONTO) NOT = " "
                                           OR PRONTO = 1
                                END-PERFORM
                                IF   PRONTO = 1
                                AND  SEQUENCIA (1) = SPACES
                                     MOVE PRONTO-A TO PRONTO
                                     IF   PONTEIRO > 1
                                          SUBTRACT 1 FROM PONTEIRO
                                     END-IF
                                END-IF
                           END-IF
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-PGDN
                      IF   (PONTEIRO + X10) < SIZE-REPKEY
                           ADD     X10         TO PONTEIRO
                           MOVE    99          TO PRONTO
                      ELSE
                           IF   PONTEIRO NOT = (SIZE-REPKEY - X10)
                                COMPUTE PONTEIRO = SIZE-REPKEY - X10
                                MOVE    99          TO PRONTO
                           END-IF
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-PGUP
                      IF   PONTEIRO > X10
                           SUBTRACT X10        FROM PONTEIRO
                           MOVE     1           TO PRONTO
                      ELSE
                           IF   PONTEIRO NOT = 1
                                MOVE 1 TO PONTEIRO
                                          PRONTO
                           END-IF
                      END-IF
                 WHEN SP2-CD-KEY = SP2-KEY-CTRL-PGUP
                 AND  SIZE-REPKEY > X11
                 AND  PONTEIRO NOT = 1
                      MOVE 1 TO PONTEIRO
                                PRONTO
                 WHEN SP2-CD-KEY = SP2-KEY-CTRL-PGDN
                 AND  SIZE-REPKEY > X11
                 AND  PONTEIRO NOT = (SIZE-REPKEY - X10)
                      COMPUTE PONTEIRO = SIZE-REPKEY - X10
                      MOVE    X11 TO PRONTO
                 WHEN SP2-CD-KEY = SP2-KEY-SELECT
                    MOVE BARRV-OFF(J) TO POS
                    IF  (BARRV-ID (J) NOT = 0)
                    AND (FIELD-AREA(J) (POS: 5) NOT = PERC)
      *                  COMPUTE PERC-PLUS  = PERC + 1
      *                  COMPUTE PERC-MINS  = PERC - 1
      *                  EVALUATE TRUE
      *                      WHEN FIELD-AREA (BARRV-OFF: 5) = PERC-PLUS
      *                       AND PONTEIRO < SIZE-REPKEY
      *                           ADD  1 TO PONTEIRO
      *                           MOVE 0 TO CPERC
      *                           MOVE FIELD-AREA (BARRV-OFF: 5) TO PERC
      *                      WHEN FIELD-AREA (BARRV-OFF: 5) = PERC-MINS
      *                       AND PONTEIRO > 1
      *                           SUBTRACT 1 FROM PONTEIRO
      *                           MOVE 0       TO CPERC
      *                           MOVE FIELD-AREA (BARRV-OFF: 5) TO PERC
      *                      WHEN OTHER
                                  MOVE BARRV-OFF (J) TO POS
                                  MOVE FIELD-AREA(J)(POS: 5) TO PERC
                                  COMPUTE PONTEIRO
                                            = (SIZE-REPKEY / 100) * PERC
                                  IF   PONTEIRO = 0
                                       MOVE 1 TO PONTEIRO
                                  END-IF
      *                  END-EVALUATE
                 END-EVALUATE
           END-PERFORM

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
           PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
           PERFORM 600-CLOSE-WINDOW THRU 600-99-FIM
           if volta-pd = 1
              close cwdirs
              move 0 to volta-pd
              go to pd
           end-if.
           GOBACK.

       000-99-FIM. EXIT.

       110-EXIBE.

           MOVE PONTEIRO TO C
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > JANSIZE
                   MOVE C         TO REPKEY-CHAVE
                   IF   REPKEY-CHAVE > SIZE-REPKEY
                        MOVE "23" TO FS-CWDIRS
                   ELSE
                        READ REPKEY
                        MOVE REPKEY-CW          TO CWDIRS-CW
                                                   CW (Y)
                        MOVE REPKEY-SEQUENCIA   TO CWDIRS-NUMERO
                                                SEQUENCIA (Y)
                        MOVE REPKEY-EXT      TO EXT (Y)
                        MOVE CW (Y)      TO CWDIRS-CW
                        MOVE SEQUENCIA (Y)  TO CWDIRS-NUMERO
                        MOVE "."         TO CWDIRS-PONTO
                        MOVE EXT    (Y)  TO CWDIRS-EXT
                        READ CWDIRS IGNORE LOCK
                   END-IF
                   IF   FS-CWDIRS < "10"
                   AND (CWDIRS-NUMERO    NOT = 0)
                   AND (CWDIRS-NUMERO    NUMERIC)
                   AND (CWDIRS-COMANDO NOT = "Erased")
                   AND (REPKEY-COMANDO NOT = "Erased")
                        MOVE REPKEY-COMANDO  TO COMANDO  (Y)
                        IF  (CWDIRS-IMPRESSO NOT = "*")
                        AND (CWDIRS-IMPRESSO NOT = "+")
                             MOVE SPACE TO CWDIRS-IMPRESSO
                        END-IF
                        MOVE CWDIRS-IMPRESSO TO IMPRESSO (Y)
                        MOVE CWDIRS-CODIGO   TO CWDIRSS  (Y)
                        MOVE CWDIRS-TITULO   TO TITULO-T (Y)
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
ene                                              (2: )
                        MOVE CWDIRS-NOTA       TO NOTA         (Y)
                        MOVE CWDIRS-CW         TO CW           (Y)
                        MOVE CWDIRS-NUMERO     TO SEQUENCIA    (Y)
                        MOVE CWDIRS-EXT        TO EXT          (Y)
                        MOVE REPKEY-CHAVE      TO CHAVE-REPKEY (Y)
                   ELSE
                        INITIALIZE RELATORIO (Y)
                        MOVE SPACES TO EMISSAO (Y)
                        IF   FS-CWDIRS = "9D"
                             MOVE "Em uso" to CWDIRSS (Y)
                        END-IF
                   END-IF
                   MOVE RELATORIO (Y) (2: 77) TO RP (Y + 1)
                   ADD 1 TO C
           END-PERFORM

           PERFORM VARYING T FROM 1 BY 1 UNTIL T > JANSIZE
               MOVE LS-ID  (T)      TO SP2-FD-ID
               MOVE 0               TO SP2-FD-VAR-LEN
               MOVE FD-OUTPUT   (T) TO NEW-OUTPUT
               MOVE FD-CUR-COLR (T) TO NEW-CUR-COLR
               MOVE X"00" TO TESTE-VERDE
               IF  CWDIRSS (T) NOT = SPACES
                   IF   NOTA (T) = "Gerando..."
                   OR   "Terminado anormal"
                   OR   "Interrompido"
                   OR   USUARIO (T) = "Importando..."
                        IF   NOTA (T) = "Gerando..."
                        OR   USUARIO (T) = "Importando..."
                             MOVE VERDE TO NEW-CUR-COLR TESTE-VERDE
                        ELSE
                             MOVE VERMELHO TO NEW-CUR-COLR
                        END-IF
                   ELSE
                        MOVE X"02" TO NEW-CUR-COLR
                   END-IF
                   COMPUTE P = FD-PROG-OFF (T) +  1
                   MOVE RELATORIO (T) (1: 77) TO FIELD-AREA(J)
                                                 (P: LARG)
      *            INSPECT FIELD-AREA(J) (P: LARG)
      *                          CONVERTING ACENTOS-437
      *                                  TO ACENTOS-850
                   INSPECT FIELD-AREA(J) (P: LARG)
                                      CONVERTING LOW-VALUE TO SPACE
                   MOVE "p" TO NEW-OUTPUT
                   IF   NEW-CUR-COLR  = VERDE
                        MOVE "g" TO NEW-OUTPUT
                   END-IF
               ELSE
                   MOVE "h" TO NEW-OUTPUT
               END-IF
               IF (LER-NOTAS NOT = 0)
               AND NEW-OUTPUT NOT = "h"
                   MOVE "g" TO NEW-OUTPUT
               END-IF
               IF   FD-OUTPUT   (T) NOT = NEW-OUTPUT
               OR   FD-CUR-COLR (T) NOT = NEW-CUR-COLR
                    CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                    CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                    MOVE NEW-OUTPUT   TO SP2-FD-OUTPUT
                    MOVE NEW-CUR-COLR TO SP2-FD-CUR-COLR
                    CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                    MOVE SP2-FD-OUTPUT   TO FD-OUTPUT   (T)
                    MOVE SP2-FD-CUR-COLR TO FD-CUR-COLR (T)
               END-IF

               MOVE NT-ID  (T) TO SP2-FD-ID
               IF  CWDIRSS (T) = SPACES
               OR  LER-NOTAS NOT = 1
                   CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   IF  SP2-FD-OUTPUT NOT = "h"
                       CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                       MOVE "h" TO SP2-FD-OUTPUT
                       CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   END-IF
               ELSE
                   IF  CWDIRSS (T) NOT = SPACES
                   AND TESTE-VERDE  = VERDE
                   OR  LER-NOTAS = 1
                       CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                       IF  SP2-FD-OUTPUT NOT = "g"
                           CALL SP2   USING SP2-DELETE-FIELD
                                            SP2-FIELD-DEF
                           MOVE "g" TO SP2-FD-OUTPUT
                           CALL SP2   USING SP2-SET-FIELD-DEF
                                            SP2-FIELD-DEF
                       END-IF
                   END-IF
               END-IF

               MOVE CM-ID  (T) TO SP2-FD-ID
               IF  CWDIRSS (T) = SPACES
               OR  LER-NOTAS NOT = 2
                   CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   IF  SP2-FD-OUTPUT NOT = "h"
                       CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                       MOVE "h" TO SP2-FD-OUTPUT
                       CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   END-IF
               ELSE
                   IF  CWDIRSS (T) NOT = SPACES
                   AND TESTE-VERDE  = VERDE
                   OR  LER-NOTAS = 2
                       CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                       IF  SP2-FD-OUTPUT NOT = "g"
                           CALL SP2   USING SP2-DELETE-FIELD
                                            SP2-FIELD-DEF
                           MOVE "g" TO SP2-FD-OUTPUT
                           CALL SP2   USING SP2-SET-FIELD-DEF
                                            SP2-FIELD-DEF
                       END-IF
                   END-IF
               END-IF

           END-PERFORM

           IF   PRONTO = 99
                PERFORM VARYING PRONTO FROM JANSIZE BY -1
                         UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                            OR PRONTO = 1
                        CONTINUE
                END-PERFORM
           ELSE
                PERFORM VARYING PRONTO FROM PRONTO BY -1
                         UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                            OR PRONTO = 1
                        CONTINUE
                END-PERFORM
                PERFORM VARYING PRONTO FROM PRONTO BY 1
                         UNTIL SEQUENCIA (PRONTO) NOT = SPACES
                            OR PRONTO = JANSIZE
                        CONTINUE
                END-PERFORM.

       110-99-FIM. EXIT.

       111-CHECK-POINT.

           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > JANSIZE
                   IF   LS-ID (PX) = SP2-CD-NEXT-FLD-ID
                   AND  LER-NOTAS = 0
                        MOVE PX TO PRONTO
                   END-IF
                   IF   NT-ID (PX) = SP2-CD-NEXT-FLD-ID
                   AND  LER-NOTAS = 1
                        MOVE PX TO PRONTO
                   END-IF
                   IF   CM-ID (PX) = SP2-CD-NEXT-FLD-ID
                   AND  LER-NOTAS = 2
                        MOVE PX TO PRONTO
                   END-IF
           END-PERFORM.

       111-99-FIM. EXIT.

       112-ORDEM.

           IF   OPCAO-SORT = 1
                MOVE 0 TO OPCAO-SORT
           ELSE
                MOVE 1 TO OPCAO-SORT
           END-IF
           EVALUATE SP2-CD-BUTTON-ID
               WHEN CODIGO-ID   MOVE 1 TO COLUNA-SORT
               WHEN DATA-ID     MOVE 2 TO COLUNA-SORT
               WHEN PAGINA-ID   MOVE 3 TO COLUNA-SORT
               WHEN NOTA-ID     MOVE 4 TO COLUNA-SORT
               WHEN USUARIO-ID  MOVE 5 TO COLUNA-SORT
           END-EVALUATE
           PERFORM 400-SP2-TOPO       THRU 400-99-FIM
           PERFORM 115-PREPARA-LISTA  THRU 115-99-FIM.

       112-99-FIM. EXIT.

       113-IMPORTA.

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
                MOVE SPACES TO CWPATH-PATH
                STRING PATH-SPOOL DELIMITED BY SPACE
                       "\*.cw6"   DELIMITED BY SIZE
                       INTO CWPATH-PATH
           ELSE
                MOVE SPACES TO CWPATH-PATH
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            RETURN-STATUS
                CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
                STRING OLD-DRIVE DELIMITED BY SPACE
                       ":\"           DELIMITED BY SPACE
                       OLD-DIRECTORY  DELIMITED BY SPACE
                       "\"            DELIMITED BY SIZE
                       PATH-SPOOL     DELIMITED BY SPACE
                       "\*.cw6"       DELIMITED BY SIZE
                INTO CWPATH-PATH
           END-IF
           CALL "CWPATH" USING PARAMETROS-CWPATH
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
                      MOVE SPACES TO PERC-TIT
                      STRING "Importando de: "
                             LB-TEXTO DELIMITED BY SIZE INTO PERC-TIT
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
                     IF   RESPOSTA = "S" OR "s"
                          CALL "CBL_DELETE_FILE" USING LB-PRNTER
                          IF RETURN-CODE = 0
                             DELETE CWDIRS RECORD
                          END-IF
                     END-IF
                     UNLOCK CWDIRS
                END-IF
           END-IF
           MOVE    1                   TO PONTEIRO
           EXEC COBOLware PROCESS CLOSE END-EXEC.

      *113-IMPORTOU.
      *
      *    PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
      *    PERFORM 400-SP2-TOPO      THRU 400-99-FIM
      *    MOVE    1                   TO PONTEIRO.

       113-99-FIM. EXIT.

       114-GRAVA-SPOOL.

           IF  (TEXTO-REG (1: 2) = "CW" OR TEXTO-REG (1: 1) = "R")
           AND  TEXTO-REG (9: 4) = ".SPL" OR ".spl"
                PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                PERFORM TEST AFTER UNTIL FS-CWDIRS < "10"
                        MOVE "CW999999"     TO CWDIRS-SPOOL
                        START CWDIRS KEY NOT GREATER CWDIRS-SPOOL
                        READ CWDIRS PREVIOUS RECORD IGNORE LOCK
                        MOVE "."               TO CWDIRS-PONTO
                        MOVE TEXTO-REG (10: 3) TO CWDIRS-EXT
                        ADD  1 TO CWDIRS-NUMERO
                        IF   CWDIRS-NUMERO    = 0
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
                CALL "CBL_SET_CSR_POS" USING X"0B0A"
                CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR
                MOVE SPACES TO PERC-OBS
                STRING CWDIRS-CODIGO DELIMITED BY SPACE
                       " "           DELIMITED BY SIZE
                       OBS-DATA      DELIMITED BY SIZE
                       " "           DELIMITED BY SIZE
                       CWDIRS-HORA   DELIMITED BY SIZE
                       ":"           DELIMITED BY SIZE
                       CWDIRS-MINUTO DELIMITED BY SIZE
                                INTO PERC-OBS
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
                EXEC COBOLware PROCESS (SHOW)
                     HEADER PERC-TIT
                     LINE 10 COLUMN 10
                     EMPTY CWDIRS-LINHAS
                     FULL RK-PRNTER
                     MESSAGE PERC-OBS
                     CANCEL PERC-OPT;PERC-OPT
                     QUESTION "Cancelar importa‡Æo ?"
                END-EXEC
                MOVE CORRENTE TO SP2-ND-NAME
                CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
                IF   PERC-OPT = "Y"
                     MOVE "S" TO RESPOSTA
                END-IF
                IF   RK-PRNTER = CWDIRS-LINHAS
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

           IF   OPCAO-SORT = 1
                MOVE LOW-VALUES  TO CWDIRS-REG
           ELSE
                MOVE HIGH-VALUES TO CWDIRS-REG
           END-IF

           PERFORM 116-RESTART-CWDIRS THRU 116-99-FIM

           IF   FS-CWDIRS NOT = "00"
                CALL "CWISAM" USING ER-CWDIRS
                IF CWSPOOLHANDLER = SPACES
                   PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
                   PERFORM 600-CLOSE-WINDOW THRU 600-99-FIM
                ELSE
                   IF LB-TEXTO NOT = SPACES
                      EXEC COBOLware Save File LB-TEXTO CLOSE END-EXEC
                   END-IF
                END-IF
                goback
           END-IF

           CLOSE REPKEY DELETE FILE REPKEY
           OPEN I-O REPKEY
           MOVE 0 TO REPKEY-CHAVE
           IF   FS-REPKEY > "09"
                CALL "CWISAM" USING ER-REPKEY
                IF CWSPOOLHANDLER = SPACES
                   PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
                   PERFORM 600-CLOSE-WINDOW THRU 600-99-FIM
                ELSE
                   IF LB-TEXTO NOT = SPACES
                      EXEC COBOLware Save File LB-TEXTO CLOSE END-EXEC
                   END-IF
                END-IF
                goback
           END-IF

           PERFORM UNTIL FS-CWDIRS > "09"
                   PERFORM 117-LER-CWDIRS THRU 117-99-FIM
                   IF   FS-CWDIRS < "10"
                   AND  CWDIRS-NUMERO    NOT = 0
                      IF   FS-CWDIRS NOT = "9D"
                        MOVE CWDIRS-SPOOL TO LB-PRNTER
                        PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
                        SET SPOOL-TEST TO TRUE
                        PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM
                        SET SPOOL-OK TO TRUE
                        IF FS-PRNTER < "10"
                        OR FS-PRNTER = "9A"
                           IF  (CWDIRS-COMANDO NOT = "Erased")
                           AND (CWDIRS-CODIGO  NOT = SPACES)
                              ADD  1             TO REPKEY-CHAVE
                              MOVE CWDIRS-CW TO REPKEY-CW
                              MOVE CWDIRS-NUMERO
                                             TO REPKEY-SEQUENCIA
                              MOVE CWDIRS-EXT    TO REPKEY-EXT
                              MOVE SPACES        TO REPKEY-COMANDO
                                                    REPKEY-NOTA
                              MOVE 0             TO REPKEY-NOVA
                              WRITE REPKEY-REG
                              READ CWDIRS
                              IF   FS-CWDIRS < "10"
                              AND (CWDIRS-NOTA = "Gerando..."
                              OR  CWDIRS-USUARIO = "Importando...")
                                   MOVE "Terminado anormal"
                                     TO CWDIRS-NOTA
                                   PERFORM 118-CONTA-FOLHAS
                                      THRU 118-99-FIM
                                   REWRITE CWDIRS-REG
                              END-IF
ana                           PERFORM 116-RESTART-CWDIRS
ana                              THRU 116-99-FIM
                              MOVE 0 TO CWDIRS-NUMERO
                              PERFORM 117-LER-CWDIRS
                                 THRU 117-99-FIM
                                      UNTIL CWDIRS-NUMERO
                                          = REPKEY-SEQUENCIA
                                        AND CWDIRS-CW
                                          = REPKEY-CW
                                            OR FS-CWDIRS > "09"
                              IF (CWSPOOLHANDLER NOT = SPACES)
                              AND(LB-TEXTO NOT = SPACES)
                              AND FS-CWDIRS < "10"
                                  INSPECT CWDIRS-PROGRAMA
                                    CONVERTING LOW-VALUES TO SPACE
                                  PERFORM 150-GRAVA-XML
                                     THRU 150-99-FIM
                              END-IF
                           ELSE
                             IF CWDIRS-CODIGO = SPACES
                                DELETE CWDIRS RECORD
                             END-IF
                           END-IF
                        ELSE
                           IF FS-PRNTER = "30" OR "35"
                              DELETE CWDIRS RECORD
                           END-IF
                        END-IF
                        IF   CWDIRS-CODIGO = SPACES
                             CALL "CBL_DELETE_FILE" USING LB-PRNTER
                        END-IF
                      END-IF
                      UNLOCK CWDIRS
                   END-IF
                   IF   FS-CWDIRS = "9D"
                        MOVE "00" TO FS-CWDIRS
                   END-IF
           END-PERFORM

           IF   ERROS NOT = ZERO
                IF   SD-ID (1 1) NOT = 0
                     MOVE SD-ID (1 1) TO SP2-SD-ID
                     CALL "CWSPID" USING SP2-SD-ID "SDelete"
                     CALL SP2   USING SP2-DELETE-STATIC
                                      SP2-STATIC-DEF
                     MOVE 0 TO SD-ID (1 1)
                END-IF
                MOVE 0 TO ERROS
           END-IF

           MOVE REPKEY-CHAVE TO SIZE-REPKEY.

       115-99-FIM. EXIT.

       116-RESTART-CWDIRS.

           IF   OPCAO-SORT = 1
                EVALUATE COLUNA-SORT
                    WHEN 1     START CWDIRS KEY NOT < CWDIRS-CODIGO
                    WHEN 3     START CWDIRS KEY NOT < CWDIRS-FOLHAS
                    WHEN 4     START CWDIRS KEY NOT < CWDIRS-NOTA
                    WHEN 5     START CWDIRS KEY NOT < CWDIRS-USUARIO
                    WHEN OTHER START CWDIRS KEY NOT < CWDIRS-EMISSAO
                END-EVALUATE
           ELSE
                EVALUATE COLUNA-SORT
                    WHEN 1     START CWDIRS KEY NOT > CWDIRS-CODIGO
                    WHEN 3     START CWDIRS KEY NOT > CWDIRS-FOLHAS
                    WHEN 4     START CWDIRS KEY NOT > CWDIRS-NOTA
                    WHEN 5     START CWDIRS KEY NOT > CWDIRS-USUARIO
                    WHEN OTHER START CWDIRS KEY NOT > CWDIRS-EMISSAO
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

           PERFORM VARYING RK-PRNTER FROM 1 BY 1
                           UNTIL FS-PRNTER NOT = "00"
                   EXEC COBOLware PROCESS (SHOW)
                        HEADER "Verificando fim anormal"
                        LINE 10 COLUMN 10
                        MESSAGE "OFF"
                   END-EXEC
                   PERFORM 335-READ-PRNTE THRU 335-99-FIM
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

           EXEC COBOLware PROCESS CLOSE
           END-EXEC
                MOVE CORRENTE TO SP2-ND-NAME
                CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF.

       118-99-FIM. EXIT.

       120-EXEC.

           CALL "CBL_SET_CSR_POS" USING ERASE-CURSOR
           MOVE 0 TO REORDENAR EXPORTOU
           MOVE PONTEIRO TO SALVA-PONTEIRO
           MOVE 1        TO VEZ
           MOVE 1        TO VEZ-NOTA
           PERFORM VARYING P FROM 1 BY 1 UNTIL P GREATER SIZE-REPKEY
                   MOVE P          TO REPKEY-CHAVE
                   IF   REPKEY-CHAVE > SIZE-REPKEY
                        MOVE "23" TO FS-CWDIRS
                   ELSE
                        READ REPKEY
                        MOVE REPKEY-CW        TO CWDIRS-CW
                        MOVE REPKEY-SEQUENCIA TO CWDIRS-NUMERO
                        MOVE REPKEY-EXT    TO CWDIRS-EXT
                        READ CWDIRS IGNORE LOCK
                   END-IF
                   IF   FS-CWDIRS < "10"
                   AND (REPKEY-COMANDO NOT = SPACES)
                   AND (REPKEY-COMANDO NOT = "*")
                   AND (REPKEY-COMANDO NOT = "Erased")
                   AND (CWDIRS-NUMERO     NOT = 0)
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
                             PERFORM 251-EXPORTA   THRU 251-99-FIM
                             MOVE "*"                TO REPKEY-COMANDO
                             MOVE "N"                TO RESPOSTA
                             REWRITE                    REPKEY-REG
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
                                       MOVE REPKEY-SEQUENCIA
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
                             PERFORM 125-PR THRU 125-99-FIM
                        END-EVALUATE
                   END-IF
                   UNLOCK CWDIRS
           END-PERFORM.

           MOVE SPACES         TO TEXTO-TELA
           MOVE LOW-VALUES     TO SAVE-TEXTO-TELA
           MOVE SALVA-PONTEIRO TO PONTEIRO
           IF   REORDENAR = 1
                PERFORM 115-PREPARA-LISTA THRU 115-99-FIM
           END-IF
           PERFORM 400-SP2-TOPO THRU 400-99-FIM.

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
           IF   IMPRIMIU
           AND  (PRINTDEL OR CWPURGEPRINT = 'ON')
                PERFORM 250-REMOVER THRU 250-99-FIM
           ELSE
                MOVE   "*" TO REPKEY-COMANDO
                REWRITE       REPKEY-REG
           END-IF.

       125-99-FIM. EXIT.

       150-GRAVA-XML.

           MOVE SPACES            TO XML-REG
           STRING PATH-SPOOL DELIMITED BY SPACE
                   BARRA-DIR DELIMITED BY SIZE
                CWDIRS-SPOOL DELIMITED BY SPACE
                                INTO Arquivo
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

      *    PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
           IF  SPOOL-NUMERO = 0
               MOVE "CWMEN9"  TO MESTRE
           END-IF
           MOVE "CWMEN9X" TO CORRENTE
           IF   SPOOL-NUMERO = 0
                MOVE SP2-CD-NEXT-FLD-ID TO SP2-FD-ID SAVE-ID
                CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                MOVE X"02"  TO SP2-FD-COLR
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF
           PERFORM 500-OPEN-WINDOW THRU 500-99-FIM
           MOVE LOW-VALUES   TO SP2-MD-DATA
           MOVE 4            TO SP2-MD-OPTION-CNT
           MOVE "CWMEN9"     TO SP2-MD-NAME
           MOVE 1            TO SP2-MDO-ID   (1)
           MOVE "~Imprimir"  TO SP2-MDO-TEXT (1)
           MOVE 2            TO SP2-MDO-ID   (2)
           MOVE "~Pesquisar" TO SP2-MDO-TEXT (2)
           MOVE 3            TO SP2-MDO-ID   (3)
           MOVE "~Zebra"     TO SP2-MDO-TEXT (3)
           MOVE 4            TO SP2-MDO-ID   (4)
           MOVE "~Sair"      TO SP2-MDO-TEXT (4)
           PERFORM ACENTOS THRU FIM-ACENTOS
                   VARYING M FROM 1 BY 1 UNTIL M > 4

           COMPUTE SP2-MD-VAR-LEN = SP2-MD-OPTION-CNT *
                  (SP2-MD-OPTN-LEN + SP2-MD-OPTC-LEN + SP2-MD-OPTV-LEN)
           CALL SP2   USING SP2-SET-MENU-DEF SP2-MENU-DEF
           CALL "CWSPID" USING FIRST-ID "FInsert"
           MOVE LOW-VALUES    TO SP2-FD-DATA
                                 SP2-FD-VAR-LENS
           MOVE "p"         TO SP2-FD-OUTPUT
           move "o"         to SP2-FD-PROG-CTRL
           MOVE "y"         TO SP2-FD-CURS-SKIP
           MOVE "n"         TO SP2-FD-CURS-SHOW
           MOVE 1           TO SP2-FD-WIDTH
           MOVE FIRST-ID    TO SP2-FD-ID
           MOVE X"70"       TO SP2-FD-COLR
           MOVE SPOOL-COLOR TO SP2-FD-COLR
           MOVE X"70"       TO SP2-FD-CUR-COLR
           MOVE 23          TO SP2-FD-ROW
           MOVE 78          TO SP2-FD-COL
           CALL SP2      USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           MOVE LOW-VALUES    TO SP2-FD-DATA
                                 SP2-FD-VAR-LENS

           SUBTRACT 1 FROM JANSIZE
           PERFORM 405-V-BARR THRU 405-99-FIM
           ADD      1   TO JANSIZE
           PERFORM 406-H-BARR THRU 406-99-FIM
           CALL SP2   USING SP2-SET-PANEL-FIELDS FIELD-AREA(J)

           MOVE CWDIRS-SPOOL  TO LB-PRNTER
           MOVE 1             TO I PONTEIRO
           MOVE CWDIRS-LINHAS TO C
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM
           PERFORM 333-OPEN-PRNTE   THRU 333-99-FIM

           IF   FS-PRNTER > "09"
                GO TO 200-99-FIM
           END-IF

           PERFORM 201-CHECK-ZEBRA THRU 201-99-FIM
           PERFORM UNTIL SP2-CD-KEY = SP2-KEY-ESC
                   MOVE 0 TO SP2-CD-KEY
                   PERFORM TEST AFTER UNTIL NOT CMD-MODOS
                   MOVE SPACE TO CMD
                   PERFORM 210-EXIBE THRU 210-99-FIM
                   CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                   MOVE CORRENTE TO SP2-CD-NEXT-PANEL
                   move FIRST-ID to SP2-CD-NEXT-FLD-ID
      *                             SP2-CD-LAST-FLD-ID
      *            CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                   CALL SP2   USING SP2-CONVERSE-PANEL SP2-CONVERSE-DATA
                   move FIRST-ID to SP2-CD-NEXT-FLD-ID
      *                             SP2-CD-LAST-FLD-ID
                   PERFORM 800-EVITA-BUG-SP2 THRU 800-99-FIM
                   IF   SP2-CD-KEY = SP2-KEY-CTRL-P
                        CALL "CBL_WRITE_SCR_CHARS"
                              USING SCREEN-POSITION
                                    CHARACTER-BUFFER
                                    STRING-LENGTH
                        CALL "CWPRTS"
                        CANCEL "CWPRTS"
                        EXIT PERFORM CYCLE
                   END-IF
      *            IF   SP2-CD-KEY = SP2-KEY-CLOSE
      *                 MOVE SP2-KEY-ESC TO SP2-CD-KEY
      *            END-IF
                   IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
                   OR   SP2-KEY-APP-CLOSE
                   or   SP2-KEY-CLOSE
                        PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
                        STOP RUN
                   END-IF
                   IF   SP2-CD-KEY = SP2-KEY-ESC
                   OR  (SP2-CD-KEY = SP2-KEY-MENU
                            AND SP2-CD-MENU-ID = 4)
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        EXIT PERFORM
                   ELSE
                       EVALUATE TRUE
                           WHEN SP2-CD-KEY = SP2-KEY-MENU
                            AND SP2-CD-MENU-ID = 1
                                MOVE "L"  TO CMD
                           WHEN SP2-CD-KEY = SP2-KEY-MENU
                            AND SP2-CD-MENU-ID = 2
                                MOVE "P"  TO CMD
                           WHEN SP2-CD-KEY = SP2-KEY-MENU
                            AND SP2-CD-MENU-ID = 3
                                MOVE "Z"  TO CMD
                       END-EVALUATE
                   END-IF
                   IF   CMD = "Z" OR "z"
                        IF   ZEBRADO = 0
                             MOVE 1 TO ZEBRADO
                        ELSE
                             MOVE 0 TO ZEBRADO
                        END-IF
                        PERFORM 201-CHECK-ZEBRA THRU 201-99-FIM
                   END-IF
                   END-PERFORM
                   EVALUATE TRUE
                    WHEN CMD = "L" OR "l"
                         MOVE    PONTEIRO         TO SALVA-PONTEIRO-2
                         PERFORM 220-IMPRIMIR   THRU 220-99-FIM
                         MOVE    1                TO I
                         MOVE    CWDIRS-LINHAS    TO C
                         MOVE    SALVA-PONTEIRO-2 TO PONTEIRO
                    WHEN CMD = "P" OR "p" OR "/"
                         PERFORM 230-LOCALIZA THRU 230-99-FIM
                    WHEN SP2-CD-KEY = SP2-KEY-DOWN
                     AND PONTEIRO < C
                         ADD 1 TO PONTEIRO
                    WHEN SP2-CD-KEY = SP2-KEY-UP
                     AND  PONTEIRO > I
                         SUBTRACT 1 FROM PONTEIRO
                    WHEN SP2-CD-KEY = SP2-KEY-PGDN
                         IF   PONTEIRO < (C - 14)
                              ADD JANSIZE TO PONTEIRO
                         ELSE
                              IF   PONTEIRO NOT = (C - 14)
                                   COMPUTE PONTEIRO = C - 14
                                   IF   PONTEIRO < 1
                                        MOVE 1 TO PONTEIRO
                                   END-IF
                              END-IF
                         END-IF
                    WHEN SP2-CD-KEY = SP2-KEY-PGUP
                         IF   PONTEIRO > (I + JANSIZE)
                              SUBTRACT JANSIZE FROM PONTEIRO
                         ELSE
                              IF   PONTEIRO NOT = I
                                   MOVE I TO PONTEIRO
                              END-IF
                         END-IF
                    WHEN SP2-CD-KEY = SP2-KEY-HOME OR SP2-KEY-CTRL-PGUP
                         MOVE I TO PONTEIRO
                    WHEN SP2-CD-KEY = SP2-KEY-END  OR SP2-KEY-CTRL-PGDN
                         COMPUTE PONTEIRO = C - 14
                         IF   PONTEIRO < I
                              MOVE I TO PONTEIRO
                         END-IF
                    WHEN SP2-CD-KEY = SP2-KEY-RIGHT
                     AND COLUNA < LIMITE
                         ADD 1 TO COLUNA
                    WHEN SP2-CD-KEY = SP2-KEY-LEFT
                     AND COLUNA > 1
                         SUBTRACT 1 FROM COLUNA
                    WHEN SP2-CD-KEY = SP2-KEY-CTRL-RIGHT
                                        OR SP2-KEY-ENTER
                         IF   (COLUNA + 10) < LIMITE
                              ADD 10 TO COLUNA
                         ELSE
                              IF   COLUNA NOT = LIMITE
                                   MOVE LIMITE TO COLUNA
                              END-IF
                         END-IF
                    WHEN SP2-CD-KEY = SP2-KEY-TAB
                      OR SP2-KEY-CTRL-LEFT
                         IF   COLUNA > 10
                              SUBTRACT 10 FROM COLUNA
                         ELSE
                              IF   COLUNA > 1
                                   MOVE 1 TO COLUNA
                              END-IF
                         END-IF
                    WHEN OTHER
                         PERFORM 202-BARRAS THRU 202-99-FIM
                   END-EVALUATE
           END-PERFORM
           CALL "CWSPID" USING FIRST-ID "FDelete"
           MOVE FIRST-ID    TO SP2-FD-ID
           CALL SP2      USING SP2-DELETE-FIELD SP2-FIELD-DEF

           MOVE 0 TO SP2-CD-KEY
           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
           MOVE "CWMENU"  TO MESTRE
           MOVE "CWMEN9"  TO CORRENTE
           CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           DISPLAY "CWTITLE"  UPON ENVIRONMENT-NAME
           DISPLAY TITULO-TT(TT) UPON ENVIRONMENT-VALUE
           SUBTRACT 1 FROM J TT

           IF   SPOOL-NUMERO = 0
                MOVE SAVE-ID  TO SP2-FD-ID
                CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                MOVE X"00"    TO SP2-FD-COLR
                MOVE SPOOL-COLOR TO SP2-FD-COLR
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF.

       200-99-FIM. EXIT.

       201-CHECK-ZEBRA.

           IF   ZEBRADO = 1
                MOVE ZEBRA-1 TO ZEBRA-P
                MOVE ZEBRA-2 TO ZEBRA-I
                IF   CWDIRS-NOTA = "Gerando..."
                OR   "Terminado anormal"
                OR   "Interrompido"
                OR   CWDIRS-USUARIO = "Importando..."
                     MOVE ZEBRA-3 TO ZEBRA-P
                     MOVE ZEBRA-4 TO ZEBRA-I
                END-IF
           ELSE
                MOVE ZEBRA-2 TO ZEBRA-I
                                ZEBRA-P
           END-IF.

       201-99-FIM. EXIT.

       202-BARRAS.

           MOVE BARRV-OFF(J) TO POS
           IF  (FIELD-AREA(J) (1: 5) NOT = PERC)
           AND (FIELD-AREA(J) (POS: 5) NOT = PERC)
      *         COMPUTE PERC-PLUS  = PERC + 1
      *         COMPUTE PERC-MINS  = PERC - 1
      *         EVALUATE TRUE
      *             WHEN FIELD-AREA (1: 5) = PERC-PLUS
      *              AND PONTEIRO < CWDIRS-LINHAS
      *                  ADD  1 TO PONTEIRO
      *                  MOVE 0 TO CPERC
      *                  MOVE FIELD-AREA (1: 5) TO PERC
      *             WHEN FIELD-AREA (1: 5) = PERC-MINS
      *              AND PONTEIRO > 1
      *                  SUBTRACT 1 FROM PONTEIRO
      *                  MOVE 0       TO CPERC
      *                  MOVE FIELD-AREA (1: 5) TO PERC
      *             WHEN OTHER
                         MOVE PERC                 TO PERC-A
                         MOVE FIELD-AREA(J) (1: 5) TO PERC
                         EVALUATE TRUE
                             WHEN PERC = PERC-A + 1
                                  ADD 1 TO PONTEIRO
                             WHEN PERC-A NOT = 0
                              AND (PERC = PERC-A - 1)
                                  SUBTRACT 1 FROM PONTEIRO
                             WHEN OTHER
                                  COMPUTE PONTEIRO
                                          = (CWDIRS-LINHAS / 100) * PERC
                         END-EVALUATE
                         IF   PONTEIRO = 0
                              MOVE 1 TO PONTEIRO
                         END-IF
      *         END-EVALUATE
           END-IF

           IF  (FIELD-AREA(J) (BARRH-OFF: 5) NOT = PERC2)
      *         COMPUTE PERC2-PLUS = PERC2 + 1
      *         COMPUTE PERC2-MINS = PERC2 - 1
      *         EVALUATE TRUE
      *             WHEN FIELD-AREA (BARRH-OFF: 5) = PERC2-PLUS
      *              AND COLUNA < (TR - 3)
      *                  ADD 1  TO COLUNA
      *                  MOVE 0 TO CPERC2
      *                  MOVE FIELD-AREA(BARRH-OFF: 5) TO PERC2
      *             WHEN FIELD-AREA (BARRH-OFF: 5) = PERC2-MINS
      *              AND I > 1
      *                  SUBTRACT 1 FROM COLUNA
      *                  MOVE 0 TO CPERC2
      *                  MOVE FIELD-AREA(BARRH-OFF: 5) TO PERC2
      *             WHEN OTHER
                         MOVE FIELD-AREA(J) (BARRH-OFF: 5) TO PERC2
                         COMPUTE COLUNA = ((TR - 3)  / 100) * PERC2
                         IF   COLUNA = 0
                              MOVE 1 TO COLUNA
                         END-IF
      *         END-EVALUATE
           END-IF.

       202-99-FIM. EXIT.

       210-EXIBE.

           MOVE SPACES     TO TEXTO-TELA
                              CHARACTER-BUFFER
      *    MOVE LOW-VALUES TO SAVE-TEXTO-TELA
           MOVE PONTEIRO   TO RK-PRNTER

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > JANSIZE
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
                   IF   RK-PRNTER (10: 1) = "0" OR "2" OR "4"
                                             OR "6" OR "8"
                        MOVE ZEBRA-P TO ZEBRA
                   ELSE
                        MOVE ZEBRA-I TO ZEBRA
                   END-IF
                   MOVE TEXTO2 (Y) TO RP (Y)
                   PERFORM 215-PUT-LINE THRU 215-99-FIM
                   ADD  1 TO RK-PRNTER
           END-PERFORM

           IF   CPERC = 1
                COMPUTE PERC = PONTEIRO / CWDIRS-LINHAS * 100
                MOVE BARRV-OFF (J) TO POS
                MOVE PERC TO FIELD-AREA(J) (POS: 5)
           ELSE
                MOVE 1 TO CPERC
           END-IF

           IF   CPERC2 = 1
                COMPUTE PERC2 = COLUNA / (TR - 3) * 100
                MOVE PERC2 TO FIELD-AREA(J) (BARRH-OFF: 5)
           ELSE
                MOVE 1 TO CPERC2
           END-IF.

       210-99-FIM. EXIT.

       215-PUT-LINE.

           IF   SD-ID (J Y) = 0
                CALL "CWSPID" USING SP2-SD-ID "SInsert"
                MOVE SP2-SD-ID TO SD-ID (J Y)
           ELSE
                IF   TEXTO2 (Y) = SAVE-TEXTO2 (Y)
                AND  ZEBRA = SAVE-ZEBRA (Y)
                     GO TO 215-99-FIM
                ELSE
                     MOVE SD-ID (J Y) TO SP2-SD-ID
                     CALL SP2   USING SP2-DELETE-STATIC SP2-STATIC-DEF
                     CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                END-IF
           END-IF
           MOVE TEXTO2 (Y) TO SP2-SD-TEXT SAVE-TEXTO2 (Y)
           IF ZEBRADO = 0
              MOVE SPOOL-COLOR TO ZEBRA
           END-IF
           MOVE ZEBRA      TO SP2-SD-COLR SAVE-ZEBRA  (Y)
           MOVE CWFONT-REFERENCE TO SP2-SD-FONT-ID
           COMPUTE SP2-SD-ROW = Y - 1
           MOVE 780        TO SP2-SD-WIDTH
           MOVE 80         TO SP2-SD-VAR-LEN
           MOVE 10         TO SP2-SD-HEIGHT
ita            if resolution = 1
ita               ADD 1 TO SP2-SD-HEIGHT
ita            end-if

           IF   CWLITS = "LOW"
                INSPECT SP2-SD-TEXT (1: 80)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-SD-TEXT (1: 80)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-SD-TEXT (1: 80)
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT SP2-SD-TEXT (1: 80)
                        CONVERTING ACENTOS-850 TO ACENTOS-437
           END-IF

           MOVE 80 TO SP2-SD-TEXT-LEN
           CALL SP2   USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       215-99-FIM. EXIT.

       220-IMPRIMIR.

           MOVE ZERO TO P-I
                        N-L FIRST-FLAG
           MOVE 9999 TO P-F

           IF   CWDIRS-FOLHAS NOT = 0
                MOVE 1             TO P-I P-I-Z
                MOVE CWDIRS-FOLHAS TO P-F P-F-Z
      *         MOVE SPACES TO POP
      *         STRING "Da folha " P-I-Z " a " P-F-Z DELIMITED BY SIZE
      *                INTO POP
      *         CALL "CWMSGW" USING "250370" POP

                PERFORM TEST AFTER
                        UNTIL (P-F NOT < P-I
                          AND  P-I NOT < 1
                          AND  P-F NOT > CWDIRS-FOLHAS)
                           OR (9999 = P-I AND P-F)
                        PERFORM 320-STOP-EXIT THRU 320-99-FIM
                        EXEC COBOLware BoxDialog
                             LINE 11 COLUMN 20
                             HEADER "Sele‡Æo de intervalo"
                             Caption(1) "Da folha:"
                             Caption(2) "At‚     :"
                             Data(1) P-I;P-I Size(1) 4 (Numeric(1))
                             Data(2) P-F;P-F Size(2) 4 (Numeric(2))
                             CANCEL ;OPT-C
      *                      COLOR 15
                        END-EXEC
                        IF   OPT-C = "Y"
                             MOVE 9999 TO P-I P-F
                        END-IF
                        END-PERFORM
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
                        EXEC COBOLware BoxDialog
                             LINE 11 COLUMN 20
                             HEADER "Limite de impressÆo"
                             Caption(1) "Linhas"
                             Data(1) N-L;N-L Size(1) 10
                             (Numeric(1))
                             CANCEL ;OPT-C
      *                      COLOR 15
                        END-EXEC
                        IF   OPT-C = "Y"
                             MOVE 9999999999 TO N-L
                             MOVE "S" TO RESPOSTA
                             EXIT PERFORM
                        END-IF
                END-PERFORM
                MOVE PONTEIRO      TO I
                COMPUTE C = PONTEIRO + N-L - 1
           END-IF.

       FECHAR-RT.

           PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM

           MOVE PONTEIRO             TO SALVA-PONTEIRO-2
           MOVE I                    TO PONTEIRO

           PERFORM 260-LISTAR      THRU 260-99-FIM
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
                                OR (PATH (MP-I:1) NOT = " ")
                   CONTINUE
           END-PERFORM

           MOVE MP-I TO MP-Y
           ADD  1    TO MP-Y
           PERFORM VARYING MP-I FROM 1 BY 1 UNTIL MP-I > MP-Y
                   IF   PATH (MP-I:1) = "\" OR "/" OR " "
                        MOVE "\" TO PATH (MP-I:1)
                        CALL "CBL_CREATE_DIR" USING PATH-MD
                   END-IF
                   MOVE PATH (MP-I:1) TO PATH-MD (MP-I:1)
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

           EXEC COBOLware BoxDialog
                COLUMN 5
                HEADER "Dados para pesquisa"
                Caption(1) "Pesquisar por:"
                Data(1) CADEIA;CADEIA Size(1) 50
                CANCEL ;OPT-C
      *         COLOR 15
           END-EXEC

           MOVE SPACE TO RESPOSTA

           IF   OPT-C = "Y"
                MOVE "S" TO RESPOSTA
                GO TO 230-99-FIM
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
                MOVE 'Procurando "' TO PERC-TIT
                MOVE CADEIA         TO PERC-TIT (13:)
                PERFORM VARYING I FROM 80 BY -1
                          UNTIL PERC-TIT (I: 1) NOT = SPACE
                                CONTINUE
                END-PERFORM
                ADD 1 TO I
                MOVE '"' TO PERC-TIT (I: 1)
                PERFORM 240-BUSCA THRU 240-99-FIM
                        VARYING RK-PRNTER FROM RK-PRNTER BY 1
                                UNTIL IGUAIS = TC
                                   OR RK-PRNTER > C
                IF   IGUAIS = TC
                     EXEC COBOLware PROCESS CLOSE END-EXEC
                     MOVE CORRENTE TO SP2-ND-NAME
                     CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
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
                     PERFORM 210-EXIBE THRU 210-99-FIM
                END-IF
           END-IF.

       230-99-FIM. EXIT.

       240-BUSCA.

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
           EXEC COBOLware PROCESS (SHOW)
                LINE 10 COLUMN 10
                HEADER PERC-TIT
                EMPTY C
                FULL RK-PRNTER
                MESSAGE CADEIA
                QUESTION "Interromper pesquisa ?"
                CANCEL PERC-OPT;PERC-OPT
           END-EXEC
           MOVE CORRENTE TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
           IF  PERC-OPT = "Y"
               MOVE "S" TO RESPOSTA
               COMPUTE RK-PRNTER = C + 1
           END-IF.

       240-99-FIM. EXIT.

       250-REMOVER.

           MOVE CWDIRS-SPOOL TO LB-PRNTER
           PERFORM 330-LABEL-PRNTER THRU 330-99-FIM

           IF   CWDIRS-NUMERO    NOT = 0
                MOVE "Erased" TO CWDIRS-COMANDO
                INITIALIZE REPKEY-SEQUENCIA
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
                    CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                RETURN-STATUS
                    CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                           SIZE-OLD-DIR
                    STRING OLD-DRIVE DELIMITED BY SPACE
                           ":\"           DELIMITED BY SPACE
                           OLD-DIRECTORY  DELIMITED BY SPACE
                           "\"            DELIMITED BY SIZE
                           PATH-SPOOL     DELIMITED BY SPACE
                           "\*.cw6"       DELIMITED BY SIZE
                    INTO CWPATH-PATH
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
                MOVE SPACES TO PERC-TIT
                STRING "Exportando para: "
                       LB-TEXTO DELIMITED BY SIZE INTO PERC-TIT
                MOVE CWDIRS-DATA       TO CWTIME-DATE
                SET  CWTIME-REVERSED   TO TRUE
                SET  CWTIME-REVERSE    TO TRUE
                CALL "CWTIME" USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL TO OBS-DATA
                MOVE SPACES TO PERC-OBS
                STRING CWDIRS-CODIGO DELIMITED BY SPACE
                       " "           DELIMITED BY SIZE
                       OBS-DATA      DELIMITED BY SIZE
                       " "           DELIMITED BY SIZE
                       CWDIRS-HORA   DELIMITED BY SIZE
                       ":"           DELIMITED BY SIZE
                       CWDIRS-MINUTO DELIMITED BY SIZE
                             INTO PERC-OBS
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
XX              END-IF
                PERFORM VARYING RK-PRNTER FROM 1 BY 1
                                UNTIL RK-PRNTER > CWDIRS-LINHAS
                                   OR RESPOSTA = "S"
                 PERFORM 335-READ-PRNTE THRU 335-99-FIM
                 MOVE PRINTS-WS   TO TEXTO-REG
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
                 EXEC COBOLware PROCESS (SHOW)
                      LINE 10 COLUMN 10
                      HEADER PERC-TIT
                      EMPTY CWDIRS-LINHAS
                      FULL RK-PRNTER
                      MESSAGE PERC-OBS
                      CANCEL PERC-OPT;PERC-OPT
                      QUESTION "Cancelar exporta‡Æo ?"
                 END-EXEC
                 MOVE CORRENTE TO SP2-ND-NAME
                 CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
                 IF   PERC-OPT = "Y"
                      MOVE "S" TO RESPOSTA
                      MOVE 2   TO EXPORTOU
                 END-IF
                END-PERFORM
                IF CWDIRS-EXT = 'spl'
                   call "CBL_CLOSE_FILE" using texto-handle
                ELSE
                   CLOSE TEXTO
                END-IF
                PERFORM 334-CLOSE-PRNTE THRU 334-99-FIM
                IF   RESPOSTA = "S"
                     DELETE FILE TEXTO
                END-IF
               MOVE "*"         TO REPKEY-COMANDO
               REWRITE             REPKEY-REG
           END-IF.

       251-99-FIM. EXIT.

       260-LISTAR.

           set CWEXEC-NOWARNING  to true
           set CWEXEC-ASSYNCRONE to true

           IF   LB-PRINTS = SPACES
                MOVE 1 TO VEZ
           END-IF

           IF   (9999 = P-I AND P-F)
                MOVE 0 TO LISTE
                GO TO 260-99-FIM
           ELSE
                MOVE 1 TO LISTE
                IF   VEZ = 1
                     PERFORM 340-SET-PRINTER THRU 340-99-FIM
                     MOVE 2 TO VEZ
                     MOVE 255 TO PRINTER-NO
                     IF IMP-0 MOVE 0 TO PRINTER-NO END-IF
                     IF IMP-1 MOVE 1 TO PRINTER-NO END-IF
                     IF IMP-2 MOVE 2 TO PRINTER-NO END-IF
                END-IF
           END-IF
      *
      *    IF   SPOOL-DEV = "$"
      *    AND (P-I NOT = 1
      *    OR   P-F NOT = CWDIRS-FOLHAS)
      *    AND  VER
      *         MOVE SPACES                         TO CWSEND-SCREENS
      *         MOVE "ImpressÆo parcial imposs¡vel" TO CWSEND-MSG
      *         CALL "CWSEND" USING PARAMETROS-CWSEND
      *         GO TO 260-99-FIM
      *    END-IF

           IF   LISTE  = 0
                GO TO 260-99-FIM.

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

           MOVE SPACES TO EXTW NEW

           IF   SPOOL-DEV = "$"
      *         COMPUTE VIA = VIAS - 1
                MOVE "sp000000.txt" TO PRINTS-WORK
                MOVE CWSPLTXT       TO PRINTS-WORK (10: 3)
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
           MOVE ALL "." TO PERC-OBS
           MOVE SPACES TO PERC-TIT
           STRING "Imprimindo em " IMPRESSORA
                  DELIMITED BY SIZE INTO PERC-TIT
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
                   EXEC COBOLware Pack String OBS-PRINTS END-EXEC
                   MOVE OBS-PRINTS TO PERC-OBS
                   MOVE 0          TO SALTO
                   COMPUTE PAGINA-LIVRE = P-I - 1
                   COMPUTE FAIXA = P-F - P-I + 1
                   MOVE 0    TO FAIXA-P
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
                     IF   width (1:1) = '9'
                          EXIT PERFORM
                     ELSE
                          PERFORM 265-GRAVA-LINHA THRU 265-99-FIM
                     END-IF
                     ADD   1          TO LP
                     IF  LP (8: 2) = "00"
                         EXEC COBOLware PROCESS (SHOW)
                              LINE 10 COLUMN 10
                              HEADER PERC-TIT
                              EMPTY C
                              FULL  RK-PRNTER
                              MESSAGE PERC-OBS
                              QUESTION "Interromper impressÆo ?"
                              CANCEL PERC-OPT;PERC-OPT
                         END-EXEC
                         MOVE CORRENTE TO SP2-ND-NAME
                         CALL SP2   USING SP2-ACTIVATE-WINDOW
                                          SP2-NAME-DEF
                     END-IF
                     MOVE "N" TO RESPOSTA
                     IF   PERC-OPT = "Y"
                          MOVE "S" TO RESPOSTA
                          PERFORM 300-INTERROMPE THRU 300-99-FIM
                     END-IF
                     IF   RESPOSTA = "S" OR "s"
                          COMPUTE RK-PRNTER = C + 1
                          EXIT PERFORM
                     END-IF
                   END-PERFORM
           END-PERFORM

           EXEC COBOLware PROCESS CLOSE END-EXEC
           MOVE CORRENTE TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF

           IF   testa-fim = 9
                COMPUTE RK-PRNTER = C + 1
                MOVE SALVA-RT TO testa-fim.

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
                      UNTIL LB-PRINTS (S4:1) NOT = SPACE
                         OR S4 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S2
                      FROM LENGTH LB-PRNTER BY -1
                      UNTIL LB-PRNTER (S2:1) NOT = SPACE
                         OR S2 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S1
                      FROM 1 BY 1 UNTIL S1 > S4
                   ADD 1 TO S3
                   EVALUATE LB-PRINTS (S1:1)
                       WHEN "$"
                            MOVE LB-PRNTER TO SPOOL-JOB (S3: )
                            INSPECT SPOOL-JOB (S3: ) CONVERTING
                                         "/" TO "\"
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
                            MOVE LB-PRINTS (S1:1) TO SPOOL-JOB(S3: 1)
                   END-EVALUATE
           END-PERFORM

           IF  SPOOL-REMOTO NOT = SPACES
               IF   SPOOL-REMOTO(1:1) = '"'
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
                               IF WINDOWS-PRINTER NOT = SPACES
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
                        WRITE TEXTO-REG
                         FROM POSTER-BUFFER
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
           END-IF.

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
                       IF   NOT FORMATO-UNIX
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
                   AND  (NOT POSTER)
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
                            ADD 1 TO PAGINA-LIVRE FAIXA-P
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
                     IF  (NOT FORMATO-UNIX)
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
                                   MOVE testa-fim TO SALVA-RT
                                   MOVE 9         TO testa-fim
                                   MOVE "+"       TO CWDIRS-IMPRESSO
                                   MOVE "S"       TO RESPOSTA
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

       300-INTERROMPE.

           MOVE "+" TO CWDIRS-IMPRESSO
           IF   SPOOL-DEV = "$"
                MOVE "S" TO DELETE-PRINTER
           END-IF

           IF   EM-LINHA = "S"
           AND  RESPOSTA = "S" OR "s"
                MOVE CWDIRS-FOLHAS        TO RK-PRNTER
                MOVE    1                 TO SALTO
                PERFORM 265-GRAVA-LINHA THRU 265-99-FIM
           END-IF.

       300-99-FIM. EXIT.

       320-STOP-EXIT.
           CONTINUE.
       320-99-FIM.

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
                IF ((CWDIRS-WIDTH NOT NUMERIC)
                OR  (CWSPOOLHANDLER NOT = SPACES))
                AND  Y = 0
                     MOVE 1         TO Y
                     MOVE LB-PRNTER TO TEXTO2(Y)
                     ADD 1 TO ERROS
                     PERFORM 215-PUT-LINE THRU 215-99-FIM
                     MOVE 0 TO Y
                     CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
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
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "03" TO CWCONF-REG03
                   SET CWSQLC-START TO TRUE
                   SET CWSQLC-NOT-LESS TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM
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
              AND  CWCONF-ARQUIVO NOT = SPACES
              AND  CWCONF-ARQUIVO NOT = "<Default>"
              AND  CWCONF-ARQUIVO NOT = "<Spool>"
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
                     IF  ASC (1 1) = 92
                     AND ASC (1 2) = 97
                         MOVE  92 TO ASC (13 1)
                         MOVE 109 TO ASC (13 2)
                     END-IF
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

       400-SP2-TOPO.

           PERFORM 420-REMOVE-FIELDS THRU 420-99-FIM
           MOVE LOW-VALUES   TO SP2-MD-DATA
           MOVE 7            TO SP2-MD-OPTION-CNT
           MOVE "CWMEN9"     TO SP2-MD-NAME
           MOVE 1            TO SP2-MDO-ID   (1)
           MOVE "~Imprimir"  TO SP2-MDO-TEXT (1)
           MOVE 2            TO SP2-MDO-ID   (2)
           MOVE "~Ver"       TO SP2-MDO-TEXT (2)
           MOVE 3            TO SP2-MDO-ID   (3)
           MOVE "~Excluir"   TO SP2-MDO-TEXT (3)
           MOVE 4            TO SP2-MDO-ID   (4)
           MOVE "~Nota"      TO SP2-MDO-TEXT (4)
           MOVE 5            TO SP2-MDO-ID   (5)
           MOVE "e~Xportar"  TO SP2-MDO-TEXT (5)
           MOVE 6            TO SP2-MDO-ID   (6)
           MOVE "i~Mportar"  TO SP2-MDO-TEXT (6)
           MOVE 7            TO SP2-MDO-ID   (7)
           MOVE "~Sair"      TO SP2-MDO-TEXT (7)
           PERFORM ACENTOS THRU FIM-ACENTOS
                   VARYING M FROM 1 BY 1 UNTIL M > 7

           COMPUTE SP2-MD-VAR-LEN = SP2-MD-OPTION-CNT *
                  (SP2-MD-OPTN-LEN + SP2-MD-OPTC-LEN + SP2-MD-OPTV-LEN)
           CALL SP2   USING SP2-SET-MENU-DEF SP2-MENU-DEF

           MOVE LOW-VALUES            TO SP2-FD-DATA
                                         SP2-FD-VAR-LENS
           MOVE "t"                   TO SP2-FD-BOR-TYPE
           MOVE 10                    TO SP2-FD-HEIGHT
           MOVE "p"                   TO SP2-FD-CTRL-TYPE
           MOVE "y"                   TO SP2-FD-OUTPUT

           MOVE "~Comando"            TO SP2-FD-VAR-DATA
           MOVE "C"                   TO SP2-FD-MNEMONIC
           MOVE 8                     TO SP2-FD-VAR-LEN
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO COMANDO-ID

           MOVE "có~Digo"             TO SP2-FD-VAR-DATA
           MOVE "D"                   TO SP2-FD-MNEMONIC
           IF   COLUNA-SORT = 1
                IF   OPCAO-SORT = 1
                     MOVE SETA-DOWN   TO SP2-FD-VAR-DATA (9:)
                ELSE
                     MOVE SETA-UP     TO SP2-FD-VAR-DATA (9:)
                END-IF
           END-IF
           MOVE 8                     TO SP2-FD-VAR-LEN
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO CODIGO-ID

           MOVE "Tipo"                TO SP2-FD-VAR-DATA
           MOVE X"00"                 TO SP2-FD-MNEMONIC
           MOVE 5                     TO SP2-FD-VAR-LEN
           MOVE "g"                   TO SP2-FD-OUTPUT
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM

           MOVE "da~Ta"               TO SP2-FD-VAR-DATA
           MOVE "T"                   TO SP2-FD-MNEMONIC
           IF   NOT (COLUNA-SORT = 1 OR 3 OR 4 OR 5)
                IF   OPCAO-SORT = 1
                     MOVE SETA-DOWN TO SP2-FD-VAR-DATA (7:)
                ELSE
                     MOVE SETA-UP   TO SP2-FD-VAR-DATA (7:)
                END-IF
           END-IF
           MOVE 10                    TO SP2-FD-VAR-LEN
           MOVE "y"                   TO SP2-FD-OUTPUT
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO DATA-ID

           MOVE "Hora"                TO SP2-FD-VAR-DATA
           MOVE X"00"                 TO SP2-FD-MNEMONIC
           MOVE 6                     TO SP2-FD-VAR-LEN
           MOVE "g"                   TO SP2-FD-OUTPUT
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM

           MOVE "Pá~ginas"            TO SP2-FD-VAR-DATA
           MOVE "P"                   TO SP2-FD-MNEMONIC
           IF   COLUNA-SORT = 3
                IF   OPCAO-SORT = 1
                     MOVE SETA-DOWN TO SP2-FD-VAR-DATA (10:)
                ELSE
                     MOVE SETA-UP   TO SP2-FD-VAR-DATA (10:)
                END-IF
           END-IF
           MOVE 09                    TO SP2-FD-VAR-LEN
           MOVE "y"                   TO SP2-FD-OUTPUT
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO PAGINA-ID

           MOVE "~Nota"               TO SP2-FD-VAR-DATA
           MOVE "N"                   TO SP2-FD-MNEMONIC
           IF   COLUNA-SORT = 4
                IF   OPCAO-SORT = 1
                     MOVE SETA-DOWN TO SP2-FD-VAR-DATA (7:)
                ELSE
                     MOVE SETA-UP   TO SP2-FD-VAR-DATA (7:)
                END-IF
           END-IF
           MOVE 20                    TO SP2-FD-VAR-LEN
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO NOTA-ID

           MOVE "~Usuário"            TO SP2-FD-VAR-DATA
           MOVE "U"                   TO SP2-FD-MNEMONIC
           IF   COLUNA-SORT = 5
                IF   OPCAO-SORT = 1
                     MOVE SETA-DOWN TO SP2-FD-VAR-DATA (10:)
                ELSE
                     MOVE SETA-UP   TO SP2-FD-VAR-DATA (10:)
                END-IF
           END-IF
           IF   SIZE-REPKEY > JANSIZE
                MOVE 11               TO SP2-FD-VAR-LEN
           ELSE
                MOVE 13               TO SP2-FD-VAR-LEN
           END-IF
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           MOVE SP2-FD-ID             TO USUARIO-ID

           MOVE LOW-VALUES            TO SP2-FD-DATA
                                         SP2-FD-VAR-LENS
           IF   SIZE-REPKEY > JANSIZE
                MOVE JANSIZE          TO ALTURA
                MOVE 78               TO LARG
                MOVE 30               TO LEN-U
           ELSE
                MOVE SIZE-REPKEY      TO ALTURA
                MOVE 80               TO LARG
                MOVE 33               TO LEN-U
           END-IF

           MOVE "Comando Codigo Tipo     Data    Hora Paginas  Nota
      -         "            Usuario" TO CHARACTER-BUFFER
           MOVE 0       TO LS
           MOVE 10      TO SP2-FD-HEIGHT
ita            if resolution = 1
ita               ADD 1 TO SP2-FD-HEIGHT
ita            end-if
           MOVE FIELDS(J) TO SAVE-FIELDS
           MOVE x"00"     TO SP2-FD-BOR-TYPE
           PERFORM ALTURA TIMES
                   ADD  1         TO SP2-FD-ROW LS

                   MOVE 0         TO SP2-FD-COL
                   MOVE LARG      TO SP2-FD-INITIAL-LEN
                   MOVE X"01"     TO SP2-FD-COLR
                   MOVE SPOOL-COLOR TO SP2-FD-COLR
                   MOVE X"02"     TO SP2-FD-CUR-COLR FD-CUR-COLR (LS)
                   MOVE "p"       TO SP2-FD-OUTPUT   FD-OUTPUT   (LS)
                   MOVE X"00"     TO SP2-FD-CURS-SKIP
                                     SP2-FD-CTRL-TYPE
                   PERFORM 410-INSERT-FIELD THRU 410-99-FIM
                   MOVE SP2-FD-ID TO LS-ID (LS)
                   MOVE SP2-FD-PROG-OFF TO FD-PROG-OFF (LS)

                   MOVE 1         TO SP2-FD-COL
                   MOVE 7         TO SP2-FD-INITIAL-LEN
                   MOVE SPOOL-COLOR TO SP2-FD-COLR
                   MOVE X"00"     TO SP2-FD-CUR-COLR
                   MOVE "h"       TO SP2-FD-OUTPUT
                   MOVE "y"       TO SP2-FD-CURS-SKIP
                   MOVE "e"       TO SP2-FD-CTRL-TYPE
                   PERFORM 410-INSERT-FIELD THRU 410-99-FIM
                   MOVE SP2-FD-ID TO CM-ID (LS)
                   COMPUTE C-OFF (LS) = SP2-FD-PROG-OFF + 1

                   MOVE COL-NOTA  TO SP2-FD-COL
                   MOVE 20        TO SP2-FD-INITIAL-LEN
                   MOVE SPOOL-COLOR TO SP2-FD-COLR
                   MOVE X"00"     TO SP2-FD-CUR-COLR
                   MOVE "h"       TO SP2-FD-OUTPUT
                   MOVE "y"       TO SP2-FD-CURS-SKIP
                   MOVE "e"       TO SP2-FD-CTRL-TYPE
                   PERFORM 410-INSERT-FIELD THRU 410-99-FIM
                   COMPUTE N-OFF (LS) = SP2-FD-PROG-OFF + 1
                   MOVE SP2-FD-ID TO NT-ID (LS)
           END-PERFORM
           MOVE 0 TO LS

           IF   SIZE-REPKEY > JANSIZE
                PERFORM 405-V-BARR THRU 405-99-FIM
           ELSE
                MOVE    0            TO BARRV-ID (J)
           END-IF.

       400-99-FIM. EXIT.

       405-V-BARR.

           MOVE 20               TO SP2-FD-WIDTH
           MOVE "v"              TO SP2-FD-CTRL-TYPE
           MOVE "s"              TO SP2-FD-PROG-CTRL
           MOVE 5                TO SP2-FD-INITIAL-LEN
           MOVE 0                TO SP2-FD-ROW
           MOVE 78               TO SP2-FD-COL
           MOVE X"00"            TO SP2-FD-OUTPUT
                                    SP2-FD-CUR-COLR
                                    SP2-FD-COLR
           MOVE "t"              TO SP2-FD-BOR-TYPE
           COMPUTE SP2-FD-HEIGHT = (JANSIZE + 1) * 10
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           COMPUTE BARRV-OFF(J) = SP2-FD-PROG-OFF + 1
           MOVE SP2-FD-ID        TO BARRV-ID (J).

       405-99-FIM. EXIT.

       406-H-BARR.

           MOVE 780              TO SP2-FD-WIDTH
           MOVE "h"              TO SP2-FD-CTRL-TYPE
           MOVE "s"              TO SP2-FD-PROG-CTRL
           MOVE 5                TO SP2-FD-INITIAL-LEN
           MOVE 23               TO SP2-FD-ROW
           MOVE 00               TO SP2-FD-COL
           MOVE X"00"            TO SP2-FD-OUTPUT
                                    SP2-FD-CUR-COLR
                                    SP2-FD-COLR
           MOVE "t"              TO SP2-FD-BOR-TYPE
           MOVE 10               TO SP2-FD-HEIGHT
           PERFORM 410-INSERT-FIELD THRU 410-99-FIM
           COMPUTE BARRH-OFF = SP2-FD-PROG-OFF + 1
           MOVE SP2-FD-ID        TO BARRH-ID.

       406-99-FIM. EXIT.

       410-INSERT-FIELD.

           CALL "CWSPID"    USING SP2-FD-ID "FInsert"
           ADD  1              TO FIELDS (J)
           MOVE FIELDS (J)     TO F
           MOVE SP2-FD-ID      TO FD-ID (J F)
           MOVE FIELDS(J)      TO SP2-FD-FLD-NUM
                                  SP2-FD-TAB-NUM
           IF   SP2-FD-CTRL-TYPE = X"00"
           OR   SP2-FD-CTRL-TYPE = "e"
           OR   SP2-FD-CTRL-TYPE = "p"
                IF   SP2-FD-CTRL-TYPE = "p"
                     ADD 1               TO SP2-FD-VAR-LEN
                     COMPUTE SP2-FD-WIDTH = SP2-FD-VAR-LEN * 10
                     MOVE SP2-FD-VAR-LEN TO SP2-FD-INITIAL-LEN
                     MOVE 0              TO SP2-FD-PROG-OFF
                     MOVE SP2-KEY-F2     TO SP2-FD-HELP-KEY
KS                   MOVE -1             TO SP2-FD-FONT-ID
                ELSE
                     COMPUTE SP2-FD-WIDTH = SP2-FD-INITIAL-LEN * 10
                     MOVE 0           TO SP2-FD-HELP-KEY
                                         SP2-FD-VAR-LEN
                END-IF
           END-IF
           MOVE SP2-FD-INITIAL-LEN TO SP2-FD-MAX-LEN
                                      SP2-FD-PROG-LEN
           MOVE "l"            TO SP2-FD-JUSTIFY
           IF   SP2-FD-CTRL-TYPE = X"00"
           OR   SP2-FD-CTRL-TYPE = "e"
           OR   SP2-FD-CTRL-TYPE = "v"
           OR   SP2-FD-CTRL-TYPE = "h"
                MOVE X"00"          TO SP2-FD-JUSTIFY
                MOVE 0              TO SP2-FD-FONT-ID
                MOVE -1             TO SP2-FD-FONT-ID
w98   *         MOVE 4              TO SP2-FD-FONT-ID
                EVALUATE TRUE
                    WHEN SP2-FD-COL = COL-NOTA
                         COMPUTE SP2-FD-PROG-OFF = SIZE-FIELDS(J)
                                                - LEN-U
                    WHEN SP2-FD-CTRL-TYPE = "e"
                         ADD 1 TO SP2-FD-PROG-OFF
                    WHEN OTHER
                         MOVE SIZE-FIELDS(J)     TO SP2-FD-PROG-OFF
                         ADD  SP2-FD-INITIAL-LEN TO SIZE-FIELDS (J)
fone                     MOVE 1                  TO SP2-FD-FONT-ID
w98                      MOVE CWFONT-REFERENCE     TO SP2-FD-FONT-ID
                END-EVALUATE
           END-IF
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           IF   SP2-FD-CTRL-TYPE = "p"
                SUBTRACT 1 FROM SP2-FD-VAR-LEN
                ADD  SP2-FD-VAR-LEN TO SP2-FD-COL
           END-IF.

       410-99-FIM. EXIT.

       420-REMOVE-FIELDS.

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > JANSIZE
                   IF   SD-ID (J Y) NOT = 0
                        MOVE SD-ID (J Y) TO SP2-SD-ID
                        CALL "CWSPID" USING SP2-SD-ID "SDelete"
                        CALL SP2   USING SP2-DELETE-STATIC
                                         SP2-STATIC-DEF
                        MOVE 0 TO SD-ID (J Y)
                   END-IF
           END-PERFORM

           PERFORM UNTIL FIELDS (J) = 0
                   MOVE FIELDS (J) TO F
                   IF   FD-ID (J F) NOT = 0
                        MOVE FD-ID (J F) TO SP2-FD-ID
                        CALL "CWSPID" USING SP2-FD-ID "FDelete"
                        CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                        MOVE 0 TO FD-ID (J F)
                        SUBTRACT 1 FROM FIELDS (J)
                   END-IF
           END-PERFORM
           INITIALIZE OFF-SETS (J)
           MOVE HIGH-VALUES TO SP2-TELA
           CALL SP2   USING SP2-SET-PANEL-FIELDS FIELD-AREA(J)
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           CALL SP2   USING SP2-CLEAR-MENU SP2-NULL-PARM.

       420-99-FIM. EXIT.

       430-ENABLE-NOTAS.

           PERFORM VARYING P FROM 1 BY 1
                      UNTIL P > ALTURA
                   MOVE 0         TO SP2-FD-VAR-LEN
                   MOVE LS-ID (P) TO SP2-FD-ID
                   CALL SP2    USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   CALL SP2    USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   MOVE "g"       TO SP2-FD-OUTPUT
                   CALL SP2    USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   IF   LER-NOTAS = 1
                        MOVE NT-ID (P) TO SP2-FD-ID
                   ELSE
                        MOVE CM-ID (P) TO SP2-FD-ID
                   END-IF
                   CALL SP2    USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   CALL SP2    USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   MOVE X"00"     TO SP2-FD-OUTPUT
                   CALL SP2    USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-PERFORM.

       430-99-FIM. EXIT.

       431-DISABLE-NOTAS.

           IF   LER-NOTAS = 1
                MOVE REPKEY-CHAVE TO SALVA-REP
                MOVE 1            TO REPKEY-CHAVE
                START REPKEY KEY NOT < REPKEY-CHAVE
                PERFORM UNTIL FS-REPKEY > "09"
                        READ REPKEY NEXT RECORD
                        IF   FS-REPKEY < "10"
                        AND  REPKEY-NOVA = 1
                             IF   REPKEY-NOTA NOT = "="
                                  MOVE REPKEY-NOTA TO SAVE-NOTA
                             END-IF
                             PERFORM 432-NOTAS THRU 432-99-FIM
                        END-IF
                END-PERFORM
                MOVE SALVA-REP    TO REPKEY-CHAVE
                READ REPKEY
           END-IF
           PERFORM VARYING P FROM 1 BY 1
                      UNTIL P > ALTURA
                   MOVE 0         TO SP2-FD-VAR-LEN
                   MOVE LS-ID (P) TO SP2-FD-ID
                   CALL SP2    USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   CALL SP2    USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   MOVE "p"       TO SP2-FD-OUTPUT
                   CALL SP2    USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   IF   LER-NOTAS = 1
                        MOVE NT-ID (P) TO SP2-FD-ID
                   ELSE
                        MOVE CM-ID (P) TO SP2-FD-ID
                   END-IF
                   CALL SP2    USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                   CALL SP2    USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   MOVE "h"       TO SP2-FD-OUTPUT
                   CALL SP2    USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-PERFORM.

       431-99-FIM. EXIT.

       432-NOTAS.

           IF   SAVE-NOTA = "Gerando..."
           OR   "Terminado anormal"
           OR   "Interrompido"
                GO TO 432-99-FIM
           END-IF

           MOVE REPKEY-CW        TO CWDIRS-CW
           MOVE REPKEY-SEQUENCIA TO CWDIRS-NUMERO
           MOVE REPKEY-EXT    TO CWDIRS-EXT
           READ CWDIRS IGNORE LOCK KEY IS CWDIRS-SPOOL

           IF   CWDIRS-NOTA = "Gerando..."
           OR   "Terminado anormal"
           OR   "Interrompido"
                GO TO 432-99-FIM
           END-IF

           IF   SAVE-NOTA NOT = CWDIRS-NOTA
                MOVE SAVE-NOTA TO CWDIRS-NOTA
                IF   COLUNA-SORT = 4
                     MOVE 1 TO REORDENAR
                END-IF
                MOVE "*" TO REPKEY-COMANDO
                MOVE 0   TO REPKEY-NOVA
                REWRITE REPKEY-REG
                MOVE 1   TO RECARGA
                REWRITE CWDIRS-REG
           END-IF.

       432-99-FIM. EXIT.

       440-SAVE-NOTA.

           IF  CWDIRSS (P) NOT = SPACES
           AND LER-NOTAS = 1
               MOVE N-OFF (P) TO OFF-W (J) POS
               IF  FIELD-AREA(J) (POS: 20) NOT = NOTA (P)
                   MOVE FIELD-AREA(J) (POS: 20) TO NOTA (P)
                   MOVE REPKEY-CHAVE            TO SALVA-REP
                   MOVE CHAVE-REPKEY (P)        TO REPKEY-CHAVE
                   READ REPKEY
                   MOVE 1 TO REPKEY-NOVA
                   MOVE NOTA (P) TO REPKEY-NOTA
                   REWRITE REPKEY-REG
                   MOVE SALVA-REP TO REPKEY-CHAVE
                   READ REPKEY
               END-IF
           END-IF

           IF  CWDIRSS (P) NOT = SPACES
           AND LER-NOTAS = 2
               MOVE C-OFF (P) TO OFF-W(J)
               IF  FIELD-AREA(J) (OFF-W(J): 7) NOT = SPACES
                   MOVE FIELD-AREA(J) (OFF-W(J): 7) TO COMANDO (P)
                   MOVE P                     TO W
                   PERFORM 450-CRITICA-COMANDO THRU 450-99-FIM
                   MOVE W                     TO P
                   MOVE REPKEY-CHAVE          TO SALVA-REP
                   MOVE CHAVE-REPKEY (P)      TO REPKEY-CHAVE
                   READ REPKEY
                   MOVE COMANDO (P) TO REPKEY-COMANDO
                   REWRITE REPKEY-REG
                   MOVE SALVA-REP TO REPKEY-CHAVE
                   READ REPKEY
               END-IF
           END-IF.

       440-99-FIM. EXIT.

       450-CRITICA-COMANDO.

           INSPECT COMANDO (W) CONVERTING MINUSCULAS
                                       TO MAIUSCULAS
           IF ((NOTA (W) = "Gerando...")
           OR   USUARIO (W) = "Importando...")
           AND (COMANDO (W) NOT = SPACES)
               MOVE SPACES TO COMANDO (W)
           END-IF
           MOVE COMANDO (W) TO COMANDO-E
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
                MOVE COMANDO-E TO COMANDO (W)
           ELSE
                IF   NOT LISTAR
                     MOVE COMANDO-T TO COMANDO (W)
                END-IF
           END-IF.

       450-99-FIM. EXIT.

       500-OPEN-WINDOW.

           ADD  1          TO J TT
           DISPLAY "CWTITLE"  UPON ENVIRONMENT-NAME
           ACCEPT  TITULO-TT(TT) FROM ENVIRONMENT-VALUE
           MOVE LOW-VALUES TO SP2-PD-DATA SP2-WD-DATA
           MOVE MESTRE     TO SP2-PD-NAME SP2-WD-NAME
           CALL SP2     USING SP2-GET-PANEL-DEF  SP2-PANEL-DEF
           CALL SP2     USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF

           IF  SPOOL-COLOR NOT = X"00"
               MOVE SPOOL-COLOR TO SP2-WD-COLR
                                   SP2-PD-COLR
           ELSE
               MOVE X"F0" TO SPOOL-COLOR
           END-IF

           IF SPOOL-NUMERO = 0
              ADD 10 TO SP2-wd-ROW
      *       ADD 10 TO SP2-pd-ROW
           END-IF
*     *    ADD 3 TO SP2-WD-ROW SP2-PD-ROW
           EVALUATE RESOLUTION
               WHEN 1 ADD 15 TO SP2-WD-COL
               WHEN 3 ADD 15 TO SP2-WD-COL
           END-EVALUATE
           MOVE CORRENTE   TO SP2-PD-NAME SP2-WD-NAME
***********DISPLAY "CWWINDOW" UPON ENVIRONMENT-NAME
***********DISPLAY CORRENTE   UPON ENVIRONMENT-VALUE
           MOVE "d"              TO SP2-WD-BOR-TYPE
           IF  CORRENTE = "CWMEN9"
               MOVE "Relat¢rios dispon¡veis" TO SP2-WD-TITLE
<>             MOVE SPOOL-COLOR TO SP2-WD-COLR
mpe>                  ADD 1 TO SP2-WD-HEIGHT
mpe>                           SP2-PD-HEIGHT
ks    *        SUBTRACT 1 FROM SP2-WD-HEIGHT
           ELSE
mpe>             SUBTRACT 1 FROM SP2-WD-HEIGHT
mpe>                             SP2-PD-HEIGHT
               MOVE "Relat¢rio" TO SP2-WD-TITLE
               IF   CWDIRS-TITULO = ALL '0'
                    MOVE SPACES TO CWDIRS-TITULO
               END-IF
               IF   CWDIRS-TITULO NOT = SPACES
                    MOVE ":" TO SP2-WD-TITLE (10: 1)
                    MOVE CWDIRS-TITULO TO SP2-WD-TITLE (12:)
                    INSPECT SP2-WD-TITLE CONVERTING X"FF" TO " "
               END-IF
xx             IF SPOOL-NUMERO = 0
                  IF  CWLINES NOT = "25"
                      ADD 1 TO SP2-WD-HEIGHT
                               SP2-PD-HEIGHT
                  END-IF
               else
                   ADD 10 TO SP2-wd-ROW
xx             END-IF
               if resolution = 1
                  ADD 1 TO SP2-WD-HEIGHT
               end-if
           END-IF
           MOVE -2               TO SP2-WD-OWNR-ID
           MOVE X"11"            TO SP2-WD-MORE-OPTIONS
           MOVE X"10"            TO SP2-WD-MORE-OPTIONS
Agua       MOVE X"00"            TO SP2-WD-MORE-OPTIONS
Agua2      MOVE X"31"            TO SP2-WD-MORE-OPTIONS
Agua2      move 0 TO SP2-wd-ROW
           IF   CWLITS = "LOW"
                INSPECT SP2-WD-TITLE CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-WD-TITLE CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
           ELSE
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-WINDOWS
           END-IF
Rofer      IF AUTOV = 1
Rofer         add 1  to SP2-WD-HEIGHT
Rofer                   SP2-PD-HEIGHT
Rofer      END-IF
           CALL SP2     USING SP2-OPEN-WINDOW SP2-WINDOW-DEF
           DISPLAY "CWTITLE"  UPON ENVIRONMENT-NAME
           DISPLAY SP2-WD-TITLE UPON ENVIRONMENT-VALUE

      *    MOVE SP2-KEY-LEFT      TO SP2-PD-CTRL-KEY (01)
      *    MOVE SP2-KEY-RIGHT     TO SP2-PD-CTRL-KEY (02)
      *    MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (03)
      *    MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (04)
           perform teclas
CU    *    CALL "CWRESE"
Agua2      MOVE X"21"             TO SP2-PD-MORE-OPTIONS
           CALL SP2     USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF
           CALL SP2     USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       500-99-FIM. EXIT.

       600-CLOSE-WINDOW.

           DISPLAY "CWTITLE"  UPON ENVIRONMENT-NAME
           DISPLAY TITULO-TT(TT) UPON ENVIRONMENT-VALUE
           CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
***********DISPLAY "CWWINDOW" UPON ENVIRONMENT-NAME
***********DISPLAY MESTRE     UPON ENVIRONMENT-VALUE
           SUBTRACT 1 FROM TT J
           IF   CORRENTE = "CWMEN9X"
           AND  SPOOL-NUMERO = 0
                DISPLAY "CWTITLE"  UPON ENVIRONMENT-NAME
                DISPLAY TITULO-TT(TT) UPON ENVIRONMENT-VALUE
                CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           END-IF.

       600-99-FIM. EXIT.

       teclas.
           INITIALIZE SP2-PD-CTRL-KEYS
                      SP2-PD-LEFT
                      SP2-PD-RIGHT
                      SP2-PD-UP
                      SP2-PD-DOWN
                      SP2-PD-TAB
                      SP2-PD-BACKTAB
                      SP2-PD-BACKSPAC
                      SP2-PD-DELETE
                      SP2-PD-INSERT
                      SP2-PD-HOME
                      SP2-PD-END
                      SP2-PD-HOME-PAN
                      SP2-PD-END-PAN
           MOVE SP2-KEY-TIMEOUT    TO SP2-PD-CTRL-KEY (01)
           MOVE SP2-KEY-CTRL-PGDN  TO SP2-PD-CTRL-KEY (02)
           MOVE SP2-KEY-CTRL-PGUP  TO SP2-PD-CTRL-KEY (03)
           MOVE SP2-KEY-PGDN       TO SP2-PD-CTRL-KEY (04)
           MOVE SP2-KEY-PGUP       TO SP2-PD-CTRL-KEY (05)
           MOVE SP2-KEY-LEFT       TO SP2-PD-CTRL-KEY (06)
           MOVE SP2-KEY-RIGHT      TO SP2-PD-CTRL-KEY (07)
           MOVE SP2-KEY-UP         TO SP2-PD-CTRL-KEY (08)
           MOVE SP2-KEY-DOWN       TO SP2-PD-CTRL-KEY (09)
           MOVE SP2-KEY-TAB        TO SP2-PD-CTRL-KEY (10)
           MOVE SP2-KEY-BACKTAB    TO SP2-PD-CTRL-KEY (11)
           MOVE SP2-KEY-HOME       TO SP2-PD-CTRL-KEY (12)
           MOVE SP2-KEY-ENTER      TO SP2-PD-CTRL-KEY (13)
           MOVE SP2-KEY-ESC        TO SP2-PD-CTRL-KEY (14)
           MOVE SP2-KEY-END        TO SP2-PD-CTRL-KEY (15)
           MOVE SP2-KEY-CTRL-LEFT  TO SP2-PD-CTRL-KEY (16)
           MOVE SP2-KEY-CTRL-RIGHT TO SP2-PD-CTRL-KEY (17)
           MOVE SP2-KEY-CTRL-FIELD TO SP2-PD-CTRL-KEY (18)
           MOVE SP2-KEY-F2         TO SP2-PD-CTRL-KEY (19)
           MOVE SP2-KEY-CTRL-P     TO SP2-PD-CTRL-KEY (20).

       fim-teclas.

       ACENTOS.

           IF   CWLITS = "LOW"
                INSPECT SP2-MDO-TEXT (M)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-MDO-TEXT (M)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-MDO-TEXT (M)
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT SP2-MDO-TEXT (M)
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF.

       FIM-ACENTOS. EXIT.

       800-EVITA-BUG-SP2.

      *    IF SP2-CD-KEY > 0
           IF  (SP2-CD-KEY NOT = SP2-KEY-CTRL-FIELD)
           AND (SP2-CD-KEY NOT = SP2-KEY-SELECT)
mem           MOVE SP2-CD-WAIT-SW   TO WAIT-SW
mem           MOVE SP2-CD-KEY       TO CD-KEY
mem           MOVE SP2-CD-BUTTON-ID TO BT-KEY
mem           MOVE "k"              TO SP2-CD-WAIT-SW
mem           CALL SP2           USING SP2-GET-INPUT SP2-CONVERSE-DATA
mem           MOVE WAIT-SW          TO SP2-CD-WAIT-SW
mem           MOVE CD-KEY           TO SP2-CD-KEY
mem           MOVE BT-KEY           TO SP2-CD-BUTTON-ID.

       800-99-FIM. EXIT.

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

       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.
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

