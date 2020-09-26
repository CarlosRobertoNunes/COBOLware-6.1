       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN0.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/1987.
       SECURITY.      *************************************************
                      *                                               *
                      *   Menu geral                                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TEXTO ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-TEXTO.

      *    SELECT OPTIONAL BUG ASSIGN TO DISK
      *           ORGANIZATION  IS LINE SEQUENTIAL
      *           LOCK MODE     IS EXCLUSIVE.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT MENU ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS MENU-CHAVE
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-MENU.

       DATA DIVISION.
       FILE SECTION.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG PIC X(503).

      *FD  BUG
      *    VALUE OF FILE-ID IS "flexus.bug".
      *
      *01  BUG-REG.
      *    10 BUG-MDO-ID            PIC  9(004)-.
      *    10 BUG-MDO-OWNR-ID       PIC  9(004)-.
      *    10 BUG-MDO-TEXT          PIC  X(030).
      *    10 BUG-MDO-TYPE          PIC  X(001).
      *    10 BUG-MDO-ACC-KEY       PIC  9(004)-.

       FD  MENU
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-MENU.

       01  MENU-REG.
           05 MENU-CHAVE.
              10 MENU-TYPE             PIC  X(001).
              10 MENU-ID        COMP-5 PIC S9(004).
           05 MENU-NIVEL               PIC  9(001).
           05 MENU-CHECK               PIC  X(001).
           05 MENU-PROG                PIC  X(008).
           05 MENU-PASS                PIC  X(006).
           05 MENU-SIZE-S              PIC  9(002).
           05 MENU-HELP                PIC  X(020).
           05 MENU-NO-OPCAO            PIC  9(002).
           05 MENU-NM-OPCAO            PIC  X(150).
           05 MENU-FUNCAO-PROGRAMA     PIC  X(034).
           05 MENU-PATH.
              10 MENU-N       OCCURS 5 PIC  X(034).
           05 MENU-BMP                 PIC  X(034).
           05 MENU-PANO                PIC  X(034).
           05 MENU-PAGINA              PIC  9(004).
           05 MENU-TIPO-REG            PIC  X(002).
           05 MENU-PAGINA-OP           PIC  9(004).
           05 MENU-ID2          COMP-5 PIC S9(004).

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 NIVEL-LOGON              PIC  X(001) VALUE SPACES.
           05 ER-TEXTO.
              10 FS-TEXTO              PIC  X(002) VALUE "00".
              10 LB-TEXTO              PIC  X(255) VALUE SPACES.
           05 TIT-WORK                 PIC  X(050) VALUE SPACES.
           05 CWMENUPAGE               PIC  X(003) VALUE SPACES.
           05 IZ                       PIC  9(002) VALUE 0.
           05 CWPROC                   PIC  X(002) VALUE SPACES.
           05 CWCOMMAREA               PIC  X(010) VALUE SPACES.
           05 CWLOGPGM                 PIC  X(010) VALUE SPACES.
           05 FCW                      PIC  X(008) VALUE SPACES.
           05 LB-FSINI                 PIC  X(255) VALUE SPACES.
           05 INISHOW                  PIC  X(003) VALUE SPACES.
           05 IMAGE                 PIC  X(050) VALUE SPACES.
           05 TIPO                  PIC  X(003) VALUE SPACES.
           05 EXTW                     PIC  X(004) VALUE SPACES.
              88 EXTW-OK VALUE 'JPG' 'BMP' 'GIF'.
           05 BUFFER                   PIC  X(170) VALUE SPACES.
           05 CWPGID-NM-OPCAO          PIC  X(034) VALUE SPACES.
           05 SHOWCOMENT               PIC  X(002) VALUE SPACES.
           05 COMPUTERNAME             PIC  X(030) VALUE SPACES.
           05 OBS                      PIC  X(035) VALUE SPACES.
           05 CWAUTOSPOOL              PIC  X(003) VALUE SPACE.
           05 OPTS                     PIC  X(018) VALUE SPACES.
           05 ESQUECI                  PIC  X(060) VALUE SPACES.
           05 FSSERVER                 PIC  X(050) VALUE SPACES.
           05 OLD-CWCONF               PIC  X(255) VALUE SPACES.
           05 NEW-CWCONF               PIC  X(255) VALUE SPACES.
           05 CWMENU-ACTIVE            PIC  X(003) VALUE SPACES.
           05 CWFULLMODE               PIC  X(004) VALUE SPACES.
           05 CWLOGP                   PIC  X(008) VALUE SPACES.
           05 CWLG                     PIC  X(003) VALUE SPACES.
           05 NUMERO                   PIC  9(018) VALUE 0.
           05 Z                        PIC  9(018) VALUE 0.
           05 CWNUMERO                 PIC  X(018) VALUE SPACES.
           05 E34                      PIC  9(004) VALUE 0.
           05 II                       PIC  9(004) VALUE 0.
           05 LIMPA                    PIC  X(034) VALUE SPACES.
           05 L                        PIC  9(002) VALUE 0.
           05 NX                       PIC  9(002) VALUE 0.
           05 PGID                     PIC  X(001) VALUE '0'.
           05 CWSAVE                   PIC  X(003) VALUE SPACES.
           05 CWLITS                   PIC  X(003) VALUE SPACES.
           05 CWACCENT                 PIC  X(003) VALUE SPACES.
           05 LEN-LEMBRETE             PIC  9(002) VALUE 0.
           05 ESQUECI-RESPOSTA         PIC  X(030) VALUE SPACES.
           05 LOGON-LINE               PIC  9(002) VALUE 11.
           05 LOGON-COLUMN             PIC  9(002) VALUE 20.
           05 LOGON-COLOR              PIC  9(003) VALUE 62.
           05 MODULO                   PIC  X(025) VALUE SPACES.
           05 PAGINA-OP                PIC  9(004) VALUE 0.
           05 PANOS-SAVE               PIC  X(160) VALUE SPACES.
           05 PANOS.
              10 PANO-COBOL            PIC  X(080) VALUE SPACES.
              10 PANO-COBOL2           PIC  X(080) VALUE SPACES.
           05 CWMENU-HELP              PIC  X(030) VALUE SPACES.
           05 FECHAR                   PIC  9(001) VALUE 0.
           05 SAVE-PAGINAX             PIC  X(006) VALUE SPACES.
           05 BM                       PIC  9(002) VALUE 0.
           05 BM-ID OCCURS 26   COMP-5 PIC S9(004).
           05 POS-BM.
              10 BM-LINE               PIC  9(002).
              10                       PIC  X(001).
              10 BM-COLUMN             PIC  9(002).
              10                       PIC  X(001).
              10 BM-HEIGHT             PIC  9(002).
              10                       PIC  X(001).
              10 BM-WIDTH              PIC  9(002).
           05 PUSH-BUTTONS                         VALUE SPACES.
              10 PB OCCURS 26          PIC  X(255).
              10 PN OCCURS 26          PIC  X(034).
           05 SARBANE                  PIC  X(003) VALUE SPACES.
           05 TRADUTOR                 PIC  X(030) VALUE SPACES.
           05 HOJE                     PIC  9(008) VALUE 0.
           05 DIAS-SENHA               PIC  9(007) VALUE 0.
           05 EXPIRE                   PIC  9(003) VALUE 0.
           05 EXPIRADA                 PIC  9(001) VALUE 0.
           05 N                        PIC  9(002) VALUE 0.
           05 N2                       PIC  9(002) VALUE 0.
           05 CA-POSI                  PIC  9(004) VALUE 0.
           05 CA-SIZE                  PIC  9(002) VALUE 0.
           05 WS-TEXTO                 PIC  X(034) VALUE SPACES.
           05 SENHA-PROV               PIC  9(006) VALUE 0.
           05 MAXUSERS                 PIC  9(006) VALUE 0.
           05 ENTROU                   PIC  9(001) VALUE 0.
           05 RETRY-CWCONF             PIC  9(001) VALUE 0.
           05 MSG-AT.
              10 MSG-AT1               PIC  X(040) VALUE SPACES.
              10 MSG-AT2               PIC  X(040) VALUE SPACES.
              10 MSG-AT3               PIC  X(040) VALUE SPACES.
           05 CWFONE                   PIC  X(043) VALUE SPACES.
           05 PFONE                                VALUE "142800".
              10                       PIC  X(004).
              10 F                     PIC  9(002).
           05 ESPACOS                  PIC  X(078) VALUE SPACES.
           05 B1                       PIC  X(078) VALUE ALL X"B1".
           05 LICENCA-2                PIC  9(009) VALUE 0.
           05 NOFRAME                  PIC  9(001) VALUE 0.
           05 CWLOGO                   PIC  X(011) VALUE SPACES.
           05 SAVE-PROGRAMA            PIC  X(008) VALUE SPACES.
           05 COL-W                    PIC S9(003) VALUE 0.
           05 LOGIN-FLAG               PIC  9(001) VALUE 0.
           05 TTY                      PIC  X(010) VALUE SPACES.
           05 TTY-NOME                 PIC  X(029) VALUE SPACES.
           05 LICENCA                  PIC  9(006) VALUE 0.
           05 TPI                      PIC  9(001) VALUE 0.
           05 CWMENU-ENV               PIC  X(011) VALUE SPACES.
              88 CWMENU-OLD                        VALUE "OLD".
           05 REVISAO                  PIC  9(002)V99 VALUE 0.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 FLAG-OPCOES              PIC  X(001) VALUE SPACE.
           05 OPCOES-VALIDAS           PIC  9(004) VALUE 0.
           05 ULTIMA-PAGINA            PIC  9(004) VALUE 9999.
           05 TESTE-CWLINE             PIC  X(001) VALUE "N".
           05 WS-OPCOES                 VALUE SPACES.
              10 WS-OPCAO OCCURS 26.
                 15 WS-NM-OPCAO        PIC  X(034).
                 15 WS-OPCAO-CHAR      PIC  X(001).
           05 LINHA-COMANDO.
              10 BYTE-L OCCURS 200     PIC  X(001).
           05 MAIOR                    PIC  9(002) VALUE 0.
           05 TAMANHO                  PIC  9(002) VALUE 0.
           05 TAMANHO2                 PIC  9(002) VALUE 0.
           05 WS-TAMANHO-OPCAO         PIC  9(002) OCCURS 26.
           05 NOME-C                   PIC  X(030) VALUE SPACES.
           05 WS-POSITION              PIC  9(004) VALUE 0.
           05 SENHA-C                  PIC  X(030) VALUE SPACES.
           05 I-NOME                   PIC  9(002) VALUE 0.
           05 I-SENHA                  PIC  9(002) VALUE 0.
           05 SET-SENHA                PIC  9(001) VALUE 0.
           05 SET-NOME                 PIC  9(001) VALUE 0.
           05 LINES-CWBOXS             PIC  9(002) VALUE 0.
           05 COLUMNS-CWBOXS           PIC  9(002) VALUE 0.
           05 TESTE-CWBOXS.
              10 BYTE-CWBOXS           PIC  X(001) OCCURS 34.
           05 ARROW                    PIC  X(001) VALUE SPACE.
           05 ARROW2                   PIC  X(001) VALUE SPACE.
           05 CHAVE-ATIVACAO           PIC  9(006) VALUE 0.
           05 OP-POP                   PIC  9(002) VALUE 0.
           05 N-POP                    PIC  9(002) VALUE 0.
      *    05 POPCOR.
      *       10 CORR-POP OCCURS 26    PIC  9(002).
           05 M-O                      PIC  9(001) VALUE 1.
           05 VEZ-CWCONF               PIC  9(001) VALUE 1.
           05 VALIDADE-ATIVA           PIC  9(008) VALUE 0.
           05 ESTE-LOGIN.
              10 VALIDADE              PIC  9(008) VALUE 0.
              10 LOGIN-HORA            PIC  9(006) VALUE 0.
           05 ULTIMO-LOGIN.
              10 ULTIMO-LOGIN-DATA     PIC  9(008) VALUE 0.
              10 ULTIMO-LOGIN-HORA     PIC  9(006) VALUE 0.
           05 TOLERANCIA.
              10 TOLERANCIA-DATA       PIC  9(008) VALUE 0.
              10 TOLERANCIA-HORA.
                 15 TOLERANCIA-HH      PIC  9(002) VALUE 0.
                 15 TOLERANCIA-MM      PIC  9(002) VALUE 0.
                 15 TOLERANCIA-SS      PIC  9(002) VALUE 0.
           05 VALIDADE2                PIC  9(008) VALUE 0.
           05 REDEFINES VALIDADE2.
              10 VALIDADE2-AA          PIC  9(004).
              10 VALIDADE2-MM          PIC  9(002).
              10 VALIDADE2-DD          PIC  9(002).
           05 EJECT-MODE-OLD           PIC  X(002) VALUE SPACES.
           05 SENHA-ESPECIAL           PIC  X(001) VALUE SPACE.
           05 TP                       PIC  X(001) VALUE SPACE.
           05 IP                       PIC  9(002) VALUE 0.
           05 YP                       PIC  9(002) VALUE 0.
           05 F54                      PIC  9(002) VALUE 0.
           05 HELP-PATH.
              10 FILLER                PIC  X(001) VALUE SPACES.
              10 HELP-PATH-99          PIC  X(099) VALUE SPACES.
      *    05 SIZE-SCREEN              PIC  9(004) VALUE 2000.
           05 ATTRIBUTE-A7             PIC  9(002) COMP-X VALUE 00.
           05 FUNCTION-A7              PIC  9(002) COMP-X VALUE 00.
           05 PARAMETER-A7             PIC  9(002) COMP-X VALUE 00.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.
           05 TESTECHAR                PIC  X(001) VALUE SPACE.
           05 PRINTER-NO               PIC  9(002) COMP-X VALUE 0.
           05 PRINTER-STATUS           PIC  9(002) COMP-X VALUE 0.
           05 PE                       PIC  9(003)        VALUE 0.
           05 OP                       PIC  Z(002)        VALUE 0.
           05 CX                       PIC  9(002) COMP-X VALUE 0.
           05 LX                       PIC  9(002) COMP-X VALUE 0.
           05 VEZ                      PIC  9(001)        VALUE 0.
           05 VEZ-LOAD                 PIC  9(001)        VALUE 0.
           05 HHMMSSDD.
              10 FILLER                PIC X(004).
              10 SS                    PIC 9(002).
              10 DD                    PIC 9(002).
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 SALVA-PAGINA             PIC  9(004) VALUE ZERO.
           05 PAGINAX                  PIC  9(004) VALUE ZERO.
           05 TESTE-OPCAO              PIC  9(003) VALUE ZERO.
           05 SPOOL                    PIC  X(006) VALUE "CWMEN9".
           05 SAVE-TASK                PIC  9(006) VALUE ZERO.
           05 E                        PIC  9(004) VALUE ZERO.
           05 I                        PIC  9(004) VALUE ZERO.
           05 I2                       PIC  9(004) VALUE ZERO.
           05 Y                        PIC  9(004) VALUE ZERO.
           05 X                        PIC  9(004) VALUE ZERO.
           05 K                        PIC  9(004) VALUE ZERO.
           05 Y2                       PIC  9(004) VALUE ZERO.
           05 QUADRO-L                 PIC  9(001) VALUE ZERO.
           05 QUADRO-U                 PIC  9(001) VALUE ZERO.
           05 FL-EXIT                  PIC  9(001) VALUE ZERO.
              88 FINALIZAR                         VALUE 1.
           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 MOLDURA                              VALUE SPACES.
              10 M-201                 PIC  X(001).
              10 M-205                 PIC  X(001).
              10 M-187                 PIC  X(001).
              10 M-186                 PIC  X(001).
              10 M-204                 PIC  X(001).
              10 M-185                 PIC  X(001).
              10 M-200                 PIC  X(001).
              10 M-188                 PIC  X(001).
           05 PROG2                    PIC  X(014) VALUE SPACES.
           05 HORIZONTAL               PIC  X(078) VALUE SPACES.
           05 TE                       PIC  9(002) VALUE ZERO.
           05 TESTE-ESPACO                         VALUE ZEROS.
              10 TEVE-ESPACO OCCURS 3  PIC  9(001).
           05 SALTO                    PIC  X(001) VALUE X"0C".
           05 CHECK-PASS               PIC  X(006) VALUE SPACES.
           05 CHECK-PASS-3                         VALUE SPACES.
              10 SENHA-X3 OCCURS 6 PIC 9(2) COMP-X.
           05 U                        PIC  9(003) VALUE ZERO.
           05 CRITICA-SENHA.
              10 SENHA-AUTO                           VALUE SPACES.
                 15 SENHA-X  OCCURS 30 PIC 9(2) COMP-X.
           05 CHECK-NOME               PIC  X(001) VALUE "N".
              88 NOME-OK                           VALUE "S".
           05 RESPOSTA                 PIC  X(001) VALUE "S".
              88 EXECUTAR                          VALUE "S" "s".
           05 LABEL-WORK.
              10 BYTE-LW      OCCURS 8 PIC X(001).
           05 ER-PRNTER.
              10 FS-PRNTER             PIC  X(002) VALUE "00".
              10 LB-PRNTER                         VALUE "PRN".
                 15 IMPR               PIC  X(008).
                 15 FILLER             PIC  X(247).
           05 MSG-E PIC X(30) VALUE "Sistema inoperante".
           05 IGNORA                   PIC  X(065) VALUE "Ignora...".
           05 CWPAGE-TXT               PIC  X(034) VALUE SPACES.
           05 FUNCAO-PROGRAMA          PIC  X(034) VALUE
              "Menu geral".
           05 FUNCAO-NOVA              PIC  X(034) VALUE SPACES.
           05 MENU-GERAL               PIC  X(034) VALUE "Menu geral".
           05 ER-MENU.
              10 FS-MENU               PIC  X(002) VALUE "00".
              10 LB-MENU               PIC  X(255) VALUE "$TEMP/cwmenu".
           05 FS-CWLOGF                PIC  X(002) VALUE "73".
           05 USUARIO                  PIC  X(060) VALUE SPACES.
           05 SISTEMA                  PIC  X(030) VALUE SPACES.
           05 APLICACAO                PIC  X(030) VALUE SPACES.
           05 SISTEMA-WK               PIC  X(030) VALUE SPACES.
           05 USUARIO-P                PIC  X(030) VALUE SPACES.
           05 SISTEMA-P                PIC  X(030) VALUE SPACES.
           05 DATA-DE-HOJE             PIC  X(010) VALUE SPACES.
           05 HORA                     PIC  X(008) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE SPACE.
           05 OPCAO                    PIC  9(002) VALUE ZERO.
           05 MAPA-MOUSE.
              10 PIC X(36) VALUE "080309031003110312031303140315031603".
              10 PIC X(36) VALUE "170318031903200308410941104111411241".
              10 PIC X(32) VALUE "13411441154116411741184119412041".
           05 REDEFINES MAPA-MOUSE.
              10 LC-MOUSE OCCURS 26.
                 15 LIN-MOUSE PIC 99.
                 15 COL-MOUSE PIC 99.
           05 OLD-DRIVE      PIC X(001) VALUE SPACE.
           05 OLD-DIRECTORY  PIC X(255) VALUE SPACES.
           05 SIZE-OLD-DIR   PIC 9(002) COMP-X VALUE 255.
           05 NEW-DRIVE      PIC X(001) VALUE SPACE.
           05 NEW-DIRECTORY  PIC X(255) VALUE SPACES.
           05 YEAR-2000      PIC 9(001) VALUE ZERO.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
              10 STRING-START         PIC  9(004) COMP-X VALUE 1.
           05 SUB-CWCONF.
              10 SM-ATT               PIC  9(004) OCCURS 7.
           05 SUB-CWCONF-2            PIC  X(028) VALUE SPACES.
           05 SM-FIL                  PIC  9(001) VALUE 0.
           05 ERRO-DRIVE     PIC X(020) VALUE "Drive X: n∆o existe".
           05 ERRO-DIRECTORY PIC X(070) VALUE "Pasta inacess°vel:".
           05 OPCAO-2                  PIC  9(002) VALUE ZERO.
              88 MUDA-NADA       VALUE 00.
              88 MUDA-EMPRESA    VALUE 02.
              88 MUDA-SENHA      VALUE 09.
              88 MUDA-SISTEMA    VALUE 10.
              88 MUDA-ROTINAS    VALUE 11.
           05 CONTROL-COLORS            PIC X(44) VALUE
              "BACKGROUND-COLOR 4,FOREGROUND-COLOR 6, HIGH".
           05 F9-DATA.
              10 F9-TEXTO   PIC X(30) VALUE SPACES.
              10 F9-PROGRAM PIC X(08) VALUE SPACES.
           05 TECLA         PIC 9(003) VALUE 0. COPY CWKEYS.
           05 TECLA-EDIT    PIC 9(003) VALUE 0. COPY CWEDIT.
           05 MIL    COMP-X PIC 9(008) VALUE 1000.
           05 XSDRUN                   PIC  X(007) VALUE SPACES.
           05 CWMENU.
              10 CWMENU-END            PIC  X(008) VALUE SPACES.
              10 CWMENU-AREA01 POINTER.
              10 CWMENU-AREA02 POINTER.
              10 CWMENU-AREA03 POINTER.
              10 CWMENU-AREA04 POINTER.
              10 CWMENU-AREA05 POINTER.
              10 CWMENU-AREA06 POINTER.
              10 CWMENU-AREA07 POINTER.
              10 CWMENU-AREA08 POINTER.
              10 CWMENU-AREA09 POINTER.
              10 CWMENU-AREA10 POINTER.

       01  AREAS-DE-TRABALHO-2.
           05 OVERLAYS.
              10 PIC X(019) VALUE "CWMEN15~Arquivos   ".
              10 PIC X(019) VALUE "      6~Empresa    ".
              10 PIC X(019) VALUE "CWMENA5~Grupos     ".
              10 PIC X(019) VALUE "CWMENB4~Impressoras".
              10 PIC X(019) VALUE "CWMEN5@mensage~Ns  ".
              10 PIC X(019) VALUE "CWMEN66~Programas  ".
              10 PIC X(019) VALUE "CWMENI6~Relator    ".
              10 PIC X(019) VALUE "CWMEN73re~Lat¢rios ".
              10 PIC X(019) VALUE "      0~Senha      ".
              10 PIC X(019) VALUE "      6sis~Tema    ".
              10 PIC X(019) VALUE "CWMEN2@su~Brotinas ".
              10 PIC X(019) VALUE "CWMEN36sup~Orte    ".
              10 PIC X(019) VALUE "CWMEN81~Usu†rios   ".
           05 REDEFINES OVERLAYS.
              10 OVERLAY-15 OCCURS 13.
                 15 OVERLAY  PIC X(006).
                 15 NIVEL    PIC X(001).
                 15 TIPO-CF  PIC X(012).
           05 OVERLAYS2.
              10 PIC X(017) VALUE "Arquivos ".
              10 PIC X(017) VALUE "Empresa ".
              10 PIC X(017) VALUE "Grupos ".
              10 PIC X(017) VALUE "Impressoras".
              10 PIC X(017) VALUE "Mensagens ".
              10 PIC X(017) VALUE "Programas ".
              10 PIC X(017) VALUE "Relator".
              10 PIC X(017) VALUE "Relat¢rios ".
              10 PIC X(017) VALUE "Senha ".
              10 PIC X(017) VALUE "Sistema ".
              10 PIC X(017) VALUE "Subrotinas ".
              10 PIC X(017) VALUE "Suporte ".
              10 PIC X(017) VALUE "Usu†rios ".
           05 REDEFINES OVERLAYS2.
              10 OVERLAY-15-2 OCCURS 13.
                 15 TIPO-CF2 PIC X(017).
           05 OPERADOR.
              10 PIC X(023) VALUE "0318~HELP      [F1]".
              10 PIC X(023) VALUE "0319~Spool     [F5]".
              10 PIC X(023) VALUE "@321~Usu†rios  [F7]".
              10 PIC X(023) VALUE "@322~Validade   [F8]".
              10 PIC X(023) VALUE "0323~Form feed     [F9]".
      *       10 PIC X(023) VALUE "@324~Licenáa  [F10]".
           05 REDEFINES OPERADOR.
              10 OCCURS 5.
                 15 TIPO-NV PIC 9(001).
                 15 TIPO-FX PIC 9(003).
                 15 TIPO-OP PIC X(019).
           05 ERRO-CWCONF   PIC X(030) VALUE "Sistema n∆o configurado".

       01  AREAS-DE-TRABALHO-GUI.
           05 CWTITLE                  PIC  X(200) VALUE SPACES.
           05 OPT                      PIC  X(001) VALUE SPACE.
           05 SEGUNDOS          COMP-X PIC  9(002) VALUE 0.
           05 PROGEMP                  PIC  X(008) VALUE SPACES.
           05 LENSENHA                 PIC  9(002) VALUE 0.
           05 MSG                      PIC  X(070) VALUE SPACES.
           05 CRT-STATUS               PIC  X(003) VALUE SPACES.
           05 X91-SRCOUT-RESULT        PIC  9(002) COMP-X.
           05 X91-SRCOUT-FUNCTION      PIC  9(002) COMP-X VALUE 47.
           05 FATOR-W           COMP-X PIC  9(002) VALUE 0.
           05 VAR-LOW VALUE SPACES.
              10 NOME-LOW              PIC  X(030).
              10 PROG-LOW              PIC  X(008).
           05 PRE-F3                   PIC  9(004) VALUE 0.
           05 POS-F3                   PIC  9(004) VALUE 0.
           05 MENU-EXTRA               PIC S9(004) COMP-5 VALUE 0.
           05 WS-TITLE                 PIC  X(150) VALUE SPACES.
           05 HELP-ID                  PIC S9(004) COMP-5 VALUE +0.
           05 WS-PANEL-ID              PIC S9(004) COMP-5 VALUE +0.
           05 TESTECHAR2               PIC  X(001) VALUE SPACE.
           05 VEZ-GUI                  PIC  9(002) VALUE 0.
           05 GUI-ON                   PIC  X(002) VALUE SPACE.
           05 FP                       PIC  9(002) VALUE 0.
           05 FP2                      PIC  9(002) VALUE 0.
           05 FP3                      PIC  9(002) VALUE 0.
           05 SALVAS VALUE SPACES.
              10 SALVA-CHAVE-GUI       PIC  X(032) OCCURS 7.
              10 I-SAVE                PIC  9(004) OCCURS 7.
              10 PAI            COMP-5 PIC S9(004) OCCURS 7.
              10 NM-OPCAO              PIC  X(034) OCCURS 7.

       COPY CWSTAT.
       COPY CWBOXS.
       COPY CWLINE.
       COPY CWACTV.
       COPY CWSEND.
       COPY CWBOXW.
       COPY CWTIME.
       COPY CWGETL.
       COPY CWUNIX.
       COPY CWLOGD.
       COPY CWSPWS.
       COPY CWNCOR.
       COPY CWREVS.
       COPY CWCONF.
       COPY CWLOGW.

       LINKAGE SECTION.

       01  NOME                     PIC  X(030).
       01  CHECK-NIVEL              PIC  9(001).
       01  TASK                     PIC  9(006).
       01  GRUPO                    PIC  X(022).
       01  PROGRAMA                 PIC  X(008).
       01  REDEFINES PROGRAMA.
           05 PK-X OCCURS 8 COMP-X  PIC  9(002).
       01  PAGINA                   PIC  9(004).
       01  MODO-MENU                PIC  9(001).
       01  CFG                      PIC  9(002).
       01  MAPA.
           05 SAVE-CWLINE-OPTION    PIC  9(003).
           05 LOGON-TYPE            PIC  9(001).
           05 NIVEL-ATUAL           PIC  9(001).
           05 SISTEMA-SM            PIC  X(030).
           05                       PIC X(24100).
           05 HELP                  PIC  9(001).
           05 SENHA                 PIC  X(030).
           05 CHECK-SENHA           PIC  X(001).
              88 SENHA-OK                       VALUE "S".

       01  IMPRESSORA               PIC  X(008).
           88 OLD-IMPRESSORA VALUE
                              "PRN" "PRN." "PRN:" "LPT1" "LPT2" "LPT3"
                              "COM1" "COM2".
       01  QUADRO                   PIC  9(002).

       01  RELATORIO                PIC  X(007).

       01  COMMAREA01               PIC X(2000).
       01  COMMAREA02               PIC X(2000).
       01  COMMAREA03               PIC X(2000).
       01  COMMAREA04               PIC X(2000).
       01  COMMAREA05               PIC X(2000).
       01  COMMAREA06               PIC X(2000).
       01  COMMAREA07               PIC X(2000).
       01  COMMAREA08               PIC X(2000).
       01  COMMAREA09               PIC X(2000).
       01  COMMAREA10               PIC X(2000).

       01  CWRUN                    PIC  X(001).
       01  CWRUN-PROGRAM            PIC  X(050).

       PROCEDURE DIVISION USING NOME     CHECK-NIVEL TASK GRUPO MAPA
                                PROGRAMA PAGINA      MODO-MENU  CFG
                                IMPRESSORA QUADRO    RELATORIO
                                COMMAREA01
                                COMMAREA02
                                COMMAREA03
                                COMMAREA04
                                COMMAREA05
                                COMMAREA06
                                COMMAREA07
                                COMMAREA08
                                COMMAREA09
                                COMMAREA10
                                CWRUN CWRUN-PROGRAM.

       ENTRY1.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           DISPLAY "25"          UPON ENVIRONMENT-VALUE
           DISPLAY "CWLOGPGM"    UPON ENVIRONMENT-NAME
           ACCEPT   CWLOGPGM     FROM ENVIRONMENT-VALUE
           INSPECT  CWLOGPGM   CONVERTING MINUSCULAS TO MAIUSCULAS
Joadir     DISPLAY "CWMENUPAGE"  UPON ENVIRONMENT-NAME
Joadir     ACCEPT   CWMENUPAGE   FROM ENVIRONMENT-VALUE
Joadir     INSPECT  CWMENUPAGE CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWMENUCOMMENT" UPON ENVIRONMENT-NAME
           ACCEPT   SHOWCOMENT    FROM ENVIRONMENT-VALUE
           INSPECT  SHOWCOMENT  CONVERTING MINUSCULAS TO MAIUSCULAS
           CALL "CWSQLC" USING "X4"
           DISPLAY "CWAUTOSPOOL" UPON ENVIRONMENT-NAME
           ACCEPT CWAUTOSPOOL FROM ENVIRONMENT-VALUE
           IF  CWAUTOSPOOL = 'ON' OR 'On' OR 'oN' OR 'on'
               CALL "CWSQLC" USING "S"
           ELSE
               CALL "CWSQLC" USING "P"
           END-IF
           DISPLAY "CWGROUP"  UPON ENVIRONMENT-NAME
           ACCEPT CWSAVE    FROM ENVIRONMENT-VALUE
           INSPECT CWSAVE   CONVERTING MINUSCULAS TO MAIUSCULAS
           IF CWSAVE = 'OLD'
             MOVE 'CWMENZ' TO OVERLAY(3)
           END-IF
           MOVE SPACES TO CWSAVE
           DISPLAY "CWLOGP"     UPON ENVIRONMENT-NAME
           ACCEPT CWLOGP        FROM ENVIRONMENT-VALUE
           DISPLAY "CWUSERNAME" UPON ENVIRONMENT-NAME
           ACCEPT NOME-C        FROM ENVIRONMENT-VALUE
           DISPLAY SPACES       UPON ENVIRONMENT-VALUE
           DISPLAY "CWPASSWORD" UPON ENVIRONMENT-NAME
           ACCEPT SENHA-C       FROM ENVIRONMENT-VALUE
           DISPLAY SPACES       UPON ENVIRONMENT-VALUE
           DISPLAY "CWNOFRAME"  UPON ENVIRONMENT-NAME
           ACCEPT CWSAVE    FROM ENVIRONMENT-VALUE
           INSPECT CWSAVE   CONVERTING MINUSCULAS TO MAIUSCULAS
           IF  CWSAVE = "ON"
               MOVE SPACES TO CWSAVE
               MOVE 1      TO NOFRAME
           END-IF
           MOVE "$CWMENU-F9" TO TIPO-OP (5)
           CALL "CWCRTS" USING "G" CRT-STATUS
           DISPLAY "CWLOGON-LINE"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO LOGON-LINE
           END-IF
           DISPLAY "CWLOGON-COLUMN"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO LOGON-COLUMN
           END-IF
           DISPLAY "CWLOGON-COLOR"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO LOGON-COLOR
           END-IF
           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   CRT-STATUS = X"FFFFFF"
                STOP RUN
           END-IF
           CALL "CWKBST" USING "C"
      *    CALL "CWFONT" USING X"FF"
      *    CANCEL "CWFONT"
           EXEC COBOLware OBJECT (DROP) END-EXEC
           DISPLAY 'CWPROC' UPON ENVIRONMENT-NAME
           ACCEPT CWPROC FROM ENVIRONMENT-VALUE
           IF CWPROC = 'ON'
              EXEC COBOLware PROCESS (CLOSE) END-EXEC
              CANCEL 'CWPROC'
           END-IF
           DISPLAY "CWSAVE" UPON ENVIRONMENT-NAME
           ACCEPT CWSAVE    FROM ENVIRONMENT-VALUE
           IF   CWSAVE = "ON"
                EXEC COBOLware SAVE (Close-All)
                END-EXEC
                CANCEL "CWSAVE"
           END-IF
           DISPLAY "OFF"    UPON ENVIRONMENT-VALUE

           ON 1
              DISPLAY "CWFULLMODE" UPON ENVIRONMENT-NAME
              ACCEPT CWFULLMODE    FROM ENVIRONMENT-VALUE
              INSPECT CWFULLMODE CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWFULL"  UPON ENVIRONMENT-NAME
              ACCEPT PANO-COBOL FROM ENVIRONMENT-VALUE
              IF   PANO-COBOL = SPACES OR LOW-VALUES
                   MOVE "Full.bmp" TO PANO-COBOL
                   DISPLAY PANO-COBOL UPON ENVIRONMENT-VALUE
              END-IF
              DISPLAY "CWFULL" UPON ENVIRONMENT-NAME
              ACCEPT  PANO-COBOL2      FROM ENVIRONMENT-VALUE
              IF   PANO-COBOL2 = SPACES OR LOW-VALUES
                   DISPLAY PANO-COBOL UPON ENVIRONMENT-VALUE
              END-IF
              MOVE PANOS TO PANOS-SAVE
              COPY CWSPPD.
              MOVE LOW-VALUES TO SP2-CD-DATA.
      *       INSPECT AREAS-DE-TRABALHO-2
      *               CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS.

           DISPLAY "CWFULL" UPON ENVIRONMENT-NAME
           DISPLAY PANO-COBOL2 UPON ENVIRONMENT-VALUE
           MOVE "CWMENU" TO MENU-PROG
                IF CWFULLMODE = "MENU"
                   DISPLAY "CWMENU-ACTIVE" UPON ENVIRONMENT-NAME
                   DISPLAY "YES"           UPON ENVIRONMENT-VALUE
                END-IF
           IF   VEZ-LOAD = 1
                MOVE "cwmenug" TO SP2-ND-NAME
                CALL SP2   USING SP2-SET-ICON-FILE-NAME SP2-NAME-DEF
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                GO TO 000-INICIO
           ELSE
                MOVE 1 TO VEZ-LOAD
                DISPLAY "CWPAGE" UPON ENVIRONMENT-NAME
                ACCEPT   CWPAGE-TXT  FROM ENVIRONMENT-VALUE
                IF   CWPAGE-TXT NOT = SPACES
                     MOVE CWPAGE-TXT TO MENU-GERAL FUNCAO-PROGRAMA
                END-IF
           END-IF

           CALL "CBL_CULL_RUN_UNITS"
                ON EXCEPTION
                   CONTINUE
           END-CALL
           DISPLAY "CWLOGO" UPON ENVIRONMENT-NAME
           ACCEPT   CWLOGO  FROM ENVIRONMENT-VALUE
           INSPECT  CWLOGO  CONVERTING MINUSCULAS TO MAIUSCULAS
           IF  (CWLOGO NOT = SPACES)
           AND (CWLOGO NOT = "OFF")
                ACCEPT CWLOGO FROM ENVIRONMENT-VALUE
           END-IF
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           DISPLAY "CWFONE" UPON ENVIRONMENT-NAME
           ACCEPT   CWFONE  FROM ENVIRONMENT-VALUE
SC         CANCEL "CWGETL"
SC         CALL "CWGETL" USING PARAMETROS-CWGETL
           MOVE CWGETL-END    TO CWMENU-END
           SET  CWMENU-AREA01 TO ADDRESS COMMAREA01
           SET  CWMENU-AREA02 TO ADDRESS COMMAREA02
           SET  CWMENU-AREA03 TO ADDRESS COMMAREA03
           SET  CWMENU-AREA04 TO ADDRESS COMMAREA04
           SET  CWMENU-AREA05 TO ADDRESS COMMAREA05
           SET  CWMENU-AREA06 TO ADDRESS COMMAREA06
           SET  CWMENU-AREA07 TO ADDRESS COMMAREA07
           SET  CWMENU-AREA08 TO ADDRESS COMMAREA08
           SET  CWMENU-AREA09 TO ADDRESS COMMAREA09
           SET  CWMENU-AREA10 TO ADDRESS COMMAREA10
           CALL 'CWSETS' USING X"01" CWMENU
           IF   CWGETL-HIGH = 1
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 13
                        MOVE TIPO-CF2 (I) TO TIPO-CF (I)
                END-PERFORM
           END-IF

           IF   CWLOGO = "OFF"
                MOVE SPACES TO CW
           ELSE
                IF   CWLOGO = SPACES
                     PERFORM VARYING COL-W FROM 1 BY 1 UNTIL COL-W > 9
                              ADD 1 TO CK (COL-W)
                     END-PERFORM
                ELSE
                     MOVE CWLOGO TO CW
                END-IF
           END-IF

           ACCEPT  LINHA-COMANDO FROM COMMAND-LINE
           DISPLAY LINHA-COMANDO UPON COMMAND-LINE
           IF   LINHA-COMANDO NOT = SPACES
                PERFORM 102-USERPASS THRU 102-99-FIM
                IF  (NOME-C  NOT = SPACES)
                OR  (SENHA-C NOT = SPACES)
                     MOVE 3 TO LOGON-TYPE
                     DISPLAY SPACES UPON COMMAND-LINE
                END-IF
           END-IF

           CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
           DISPLAY "CWMENU"        UPON ENVIRONMENT-NAME
           ACCEPT  CWMENU-ENV      FROM ENVIRONMENT-VALUE
           IF   CWUNIX-ON
                DISPLAY "TTY"      UPON ENVIRONMENT-NAME
                ACCEPT  TTY        FROM ENVIRONMENT-VALUE
           END-IF
           PERFORM 250-AJUSTA-TTY-NOME THRU 250-99-FIM
           INSPECT CWMENU-ENV CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE SAVE-CWLINE-OPTION   TO CWLINE-OPTION
           SET CWBOXS-TIMEOUT-ENABLE TO TRUE
           SET CWSEND-TIMEOUT-ENABLE TO TRUE.

       000-INICIO.

           DISPLAY "CWTITLE" UPON ENVIRONMENT-NAME
           ACCEPT   CWTITLE  FROM ENVIRONMENT-VALUE

           IF  CWTITLE = SPACES
               MOVE "COBOLware 6.1"   TO CWTITLE
               MOVE CWREVS-REVISAO    TO CWTITLE (133: )
           END-IF
           MOVE CWTITLE TO  SP2-WD-TITLE
           IF CWTITLE (1: 1) = X"B5"
              MOVE CWTITLE(2:) TO  SP2-WD-TITLE
           ELSE
              MOVE CWTITLE TO  SP2-WD-TITLE
           END-IF
           INSPECT SP2-WD-TITLE CONVERTING X"FFC6" TO "  "
           CALL "cobwareg" USING SP2-WD-TITLE TASK STATUS-DEF
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE "LG" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               IF   CWCONF-EXPIRE NOT NUMERIC
                    MOVE 0 TO CWCONF-EXPIRE
               END-IF
               MOVE CWCONF-EXPIRE TO EXPIRE
           ELSE
               MOVE 0             TO EXPIRE
           END-IF

           IF   PROGRAMA = "CWMEN8"
                MOVE "PS" TO CWCONF-REG
                MOVE NOME TO CWCONF-NOME
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "10"
                     IF   CWCONF-PRINTER-DEFAULT = SPACES OR LOW-VALUES
                          MOVE "<Default>" TO CWCONF-PRINTER-DEFAULT
                     END-IF
                     MOVE CWCONF-PRINTER-DEFAULT TO IMPRESSORA
                     PERFORM 170-CHECK-SPOOL THRU 170-99-FIM
                ELSE
                     MOVE "N" TO CHECK-NOME
                END-IF
           END-IF

SC    *    CALL "CWGETL" USING PARAMETROS-CWGETL

           IF   CWGETL-TIMEOUT NOT = 0
                IF   CWGETL-TIMEOUT > 255
                     MOVE 255 TO SEGUNDOS
                ELSE
                     MOVE CWGETL-TIMEOUT TO SEGUNDOS
                END-IF
           ELSE
                MOVE 0              TO SEGUNDOS
           END-IF
           MOVE SEGUNDOS (1: 1) TO SP2-CD-TIMEOUT
           IF   SEGUNDOS = 0
                MOVE X"00" TO SP2-CD-WAIT-SW
           ELSE
                MOVE "a" TO SP2-CD-WAIT-SW
           END-IF

           PERFORM 800-NEW-PRINTER THRU 800-99-FIM
           PERFORM 251-CURDIR      THRU 251-99-FIM
           PERFORM 166-PERSONAL    THRU 166-99-FIM
           IF  MODO-MENU = 1
               MOVE SISTEMA-SM TO SISTEMA
           END-IF

           MOVE CWCONF-QUADRO       TO QUADRO-L
           IF   QUADRO = 99
                MOVE QUADRO-L       TO QUADRO
           END-IF
           PERFORM 146-SET-QUADRO

           IF   PROGRAMA = "CWMENU"
                PERFORM 120-TIME
                IF   CWRUN NOT = 1
                     PERFORM 130-GRAVA-CWLOGF  THRU 130-99-FIM
                END-IF
           ELSE
                PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                MOVE "S" TO CHECK-NOME
                            CHECK-SENHA
                PERFORM 120-TIME
           END-IF
           PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           CALL "CWUSER" USING X"01" *> Initial
           DISPLAY (1, 1) ERASE
           PERFORM 810-PGID THRU 810-99-FIM.

       050-INICIO.

       100-INICIO.

           IF   FS-CWCONF < "10"
                PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                        UNTIL FINALIZAR
           ELSE
                DISPLAY ERRO-CWCONF
           END-IF

           MOVE SPACE TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG.

       000-FIM.

       100-PROCESSAMENTO.

           IF   CHECK-NOME = "N"
                MOVE SPACES TO LB-PRNTER-2
           END-IF

           IF   CWRUN = 1
                DISPLAY "CWTEST"      UPON ENVIRONMENT-NAME
                ACCEPT  LINHA-COMANDO FROM ENVIRONMENT-VALUE
                IF LINHA-COMANDO = SPACES
                   DISPLAY "CWCMDL" UPON ENVIRONMENT-NAME
                   ACCEPT  LINHA-COMANDO FROM ENVIRONMENT-VALUE
                END-IF
                IF LINHA-COMANDO NOT = SPACES
                   PERFORM 102-USERPASS THRU 102-99-FIM
                END-IF
           END-IF

           IF   EXECUTAR
                IF   PROGRAMA NOT = "CWMENU"
                     MOVE "Processamento encerrado" TO FUNCAO-PROGRAMA
                     PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                     MOVE   "CWMENU"                TO PROGRAMA
                     PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                     MOVE CWGETL-LOG   TO SET-LOG
                     CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                     MOVE MENU-GERAL TO FUNCAO-PROGRAMA
                END-IF
                IF   VEZ = 0
                     MOVE   1         TO VEZ
                     PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                END-IF
           END-IF

           DISPLAY "CWLG"       UPON ENVIRONMENT-NAME
           ACCEPT  CWLG         FROM ENVIRONMENT-VALUE
           IF  CWLG = "ON"
               MOVE "N" TO CHECK-NOME
                           CHECK-SENHA
               DISPLAY "OFF" UPON ENVIRONMENT-VALUE
           END-IF

           IF  (NOT NOME-OK)
           OR  (NOT SENHA-OK)
                CALL "CWLOCK" USING "U" NOME TASK
                MOVE 0 TO LOGIN-FLAG
                PERFORM  140-CHECK-NOME THRU 140-99-FIM
                         UNTIL (NOME-OK
                         AND   SENHA-OK)
                         OR    RESPOSTA = "N"
                CALL "CWUSER" USING X"03"  *> Clear fields
                IF   RESPOSTA = "N"
                     MOVE 1 TO FL-EXIT
                ELSE
                     MOVE CWGETL-LOG  TO SET-LOG
                     CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                END-IF
           ELSE
               CALL "CWLOCK" USING "M" NOME TASK PROGRAMA
           END-IF

           IF   CWRUN = 1
                IF   FL-EXIT = 1
                OR  (NOT NOME-OK)
                OR  (NOT SENHA-OK)
                      STOP RUN
                END-IF
                IF CWFULLMODE = "MENU"
                   DISPLAY "CWMENU-ACTIVE" UPON ENVIRONMENT-NAME
                   DISPLAY "NO"            UPON ENVIRONMENT-VALUE
                END-IF
                DISPLAY "CWSARBANE" UPON ENVIRONMENT-NAME
                ACCEPT     SARBANE  FROM ENVIRONMENT-VALUE
                IF   SARBANE NUMERIC
                AND  CHECK-NIVEL = 9
                     EXEC COBOLware Send
                          Message
                   "PrivilÇgio de super-usu†rio n∆o permite executar"
                     END-EXEC
                     STOP RUN
                END-IF
                IF   EXPIRADA = 1
                     EXEC COBOLware Send
                          Message "Senha expirada n∆o permite executar"
                     END-EXEC
                     STOP RUN
                END-IF
                INSPECT  CWRUN-PROGRAM
                         CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE CWRUN-PROGRAM             TO PROGRAMA
      *         MOVE "DESENVOLVIMENTO (CWRUN)" TO NOME
                MOVE SPACES                    TO FUNCAO-PROGRAMA
                STRING "Executando " DELIMITED BY SIZE
                        CWRUN-PROGRAM DELIMITED BY SPACE
                        " sem menu" DELIMITED BY SIZE
                        INTO FUNCAO-PROGRAMA
                MOVE CWGETL-LOG  TO SET-LOG
                CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                IF  (GRUPO NOT = SPACES)
                AND (GRUPO NOT = "Acesso sem restriáîes")
                AND (GRUPO NOT = "Acesso sem restriá‰es")
                AND (GRUPO NOT = "Acesso sem restricoes")
                AND (GRUPO NOT = "Acesso irrestrito")
                     SET CWSQLC-OPEN TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWGRPS-REG
                                         FS-CWGRPS
                                         KGR PGR
                     MOVE "GU"            TO CWGRPS-REG
                     MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWGRPS-REG
                                         FS-CWGRPS
                                         KGR PGR
                     IF   FS-CWGRPS < "09"
                          MOVE CWGRPS-ADM TO ADM
                          MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                          MOVE PROGRAMA        TO CWGRPS-PROG-GRUPO
                          SET CWSQLC-READ TO TRUE
                          SET CWSQLC-EQUAL TO TRUE
                          SET CWSQLC-IGNORE-LOCK TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWGRPS-REG
                                              FS-CWGRPS
                                              KGR PGR
                          IF  (FS-CWGRPS < "09"
                          AND  CWGRPS-ACESSO-GRUPO NOT = SPACE
                          AND  ADM                 NOT = "I")
                          OR  (FS-CWGRPS = "23"
                          AND  ADM                     = "I")
                               MOVE "M¢dulo n∆o autorizado"
                                 TO CWSEND-MSG
                               CALL "CWSEND" USING PARAMETROS-CWSEND
                               SET CWSQLC-CLOSE TO TRUE
                               CALL "CWCONF" USING CWSQLC
                                                   CWGRPS-REG
                                                   FS-CWGRPS
                                                   KGR PGR
                               STOP RUN
                          END-IF
                     END-IF
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWGRPS-REG
                                         FS-CWGRPS
                                         KGR PGR
                END-IF
                PERFORM 810-PGID THRU 810-99-FIM
                CALL "CWLOCK" USING "M" NOME TASK PROGRAMA
                PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                PERFORM 170-CHECK-SPOOL THRU 170-99-FIM
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                DISPLAY 'CWRUNTITLE' UPON ENVIRONMENT-NAME
                MOVE SPACES TO SP2-WD-TITLE
                ACCEPT  SP2-WD-TITLE FROM ENVIRONMENT-VALUE
                IF SP2-WD-TITLE NOT = SPACES
                   INSPECT SP2-WD-TITLE CONVERTING '_' TO SPACE
                   CALL SP2 USING SP2-SET-WINDOW-DEF SP2-WINDOW-DEF
                   DISPLAY SPACES UPON ENVIRONMENT-VALUE
                END-IF
                GOBACK
           END-IF

           PERFORM 170-CHECK-SPOOL THRU 170-99-FIM.

       act.
           MOVE 0 TO SP2-CD-MENU-ID
           PERFORM 110-ACCEPT-TELA THRU 110-99-FIM
                   UNTIL SP2-CD-MENU-ID NOT = ZERO
                      OR NOT NOME-OK
                      OR NOT SENHA-OK

           IF  NOME-OK
           AND SENHA-OK
           AND (NOT FINALIZAR)
               IF  SP2-CD-MENU-ID NOT = 0
                   IF   SP2-CD-KEY = SP2-KEY-ENTER
                        MOVE "B" TO MENU-TYPE
                   ELSE
                        MOVE SPACES         TO MENU-TYPE
                   END-IF
                   MOVE SP2-CD-MENU-ID TO MENU-ID
                   MOVE 0              TO MENU-PAGINA
                   READ MENU
                   IF   MENU-BMP NOT = SPACES
                   AND (MENU-BMP NOT = LOW-VALUES)
                        DISPLAY "CWFULL"    UPON ENVIRONMENT-NAME
                        IF MENU-PANO = SPACES
                           DISPLAY MENU-BMP  UPON ENVIRONMENT-VALUE
                        ELSE
                           DISPLAY MENU-PANO UPON ENVIRONMENT-VALUE
                        END-IF
                        DISPLAY "CWFULLCLOSE" UPON ENVIRONMENT-NAME
                        IF MENU-PANO = SPACES
                           DISPLAY MENU-BMP  UPON ENVIRONMENT-VALUE
                        ELSE
                           DISPLAY MENU-PANO UPON ENVIRONMENT-VALUE
                        END-IF
250416*                 CALL "CWUSER" USING "H" *> Refresh image
250416                  CALL "CWUSER" USING X"04" *> Refresh image
                   ELSE
                        DISPLAY "CWFULLCLOSE" UPON ENVIRONMENT-NAME
                        DISPLAY SPACES        UPON ENVIRONMENT-VALUE
                   END-IF
                   IF   MENU-PAGINA NOT = 0
                        MOVE MENU-PAGINA TO PAGINA
                        MOVE 1           TO MODO-MENU
                        MOVE MENU-FUNCAO-PROGRAMA TO SISTEMA
                                                     SISTEMA-SM
                        PERFORM GUI-TITLE-UPDATE
                           THRU END-GUI-TITLE-UPDATE
                        GO TO ACT
                   END-IF
                   MOVE MENU-FUNCAO-PROGRAMA TO FUNCAO-PROGRAMA
                                                MENU-GERAL
                   MOVE MENU-PROG            TO PROGRAMA
                   CALL "CWMODE" USING "G" MENU-PAGINA-OP MODULO
                                                          MENU-TIPO-REG
                   CALL "CWMODE" USING "W" MENU-PAGINA-OP MODULO
                                                          MENU-TIPO-REG
                   MOVE MENU-HELP            TO RELATORIO
                   PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                   IF RELATORIO (1:1) = '&'
                      DISPLAY 'CWPOSIT' UPON ENVIRONMENT-NAME
                      MOVE MENU-HELP(2:)  TO FCW
                      DISPLAY FCW       UPON ENVIRONMENT-VALUE
                   END-IF
               END-IF
               CALL "CWVARX" USING PROGRAMA LENGTH OF PROGRAMA
               INSPECT PROGRAMA CONVERTING MINUSCULAS TO MAIUSCULAS
               EVALUATE PROGRAMA
                   WHEN "GRPAGE"
                         MOVE "CWPAGE" TO PROGRAMA
                   WHEN "GRBOXS"
                         MOVE "CWBOXS" TO PROGRAMA
                   WHEN "GRMENU"
                         MOVE "CWMENU" TO PROGRAMA
                     PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
               END-EVALUATE
               IF   PROGRAMA = "CWPAGE"
                    MOVE "CWMENU" TO PROGRAMA
                    MOVE PAGINA   TO SALVA-PAGINA
                    MOVE "N"      TO RESPOSTA
                    MOVE 5        TO YP
                    MOVE 0        TO PAGINA
                    PERFORM VARYING IP FROM 20 BY -1
                            UNTIL IP = 0
                            MOVE MENU-HELP (IP: 1) TO TP
                            IF TP NUMERIC
                            AND YP > 1
                                SUBTRACT 1 FROM YP
                                MOVE TP TO PAGINA (YP: 1)
                            END-IF
                    END-PERFORM
                    MOVE PAGINA  TO CWCONF-PAGINA
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    SET CWSQLC-IGNORE-LOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF   FS-CWCONF = "23"
                         MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                         STRING "P†gina n∆o existe: " DELIMITED SIZE
                                 PAGINA               DELIMITED SIZE
                                 INTO CWSEND-MSG
                         CALL "CWSEND" USING PARAMETROS-CWSEND
                         MOVE SALVA-PAGINA TO PAGINA
                         MOVE "N" TO RESPOSTA
                    ELSE
                         PERFORM 103-AJUSTA-NM THRU 103-99-FIM
                         MOVE FUNCAO-PROGRAMA TO CWPAGE-TXT
                                                 MENU-GERAL
                         DISPLAY "CWPAGE"     UPON ENVIRONMENT-NAME
                         DISPLAY  CWPAGE-TXT  UPON ENVIRONMENT-VALUE
                    END-IF
                    PERFORM 180-EXIBE-OPCOES THRU 180-99-FIM
                    MOVE 0 TO OPCAO
                    GO TO 100-99-FIM
               END-IF
               PERFORM 101-SENHA-ESPECIAL THRU 101-99-FIM
               IF   RESPOSTA = "N"
                    GO TO 100-99-FIM
               END-IF
               IF   PROGRAMA NOT = "CWBOXS"
                    PERFORM 103-AJUSTA-NM THRU 103-99-FIM
      *        ELSE
                    IF   NIVEL-ATUAL NOT = 0
                         MOVE CWBOXS-TEXT (CWBOXS-OPTION)
                           TO FUNCAO-PROGRAMA
                         INSPECT FUNCAO-PROGRAMA CONVERTING "_" TO SPACE
                    END-IF
               END-IF
               DISPLAY "CWAPPLICATION" UPON ENVIRONMENT-NAME
               DISPLAY FUNCAO-PROGRAMA UPON ENVIRONMENT-VALUE
               IF   HELP = 1
               AND (PROGRAMA NOT = "CWMENU")
               AND (PROGRAMA NOT = "CWBOXS")
                    MOVE "CWMEN4" TO PROGRAMA
               END-IF
               IF   PROGRAMA NOT = "CWBOXS"
                    CALL "CWPAGE" USING "!"
                    CANCEL "CWPAGE"
                    PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
               END-IF
               IF   OPCAO NOT = 0
                    MOVE "S" TO RESPOSTA
               END-IF
               IF  (MENU-CHECK NOT = SPACE)
               AND (MENU-CHECK NOT = "n")
               AND (MENU-CHECK NOT = "N")
               AND (MENU-CHECK NOT = "0")
               AND (MENU-CHECK NOT = X"00")
               AND  HELP = ZERO
hdbad *        AND (OPCAO NOT = 0)
               AND  TESTE-CWLINE = "N"
                    MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                    PERFORM 103-AJUSTA-NM THRU 103-99-FIM
                    PERFORM TEST AFTER
                               VARYING U FROM LENGTH OF FUNCAO-PROGRAMA
                                    BY -1
                               UNTIL FUNCAO-PROGRAMA (U: 1) NOT = SPACES
                                  CONTINUE
                    END-PERFORM
                    STRING "Executar "             DELIMITED SIZE
                            FUNCAO-PROGRAMA (1: U) DELIMITED SIZE
                            " ?"                   DELIMITED SIZE
                            INTO CWSEND-MSG
                    MOVE SPACES      TO CWSEND-SCREENS
                    MOVE "  ~Sim__"  TO CWSEND-SCREEN (1)
                    MOVE "  ~N∆o__"  TO CWSEND-SCREEN (2)
                    MOVE 2          TO CWSEND-OPTION
                    CALL "CWSEND" USING PARAMETROS-CWSEND
                    IF   CWSEND-OPTION = 1
                         MOVE "S" TO RESPOSTA
                    ELSE
                         MOVE "N" TO RESPOSTA
                    END-IF
               END-IF
               IF   TESTE-CWLINE = "S"
               AND (NIVEL-ATUAL  = 0)
                    MOVE "N" TO RESPOSTA TESTE-CWLINE
               END-IF
               IF   NOT EXECUTAR
                    MOVE MENU-GERAL TO FUNCAO-PROGRAMA
                    PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
               ELSE
                    PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                    MOVE  CWGETL-LOG TO SET-LOG
                    CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                    MOVE  ZERO       TO OPCAO
                    IF HELP = ZERO
                    OR PROGRAMA = "CWMENU"
                       IF PROGRAMA NOT = "CWMENU"
                          IF CWFULLMODE = "MENU"
                             DISPLAY "CWMENU-ACTIVE"
                                UPON ENVIRONMENT-NAME
                             DISPLAY "NO"
                                UPON ENVIRONMENT-VALUE
                          END-IF
                          SET CWSQLC-CLOSE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                          IF XSDRUN NOT = SPACES
                             MOVE XSDRUN TO PROGRAMA
                             MOVE SPACES TO XSDRUN
                          END-IF
                          CALL "CWLOCK" USING "M" NOME TASK PROGRAMA
                          IF  MENU-HELP (1: 1) = "$"
                              MOVE MENU-HELP (2: 4) TO CA-POSI
                              MOVE MENU-HELP (7: 2) TO CA-SIZE
                              IF  (CA-POSI NOT = 0)
                              AND (CA-SIZE NOT = 0)
                              MOVE MENU-HELP (10: )
                                  TO COMMAREA01 (CA-POSI: CA-SIZE)
                               END-IF
                          END-IF
                          IF  MENU-HELP (1: 1) = "="
                              MOVE MENU-HELP (2: ) TO OPTS
                              DISPLAY "CWOPTS" UPON ENVIRONMENT-NAME
                              DISPLAY OPTS     UPON ENVIRONMENT-VALUE
                          END-IF
                          DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                          DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                          IF MODO-MENU = 1
                             DISPLAY "CWFULL"  UPON ENVIRONMENT-NAME
                             DISPLAY PANO-COBOL2 UPON ENVIRONMENT-VALUE
                          END-IF
LOGON                     MOVE MENU-NIVEL TO NIVEL-LOGON
LOGON                     DISPLAY 'CWLOGON-NIVEL' UPON ENVIRONMENT-NAME
LOGON                     DISPLAY NIVEL-LOGON     UPON ENVIRONMENT-VALUE
                          EXIT PROGRAM
                       ELSE
                           PERFORM 190-MUDA-PATH  THRU 190-99-FIM
Mollo                      MOVE '990001'    TO CWCONF-CHAVE
Mollo                      SET CWSQLC-READ  TO TRUE
Mollo                      SET CWSQLC-EQUAL TO TRUE
Mollo                      CALL "CWCONF" USING CWSQLC CWCONF-REG
Mollo                                              FS-CWCONF KCO PCO
                       END-IF
                    ELSE
                         DISPLAY "CWMENU-HELP" UPON ENVIRONMENT-NAME
                         ACCEPT   CWMENU-HELP  FROM ENVIRONMENT-VALUE
                         IF   CWMENU-HELP = SPACES
                              EXEC COBOLware HELP
                                   File MENU-HELP
                                   COLOR-FRAME WHITE-BLACK-LOW
                              END-EXEC
                         ELSE
                              CALL CWMENU-HELP USING MENU-PROG
                              ON EXCEPTION
                                 MOVE SPACES TO CWSEND-SCREENS
                                 STRING 'M¢dulo de help "'
                                        DELIMITED BY SIZE
                                        CWMENU-HELP DELIMITED BY SPACE
                                        '" n∆o encontrado.'
                                        DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                                 CALL "CWSEND" USING PARAMETROS-CWSEND
                              END-CALL
                              CANCEL CWMENU-HELP
                         END-IF
      *                  MOVE MENU-HELP TO HELP-PATH
      *                  SET CWSQLC-CLOSE TO TRUE
      *                  CALL "CWCONF" USING CWSQLC
      *                                      CWCONF-REG
      *                                      FS-CWCONF
      *                                      KCO PCO
      *                  CALL "CWMEN4" USING HELP-PATH TASK
      *                   ON OVERFLOW
      *                      MOVE SPACES TO CWSEND-SCREENS
      *                      MOVE "Falha no overlay CWMEN4 "
      *                        TO CWSEND-MSG
      *                      CALL "CWSEND" USING PARAMETROS-CWSEND
      *                      END-CALL
      *                   END-CALL
      *                  SET CWSQLC-UPDATE TO TRUE
      *                  CALL "CWCONF" USING CWSQLC
      *                                      CWCONF-REG
      *                                      FS-CWCONF
      *                                      KCO PCO
      *                  PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
      *                  CANCEL "CWMEN4"
                         MOVE "CWMEN4" TO PROGRAMA
                         DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                         DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                         EXIT PROGRAM.

       100-99-FIM. EXIT.

       101-SENHA-ESPECIAL.

           IF   EXPIRADA = 1
                EXEC COBOLware Send
                     Message
             "Senha expirada n∆o permite operaá‰es, altere sua senha."
                END-EXEC
                CALL "CWPASS" USING NOME
                CANCEL "CWPASS"
                STOP RUN
           END-IF
           DISPLAY "CWSARBANE" UPON ENVIRONMENT-NAME
           ACCEPT     SARBANE  FROM ENVIRONMENT-VALUE
           IF   SARBANE NUMERIC
           AND  CHECK-NIVEL = 9
                EXEC COBOLware Send
                     Message
                     "PrivilÇgio de super-usu†rio n∆o permite operaá‰es"
                END-EXEC
                MOVE "N" TO RESPOSTA
                GO TO 101-99-FIM
           END-IF
           IF   MENU-PASS = SPACES
                MOVE "S" TO RESPOSTA
                GO TO 101-99-FIM
           END-IF
           MOVE MENU-PASS TO CHECK-PASS-3
           INSPECT CHECK-PASS-3 CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE SPACES TO CHECK-PASS
           MOVE "S"    TO RESPOSTA
           PERFORM UNTIL (CHECK-PASS-3 = CHECK-PASS)
                   OR (NOT EXECUTAR)
                   OR (PROGRAMA     = "CWMENU")
               IF   CWGETL-AUTOPASS = 1
                    MOVE MENU-SIZE-S TO LENSENHA
               ELSE
                    MOVE LENGTH OF CHECK-PASS TO LENSENHA
               END-IF
               MOVE MENU-NM-OPCAO TO MSG
               EXEC COBOLware BoxDialog
                    LINE   LOGON-LINE COLUMN LOGON-COLUMN
                    HEADER MSG
                    COLOR LOGON-COLOR
                    Caption(1) "Opá∆o requer senha"
                    Size(1) LENSENHA (Secure(1))
                    Data(1) CHECK-PASS;CHECK-PASS
                    CANCEL ;RESPOSTA
               END-EXEC
               IF   RESPOSTA = "Y"
                    MOVE "N" TO RESPOSTA
                    MOVE 0   TO OPCAO
               ELSE
                    MOVE "S" TO RESPOSTA
                    INSPECT CHECK-PASS CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
                    IF CHECK-PASS-3 NOT = CHECK-PASS
                       EXEC COBOLware Send
                            Message "Senha especial incorreta"
                       END-EXEC
                    END-IF
               END-IF
           END-PERFORM.

       101-99-FIM. EXIT.

       102-USERPASS.

           INSPECT LINHA-COMANDO CONVERTING MINUSCULAS TO MAIUSCULAS
      *    MOVE SPACES TO NOME-C
      *                   SENHA-C
301116     DISPLAY "CWUSERNAME" UPON ENVIRONMENT-NAME
301116     DISPLAY SPACES       UPON ENVIRONMENT-VALUE
301116     DISPLAY "CWPASSWORD" UPON ENVIRONMENT-NAME
301116     DISPLAY SPACES       UPON ENVIRONMENT-VALUE
           MOVE 0      TO I-NOME
                          I-SENHA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 200
                   IF  BYTE-L (I) = SPACE
                       MOVE 0 TO SET-NOME SET-SENHA
                   END-IF
                   IF  BYTE-L (I) = "/"
                       MOVE 0 TO SET-NOME SET-SENHA
                       ADD 1  TO I
                       IF   I < 194
                            IF   LINHA-COMANDO (I: 8) = "NOFRAME "
                                 MOVE 1 TO NOFRAME
                            END-IF
                       END-IF
                       EVALUATE TRUE
                           WHEN BYTE-L (I) = "U" OR "u"
301116                          move spaces to nome-c
                                ADD 1 TO I
                                IF  BYTE-L (I) = ":"
                                    MOVE 1 TO SET-NOME
                                    ADD  1 TO I
                                END-IF
                           WHEN BYTE-L (I) = "P" OR "p" OR "S" OR "s"
301116                          move spaces to senha-c
                                ADD 1 TO I
                                IF  BYTE-L (I) = ":"
                                    MOVE 1 TO SET-SENHA
                                    ADD  1 TO I
                                END-IF
                       END-EVALUATE
                   END-IF
                   IF   SET-NOME = 1
                        ADD  1          TO I-NOME
                        MOVE BYTE-L (I) TO NOME-C (I-NOME: 1)
                        IF   I-NOME = 30
                             MOVE 0 TO I-NOME SET-NOME
                        END-IF
                   END-IF
                   IF   SET-SENHA = 1
                        ADD  1          TO I-SENHA
                        MOVE BYTE-L (I) TO SENHA-C (I-SENHA: 1)
                        IF   I-SENHA = 30
                             MOVE 0 TO I-SENHA SET-SENHA
                        END-IF
                   END-IF
           END-PERFORM.

       102-99-FIM. EXIT.

       103-AJUSTA-NM.

      *    MOVE WS-NM-OPCAO (OC) TO FUNCAO-NOVA
      *    MOVE SPACES           TO FUNCAO-PROGRAMA
      *    MOVE 0                TO Y
      *    PERFORM VARYING I FROM 1 BY 1
      *              UNTIL I > LENGTH OF FUNCAO-PROGRAMA
      *            IF   FUNCAO-NOVA (I: 1) NOT = X"7E"
      *                 ADD 1 TO Y
      *                 MOVE FUNCAO-NOVA     (I: 1)
      *                   TO FUNCAO-PROGRAMA (Y: 1)
      *            END-IF
      *    END-PERFORM
      *    INSPECT FUNCAO-PROGRAMA CONVERTING "_" TO SPACE.
      *
       103-99-FIM. EXIT.

       110-ACCEPT-TELA.

           DISPLAY (1, 1) ERASE
           CALL "CWUSER" USING "R" *> Refresh screen
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
           CLOSE MENU
           OPEN I-O MENU
           INITIALIZE SALVAS
           MOVE LOW-VALUES TO SP2-MD-DATA
           MOVE 0          TO SP2-MD-OPTION-CNT
           MOVE "CWMEN0"   TO SP2-MD-NAME
      *    MOVE 1          TO SP2-MDO-ACC-KEY (1)
           IF   MODO-MENU = 0
                MOVE "99"  TO CWCONF-REG99
           ELSE
                MOVE "SM"  TO CWCONF-REG99
           END-IF

           MOVE PAGINA     TO CWCONF-PAGINA
           MOVE 1          TO N
           IF  PAGINA > 1
           AND MODO-MENU = 0
               MOVE 0 TO OPCOES-VALIDAS
               PERFORM TEST AFTER UNTIL OPCOES-VALIDAS NOT = 0
                                  OR FS-CWCONF > "09"
                       SUBTRACT 1 FROM CWCONF-PAGINA
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-EQUAL TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF  FS-CWCONF < "10"
                           PERFORM GUI250-CHECK-ACESSO
                              THRU GUI250-99-FIM
                              VARYING I FROM 1 BY 1 UNTIL I > 26
                       END-IF
               END-PERFORM
               IF  FS-CWCONF < "10"
                   ADD  1   TO SP2-MD-OPTION-CNT
                   MOVE "<" TO SP2-MDO-TEXT(SP2-MD-OPTION-CNT)
                   MOVE SP2-MD-OPTION-CNT
                     TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
                   MOVE SP2-KEY-PGUP      TO SP2-MDO-ACC-KEY
                                             (SP2-MD-OPTION-CNT)
               END-IF
               MOVE PAGINA     TO CWCONF-PAGINA
           END-IF
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM GUI240-VARRE THRU GUI240-99-FIM
           IF   OPCOES-VALIDAS = 0
           AND  PAGINA > 1
           AND  MODO-MENU = 0
                IF   SP2-CD-KEY = SP2-KEY-PGDN
                     ADD 1 TO PAGINA
                ELSE
                     SUBTRACT 1 FROM PAGINA
                END-IF
                CLOSE MENU
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
                GO TO 110-ACCEPT-TELA
           END-IF
           MOVE 0 TO OPCOES-VALIDAS
           PERFORM TEST AFTER UNTIL OPCOES-VALIDAS NOT = 0
                              OR FS-CWCONF > "09"
                   ADD 1 TO CWCONF-PAGINA
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF  FS-CWCONF < "10"
                       PERFORM GUI250-CHECK-ACESSO THRU GUI250-99-FIM
                               VARYING I FROM 1 BY 1 UNTIL I > 26
                   END-IF
           END-PERFORM
           IF   MODO-MENU = 1
                ADD  1                 TO SP2-MD-OPTION-CNT
                MOVE "Portal"  TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                MOVE 0                 TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
                MOVE SP2-KEY-PGUP      TO SP2-MDO-ACC-KEY
                                          (SP2-MD-OPTION-CNT)
           END-IF
           IF  FS-CWCONF < "10"
           AND MODO-MENU = 0
               ADD  1                 TO SP2-MD-OPTION-CNT
               MOVE ">"               TO SP2-MDO-TEXT(SP2-MD-OPTION-CNT)
               MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
               MOVE SP2-KEY-PGUP      TO SP2-MDO-ACC-KEY
                                         (SP2-MD-OPTION-CNT)
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
           INITIALIZE MENU-REG
           MOVE "CWMENU" TO MENU-PROG
           MOVE SP2-MD-OPTION-CNT TO MENU-EXTRA
IQ    *    IF   CWGETL-SPOOL NOT = "2"
                ADD  1              TO SP2-MD-OPTION-CNT
...   *         MOVE X"2A" TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
...             MOVE "Ferramentas" TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                PERFORM ACENTOS THRU FIM-ACENTOS
                MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
                                          PAI (N)
                MOVE "s" TO SP2-MDO-TYPE (SP2-MD-OPTION-CNT)

                ADD  1               TO SP2-MD-OPTION-CNT
                ADD  1               TO N
                MOVE "~Configura" TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                PERFORM ACENTOS THRU FIM-ACENTOS
                MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
                                          PAI (N)
                MOVE PAI (N - 1) TO SP2-MDO-OWNR-ID
                                    (SP2-MD-OPTION-CNT)
                MOVE "s" TO SP2-MDO-TYPE (SP2-MD-OPTION-CNT)
                MOVE SP2-MD-OPTION-CNT TO PRE-F3
                ADD  1   TO N
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 13
                        IF   NIVEL (I) NOT > CHECK-NIVEL
                             ADD  1           TO SP2-MD-OPTION-CNT
                             MOVE TIPO-CF (I) TO SP2-MDO-TEXT
                                                (SP2-MD-OPTION-CNT)
                             PERFORM ACENTOS THRU FIM-ACENTOS
                             MOVE PAI (N - 1) TO SP2-MDO-OWNR-ID
                                                (SP2-MD-OPTION-CNT)
                             MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID
                                      (SP2-MD-OPTION-CNT)
                             MOVE SPACES            TO MENU-TYPE
                             MOVE SP2-MD-OPTION-CNT TO MENU-ID
                             MOVE TIPO-NV       (I) TO MENU-NIVEL
                             MOVE SPACE             TO MENU-CHECK
                             MOVE OVERLAY       (I) TO MENU-PROG
                             MOVE SPACES            TO MENU-PASS
                                                       MENU-HELP
                             MOVE 0                TO MENU-SIZE-S
                             MOVE I                TO MENU-NO-OPCAO
                             MOVE TIPO-CF2     (I) TO MENU-NM-OPCAO
                             PERFORM ACENTOS2 THRU FIM-ACENTOS2
                             MOVE X"00"            TO MENU-NM-OPCAO
                                                      (18: 1)
                             MOVE CWCONF-TIPO      TO MENU-TIPO-REG
<SC>                         move SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
<SC>                           to menu-funcao-programa
                             WRITE MENU-REG
                        END-IF
                END-PERFORM
                SUBTRACT 1 FROM N
                MOVE SP2-MD-OPTION-CNT TO POS-F3
f9>             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                     IF   TIPO-OP (I) (1: 1) = "$"
                          MOVE  TIPO-OP(I) (2:)  TO F9-TEXTO
                          DISPLAY F9-TEXTO     UPON ENVIRONMENT-NAME
                          MOVE SPACES TO TIPO-OP(I)
                                         F9-TEXTO F9-PROGRAM
                          ACCEPT F9-TEXTO FROM ENVIRONMENT-VALUE
                          IF  F9-TEXTO NOT = SPACES
                              PERFORM VARYING Y FROM 1 BY 1
                                        UNTIL Y > LENGTH OF F9-TEXTO
                                           OR (F9-PROGRAM NOT = SPACES)
                                   IF F9-TEXTO (Y:1) = ","
                                      ADD 1 TO Y
                                      MOVE F9-TEXTO (Y:) TO F9-PROGRAM
                                      SUBTRACT 1 FROM Y
                                      MOVE SPACES TO F9-TEXTO (Y:)
                                   END-IF
                              END-PERFORM
                          END-IF
                          IF  F9-PROGRAM NOT = SPACES
                              MOVE F9-TEXTO TO TIPO-OP (I)
                          END-IF
                     END-IF
                     IF  (TIPO-NV (I) NOT > CHECK-NIVEL)
                     AND (TIPO-OP (I) NOT = SPACES)
                        ADD  1 TO SP2-MD-OPTION-CNT
                        IF   I = 1
                             IF   HELP = 1
                                  MOVE "~Modo normal        [F1]"
                                    TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                             ELSE
                                  MOVE "~Modo help  [F1]"
                                    TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                             END-IF
                             PERFORM ACENTOS THRU FIM-ACENTOS
                             MOVE SP2-MD-OPTION-CNT TO HELP-ID
                             MOVE TIPO-FX (I) TO SP2-MDO-ACC-KEY
                                            (SP2-MD-OPTION-CNT)
                        ELSE
                              MOVE TIPO-OP (I)
                                TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                              PERFORM ACENTOS THRU FIM-ACENTOS
                        END-IF
                        MOVE TIPO-FX (I) TO SP2-MDO-ACC-KEY
                                            (SP2-MD-OPTION-CNT)
                        MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID
                                           (SP2-MD-OPTION-CNT)
                        MOVE PAI (N - 1) TO SP2-MDO-OWNR-ID
                                    (SP2-MD-OPTION-CNT)
                     END-IF
                END-PERFORM
IQ    *    END-IF
           COMPUTE SP2-MD-VAR-LEN = SP2-MD-OPTION-CNT *
                 (SP2-MD-OPTN-LEN + SP2-MD-OPTC-LEN + SP2-MD-OPTV-LEN)
      *    if tradutor = "F:\tradutor"
      *       open output bug
      *       perform varying i from 1 by 1 until i > SP2-MD-OPTION-CNT
      *            move SP2-MDO-TEXT (i) to ws-texto
      *            move i to SP2-MDO-TEXT (i)
      *            move ws-texto to SP2-MDO-TEXT (i) (6: )
      *            MOVE SP2-MDO-ID      (i) TO BUG-MDO-ID
      *            MOVE SP2-MDO-OWNR-ID (i) TO BUG-MDO-OWNR-ID
      *            MOVE SP2-MDO-TEXT    (i) TO BUG-MDO-TEXT
      *            MOVE SP2-MDO-TYPE    (i) TO BUG-MDO-TYPE
      *            MOVE SP2-MDO-ACC-KEY (i) TO BUG-MDO-ACC-KEY
      *            write bug-reg
      *       end-perform
      *       close bug
      *    end-if
           CALL SP2   USING SP2-SET-MENU-DEF SP2-MENU-DEF
           MOVE "Menu geral" TO MENU-NM-OPCAO
                                FUNCAO-PROGRAMA
           PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE.
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       110-CONVERSE.

           MOVE "CWMENU"            TO SP2-CD-NEXT-PANEL
           MOVE WS-PANEL-ID         TO SP2-CD-NEXT-FLD-ID
           MOVE 0                   TO SP2-CD-KEY
           CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
           PERFORM CONVERTE-TECLA
           CALL "CWAKEY" USING TECLA MIL
           ACCEPT TECLA FROM ESCAPE KEY
           IF   SP2-CD-KEY  = SP2-KEY-TIMEOUT
                MOVE SP2-KEY-ESC TO SP2-CD-KEY
                MOVE 1           TO FECHAR
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-ENTER
           AND (SP2-CD-BUTTON-ID NOT = 0)
                MOVE "B"              TO MENU-TYPE
                MOVE SP2-CD-BUTTON-ID TO MENU-ID
                READ MENU
                IF   MENU-BMP NOT = SPACES
                AND (MENU-BMP NOT = LOW-VALUES)
                     DISPLAY "CWFULL"    UPON ENVIRONMENT-NAME
                     DISPLAY MENU-BMP       UPON ENVIRONMENT-VALUE
                     IF MENU-PANO = SPACES
                        DISPLAY MENU-BMP  UPON ENVIRONMENT-VALUE
                     ELSE
                        DISPLAY MENU-PANO UPON ENVIRONMENT-VALUE
                     END-IF
                     CALL "CWUSER" USING "H" *> Refresh image
                END-IF
                MOVE SP2-KEY-MENU TO SP2-CD-KEY
                MOVE MENU-ID2     TO SP2-CD-MENU-ID
           END-IF
           IF  (SP2-CD-KEY = SP2-KEY-MENU OR SP2-KEY-ESC)
           AND  SP2-CD-MENU-ID = 0
           AND  MODO-MENU = 1
                CALL SP2   USING SP2-CLEAR-MENU SP2-NULL-PARM
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE 0 TO SP2-CD-KEY
                DISPLAY "CWFULL" UPON ENVIRONMENT-NAME
                DISPLAY PANO-COBOL2 UPON ENVIRONMENT-VALUE
                CALL "CWUSER" USING "H" *> Refresh image
                MOVE "CWMEN4" TO PROGRAMA
                MOVE 0 TO MODO-MENU
                MOVE 1 TO PAGINA
                DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                GOBACK
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-MENU
           AND  SP2-CD-MENU-ID = HELP-ID
                MOVE SP2-KEY-F1 TO SP2-CD-KEY
                MOVE 0          TO SP2-CD-MENU-ID
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-MENU
           AND  SP2-CD-MENU-ID > PRE-F3
           AND (SP2-CD-MENU-ID NOT > POS-F3)
                MOVE SP2-KEY-F3     TO SP2-CD-KEY
                MOVE SPACES         TO MENU-TYPE
                MOVE SP2-CD-MENU-ID TO MENU-ID
                READ MENU
                MOVE 0              TO SP2-CD-MENU-ID
                MOVE MENU-NM-OPCAO TO FUNCAO-PROGRAMA
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                GO TO 110-ESC
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-ESC
           OR   SP2-CD-KEY = SP2-KEY-F1
           OR   SP2-CD-KEY = SP2-KEY-F5
           OR   SP2-CD-KEY = SP2-KEY-F7
           OR   SP2-CD-KEY = SP2-KEY-F8
           OR   SP2-CD-KEY = SP2-KEY-F9
           OR   SP2-CD-KEY = SP2-KEY-F10
           OR   SP2-CD-KEY = SP2-KEY-PGDN
           OR   SP2-CD-KEY = SP2-KEY-PGUP
                GO TO 110-ESC
           END-IF
           IF   SP2-CD-KEY = SP2-KEY-CLOSE
           OR   SP2-KEY-SYS-SHUTDOWN
           OR   SP2-KEY-APP-CLOSE
                MOVE SP2-KEY-CLOSE TO SP2-CD-KEY
                MOVE 1             TO FECHAR
                GO TO 110-ESC
           END-IF

           IF   SP2-CD-KEY = SP2-KEY-MENU
                IF   SP2-MDO-ACC-KEY (SP2-CD-MENU-ID) NOT = 0
                     MOVE SP2-MDO-ACC-KEY (SP2-CD-MENU-ID) TO SP2-CD-KEY
                     MOVE SPACES TO MENU-NM-OPCAO
                     IF   SP2-MDO-TEXT (SP2-CD-MENU-ID) (1: 1) = "<"
                                                              OR ">"
                          MOVE SP2-MDO-ACC-KEY (SP2-CD-MENU-ID)
                            TO SP2-CD-KEY
                     ELSE
                          STRING SP2-MDO-TEXT (SP2-CD-MENU-ID) (2: )
                                 DELIMITED BY "["
                                 X"00" DELIMITED BY SIZE                          "
                            INTO MENU-NM-OPCAO
                          MOVE X"00"         TO MENU-NM-OPCAO (18: )
                          MOVE MENU-NM-OPCAO TO FUNCAO-PROGRAMA
                          PERFORM GUI-TITLE-UPDATE
                             THRU END-GUI-TITLE-UPDATE
                     END-IF
                END-IF
                GO TO 110-ESC
           END-IF
           MOVE SP2-CD-NEXT-FLD-ID TO WS-PANEL-ID
           GO TO 110-CONVERSE.

       110-ESC.

           PERFORM UNTIL BM = 0
                   MOVE BM-ID (BM) TO SP2-FD-ID
                   CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   CALL "CWSPID" USING SP2-FD-ID "FDelete"
                   SUBTRACT 1 FROM BM
           END-PERFORM

           IF   SP2-CD-KEY = SP2-KEY-ESC
                IF   FECHAR = 1
                     MOVE 1 TO CWSEND-OPTION
                ELSE
                     MOVE "Sair do sistema ?" TO CWSEND-MSG
                     MOVE SPACES      TO CWSEND-SCREENS
                     MOVE "__~Sair__" TO CWSEND-SCREEN (1)
                     MOVE "_~Voltar_" TO CWSEND-SCREEN (2)
                     MOVE 1           TO CWSEND-OPTION
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
                IF   CWSEND-TIMEOUT-ON
                     MOVE 1 TO CWSEND-OPTION
                END-IF
                IF   CWSEND-OPTION = 2 OR 0
                     MOVE 0 TO SP2-CD-KEY
                END-IF
                IF   CWSEND-OPTION = 1
                     CALL SP2   USING SP2-CLEAR-MENU SP2-NULL-PARM
                     MOVE SPACE TO FLAG-OPCOES
                     CALL "CWLOCK" USING "U" NOME TASK
      *              MOVE SP2-KEY-CLOSE TO SP2-CD-KEY
                     MOVE SPACES TO SENHA-C
                     DISPLAY "CWPASSWORD" UPON ENVIRONMENT-NAME
                     DISPLAY SPACE        UPON ENVIRONMENT-VALUE
                     MOVE "CWMENU" TO PROGRAMA
                     DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                     DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                     GOBACK
                END-IF
           END-IF

           IF   SP2-CD-KEY = SP2-KEY-CLOSE
                IF   CWGETL-LOGOUT NOT = SPACES
                     CALL CWGETL-LOGOUT USING COMMAREA01
                                              COMMAREA02
                                              COMMAREA03
                                              COMMAREA04
                                              COMMAREA05
                                              COMMAREA06
                                              COMMAREA07
                                              COMMAREA08
                                              COMMAREA09
                                              COMMAREA10
                          ON OVERFLOW
                             MOVE SPACES TO CWSEND-MSG
                             STRING "Imposs°vel executar o programa "
                                     DELIMITED BY SIZE
                                     CWGETL-LOGOUT DELIMITED BY SPACE
                             INTO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             END-CALL
                             STOP RUN
                     END-CALL
                END-IF
                MOVE "CWMENU" TO PROGRAMA
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                STOP RUN
           END-IF

           IF   SP2-CD-KEY = SP2-KEY-ESC
           OR   SP2-CD-KEY = SP2-KEY-F3
           OR   SP2-CD-KEY = SP2-KEY-F1
           OR   SP2-CD-KEY = SP2-KEY-F5
           OR   SP2-CD-KEY = SP2-KEY-F7
           OR   SP2-CD-KEY = SP2-KEY-F8
           OR   SP2-CD-KEY = SP2-KEY-F9
           OR   SP2-CD-KEY = SP2-KEY-F10
           OR   SP2-CD-KEY = SP2-KEY-PGDN
           OR   SP2-CD-KEY = SP2-KEY-PGUP
                MOVE ZERO TO OPCAO
                IF   SP2-CD-KEY = SP2-KEY-F10
                     MOVE 0 TO SP2-CD-MENU-ID
                     MOVE "L5" TO CWCONF-REG
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     MOVE "O produto n∆o est† licenciado." TO CWSEND-MSG
                     MOVE SPACES           TO CWSEND-SCREENS
                     IF   FS-CWCONF < "10"
                     AND  CWCONF-LICENCIADO NOT = SPACES
                          MOVE CWCONF-LICENCA-X TO LICENCA-2
                          MOVE SPACES           TO CWSEND-MSG
                          STRING "Licenciado: "
                                  CWCONF-LICENCIADO DELIMITED BY SIZE
                             INTO CWSEND-MSG
                     END-IF
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
           ELSE
                MOVE CWGETL-LOG  TO SET-LOG
                CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                CALL "CWATCH"
                IF   OPCAO NOT = ZERO
                     PERFORM VARYING I FROM 1 BY 1
                             UNTIL I > 26
                             IF   OPCAO = CWCONF-NO-OPCAO (I)
                                  MOVE 30 TO I
                             END-IF
                     END-PERFORM
                END-IF
           END-IF.

       110-FUNCOES.

           CALL SP2   USING SP2-CLEAR-MENU SP2-NULL-PARM
           MOVE SPACE TO FLAG-OPCOES

           IF   SP2-CD-KEY = SP2-KEY-F7
                MOVE 0 TO SP2-CD-MENU-ID
                CALL "CWLOGD"
                CANCEL "CWLOGD"
           ELSE
           IF   SP2-CD-KEY = SP2-KEY-F8
           AND  CHECK-NIVEL = 9
                MOVE 0 TO SP2-CD-MENU-ID
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                CALL "CWMENC"
                     ON OVERFLOW
                        MOVE SPACES TO CWSEND-SCREENS
                        MOVE "Falha no overlay CWMENC " TO CWSEND-MSG
                        CALL "CWSEND" USING PARAMETROS-CWSEND END-CALL
                     END-CALL
                CANCEL "CWMENC"
                SET CWSQLC-UPDATE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
           IF   SP2-CD-KEY = SP2-KEY-F9
                MOVE 0 TO SP2-CD-MENU-ID
                IF   F9-PROGRAM NOT = SPACES
                     DISPLAY (1, 1) ERASE
                     MOVE F9-TEXTO   TO FUNCAO-PROGRAMA
                     MOVE F9-PROGRAM TO PROGRAMA
                     PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                     DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                     DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
                     CALL PROGRAMA USING COMMAREA01
                                         COMMAREA02
                                         COMMAREA03
                                         COMMAREA04
                                         COMMAREA05
                                         COMMAREA06
                                         COMMAREA07
                                         COMMAREA08
                                         COMMAREA09
                                         COMMAREA10
                          ON OVERFLOW
                             MOVE SPACES TO CWSEND-MSG
                             STRING "Imposs°vel executar o programa "
                                     DELIMITED BY SIZE
                                     PROGRAMA DELIMITED BY SPACE
                             INTO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                     END-CALL
                     CANCEL PROGRAMA
                     MOVE MENU-GERAL TO FUNCAO-PROGRAMA
                     MOVE "CWMENU"   TO PROGRAMA
                ELSE
                     CALL "PC_TEST_PRINTER" USING PRINTER-NO
                                                  PRINTER-STATUS
                     ON   OVERFLOW
                          MOVE 144 TO PRINTER-STATUS
                     END-CALL
                     IF   PRINTER-STATUS = 144
                          DISPLAY SALTO UPON PRINTER
                     END-IF
                END-IF
           ELSE
           IF   SP2-CD-KEY = SP2-KEY-F5
                MOVE 0 TO SP2-CD-MENU-ID
                IF   HELP  = ZERO
                     MOVE "Controle de SPOOL" TO FUNCAO-PROGRAMA
                     DISPLAY (1, 1) ERASE
                     PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                     PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                     CALL SPOOL USING "R0000000.SPL"
                           ON OVERFLOW
                             MOVE SPACES TO CWSEND-SCREENS
                             MOVE "Falha no overlay CWMEN9 "
                               TO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                              END-CALL
                          END-CALL
                     PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                     CANCEL SPOOL
                     MOVE MENU-GERAL TO FUNCAO-PROGRAMA
                     DISPLAY (1, 1) ERASE
                     PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                END-IF
           ELSE
           IF   SP2-CD-KEY = SP2-KEY-F3
                MOVE 0 TO SP2-CD-MENU-ID
                IF   HELP  = ZERO
                     MOVE CWCONF-CHAVE TO SALVA-CHAVE
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     MOVE ZERO TO OPCAO-2
                     PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                     PERFORM 150-CWCONFIGURA THRU 150-99-FIM
                     PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                     SET CWSQLC-UPDATE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     MOVE "PS" TO CWCONF-REG
                     MOVE NOME TO CWCONF-NOME
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   FS-CWCONF < "10"
                          IF   CWCONF-PRINTER-DEFAULT = SPACES
                          OR   LOW-VALUES
                               MOVE "<Default>"
                                 TO CWCONF-PRINTER-DEFAULT
                          END-IF
                          MOVE CWCONF-PRINTER-DEFAULT TO IMPRESSORA
                     END-IF
                     PERFORM 170-CHECK-SPOOL THRU 170-99-FIM
                     MOVE SALVA-CHAVE TO CWCONF-CHAVE
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF  (NOT MUDA-EMPRESA)
                     AND (NOT MUDA-SENHA)
                     AND (NOT MUDA-SISTEMA)
                     AND (NOT MUDA-NADA)
                          MOVE MENU-GERAL          TO FUNCAO-PROGRAMA
                          MOVE "CWMENU"            TO PROGRAMA
                          DISPLAY (1, 1) ERASE
                          PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                     END-IF
                END-IF
           ELSE
           IF   SP2-CD-KEY = SP2-KEY-F1
                MOVE 0 TO SP2-CD-MENU-ID
                IF   HELP = ZERO
                     MOVE 1 TO HELP
                     PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
                ELSE
                     MOVE ZERO TO HELP
                     MOVE MENU-GERAL TO FUNCAO-PROGRAMA
                     PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM.

           IF   SP2-CD-KEY = SP2-KEY-PGDN
                MOVE 0 TO SP2-CD-MENU-ID
                MOVE PAGINA TO SALVA-PAGINA
                ADD  1             TO PAGINA
                MOVE PAGINA  TO CWCONF-PAGINA
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF NOT = "00"
                     MOVE SALVA-PAGINA TO CWCONF-PAGINA PAGINA
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                END-IF
                MOVE ZERO TO OPCAO SP2-CD-MENU-ID
           ELSE
                IF   SP2-CD-KEY = SP2-KEY-PGUP
                     MOVE 0 TO SP2-CD-MENU-ID
                     MOVE PAGINA TO SALVA-PAGINA
                     SUBTRACT 1 FROM PAGINA
                     MOVE PAGINA TO CWCONF-PAGINA
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   FS-CWCONF NOT = "00"
                          MOVE SALVA-PAGINA TO CWCONF-PAGINA
                                               PAGINA
                          SET CWSQLC-READ TO TRUE
                          SET CWSQLC-EQUAL TO TRUE
                          SET CWSQLC-IGNORE-LOCK TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                     END-IF
                     MOVE ZERO TO OPCAO SP2-CD-MENU-ID
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       120-TIME.

           SET CWTIME-NORMAL      TO TRUE
           SET CWTIME-TODAY       TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           SET CWTIME-WEEK        TO TRUE
           MOVE CWTIME-DATE-FINAL TO CWTIME-DATE
           CALL "CWTIME"       USING PARAMETROS-CWTIME.

       120-99-FIM. EXIT.

       130-GRAVA-CWLOGF.

           PERFORM 131-DATE-TIME THRU 131-99-FIM

           IF ((PROGRAMA (1: 5) = "CWMEN")
           AND (PROGRAMA NOT = "CWMENU"))
           OR ((PROGRAMA (1: 5) = "CWMEN")
           AND  FS-CWLOGF NOT = "73")
           OR   CWGETL-LOG = 0
           OR   HELP = 1
                EXIT PARAGRAPH
           END-IF

           IF  (PROGRAMA = 'CWREL2' OR 'XSDRUN')
           AND  RELATORIO NOT = SPACES
                MOVE PROGRAMA  TO XSDRUN
                MOVE RELATORIO TO PROGRAMA
           END-IF

           CALL "CWGETU" USING NOME TASK PROGRAMA "#"
           MOVE FUNCAO-PROGRAMA    TO OBS
           CALL "CWLOGW" USING "#" OBS.

       130-99-FIM. EXIT.

       131-DATE-TIME.

           SET CWTIME-REVERSED     TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL  TO HOJE
           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL  TO CWTIME-TIME
           SET CWTIME-EDIT         TO TRUE
           MOVE CWTIME-DATE-FINAL  TO CWTIME-DATE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED TO DATA-DE-HOJE
           MOVE CWTIME-TIME-EDITED TO HORA.

       131-99-FIM. EXIT.

       140-CHECK-NOME.

           MOVE ZERO        TO HELP
           MOVE MENU-GERAL  TO FUNCAO-PROGRAMA
           MOVE SPACES      TO CHECK-PASS-3
           MOVE  CWGETL-LOG TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG

           MOVE QUADRO-L TO QUADRO
           PERFORM 146-SET-QUADRO
           IF   LOGON-TYPE = 1
                MOVE "N"    TO RESPOSTA
                GO TO 140-MASTER
           ELSE
                MOVE SPACES TO RESPOSTA
           END-IF
           IF   LOGON-TYPE = 3
                MOVE 1 TO LOGON-TYPE
           END-IF.

       LOGIN.

           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           MOVE SPACE TO RESPOSTA

           IF  NOT NOME-OK
           OR  NOT SENHA-OK
               MOVE 1 TO ENTROU
               IF   NOME = "LOGON"
                    MOVE SPACES TO NOME
               END-IF
               MOVE "N" TO CHECK-SENHA
               IF  CWLOGPGM NOT = SPACES
                   CALL CWLOGPGM USING NOME SENHA
                                       RESPOSTA
                                       NOME-C SENHA-C
                   ON EXCEPTION
                      CONTINUE
                   NOT ON EXCEPTION
                       GO TO LOGIN-OK
                   END-CALL
               END-IF
               IF  NOME-OK
               OR (NOME-C NOT = SPACES)
                   IF  NOME-C = SPACES
                   AND (NOME NOT = SPACES)
                       MOVE NOME TO NOME-C
                   END-IF
                   IF  SENHA-C = SPACES
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR LOGON-COLOR
                            Caption(1) "Usu†rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) NOME-C (Protected(1))
                            Data(2) SENHA-C;SENHA
                            CANCEL ;RESPOSTA
                            COLOR 23
                       END-EXEC
                   ELSE
                       MOVE SENHA-C TO SENHA
                   END-IF
                   MOVE NOME-C TO NOME
               ELSE
                   IF  SENHA-C NOT = SPACES
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR LOGON-COLOR
                            Caption(1) "Usu†rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) NOME-C;NOME
                            Data(2) SENHA-C (Protected(2))
                            CANCEL ;RESPOSTA
                       END-EXEC
                       MOVE SENHA-C TO SENHA
                   ELSE
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR LOGON-COLOR
                            Caption(1) "Usu†rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) NOME-C;NOME
                            Data(2) SENHA-C;SENHA
                            CANCEL ;RESPOSTA
                       END-EXEC
                   END-IF
               END-IF
               IF  RESPOSTA = "Y"
                   MOVE "N" TO CHECK-NOME
                               CHECK-SENHA
                               RESPOSTA
               ELSE
                   MOVE SPACE TO RESPOSTA
               END-IF
           END-IF.

       LOGIN-OK.

           IF  (NOT NOME-OK)
           AND (RESPOSTA NOT = "N")
               IF   CHECK-NOME = "+"
                    MOVE "S" TO CHECK-NOME
               END-IF
               IF   NOME-C = SPACES
                    MOVE 1     TO PAGINA
                    MOVE 9999  TO ULTIMA-PAGINA
                    MOVE SPACE TO FLAG-OPCOES
               ELSE
                    MOVE NOME-C TO NOME
                    MOVE SPACES TO NOME-C
               END-IF
               INSPECT NOME CONVERTING MINUSCULAS TO MAIUSCULAS
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               MOVE "PS" TO CWCONF-REG
               MOVE NOME TO CWCONF-NOME
               SET CWSQLC-READ TO TRUE
               SET CWSQLC-EQUAL TO TRUE
               SET CWSQLC-IGNORE-LOCK TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF   NOME NOT = SPACES
               AND  FS-CWCONF = "00"
                    CALL "CWMENL" USING FS-CWCONF
                                        CWCONF-NOME
                                        CWCONF-GRUPO
                    CANCEL "CWMENL"
               END-IF
               PERFORM 250-AJUSTA-TTY-NOME THRU 250-99-FIM
               IF   FS-CWCONF NOT = "00"
               OR   NOME = SPACES
               OR   NOME = LOW-VALUES
                    MOVE "Informe nome do usu†rio."
                      TO CWSEND-MSG
                    IF   NOME NOT = LOW-VALUES
                    AND  NOME NOT = SPACES
                         MOVE SPACES TO CWSEND-MSG
                         PERFORM VARYING I FROM LENGTH NOME
                                     BY -1
                                  UNTIL NOME (I: 1) NOT = SPACE
                                 CONTINUE
                         END-PERFORM
                         STRING 'Usu†rio "' DELIMITED BY SIZE
                               NOME (1: I) DELIMITED BY SIZE
                         '" n∆o autorizado.' DELIMITED BY SIZE
                                       INTO CWSEND-MSG
                    END-IF
                    CALL "CWSEND" USING PARAMETROS-CWSEND
               ELSE
                    IF   CWCONF-SENHA = SPACES
                         CALL "CWPASS" USING NOME
                         CANCEL "CWPASS"
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         SET SENHA-OK TO TRUE
                    END-IF
                    MOVE "S"          TO CHECK-NOME
                    MOVE CWCONF-GRUPO TO GRUPO
                    display 'cwsgroup' upon environment-name
                    display grupo      upon environment-value
                    MOVE CWCONF-NIVEL-PS TO CHECK-NIVEL
                    PERFORM 145-VALIDADE THRU 145-99-FIM
               END-IF
               PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
           END-IF

           IF  CWGETL-RETRY > 2
           AND(CWCONF-LOGIN-ERRO NOT LESS CWGETL-RETRY)
               IF  CWCONF-NIVEL-PS = 9
               AND HOJE > CWCONF-LOGIN-LAST
                   MOVE 0 TO CWCONF-LOGIN-ERRO
               ELSE
                   EXEC COBOLware Send
                        Message
                        "Senha expirada por violaá∆o de seguranáa."
                   END-EXEC
                   MOVE 2 TO EXPIRADA
               END-IF
           END-IF

           IF  CWCONF-BLOQUEADO = 1
               EXEC COBOLware Send
                  Message "Usu†rio bloqueado pelo administrador."
               END-EXEC
               MOVE "N" TO CHECK-NOME
           END-IF

           IF  NOT NOME-OK
           AND (RESPOSTA NOT = "N")
               GO TO LOGIN
           END-IF

           IF  NOME-OK
           AND (NOT SENHA-OK)
           AND (RESPOSTA NOT = "N")
               DISPLAY "FSSERVER" UPON ENVIRONMENT-NAME
               ACCEPT  FSSERVER   FROM ENVIRONMENT-VALUE
               IF  FSSERVER NOT = SPACES
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   MOVE "PS" TO CWCONF-REG
                   MOVE NOME TO CWCONF-NOME
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF  FS-CWCONF < "10"
                       MOVE CWCONF-SENHA   TO SENHA-AUTO
                       MOVE CWCONF-ESQUECI TO ESQUECI
                       CALL "CWFSPW" USING CWCONF-CHAVE
                                           CWCONF-SENHA
                                           CWCONF-SIZE-PS
                                           CWCONF-FATOR-PS
                                           CWCONF-ESQUECI-SIZE
                                           CWCONF-ESQUECI-FATOR
                                           CWCONF-ESQUECI
                       IF  (CWCONF-SENHA   NOT = SENHA-AUTO)
                       OR  (CWCONF-ESQUECI NOT = ESQUECI)
                            SET CWSQLC-REWRITE TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                       END-IF
                   END-IF
               END-IF
               MOVE CWCONF-SENHA TO SENHA-AUTO
               CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                       CWCONF-FATOR-PS
                                       SENHA-AUTO
               INSPECT SENHA-AUTO CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
               INSPECT SENHA CONVERTING MINUSCULAS TO MAIUSCULAS
               IF   CWCONF-PRINTER-DEFAULT = SPACES
               OR   LOW-VALUES
                    MOVE "Spool" TO CWCONF-PRINTER-DEFAULT
               END-IF
               SET CWTIME-NORMAL       TO TRUE
               SET CWTIME-TODAY        TO TRUE
               CALL "CWTIME"        USING PARAMETROS-CWTIME
               MOVE 0 TO SENHA-PROV
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
                       IF   SENHA-X (I) NOT = 32
                            ADD SENHA-X (I) TO SENHA-PROV
                       END-IF
               END-PERFORM
               MOVE CWTIME-DATE-FINAL (8: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-TIME-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (3: 2) TO N
               ADD  N                   TO SENHA-PROV
               COMPUTE SENHA-PROV = (1 / SENHA-PROV) * 100000000
               IF   SENHA = SENHA-AUTO
               OR   SENHA (1: 6) = SENHA-PROV
                    IF   SENHA (1: 6) = SENHA-PROV
                         CALL "CWGETU" USING NOME TASK PROGRAMA "$"
                    ELSE
                         CALL "CWGETU" USING NOME TASK PROGRAMA "%"
                    END-IF
      *             MOVE 0 TO CWCONF-LOGIN-ERRO
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    CALL "CWGETU" USING NOME TASK PROGRAMA "3"
                    MOVE 1                      TO LOGIN-FLAG
                    MOVE CWCONF-PRINTER-DEFAULT TO IMPRESSORA
                    MOVE "S"                    TO CHECK-SENHA
                    MOVE CWCONF-NIVEL-PS        TO CHECK-NIVEL
               ELSE
                    IF   CWCONF-LOGIN-ERRO < 3
                    AND (SENHA NOT = SPACES)
                         ADD 1 TO CWCONF-LOGIN-ERRO
                         SET CWSQLC-REWRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                    END-IF
                    IF   SENHA = SPACES
                         MOVE "Informe sua senha." TO CWSEND-MSG
                         IF   CWCONF-ESQUECI NOT = SPACES
                              MOVE "~OK"       TO CWSEND-SCREEN(1)
                              MOVE "~Esqueci"  TO CWSEND-SCREEN(2)
                         END-IF
                    ELSE
                         IF CWGETL-RETRY < 3
                            MOVE 0 TO CWCONF-LOGIN-ERRO
                         END-IF
                         PERFORM VARYING I FROM LENGTH NOME
                              BY -1 UNTIL I = 1
                                     OR (NOME (I:1) <> ' ')
                                 CONTINUE
                         END-PERFORM
                         MOVE SPACES TO OBS
                         STRING '"' DELIMITED BY SIZE
                                NOME(1:I) DELIMITED BY SIZE
                               '", com senha incorreta.'
                               DELIMITED BY SIZE
                           INTO OBS
                         CALL "CWLOGW" USING "#" OBS
                         EVALUATE CWCONF-LOGIN-ERRO
                             WHEN 1 THRU (CWGETL-RETRY - 2)
                                  MOVE
               "Senha incorreta, pode ser bloqueada se insistir."
                               TO CWSEND-MSG
                             WHEN (CWGETL-RETRY - 1)  MOVE
                    "Senha incorreta, ser† bloqueada no pr¢ximo erro."
                               TO CWSEND-MSG
                             WHEN CWGETL-RETRY MOVE
                                   "Senha incorreta, bloqueada."
                                      TO CWSEND-MSG
                                  STRING '"' DELIMITED BY SIZE
                                         NOME(1:I) DELIMITED BY SIZE
                                        '", bloqueado pelo sistema'
                                        DELIMITED BY SIZE
                                    INTO OBS
                                  CALL "CWLOGW" USING "#" OBS
                             WHEN OTHER
                                  MOVE "Senha incorreta." TO CWSEND-MSG
                         END-EVALUATE
                    END-IF
                    CALL "CWSEND" USING PARAMETROS-CWSEND
                    MOVE SPACES TO SENHA SENHA-C
                    MOVE SPACES TO CWSEND-SCREENS
                    IF   CWSEND-OPTION = 2
                         CALL "CWCODE" USING "D" CWCONF-ESQUECI-SIZE
                                                 CWCONF-ESQUECI-FATOR
                                                 CWCONF-ESQUECI
                         PERFORM VARYING LEN-LEMBRETE FROM
                                 LENGTH OF CWCONF-RESPOSTA BY -1
                                 UNTIL CWCONF-RESPOSTA
                                 (LEN-LEMBRETE: 1) <> SPACE
                                  CONTINUE
                         END-PERFORM
                         EXEC COBOLware BoxDialog
                              LINE 11 COLUMN 22
                              HEADER "Lembrete da senha"
                              Caption(1) CWCONF-PERGUNTA
                              Size(1) LEN-LEMBRETE (Secure(1))
                             Data(1) ESQUECI-RESPOSTA;ESQUECI-RESPOSTA
                             Color 23
                         END-EXEC
                         INSPECT ESQUECI-RESPOSTA
                                 CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
                         INSPECT ESQUECI-RESPOSTA
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-OFF
                         INSPECT CWCONF-RESPOSTA
                                 CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
                         INSPECT CWCONF-RESPOSTA
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-OFF
                         IF   CWCONF-RESPOSTA = ESQUECI-RESPOSTA
                              CALL "CWCODE" USING "C"
                                                  CWCONF-ESQUECI-SIZE
                                                  CWCONF-ESQUECI-FATOR
                                                  CWCONF-ESQUECI
                              MOVE SPACES TO CWCONF-SENHA
                              SET CWSQLC-REWRITE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                              CALL "CWPASS" USING NOME
                              CANCEL "CWPASS"
                              SET CWSQLC-READ TO TRUE
                              SET CWSQLC-EQUAL TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                              GO TO LOGIN
                         ELSE
                              EXEC COBOLware Send
                              Message
                              "Resposta errada contate o administrador"
                              END-EXEC
                              SET CWSQLC-CLOSE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                              STOP RUN
                         END-IF
                    END-IF
               END-IF
           END-IF

           IF  NOT SENHA-OK
           AND (RESPOSTA NOT = "N")
               GO TO LOGIN
           END-IF

           IF   NOME-OK
           AND  SENHA-OK
BORGH           MOVE 0 TO CWCONF-LOGIN-ERRO
                IF  EXPIRADA = 2
                    MOVE 1 TO EXPIRADA
                ELSE
                    MOVE 0 TO EXPIRADA
                END-IF
                MOVE HOJE TO CWCONF-LOGIN-LAST
                IF   CWCONF-DATA-SENHA = 0
                OR   CWCONF-DATA-SENHA = LOW-VALUES
                OR  (CWCONF-DATA-SENHA NOT NUMERIC)
                     MOVE HOJE TO CWCONF-DATA-SENHA
                ELSE
                     IF  EXPIRE NOT = 0
                         EXEC COBOLware Time (Interval) (AAAAMMDD)
                              Date CWCONF-DATA-SENHA
                              Date-Final HOJE
                              DAYS-FINAL;DIAS-SENHA
                         END-EXEC
                         IF   DIAS-SENHA > EXPIRE
                              EXEC COBOLware Send
           Message "Senha expirada opá‰es bloqueadas, altere sua senha."
                              END-EXEC
                              MOVE SPACES TO CWCONF-SENHA
                              SET CWSQLC-REWRITE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
      *                       MOVE 1 TO EXPIRADA
                              CALL "CWPASS" USING NOME
                              CANCEL "CWPASS"
                              SET CWSQLC-READ TO TRUE
                              SET CWSQLC-EQUAL TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                         END-IF
                     END-IF
                END-IF
                SET CWSQLC-REWRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   ENTROU = 1
           AND  NOME-OK
           AND  SENHA-OK
                CALL "CWLOCK" USING "L" NOME TASK
                MOVE 0                  TO ENTROU
                DISPLAY (1, 1) ERASE
           END-IF

           PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE.

       140-MASTER.

           IF   NOME-OK
           AND  SENHA-OK
           AND  COMMAREA01 = ALL X"B0"
                DISPLAY "CWCOMMAREA"    UPON ENVIRONMENT-NAME
                ACCEPT   CWCOMMAREA    FROM ENVIRONMENT-VALUE
                INSPECT  CWCOMMAREA  CONVERTING MINUSCULAS TO MAIUSCULAS
                IF  CWCOMMAREA = 'SPACES'
                OR  'SPACE'
                OR  'BRANCO'
                OR  'BRANCOS'
                OR  'ESPACOS'
                OR  'ESPACO'
                OR  'BLANK'
                OR  'BLANKS'
                    MOVE SPACES TO COMMAREA01
                                   COMMAREA02
                                   COMMAREA03
                                   COMMAREA04
                                   COMMAREA05
                                   COMMAREA06
                                   COMMAREA07
                                   COMMAREA08
                                   COMMAREA09
                                   COMMAREA10
                ELSE
                    MOVE ALL X"00" TO COMMAREA01
                                      COMMAREA02
                                      COMMAREA03
                                      COMMAREA04
                                      COMMAREA05
                                      COMMAREA06
                                      COMMAREA07
                                      COMMAREA08
                                      COMMAREA09
                                      COMMAREA10
                END-IF
                IF   CWGETL-MASTER NOT = SPACES
                     CALL CWGETL-MASTER USING COMMAREA01
                                              COMMAREA02
                                              COMMAREA03
                                              COMMAREA04
                                              COMMAREA05
                                              COMMAREA06
                                              COMMAREA07
                                              COMMAREA08
                                              COMMAREA09
                                              COMMAREA10
                          ON OVERFLOW
                             MOVE SPACES TO CWSEND-MSG
                             STRING "Imposs°vel executar o programa "
                                     DELIMITED BY SIZE
                                     CWGETL-MASTER DELIMITED BY SPACE
                             INTO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             END-CALL
                             STOP RUN
                     END-CALL
                     IF   COMMAREA01 (1: 4) = "ERRO"
                          CANCEL CWGETL-MASTER
                          MOVE ALL X"B0" TO COMMAREA01
                          MOVE SPACE     TO CHECK-NOME
                                            CHECK-SENHA
                          MOVE 0         TO LOGON-TYPE
                          GO TO 140-CHECK-NOME
                     END-IF
                END-IF
           END-IF

           IF   CWGETL-LOGIN NOT = SPACES
           AND  LOGIN-FLAG = 1
                MOVE 0 TO LOGIN-FLAG
                CALL CWGETL-LOGIN USING COMMAREA01
                                        COMMAREA02
                                        COMMAREA03
                                        COMMAREA04
                                        COMMAREA05
                                        COMMAREA06
                                        COMMAREA07
                                        COMMAREA08
                                        COMMAREA09
                                        COMMAREA10
                     ON OVERFLOW
                        MOVE SPACES TO CWSEND-MSG
                        STRING "Imposs°vel executar o programa "
                                DELIMITED BY SIZE
                                CWGETL-LOGIN DELIMITED BY SPACE
                        INTO CWSEND-MSG
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                        END-CALL
                        STOP RUN
                END-CALL
                IF   COMMAREA01 (1: 4) = "ERRO"
                     MOVE SPACE     TO CHECK-NOME
                                       CHECK-SENHA
                     MOVE 0         TO LOGON-TYPE
                     GO TO 140-CHECK-NOME
                 END-IF
                 PERFORM 166-PERSONAL THRU 166-99-FIM
           END-IF.

       140-99-FIM. EXIT.

       145-VALIDADE.

           IF   CWCONF-QUADRO-PS NUMERIC
                MOVE CWCONF-QUADRO-PS TO QUADRO
                PERFORM 146-SET-QUADRO
           END-IF

           IF   CWCONF-NIVEL-PS > 8
                GO TO 145-99-FIM
           END-IF

           MOVE "MX" TO CWCONF-REGLG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-MAXUSERS TO MAXUSERS
           END-IF

           IF   MAXUSERS NOT = 0
                CALL "CWLOGD" USING PARAMETROS-CWLOGD
                CANCEL "CWLOGD"
                IF   CWLOGD-USERS NOT < MAXUSERS
                     MOVE SPACES TO CWSEND-MSG
                     PERFORM VARYING I FROM 1 BY 1
                             UNTIL MAXUSERS (I: 1) NOT = "0"
                             CONTINUE
                     END-PERFORM
                     IF  MAXUSERS = 1
                         MOVE "Sistema monousu†rio ocupado"
                           TO CWSEND-MSG
                     ELSE
                          STRING "Excedeu o limite licenciado de "
                                  MAXUSERS (I: )
                                  " usu†rios simultÉneos"
                                  DELIMITED BY SIZE
                            INTO CWSEND-MSG
                     END-IF
                     MOVE " ~Fechar_" TO CWSEND-SCREEN (1)
                     MOVE "~Detalhes" TO CWSEND-SCREEN (2)
                     PERFORM TEST AFTER UNTIL CWSEND-OPTION NOT = 2
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                             IF   CWSEND-OPTION = 2
                                  CALL "CWLOGD"
                                  CANCEL "CWLOGD"
                             END-IF
                     END-PERFORM
                     MOVE SPACES TO CWSEND-SCREENS
                     MOVE "N"         TO CHECK-NOME
                     GO TO 145-ABORT
                END-IF
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       145-VALIDADE-RETRY.

           MOVE "VD" TO CWCONF-REGLG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF < "10"
                SET CWTIME-REVERSED     TO TRUE
                SET CWTIME-TODAY        TO TRUE
                CALL "CWTIME"        USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL  TO VALIDADE
                IF  CWCONF-FLAG-2000 NOT = 1
                    MOVE 1                   TO CWCONF-FLAG-2000
                    MOVE CWCONF-OLD-VALIDADE TO CWCONF-VALIDADE
                    ADD  1900                TO CWCONF-VALIDADE
                    MOVE VALIDADE            TO CWCONF-ULTIMO-LOGIN-DATA
                    MOVE 99999999            TO CWCONF-VALIDADE-10
                                                CWCONF-VALIDADE-15
                                                CWCONF-VALIDADE-20
                    MOVE CWTIME-TIME-FINAL   TO CWCONF-ULTIMO-LOGIN-HORA
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                END-IF
                MOVE CWTIME-TIME-FINAL        TO LOGIN-HORA
                MOVE CWCONF-VALIDADE          TO VALIDADE2
                MOVE CWCONF-ULTIMO-LOGIN-DATA TO ULTIMO-LOGIN-DATA
                MOVE CWCONF-ULTIMO-LOGIN-HORA TO ULTIMO-LOGIN-HORA
                MOVE CWCONF-VALIDADE          TO VALIDADE-ATIVA
                IF   CWCONF-VALIDADE-10 NOT = 99999999
                     MOVE CWCONF-VALIDADE-10 TO VALIDADE-ATIVA
                END-IF
                IF   CWCONF-VALIDADE-15 NOT = 99999999
                     MOVE CWCONF-VALIDADE-15 TO VALIDADE-ATIVA
                END-IF
                IF   CWCONF-VALIDADE-20 NOT = 99999999
                     MOVE CWCONF-VALIDADE-20 TO VALIDADE-ATIVA
                END-IF
                MOVE ULTIMO-LOGIN TO TOLERANCIA
                PERFORM 75 TIMES
                        IF   TOLERANCIA-MM = 0
                             MOVE 59 TO TOLERANCIA-MM
                             IF   TOLERANCIA-HH = 0
                                  MOVE 23 TO TOLERANCIA-HH
                                  SET CWTIME-REVERSED TO TRUE
                                  SET CWTIME-SUBTRACT-DAYS TO TRUE
                                  MOVE ULTIMO-LOGIN-DATA TO CWTIME-DATE
                                  MOVE 1 TO CWTIME-DAYS
                                  CALL "CWTIME" USING PARAMETROS-CWTIME
                                  MOVE CWTIME-DATE-FINAL
                                    TO TOLERANCIA-DATA
                             ELSE
                                  SUBTRACT 1 FROM TOLERANCIA-HH
                             END-IF
                        ELSE
                             SUBTRACT 1 FROM TOLERANCIA-MM
                        END-IF
                END-PERFORM
                IF   ESTE-LOGIN < TOLERANCIA
                AND  CWCONF-DESTRAVA = "S"
                AND  CWCONF-TENTATIVAS > 2
                     MOVE 0 TO CWCONF-TENTATIVAS
                     MOVE ESTE-LOGIN TO ULTIMO-LOGIN
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                END-IF
                IF   ESTE-LOGIN < TOLERANCIA
                     DISPLAY 'COMPUTERNAME' UPON ENVIRONMENT-NAME
                     ACCEPT   COMPUTERNAME  FROM ENVIRONMENT-VALUE
                     IF COMPUTERNAME NOT = SPACES
                        MOVE SPACES TO OBS
                        STRING "Rel¢gio/Calend†rio:" DELIMITED BY SIZE
                               COMPUTERNAME         DELIMITED BY SPACE
                          INTO OBS
                        CALL "CWLOGW" USING "#" OBS
                     END-IF
                     MOVE "Violaá∆o de seguranáa: (Rel¢gio/Calend†rio)"
                       TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     MOVE "N"         TO CHECK-NOME
                     IF  CWCONF-DESTRAVA = "S"
                         ADD 1 TO CWCONF-TENTATIVAS
                         SET CWSQLC-REWRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                     END-IF
                     GO TO 145-ABORT
                END-IF
                IF   VALIDADE > VALIDADE-ATIVA
                OR   CWCONF-TRAVADO = "S"
                     MOVE "S" TO CWCONF-TRAVADO
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     MOVE "N"             TO CHECK-NOME
                     IF CWCONF-SENHA-ATIVACAO NUMERIC
                        IF   CWCONF-ULTIMO-LOGIN-DATA NUMERIC
                        AND  ESTE-LOGIN < ULTIMO-LOGIN
                             MOVE CWCONF-VALIDADE
                               TO CWCONF-ULTIMO-LOGIN-DATA
                             MOVE 0 TO CWCONF-ULTIMO-LOGIN-HORA
                             ADD 1 TO VALIDADE2-MM
                             IF VALIDADE2-MM > 12
                                MOVE 1 TO VALIDADE2-MM
                                ADD  1 TO VALIDADE2-AA
                             END-IF
                             MOVE VALIDADE2 TO VALIDADE
                        END-IF
                        PERFORM UNTIL
                                VALIDADE (1: 6) NOT > VALIDADE2 (1: 6)
                                ADD 1 TO VALIDADE2-MM
                                IF VALIDADE2-MM > 12
                                   MOVE 1 TO VALIDADE2-MM
                                   ADD  1 TO VALIDADE2-AA
                                END-IF
                        END-PERFORM
                        MOVE 14 TO CWBOXW-LINE
                        MOVE 26 TO CWBOXW-COLUMN
                        IF   CWCONF-EXIBE-LICENCA = "S"
                             MOVE 07 TO CWBOXW-VERTICAL-LENGTH
                        ELSE
                             MOVE 05 TO CWBOXW-VERTICAL-LENGTH
                        END-IF
                        MOVE 43 TO CWBOXW-HORIZONTAL-LENGTH
                        MOVE 78 TO CWBOXW-COLOR-FRAME
                                   CWBOXW-COLOR-BORDER
                        SET CWBOXW-OPEN TO TRUE
                        CALL "CWBOXW" USING PARAMETROS-CWBOXW
                        MOVE CWCONF-SENHA-ATIVACAO TO LICENCA
                        MOVE SPACES                TO MSG-AT1
                                                      MSG-AT2
                        MOVE 0                     TO CHAVE-ATIVACAO
                        STRING "Prazo de validade vencido em "
                                VALIDADE2-DD "/" VALIDADE2-MM
                                             "/" VALIDADE2-AA
                                       DELIMITED BY SIZE
                                                 INTO MSG-AT1
                        STRING "Licenáa: "
                                LICENCA DELIMITED BY SIZE
                                                 INTO MSG-AT2
                        MOVE SPACES                   TO MSG-AT3
                        move VALIDADE2 to CWCONF-VALIDADE
                        PERFORM UNTIL VALIDADE < VALIDADE2
                                ADD 1 TO VALIDADE2-MM
                                IF VALIDADE2-MM > 12
                                   MOVE 1 TO VALIDADE2-MM
                                   ADD  1 TO VALIDADE2-AA
                                END-IF
                        END-PERFORM
                        STRING  "Chave de ativaá∆o atÇ "
                                VALIDADE2-DD "/" VALIDADE2-MM
                                             "/" VALIDADE2-AA ": "
                                       DELIMITED BY SIZE
                                                 INTO MSG-AT3
*******                 CALL "CWTEXT" USING MSG-AT LENGTH OF MSG-AT
                        CALL X"E5"
                        IF   CWFONE NOT = SPACES
                             PERFORM VARYING F FROM LENGTH OF CWFONE
                                         BY -1 UNTIL F = 1
                                           OR CWFONE (F: 1) NOT = SPACE
                                     CONTINUE
                             END-PERFORM
                             CALL "CWMSGW" USING PFONE CWFONE
                        END-IF
                        DISPLAY "CWACTF" UPON ENVIRONMENT-NAME
                        MOVE SPACES TO LB-TEXTO
                        ACCEPT LB-TEXTO  FROM ENVIRONMENT-VALUE
                        IF LB-TEXTO = SPACES
                           MOVE '$COBOLWARE/system.key'
                               TO LB-TEXTO
                        END-IF
                        OPEN INPUT TEXTO
                        IF FS-TEXTO = '00'
                           READ TEXTO INTO CHAVE-ATIVACAO(1:)
                           CLOSE TEXTO
                        END-IF
                        IF   CWCONF-EXIBE-LICENCA = "S"
                             CALL "CWMSGW" USING "162940" MSG-AT1
                             CALL "CWMSGW" USING "182940" MSG-AT2
                             CALL "CWMSGW" USING "202940" MSG-AT3
                             IF FS-TEXTO NOT = '00'
                                ACCEPT CHAVE-ATIVACAO AT 2063
                                    WITH UPDATE REVERSE-VIDEO PROMPT
                             END-IF
                        ELSE
                             CALL "CWMSGW" USING "162940" MSG-AT1
                             CALL "CWMSGW" USING "182940" MSG-AT3
                             IF FS-TEXTO NOT = '00'
                                ACCEPT CHAVE-ATIVACAO AT 1863
                                    WITH UPDATE REVERSE-VIDEO PROMPT
                             END-IF
                        END-IF
                        SET CWBOXW-CLOSE TO TRUE
                        CALL "CWBOXW" USING PARAMETROS-CWBOXW
                        IF   CHAVE-ATIVACAO NOT = 0
                             MOVE CWCONF-VALIDADE TO CWACTV-VALIDADE
                             SET CWTIME-REVERSED     TO TRUE
                             SET CWTIME-REVERSE      TO TRUE
                             MOVE CWACTV-VALIDADE    TO CWTIME-DATE
                             CALL "CWTIME"     USING PARAMETROS-CWTIME
                             MOVE CWTIME-DATE-FINAL  TO CWACTV-VALIDADE
                             MOVE CWCONF-SENHA-ATIVACAO TO CWACTV-SENHA
                             CALL "CWACTV" USING PARAMETROS-CWACTV
                             IF   CHAVE-ATIVACAO = CWACTV-ATIVACAO
                             OR   CHAVE-ATIVACAO = CWACTV-ATIVACAO-10
                             OR   CHAVE-ATIVACAO = CWACTV-ATIVACAO-15
                             OR   CHAVE-ATIVACAO = CWACTV-ATIVACAO-20
                                  MOVE "N"            TO CWCONF-TRAVADO
                                  SET  CWTIME-NORMAL  TO TRUE
                                  SET  CWTIME-REVERSE TO TRUE
                                  EVALUATE CHAVE-ATIVACAO
                                      WHEN CWACTV-ATIVACAO
                                           MOVE 99999999
                                             TO CWCONF-VALIDADE-10
                                                CWCONF-VALIDADE-15
                                                CWCONF-VALIDADE-20
                                           MOVE VALIDADE2
                                             TO CWCONF-VALIDADE
                                      WHEN CWACTV-ATIVACAO-10
                                           MOVE 99999999
                                             TO CWCONF-VALIDADE-15
                                                CWCONF-VALIDADE-20
                                           MOVE CWACTV-VALIDADE-10
                                             TO CWTIME-DATE
                                           CALL "CWTIME"
                                          USING PARAMETROS-CWTIME
                                           MOVE CWTIME-DATE-FINAL
                                             TO CWCONF-VALIDADE-10
                                      WHEN CWACTV-ATIVACAO-15
                                           MOVE 99999999
                                             TO CWCONF-VALIDADE-10
                                                CWCONF-VALIDADE-20
                                           MOVE CWACTV-VALIDADE-15
                                             TO CWTIME-DATE
                                           CALL "CWTIME"
                                          USING PARAMETROS-CWTIME
                                           MOVE CWTIME-DATE-FINAL
                                             TO CWCONF-VALIDADE-15
                                      WHEN CWACTV-ATIVACAO-20
                                           MOVE 99999999
                                             TO CWCONF-VALIDADE-10
                                                CWCONF-VALIDADE-15
                                           MOVE CWACTV-VALIDADE-20
                                             TO CWTIME-DATE
                                           CALL "CWTIME"
                                          USING PARAMETROS-CWTIME
                                           MOVE CWTIME-DATE-FINAL
                                             TO CWCONF-VALIDADE-20
                                  END-EVALUATE
                                  SET CWSQLC-REWRITE TO TRUE
                                  CALL "CWCONF" USING CWSQLC
                                                      CWCONF-REG
                                                      FS-CWCONF
                                                      KCO PCO
                                  MOVE "S" TO CHECK-NOME
                                  IF   CHAVE-ATIVACAO NOT =
                                       CWACTV-ATIVACAO
                                       GO TO 145-VALIDADE-RETRY
                                  END-IF
                             ELSE
                                  CALL X"E5"
                                  MOVE SPACES TO CWSEND-SCREENS
                                  MOVE "Chave de ativaá∆o inv†lida"
                                    TO CWSEND-MSG
                                  CALL "CWSEND" USING PARAMETROS-CWSEND
                                  DELETE FILE TEXTO
                             END-IF
                        END-IF
                     ELSE
                        CALL X"E5"
                        MOVE SPACES TO CWSEND-SCREENS
                        MOVE "Prazo de validade vencido" TO CWSEND-MSG
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                     END-IF
                ELSE
                     MOVE VALIDADE   TO CWCONF-ULTIMO-LOGIN-DATA
                     MOVE LOGIN-HORA TO CWCONF-ULTIMO-LOGIN-HORA
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                END-IF
           END-IF.

       145-ABORT.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "PS" TO CWCONF-REG
           MOVE NOME TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       145-99-FIM. EXIT.

       146-SET-QUADRO.

           MOVE BASE-MOLDURA (QUADRO) TO MOLDURA
           MOVE "M"                   TO SET-LOG
           MOVE TASK                  TO SAVE-TASK
           COMPUTE TASK = QUADRO - 1
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
           MOVE SAVE-TASK             TO TASK
           MOVE SPACES                TO HORIZONTAL
      *    INSPECT HORIZONTAL CONVERTING SPACE TO M-205.
           move '234567890' to horizontal.
           move all '1234567890' to horizontal (10: ).

       150-CWCONFIGURA.

           MOVE MENU-NO-OPCAO TO OPCAO-2

           IF   OVERLAY (OPCAO-2) NOT = SPACES
                MOVE    OVERLAY (OPCAO-2)  TO PROGRAMA
                MOVE    TIPO-CF2 (OPCAO-2) TO FUNCAO-PROGRAMA
                INSPECT FUNCAO-PROGRAMA CONVERTING "_" TO SPACE
                MOVE CWGETL-LOG            TO SET-LOG
                CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                DISPLAY (1, 1) ERASE
                MOVE OVERLAY (OPCAO-2) TO PROGRAMA
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
                DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
cavalo          MOVE PANOS-SAVE      TO PANOS
cavalo          DISPLAY "CWFULL"     UPON ENVIRONMENT-NAME
cavalo          DISPLAY PANO-COBOL   UPON ENVIRONMENT-VALUE
cavalo          DISPLAY "CWFULL"     UPON ENVIRONMENT-NAME
                DISPLAY  PANO-COBOL2 UPON ENVIRONMENT-VALUE
                EXIT PROGRAM
           END-IF
           IF   MUDA-EMPRESA
                PERFORM 160-LER-00 THRU 160-99-FIM
                EXEC COBOLware BoxDialog
                     LINE 11 COLUMN 22
                     HEADER "Nome da empresa em"
                     Caption(1) "Telas     "
                     Caption(2) "Relat¢rios"
                    Data(1) CWCONF-USUARIO  ;CWCONF-USUARIO   Size(1) 30
                    Data(2) CWCONF-USUARIO-P;CWCONF-USUARIO-P Size(2) 30
                     CANCEL OPT
                END-EXEC
                IF   OPT NOT = "^"
                     PERFORM 165-GRAVA-00 THRU 165-99-FIM
                     PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   MUDA-SENHA
                CALL "CWPASS" USING NOME "M"
                CANCEL "CWPASS"
           END-IF
           IF   MUDA-SISTEMA
                PERFORM 160-LER-00 THRU 160-99-FIM
                EXEC COBOLware BoxDialog
                     LINE 11 COLUMN 22
                     HEADER "Nome do sistema em"
                     Caption(1) "Telas     "
                     Caption(2) "Relat¢rios"
                    Data(1) CWCONF-SISTEMA  ;CWCONF-SISTEMA   Size(1) 30
                    Data(2) CWCONF-SISTEMA-P;CWCONF-SISTEMA-P Size(2) 30
                     CANCEL OPT
                END-EXEC
                IF   OPT NOT = "Y"
                     PERFORM 165-GRAVA-00 THRU 165-99-FIM
                     PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF.

       150-99-FIM. EXIT.

       160-LER-00.

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "00" TO CWCONF-REG00
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC
                               CWCONF-REG
                               FS-CWCONF
                               KCO PCO
           CALL "CWCODE" USING "D" CWCONF-SIZE-U
                                   CWCONF-FATOR-00-U
                                   CWCONF-USUARIO
           CALL "CWCODE" USING "D" CWCONF-SIZE-S
                                   CWCONF-FATOR-00-S
                                   CWCONF-SISTEMA
           CALL "CWCODE" USING "D" CWCONF-SIZE-UP
                                   CWCONF-FATOR-00-UP
                                   CWCONF-USUARIO-P
           CALL "CWCODE" USING "D" CWCONF-SIZE-SP
                                   CWCONF-FATOR-00-SP
                                   CWCONF-SISTEMA-P.

       160-99-FIM. EXIT.

       165-GRAVA-00.

           PERFORM TEST AFTER UNTIL DD NOT EQUAL ZERO
                   ACCEPT HHMMSSDD FROM TIME
                   IF   DD = 0
                        MOVE SS TO DD
                   END-IF
           END-PERFORM
           MOVE LENGTH OF CWCONF-USUARIO TO CWCONF-SIZE-U CWCONF-SIZE-UP
                                            CWCONF-SIZE-S CWCONF-SIZE-SP
           CALL "CWCODE" USING "C" CWCONF-SIZE-U
                                   CWCONF-FATOR-00-U
                                   CWCONF-USUARIO
           CALL "CWCODE" USING "C" CWCONF-SIZE-S
                                   CWCONF-FATOR-00-S
                                   CWCONF-SISTEMA
           CALL "CWCODE" USING "C" CWCONF-SIZE-UP
                                   CWCONF-FATOR-00-UP
                                   CWCONF-USUARIO-P
           CALL "CWCODE" USING "C" CWCONF-SIZE-SP
                                   CWCONF-FATOR-00-SP
                                   CWCONF-SISTEMA-P
           SET CWSQLC-REWRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM 166-PERSONAL THRU 166-99-FIM.

       165-99-FIM. EXIT.

       166-PERSONAL.

           MOVE "00" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE CWCONF-USUARIO  TO USUARIO
           CALL "CWCODE" USING "D" CWCONF-SIZE-U
                                   CWCONF-FATOR-00-U
                                   USUARIO
           CALL "CWVARX" USING USUARIO LENGTH OF USUARIO
           MOVE CWCONF-SISTEMA  TO SISTEMA
           CALL "CWCODE" USING "D" CWCONF-SIZE-S
                                   CWCONF-FATOR-00-S
                                   SISTEMA
           CALL "CWVARX" USING SISTEMA LENGTH OF SISTEMA
           MOVE SISTEMA TO APLICACAO.

       166-99-FIM. EXIT.

       155-EXIBE-TPR8.

             IF   INISHOW = "ON"
                  DISPLAY "CWINI"  UPON ENVIRONMENT-NAME
                  ACCEPT  LB-FSINI FROM ENVIRONMENT-VALUE
                  DISPLAY LB-FSINI AT 2501 WITH SIZE 79
             END-IF
             MOVE PROGRAMA TO SAVE-PROGRAMA
             IF   PROGRAMA = "CWREL1" OR "CWREL2" OR "CWREL3"
                  MOVE "RELATOR" TO PROGRAMA
             END-IF
             IF   PROGRAMA = "XSDRUN"
             AND  RELATORIO NOT = SPACES
                  MOVE RELATORIO TO PROGRAMA
             END-IF

             MOVE 1        TO NX
             PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
             MOVE 0        TO NX
             MOVE SAVE-PROGRAMA TO PROGRAMA.

       155-99-FIM. EXIT.

       170-CHECK-SPOOL.

           MOVE "03"       TO CWCONF-REG03
           MOVE IMPRESSORA TO CWCONF-ARQUIVO

           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE CWCONF-ARQUIVO      TO LB-PRNTER

           IF   FS-CWCONF < "10"
                MOVE IMPR TO LB-PRNTER-2
           ELSE
                MOVE "Spool" TO LB-PRNTER-2
           END-IF
           PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE.

       170-99-FIM. EXIT.

       180-EXIBE-OPCOES.

           MOVE 7      TO CWLINE-LINE
           MOVE 4      TO CWLINE-COLUMN
           MOVE SPACES TO CWLINE-SCREENS
           MOVE "99"   TO CWCONF-REG
           MOVE PAGINA TO CWCONF-PAGINA
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE SPACES     TO WS-OPCOES

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 26
                      OR FS-CWCONF NOT = "00"
                CALL "CWVARX" USING CWCONF-NM-OPCAO (I)
                          LENGTH OF CWCONF-NM-OPCAO (I)
                MOVE CWCONF-NM-OPCAO (I) TO TESTECHAR
                INSPECT CWCONF-NM-OPCAO (I)
                        CONVERTING X"98" TO X"20"
                IF   TESTECHAR = "/"
                     MOVE CWCONF-NM-OPCAO(I) (3:  ) TO WS-NM-OPCAO  (I)
                     MOVE CWCONF-NM-OPCAO(I) (2: 1) TO WS-OPCAO-CHAR(I)
                ELSE
                     MOVE CWCONF-NM-OPCAO(I)        TO WS-NM-OPCAO  (I)
                END-IF
eri>            MOVE 0      TO Y2
                MOVE SPACES TO FUNCAO-PROGRAMA
                PERFORM VARYING I2 FROM 1 BY 1
                          UNTIL I2 > LENGTH OF FUNCAO-PROGRAMA
                        IF WS-NM-OPCAO (I) (I2: 1) NOT = X"7E"
                           ADD 1 TO Y2
                           MOVE WS-NM-OPCAO (I) (I2: 1)
                             TO FUNCAO-PROGRAMA (Y2: 1)
                        END-IF
                END-PERFORM
                MOVE FUNCAO-PROGRAMA TO WS-NM-OPCAO (I)
                INSPECT FUNCAO-PROGRAMA CONVERTING "_" TO SPACE
                PERFORM 230-CHECK-ACESSO THRU 230-99-FIM
                IF   CWCONF-NIVEL (I) NOT NUMERIC
                     MOVE 0 TO CWCONF-NIVEL (I)
                END-IF
                IF  (CWCONF-NIVEL (I) > CHECK-NIVEL)
                OR  (WS-NM-OPCAO  (I) = SPACES)
                     MOVE ALL " "  TO WS-NM-OPCAO (I)
                     MOVE ZERO     TO CWCONF-NO-OPCAO (I)
                ELSE
                     ADD  CWCONF-NO-OPCAO (I) TO TESTE-OPCAO
                     IF   CWCONF-NO-OPCAO (I) NOT = 0
                          MOVE LIN-MOUSE  (I) TO LX
                          MOVE COL-MOUSE  (I) TO CX
                          PERFORM VARYING Y FROM 34 BY -1
                                  UNTIL CWCONF-NM-OPCAO (I) (Y: 1)
                                        NOT = SPACE
                                  CONTINUE
                          END-PERFORM
                          IF   CWCONF-NO-OPCAO (I) < 10
                               ADD 2 TO Y
                               ADD 1 TO CX
                          ELSE
                               ADD 3 TO Y
                          END-IF
                          IF   CWCONF-NO-OPCAO (I) NOT NUMERIC
                               MOVE 0 TO CWCONF-NO-OPCAO (I)
                          END-IF
                     END-IF
                END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
                   IF   CWCONF-NO-OPCAO (I) NOT NUMERIC
                        MOVE 0 TO CWCONF-NO-OPCAO (I)
                   END-IF
           END-PERFORM

           DISPLAY (1, 1) ERASE
           PERFORM 155-EXIBE-TPR8 THRU 155-99-FIM
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR.

       180-99-FIM. EXIT.

       190-MUDA-PATH.

           CANCEL 'CWIAEF'
           MOVE    "73"                TO FS-CWLOGF
           PERFORM 130-GRAVA-CWLOGF  THRU 130-99-FIM
           MOVE    "73"                TO FS-CWLOGF
           MOVE MENU-GERAL             TO FUNCAO-PROGRAMA
           MOVE MENU-HELP              TO HELP-PATH
           PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE    0                TO VEZ

           IF   HELP-PATH (1: 1) = "!"
                CANCEL 'CWATTR'
                CALL   'CWATTR'
                MOVE SPACES TO CWSEND-MSG
                               CWSEND-SCREENS
                STRING 'Vari†vel de ambiente ' DELIMITED BY SIZE
                                  HELP-PATH-99 DELIMITED BY SPACE
                       ' n∆o configurada'      DELIMITED BY SIZE
                INTO CWSEND-MSG
                DISPLAY HELP-PATH-99 UPON ENVIRONMENT-NAME
                MOVE SPACES TO  HELP-PATH
                ACCEPT  HELP-PATH   FROM ENVIRONMENT-VALUE
                IF HELP-PATH = SPACES
                   CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
           END-IF

           IF   HELP-PATH (1: 1) = "$"
                DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
                ACCEPT OLD-CWCONF FROM ENVIRONMENT-VALUE
                MOVE HELP-PATH (2: ) TO NEW-CWCONF
                DISPLAY NEW-CWCONF UPON ENVIRONMENT-VALUE
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF NOT = "00"
                     IF   FS-CWCONF = "30" OR "35"
                          MOVE SPACES      TO CWSEND-SCREENS
                          MOVE ERRO-CWCONF TO CWSEND-MSG
                          CALL "CWSEND" USING PARAMETROS-CWSEND
                     ELSE
                          CALL "CWCONF" USING "ISAM"
                     END-IF
                     DISPLAY "CWCONF"   UPON ENVIRONMENT-NAME
                     DISPLAY OLD-CWCONF UPON ENVIRONMENT-VALUE
                     PERFORM 000-INICIO THRU 050-INICIO
                     GO TO 190-99-FIM
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                CANCEL 'CWLOGW'
                GO TO 190-RETRY
           END-IF
           IF   CWUNIX-OFF
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            PRINTER-STATUS
           END-IF
           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
           MOVE OLD-DIRECTORY       TO NEW-DIRECTORY
           IF   CWUNIX-OFF
                MOVE "\"            TO OLD-DIRECTORY
           ELSE
                MOVE "/"            TO OLD-DIRECTORY
           END-IF
           MOVE NEW-DIRECTORY       TO OLD-DIRECTORY (2: )
           MOVE HELP-PATH (2: 1)    TO NEW-DRIVE

           IF   CWUNIX-ON
           OR  (NEW-DRIVE NOT = ":")
                MOVE HELP-PATH TO NEW-DIRECTORY
                                  ERRO-DIRECTORY (20: )
           ELSE
                IF   NEW-DRIVE = ":"
                     MOVE HELP-PATH (3: )    TO NEW-DIRECTORY
                                                ERRO-DIRECTORY (20: )
                     MOVE HELP-PATH (1: 1)   TO NEW-DRIVE
                                                ERRO-DRIVE (7: 1)
                     CALL "PC_SET_DRIVE"  USING NEW-DRIVE
                                      RETURNING RETURN-CODE
                     IF   RETURN-CODE NOT = 0
                          MOVE ERRO-DRIVE TO CWSEND-MSG
                          MOVE SPACES TO CWSEND-SCREENS
                          CALL "CWSEND" USING PARAMETROS-CWSEND
                          GO TO 190-99-FIM
                     END-IF
                     CANCEL "CWGETS"
                     CANCEL "CWGETL"
                END-IF
           END-IF

           IF   NEW-DIRECTORY NOT = SPACES
                CANCEL "CWGETS"
                CANCEL "CWGETL"
                INSPECT NEW-DIRECTORY CONVERTING X"20" TO X"00"
                CALL "CBL_CHANGE_DIR" USING NEW-DIRECTORY
                                  RETURNING RETURN-CODE
                IF   RETURN-CODE = 0
                     MOVE 0 TO RETRY-CWCONF
                     CALL "CWLOCK" USING "U" NOME TASK
                     CALL "CWLOCK" USING "M" NOME TASK PROGRAMA
                     IF  FSSERVER = SPACES
                         CANCEL 'CWLOGW'
                     END-IF
                ELSE
                     IF   CWUNIX-OFF
                          CALL "PC_SET_DRIVE"   USING OLD-DRIVE
                     END-IF
                     INSPECT OLD-DIRECTORY CONVERTING X"20" TO X"00"
                     CALL "CBL_CHANGE_DIR" USING OLD-DIRECTORY
                     INSPECT ERRO-DIRECTORY CONVERTING X"00" TO X"20"
                     MOVE 70              TO Y
                     PERFORM TEST AFTER
                            UNTIL Y = 1
                               OR ERRO-DIRECTORY (Y: 1) NOT = SPACES
                             SUBTRACT 1 FROM Y
                     END-PERFORM
                     ADD 2 TO Y
                     MOVE SPACES TO CWSEND-SCREENS
                     MOVE ERRO-DIRECTORY TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     PERFORM 000-INICIO THRU 050-INICIO
                     GO TO 190-99-FIM
                END-IF
           END-IF.

       190-RETRY.

           IF   CHECK-PASS-3 = SPACES
                SET CWSQLC-UPDATE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF NOT = "00"
                     IF   RETRY-CWCONF = 0
                     AND (FS-CWCONF = "30" OR "35")
                          CALL "CW3050"
                          GO TO 190-RETRY
                     END-IF
                     IF   CWUNIX-OFF
                          CALL "PC_SET_DRIVE"   USING OLD-DRIVE
                     END-IF
                     INSPECT OLD-DIRECTORY CONVERTING X"20" TO X"00"
                     CALL "CBL_CHANGE_DIR" USING OLD-DIRECTORY
                     MOVE SPACES TO CWSEND-SCREENS
                     MOVE ERRO-CWCONF TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                ELSE
                     MOVE 1    TO PAGINA
                     MOVE 0    TO SALVA-PAGINA
                                  RETRY-CWCONF
                     DISPLAY "PGID" UPON ENVIRONMENT-NAME
                     DISPLAY '0'    UPON ENVIRONMENT-VALUE
                     MOVE "PS" TO CWCONF-REG
                     MOVE NOME TO CWCONF-NOME
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   FS-CWCONF > "09"
                     OR   CWCONF-BLOQUEADO = 1
                          MOVE SPACES TO CWSEND-MSG
                          STRING "Acesso do usu†rio " DELIMITED SIZE
                                  NOME                DELIMITED SPACE
                                  " n∆o permitido !"  DELIMITED SIZE
                                  INTO CWSEND-MSG
                          MOVE 1            TO CWSEND-OPTION
                          MOVE "~Retorna"   TO CWSEND-SCREEN (1)
                          MOVE "~Encerra"   TO CWSEND-SCREEN (2)
                          MOVE " ~Outro"   TO CWSEND-SCREEN (3)
                          PERFORM TEST AFTER UNTIL CWSEND-OPTION NOT = 0
                                  CALL "CWSEND" USING PARAMETROS-CWSEND
                          END-PERFORM
                          EVALUATE CWSEND-OPTION
                              WHEN 1 SET CWSQLC-CLOSE TO TRUE
                                     CALL "CWCONF" USING CWSQLC
                                                         CWCONF-REG
                                                         FS-CWCONF
                                                         KCO PCO
                                     IF   HELP-PATH (1: 1) = "$"
                                          DISPLAY "CWCONF"
                                             UPON ENVIRONMENT-NAME
                                          DISPLAY OLD-CWCONF
                                             UPON ENVIRONMENT-VALUE
                                     ELSE
                                     IF   CWUNIX-OFF
                                          CALL "PC_SET_DRIVE"
                                             USING OLD-DRIVE
                                     END-IF
                                     CALL "CBL_CHANGE_DIR" USING
                                                           OLD-DIRECTORY
                                     END-IF
                                     GO TO 190-RETRY
                              WHEN 2 SET CWSQLC-CLOSE TO TRUE
                                     CALL "CWCONF" USING CWSQLC
                                                         CWCONF-REG
                                                         FS-CWCONF
                                                         KCO PCO
                                     IF   HELP-PATH (1: 1) NOT = "$"
                                     IF   CWUNIX-OFF
                                          CALL "PC_SET_DRIVE" USING
                                                              OLD-DRIVE
                                     END-IF
                                     CALL "CBL_CHANGE_DIR" USING
                                                           OLD-DIRECTORY
                                     END-IF
                                     STOP RUN
                              WHEN 3 MOVE "N" TO CHECK-NOME
                                                 CHECK-SENHA
                                     MOVE "LOGON" TO NOME
                                     CALL "CWTASK" USING "1" TASK
                                     CANCEL "CWTASK"
Mollo                                CALL "CWGETU" USING NOME TASK
Mollo                                                    PROGRAMA '9'
                                     PERFORM GUI-TITLE-UPDATE
                                        THRU END-GUI-TITLE-UPDATE
                          END-EVALUATE
                     ELSE
                         MOVE CWCONF-NIVEL-PS  TO CHECK-NIVEL
                         MOVE CWCONF-GRUPO     TO GRUPO
                         display 'cwsgroup' upon environment-name
                         display grupo      upon environment-value
                         CALL "CWTASK" USING "1" TASK
                         CANCEL "CWTASK"
Mollo                    CALL "CWGETU" USING NOME TASK PROGRAMA '9'
Mollo                    CANCEL 'CWGETL'
Mollo                    CALL "CWGETL" USING PARAMETROS-CWGETL
                         MOVE CWCONF-SENHA TO SENHA-AUTO
                         CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                                 CWCONF-FATOR-PS
                                                 SENHA-AUTO
                         INSPECT SENHA-AUTO
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                         IF   SENHA NOT = SENHA-AUTO
                              MOVE "N"  TO CHECK-SENHA
                              MOVE "+"  TO CHECK-NOME
                              MOVE NOME TO NOME-C
                         END-IF
                     END-IF
                END-IF
                PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
                CALL CHECK-PASS-3
                     ON OVERFLOW
                        MOVE SPACES TO CWSEND-MSG
                        STRING "Menu secund†rio n∆o dispon°vel "
                               CHECK-PASS-3 DELIMITED BY SIZE
                               INTO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     END-CALL
                END-CALL
                CANCEL CHECK-PASS-3
                IF   CWUNIX-OFF
                     CALL "PC_SET_DRIVE"       USING OLD-DRIVE
                END-IF
                INSPECT OLD-DIRECTORY CONVERTING X"20" TO X"00"
                CALL "CBL_CHANGE_DIR"     USING OLD-DIRECTORY
            END-IF.

            MOVE   0             TO RETURN-CODE
            PERFORM 000-INICIO THRU 050-INICIO.

       190-99-FIM. EXIT.

       220-REMOVE-OVERLAYS.

           CALL "CWUSER" USING X"01"
           CANCEL "CWBOXR"
           COPY CWOVRL.

       220-99-FIM. EXIT.

       230-CHECK-ACESSO.

           IF  GRUPO = SPACES
               display 'cwsgroup' upon environment-name
               accept  grupo      from environment-value
           END-IF

           IF   I = 1
                MOVE 0     TO OPCOES-VALIDAS
                MOVE "GU"  TO CWGRPS-REG
                MOVE GRUPO TO CWGRPS-NOME-GRUPO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
                IF   FS-CWGRPS < "09"
                     MOVE CWGRPS-ADM TO ADM
                END-IF
           END-IF

           IF  (GRUPO NOT = SPACES)
           AND (GRUPO NOT = "Acesso sem restriáîes")
           AND (GRUPO NOT = "Acesso sem restriá‰es")
           AND (GRUPO NOT = "Acesso sem restricoes")
           AND (GRUPO NOT = "Acesso irrestrito")
           AND ((CWCONF-PROG (I) NOT = SPACES) OR SHOWCOMENT = 'ON')
                MOVE "GU"            TO CWGRPS-REG
                MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                MOVE CWCONF-PROG (I) TO CWGRPS-PROG-GRUPO
                IF CWCONF-PROG (I) NOT = SPACES
                   CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                           CWCONF-FATOR-P-99 (I)
                                           CWGRPS-PROG-GRUPO
                   CALL "CWVARX" USING CWGRPS-PROG-GRUPO
                             LENGTH OF CWGRPS-PROG-GRUPO
                END-IF
                IF   CWGRPS-PROG-GRUPO = SPACES
                     MOVE SPACES TO CWCONF-PROG    (I)
                                    CWCONF-NM-OPCAO(I)
                     GO TO SKIP-VAR2
                END-IF
                INSPECT CWGRPS-REG CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   CWGRPS-PROG-GRUPO = "CWMENU" OR 'GRMENU'
                     ADD 1 TO OPCOES-VALIDAS
                     GO TO 230-99-FIM
                END-IF
                IF   CWGRPS-PROG-GRUPO = "CWBOXS" OR "GRBOXS"
                                      OR "CWPAGE" OR 'GRPAGE'
                     IF CWGRPS-PROG-GRUPO = "CWPAGE" OR 'GRPAGE'
                        MOVE ";"          TO CWGRPS-PROG-GRUPO (1: 1)
                     ELSE
                        MOVE ":"          TO CWGRPS-PROG-GRUPO (1: 1)
                     END-IF
                     MOVE CWCONF-HELP (I) TO CWGRPS-PROG-GRUPO (2: )
                ELSE
                     IF   CWGRPS-PROG-GRUPO = "CWREL2" OR "XSDRUN"
                     AND (CWCONF-HELP (I) NOT = SPACES)
                          MOVE "*"  TO CWGRPS-PROG-GRUPO (1: 1)
                          MOVE CWCONF-HELP (I)
                            TO CWGRPS-PROG-GRUPO (2: )
                          INSPECT CWGRPS-PROG-GRUPO (2: )
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                END-IF
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
                IF  (FS-CWGRPS < "09"
                AND  CWGRPS-ACESSO-GRUPO NOT = SPACE
                AND  ADM                 NOT = "I")
                OR  (FS-CWGRPS = "23"
                AND  ADM                     = "I")
                     MOVE SPACES TO WS-NM-OPCAO (I)
                                    CWCONF-PROG (I)
                     MOVE 0      TO CWCONF-NO-OPCAO (I)
                ELSE
                     IF   FS-CWGRPS > "09"
                     AND  FS-CWGRPS NOT = "23"
                          CALL "CWCONF" USING "ISAM"
                     END-IF
                END-IF
           ELSE
                IF   CWCONF-PROG (I) NOT = SPACES
                     MOVE CWCONF-PROG (I) TO CWGRPS-PROG-GRUPO
                     CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                             CWCONF-FATOR-P-99 (I)
                                             CWGRPS-PROG-GRUPO
                     CALL "CWVARX" USING CWGRPS-PROG-GRUPO
                               LENGTH OF CWGRPS-PROG-GRUPO
                     IF   CWGRPS-PROG-GRUPO = SPACES
                          MOVE SPACES TO CWCONF-PROG    (I)
                                         CWCONF-NM-OPCAO(I)
                     END-IF
                END-IF
           END-IF.

       SKIP-VAR2.

           IF   CWCONF-HELP (I) = "F9" OR "f9"
                MOVE CWCONF-NM-OPCAO (I) TO F9-TEXTO
                                            TIPO-OP (5)
                MOVE CWCONF-PROG     (I) TO F9-PROGRAM
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        F9-PROGRAM
                MOVE SPACES TO CWCONF-PROG (I)
                               CWCONF-NM-OPCAO (I)
                               CWCONF-HELP     (I)
           END-IF

           IF   CWCONF-PROG (I) NOT = SPACES
                ADD 1 TO OPCOES-VALIDAS
           END-IF
           IF   I = 26
           AND  OPCOES-VALIDAS = 0
           AND  FLAG-OPCOES = SPACE
                MOVE ">" TO FLAG-OPCOES
           END-IF.

       230-99-FIM. EXIT.

       250-AJUSTA-TTY-NOME.

           MOVE SPACES TO TTY-NOME

           IF  (FS-CWCONF NOT = "00")
           OR   NOME = SPACES
           OR   NOME = LOW-VALUES
                IF   TTY = SPACES
                     MOVE "LOGON" TO TTY-NOME
                ELSE
                     STRING TTY  DELIMITED BY SPACE
                            "/"  DELIMITED BY SIZE
                         "LOGON" DELIMITED BY SIZE
                       INTO TTY-NOME
                END-IF
           ELSE
                IF   TTY = SPACES
                     MOVE NOME TO TTY-NOME
                ELSE
                     STRING TTY  DELIMITED BY SPACE
                            "/"  DELIMITED BY SIZE
                            NOME DELIMITED BY SIZE
                       INTO TTY-NOME
                END-IF
           END-IF
           PERFORM 251-CURDIR THRU 251-99-FIM
           ON 1 GO TO 250-99-FIM.
           IF   NOFRAME = 0
                PERFORM 131-DATE-TIME THRU 131-99-FIM
                PERFORM GUI-TITLE-UPDATE THRU END-GUI-TITLE-UPDATE
           END-IF.

       250-99-FIM. EXIT.

       251-CURDIR.

           MOVE SPACES TO CURDIR OLD-DIRECTORY
           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
           IF   CWUNIX-OFF
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            PRINTER-STATUS
                STRING OLD-DRIVE DELIMITED BY SIZE
                       ":\" DELIMITED BY SIZE
                       OLD-DIRECTORY DELIMITED BY SPACE
                INTO CURDIR
           ELSE
                MOVE OLD-DIRECTORY TO CURDIR
           END-IF
           INSPECT CURDIR CONVERTING X"00" TO SPACE
           CALL "CWSETS" USING "S" "DIR" CURDIR.
           IF   CWGETL-DIR NOT = 1
                MOVE SPACES TO CURDIR
           END-IF.

       251-99-FIM. EXIT.

       800-NEW-PRINTER.

           MOVE "00" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE CWCONF-EJECT-MODE-OLD TO EJECT-MODE-OLD
           MOVE "02PRN" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-LABEL TO IMPRESSORA
                SET CWSQLC-DELETE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "PS" TO CWCONF-REG
                SET CWSQLC-START TO TRUE
                SET CWSQLC-NOT-LESS TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                PERFORM TEST AFTER UNTIL CWCONF-TIPO NOT = "PS"
                                      OR FS-CWCONF = "10"
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-NEXT TO TRUE
                        SET CWSQLC-IGNORE-LOCK TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                        IF   FS-CWCONF < "10"
                        AND  CWCONF-TIPO = "PS"
                             IF   IMPRESSORA = SPACES OR LOW-VALUES
                                  MOVE "Spool" TO IMPRESSORA
                             END-IF
                             MOVE IMPRESSORA
                               TO CWCONF-PRINTER-DEFAULT
                             MOVE SPACES TO CWCONF-PATH-SPOOL
                             SET CWSQLC-REWRITE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                        END-IF
                END-PERFORM
                IF   OLD-IMPRESSORA
                     MOVE "03"           TO CWCONF-REG03
                     MOVE EJECT-MODE-OLD TO CWCONF-EJECT-MODE-OLD
                     MOVE IMPRESSORA     TO CWCONF-ARQUIVO CWCONF-LABEL
                     SET CWSQLC-WRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                END-IF
          END-IF.

       800-99-FIM. EXIT.

       810-PGID.

           DISPLAY "PGID" UPON ENVIRONMENT-NAME
           ACCEPT PGID    FROM ENVIRONMENT-VALUE
           IF PGID = '1'
              GO TO 810-99-FIM
           END-IF
           DISPLAY '1'    UPON ENVIRONMENT-VALUE
           MOVE "99"           TO CWCONF-REG
           SET CWSQLC-START    TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM UNTIL FS-CWCONF > "09"
                      OR (CWCONF-TIPO NOT = "99")
                   SET CWSQLC-READ        TO TRUE
                   SET CWSQLC-NEXT        TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                       FS-CWCONF KCO PCO
                        IF   FS-CWCONF < "10"
                        AND  CWCONF-TIPO = "99"
                             PERFORM 820-SET-PGID
                                THRU 820-99-FIM
                                     VARYING i FROM 1 BY 1
                                       UNTIL i > 26
                        END-IF
           END-PERFORM
           MOVE "SM"           TO CWCONF-REG
           SET CWSQLC-START    TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM UNTIL FS-CWCONF > "09"
                      OR (CWCONF-TIPO NOT = "SM")
                   SET CWSQLC-READ        TO TRUE
                   SET CWSQLC-NEXT        TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                       FS-CWCONF KCO PCO
                        IF   FS-CWCONF < "10"
                        AND  CWCONF-TIPO = "SM"
                             PERFORM 820-SET-PGID
                                THRU 820-99-FIM
                                     VARYING i FROM 1 BY 1
                                       UNTIL i > 26
                        END-IF
           END-PERFORM.

       810-99-FIM. EXIT.

       820-SET-PGID.

           IF  (CWCONF-NM-OPCAO (i) NOT = SPACES)
           AND (CWCONF-PROG     (i) NOT = SPACES)
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (i)
                                        CWCONF-FATOR-P-99 (i)
                                        CWCONF-PROG       (i)
                MOVE CWCONF-PROG (i)     TO MENU-PROG
                MOVE CWCONF-NM-OPCAO (i) TO MENU-NM-OPCAO
                MOVE 1                   TO i2
                IF MENU-NM-OPCAO (1: 1) = '/'
                   MOVE 3 TO i2
                END-IF
                INSPECT MENU-NM-OPCAO CONVERTING '_' TO SPACE
                PERFORM 830-WRITE-PGID THRU 830-99-FIM
           END-IF.

       820-99-FIM. EXIT.

       830-WRITE-PGID.

           MOVE SPACES TO CWPGID-NM-OPCAO
           MOVE 0      TO Z
           PERFORM VARYING i2 FROM i2 BY 1
                     UNTIL i2 > LENGTH OF MENU-NM-OPCAO
                        OR Z  > LENGTH OF CWPGID-NM-OPCAO
                     IF MENU-NM-OPCAO (i2: 1) NOT = '~'
                     AND Z < LENGTH OF CWPGID-NM-OPCAO
                     AND i2 < LENGTH OF MENU-NM-OPCAO
                        ADD 1 TO Z
                        MOVE MENU-NM-OPCAO (i2: 1)
                          TO CWPGID-NM-OPCAO (Z:1)
                     END-IF
           END-PERFORM
           IF MENU-PROG(1:2) NOT = 'CW'
              CALL "CWPGID" USING "W" MENU-PROG CWPGID-NM-OPCAO
           END-IF.

       830-99-FIM. EXIT.

       GUi240-VARRE.

           MOVE SPACES TO PUSH-BUTTONS
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > 26
                        OR FS-CWCONF NOT = "00"
             MOVE 0 TO MENU-PAGINA
             PERFORM GUi250-CHECK-ACESSO THRU GUi250-99-FIM
             IF  CWCONF-NIVEL (I) NOT NUMERIC
                 MOVE 0 TO CWCONF-NIVEL (I)
             END-IF
             IF  (CWCONF-NIVEL    (I) NOT > CHECK-NIVEL)
             AND (CWCONF-NM-OPCAO (I) NOT = SPACES)
             AND ((CWCONF-PROG (I) NOT = SPACES) OR SHOWCOMENT = 'ON')
                 CALL "CWVARX" USING CWCONF-NM-OPCAO(I)
                           LENGTH OF CWCONF-NM-OPCAO(I)
                 MOVE CWCONF-NM-OPCAO(I) TO TESTECHAR
      *          INSPECT CWCONF-NM-OPCAO (I)
      *                  CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                 INSPECT CWCONF-NM-OPCAO (I)
                         CONVERTING X"10" TO SPACE
                 MOVE SPACES TO TESTECHAR2
                 IF   TESTECHAR = "/"
                      MOVE CWCONF-NM-OPCAO(I) (3:  ) TO FUNCAO-PROGRAMA
                      MOVE CWCONF-NM-OPCAO(I) (2: 1) TO TESTECHAR2
                 ELSE
                      MOVE CWCONF-NM-OPCAO(I) TO FUNCAO-PROGRAMA
                 END-IF
                 INSPECT FUNCAO-PROGRAMA CONVERTING "_" TO SPACE
                 PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 34
                         OR FUNCAO-PROGRAMA (Y: 1) = ":"
                            CONTINUE
                 END-PERFORM
                 MOVE SPACES TO EXTW
                 PERFORM VARYING Y2 FROM Y BY 1 UNTIL Y2 > 34
                                                   OR Y  > 34
                         IF FUNCAO-PROGRAMA (Y2: 1) = "."
                            ADD 1 TO Y2
                            MOVE FUNCAO-PROGRAMA (Y2: )
                              TO EXTW
                            INSPECT EXTW CONVERTING
                                    MINUSCULAS TO MAIUSCULAS
                            IF EXTW-OK
                               EXIT PERFORM
                            END-IF
                         END-IF
                 END-PERFORM
                 IF  EXTW-OK
                     ADD 1 TO Y
                     IF   FUNCAO-PROGRAMA (Y: ) NOT = SPACES
                          MOVE FUNCAO-PROGRAMA (Y: ) TO PB (I)
                          SUBTRACT 1 FROM Y
                          MOVE SPACES TO FUNCAO-PROGRAMA (Y: )
                          PERFORM VARYING Y2 FROM 1 BY 1 UNTIL Y2 > 34
                                  IF PB(I)(Y2:1) = ';'
                                     ADD 1            TO Y2
                                     MOVE PB(I) (Y2:) TO PN (I)
                                     SUBTRACT 1     FROM Y2
                                     MOVE SPACES      TO PB(I)(Y2:)
                                     EXIT PERFORM
                                  END-IF
                          END-PERFORM
                     END-IF
                 END-IF
                 ADD  1                 TO SP2-MD-OPTION-CNT
                 IF CWCONF-PROG (I) =  SPACE
                    MOVE "g" TO SP2-MDO-STATE (SP2-MD-OPTION-CNT)
                    MOVE SPACES TO PROGRAMA RELATORIO
                 ELSE
                    MOVE CWCONF-PROG (I) TO PROGRAMA
                    MOVE CWCONF-HELP (I) TO RELATORIO
                    CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                            CWCONF-FATOR-P-99 (I)
                                            PROGRAMA
                    INSPECT PROGRAMA CONVERTING
                                     MINUSCULAS TO MAIUSCULAS
                 END-IF
                 MOVE SP2-MD-OPTION-CNT TO SP2-MDO-ID(SP2-MD-OPTION-CNT)
                                           PAI (N)
                 IF   N > 1
                      MOVE PAI (N - 1)
                        TO SP2-MDO-OWNR-ID (SP2-MD-OPTION-CNT)
                 ELSE
                      MOVE 0
                        TO SP2-MDO-OWNR-ID (SP2-MD-OPTION-CNT)
                 END-IF

                 PERFORM GUI280-FUNCAO-PROGRAMA THRU GUI280-99-FIM

                 IF   PROGRAMA = "CWBOXS" OR "GRBOXS"
                      MOVE "s" TO SP2-MDO-TYPE (SP2-MD-OPTION-CNT)
                      PERFORM GUI260-SUBMENU THRU GUI260-99-FIM
                 ELSE
                      MOVE SPACES              TO MENU-TYPE
                      MOVE SP2-MDO-OWNR-ID (SP2-MD-OPTION-CNT)
                                               TO MENU-ID
                      READ MENU
                      MOVE SPACES              TO MENU-TYPE
                      MOVE SP2-MDO-ID(SP2-MD-OPTION-CNT)
                                               TO MENU-ID
                      MOVE CWCONF-NIVEL    (I) TO MENU-NIVEL
                      MOVE CWCONF-CHECK    (I) TO MENU-CHECK
                      MOVE PROGRAMA            TO MENU-PROG
                      MOVE CWCONF-PASS     (I) TO MENU-PASS
                      CALL "CWCODE" USING "D" CWCONF-SIZE-S-99  (I)
                                              CWCONF-FATOR-S-99 (I)
                                              MENU-PASS
                      MOVE CWCONF-SIZE-S-99 (I) TO MENU-SIZE-S
                      MOVE CWCONF-HELP     (I) TO MENU-HELP
                      MOVE CWCONF-NO-OPCAO (I) TO MENU-NO-OPCAO
                      MOVE CWCONF-NM-OPCAO (I) TO MENU-FUNCAO-PROGRAMA
pT                    MOVE SPACES TO MENU-PATH
                      PERFORM VARYING N2 FROM 1 BY 1 UNTIL N2 = N
PT                       MOVE NM-OPCAO(N2) TO MENU-N(N2)
dg2                      MOVE 1            TO I2
                         PERFORM VARYING Y FROM 34 BY -1
                                   UNTIL Y = 1
                                      OR NM-OPCAO(N2)(Y: 1) NOT = SPACE
                                 CONTINUE
                         END-PERFORM
                         MOVE NM-OPCAO (N2) TO MENU-NM-OPCAO (I2: )
                         ADD  Y             TO I2
                         MOVE " - "         TO MENU-NM-OPCAO (I2: )
                         ADD  3             TO I2
                      END-PERFORM
                      MOVE CWCONF-NM-OPCAO (I) TO MENU-NM-OPCAO (I2: )
      *               PERFORM 830-WRITE-PGID THRU 830-99-FIM
                      PERFORM VARYING I2 FROM LENGTH MENU-NM-OPCAO BY -1
                              UNTIL MENU-NM-OPCAO (I2: 1) NOT = SPACE
                                    CONTINUE
                      END-PERFORM
                      ADD  1             TO I2
                      MOVE X"00"         TO MENU-NM-OPCAO (I2: )
                      MOVE CWCONF-PAGINA TO MENU-PAGINA-OP
                      MOVE CWCONF-TIPO   TO MENU-TIPO-REG
                      WRITE MENU-REG
                 END-IF
                 MOVE "CWMENU" TO PROGRAMA
             END-IF
           END-PERFORM.

       GUI240-99-FIM. EXIT.

       GUI250-CHECK-ACESSO.

           IF   I = 1
                MOVE 0               TO OPCOES-VALIDAS
                MOVE "GU"            TO CWGRPS-REG
                MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
                IF   FS-CWGRPS < "09"
                     MOVE CWGRPS-ADM TO ADM
                END-IF
           END-IF

           IF  (GRUPO NOT = SPACES)
           AND (GRUPO NOT = "Acesso sem restriáîes")
           AND (GRUPO NOT = "Acesso sem restriá‰es")
           AND (GRUPO NOT = "Acesso sem restricoes")
           AND (GRUPO NOT = "Acesso irrestrito")
CUCO  *    AND ((CWCONF-PROG (I) NOT = SPACES) OR SHOWCOMENT = 'ON')
                MOVE "GU"            TO CWGRPS-REG
                MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                MOVE CWCONF-PROG (I) TO CWGRPS-PROG-GRUPO
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        CWGRPS-PROG-GRUPO
                CALL "CWVARX" USING CWGRPS-PROG-GRUPO
                          LENGTH OF CWGRPS-PROG-GRUPO
                IF   CWGRPS-PROG-GRUPO = SPACES
                     MOVE SPACES TO CWCONF-PROG    (I)
                                    CWCONF-NM-OPCAO(I)
                     GO TO SKIP-VAR1
                END-IF
                INSPECT CWGRPS-REG CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   CWGRPS-PROG-GRUPO = "CWMENU" OR 'GRMENU'
                     ADD 1 TO OPCOES-VALIDAS
                     GO TO GUI250-99-FIM
                END-IF
                IF   CWGRPS-PROG-GRUPO = "CWBOXS" OR "GRBOXS"
                                      OR "CWPAGE" OR 'GRPAGE'
                     IF CWGRPS-PROG-GRUPO = "CWPAGE" OR 'GRPAGE'
                        MOVE ";"          TO CWGRPS-PROG-GRUPO (1: 1)
                     ELSE
                        MOVE ":"          TO CWGRPS-PROG-GRUPO (1: 1)
                     END-IF
                     MOVE CWCONF-HELP (I) TO CWGRPS-PROG-GRUPO (2: )
                     CALL "CWPAGE" USING CWGRPS-PROG-GRUPO
                                         GRUPO CHECK-NIVEL
                                         CWCONF-PROG (I)
                ELSE
                     IF   CWGRPS-PROG-GRUPO = "CWREL2"
                     AND (CWCONF-HELP (I) NOT = SPACES)
                          MOVE "*"  TO CWGRPS-PROG-GRUPO (1: 1)
                          MOVE CWCONF-HELP (I)
                            TO CWGRPS-PROG-GRUPO (2: )
                          INSPECT CWGRPS-PROG-GRUPO (2: )
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                END-IF
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KGR PGR
                IF  (FS-CWGRPS < "09"
                AND  CWGRPS-ACESSO-GRUPO NOT = SPACE
                AND  ADM                 NOT = "I")
                OR  (FS-CWGRPS = "23"
                AND  ADM                     = "I")
                     MOVE SPACES TO CWCONF-PROG (I)
                     MOVE 0      TO CWCONF-NO-OPCAO (I)
                ELSE
                     IF   FS-CWGRPS > "09"
                     AND  FS-CWGRPS NOT = "23"
                          CALL "CWCONF" USING "ISAM"
                     END-IF
                END-IF
          ELSE
                IF   CWCONF-PROG (I) NOT = SPACES
                     MOVE CWCONF-PROG (I) TO CWGRPS-PROG-GRUPO
                     CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                             CWCONF-FATOR-P-99 (I)
                                             CWGRPS-PROG-GRUPO
                     CALL "CWVARX" USING CWGRPS-PROG-GRUPO
                               LENGTH OF CWGRPS-PROG-GRUPO
                     IF   CWGRPS-PROG-GRUPO = SPACES
                          MOVE SPACES TO CWCONF-PROG    (I)
                                         CWCONF-NM-OPCAO(I)
                     END-IF
                END-IF
           END-IF.

       SKIP-VAR1.

           IF   CWCONF-HELP (I) = "F9" OR "f9"
                MOVE CWCONF-NM-OPCAO (I) TO F9-TEXTO
                                            TIPO-OP (5)
                MOVE CWCONF-PROG     (I) TO F9-PROGRAM
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        F9-PROGRAM
                MOVE SPACES TO CWCONF-PROG (I)
                               CWCONF-NM-OPCAO (I)
                               CWCONF-HELP     (I)
           END-IF

           IF   CWCONF-PROG (I) NOT = SPACES
                ADD 1 TO OPCOES-VALIDAS
SG              INSPECT CWCONF-NM-OPCAO (I)  CONVERTING X"B3BA"
SG                   TO X"2020"
SG              MOVE    CWCONF-NM-OPCAO (I)  TO LIMPA
SG              PERFORM VARYING L FROM 1 BY 1 UNTIL L > 33
SG                      OR LIMPA(L:1) NOT = SPACE
SG                      CONTINUE
SG              END-PERFORM
SG              MOVE LIMPA (L:) TO CWCONF-NM-OPCAO (I)
           END-IF
           IF   I = 26
           AND  OPCOES-VALIDAS = 0
           AND  FLAG-OPCOES = SPACE
                MOVE ">" TO FLAG-OPCOES
           END-IF.

       GUI250-99-FIM. EXIT.

       GUI260-SUBMENU.

           IF SP2-MDO-STATE (SP2-MD-OPTION-CNT) = 'g'
pop           MOVE x"00" TO SP2-MDO-TYPE (SP2-MD-OPTION-CNT)
              GO TO GUI260-99-FIM
           END-IF

           IF  PB (I) NOT = SPACES
               MOVE SPACES TO POS-BM
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 34
                       IF   CWCONF-HELP (I) (Y: 1) = ":"
                            ADD 1 TO Y
                            MOVE CWCONF-HELP (I) (Y: ) TO POS-BM
                            SUBTRACT 1 FROM Y
                            MOVE SPACES TO CWCONF-HELP (I) (Y: )
                       END-IF
               END-PERFORM
           END-IF
           PERFORM GUI270-PAGINAX THRU GUI270-99-FIM
           IF  PB (I) NOT = SPACES
               MOVE SPACES            TO MENU-TYPE
               MOVE SP2-MD-OPTION-CNT TO MENU-ID
               MOVE FUNCAO-PROGRAMA   TO MENU-FUNCAO-PROGRAMA
               MOVE PB (I)            TO MENU-BMP
               MOVE PN (I)            TO MENU-PANO
               MOVE PAGINAX           TO MENU-PAGINA
               MOVE CWCONF-TIPO       TO MENU-TIPO-REG
               WRITE MENU-REG
               MOVE X"00"             TO SP2-MDO-TYPE(SP2-MD-OPTION-CNT)
               IF   POS-BM NOT = SPACES
                    MOVE LOW-VALUES    TO SP2-FD-DATA
                                          SP2-FD-VAR-LENS
                    CALL "CWSPID" USING SP2-FD-ID "FInsert"
                    ADD  1                  TO BM
                    MOVE "B"                TO MENU-TYPE
                    MOVE BM                 TO SP2-FD-FLD-NUM
                                               SP2-FD-TAB-NUM
                    add 1 to SP2-FD-FLD-NUM
                              SP2-FD-TAB-NUM
                    MOVE  SP2-FD-ID         TO BM-ID (BM) MENU-ID
                    MOVE  SP2-MD-OPTION-CNT TO MENU-ID2
                    MOVE  PB (I)            TO MENU-FUNCAO-PROGRAMA
                                               MENU-BMP
                    MOVE PN (I)            TO MENU-PANO
                    MOVE CWCONF-TIPO       TO MENU-TIPO-REG
                    WRITE MENU-REG
                    MOVE 0 TO MENU-ID2
      *             DISPLAY "X" AT LINE BM-LINE COLUMN BM-COLUMN
                    COMPUTE SP2-FD-ROW    = BM-LINE   - 1
                    COMPUTE SP2-FD-COL    = BM-COLUMN - 1
                    COMPUTE SP2-FD-WIDTH  = BM-WIDTH  * 10
                    COMPUTE SP2-FD-HEIGHT = BM-HEIGHT * 10
                    MOVE SPACES TO SP2-FD-VAR-DATA
                    PERFORM VARYING Y2
                            FROM LENGTH OF PB (I)
                              BY -1
                                 UNTIL Y2 = 0
                                 OR PB(I) (Y2:1) = '.'
                            CONTINUE
                    END-PERFORM
                    ADD 1 TO Y2
                    MOVE PB(I) (Y2:) TO tipo
                    INSPECT tipo CONVERTING MAIUSCULAS
                                         TO MINUSCULAS
                    IF   tipo = 'bmp'
                         MOVE PB(I) TO IMAGE
                    ELSE
                         CALL "CWWBMP" USING PB(I) IMAGE
                    END-IF
                    PERFORM VARYING Y FROM LENGTH IMAGE BY -1
                              UNTIL Y = 1
                                 OR IMAGE (Y: 1) NOT = SPACE
                              CONTINUE
                    END-PERFORM
                    COMPUTE SP2-FD-INITIAL-LEN = 8 + Y
                    MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                    MOVE IMAGE           TO SP2-FD-VAR-DATA (9: )
                    MOVE X"01"           TO SP2-FD-TYPE
                    MOVE "i"             TO SP2-FD-CTRL-TYPE
                    MOVE "f"             to SP2-FD-SPEC-FMT
                    MOVE "y"             TO SP2-FD-OUTPUT
                    MOVE "w"             TO SP2-FD-CURS-SKIP
                    MOVE "d"             TO SP2-FD-BOR-TYPE
                    MOVE SP2-KEY-ENTER   TO SP2-FD-HELP-KEY
                    COMPUTE Y = SP2-FD-VAR-LEN + 1
                    MOVE 0 TO Y2
                    PERFORM 34 TIMES
                            ADD 1 TO Y2
                            IF FUNCAO-PROGRAMA(Y2: 1) NOT = "~"
                               MOVE FUNCAO-PROGRAMA(Y2: 1)
                                 TO SP2-FD-VAR-DATA (Y: 1)
                               ADD 1 TO Y
                            END-IF
                    END-PERFORM
      *             MOVE FUNCAO-PROGRAMA TO SP2-FD-VAR-DATA (Y: 34)
                    MOVE 34              TO SP2-FD-MSG-TEXT-LEN
                    ADD  SP2-FD-MSG-TEXT-LEN TO SP2-FD-VAR-LEN
                    MOVE CWCONF-CHAVE TO SAVE-PAGINAX
                    MOVE "SM"    TO CWCONF-REG99
                    MOVE PAGINAX TO CWCONF-PAGINA
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    SET CWSQLC-IGNORE-LOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF   FS-CWCONF > "09"
                          MOVE "g" TO SP2-FD-OUTPUT
                                      SP2-MDO-STATE (SP2-MD-OPTION-CNT)
                    END-IF
                    MOVE SAVE-PAGINAX TO CWCONF-CHAVE
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    SET CWSQLC-IGNORE-LOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
               END-IF
               GO TO GUI260-99-FIM
           END-IF
           IF N NOT < 7 GO TO GUI260-99-FIM.
           MOVE CWCONF-CHAVE        TO SALVA-CHAVE-GUI (N)
           CALL "CWVARX" USING CWCONF-NM-OPCAO(I)
                     LENGTH OF CWCONF-NM-OPCAO(I)
           MOVE CWCONF-NM-OPCAO (I) TO NM-OPCAO (N)
           MOVE I                   TO I-SAVE (N)
           ADD  1                   TO N
LD         If PAGINAX = CWCONF-PAGINA
LD    *    AND CWCONF-TIPO = "*SM"
LD         AND CWCONF-TIPO = "SM"
LD            MOVE "77" TO FS-CWCONF
LD         ELSE
              MOVE "SM"    TO CWCONF-REG99
              MOVE PAGINAX TO CWCONF-PAGINA
              SET CWSQLC-READ TO TRUE
              SET CWSQLC-EQUAL TO TRUE
              SET CWSQLC-IGNORE-LOCK TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
Joadir        IF (CWMENUPAGE = 'ON' OR 'ALL')
Joadir        AND (N > 2 OR CWMENUPAGE = 'ALL')
Joadir        AND (CWCONF-PAGINA NOT = ZERO)
Joadir        AND  FS-CWCONF = '00'
Joadir           PERFORM VARYING IZ FROM 1 BY 1
Joadir                    UNTIL CWCONF-PAGINA(IZ:1) NOT = '0'
Joadir                   CONTINUE
Joadir           END-PERFORM
Joadir           MOVE SPACES TO TIT-WORK
Joadir           STRING SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
Joadir                  ' ('
Joadir                  CWCONF-PAGINA (IZ:)
Joadir                  ')' DELIMITED BY SIZE
Joadir                  INTO TIT-WORK
Joadir            EXEC COBOLware Pack
Joadir                 String TIT-WORK
Joadir            END-EXEC
Joadir            MOVE TIT-WORK TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
Joadir        END-IF
LD         END-iF
           IF   FS-CWCONF > "09"
                IF   FS-CWCONF = "77"
                     PERFORM VARYING E34 FROM 34 BY -1
                             UNTIL SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                                  (E34: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     IF E34 < 34
                        ADD 1 TO E34
                     END-IF
                     MOVE "*"
                       TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT) (E34:1)
                ELSE
                     MOVE "ERRO"
                       TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                END-IF
           ELSE
                PERFORM GUI240-VARRE THRU GUI240-99-FIM
           END-IF
           SUBTRACT 1 FROM N
           MOVE SALVA-CHAVE-GUI (N) TO CWCONF-CHAVE
           MOVE I-SAVE (N)      TO I
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       GUI260-99-FIM. EXIT.

       GUI270-PAGINAX.

           MOVE 0            TO PAGINAX
           MOVE 7            TO SM-FIL
           MOVE 5            TO YP
           PERFORM VARYING IP FROM 20 BY -1
                   UNTIL IP = 0
                      OR SM-FIL < 1
                   MOVE CWCONF-HELP (I) (IP: 1) TO TP
                   IF   TP NUMERIC
                   AND  YP > 1
                        SUBTRACT 1 FROM YP
                        MOVE     TP  TO PAGINAX (YP: 1)
                   ELSE
                       IF   TP = ","
                            MOVE 0       TO PAGINAX
                            MOVE 5       TO YP
                            SUBTRACT 1 FROM SM-FIL
                       END-IF
                   END-IF
           END-PERFORM.

       GUI270-99-FIM. EXIT.

       GUI280-FUNCAO-PROGRAMA.

           IF  SP2-MDO-TYPE (SP2-MD-OPTION-CNT) = "s"
               INSPECT FUNCAO-PROGRAMA CONVERTING
                                       MAIUSCULAS TO MINUSCULAS
               move space to TESTECHAR2
           END-IF
           INSPECT TESTECHAR2 CONVERTING
                              MINUSCULAS TO MAIUSCULAS
           PERFORM VARYING VEZ-GUI FROM 1 BY 1 UNTIL VEZ-GUI > 2
                   MOVE 0      TO FP2 FP3
                   MOVE SPACES TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                                  CWCONF-NM-OPCAO (I)
                   PERFORM VARYING FP FROM 1 BY 1
                             UNTIL FP  > LENGTH OF SP2-MDO-TEXT (1)
                                OR FP2 > LENGTH OF SP2-MDO-TEXT (1)
                           IF   FUNCAO-PROGRAMA (FP: 1) = X"7E"
                                MOVE SPACE TO FUNCAO-PROGRAMA (FP: 1)
<7E>                            ADD 1 TO FP
<7E>                            MOVE FUNCAO-PROGRAMA (FP: 1)
<7E>                             TO TESTECHAR2
<7E>                            INSPECT TESTECHAR2 CONVERTING
<7E>                                    MINUSCULAS TO MAIUSCULAS
<7E>                            MOVE TESTECHAR2 TO FUNCAO-PROGRAMA(FP:1)
                           END-IF
                           MOVE FUNCAO-PROGRAMA (FP: 1) TO TESTECHAR
                           IF   VEZ-GUI = 2
                                INSPECT TESTECHAR CONVERTING
                                        MINUSCULAS TO MAIUSCULAS
                           END-IF
                           IF   TESTECHAR2 NOT = SPACES
                           AND  TESTECHAR = TESTECHAR2
                                ADD  1      TO FP2
                                MOVE X"7E"  TO SP2-MDO-TEXT
                                            (SP2-MD-OPTION-CNT) (FP2: 1)
                                MOVE SPACES TO TESTECHAR2
                           END-IF
                           ADD 1 TO FP2
                           MOVE FUNCAO-PROGRAMA (FP: 1)
                             TO SP2-MDO-TEXT (SP2-MD-OPTION-CNT)(FP2: 1)
                           IF   FUNCAO-PROGRAMA (FP: 1) NOT = X"7E"
                                ADD  1 TO FP3
                                MOVE FUNCAO-PROGRAMA (FP: 1)
                                  TO CWCONF-NM-OPCAO (I) (FP3: 1)
                           END-IF
                   END-PERFORM
                   MOVE SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                     TO FUNCAO-PROGRAMA
                   IF   VEZ-GUI = 2
                        PERFORM ACENTOS THRU FIM-ACENTOS
                   END-IF
                   INSPECT TESTECHAR2 CONVERTING
                                      MINUSCULAS TO MAIUSCULAS
           END-PERFORM.

       GUI280-99-FIM. EXIT.

       GUI-TITLE-UPDATE.

           MOVE LOW-VALUES TO SP2-WD-DATA
           MOVE "CWMENU"   TO SP2-WD-NAME
           MOVE LOW-VALUES TO SP2-PD-DATA
           MOVE "CWMENU"   TO SP2-PD-NAME
           CALL SP2   USING SP2-GET-PANEL-DEF  SP2-PANEL-DEF
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           PERFORM GUI-TITLE THRU END-GUI-TITLE
           MOVE STATUS-DEF        TO SP2-PD-MSG-TEXT
           IF   CWLITS = "LOW"
                INSPECT SP2-WD-TITLE CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-WD-TITLE CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
Frango*    ELSE
Frango*         INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
Frango*                                      TO ACENTOS-WINDOWS
           END-IF
Frango*
           CALL SP2   USING SP2-SET-WINDOW-DEF SP2-WINDOW-DEF
           CALL "CWRESE"
           CALL SP2   USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF.

       END-GUI-TITLE-UPDATE. EXIT.

       GUI-TITLE.

           MOVE SPACES          TO SP2-WD-TITLE WS-TITLE
           MOVE SISTEMA         TO SISTEMA-WK
           MOVE SPACES          TO SISTEMA
           MOVE 0               TO Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
                   IF SISTEMA-WK (I:1) NOT = "~"
                      ADD 1 TO Y
                      MOVE SISTEMA-WK (I:1) TO SISTEMA (Y:1)
                   END-IF
           END-PERFORM
           PERFORM VARYING I FROM 30 BY -1
                   UNTIL I = 1
                         OR SISTEMA (I: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           IF SISTEMA = APLICACAO
              STRING SISTEMA (1: I) " - " USUARIO DELIMITED BY SIZE
                     INTO WS-TITLE
           ELSE
              PERFORM VARYING Y FROM LENGTH APLICACAO BY -1
                UNTIL Y = 0 OR (APLICACAO (Y:1) NOT = SPACE)
                     CONTINUE
              END-PERFORM
              IF Y > 0
                 PERFORM VARYING K FROM LENGTH USUARIO BY -1
                   UNTIL K = 0 OR (USUARIO (K:1) NOT = SPACE)
                        CONTINUE
                 END-PERFORM
                 STRING APLICACAO (1: Y) " - " USUARIO (1: K)
                    " - " SISTEMA (1: I)  DELIMITED BY SIZE
                    INTO WS-TITLE
              ELSE
                 STRING " - " USUARIO
                         " - " SISTEMA (1: I) DELIMITED BY SIZE
                        INTO WS-TITLE
              END-IF
           END-IF
           MOVE 0 TO Y
           MOVE SPACES TO SP2-WD-TITLE
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 93
                      OR Y > 93
                   IF  WS-TITLE (I: 1) > X"1F"
      *            AND WS-TITLE (I: 1) < X"A6"
                       ADD  1               TO Y
                       MOVE WS-TITLE (I: 1) TO SP2-WD-TITLE (Y: 1)
                   END-IF
           END-PERFORM
           PERFORM VARYING Y FROM LENGTH OF SP2-WD-TITLE BY -1
                      UNTIL Y = 1
                         OR SP2-WD-TITLE (Y: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           PERFORM VARYING K FROM LENGTH NOME BY -1
                     UNTIL K = 1
                        OR NOME (K: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           MOVE NOME     TO NOME-LOW
           MOVE PROGRAMA TO PROG-LOW
           IF  NOME = "LOGON"
               MOVE "CWMENU" TO PROG-LOW
           END-IF
      *    INSPECT VAR-LOW CONVERTING MAIUSCULAS TO MINUSCULAS
           IF  PROG-LOW (1: 5) = "CWREL" or "cwrel"
               MOVE "Relator" TO PROG-LOW
           END-IF
           IF  NOFRAME = 0
               MOVE SPACES TO WS-TITLE
               IF NX NOT = 0
                  MOVE SPACES TO BUFFER
                  INSPECT MENU-PATH CONVERTING X'00' TO SPACE
                  MOVE 0 TO Z
                  PERFORM VARYING N FROM 1 BY 1 UNTIL N > 5
                                                OR MENU-N (N) = SPACES
                          PERFORM VARYING X FROM 34 BY -1
                                  UNTIL X = 0
                                     OR MENU-N (N)(X:1) NOT = SPACE
                                  CONTINUE
                          END-PERFORM
                          PERFORM VARYING I FROM 1 BY 1
                                        UNTIL I > 34
                                        OR (MENU-N(N)(I:1)
                                                     NOT = SPACE)
                                  CONTINUE
                          END-PERFORM
                          PERFORM VARYING I FROM I BY 1 UNTIL I > X
                              IF MENU-N(N)(I:2) = SPACE
                                 EXIT PERFORM CYCLE
                              END-IF
                              ADD 1 TO Z
                              MOVE MENU-N(N) (I:1) TO BUFFER (Z:1)
                          END-PERFORM
                          ADD 1 TO Z
      *                   MOVE "/" TO BUFFER (Z:1)
                          MOVE " - " TO BUFFER (Z:3)
                          ADD 2 TO Z
                  END-PERFORM
                  ADD 1 TO Z
                  MOVE FUNCAO-PROGRAMA TO BUFFER(Z:)
                  IF   HELP  = 1
                       PERFORM VARYING Z FROM LENGTH OF BUFFER BY -1
                               UNTIL BUFFER(Z:1) NOT = SPACE
                               CONTINUE
                       END-PERFORM
                       ADD 1 TO Z
                       MOVE " (HELP)" TO WS-TITLE (Z:)
                  END-IF
                  STRING SP2-WD-TITLE (1: Y) " - " BUFFER
                         DELIMITED BY SIZE
                    INTO WS-TITLE
                  PERFORM VARYING Z FROM LENGTH OF WS-TITLE BY -1
                          UNTIL WS-TITLE(Z:1) NOT = SPACE
                          CONTINUE
                  END-PERFORM
                  IF Z > LENGTH OF SP2-WD-TITLE
                     MOVE 0 TO NX
                  END-IF
               END-IF
               IF NX = 0
                  STRING SP2-WD-TITLE (1: Y) " - " FUNCAO-PROGRAMA
                         DELIMITED BY SIZE
                    INTO WS-TITLE
               END-IF
           ELSE
               MOVE SP2-WD-TITLE (1: Y) TO WS-TITLE
           END-IF
           IF   HELP  = 1
           AND  NX = 0
                PERFORM VARYING Y FROM LENGTH OF WS-TITLE BY -1
                        UNTIL WS-TITLE(Y:1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                ADD 1 TO Y
                MOVE " (HELP)" TO WS-TITLE (Y:)
           END-IF
           INSPECT PROG-LOW CONVERTING X"00" TO SPACE
           MOVE NOME       TO STATUS-NOME
           MOVE PROG-LOW   TO STATUS-PROGRAM
           IF  PROG-LOW = SPACES
           OR  PROG-LOW (1: 5) = "CWMEN"
               MOVE "CWMENU" TO STATUS-PROGRAM
           END-IF
//         IF  STATUS-PROGRAM (1: 1) = "/"
               MOVE "#" TO STATUS-PROGRAM (1: 1)
           END-IF
           PERFORM VARYING K FROM LENGTH OF STATUS-PROGRAM BY -1
                     UNTIL K = 1
                        OR STATUS-PROGRAM (K: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           COMPUTE PLEN = K * 10
           COMPUTE DLEN = 467 - PLEN
           MOVE 240 TO NLEN
           IF   CWGETL-DIR NOT = 1
                ADD DLEN TO NLEN
                MOVE 0   TO DLEN
           END-IF
           PERFORM VARYING K FROM LENGTH OF LB-PRNTER-2 BY -1
                     UNTIL K = 8
                        OR LB-PRNTER-2 (K: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           COMPUTE ILEN = K * 10
           IF   DLEN NOT = 0
                SUBTRACT ILEN FROM DLEN
           END-IF
           MOVE WS-TITLE TO SP2-WD-TITLE
           INSPECT SP2-WD-TITLE
                   CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           DISPLAY "CWTITLE"    UPON ENVIRONMENT-NAME
           DISPLAY SP2-WD-TITLE UPON ENVIRONMENT-VALUE.

       END-GUI-TITLE. EXIT.

       ACENTOS.

           IF   CWLITS = "LOW"
                INSPECT SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT SP2-MDO-TEXT (SP2-MD-OPTION-CNT)
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF.

       FIM-ACENTOS. EXIT.

       ACENTOS2.

           IF   CWLITS = "LOW"
                INSPECT MENU-NM-OPCAO
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT MENU-NM-OPCAO
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT MENU-NM-OPCAO
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT MENU-NM-OPCAO
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF.

       FIM-ACENTOS2. EXIT.

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

       CONVERTE-TECLA.

           COPY CWSPKY.

       FIM-CONVERTE-TECLA. EXIT.

       END PROGRAM CWMEN0.

