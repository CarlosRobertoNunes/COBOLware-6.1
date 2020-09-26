       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWIMPR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/12/1987.
       SECURITY.      *************************************************
                      *                                               *
                      *   Subrotina para emissao de relatorios        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
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

           SELECT CWSPLN ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  RELATIVE KEY  IS RK-CWSPLN
                  ACCESS MODE   IS RANDOM
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CWSPLN.

           SELECT TEXTO ASSIGN TO DISK
                  LOCK MODE    IS EXCLUSIVE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-TEXTO.

       DATA DIVISION.
       FILE SECTION.

       COPY CWDIRS.

       FD  PRINTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS PRINTS-LB.

       01  PRINTS-REG                     PIC X(001).

       FD  CWSPLN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWSPLN.

       01  CWSPLN-REG.
           05 CWSPLN-TASK              PIC  9(006).

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG PIC X(503).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 BARRA-DIR               PIC  X(001) VALUE SPACE.
           05 NIVEL                   PIC  9(001) VALUE ZERO.
           05 igual                   PIC  9(003) VALUE ZERO.
           05 INCLUDE                 PIC  9(001) VALUE ZERO.
           05 TEST-INCLUDE            PIC  X(008) VALUE SPACES.
           05 TEST-QUIET              PIC  X(002) VALUE SPACES.
           05 NUMERO                  PIC  9(018) VALUE 0.
           05 CWNUMERO                PIC  X(018) VALUE SPACES.
           05 POSTER-BUFFER.
              10 POSTER-LINE          PIC  X(050) OCCURS 3.
           05 P                COMP-X PIC  9(002) VALUE 0.
           05 PAGEOFF                 PIC  X(003) VALUE SPACES.
           05 FOLDOFF                 PIC  X(003) VALUE SPACES.
           05 NEW                     PIC  X(050) VALUE SPACES.
           05 EXTW                    PIC  X(004) VALUE SPACES.
           05 AFTER-0                 PIC  9(001) VALUE 0.
           05 ER-TEXTO.
              10 FS-TEXTO             PIC  X(002) VALUE "00".
              10 LB-TEXTO             PIC  X(050) VALUE SPACES.
           05 ARROBA                  PIC  9(002) VALUE 0.
      *    05 PONTO                   PIC  9(002) VALUE 0.
           05 CWPRINTFONT             PIC  X(050) VALUE SPACES.
           05 E-MAIL                  PIC  X(050) VALUE SPACES.
           05 BUFFER-CWIMPR           PIC  X(3518) VALUE SPACES.
           05 EMAIL-TEXT              PIC  X(255) VALUE SPACES.
           05 SPOOL-KEEP              PIC  X(001) VALUE "0".
           05 PRN                     PIC  X(050) VALUE SPACES.
           05 LPT                     PIC  X(050) VALUE SPACES.
           05 TEMP                    PIC  X(050) VALUE SPACES.
           05 SPL000000               PIC  X(012) VALUE "Rn000000.SPL".
           05 SPL999999               PIC  X(012) VALUE "Rn999999.SPL".
           05 CWSPLEXT                PIC  X(003) VALUE SPACES.
           05 CWSPLTXT                PIC  X(003) VALUE SPACES.
           05 ORIENTACAO              PIC  X(001) VALUE "R".
           05 POSTER-C                PIC  X(050) VALUE SPACES.
           05 WINPRINT                            VALUE SPACES.
              10                      PIC  X(008).
              88 POSTER VALUE "WINDOWS " "WINDOWS:"
                              "USBTEXT " "USBTEXT:"
                              "WINVIEW ".
              10                      PIC  X(042).
           05 WINDOWS-PRINTER         PIC  X(042) VALUE SPACES.
           05 COBWARE                 PIC  X(050) VALUE SPACES.
           05 PAP                     PIC  X(080) VALUE SPACES.
           05 OLD-DRIVE               PIC  X(001) VALUE SPACE.
           05 AUTOVIEW                PIC  9(001) VALUE 0.
           05 RETURN-STATUS           PIC  9(002) COMP-5 VALUE 0.
           05 COM-ESTILO              PIC  9(001) VALUE 0.
           05 PAGINA                  PIC  9(004) VALUE 0.
           05 FOLHA                   PIC  9(004) VALUE 0.
           05 PAGE-OFF                PIC  9(001) VALUE 0.
           05 FOLD-OFF                PIC  9(001) VALUE 0.
           05 WEEK-OFF                PIC  9(001) VALUE 0.
           05 DATE-OFF                PIC  9(001) VALUE 0.
           05 TIME-OFF                PIC  9(001) VALUE 0.
           05 ESPACOS                 PIC  X(078) VALUE SPACES.
           05 SEM-PATH                PIC  X(012) VALUE SPACES.
           05 INSTANCIA-ESPECIAL      PIC  X(006) VALUE "<>".
              88 ORIGINAL                         VALUE X"3C3E".
           05 INSTANCIA               PIC  X(006) VALUE "CWIMPR".
           05                         PIC  X(001)  VALUE SPACE.
              88 TRADUTOR                          VALUE "*".
           05                         PIC  X(001)  VALUE SPACE.
              88 QUIET                             VALUE "q".
              88 NO-QUIET                          VALUE SPACE.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
              10 STRING-START         PIC  9(004) COMP-X VALUE 1.
           05 SPOOL-COMMAND                        VALUE SPACES.
              10 SPOOL-COMMAND-BYTE    PIC  X(001) OCCURS 50.
           05 RESULTADO         COMP-X PIC  9(002) VALUE ZERO.
           05 FUNCAO-35         COMP-X PIC  9(002) VALUE 35.
           05 SIZE-PAGE                PIC  9(006) VALUE 0.
           05 SPOOL-DEV                PIC  X(001) VALUE SPACE.
           05 SPOOL-REMOTO             PIC  X(050) VALUE SPACE.
           05 SPOOL-CMD                PIC  X(050) VALUE SPACE.
           05 FOLHA-1                  PIC  9(004) VALUE 1.
           05 START-REPORT             PIC  9(001) VALUE 1.
           05 PRINTS-WS                PIC  X(550) VALUE SPACES.
           05 PRINT-X                  PIC  9(002) COMP-X.
           05 E1                       PIC  9(003) VALUE 0.
           05 E2                       PIC  9(002) VALUE 0.
           05 ASCII-I                  PIC  X(050) VALUE LOW-VALUES.
           05 ASCII-F                  PIC  X(050) VALUE LOW-VALUES.
           05 OLD-CWDIRS               PIC  X(050) VALUE SPACES.
           05 OLD-CWDIRS-IDX           PIC  X(050) VALUE SPACES.
           05 SAVE-CWDIRS              PIC  X(050) VALUE SPACES.
           05 VEZ-CWDIRS               PIC  9(001) VALUE 1.
           05 SALVA-CWCONF             PIC X(2008) VALUE SPACE.
           05 FIM                      PIC  X(001) VALUE SPACE.
           05 APELIDO                  PIC  X(008) VALUE SPACES.
           05 PATH-SPOOL               PIC  X(030) VALUE SPACES.
           05 TIPO-SAIDA               PIC  X(001) VALUE SPACE.
              88 IMPRESSORA VALUE "I".
              88 SPOOL-ON   VALUE "S".
           05 SALVA-DETAIL             PIC  X(223) VALUE SPACES.
           05 SALVA-PAGINA-FOLHA       PIC  9(001) VALUE 0.
           05 APAGA-LINHA              PIC  9(002) COMP-X VALUE 0.
           05 CX                       PIC  9(002) COMP-X VALUE 0.
           05 BLANK-ON                 PIC  9(001) VALUE 1.
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 PRINTER-NO               PIC  9(002) COMP-X VALUE 0.
           05 PRINTER-STATUS           PIC  9(002) COMP-X VALUE 0.
           05 HEADERS                  PIC  9(001) VALUE ZERO.
           05 VEZ                      PIC  9(003) VALUE ZERO.
           05 OPERADOR                 PIC  X(030).
           05 TASK                     PIC  9(006) VALUE 0.
           05 STATUS-REPORT            PIC  9(001) VALUE 1.
           05 SPOOL                    PIC  X(050) VALUE "PRN".
           05 SPOOL-JOB                PIC  X(255) VALUE SPACES.
           05 SPOOL-LABEL              PIC  X(050) VALUE SPACES.
           05 SPOOL-WORK               PIC  X(050) VALUE SPACES.
           05 TI                       PIC  9(004) VALUE ZERO.
           05 PRIMEIRA                 PIC  9(004) VALUE ZERO.
           05 ULTIMA                   PIC  9(004) VALUE ZERO.
           05 QUEUE-REPORT             PIC  9(004) VALUE ZERO.
           05 FOLHA-REPORT             PIC  9(004) VALUE ZERO.
           05 RK-PRINTS           COMP PIC  9(010) VALUE ZERO.
           05 ER-CWDIRS.
              10 FS-CWDIRS             PIC  X(002) VALUE "00".
              10 LB-CWDIRS             PIC  X(050) VALUE "CWDIRS".
           05 TITULO.
              10 BYTE-TITULO           PIC  X(001) OCCURS 500.
           05 SUB-TITULO.
              10 BYTE-SUB-TITULO       PIC  X(001) OCCURS 500.
           05 TITULO-W                 PIC  X(174) VALUE SPACES.
           05 TAMANHO                  PIC  9(003) VALUE ZERO.
           05 SIZE-LINE                PIC  9(003) VALUE ZERO.
           05 LIMITE                   PIC  9(003) VALUE ZERO.
           05 ER-CWSPLN.
              10 FS-CWSPLN             PIC  X(002) VALUE "00".
              10 LB-CWSPLN             PIC  X(050) VALUE "CWSPLN".
           05 RK-CWSPLN           COMP PIC  9(003) VALUE 1.
           05 DIRECAO                  PIC  9(001) VALUE 2.
           05 CHAMADAS         COMP-3  PIC  9(009) VALUE ZERO.
           05 LINES-TOP                PIC  9(002) VALUE 10.
           05 LINES-TOP-2              PIC  9(002) VALUE 11.
           05 INT1 PIC X(18) VALUE "------------------".
           05 INT2 PIC X(18) VALUE "LISTAGEM CANCELADA".
           05 TIPO-FORM                PIC  X(020) VALUE
              "(132 Colunas)".
           05 DESPROGRAMA              PIC  X(002) VALUE LOW-VALUES.
           05 CHAR                     PIC  X(001) VALUE SPACE.
           05 SUBSCRIPTS.
              10 U                     PIC  9(003) VALUE ZERO.
              10 T                     PIC  9(003) VALUE ZERO.
              10 J                     PIC  9(003) VALUE ZERO.
              10 I                     PIC  9(003) VALUE ZERO.
              10 II                    PIC  9(003) VALUE ZERO.
              10 E                     PIC  9(003) VALUE ZERO.
              10 S1                    PIC  9(003) VALUE ZERO.
              10 S2                    PIC  9(003) VALUE ZERO.
              10 S3                    PIC  9(003) VALUE ZERO.
              10 S4                    PIC  9(003) VALUE ZERO.
              10 R                     PIC  9(003) VALUE ZERO.
              10 R2                    PIC  9(003) VALUE ZERO.
              10 D2                    PIC  9(006) VALUE ZERO.
              10 A                     PIC  9(003) VALUE ZERO.
              10 Y                     PIC  9(003) VALUE ZERO.
              10 L                     PIC  9(003) VALUE ZERO.
              10 L2                    PIC  9(003) VALUE ZERO.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 PARAR      VALUE "S" "s".
              88 CANCELOU   VALUE "C".
              88 DESTROI    VALUE "D".
           05 FL-PRINTS                PIC  X(001) VALUE "N".
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 EJECT-MODE               PIC  X(002) VALUE SPACES.
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
           05 LB-PRINTS                            VALUE "PRINTS".
              88 IMP-0 VALUE "PRN:"  "LPT1:" "COM1:".
              88 IMP-1 VALUE         "LPT2:" "COM2:".
              88 IMP-2 VALUE         "LPT3:".
              10 BYTE-LB-PRINTS OCCURS 50 PIC X.
           05 ER-PRINTS.
              10 FS-PRINTS             PIC  X(002) VALUE "00".
              10 PRINTS-LB             PIC  X(100) VALUE "PRN".
              10 REDEFINES PRINTS-LB.
                 15 IMPR               PIC  X(008).
                 15                    PIC  X(042).
              10 REDEFINES PRINTS-LB.
                 15 BYTE-PRINTS-LB OCCURS 50 PIC 99 COMP-X.
              10 REDEFINES PRINTS-LB.
                 15 PRINTS-LB-ALFA OCCURS 50 PIC X.
           05 HOJE-ESPECIAL            PIC  9(006) VALUE 0.
           05 HORA-ESPECIAL            PIC  9(006) VALUE 0.
           05 HOJE.
              10 AA-H                  PIC  9(004).
              10 MM-H                  PIC  9(002).
              10 DD-H                  PIC  9(002).
           05 HORA.
              10 HH                    PIC  X(002).
              10 MM                    PIC  X(002).
              10 SS                    PIC  X(002).
              10 XX                    PIC  X(002).
           05 DATA-DE-HOJE.
              10 DD-H                  PIC  9(002) VALUE 0.
              10 FILLER                PIC  X(001) VALUE "/".
              10 MM-H                  PIC  9(002) VALUE 0.
              10 FILLER                PIC  X(001) VALUE "/".
              10 AA-H                  PIC  9(004) VALUE 0.
           05 DATA-HORA.
              10 CLIC-DATA-DE-HOJE     PIC  X(011) VALUE SPACES.
              10 CLIC-HORA.
                 15 CLIC-HH            PIC  9(002) VALUE ZEROS.
                 15 CLIC-HP            PIC  X(001) VALUE ":".
                 15 CLIC-MM            PIC  9(002) VALUE ZEROS.
           05 RELATORIO                PIC  X(007) VALUE SPACES.
           05 PAGINADO.
              10 DIA-SEMANA            PIC  X(003) VALUE SPACES.
              10 FILLER                PIC  X(001) VALUE SPACE.
              10 PAGE-TXT-pol.
                 15 FILLER             PIC  X(003) VALUE SPACES.
                 15 PAGINA-pol         PIC  9(004) VALUE ZEROS.
                 15 FILLER             PIC  X(001) VALUE "/".
              10 FOLHA-pol             PIC  9(004) VALUE ZEROS.
           05 TIT-W                    PIC  X(500) VALUE SPACES.
           05 programa-pol             PIC  X(002) VALUE low-values.
           05 clic-usuario             PIC  X(030) VALUE spaces.
           05 clic-sistema             PIC  X(030) VALUE spaces.
           05 file-handle              PIC  X(004) VALUE high-values.
           05 file-offset              pic x(8) comp-x.
           05 byte-count               pic x(4) comp-x.
           05 flags                    pic x comp-x.

           05 MSG                      PIC  X(030) VALUE "<<PRINTER>>".
           05 APAGA                    PIC  X(030) VALUE "<<<<<+>>>>>".
           05 VEZ-REPORT               PIC  9(001) VALUE 1.
           05 VEZ-CABECALHO            PIC  9(001) VALUE 2.
           05 VEZ-ERASE                PIC  9(001) VALUE 1.
           05 HORA-RELATORIO-ANTERIOR  PIC  X(010) VALUE ZERO.
           05 RE-START                 PIC  9(004) VALUE ZERO.
           05 LINES-PRINTER            PIC  9(010) VALUE 1000.
           05 DETALHE-ANTERIOR         PIC  X(500).
           05 FILLER REDEFINES DETALHE-ANTERIOR.
              10 BYTE-1 OCCURS 500 PIC X.
           05 DETALHE-ATUAL            PIC  X(500).
           05 FILLER REDEFINES DETALHE-ATUAL.
              10 BYTE-2 OCCURS 550 PIC X.
           05 TAB-ANTI-REPETECO        VALUE ALL "0".
              10 ADRESS OCCURS 6.
                 15 POS-STA PIC 9(003).
                 15 POS-END PIC 9(003).
           05 TAB-EM-BRANCO            VALUE ALL "0".
              10 ADRESS2 OCCURS 6.
                 15 POS-STA2 PIC 9(003).
                 15 POS-END2 PIC 9(003).
           05 CHECK-I-O      PIC X(009).
              88 FECHAR                 VALUE "CLOSE".
              88 ABRIR                  VALUE "OPEN".
              88 DESLIGAR-LINHA-BRANCA  VALUE "BLANK OFF".
           05 MP-I PIC 99 VALUE 0.
           05 MP-Y PIC 99 VALUE 0.
           05 PATH-W.
              10 BYTE-PATH PIC X OCCURS 50.
           05 PATH-MD VALUE SPACES.
              10 BYTE-PATH-MD PIC X OCCURS 50.
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

       01  PRNTER-REG.
           10 FILLER                   PIC  X(550).

       COPY CWMOUS.
       COPY CWSEND.
       COPY CWTIME.
       COPY CWUNIX.
       COPY CWEXEC.
       COPY CWBOXS.
       COPY CWGETL.
       COPY CWCONF.
       COPY CWGETS.

       01  LINHAS-pol                                    VALUE SPACES.
           05 LINHA-1                        PIC  X(220).
           05 LINHA-2                        PIC  X(220).
           05 LINHA-3                        PIC  X(220).

       01  AREAS-DE-TRABALHO-2.
           05 CWMENU                   PIC  X(001) VALUE SPACE.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 MSG-C VALUE "Cancelado: XXXXXXX p g.9999/9999".
              10 FILLER   PIC X(11).
              10 CWDIRS-C PIC X(7).
              10 FILLER   PIC X(5).
              10 PAGINA-C PIC X(4).
              10 FILLER   PIC X.
              10 FOLHA-C  PIC X(4).

       LINKAGE SECTION.

       01  PARAMETROS.
           05 CWIMPR-SETS                     PIC X(002).
              88 CWIMPR-SET-NO-OPTION                   VALUE "00".
              88 CWIMPR-SET-PAGE                        VALUE "01".
              88 CWIMPR-GET-PAGE                        VALUE "02".
              88 CWIMPR-SET-DATE                        VALUE "03".
              88 CWIMPR-SET-TIME                        VALUE "04".
              88 CWIMPR-SET-SIZE-PAGE                   VALUE "05".
              88 CWIMPR-SET-QUIET                       VALUE "06".
              88 CWIMPR-SET-PAGE-OFF                    VALUE "07".
              88 CWIMPR-SET-FOLD-OFF                    VALUE "08".
              88 CWIMPR-SET-WEEK-OFF                    VALUE "09".
              88 CWIMPR-SET-FOLD-PAGE-OFF               VALUE "10".
              88 CWIMPR-SET-DATE-OFF                    VALUE "11".
              88 CWIMPR-SET-TIME-OFF                    VALUE "12".
              88 CWIMPR-SET-PRINTER-SET                 VALUE "13".
              88 CWIMPR-SET-PRINTER-RESET               VALUE "14".
           05 CWIMPR-REPORT                  PIC X(007).
           05 CWIMPR-FORM-TYPE               PIC X(001).
              88 CWIMPR-SIZE-132                        VALUE "1".
              88 CWIMPR-SIZE-080                        VALUE "2".
              88 CWIMPR-SIZE-220                        VALUE "3".
              88 CWIMPR-END-PRINT                       VALUE "9".
           05 CWIMPR-SIZE-PAGE               PIC 9(003).
           05 CWIMPR-TITLE                   PIC X(174).
           05 CWIMPR-SUB-TITLE               PIC X(174).
           05 CWIMPR-HEADERS                            VALUE SPACES.
              15 CWIMPR-HEADER OCCURS 5      PIC X(500).
           05 CWIMPR-DETAIL                  PIC X(500).
           05 CWIMPR-TIME-REPORT             PIC X(010).
           05 CWIMPR-SPECIAL-PAGE            PIC 9(004).
           05 CWIMPR-SPECIAL-FOLD            PIC 9(004).
           05 CWIMPR-SPECIAL-DATE-TIME       PIC 9(006).
           05 CWIMPR-PAGE-TXT                PIC X(007).
           05 CWIMPR-TASK                    PIC 9(006).
           05 CWIMPR-NOTE                    PIC X(020).
           05 CWIMPR-ENTERPRISE              PIC X(030).
           05 CWIMPR-SYSTEM                  PIC X(030).
           05 CWIMPR-FULL             COMP-3 PIC 9(018).
           05 CWIMPR-EMPTY            COMP-3 PIC 9(018).
           05 CWIMPR-WIDTH                   PIC 9(003).
           05 CWIMPR-RESERVED                PIC X(017).

       PROCEDURE DIVISION USING PARAMETROS.

       000-INICIO.

           IF   CWIMPR-SIZE-PAGE = 59 AND CHAMADAS = 0
                DISPLAY "CWIMPR-SIZE-PAGE" UPON ENVIRONMENT-NAME
                PERFORM AJUSTA
                IF  NUMERO NOT = 0
                    MOVE NUMERO TO CWIMPR-SIZE-PAGE
                END-IF
           END-IF

           ON   1
                DISPLAY "CWPRINTFONT" UPON ENVIRONMENT-NAME
                ACCEPT CWPRINTFONT  FROM ENVIRONMENT-VALUE
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                ACCEPT COBWARE FROM ENVIRONMENT-VALUE
                IF  INSTANCIA = INSTANCIA-ESPECIAL
                    SET  TRADUTOR TO TRUE
                ELSE
                    DISPLAY "CWIMPR-QUIET" UPON ENVIRONMENT-NAME
                    ACCEPT TEST-QUIET FROM ENVIRONMENT-VALUE
                    INSPECT TEST-QUIET
                            CONVERTING MINUSCULAS TO MAIUSCULAS
                    IF TEST-QUIET = 'ON'
                       SET QUIET TO TRUE
                    END-IF
                END-IF
                DISPLAY "CWSPLEXT" UPON ENVIRONMENT-NAME
                ACCEPT CWSPLEXT    FROM ENVIRONMENT-VALUE
                IF   CWSPLEXT = SPACES OR LOW-VALUES
                     MOVE "SPL" TO CWSPLEXT
                END-IF
                MOVE CWSPLEXT TO SPL000000 (10: 3)
                                 SPL999999 (10: 3)
                DISPLAY "CWSPLTXT" UPON ENVIRONMENT-NAME
                ACCEPT CWSPLTXT    FROM ENVIRONMENT-VALUE
                IF   CWSPLTXT = SPACES OR LOW-VALUES
                     MOVE "txt" TO CWSPLTXT
                END-IF
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF CWUNIX-ON
                   MOVE '/' TO BARRA-DIR
                ELSE
                   MOVE '\' TO BARRA-DIR
                END-IF
                CALL "CWGETL" USING PARAMETROS-CWGETL
                IF   CWGETL-SPOOL = "2"
                     DISPLAY "LPT" UPON ENVIRONMENT-NAME
                     ACCEPT LPT FROM ENVIRONMENT-VALUE
                     IF   LPT (1: 2) = "\\"
                     OR   LPT (1: 5) = "/dev/"
                          MOVE LPT TO PRN
                     ELSE
                          IF   LPT (1: 1) < "1"
                          OR   LPT (1: 1) > "9"
                               IF   CWUNIX-ON
                                    MOVE "0" TO LPT
                               ELSE
                                    MOVE "1" TO LPT
                               END-IF
                               MOVE SPACES TO PRN
                               IF    CWUNIX-ON
                                     STRING "/dev/lp" LPT (1: 1)
                                             DELIMITED BY SIZE
                                       INTO PRN
                               ELSE
                                     STRING "LPT" LPT (1: 1) ":"
                                             DELIMITED BY SIZE
                                       INTO PRN
                               END-IF
                          END-IF
                     END-IF
                END-IF.

           MOVE PARAMETROS  TO BUFFER-CWIMPR

           IF  SPOOL-KEEP = 1
               CALL "CWIMPF" USING BUFFER-CWIMPR
           END-IF

           IF   CWIMPR-SET-SIZE-PAGE
                MOVE CWIMPR-SPECIAL-DATE-TIME TO SIZE-PAGE
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-DATE
                MOVE CWIMPR-SPECIAL-DATE-TIME TO HOJE-ESPECIAL
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-TIME
                MOVE CWIMPR-SPECIAL-DATE-TIME TO HORA-ESPECIAL
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-QUIET
                SET QUIET                TO TRUE
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-GET-PAGE
                MOVE PAGINA TO CWIMPR-SPECIAL-PAGE
                MOVE FOLHA  TO CWIMPR-SPECIAL-FOLD
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-WEEK-OFF
                MOVE 1                   TO WEEK-OFF
                MOVE SPACES              TO DIA-SEMANA
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-DATE-OFF
                MOVE 1 TO DATE-OFF
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-TIME-OFF
                MOVE 1 TO TIME-OFF
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-PRINTER-SET
                INSPECT CWIMPR-DETAIL (1: 2) CONVERTING SPACE TO X"00"
                MOVE CWIMPR-DETAIL TO PROGRAMA-POL
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-PRINTER-RESET
                INSPECT CWIMPR-DETAIL (1: 2) CONVERTING SPACE TO X"00"
                MOVE CWIMPR-DETAIL TO DESPROGRAMA
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-PAGE-OFF
           OR   CWIMPR-SET-FOLD-OFF
           OR   CWIMPR-SET-FOLD-PAGE-OFF
                MOVE CWIMPR-PAGE-TXT TO PAGE-TXT-POL
                IF   CWIMPR-SET-FOLD-PAGE-OFF
                     MOVE 1 TO FOLD-OFF
                               PAGE-OFF
                     MOVE SPACES TO FOLHA-POL (1: 4)
                ELSE
                     IF   CWIMPR-SET-PAGE-OFF
                          MOVE 1 TO PAGE-OFF
                          MOVE 0 TO FOLD-OFF
                     ELSE
                          MOVE 0 TO PAGE-OFF
                          MOVE 1 TO FOLD-OFF
                     END-IF
                END-IF
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF

           IF   CWIMPR-SET-PAGE
                MOVE 0                TO PAGE-OFF
                MOVE 0                TO FOLD-OFF
                MOVE "   0000/"       TO PAGE-TXT-POL
                MOVE CWIMPR-SPECIAL-PAGE TO PAGINA PAGINA-POL
                MOVE CWIMPR-SPECIAL-FOLD TO FOLHA  FOLHA-POL FOLHA-1
                MOVE 1                TO SALVA-PAGINA-FOLHA
                MOVE 1000             TO LINES-PRINTER
                SET CWIMPR-SET-NO-OPTION TO TRUE
                EXIT PROGRAM
           END-IF.

       CHECK-INCLUDE.

           IF   CWIMPR-END-PRINT
                PERFORM 180-REGISTRA-FIM THRU 180-99-FIM
           END-IF

           IF   CWIMPR-WIDTH NOT NUMERIC
           OR   CWIMPR-WIDTH = 0
                EVALUATE TRUE
                    WHEN CWIMPR-END-PRINT
                         CONTINUE
                    WHEN CWIMPR-SIZE-220
                         MOVE 220 TO CWIMPR-WIDTH
                    WHEN CWIMPR-SIZE-080
                         MOVE  80 TO CWIMPR-WIDTH
                    WHEN OTHER
                         MOVE 132 TO CWIMPR-WIDTH
                END-EVALUATE
           ELSE
                IF   CWIMPR-WIDTH > 500
                     MOVE 500 TO CWIMPR-WIDTH
                END-IF
           END-IF

           MOVE CWIMPR-TIME-REPORT      TO CHECK-I-O
           INSPECT CHECK-I-O CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   DESLIGAR-LINHA-BRANCA
                SET CWIMPR-SET-NO-OPTION TO TRUE
                MOVE    0            TO BLANK-ON
                ACCEPT CWIMPR-TIME-REPORT FROM TIME
                EXIT PROGRAM
           END-IF

           IF CWIMPR-DETAIL(1:1) = "$"
              MOVE CWIMPR-DETAIL(2:8) TO TEST-INCLUDE
              INSPECT TEST-INCLUDE CONVERTING MINUSCULAS
                                           TO MAIUSCULAS
              IF TEST-INCLUDE = "INCLUDE "
                 MOVE "$INCLUDE" TO CWIMPR-DETAIL(1:8)
              END-IF
           END-IF

           IF CWIMPR-DETAIL(1:8) = "$INCLUDE"
              MOVE CWIMPR-DETAIL(10:) TO LB-TEXTO
              MOVE SPACES TO CWIMPR-DETAIL
              OPEN INPUT TEXTO
              IF FS-TEXTO < "10"
                 READ TEXTO INTO CWIMPR-DETAIL
                 IF FS-TEXTO < "10"
                    MOVE 1 TO INCLUDE
                 END-IF
              END-IF
              IF INCLUDE = 0
                 EXIT PROGRAM
              END-IF
           END-IF

           IF   NOT FECHAR
                ADD     1                   TO CHAMADAS
                PERFORM 800-INICIAIS      THRU 800-99-FIM
                IF   NOT ABRIR
                     PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                ELSE
                     MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
                     MOVE 1           TO CWMOUS-MODE
                     CALL "CWMOUS" USING PARAMETROS-CWMOUS
                END-IF
                MOVE SPACES TO CHECK-I-O
           ELSE
                IF   CHAMADAS NOT = ZERO
                     PERFORM 900-FINAIS THRU 900-99-FIM
                END-IF
                IF   CWDIRS-LINHAS numeric
                and  CWDIRS-LINHAS > 0
                and  AUTOVIEW = 1
                and (NOT IMPRESSORA)
                AND ("CWHELP" NOT = CWIMPR-REPORT)
                     CALL "CWMEN9" USING CWDIRS-SPOOL
                     CANCEL "CWMEN9"
                     CALL "CWATCH"
                END-IF
                move 0 to CWDIRS-LINHAS
           END-IF

           SET CWIMPR-SET-NO-OPTION TO TRUE

           IF INCLUDE = 1
              READ TEXTO INTO CWIMPR-DETAIL
                   AT END
                      CLOSE TEXTO
                      DELETE FILE TEXTO
                      MOVE 0 TO INCLUDE
                   NOT AT END
                       GO TO CHECK-INCLUDE
              END-READ
           END-IF.

       000-99-FIM. EXIT PROGRAM.

       100-PROCESSAMENTO.

           CALL "CWATCH"

           IF  (CWIMPR-REPORT NOT = RELATORIO)
           AND (RELATORIO NOT = SPACES)
                IF  (DESPROGRAMA        NOT = LOW-VALUES)
                AND (RK-PRINTS          NOT = 0)
                     MOVE  DESPROGRAMA  TO PRNTER-REG
                                           PRINTS-WS
                     MOVE  LOW-VALUES   TO DESPROGRAMA
                     PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
                END-IF
                MOVE ALL "0" TO SUBSCRIPTS
                                TAB-ANTI-REPETECO
                                TAB-EM-BRANCO
                MOVE 3       TO VEZ-REPORT
                PERFORM 800-INICIAIS THRU 800-99-FIM.

           IF   CWIMPR-TIME-REPORT NOT = HORA-RELATORIO-ANTERIOR
           OR   CWIMPR-REPORT      NOT = RELATORIO
                MOVE CWIMPR-REPORT      TO RELATORIO
                MOVE CWIMPR-TIME-REPORT TO HORA-RELATORIO-ANTERIOR
                MOVE CWIMPR-TITLE       TO TITULO
                MOVE CWIMPR-SUB-TITLE   TO SUB-TITULO
                IF   SALVA-PAGINA-FOLHA = 0
                     IF   PAGE-OFF = 0
                     AND  FOLD-OFF = 0
                          MOVE ZERO TO PAGINA-POL
                     END-IF
                     MOVE ZERO      TO PAGINA
                     MOVE SIZE-PAGE TO LINES-PRINTER
                END-IF
                PERFORM 170-ALINHA-TITULOS THRU 170-99-FIM
           END-IF

           IF   CWIMPR-SIZE-PAGE = 99
                IF   PRIMEIRA = 0
                     MOVE 1 TO PRIMEIRA
                     MOVE 0 TO LINES-TOP
                END-IF
                IF   CWIMPR-DETAIL (1: 1) = X"0C"
                     ADD 1             TO FOLHA-REPORT FOLHA
                     MOVE FOLHA-REPORT TO ULTIMA
                     IF  (LINES-PRINTER NOT = 0)
                     AND  FOLHA-REPORT = 1
                          ADD 1 TO FOLHA-REPORT FOLHA
                     END-IF
                END-IF
           ELSE
           IF  (CWIMPR-SIZE-PAGE NOT = 99)
           AND (LINES-PRINTER NOT < SIZE-PAGE)
                IF   SALVA-PAGINA-FOLHA = 1
                     MOVE 0 TO SALVA-PAGINA-FOLHA
                ELSE
                     ADD 1 TO FOLHA
                              PAGINA
                     IF   PAGE-OFF = 0
                     AND  FOLD-OFF = 0
                          MOVE PAGINA TO PAGINA-POL
                          MOVE FOLHA  TO FOLHA-POL
                     ELSE
                          IF   PAGE-OFF = 0
                               MOVE FOLHA  TO FOLHA-POL
                          END-IF
                          IF   FOLD-OFF = 0
                               MOVE PAGINA TO FOLHA-POL
                          END-IF
                     END-IF
                END-IF
                ADD 1 TO FOLHA-REPORT
                IF    PRIMEIRA = ZERO
                      MOVE FOLHA TO PRIMEIRA
                END-IF
                MOVE FOLHA      TO ULTIMA
                MOVE LINES-TOP  TO LINES-PRINTER
                IF   FOLHA NOT < RE-START
                     PERFORM 140-CABECALHO THRU 140-99-FIM
                             TEST AFTER
                                  UNTIL TRADUTOR OR QUIET
                                     OR SPOOL-DEV = "$"
                                     OR (E-MAIL NOT = SPACES)
                                     OR VEZ-CABECALHO = 2.

           ADD   1            TO LINES-PRINTER
           MOVE DETALHE-ATUAL TO DETALHE-ANTERIOR
           MOVE CWIMPR-DETAIL TO DETALHE-ATUAL

           IF   LINES-PRINTER > LINES-TOP-2
           AND (FOLHA NOT < RE-START)
           AND  DETALHE-ANTERIOR NOT = SPACES
           AND  DETALHE-ATUAL    NOT = SPACES
                PERFORM 110-EVITA-REPETICAO THRU 110-99-FIM
                        VARYING Y FROM 1 BY 1
                        UNTIL Y > 6
                        OR    ADRESS (Y) = ZEROS.

           PERFORM 111-APAGA-CAMPOS THRU 111-99-FIM
                   VARYING Y FROM 1 BY 1
                   UNTIL Y > 6
                   OR    ADRESS2 (Y) = ZEROS.

           IF  NO-QUIET
           AND ((FOLHA NOT < RE-START)
           AND  (DIRECAO = 2))
                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                 MSG
                                                 X"001E"
           END-IF

           IF   FOLHA NOT < RE-START
                IF   CWIMPR-SIZE-PAGE = 99
                AND (PROGRAMA-POL NOT = LOW-VALUES)
                     IF   CWIMPR-DETAIL (1: 1) = X"0C"
                          MOVE CWIMPR-DETAIL(2: ) TO SALVA-DETAIL
                          MOVE PROGRAMA-POL       TO CWIMPR-DETAIL(1: 2)
                          MOVE X"0C"              TO CWIMPR-DETAIL(3: 1)
                          MOVE SALVA-DETAIL       TO CWIMPR-DETAIL(4: )
                     ELSE
                          MOVE CWIMPR-DETAIL      TO SALVA-DETAIL
                          MOVE SALVA-DETAIL       TO CWIMPR-DETAIL(3: )
                          MOVE PROGRAMA-POL       TO CWIMPR-DETAIL(1: 2)
                     END-IF
                END-IF
                MOVE  CWIMPR-DETAIL TO PRNTER-REG
                                       PRINTS-WS
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
           ELSE
                IF  NO-QUIET
                AND NEW = SPACES
                    CALL "CBL_WRITE_SCR_CHARS" USING X"1618"
                                                     FOLHA
                                                     X"0004"
                END-IF
           END-IF

           IF  NO-QUIET
           AND ((FOLHA NOT < RE-START)
           AND  (DIRECAO = 2))
                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                 APAGA
                                                 X"001E"
           END-IF.

       100-99-FIM. EXIT.

       110-EVITA-REPETICAO.

           PERFORM VARYING I FROM POS-STA (Y) BY 1
                           UNTIL I > POS-END (Y)
                           OR   BYTE-1 (I) NOT = BYTE-2 (I)
                    MOVE SPACE TO CWIMPR-DETAIL (I: 1)
           END-PERFORM

           IF   I NOT > POS-END (Y)
           AND  BYTE-1 (I) NOT = BYTE-2 (I)
                PERFORM VARYING I FROM POS-STA (Y) BY 1
                                UNTIL I > POS-END (Y)
                        MOVE DETALHE-ATUAL (I: 1)
                          TO CWIMPR-DETAIL (I: 1)
                END-PERFORM.

       110-99-FIM. EXIT.

       111-APAGA-CAMPOS.

           PERFORM VARYING I FROM POS-STA2 (Y) BY 1
                           UNTIL I > POS-END2 (Y)
                   MOVE SPACE TO CWIMPR-DETAIL (I: 1)
           END-PERFORM.

       111-99-FIM. EXIT.

       140-CABECALHO.

           MOVE PROGRAMA-POL TO PRNTER-REG

           IF   FOLHA > FOLHA-1
                MOVE X"0C" TO PRNTER-REG (3:1)
           ELSE
                MOVE X"00" TO PRNTER-REG (3:1)
           END-IF

           IF   DIRECAO = 2
           AND  NO-QUIET
                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                 MSG
                                                 X"001E"
           END-IF

           COMPUTE P = CWIMPR-WIDTH - 15
           MOVE PAGINADO TO LINHA-1 (P: 16)
           MOVE LINHA-1 TO PRNTER-REG (4:)
           PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
           MOVE LINHA-2 TO PRNTER-REG
           PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
           MOVE LINHA-3 TO PRNTER-REG
           PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM

           MOVE 0        TO HEADERS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                   IF   CWIMPR-HEADER (I) NOT = SPACES
                        MOVE  CWIMPR-HEADER (I) TO PRNTER-REG
                        ADD   1          TO HEADERS
                        PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
                   END-IF
           END-PERFORM

           IF   HEADERS NOT = 0
                MOVE  ALL "-"    TO PRNTER-REG (1: CWIMPR-WIDTH)
                ADD   1          TO HEADERS
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
           END-IF

           IF   BLANK-ON = 1
                MOVE SPACES     TO PRNTER-REG
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
           END-IF

           IF   DIRECAO = 2
           AND  NO-QUIET
                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                 APAGA
                                                 X"001E"
           END-IF

           IF   VEZ-CABECALHO = 1
           AND NO-QUIET
           AND (NOT SPOOL-DEV = "$")
           AND (E-MAIL = SPACES)
                MOVE "Posicionamento correto ?"
                  TO CWSEND-MSG
                MOVE SPACES     TO CWSEND-SCREENS RESPOSTA
                MOVE "  ~Sim__" TO CWSEND-SCREEN (1)
                MOVE "  ~NÆo "  TO CWSEND-SCREEN (2)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 1
                     MOVE 2     TO VEZ-CABECALHO
                     MOVE "S"   TO RESPOSTA
                ELSE
                     MOVE "N" TO RESPOSTA
                END-IF
           END-IF.

       140-99-FIM. EXIT.

       170-ALINHA-TITULOS.

           MOVE CLIC-USUARIO TO LINHA-1
           PERFORM VARYING T FROM 174 BY -1
                             UNTIL T = 1
                                OR (TIT-W (T: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM

           IF   TITULO NOT = SPACES
                MOVE SPACES TO TIT-W
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > LIMITE
                             OR BYTE-TITULO (I) NOT = SPACE
                           CONTINUE
                END-PERFORM
                MOVE ZERO TO T
                PERFORM VARYING I FROM I BY 1 UNTIL I > LIMITE
                        COMPUTE Y = I + 1
                   IF   NOT (BYTE-TITULO (I) = SPACE
                   AND       BYTE-TITULO (Y) = SPACE)
                        ADD  1               TO  T
                        MOVE BYTE-TITULO (I) TO TIT-W (T: 1)
                   END-IF
                END-PERFORM
                PERFORM VARYING T FROM 174 BY -1
                                  UNTIL T = 1
                                     OR (TIT-W (T: 1) NOT = SPACE)
                        CONTINUE
                END-PERFORM
                COMPUTE P = ((CWIMPR-WIDTH / 2) - (T / 2)) + 1
                MOVE TIT-W TO LINHA-1 (P: T)
           END-IF

           MOVE CLIC-SISTEMA TO LINHA-2
           COMPUTE P = CWIMPR-WIDTH - 15
           MOVE DATA-HORA TO LINHA-2 (P: 16)

           IF   SUB-TITULO NOT = SPACES
                MOVE SPACES TO TIT-W
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > LIMITE
                             OR BYTE-SUB-TITULO (I) NOT = SPACE
                          CONTINUE
                END-PERFORM
                MOVE ZERO TO T
                PERFORM VARYING I FROM I BY 1 UNTIL I > LIMITE
                        COMPUTE Y = I + 1
                        IF   NOT (BYTE-SUB-TITULO (I) = SPACE
                        AND       BYTE-SUB-TITULO (Y) = SPACE)
                             ADD  1                   TO T
                             MOVE BYTE-SUB-TITULO (I) TO TIT-W (T: 1)
                END-PERFORM
                COMPUTE P = ((CWIMPR-WIDTH / 2) - (T / 2)) + 1
                MOVE TIT-W TO LINHA-2 (P: T)
           END-IF

           MOVE ALL "-"   TO LINHA-3 (1: CWIMPR-WIDTH)
           COMPUTE P = CWIMPR-WIDTH - 7
           PERFORM VARYING I FROM LENGTH CWIMPR-REPORT BY -1
                             UNTIL I = 1
                                OR((CWIMPR-REPORT (I: 1) NOT = SPACE)
                                AND(CWIMPR-REPORT (I: 1) NOT = '-'))
                   ADD 1 TO P
           END-PERFORM
           MOVE CWIMPR-REPORT (1: I) TO LINHA-3 (P: I).

       170-99-FIM. EXIT.

       180-REGISTRA-FIM.

           MOVE  "?"              TO CWMENU
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     CWMENU

           MOVE 255               TO RETURN-CODE
           MOVE  "2"              TO CWMENU
           CALL  "CWGETU"      USING MSG-C
                                     TASK
                                     PROGRAMA
                                     CWMENU

           STOP RUN.

       180-99-FIM. EXIT.

       190-INSERE-TASK.

           MOVE  "?"              TO CWMENU
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     CWMENU.

           MOVE 1 TO I

           IF   NOT IMPRESSORA
                PERFORM 230-ADD-TASK THRU 230-99-FIM
                IF   PRINTS-LB-ALFA (1) = "."
                     MOVE SPACES TO PRINTS-LB
                END-IF
                PERFORM 195-INSERE-PATH THRU 195-99-FIM
          END-IF.

       190-99-FIM. EXIT.

       195-INSERE-PATH.

           IF   PATH-SPOOL NOT = SPACES
                MOVE PATH-SPOOL TO LB-PRINTS
                                   PATH-W
                PERFORM 220-MAKE-PATH THRU 220-99-FIM
                PERFORM VARYING I FROM 1 BY 1
                        UNTIL BYTE-LB-PRINTS (I) = SPACES
                           OR I > 30
                        IF   BYTE-LB-PRINTS (I) = "/"
                        AND  CWUNIX-OFF
                             MOVE "\" TO BYTE-LB-PRINTS (I)
                        END-IF
                        IF  BYTE-LB-PRINTS (I) = "\"
                        AND CWUNIX-ON
                            MOVE "/" TO BYTE-LB-PRINTS (I)
                        END-IF
                END-PERFORM
                SUBTRACT 1 FROM I
                IF   BYTE-LB-PRINTS (I) = "\" OR "/"
                     SUBTRACT 1 FROM I
                END-IF
                ADD  1         TO I
                IF   CWUNIX-ON
                     MOVE "/"  TO LB-PRINTS (I: )
                ELSE
                     MOVE "\"  TO LB-PRINTS (I: )
                END-IF
                ADD  1         TO I
                MOVE LB-PRINTS TO LB-CWDIRS
           ELSE
                MOVE 1 TO I
           END-IF

           MOVE PRINTS-LB      TO LB-PRINTS     (I: )
                                  SEM-PATH
           MOVE "CWDIRS"       TO LB-CWDIRS     (I: ).

       195-99-FIM. EXIT.

       200-IMPRIME-LINHA.

           ADD  1 TO RK-PRINTS

           IF   IMPRESSORA
                MOVE 0   TO APAGA-LINHA
                MOVE 144 TO PRINTER-STATUS
                IF  (PRINTER-NO NOT = 255)
                AND ((CWUNIX-DOS16 OR CWUNIX-DOS32) AND DOS-DEVICE)
                AND  SPOOL-DEV NOT = "$"
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
                                  IF   APAGA-LINHA = 0
                                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                             "Impressora fora de linha "
                                                 X"0019"
                                       CALL X"E5"
                                       MOVE 1 TO APAGA-LINHA
                                  END-IF
                                  PERFORM 210-CHECK-INTERRUPCAO
                                     THRU 210-99-FIM
                             END-IF
                     END-PERFORM
                END-IF
                IF   APAGA-LINHA = 1
                AND  NO-QUIET
                     CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                      ESPACOS
                                                      X"0044"
                END-IF
                IF   VEZ-ERASE = 1
                     MOVE 2 TO VEZ-ERASE
                     IF   NO-QUIET
                          CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                           ESPACOS
                                                           X"0044"
                     END-IF
                END-IF
                IF   NO-QUIET
                     IF   DIRECAO = 0
                          CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                           MSG
                                                           X"001E"
                          MOVE 1 TO DIRECAO
                     ELSE
                          IF DIRECAO = 1
                             CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                              APAGA
                                                              X"001E"
                             MOVE 0 TO DIRECAO
                          END-IF
                     END-IF
                END-IF
                PERFORM 205-GRAVA-LINHA THRU 205-99-FIM
           ELSE
                MOVE X'0D0A' TO PRNTER-REG (byte-count - 1: 2)
                call "CBL_WRITE_FILE" using file-handle
                                 file-offset
                                 byte-count
                                 X"00"
                                 PRNTER-REG
                IF RETURN-CODE = 0
                   ADD byte-count TO file-offset
                ELSE
                   MOVE '24' TO FS-PRINTS
                END-IF
                IF   FS-PRINTS NOT = "00"
                AND  FS-PRINTS NOT = "37"
                     CALL "CWISAM" USING ER-PRINTS
                     MOVE    "C"           TO RESPOSTA
                     MOVE    CWIMPR-REPORT TO CWDIRS-C
                     MOVE    PAGINA-pol    TO PAGINA-C
                     MOVE    FOLHA-pol     TO FOLHA-C
                     PERFORM 900-FINAIS    THRU 900-99-FIM
                     MOVE    "9"           TO CWIMPR-FORM-TYPE
                     EXIT PROGRAM
                END-IF
           END-IF

           PERFORM 210-CHECK-INTERRUPCAO THRU 210-99-FIM.

       200-99-FIM. EXIT.

       205-GRAVA-LINHA.

           IF   START-REPORT = 1
md    *    AND  SPOOL-DEV NOT = "$"
                MOVE 0 TO START-REPORT
                IF   EJECT-MODE = "10" OR "11"
                     WRITE PRINTS-REG FROM X"0C"
                END-IF
                IF   ASCII-I NOT = LOW-VALUES
                     PERFORM VARYING A FROM 1 BY 1 UNTIL A > 50
                        IF   ASCII-I (A: 1) = "\"
                        AND (NOT POSTER)
                             ADD  1              TO A
                             MOVE ASCII-I (A: 1) TO PRINT-X (1: 1)
                             IF   PRINT-X > 96
                             AND  PRINT-X < 113
                                  COMPUTE E1 = PRINT-X - 96
                                  MOVE 1 TO COM-ESTILO
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
                                  SUBTRACT 1 FROM A
                             END-IF
                      ELSE
                             IF   ASCII-I (A: 1) NOT = X"00"
                                  WRITE PRINTS-REG FROM ASCII-I (A: 1)
                             END-IF
                      END-IF
                     END-PERFORM
                END-IF
           END-IF

           MOVE PRNTER-REG TO PRINTS-WS

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
           AND RK-PRINTS = 1
               IF PRINTS-WS (CWIMPR-WIDTH:) = SPACES
                  MOVE X'2E' TO PRINTS-WS (CWIMPR-WIDTH: )
               END-IF
           END-IF
           PERFORM VARYING TAMANHO FROM SIZE-LINE BY -1
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
                             MOVE 1 TO COM-ESTILO
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
                             SUBTRACT 1 FROM A
                        END-IF
                   ELSE
                        IF   A = TAMANHO
                        AND  PRINTS-WS (A: 1) = X"0D"
                             MOVE 1 TO AFTER-0
                        ELSE
                             IF PRINTS-WS (A: 1) NOT = X"00"
                                IF POSTER
                                AND (PRINTS-WS(A: 1) = X"0F" OR X"12")
                                    CONTINUE
                                ELSE
                                  WRITE PRINTS-REG FROM PRINTS-WS (A: 1)
                                END-IF
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           IF  AFTER-0 = 1
               WRITE PRINTS-REG FROM X"0D"
           ELSE
               IF  (CWUNIX-OFF AND NOT FORMATO-UNIX)
               OR   FORMATO-DOS
                    WRITE PRINTS-REG FROM X"0D"
               END-IF
               WRITE PRINTS-REG FROM X"0A"
           END-IF.

       205-99-FIM. EXIT.

       210-CHECK-INTERRUPCAO.

           IF   TRADUTOR
                EXIT PARAGRAPH
           END-IF

           IF   NO-QUIET
                IF   CWIMPR-EMPTY NOT NUMERIC
                     MOVE 0 TO CWIMPR-EMPTY
                END-IF

                IF   CWIMPR-FULL NOT NUMERIC
                     MOVE 0 TO CWIMPR-FULL
                END-IF
                IF CWIMPR-EMPTY NOT = 0
                AND SPOOL-KEEP = "0"
                   EXEC COBOLware PROCESS (SHOW)
                        HEADER CWIMPR-TITLE
                        LINE 16 COLUMN 12
                        MESSAGE "Gerando relat¢rio"
                        QUESTION "Deseja interromper o relat¢rio ? "
                        CANCEL RESPOSTA;RESPOSTA
                        EMPTY CWIMPR-EMPTY
                        FULL  CWIMPR-FULL
                  END-EXEC
                END-IF
                IF  RESPOSTA = "Y"
                    MOVE "S" TO RESPOSTA
                END-IF
                GO TO 210-99-FIM
           END-IF
           IF   QUIET
                GO TO 210-99-FIM
           END-IF
           CALL "CBL_WRITE_SCR_CHARS" USING X"1635"
                                            "[esc]-Interrompe"
                                            X"0010"
           MOVE LOW-VALUES  TO PARAMETROS-CWMOUS
           MOVE 1           TO CWMOUS-MODE
           MOVE ALL X"01"   TO CWMOUS-LINE (23) (54: 16)
           CALL "CWMOUS" USING PARAMETROS-CWMOUS

           IF   CWMOUS-KEY = 1
                MOVE X"1B" TO CHAR
           END-IF

           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS

           IF   KEY-STATUS   = 1
           OR   CWMOUS-KEY = 1
                IF   CWMOUS-KEY NOT = 1
                     CALL "CBL_READ_KBD_CHAR" USING CHAR
                END-IF
                IF   CHAR = X"1B"
                     MOVE 0 TO APAGA-LINHA
                     MOVE "Deseja interromper o relat¢rio ? "
                       TO CWSEND-MSG
                     MOVE SPACES      TO CWSEND-SCREENS RESPOSTA
                     MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
                     MOVE "~Continuar" TO CWSEND-SCREEN (2)
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     IF   CWSEND-OPTION = 1
                          MOVE "S" TO RESPOSTA
                     END-IF
                     IF   PARAR
                          MOVE    "C"           TO RESPOSTA
                          MOVE    CWIMPR-REPORT TO CWDIRS-C
                          MOVE    PAGINA        TO PAGINA-C
                          MOVE    FOLHA         TO FOLHA-C
                          PERFORM 900-FINAIS  THRU 900-99-FIM
                          MOVE    "9"           TO CWIMPR-FORM-TYPE
                          EXIT PROGRAM.

       210-99-FIM. EXIT.

       220-MAKE-PATH.

           IF   PATH-W = SPACES
                GO TO 220-99-FIM.

           MOVE SPACES TO PATH-MD
           PERFORM VARYING MP-I FROM 50 BY -1
                             UNTIL MP-I = 0
                                OR (BYTE-PATH (MP-I) NOT = " ")
                   CONTINUE
           END-PERFORM

           MOVE MP-I TO MP-Y
           ADD  1    TO MP-Y
           PERFORM VARYING MP-I FROM 1 BY 1 UNTIL MP-I > MP-Y
                   IF   BYTE-PATH (MP-I) = "\" OR "/" OR " "
                        IF   CWUNIX-ON
                             MOVE "/" TO BYTE-PATH (MP-I)
                        ELSE
                             MOVE "\" TO BYTE-PATH (MP-I)
                        END-IF
                        CALL "CBL_CREATE_DIR" USING PATH-MD
                   END-IF
                   MOVE BYTE-PATH (MP-I) TO BYTE-PATH-MD (MP-I)
           END-PERFORM.

       220-99-FIM. EXIT.

       230-ADD-TASK.

           EXEC COBOLware GetSystem
                LEVEL;NIVEL
           END-EXEC

           IF   SPOOL-DEV = "$"
                MOVE "sp000000.txt" TO PRINTS-LB
                MOVE CWSPLTXT       TO PRINTS-LB (10: )
                IF   CWUNIX-OFF
                     MOVE CWCONF-LABEL (2: ) TO WINPRINT POSTER-C
                     IF   WINPRINT (8: 1) = ":"
                          MOVE WINPRINT (9: ) TO WINDOWS-PRINTER
                          MOVE SPACES TO WINPRINT (9:)
                          PERFORM POSTER-CHECK THRU FIM-POSTER-CHECK
                     END-IF
                     INSPECT WINPRINT CONVERTING MINUSCULAS
                                              TO MAIUSCULAS
                     IF   POSTER
                          MOVE "wp"  TO PRINTS-LB (01: 2)
                          MOVE "spl" TO PRINTS-LB (10: 3)
                     END-IF
                END-IF
           ELSE
                MOVE NIVEL TO SPL000000(2:1)
                MOVE SPL000000      TO PRINTS-LB
           END-IF

           IF   POSTER
                CALL "CWTASK" USING "3" CWDIRS-NUMERO
                MOVE '.' TO CWDIRS-PONTO
           ELSE
                MOVE NIVEL TO SPL999999(2:1)
                MOVE SPL999999      TO CWDIRS-SPOOL
                START CWDIRS KEY NOT GREATER CWDIRS-SPOOL
                IF   FS-CWDIRS = "47"
                     PERFORM 195-INSERE-PATH THRU 195-99-FIM
                     OPEN INPUT CWDIRS
                     START CWDIRS KEY NOT GREATER CWDIRS-SPOOL
                     READ CWDIRS PREVIOUS RECORD IGNORE LOCK
                     CLOSE CWDIRS
                ELSE
                     READ CWDIRS PREVIOUS RECORD IGNORE LOCK
                END-IF
                ADD     1                 TO CWDIRS-NUMERO
                IF   CWDIRS-NUMERO = 0
                     MOVE 1 TO CWDIRS-NUMERO
                END-IF
           END-IF
           MOVE    CWDIRS-NUMERO     TO CWIMPR-TASK
           MOVE    CWSPLEXT          TO CWDIRS-SPOOL (10: 3)
           MOVE    "R"               TO CWDIRS-SPOOL (1:1)
           MOVE    NIVEL             TO CWDIRS-SPOOL (2:1)
           MOVE    CWDIRS-SPOOL      TO PRINTS-LB
           PERFORM 195-INSERE-PATH THRU 195-99-FIM.

       230-99-FIM. EXIT.

       240-LABEL-SPOOL.

           MOVE    CWCONF-LABEL (2: ) TO SPOOL-WORK
                                         WINPRINT POSTER-C
           PERFORM 230-ADD-TASK     THRU 230-99-FIM

==>        IF   SPOOL-DEV = "$"
==>             MOVE SPACES TO TEMP
==>             DISPLAY "CWSPLTMP" UPON ENVIRONMENT-NAME
==>             ACCEPT   TEMP  FROM ENVIRONMENT-VALUE
==>             IF   TEMP NOT = SPACES
==>                  MOVE TEMP TO PATH-W
==>                  PERFORM 220-MAKE-PATH THRU 220-99-FIM
==>                  PERFORM VARYING S1 FROM LENGTH OF LB-PRINTS BY -1
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
==>        END-IF

           MOVE    LB-PRINTS          TO SPOOL-LABEL
           MOVE    SPOOL-WORK         TO LB-PRINTS

           MOVE SPACES    TO SPOOL-JOB
           MOVE 0         TO S1 S3 S4
           ADD  12        TO I

           PERFORM VARYING S4
                      FROM 50 BY -1
                      UNTIL BYTE-LB-PRINTS (S4) NOT = SPACE
                         OR S4 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S2
                      FROM 50 BY -1
                      UNTIL SPOOL-LABEL (S2: 1) NOT = SPACE
                         OR S2 = 1
                   CONTINUE
           END-PERFORM

           PERFORM VARYING S1
                      FROM 1 BY 1 UNTIL S1 > S4
                   ADD 1 TO S3
                   EVALUATE BYTE-LB-PRINTS (S1)
                       WHEN "$"
                            MOVE SPOOL-LABEL TO SPOOL-JOB (S3: )
                            IF   CWUNIX-OFF
                                 INSPECT SPOOL-JOB (S3: ) CONVERTING
                                         "/" TO "\"
                            END-IF
                            ADD  S2          TO S3
                            SUBTRACT 1     FROM S3
                       WHEN "#"
                            MOVE "1"         TO SPOOL-JOB (S3: )
                       WHEN "@"
                            MOVE CWIMPR-REPORT TO SPOOL-JOB (S3: )
                            ADD  6             TO S3
                       WHEN OTHER
                            MOVE BYTE-LB-PRINTS (S1) TO SPOOL-JOB(S3: 1)
                   END-EVALUATE
           END-PERFORM

           MOVE SPOOL-LABEL TO PRINTS-LB LB-PRINTS.

       240-99-FIM. EXIT.

       800-INICIAIS.

           IF   VEZ-REPORT = 1
                DISPLAY "CWIMPR-PAGE" UPON ENVIRONMENT-NAME
                MOVE SPACES TO PAGEOFF
                ACCEPT  PAGEOFF  FROM ENVIRONMENT-VALUE
                INSPECT PAGEOFF  CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWIMPR-FOLD" UPON ENVIRONMENT-NAME
                MOVE SPACES TO FOLDOFF
                ACCEPT  FOLDOFF  FROM ENVIRONMENT-VALUE
                INSPECT FOLDOFF  CONVERTING MINUSCULAS TO MAIUSCULAS
                IF  PAGEOFF = "OFF" OR FOLDOFF = "OFF"
                    MOVE CWIMPR-PAGE-TXT TO PAGE-TXT-POL
                    IF   PAGEOFF = "OFF" AND FOLDOFF = "OFF"
                         MOVE 1 TO FOLD-OFF
                                   PAGE-OFF
                         MOVE SPACES TO FOLHA-POL (1: 4)
                    ELSE
                         IF   PAGEOFF = "OFF"
                              MOVE 1 TO PAGE-OFF
                              MOVE 0 TO FOLD-OFF
                         ELSE
                              MOVE 0 TO PAGE-OFF
                              MOVE 1 TO FOLD-OFF
                         END-IF
                    END-IF
                END-IF
                MOVE 10 TO LINES-TOP
                COMPUTE LIMITE     = CWIMPR-WIDTH - 31 - 17
                COMPUTE SIZE-LINE  = CWIMPR-WIDTH + 3
                compute byte-count = CWIMPR-WIDTH + 5
                IF   CWIMPR-WIDTH > 132
                     MOVE 223             TO SIZE-LINE
                     MOVE "(220 em 132c)" TO TIPO-FORM
                ELSE
                     IF   CWIMPR-WIDTH < 81
                          MOVE 083            TO SIZE-LINE
                          MOVE "(80 Colunas)" TO TIPO-FORM
                     ELSE
                          MOVE 135             TO SIZE-LINE
                          MOVE "(132 Colunas)" TO TIPO-FORM
                     END-IF
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                     IF   CWIMPR-HEADER(I) = SPACES
                          SUBTRACT 1 FROM LINES-TOP
                     END-IF
                END-PERFORM
                IF   BLANK-ON = 0
                     SUBTRACT 1 FROM LINES-TOP
                END-IF
                IF   CWIMPR-HEADERS  = SPACES
                     SUBTRACT 1 FROM LINES-TOP
                END-IF
                COMPUTE LINES-TOP-2 = LINES-TOP + 1.

           IF   VEZ-REPORT = 1
           OR   VEZ-REPORT = 3
           OR   ABRIR
                IF   SIZE-PAGE = 0
                     MOVE CWIMPR-SIZE-PAGE TO SIZE-PAGE
                END-IF
                IF   HOJE-ESPECIAL NOT = 0
                     MOVE ZERO                 TO HOJE
                     MOVE HOJE-ESPECIAL (1: 2) TO HOJE (7: 2)
                     MOVE HOJE-ESPECIAL (3: 2) TO HOJE (5: 2)
                     MOVE HOJE-ESPECIAL (5: 2) TO HOJE (3: 2)
                     IF   AA-H IN HOJE  < 90
                          ADD 2000 TO AA-H IN HOJE
                     ELSE
                          ADD 1900 TO AA-H IN HOJE
                     END-IF
                ELSE
                     SET CWTIME-REVERSED     TO TRUE
                     SET CWTIME-TODAY        TO TRUE
                     CALL "CWTIME"        USING PARAMETROS-CWTIME
                     MOVE CWTIME-DATE-FINAL  TO HOJE
                END-IF
                MOVE    CORR HOJE            TO DATA-DE-HOJE
                IF   DATE-OFF = 0
                     MOVE DATA-DE-HOJE       TO CLIC-DATA-DE-HOJE
                ELSE
                     MOVE SPACES             TO CLIC-DATA-DE-HOJE
                END-IF
                IF   WEEK-OFF = 0
                     MOVE    HOJE          TO CWTIME-DATE
                     SET CWTIME-REVERSED   TO TRUE
                     SET CWTIME-WEEK       TO TRUE
                     CALL "CWTIME"      USING PARAMETROS-CWTIME
                     MOVE CWTIME-WEEK-CHAR TO DIA-SEMANA
                END-IF
                PERFORM 810-LER-CWCONF THRU 810-99-FIM.

           IF   VEZ-REPORT = 1
                IF   HORA-ESPECIAL NOT = 0
                     MOVE HORA-ESPECIAL TO HORA
                ELSE
                     ACCEPT HORA FROM TIME
                END-IF
                IF  TIME-OFF = 0
                    MOVE    HH               TO CLIC-HH
                    MOVE    ":"              TO CLIC-HP
                    MOVE    MM               TO CLIC-MM
                ELSE
                    MOVE SPACES TO CLIC-HORA
                END-IF
                PERFORM 820-CHECK-PRINTS THRU 820-99-FIM
                        UNTIL FL-PRINTS = "O"
                IF   IMPRESSORA
                AND  CWGETL-PRTPOSIT = 1
                     MOVE 255 TO PRINTER-NO
                     IF IMP-0 MOVE 0 TO PRINTER-NO END-IF
                     IF IMP-1 MOVE 1 TO PRINTER-NO END-IF
                     IF IMP-2 MOVE 2 TO PRINTER-NO END-IF
                     MOVE 1 TO VEZ-CABECALHO
                     MOVE 0 TO DIRECAO
                     IF  NO-QUIET
                     AND (NOT IMPRESSORA)
                          MOVE SPACES TO PAP
                          STRING "Impressora: "
                                        APELIDO DELIMITED BY SIZE
                                           INTO PAP
                          CALL "CBL_WRITE_SCR_CHARS" USING X"0428"
                                                           PAP
                                                           X"0014"
                          IF   SPOOL-DEV NOT = "$"
                               MOVE SPACES TO CWSEND-SCREENS
                                              CWSEND-MSG
                               STRING "Check a impressora "
                                       DELIMITED BY SIZE
                               APELIDO DELIMITED BY SPACE
                               " "     DELIMITED BY SIZE
                               TIPO-FORM DELIMITED BY SIZE
                                         INTO CWSEND-MSG
                               CALL "CWSEND" USING PARAMETROS-CWSEND
                          IF   CWIMPR-SIZE-PAGE NOT = 99
                               CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                                ESPACOS
                                                                X"0044"
                               CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
            "Se desejar RE-START, informe a primeira folha:    <Enter>"
                                                                X"0039"
                               MOVE LOW-VALUES TO PARAMETROS-CWMOUS
                               MOVE ALL X"01" TO CWMOUS-LINE(23) (54: 5)
                               MOVE "2350"    TO CWMOUS-CURSOR-POSITION
                               CALL "CWMOUS" USING PARAMETROS-CWMOUS
                               IF   CWMOUS-KEY = 1
                                    MOVE X"0D" TO CHAR
                               END-IF
                               CALL "CBL_GET_KBD_STATUS" USING
                                    KEY-STATUS
                               IF   KEY-STATUS   = 1
                               OR   CWMOUS-KEY = 1
                                    IF   CWMOUS-KEY NOT = 1
                                         ACCEPT (23, 50) RE-START
                                    END-IF
                               END-IF
                               CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                                ESPACOS
                                                                X"0044"
                               IF   RE-START NOT = ZERO
                                    MOVE SPACES TO PAP
                                    STRING "Preparando RE-START..."
                                            FOLHA-POL "/" RE-START
                                            DELIMITED BY SIZE INTO PAP
                               CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                                PAP
                                                                X"0044"
                               END-IF
                          END-IF
                          END-IF
                     END-IF
                ELSE
                     IF  NO-QUIET
                     AND (IMPRESSORA)
                          MOVE SPACES TO PAP
                          STRING "Spool em: " SEM-PATH DELIMITED BY SIZE
                                                   INTO PAP
                          CALL "CBL_WRITE_SCR_CHARS" USING X"0428"
                                                           PAP
                                                           X"001C"
                     END-IF
                     MOVE    SEM-PATH        TO MSG
                     MOVE    SPACES          TO APAGA
                     IF   STATUS-REPORT = 2
                          PERFORM 860-CHECK-DIR THRU 860-99-FIM
                     END-IF
                     PERFORM 860-CHECK-DIR THRU 860-99-FIM.

           IF   ABRIR
                MOVE HORA-RELATORIO-ANTERIOR TO CWIMPR-TIME-REPORT.

           MOVE 2 TO VEZ-REPORT.

       800-99-FIM. EXIT.

       810-LER-CWCONF.

           CALL "CWGETS" USING PARAMETROS-CWGETS
           IF   CWIMPR-ENTERPRISE NOT = SPACES
                MOVE CWIMPR-ENTERPRISE         TO CLIC-USUARIO
           ELSE
                MOVE CWGETS-EMPRESA-RELATORIOS TO CLIC-USUARIO
           END-IF
           IF   CWIMPR-SYSTEM NOT = SPACES
                MOVE  CWIMPR-SYSTEM            TO CLIC-SISTEMA
           ELSE
                MOVE CWGETS-SISTEMA-RELATORIOS TO CLIC-SISTEMA
           END-IF
           MOVE LOW-VALUES TO ASCII-I ASCII-F
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           CALL "CWCONF" USING "ISAM"
           MOVE  "?"              TO CWMENU
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     CWMENU.

           MOVE 0        TO AUTOVIEW
           MOVE "PS"     TO CWCONF-TIPO
           MOVE OPERADOR TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF = "23"
                MOVE "SPOOL"  TO CWCONF-PATH-SPOOL
                MOVE "CWSRUN" TO CWCONF-PRINTER-DEFAULT
           ELSE
               IF   CWCONF-AUTOVIEW = 1
                    MOVE 1 TO AUTOVIEW
               END-IF
               IF   CWCONF-PATH-SPOOL = SPACES
                    MOVE "SPOOL"  TO CWCONF-PATH-SPOOL
               ELSE
                    IF   CWCONF-PATH-SPOOL (1: 1) = "\"
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
           END-IF

           IF   CWCONF-PRINTER-DEFAULT = SPACES OR LOW-VALUES OR "Spool"
                MOVE "<Spool>" TO CWCONF-PRINTER-DEFAULT
           END-IF

           MOVE CWCONF-PATH-SPOOL       TO PATH-SPOOL
           MOVE "03"                    TO CWCONF-TIPO
           MOVE CWCONF-PRINTER-DEFAULT  TO CWCONF-ARQUIVO
           MOVE CWCONF-PRINTER-DEFAULT2 TO CWCONF-ARQUIVO(9:)
           INSPECT CWCONF-ARQUIVO CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE CWCONF-ARQUIVO TO APELIDO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "9D"
                CALL "CWCONF" USING "ISAM"
           END-IF

           MOVE SPACES TO ESTILO
                          WINPRINT POSTER-C
           MOVE "0"    TO SPOOL-KEEP
           IF   FS-CWCONF < "10"
                SET IMPRESSORA         TO TRUE
                MOVE CWCONF-ESTILO     TO ESTILO
                MOVE CWCONF-FORMATO    TO FORMATO
                MOVE CWCONF-CODEPAGE   TO CODEPAGE
                MOVE CWCONF-EJECT-MODE TO EJECT-MODE
                IF   CWCONF-ASCII = X"FF"
                     MOVE CWCONF-CADEIA-ASCII-INICIAL TO ASCII-I
                     MOVE CWCONF-CADEIA-ASCII-FINAL   TO ASCII-F
                END-IF
                PERFORM 910-REMOTA THRU 910-99-FIM
                MOVE CWCONF-LABEL      TO PRINTS-LB SPOOL-DEV
                IF   SPOOL-DEV NOT = "$"
                     PERFORM 815-CHECK-SPOOL-COMMAND THRU 815-99-FIM
                ELSE
                     PERFORM 240-LABEL-SPOOL THRU 240-99-FIM
                END-IF
           ELSE
                MOVE CWCONF-PRINTER-DEFAULT TO CWCONF-LABEL
                SET SPOOL-ON TO TRUE
                MOVE "00" TO EJECT-MODE
           END-IF
      *    IF   CWCONF-LABEL NOT = SPOOL
           IF   SPOOL-DEV NOT = "$"
                MOVE CWCONF-LABEL TO PRINTS-LB
                                     SPOOL
                PERFORM 190-INSERE-TASK THRU 190-99-FIM
                IF   FL-PRINTS = "O"
                     MOVE "N"                 TO FL-PRINTS
                     call "CBL_CLOSE_FILE" using file-handle
                     move high-values         to file-handle
                     move 0                   to file-offset
                     MOVE 1                   TO VEZ-REPORT
                END-IF
           END-IF

           MOVE "94"          TO CWCONF-TIPO
           MOVE CWIMPR-REPORT TO CWCONF-RELATORIO
           MOVE OPERADOR      TO CWCONF-NAME-REPORT
           SET CWSQLC-READ    TO TRUE
           SET CWSQLC-EQUAL   TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "23"
                MOVE "94"              TO CWCONF-TIPO
                MOVE CWIMPR-REPORT     TO CWCONF-RELATORIO
                MOVE SPACES            TO CWCONF-NAME-REPORT
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-EQUAL       TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   FS-CWCONF < "10"
                IF  CWCONF-QUIET = "1"
                    SET QUIET TO TRUE
                END-IF
                MOVE CWCONF-PROGRAMA-PRINTER TO PROGRAMA-POL
                MOVE CWCONF-DESPROGRAMA      TO DESPROGRAMA
                MOVE CWCONF-TIPO-FORM        TO TIPO-FORM
                IF   CWCONF-TITLE NOT = SPACES
                     MOVE CWCONF-TITLE TO CWIMPR-TITLE
                END-IF
                IF   CWCONF-SUB-TITLE NOT = SPACES
                     MOVE CWCONF-SUB-TITLE TO CWIMPR-SUB-TITLE
                END-IF
                IF  (CWCONF-EMPRESA-ALT NOT = SPACES)
                AND (CWCONF-EMPRESA-ALT NOT = LOW-VALUES)
                     MOVE CWCONF-EMPRESA-ALT TO CLIC-USUARIO
                END-IF
                IF  (CWCONF-SISTEMA-ALT NOT = SPACES)
                AND (CWCONF-SISTEMA-ALT NOT = LOW-VALUES)
                     MOVE CWCONF-SISTEMA-ALT TO CLIC-SISTEMA
                END-IF
                IF   CWCONF-SIZE-PAGE NUMERIC
                AND  CWCONF-SIZE-PAGE NOT = ZERO
                     MOVE CWCONF-SIZE-PAGE TO CWIMPR-SIZE-PAGE
                                              SIZE-PAGE
                END-IF
                PERFORM 830-CARGA-CAMPOS THRU 830-99-FIM
                        VARYING I FROM 1 BY 1
                                UNTIL I > 6
                PERFORM 840-CARGA-CAMPOS-APAGADOS THRU 840-99-FIM
                        VARYING Y FROM 1 BY 1
                                UNTIL Y > 6
                IF  (CWCONF-SAIDA NOT = SPACE)
                AND (CWCONF-SAIDA NOT = SPOOL)
revair          AND (CWCONF-SAIDA NOT = "<Spool>")
rofer           AND (CWCONF-SAIDA NOT = "<Default>")
                     MOVE  CWCONF-SAIDA TO PRINTS-LB
                                           SPOOL
                     MOVE CWCONF-REG94 TO SALVA-CWCONF
                     MOVE "03"      TO CWCONF-TIPO
                     MOVE PRINTS-LB TO CWCONF-ARQUIVO
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   FS-CWCONF < "10"
                     AND  NO-QUIET
                     AND  CWCONF-KEEP = "1"
                          MOVE "2"   TO SPOOL-KEEP
                          MOVE "23"  TO FS-CWCONF
                     END-IF
                     IF   FS-CWCONF < "10"
                          SET IMPRESSORA         TO TRUE
                          IF  CWCONF-KEEP = "1"
                              IF  NOT TRADUTOR
                                  MOVE "1"         TO SPOOL-KEEP
                                  CALL "CWIMPF" USING BUFFER-CWIMPR
                              END-IF
                          END-IF
                          MOVE CWCONF-ESTILO     TO ESTILO
                          MOVE CWCONF-FORMATO    TO FORMATO
                          MOVE CWCONF-CODEPAGE   TO CODEPAGE
                          MOVE CWCONF-EJECT-MODE TO EJECT-MODE
                          MOVE CWCONF-ARQUIVO    TO APELIDO
                          perform 910-remota thru 910-99-fim
                          MOVE CWCONF-LABEL      TO PRINTS-LB SPOOL-DEV
                          IF   CWCONF-ASCII = X"FF"
                               MOVE CWCONF-CADEIA-ASCII-INICIAL
                                 TO ASCII-I
                               MOVE CWCONF-CADEIA-ASCII-FINAL
                                 TO ASCII-F
                          END-IF
                          IF   SPOOL-DEV NOT = "$"
                               PERFORM 815-CHECK-SPOOL-COMMAND
                                  THRU 815-99-FIM
                          ELSE
                               PERFORM 240-LABEL-SPOOL THRU 240-99-FIM
                          END-IF
                     ELSE
                          SET SPOOL-ON TO TRUE
                          MOVE "00" TO EJECT-MODE
                     END-IF
                     MOVE SALVA-CWCONF TO CWCONF-REG94
                     IF   SPOOL-DEV NOT = "$"
                          PERFORM 190-INSERE-TASK THRU 190-99-FIM
                     END-IF
                     IF   FL-PRINTS = "O"
                          MOVE  "N"          TO FL-PRINTS
                          call "CBL_CLOSE_FILE" using file-handle
                          move high-values to file-handle
                          move 0 to file-offset
                          MOVE  1            TO VEZ-REPORT
                     END-IF
                END-IF
           ELSE
                IF   FS-CWCONF NOT = "23"
                     CALL "CWCONF" USING "ISAM"
                END-IF
           END-IF

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

           IF   CWGETL-SPOOL = "2"
                MOVE PRN TO SPOOL-LABEL PRINTS-LB LB-PRINTS APELIDO
                SET IMPRESSORA TO TRUE
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       810-99-FIM. EXIT.

       815-CHECK-SPOOL-COMMAND.

           MOVE CWCONF-LABEL TO E-MAIL
           MOVE 0            TO ARROBA
      *    MOVE 0            TO PONTO
           PERFORM VARYING E FROM 1 BY 1 UNTIL E > LENGTH OF E-MAIL
                   IF   E-MAIL (E: 1) = "@"
                        MOVE E TO ARROBA
                   END-IF
      *            IF   E-MAIL (E: 1) = "."
      *            AND  ARROBA > 0
      *                 MOVE E TO PONTO
      *            END-IF
           END-PERFORM
      *    IF  PONTO > ARROBA
           IF  ARROBA NOT = 0
               MOVE SPACES TO CWCONF-LABEL
               STRING "em" TASK ".txt" DELIMITED BY SIZE
                          INTO CWCONF-LABEL
               MOVE CWCONF-LABEL TO PRINTS-LB SPOOL-DEV
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
           IF   SPOOL-COMMAND-BYTE (1)  = "("
                MOVE CWCONF-LABEL (2: ) TO SPOOL-COMMAND
                PERFORM VARYING R FROM 1 BY 1
                          UNTIL R > 50
                             OR SPOOL-COMMAND-BYTE (R) = ")"
                        CONTINUE
                END-PERFORM
                MOVE R TO R2
                IF R < 50
                   ADD 1 TO R
                   PERFORM VARYING R FROM R BY 1
                             UNTIL R > 50
                                OR SPOOL-COMMAND-BYTE (R) NOT = " "
                           CONTINUE
                   END-PERFORM
                   IF R < 50
                      MOVE SPOOL-COMMAND (R: ) TO CWCONF-LABEL
                      MOVE SPACES       TO SPOOL-COMMAND (R2: )
                      IF SPOOL-COMMAND NOT = SPACES
                         MOVE SPOOL-COMMAND TO CWEXEC-COMANDO
                         set CWEXEC-NOWARNING  to true
                         set CWEXEC-ASSYNCRONE to true
                         CALL "CWEXE2" USING PARAMETROS-CWEXEC
                      END-IF
                   END-IF
                END-IF
           END-IF.

       815-99-FIM. EXIT.

       820-CHECK-PRINTS.

           IF  NO-QUIET
               CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                ESPACOS
                                                X"0044"
           END-IF

           MOVE SPACES  TO RESPOSTA
                           NEW
                           EXTW

           IF   IMPRESSORA
                MOVE "O" TO FL-PRINTS
                CALL "CWLABE" USING PRINTS-LB NEW EXTW
                CANCEL "CWLABE"
                IF  (EXTW NOT = SPACES)
                AND NEW = SPACES
                    SET CWIMPR-END-PRINT TO TRUE
                    GO TO 820-99-FIM
                END-IF
                IF  NEW NOT = SPACES
                    SET QUIET TO TRUE
                    CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                     NEW
                                                     X"001E"
                END-IF
                OPEN OUTPUT PRINTS
           ELSE
                IF file-handle NOT = high-values
                   call "CBL_CLOSE_FILE" using file-handle
                   move high-values to file-handle
                END-IF
                move 0 to file-offset
                call "CBL_CREATE_FILE" using LB-PRINTS
                                           X"02" X"00" X"00"
                                           file-handle
                MOVE "O" TO FL-PRINTS
                MOVE "D" TO RESPOSTA.

       820-99-FIM. EXIT.

       830-CARGA-CAMPOS.

           IF    CWCONF-FIM-TB (I) > CWCONF-INICIO-TB (I)
           AND   CWCONF-INICIO-TB (I) NOT = ZERO
                 ADD  1                    TO L
                 MOVE CWCONF-INICIO-TB (I) TO POS-STA (L)
                 MOVE CWCONF-FIM-TB    (I) TO POS-END (L).

       830-99-FIM. EXIT.

       840-CARGA-CAMPOS-APAGADOS.

           IF    CWCONF-FIM-AP (Y) > CWCONF-INICIO-AP (Y)
           AND   CWCONF-INICIO-AP (Y) NOT = ZERO
                 ADD  1                    TO L2
                 MOVE CWCONF-INICIO-AP (Y) TO POS-STA2 (L2)
                 MOVE CWCONF-FIM-AP    (Y) TO POS-END2 (L2)
                 PERFORM VARYING I FROM CWCONF-INICIO-AP (Y) BY 1
                                   UNTIL I > CWCONF-FIM-AP (Y)
                         MOVE SPACE TO CWIMPR-HEADER(1) (I: 1)
                                       CWIMPR-HEADER(2) (I: 1)
                                       CWIMPR-HEADER(3) (I: 1)
                                       CWIMPR-HEADER(4) (I: 1)
                                       CWIMPR-HEADER(5) (I: 1)
                 END-PERFORM
           END-IF.

       840-99-FIM. EXIT.

       860-CHECK-DIR.

           IF  STATUS-REPORT = 9
               GO TO 860-99-FIM.

           CLOSE CWDIRS
           OPEN INPUT CWDIRS
           IF   FS-CWDIRS (1: 1) = "9"
                CALL "CWISAM" USING ER-CWDIRS
           END-IF
           IF   VEZ-CWDIRS = 2
                IF  (FS-CWDIRS = "30" OR "35")
                     OPEN OUTPUT CWDIRS
                     CLOSE CWDIRS
                     OPEN I-O CWDIRS
                ELSE
                     IF   FS-CWDIRS > "09"
                          CALL "CWISAM" USING ER-CWDIRS
                          SET CWIMPR-END-PRINT    TO TRUE
                          GOBACK
                     ELSE
                          CLOSE CWDIRS
                          OPEN I-O CWDIRS
                     END-IF
                END-IF
           END-IF

           IF   FS-CWDIRS > "09"
                MOVE "A lista de spool ser  reiniciada" TO CWSEND-MSG
                IF   FS-CWDIRS = "30" OR "35"
                     CONTINUE
                ELSE
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     DELETE FILE CWDIRS
                END-IF
                OPEN OUTPUT CWDIRS
                CLOSE CWDIRS
                GO TO 860-CHECK-DIR
           ELSE
                CLOSE CWDIRS
                OPEN I-O CWDIRS
           END-IF

           IF   STATUS-REPORT = 1
                PERFORM TEST AFTER UNTIL STATUS-REPORT = 2
                                     AND FS-CWDIRS < "10"
                        MOVE SEM-PATH        TO CWDIRS-SPOOL
                        READ CWDIRS IGNORE LOCK
                        IF   FS-CWDIRS < "10"
                             PERFORM 230-ADD-TASK THRU 230-99-FIM
                        END-IF
                        MOVE ZERO             TO FOLHA-REPORT
                                                 RK-PRINTS
                        MOVE SPACES           TO CWDIRS-COMANDO
                                                 CWDIRS-ESTILO
                                                 CWDIRS-IMPRESSO
                        MOVE CWIMPR-REPORT    TO CWDIRS-CODIGO
                        MOVE PROGRAMA-pol     TO CWDIRS-PROGRAMA
                        EVALUATE TRUE
                                 WHEN CWIMPR-WIDTH = 132
                                      MOVE '1' TO CWDIRS-TIPO
                                 WHEN CWIMPR-WIDTH = 80
                                      MOVE '2' TO CWDIRS-TIPO
                                 WHEN CWIMPR-WIDTH = 220
                                      MOVE '3' TO CWDIRS-TIPO
                                 WHEN OTHER
                                      MOVE CWIMPR-FORM-TYPE
                                               TO CWDIRS-TIPO
                        END-EVALUATE
                        MOVE ZERO             TO CWDIRS-FOLHAS
                                                 CWDIRS-LINHAS
                        MOVE HH               TO CWDIRS-HORA
                        MOVE MM               TO CWDIRS-MINUTO
                        MOVE HOJE             TO CWDIRS-DATA
                        MOVE OPERADOR         TO CWDIRS-USUARIO
                        MOVE "Gerando..."     TO CWDIRS-NOTA
                        MOVE CWIMPR-TITLE     TO CWDIRS-TITULO
                        MOVE CWIMPR-WIDTH     TO CWDIRS-WIDTH
                        WRITE CWDIRS-REG
                        IF   FS-CWDIRS = "22"
                             PERFORM 230-ADD-TASK THRU 230-99-FIM
                             MOVE   "22"            TO FS-CWDIRS
                        ELSE
                             IF   FS-CWDIRS > "09"
                                  CALL "CWISAM" USING ER-CWDIRS
                             ELSE
                                  READ CWDIRS WITH LOCK
                                  MOVE 2 TO STATUS-REPORT
                             END-IF
                        END-IF
                END-PERFORM
           ELSE
                PERFORM TEST AFTER UNTIL FS-CWDIRS NOT = "9D"
                        MOVE SEM-PATH     TO CWDIRS-SPOOL
                        MOVE 1            TO STATUS-REPORT
                        READ CWDIRS LOCK KEY IS CWDIRS-SPOOL
                        IF   FS-CWDIRS = "9D"
                             CALL "CWISAM" USING ER-CWDIRS
                        END-IF
                END-PERFORM
                IF   FS-CWDIRS > "09"
                     CALL "CWISAM" USING ER-CWDIRS
                ELSE
                     MOVE FOLHA-REPORT    TO CWDIRS-FOLHAS
                     MOVE HH              TO CWDIRS-HORA
                     MOVE MM              TO CWDIRS-MINUTO
                     MOVE HOJE            TO CWDIRS-DATA
                     MOVE COM-ESTILO      TO CWDIRS-ESTILO
                     MOVE RK-PRINTS       TO CWDIRS-LINHAS
                     IF   CWIMPR-NOTE = SPACES
                          MOVE SEM-PATH    TO CWDIRS-NOTA
ks                        IF CWIMPR-TITLE NOT = SPACES
ks                           MOVE CWIMPR-TITLE TO CWDIRS-NOTA
ks                        END-IF
                     ELSE
                          MOVE CWIMPR-NOTE TO CWDIRS-NOTA
                     END-IF
                     REWRITE CWDIRS-REG
                     UNLOCK CWDIRS
                END-IF
           END-IF

           CLOSE CWDIRS.

       860-99-FIM. EXIT.

       900-FINAIS.

           IF   CANCELOU
           AND  NOT (IMPRESSORA AND PRINTER-STATUS NOT = 144)
                MOVE  INT1        TO PRNTER-REG
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
                MOVE  INT2        TO PRNTER-REG
                MOVE "Interrompido" TO CWIMPR-NOTE
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM
                MOVE  INT1        TO PRNTER-REG
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM.

           IF   DESPROGRAMA NOT = LOW-VALUES
           AND  NOT (IMPRESSORA AND PRINTER-STATUS NOT = 144)
                MOVE  DESPROGRAMA TO PRNTER-REG
                PERFORM 200-IMPRIME-LINHA THRU 200-99-FIM.

           IF   IMPRESSORA
                IF ((ASCII-F NOT = LOW-VALUES)
                OR  (EJECT-MODE = "01" OR "11"))
md    *         AND  SPOOL-DEV NOT = "$"
                     PERFORM VARYING A FROM 1 BY 1 UNTIL A > 50
                        IF   ASCII-F (A: 1) = "\"
                        AND (NOT POSTER)
                             ADD  1              TO A
                             MOVE ASCII-F (A: 1) TO PRINT-X (1: 1)
                             IF   PRINT-X > 96
                             AND  PRINT-X < 113
                                  COMPUTE E1 = PRINT-X - 96
                                  MOVE 1 TO COM-ESTILO
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
                                  SUBTRACT 1 FROM A
                             END-IF
                      ELSE
                             IF   ASCII-F (A: 1) NOT = X"00"
                                  WRITE PRINTS-REG FROM ASCII-F (A: 1)
                             END-IF
                      END-IF
                     END-PERFORM
                     IF   EJECT-MODE = "01" OR "11"
                          WRITE PRINTS-REG FROM X"0C"
                     END-IF
                END-IF
                CLOSE PRINTS
                IF   E-MAIL NOT = SPACES
                     IF   CWIMPR-TITLE = SPACES
                          MOVE "Spool COBOLware 5.0" TO CWIMPR-TITLE
                     END-IF
                     MOVE SPACES TO EMAIL-TEXT
                    IF CWUNIX-OFF
                     STRING
                 "Em anexo relat¢rio gerado por sistema no formato PDF^"
                            CLIC-SISTEMA "^"
                            CLIC-USUARIO "^^"
                            "Para visualizar e imprimir o anexo, pode "
                            "ser usado o Adobe Reader em: "
                          "http://www.brasil.adobe.com/products/acrobat"
                          "/readermain.html"
                                DELIMITED BY SIZE
                                INTO EMAIL-TEXT
                     ELSE
                          STRING
                          "Em anexo relat¢rio gerado por sistema^"
                           CLIC-SISTEMA "^"
                           CLIC-USUARIO
                           DELIMITED BY SIZE INTO EMAIL-TEXT
                     END-IF
                     EXEC COBOLware Mail
                          TO E-MAIL
                          REPORT PRINTS-LB
                          Subject CWIMPR-TITLE
                             Text EMAIL-TEXT
                     END-EXEC
                END-IF
                IF   SPOOL-DEV = "$"
                     set CWEXEC-NOWARNING  to true
                     set CWEXEC-ASSYNCRONE to true
                     IF  SPOOL-REMOTO NOT = SPACES
                         IF   CWUNIX-DOS16
                         OR   CWUNIX-WIN16
                         OR   SPOOL-REMOTO(1:1) = '"'
                              MOVE SPACES TO SPOOL-CMD CWEXEC-COMANDO
                              DISPLAY "OS" UPON ENVIRONMENT-NAME
                              ACCEPT SPOOL-CMD  FROM ENVIRONMENT-VALUE
                              IF  SPOOL-CMD NOT = "Windows_NT"
                                  STRING "COPY "      DELIMITED BY SIZE
                                         LB-PRINTS    DELIMITED BY SPACE
                                         " "          DELIMITED BY SIZE
                                   SPOOL-REMOTO(1:D2) DELIMITED BY SIZE
                                         INTO CWEXEC-COMANDO
                              ELSE
                                  STRING "PRINT /D:"   DELIMITED BY SIZE
                                    SPOOL-REMOTO(1:D2) DELIMITED BY SIZE
                                         " "          DELIMITED BY SIZE
                                         LB-PRINTS    DELIMITED BY SPACE
                                         INTO CWEXEC-COMANDO
                              END-IF
                              CALL "CWEXE2" USING PARAMETROS-CWEXEC
                         ELSE
                             CALL "CBL_COPY_FILE" USING LB-PRINTS
                                                        SPOOL-REMOTO
                         END-IF
                         CALL "CBL_DELETE_FILE" USING LB-PRINTS
                         NEXT SENTENCE
                     END-IF
                     CALL "CBL_READ_SCR_CHATTRS" USING X"0000"
                                                       CARACTER-BUFFER
                                                       ATTRIBUTE-BUFFER
                                                       STRING-LENGTH
                     IF   CWUNIX-OFF
                          set CWEXEC-NOWARNING  to true
                          set CWEXEC-ASSYNCRONE to true
                          INSPECT WINPRINT CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                          IF   POSTER
                               MOVE "D" TO ORIENTACAO
                               PERFORM VARYING I FROM 9 BY 1
                                       UNTIL I > LENGTH OF WINPRINT
                                          OR WINPRINT (I: 1) NOT = SPACE
                                          CONTINUE
                               END-PERFORM
                               IF I < LENGTH OF WINPRINT
                                  MOVE WINPRINT (I: 1) TO ORIENTACAO
                               END-IF
                               PERFORM POSTER-CHECK
                                  THRU FIM-POSTER-CHECK
                               IF   ORIENTACAO = "S" OR "s"
                                    MOVE 10           TO CWBOXS-LINE
                                                         CWBOXS-COLUMN
                                    MOVE "Orienta‡Æo:" TO CWBOXS-TITLE
                                    MOVE SPACES       TO CWBOXS-ITENS
                                    MOVE "~Paisagem"  TO CWBOXS-TEXT (1)
                                    MOVE "~Retrato"   TO CWBOXS-TEXT (2)
                                    MOVE 0            TO CWBOXS-OPTION
                                    PERFORM UNTIL CWBOXS-OPTION <> 0
                                            CALL "CWBOXS"
                                            USING PARAMETROS-CWBOXS
                                    END-PERFORM
                                    MOVE CWBOXS-OPTION-CHAR
                                      TO ORIENTACAO
                               END-IF
                               MOVE SPACES TO SPOOL-JOB
                               IF   ORIENTACAO = "D"
                                    MOVE SPACES TO LB-TEXTO
                                    STRING "poster"  DELIMITED BY SIZE
                                           TASK      DELIMITED BY SIZE
                                           ".ini"    DELIMITED BY SIZE
                                           INTO LB-TEXTO
                                    STRING  COBWARE DELIMITED BY SPACE
                                     "\POSTER.EXE " DELIMITED BY SIZE
                                           LB-TEXTO DELIMITED BY SPACE
                                                 INTO SPOOL-JOB
                                    OPEN OUTPUT TEXTO
                                    WRITE TEXTO-REG FROM "[Config]"
                                    MOVE "Infile=" TO TEXTO-REG
                                    MOVE SPOOL-LABEL TO TEXTO-REG(8:)
                                    WRITE TEXTO-REG
                                    IF   WINPRINT (1: 8) = "WINVIEW "
                                         WRITE TEXTO-REG
                                          FROM "OutFile=VIEW"
                                         WRITE TEXTO-REG
                                          FROM "PrinterMode=Graphic"
                                    ELSE
                                          IF WINPRINT(1:8) = "WINDOWS:"
                                          OR WINPRINT(1:8) = "USBTEXT:"
                                             IF   WINDOWS-PRINTER
                                                  NOT = SPACES
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
                                             WRITE TEXTO-REG
                                              FROM "Quiet=Yes"
                                          ELSE
                                             WRITE TEXTO-REG
                                             FROM "OutFile=PRINTER"
                                          END-IF
                                    END-IF
                                    IF CWPRINTFONT NOT = SPACES
                                       MOVE "Font="     TO TEXTO-REG
                                       MOVE CWPRINTFONT TO TEXTO-REG(6:)
                                       WRITE TEXTO-REG
                                    END-IF
                                    MOVE SPACES TO TEXTO-REG
                                    STRING "Title="   DELIMITED BY SIZE
                                        CWDIRS-CODIGO DELIMITED BY SPACE
                                                 ": " DELIMITED BY SIZE
                                             TITULO   DELIMITED BY SIZE
                                             INTO TEXTO-REG
                                    WRITE TEXTO-REG
                                    IF   WINPRINT (1: 8) = "USBTEXT "
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
                                    MOVE TITULO TO TITULO-W
                                    INSPECT TITULO-W
                                            CONVERTING "><" TO "]["
                                    STRING COBWARE DELIMITED BY SPACE
                                   "\WINPRINT.EXE " DELIMITED BY SIZE
                                         ORIENTACAO DELIMITED BY SIZE
                                                " " DELIMITED BY SIZE
                                        SPOOL-LABEL DELIMITED BY SPACE
                                                " " DELIMITED BY SIZE
                                      CWIMPR-REPORT DELIMITED BY SPACE
                                               ": " DELIMITED BY SIZE
                                           TITULO-W DELIMITED BY SIZE
                                               INTO SPOOL-JOB
                               END-IF
                               MOVE SPOOL-JOB TO CWEXEC-COMANDO
                               MOVE 5         TO CWEXEC-RETORNO
                               CALL "CWEXE2" USING PARAMETROS-CWEXEC
                          ELSE
                               MOVE SPOOL-JOB TO CWEXEC-COMANDO
                               CALL "CWEXE2" USING PARAMETROS-CWEXEC
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
                                                        CARACTER-BUFFER
                                                        ATTRIBUTE-BUFFER
                                                        STRING-LENGTH
                ELSE
                     IF  (NEW NOT = SPACE)
                     AND (EXTW NOT = SPACES)
                          set CWEXEC-NOWARNING  to true
                          set CWEXEC-ASSYNCRONE to true
                          PERFORM POSTER-CHECK THRU FIM-POSTER-CHECK
                          MOVE SPACES TO LB-TEXTO CWEXEC-COMANDO
                          STRING "poster"  DELIMITED BY SIZE
                                 TASK      DELIMITED BY SIZE
                                 ".ini"    DELIMITED BY SIZE
                                 INTO LB-TEXTO
                          STRING  COBWARE DELIMITED BY SPACE
                           "\POSTER.EXE " DELIMITED BY SIZE
                                 LB-TEXTO DELIMITED BY SPACE
                                       INTO CWEXEC-COMANDO
                          OPEN OUTPUT TEXTO
                          WRITE TEXTO-REG FROM "[Config]"
                          MOVE "Infile=" TO TEXTO-REG
                          MOVE PRINTS-LB TO TEXTO-REG(8:)
                          WRITE TEXTO-REG
                          MOVE "Outfile=" TO TEXTO-REG
                          MOVE NEW        TO TEXTO-REG(9:)
                          WRITE TEXTO-REG
                          WRITE TEXTO-REG FROM "Quiet=Yes"
                          MOVE "OutputFormat=" TO TEXTO-REG
                          INSPECT EXTW CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
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
                             WRITE TEXTO-REG FROM POSTER-BUFFER
                             MOVE  SPACES      TO POSTER-BUFFER
                          END-IF
                          CLOSE TEXTO
                          CALL "CWEXE2" USING  PARAMETROS-CWEXEC
                     END-IF
                END-IF
           ELSE
                IF   STATUS-REPORT = 2
                     PERFORM 860-CHECK-DIR THRU 860-99-FIM
                END-IF
                call "CBL_CLOSE_FILE" using file-handle
                move high-values to file-handle.

           MOVE   "N"              TO FL-PRINTS
           MOVE "   0000/"         TO PAGE-TXT-pol
           MOVE   ZERO             TO CHAMADAS
                                      PAGE-OFF
                                      FOLD-OFF
                                      WEEK-OFF
                                      DATE-OFF
                                      TIME-OFF
                                      RE-START
                                      FOLHA
                                      FOLHA-POL
                                      ULTIMA
                                      PRIMEIRA
                                      RK-PRINTS
                                      HOJE-ESPECIAL
                                      HORA-ESPECIAL
                                      SIZE-PAGE
                                      COM-ESTILO
           MOVE   1                TO VEZ-REPORT
                                      BLANK-ON
                                      VEZ-ERASE
                                      START-REPORT
                                      FOLHA-1
           MOVE   2                TO VEZ-CABECALHO
           MOVE   900              TO LINES-PRINTER
           MOVE   SPACES           TO RELATORIO
                                      SPOOL-DEV
                                      SPOOL-REMOTO
                                      FORMATO
                                      CODEPAGE
                                      NEW
                                      EXTW
                                      CWIMPR-DETAIL
           MOVE   LOW-VALUES       TO DESPROGRAMA
                                      PROGRAMA-POL
           MOVE   "PRN"            TO PRINTS-LB
                                      SPOOL
           MOVE   ALL "0"          TO SUBSCRIPTS
                                      TAB-ANTI-REPETECO
                                      TAB-EM-BRANCO
                                      SPOOL-KEEP
           IF   QUIET
                SET NO-QUIET TO TRUE
           END-IF
           EXEC COBOLware PROCESS (CLOSE) END-EXEC
           ACCEPT CWIMPR-TIME-REPORT FROM TIME.

       900-99-FIM. EXIT.

       910-REMOTA.

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
                MOVE "$!"         TO CWCONF-LABEL
           ELSE
                MOVE SPACES       TO SPOOL-REMOTO
           END-IF.

       910-99-FIM. EXIT.

       POSTER-CHECK.

           MOVE SPACES TO POSTER-BUFFER
           MOVE 0      TO P
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
                              ADD 1 TO P
                              IF POSTER-C (I: 1) = ';'
                                 MOVE X'0D' TO POSTER-BUFFER(P:1)
                                 ADD  1     TO P
                                 MOVE X'0A' TO POSTER-BUFFER(P:1)
                              ELSE
                                 MOVE POSTER-C (I: 1)
                                   TO POSTER-BUFFER(P:1)
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
       END PROGRAM CWIMPR.
