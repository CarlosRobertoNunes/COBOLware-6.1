      *
     <*>PARM DEBUG(FLOW FLDCHK)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SAMPLE1.
       AUTHOR.        Traduzido de Easytrieve (macro esy.kex v.0.10).
       DATE-WRITTEN.  08/03/2021 08:41:13 08:41:18.
       SECURITY.      *************************************************
                      *                                               *
                      * Gera relatorios:                              *
                      *                                               *
                      * UPD-RPT:                                      *
                      * ANNUAL UPDATE REPORT - SALARIED EMPLOYEES     *
                      *                                               *
                      * BONUSRPT:                                     *
                      * ANNUAL BONUS REPORT - SENIOR EMPLOYEES        *
                      *                                               *
                      *                                               *
                      *************************************************
                      *            KEDIT 5.50 X16 32-BIT NOV 27 2007  *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      SYSOUT        IS ERRPRINT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT PERSNL ASSIGN TO SYS010
                  ORGANIZATION  IS SEQUENTIAL
                  FILE STATUS   IS FL-STA-PERSNL.
      *
           SELECT WORKSDS ASSIGN TO WORKSDS
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS WORKSDS-KEY
                                  WITH DUPLICATES
                  FILE STATUS  IS FL-STA-WORKSDS.
      *
           SELECT UPD-RPT ASSIGN TO UPDRPT.
      *
           SELECT BONUSRPT ASSIGN TO BONUSRPT.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  PERSNL
           RECORDING MODE IS FIXED
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 12 RECORDS.
      *
       01  REG-PERSNL.
           03 FILLER                         PIC  X(016).
           03 NAME.
              04 LAST-NAME                   PIC  X(008).
              04 FILLER                      PIC  X(008).
           03 FILLER                         PIC  X(061).
           03 PAY-GROSS                      PIC S9(004)V9(02) COMP-3.
           03 DEPT                           PIC  9(003).
           03 FILLER                         PIC  X(035).
           03 DATE-OF-HIRE                   PIC  9(006).
           03 REDEFINES DATE-OF-HIRE.
              04 HIRE-MM                     PIC  9(002).
              04 HIRE-DD                     PIC  9(002).
              04 HIRE-YY                     PIC  9(002).
           03 FILLER                         PIC  X(009).
      *
       FD  UPD-RPT
           LABEL RECORD IS OMITTED.
      *
       01  REG-UPD-RPT                       PIC  X(063).
      *
       FD  BONUSRPT
           LABEL RECORD IS OMITTED.
      *
       01  REG-BONUSRPT                      PIC  X(060).
      *
       FD  WORKSDS
           RECORD CONTAINS 1024 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *
       01  REG-WORKSDS.
           03 WORKSDS-KEY.
              06 WORKSDS-REPORT              PIC  X(008).
              06 WORKSDS-STRING.
                 09 WORKSDS-SEQUENCE         PIC  9(017).
           03 FILLER                         PIC  X(999).
      *
       01  REG-WORKSDS-UPD-RPT.
           03 FILLER                         PIC  X(008).
           03 WORKSDS-UPD-RPT.
              06 UPD-RPT-DEPT                PIC 9(003).
              06 UPD-RPT-LAST-NAME           PIC  X(008).
              06 UPD-RPT-SERVICE             PIC  9(002).
              06 UPD-RPT-RAISE               PIC S9(004)V99.
              06 UPD-RPT-SALARY              PIC S9(004)V99.
              06 FILLER                      PIC  X(991).
      *
       01  REG-WORKSDS-BONUSRPT.
           03 FILLER                         PIC  X(008).
           03 WORKSDS-BONUSRPT.
              06 BONUSRPT-DEPT               PIC 9(003).
              06 BONUSRPT-LAST-NAME          PIC  X(008).
              06 BONUSRPT-SERVICE            PIC  9(002).
              06 BONUSRPT-BONUS              PIC S9(004)V99.
              06 FILLER                      PIC  X(997).
      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*
       77  FILLER                     PIC  X(31)  VALUE
                                     'III WORKING-STORAGE SECTION III'.
      *================================================================*
      *----------------------------------------------------------------*
      *        SUBPROGRAMAS                                            *
      *----------------------------------------------------------------*
      *
       77  SB-CANCEL                     PIC  X(08)  VALUE '$CANCEL'.
       77  SB-SC5CP                      PIC  X(08)  VALUE '$SC5CP'.
      *
      *----------------------------------------------------------------*
      *-- VARIAVEIS PARA CHAMADA $SC5CP - VER AMBIENTE EXECUCAO        *
      *----------------------------------------------------------------*
       01  WRETCODE.
           03 WRETC                          PIC  X.
           03 WRETCSUB                       PIC  X.
           03 WMACRO                         PIC  X(8).
           03 WRC                            PIC  9(9)         COMP.
           03 WRS                            PIC  9(9)         COMP.
       01  WFUNCAO                           PIC  X(5)   VALUE 'AMB'.
       01  WDADOS.
           03 WAMB                           PIC  X(3)   VALUE SPACES.
           03 WCPUID                         PIC  X(4)   VALUE SPACES.
      *
      *----------------------------------------------------------------*
      * CAMPOS DE DATA E HORA                                          *
      *----------------------------------------------------------------*
      *
       01  WDATAAAMMDD.
           03 WDATAAA                        PIC  X(04)  VALUE SPACES.
           03 WDATAMM                        PIC  X(02)  VALUE SPACES.
           03 WDATADD                        PIC  X(02)  VALUE SPACES.
      *
       01  WS-DATA-HORA.
           03  WS-DATACURRENT.
               05 WS-ANO-DATA                PIC  X(04)  VALUE SPACES.
               05 WS-MES-DATA                PIC  X(02)  VALUE SPACES.
               05 WS-DIA-DATA                PIC  X(02)  VALUE SPACES.
           03  WS-HORACURRENT.
               05 WS-HOR-HORA                PIC  X(02)  VALUE SPACES.
               05 WS-MIN-HORA                PIC  X(02)  VALUE SPACES.
               05 WS-SEG-HORA                PIC  X(02)  VALUE SPACES.
               05 WS-CEN-HORA                PIC  X(02)  VALUE SPACES.
       01  FILLER REDEFINES     WS-DATA-HORA.
           03  SYSDATE-LONG                  PIC  9(008).
           03  FILLER REDEFINES SYSDATE-LONG.
               05 FILLER                     PIC  X(002).
               05 SYSDATE                    PIC  9(006).
           03  SYSTIME                       PIC  9(008).
      *
       01  WS-DATA-HORADISP.
           03  WS-DATADISP.
               05  WS-DIA-DISP               PIC  X(02)  VALUE SPACES.
               05  FILLER                    PIC  X(01)  VALUE '/'.
               05  WS-MES-DISP               PIC  X(02)  VALUE SPACES.
               05  FILLER                    PIC  X(01)  VALUE '/'.
               05  WS-ANO-DISP               PIC  X(04)  VALUE SPACES.
               05  FILLER                    PIC  X(02)  VALUE SPACES.
           03  WS-HORACURRENTP.
               05  WS-HOR-DISP               PIC  X(02)  VALUE SPACES.
               05  FILLER                    PIC  X(01)  VALUE ':'.
               05  WS-MIN-DISP               PIC  X(02)  VALUE SPACES.
               05  FILLER                    PIC  X(01)  VALUE ':'.
               05  WS-SEG-DISP               PIC  X(02)  VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *        LINHAS DE IMPRESSAO DO RELATORIO UPD-RPT                *
      *----------------------------------------------------------------*
      *
       01  UPD-RPT-LAYOUT.
       02  UPD-RPT-TIT1.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(041) VALUE
              "ANNUAL UPDATE REPORT - SALARIED EMPLOYEES".
       02  UPD-RPT-TOP1.
           05 FILLER                         PIC  X(012) VALUE SPACES.
           05 FILLER                         PIC  X(041) VALUE
              "DEPT  NAME      SERV     RAISE     SALARY".
       02  UPD-RPT-DET1.
           05 FILLER                         PIC  X(012) VALUE SPACES.
           05 UPD-RPT-DET1-DEPT              PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 UPD-RPT-DET1-LAST-NAME         PIC  X(008) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 UPD-RPT-DET1-SERVICE           PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 UPD-RPT-DET1-RAISE             PIC  Z.ZZ9,99.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 UPD-RPT-DET1-SALARY            PIC  Z.ZZ9,99.
       02  UPD-RPT-SUM1.
           05 FILLER                         PIC  X(012) VALUE SPACES.
           05 UPD-RPT-SUM1-CAPTION           PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(013) VALUE SPACES.
           05 UPD-RPT-SUM1-RAISE             PIC  Z.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 UPD-RPT-SUM1-SALARY            PIC  ZZZ.ZZ9,99.
      *
      *----------------------------------------------------------------*
      *        LINHAS DE IMPRESSAO DO RELATORIO BONUSRPT               *
      *----------------------------------------------------------------*
      *
       01  BONUSRPT-LAYOUT.
       02  BONUSRPT-TIT1.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(038) VALUE
              "ANNUAL BONUS REPORT - SENIOR EMPLOYEES".
       02  BONUSRPT-TOP1.
           05 FILLER                         PIC  X(015) VALUE SPACES.
           05 FILLER                         PIC  X(031) VALUE
              "DEPT  LAST-NAME SERVIC E  BONUS".
       02  BONUSRPT-DET1.
           05 FILLER                         PIC  X(016) VALUE SPACES.
           05 BONUSRPT-DET1-DEPT             PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 BONUSRPT-DET1-LAST-NAME        PIC  X(008) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 BONUSRPT-DET1-SERVICE          PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 BONUSRPT-DET1-BONUS            PIC  Z.ZZ9,99.
      *----------------------------------------------------------------*
      **** VARIAVEIS DE CONTROLE DOS RELATORIOS                        *
      *----------------------------------------------------------------*
      *
       01  WS-CONTROLE-RELATORIOS.
       02  WS-DISPLAYS.
           03 SALARY                         PIC S9(004)V99 COMP-3 VALUE
              ZEROS.
           03 BONUS                          PIC S9(004)V99 COMP-3 VALUE
              ZEROS.
           03 RAISE                          PIC S9(004)V99 COMP-3 VALUE
              ZEROS.
           03 SERVICE                        PIC  9(002)    VALUE ZEROS.
       02  WS-COMPACTADOS COMP-3.
           03 BREAK-LEVEL                    PIC  9(003).
           03 LEVEL                          PIC  9(003).
           03 LINE-COUNT                     PIC  9(018).
              88 REPORT-CLOSED                           VALUE ZERO.
           03 DETAIL-COUNT                   PIC  9(018).
              88 FIRST-DETAIL                            VALUE 0.
           03 PAGE-COUNT                     PIC  9(018).
           03 UPD-RPT-SUM-RAISE  OCCURS 2    PIC S9(016)V9(02).
           03 UPD-RPT-SUM-SALARY OCCURS 2    PIC S9(016)V9(02).
           03 I                              PIC  9(008)   VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *        AREA DE VARIAVES AUXILIARES                             *
      *----------------------------------------------------------------*
      *
       01  WS-VARIAVEIS.
           03 TRACOS                         PIC  X(118)    VALUE
              ALL '-'.
           03 PGMID                          PIC  X(009)    VALUE
              '#SAMPLE1.'.
           03 WS-DISPLAY                     PIC  -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           03 CURR-DATE                      PIC  9(006)    VALUE ZEROS.
           03 REDEFINES CURR-DATE.
              04 CURR-MM                     PIC  9(002).
              04 CURR-DD                     PIC  9(002).
              04 CURR-YY                     PIC  9(002).
           03 FL-STA-PERSNL                  PIC  X(002)    VALUE ZEROS.
              88 FL-PERSNL-EOF                              VALUE '10'.
              88 FL-PERSNL-OK                               VALUE '00'.
           03 FL-STA-WORKSDS                 PIC  X(002)    VALUE ZEROS.
              88 FL-WORKSDS-DUPALT                          VALUE '02'.
              88 FL-WORKSDS-DUPKEY                          VALUE '22'.
              88 FL-WORKSDS-EOF                             VALUE '10'.
              88 FL-WORKSDS-NEW                             VALUE '05'.
              88 FL-WORKSDS-OK                              VALUE '00'.
           03 BUFFER                         PIC  X(063)    VALUE ZEROS.
           03 WS-GRAVADOS-WORKSDS     COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-LIDOS-PERSNL         COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-LIDOS-WORKSDS        COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-ULT-LIDO-PERSNL             PIC  X(150)    VALUE SPACE.
           03 WS-ULT-LIDO-WORKSDS            PIC  X(1024)   VALUE SPACE.
      *
      *----------------------------------------------------------------*
      *--> AREA MONTAR TABELAS                                         *
      *----------------------------------------------------------------*
      *
       01  WS-TABELA-FIXA.
      *
        03  WS-OCOR1-MENSAGEM.
          05  WS-OCOR1-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR1-SECTION-PGM           PIC  X(032) VALUE
          'ABRIR-ENTRADA-PERSNL'.
          05  WS-OCOR1-COD-MSG               PIC  X(004) VALUE '0001'.
          05  WS-OCOR1-COMPL                 PIC  X(100) VALUE
          'ERRO DE ABERTURA DO ARQUIVO PERSNL'.
          05  WS-OCOR1-QSAM-MSG.
            07  WS-OCOR1-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR1-QSAM-DDN            PIC  X(008) VALUE 'SYS010'.
          05  FILLER                         PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR2-MENSAGEM.
          05  WS-OCOR2-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR2-SECTION-PGM           PIC  X(032) VALUE
          'RT-LER-PERSNL'.
          05  WS-OCOR2-COD-MSG               PIC  X(004) VALUE '0002'.
          05  WS-OCOR2-COMPL                 PIC  X(100) VALUE
          'ERRO DE LEITURA DO ARQUIVO PERSNL'.
          05  WS-OCOR2-QSAM-MSG.
            07  WS-OCOR2-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR2-QSAM-DDN            PIC  X(008) VALUE 'SYS010'.
          05  FILLER                         PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR3-MENSAGEM.
          05  WS-OCOR3-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR3-SECTION-PGM           PIC  X(032) VALUE
          'RT-FECHAR-PERSNL'.
          05  WS-OCOR3-COD-MSG               PIC  X(004) VALUE '0003'.
          05  WS-OCOR3-COMPL                 PIC  X(100) VALUE
          'ERRO AO FECHAR O ARQUIVO PERSNL'.
          05  WS-OCOR3-QSAM-MSG.
            07  WS-OCOR3-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR3-QSAM-DDN            PIC  X(008) VALUE 'SYS010'.
          05  FILLER                         PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR4-MENSAGEM.
          05  WS-OCOR4-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR4-SECTION-PGM           PIC  X(032) VALUE
          'ABRIR-ATUALIZAR-WORKSDS'.
          05  WS-OCOR4-COD-MSG               PIC  X(004) VALUE '0004'.
          05  WS-OCOR4-COMPL                 PIC  X(100) VALUE
          'ERRO DE ABERTURA DO ARQUIVO WORKSDS'.
          05  WS-OCOR4-QSAM-MSG.
            07  WS-OCOR4-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR4-QSAM-DDN            PIC  X(008) VALUE
               'WORKSDS'.
          05  FILLER                         PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR5-MENSAGEM.
          05  WS-OCOR5-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR5-SECTION-PGM           PIC  X(032) VALUE
          'RT-LER-WORKSDS'.
          05  WS-OCOR5-COD-MSG               PIC  X(004) VALUE '0005'.
          05  WS-OCOR5-COMPL                 PIC  X(100) VALUE
          'ERRO DE LEITURA DO ARQUIVO WORKSDS'.
          05  WS-OCOR5-QSAM-MSG.
            07  WS-OCOR5-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR5-QSAM-DDN            PIC  X(008) VALUE
               'WORKSDS'.
          05  FILLER                         PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR6-MENSAGEM.
          05  WS-OCOR6-LAYOUT                PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR6-SECTION-PGM           PIC  X(032) VALUE
          'RT-GRAVAR-WORKSDS'.
          05  WS-OCOR6-COD-MSG               PIC  X(004) VALUE '0006'.
          05  WS-OCOR6-COMPL                 PIC  X(100) VALUE
          'ERRO DE GRAVACAO NO ARQUIVO WORKSDS'.
          05  WS-OCOR6-QSAM-MSG.
            07  WS-OCOR6-QSAM-STAT           PIC  X(002) VALUE SPACES.
            07  WS-OCOR6-QSAM-DDN            PIC  X(008) VALUE
               'WORKSDS'.
            07  FILLER                       PIC  X(015) VALUE
               ' WORKSDS-KEY = '.
            07  WS-OCOR6-WORKSDS-KEY         PIC  X(015) VALUE SPACES.
          05  FILLER                         PIC  X(152) VALUE SPACES.
      *
       01  WS-TABELA-ALTER.
        03  WS-TABELA-ALTERX   OCCURS 6 TIMES INDEXED BY INDALT.
         05 WS-ALTER-LAYOUT                  PIC  X(004).
         05 WS-ALTER-SECTION-PGM             PIC  X(032).
         05 WS-ALTER-COD-MSG                 PIC  X(004).
         05 WS-ALTER-COMPL                   PIC  X(100).
         05 WS-ALTER-QSAM-MSG                PIC  X(190).
      *
      *----------------------------------------------------------------*
       COPY 'SC5LDIS1'.
      *================================================================*
       01  FILLER                            PIC  X(32)  VALUE
                                     'FFF  FIM DAWORKING-STORAGE  FFF'.
      *================================================================*
      *                                                                *
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
      *                        ROTINA PRINCIPAL                        *
      *----------------------------------------------------------------*
       RT-PRINCIPAL                                SECTION.
      *
           PERFORM RT-INICIAR
      *
           PERFORM RT-PROCESSAR
      *
           PERFORM RT-FINALIZAR
      *
           MOVE ZEROS TO RETURN-CODE
      *
           GOBACK.
      *
       RT-PRINCIPALX.                              EXIT.
      *----------------------------------------------------------------*
      *                     ROTINA DE INICIALIZACAO                    *
      *----------------------------------------------------------------*
       RT-INICIAR                                  SECTION.
      *
           DISPLAY PGMID '001I ' FUNCTION WHEN-COMPILED
           PERFORM RT-OBTER-AMBIENTE
           MOVE HIGH-VALUES TO UPD-RPT-DET1-DEPT              (1:)
      *
           PERFORM RT-OBTER-HORA
      *
           DISPLAY PGMID '001I' TRACOS
           DISPLAY PGMID '001I- INICIO PROC: ' WS-DATA-HORADISP
           DISPLAY PGMID '001I' TRACOS
      *
           PERFORM RT-ABRIR-ATUALIZAR-WORKSDS.
      *
       RT-INICIARX.                                EXIT.
      *----------------------------------------------------------------*
      *               ROTINA PARA PROCESSAMENTO PRINCIPAL              *
      *----------------------------------------------------------------*
       RT-PROCESSAR                                SECTION.
      *
           PERFORM RT-ABRIR-ENTRADA-PERSNL
      *
           PERFORM RT-ENTRADA-PERSNL
                   UNTIL FL-PERSNL-EOF
      *
           PERFORM RT-FECHAR-PERSNL.
      *
       SERVICE-CALC.
      *
[   ]      COMPUTE SERVICE = CURR-YY - HIRE-YY
[   ]      IF CURR-MM LESS HIRE-MM
[   ]         SUBTRACT 1 FROM SERVICE
[   ]      END-IF
[   ]      IF CURR-MM NOT EQUAL HIRE-MM
[   ]         GO TO QUIT-SERV-CALC
[   ]      END-IF
[   ]      IF CURR-DD LESS HIRE-DD
[   ]         SUBTRACT 1 FROM SERVICE
[   ]      END-IF.
[   ]  QUIT-SERV-CALC.
      *
       RAISE-CALC.
      *
[   ]      IF DEPT LESS 940
[   ]         COMPUTE RAISE = SALARY * 0,1
[   ]      ELSE
[   ]         COMPUTE RAISE = SALARY * 0,15
[   ]      END-IF.
      *
       BONUS-CALC.
      *
[   ]      IF SALARY GREATER 29999
[   ]         DISPLAY PGMID '200I- ' LAST-NAME '     '
[   ]                 'INELIGIBLE FOR BONUS' UPON ERRPRINT
[   ]         GO TO QUIT-BONUS
[   ]      END-IF
[   ]      IF SERVICE GREATER 19
[   ]         MOVE 2000 TO BONUS
[   ]      ELSE
[   ]         MOVE 1000 TO BONUS
[   ]      END-IF
[   ]      PERFORM RT-CARREGA-BONUSRPT.
[   ]  QUIT-BONUS.
      *
           MOVE HIGH-VALUES         TO WORKSDS-KEY
           MOVE 'UPD-RPT'           TO WORKSDS-REPORT
           PERFORM RT-GRAVAR-WORKSDS
           MOVE HIGH-VALUES         TO WORKSDS-KEY
           MOVE 'BONUSRPT'          TO WORKSDS-REPORT
           PERFORM RT-GRAVAR-WORKSDS
      *
           PERFORM RT-GERA-RELATORIOS.
      *
       RT-PROCESSARX.                              EXIT.
      *----------------------------------------------------------------*
      *              OBTER AMBIENTE DE EXECUCAO DO SAMPLE1             *
      *----------------------------------------------------------------*
       RT-OBTER-AMBIENTE                           SECTION.
      *
           CALL SB-SC5CP USING WRETCODE WFUNCAO WDADOS
      **      WABM   DES/HOM/PRD
           IF WRETC  NOT = '0'
              MOVE 'DES'              TO WAMB
           END-IF.
      *
       RT-OBTER-AMBIENTEX.                         EXIT.
      *----------------------------------------------------------------*
      *         ROTINA PARA OBTER DATA E HORA DO PROCESSAMENTO         *
      *----------------------------------------------------------------*
       RT-OBTER-HORA                               SECTION.
      *
           MOVE FUNCTION CURRENT-DATE(1:20)
                                       TO WS-DATA-HORA
           MOVE WS-ANO-DATA            TO WS-ANO-DISP
           MOVE WS-MES-DATA            TO WS-MES-DISP
           MOVE WS-DIA-DATA            TO WS-DIA-DISP
           MOVE WS-HOR-HORA            TO WS-HOR-DISP
           MOVE WS-MIN-HORA            TO WS-MIN-DISP
           MOVE WS-SEG-HORA            TO WS-SEG-DISP
      *
           MOVE WS-ANO-DATA            TO WDATAAA
           MOVE WS-MES-DATA            TO WDATAMM
           MOVE WS-DIA-DATA            TO WDATADD.
      *
       RT-OBTER-HORAX.                             EXIT.
      *----------------------------------------------------------------*
      *                     ROTINA DE CANCELAMENTO                     *
      *----------------------------------------------------------------*
       RT-CANCELAR                                 SECTION.
      *
           DISPLAY PGMID '900I' TRACOS
           DISPLAY PGMID '900I- ESTATISTICA AUXILIAR PARA CANCEL'
           DISPLAY PGMID '900I' TRACOS
      *
           MOVE    WS-LIDOS-PERSNL                         TO WS-DISPLAY
           DISPLAY PGMID '002I- LIDOS DE PERSNL............:' WS-DISPLAY
           MOVE    WS-LIDOS-WORKSDS                        TO WS-DISPLAY
           DISPLAY PGMID '005I- LIDOS DE WORKSDS...........:' WS-DISPLAY
           MOVE    WS-GRAVADOS-WORKSDS                     TO WS-DISPLAY
           DISPLAY PGMID '006I- GRAVADOS EM WORKSDS........:' WS-DISPLAY
           DISPLAY PGMID '900I' TRACOS
      *
           DISPLAY PGMID '002I- ULTIMO LIDO DE PERSNL:'
                   WS-ULT-LIDO-PERSNL
           DISPLAY PGMID '005I- ULTIMO LIDO DE WORKSDS:'
                   WS-ULT-LIDO-WORKSDS
           CALL SB-CANCEL.
      *
       RT-CANCELARX.                               EXIT.
      *----------------------------------------------------------------*
      *            ROTINA DE FINALIZACAO DO PROGRAMA SAMPLE1           *
      *----------------------------------------------------------------*
       RT-FINALIZAR                                SECTION.
      *
           PERFORM RT-OBTER-HORA
      *
           DISPLAY PGMID '999I' TRACOS
           DISPLAY PGMID '999I- FIM DE PROC: ' WS-DATA-HORADISP
           DISPLAY PGMID '999I' TRACOS
           DISPLAY PGMID '999I- ESTATISTICA DE PROCESSAMENTO'
           DISPLAY PGMID '999I' TRACOS
           MOVE    WS-LIDOS-PERSNL                         TO WS-DISPLAY
           DISPLAY PGMID '002I- LIDOS DE PERSNL............:' WS-DISPLAY
           MOVE    WS-LIDOS-WORKSDS                        TO WS-DISPLAY
           DISPLAY PGMID '005I- LIDOS DE WORKSDS...........:' WS-DISPLAY
           MOVE    WS-GRAVADOS-WORKSDS                     TO WS-DISPLAY
           DISPLAY PGMID '006I- GRAVADOS EM WORKSDS........:' WS-DISPLAY
           DISPLAY PGMID '999I' TRACOS.
      *
           CLOSE WORKSDS.
      *
       RT-FINALIZARX.                              EXIT.
      *
      *----------------------------------------------------------------*
      *                     ABRIR O ARQUIVO PERSNL                     *
      *----------------------------------------------------------------*
       RT-ABRIR-ENTRADA-PERSNL                     SECTION.
      *
           OPEN INPUT PERSNL
           IF  NOT FL-PERSNL-OK
               SET INDALT TO 1
               PERFORM RT-MONTA-MSG
           END-IF.
      *
       RT-ABRIR-ENTRADA-PERSNLX.                   EXIT.
      *----------------------------------------------------------------*
      *            ROTINA ENTRADA-PERSNL (DE JOB EASYTRIEVE)           *
      *----------------------------------------------------------------*
       RT-ENTRADA-PERSNL                           SECTION.
      *
           PERFORM RT-LER-PERSNL
           IF  FL-PERSNL-EOF
               EXIT SECTION
           END-IF
[   ] *    * Macro "GETDATE.ESY" carregada OK.
[   ]      PERFORM RT-OBTER-HORA
[   ]      MOVE SYSDATE TO CURR-DATE
[   ] *    * Fim da macro GETDATE.ESY
[   ]      COMPUTE SALARY = PAY-GROSS * 52
[   ]      PERFORM SERVICE-CALC
[   ]      IF SERVICE LESS 1
[   ]         EXIT SECTION
[   ]      END-IF
[   ]      PERFORM RAISE-CALC
[   ]      MOVE ZERO TO BONUS
[   ]      IF SERVICE GREATER 14
[   ]         PERFORM BONUS-CALC
[   ]      END-IF
[   ]      COMPUTE SALARY = SALARY + RAISE + BONUS
[   ]      PERFORM RT-CARREGA-UPD-RPT.
[   ] *    *
      *
       RT-ENTRADA-PERSNLX.                         EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA LEITURA SEQUENCIAL DO ARQUIVO PERSNL        *
      *----------------------------------------------------------------*
       RT-LER-PERSNL                               SECTION.
      *
          READ PERSNL INTO WS-ULT-LIDO-PERSNL
      *
          IF  FL-PERSNL-OK
              ADD 1 TO WS-LIDOS-PERSNL
          ELSE
              IF  NOT FL-PERSNL-EOF
                  SET INDALT TO 2
                  PERFORM RT-MONTA-MSG
              ELSE
                  IF  FL-PERSNL-EOF
                  AND WS-LIDOS-PERSNL EQUAL ZERO
                      DISPLAY PGMID '002I  *========================*'
                      DISPLAY PGMID '002I  * ARQUIVO "PERSNL" VAZIO *'
                      DISPLAY PGMID '002I  *========================*'
                      DISPLAY PGMID '002I  DDN = SYS010'
                  END-IF
              END-IF
          END-IF.
      *
       RT-LER-PERSNLX.                             EXIT.
      *----------------------------------------------------------------*
      *                     FECHAR O ARQUIVO PERSNL                    *
      *----------------------------------------------------------------*
       RT-FECHAR-PERSNL                            SECTION.
      *
          CLOSE PERSNL
          IF  NOT FL-PERSNL-OK
              SET INDALT TO 3
              PERFORM RT-MONTA-MSG
          END-IF.
      *
       RT-FECHAR-PERSNLX.                          EXIT.
      *----------------------------------------------------------------*
      *                     ABRIR O ARQUIVO WORKSDS                    *
      *----------------------------------------------------------------*
       RT-ABRIR-ATUALIZAR-WORKSDS                  SECTION.
      *
           OPEN I-O WORKSDS
           IF  FL-WORKSDS-OK
               CLOSE WORKSDS
               DELETE FILE WORKSDS
               OPEN I-O WORKSDS
           END-IF
      *
           IF  NOT FL-WORKSDS-OK
           AND NOT FL-WORKSDS-NEW
               SET INDALT TO 4
               PERFORM RT-MONTA-MSG
           END-IF.
      *
       RT-ABRIR-ATUALIZAR-WORKSDSX.                EXIT.
      *----------------------------------------------------------------*
      *               ROTINA PARA GERACAO DE RELATORIO(S)              *
      *----------------------------------------------------------------*
       RT-GERA-RELATORIOS                          SECTION.
      *
           INITIALIZE WS-CONTROLE-RELATORIOS
           INITIALIZE WORKSDS-KEY
           START WORKSDS KEY NOT LESS WORKSDS-KEY
           PERFORM UNTIL FL-WORKSDS-EOF
                PERFORM RT-LER-WORKSDS
                IF FL-WORKSDS-OK
                   EVALUATE WORKSDS-REPORT
                      WHEN "UPD-RPT"
                            PERFORM RT-GERAR-RELATORIO-UPD-RPT
                      WHEN "BONUSRPT"
                            PERFORM RT-GERAR-RELATORIO-BONUSRPT
                   END-EVALUATE
                END-IF
           END-PERFORM.
      *
       RT-GERA-RELATORIOSX.                        EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA LEITURA SEQUENCIAL DO ARQUIVO WORKSDS       *
      *----------------------------------------------------------------*
       RT-LER-WORKSDS                              SECTION.
      *
          READ WORKSDS NEXT RECORD
      *
          IF  FL-WORKSDS-OK
          OR  FL-WORKSDS-DUPALT
              ADD 1 TO WS-LIDOS-WORKSDS
          ELSE
              IF  NOT FL-WORKSDS-EOF
                  SET INDALT TO 5
                  PERFORM RT-MONTA-MSG
              ELSE
                  IF  FL-WORKSDS-EOF
                  AND WS-LIDOS-WORKSDS EQUAL ZERO
                      DISPLAY PGMID '005I  *=========================*'
                      DISPLAY PGMID '005I  * ARQUIVO "WORKSDS" VAZIO *'
                      DISPLAY PGMID '005I  *=========================*'
                      DISPLAY PGMID '005I  DDN = WORKSDS'
                  END-IF
              END-IF
          END-IF.
      *
       RT-LER-WORKSDSX.                            EXIT.
      *----------------------------------------------------------------*
      *             ROTINA PARA GRAVACAO NO ARQUIVO WORKSDS            *
      *----------------------------------------------------------------*
       RT-GRAVAR-WORKSDS                           SECTION.
      *
          WRITE REG-WORKSDS
      *
          IF  FL-WORKSDS-OK
          OR  FL-WORKSDS-DUPALT
              ADD 1 TO WS-GRAVADOS-WORKSDS
          ELSE
              IF   NOT FL-WORKSDS-DUPKEY
                   SET INDALT TO 6
                   PERFORM RT-MONTA-MSG
              END-IF
          END-IF.
      *
       RT-GRAVAR-WORKSDSX.                         EXIT.
      *----------------------------------------------------------------*
      *                  GERACAO DO RELATORIO UPD-RPT                  *
      *----------------------------------------------------------------*
       RT-GERAR-RELATORIO-UPD-RPT                  SECTION.
      *
          EVALUATE TRUE
              WHEN WORKSDS-STRING EQUAL   HIGH-VALUES
                   MOVE 2 TO BREAK-LEVEL
              WHEN UPD-RPT-DEPT GREATER UPD-RPT-DET1-DEPT
                   MOVE 1 TO BREAK-LEVEL
              WHEN OTHER
                   MOVE ZERO TO BREAK-LEVEL
          END-EVALUATE
      *
          IF BREAK-LEVEL NOT EQUAL ZERO
             PERFORM VARYING LEVEL
                     FROM    1
                     BY      1
                     UNTIL LEVEL GREATER BREAK-LEVEL
                  EVALUATE LEVEL
                      WHEN 1
                           MOVE UPD-RPT-DET1-DEPT
                             TO UPD-RPT-SUM1-CAPTION
                      WHEN 2
                           MOVE 'FINAL' TO UPD-RPT-SUM1-CAPTION
                  END-EVALUATE
                  MOVE    UPD-RPT-SUM-RAISE(LEVEL)
                    TO    UPD-RPT-SUM1-RAISE
                  MOVE    UPD-RPT-SUM-SALARY(LEVEL)
                    TO    UPD-RPT-SUM1-SALARY
                  MOVE  SPACES       TO REG-UPD-RPT
                  PERFORM RT-IMPRIMIR-LINHA-UPD-RPT
                  MOVE  UPD-RPT-SUM1 TO REG-UPD-RPT
                  PERFORM RT-IMPRIMIR-LINHA-UPD-RPT
                  INITIALIZE UPD-RPT-SUM-RAISE(LEVEL)
                             UPD-RPT-SUM-SALARY(LEVEL)
             END-PERFORM
          END-IF
      *
          IF   WORKSDS-STRING EQUAL HIGH-VALUES
               CLOSE UPD-RPT
               INITIALIZE WS-CONTROLE-RELATORIOS
               EXIT SECTION
          END-IF
          PERFORM VARYING LEVEL
                  FROM 2 BY -1
                  UNTIL LEVEL EQUAL ZERO
              ADD UPD-RPT-RAISE             TO UPD-RPT-SUM-RAISE(LEVEL)
              ADD UPD-RPT-SALARY            TO UPD-RPT-SUM-SALARY(LEVEL)
          END-PERFORM
      *
          MOVE    UPD-RPT-DEPT      TO UPD-RPT-DET1-DEPT
          MOVE    UPD-RPT-LAST-NAME TO UPD-RPT-DET1-LAST-NAME
          MOVE    UPD-RPT-SERVICE   TO UPD-RPT-DET1-SERVICE
          MOVE    UPD-RPT-RAISE     TO UPD-RPT-DET1-RAISE
          MOVE    UPD-RPT-SALARY    TO UPD-RPT-DET1-SALARY
          MOVE    UPD-RPT-DET1      TO BUFFER
          PERFORM RT-IMPRIMIR-LINHA-UPD-RPT
      *
          ADD     1                         TO DETAIL-COUNT.
      *
       RT-GERAR-RELATORIO-UPD-RPTX.                EXIT.
      *----------------------------------------------------------------*
      *       ROTINA DE IMPRESSAO DE LINHA DO UPD-RPT E CABECALHO      *
      *----------------------------------------------------------------*
       RT-IMPRIMIR-LINHA-UPD-RPT                   SECTION.
      *
          IF REPORT-CLOSED
             OPEN OUTPUT UPD-RPT
             MOVE 52               TO LINE-COUNT
          END-IF
      *
          IF  LINE-COUNT NOT LESS 51
              PERFORM RT-IMPRIMIR-RODAPE-UPD-RPT
              ADD   1              TO PAGE-COUNT
              WRITE REG-UPD-RPT  FROM UPD-RPT-TIT1 AFTER PAGE
              WRITE REG-UPD-RPT  FROM UPD-RPT-TOP1 AFTER 2
              MOVE  SPACES         TO REG-UPD-RPT
              WRITE REG-UPD-RPT                    AFTER 1
              MOVE  4              TO LINE-COUNT
              SET   FIRST-DETAIL   TO TRUE
          END-IF.
      *
          ADD   1                  TO LINE-COUNT
          WRITE REG-UPD-RPT FROM BUFFER            AFTER 1.
      *.
       RT-IMPRIMIR-LINHA-UPD-RPTX.                 EXIT.
      *----------------------------------------------------------------*
      *            ROTINA DE IMPRESSAO DO RODAPE DO UPD-RPT            *
      *----------------------------------------------------------------*
       RT-IMPRIMIR-RODAPE-UPD-RPT                  SECTION.
      *
          IF  LINE-COUNT EQUAL 52
              EXIT SECTION
          END-IF.
      *
       RT-IMPRIMIR-RODAPE-UPD-RPTX.                EXIT.
      *----------------------------------------------------------------*
      *                  GERACAO DO RELATORIO BONUSRPT                 *
      *----------------------------------------------------------------*
       RT-GERAR-RELATORIO-BONUSRPT                 SECTION.
      *
          IF   WORKSDS-STRING EQUAL HIGH-VALUES
               CLOSE BONUSRPT
               INITIALIZE WS-CONTROLE-RELATORIOS
               EXIT SECTION
          END-IF
      *
          MOVE    BONUSRPT-DEPT      TO BONUSRPT-DET1-DEPT
          MOVE    BONUSRPT-LAST-NAME TO BONUSRPT-DET1-LAST-NAME
          MOVE    BONUSRPT-SERVICE   TO BONUSRPT-DET1-SERVICE
          MOVE    BONUSRPT-BONUS     TO BONUSRPT-DET1-BONUS
          MOVE    BONUSRPT-DET1      TO BUFFER
          PERFORM RT-IMPRIMIR-LINHA-BONUSRPT
      *
          ADD     1                           TO DETAIL-COUNT.
      *
       RT-GERAR-RELATORIO-BONUSRPTX.               EXIT.
      *----------------------------------------------------------------*
      *      ROTINA DE IMPRESSAO DE LINHA DO BONUSRPT E CABECALHO      *
      *----------------------------------------------------------------*
       RT-IMPRIMIR-LINHA-BONUSRPT                  SECTION.
      *
          IF REPORT-CLOSED
             OPEN OUTPUT BONUSRPT
             MOVE 59               TO LINE-COUNT
          END-IF
      *
          IF  LINE-COUNT NOT LESS 58
              PERFORM RT-IMPRIMIR-RODAPE-BONUSRPT
              ADD   1              TO PAGE-COUNT
              WRITE REG-BONUSRPT FROM BONUSRPT-TIT1 AFTER PAGE
              WRITE REG-BONUSRPT FROM BONUSRPT-TOP1 AFTER 2
              MOVE  SPACES         TO REG-BONUSRPT
              WRITE REG-BONUSRPT                    AFTER 1
              MOVE  4              TO LINE-COUNT
              SET   FIRST-DETAIL   TO TRUE
          END-IF.
      *
          ADD   1                  TO LINE-COUNT
          WRITE REG-BONUSRPT FROM BUFFER            AFTER 1.
      *.
       RT-IMPRIMIR-LINHA-BONUSRPTX.                EXIT.
      *----------------------------------------------------------------*
      *            ROTINA DE IMPRESSAO DO RODAPE DO BONUSRPT           *
      *----------------------------------------------------------------*
       RT-IMPRIMIR-RODAPE-BONUSRPT                 SECTION.
      *
          IF  LINE-COUNT EQUAL 59
              EXIT SECTION
          END-IF.
      *
       RT-IMPRIMIR-RODAPE-BONUSRPTX.               EXIT.
      *----------------------------------------------------------------*
      *     GRAVAR DADOS DA LINHA DO RELATORIO BONUSRPT NO WORKSDS     *
      *----------------------------------------------------------------*
       RT-CARREGA-BONUSRPT                         SECTION.
      *
          ADD  1                    TO WS-GRAVADOS-WORKSDS
          MOVE 'BONUSRPT'           TO REG-WORKSDS
          MOVE WS-GRAVADOS-WORKSDS TO WORKSDS-SEQUENCE
          MOVE DEPT                 TO BONUSRPT-DEPT
          MOVE LAST-NAME            TO BONUSRPT-LAST-NAME
          MOVE SERVICE              TO BONUSRPT-SERVICE
          MOVE BONUS                TO BONUSRPT-BONUS
          PERFORM RT-GRAVAR-WORKSDS.
      *
       RT-CARREGA-BONUSRPTX.                       EXIT.
      *----------------------------------------------------------------*
      *      GRAVAR DADOS DA LINHA DO RELATORIO UPD-RPT NO WORKSDS     *
      *----------------------------------------------------------------*
       RT-CARREGA-UPD-RPT                          SECTION.
      *
          ADD  1                    TO WS-GRAVADOS-WORKSDS
          MOVE 'UPD-RPT'            TO REG-WORKSDS
          MOVE WS-GRAVADOS-WORKSDS TO WORKSDS-SEQUENCE
          MOVE DEPT                 TO UPD-RPT-DEPT
          MOVE LAST-NAME            TO UPD-RPT-LAST-NAME
          MOVE SERVICE              TO UPD-RPT-SERVICE
          MOVE RAISE                TO UPD-RPT-RAISE
          MOVE SALARY               TO UPD-RPT-SALARY
          PERFORM RT-GRAVAR-WORKSDS.
      *
       RT-CARREGA-UPD-RPTX.                        EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA MONTAR AS MENSAGENS DA TABELA DE ERRO       *
      *----------------------------------------------------------------*
       RT-MONTA-MSG                                SECTION.
      *
           MOVE WORKSDS-KEY             TO WS-OCOR6-WORKSDS-KEY
      *
           MOVE FL-STA-PERSNL           TO WS-OCOR1-QSAM-STAT
                                           WS-OCOR2-QSAM-STAT
                                           WS-OCOR3-QSAM-STAT
      *
           MOVE FL-STA-WORKSDS          TO WS-OCOR4-QSAM-STAT
                                           WS-OCOR5-QSAM-STAT
                                           WS-OCOR6-QSAM-STAT
      *
           MOVE WS-TABELA-FIXA          TO WS-TABELA-ALTER
           MOVE WS-ALTER-LAYOUT(INDALT)
                                        TO SC5LDIS1-LAYOUT
           MOVE WS-ALTER-SECTION-PGM(INDALT)
                                        TO SC5LDIS1-SECTION-PGM
           MOVE WS-ALTER-COD-MSG(INDALT)
                                        TO SC5LDIS1-COD-MSG
           MOVE WS-ALTER-QSAM-MSG(INDALT)
                                        TO SC5LDIS1-QSAM-MSG
                                           SC5LDIS1-AREAMSG
      *
           MOVE 'SAMPLE1'               TO SC5LDIS1-PGM.
      *
           PERFORM RT-8888-FORMATAR-MSG-PAD.
      *
           PERFORM RT-CANCELAR.
      *
       RT-MONTA-MSGX.                              EXIT.
       COPY 'SC5LDIS2'.
      ******************************************************************
      *                      FIM  DO  PROGRAMA                         *
      ******************************************************************
