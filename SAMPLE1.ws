     <*>PARM DEBUG(FLOW FLDCHK)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SAMPLE1.
       AUTHOR.        Traduzido de Easytrieve (macro esy.kex v.0.10).
       DATE-WRITTEN.  21/02/2021 21:43:47.
       SECURITY.      *************************************************
                      *                                               *
                      *  Descreva a finalidade do seu novo programa   *
                      *                                               *
                      *************************************************
                      *            KEDIT 5.50 X16 32-BIT NOV 27 2007  *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
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
           03 NAME                           PIC  X(016).
           03 REDEFINES NAME.
              04 LAST-NAME                   PIC  X(008).
              04 FILLER                      PIC  X(008).
           03 FILLER                         PIC  X(061).
           03 PAY-GROSS               COMP-3 PIC S9(004)V9(02).
           03 DEPT                           PIC S9(003).
           03 FILLER                         PIC  X(035).
           03 DATE-OF-HIRE                   PIC S9(006).
           03 REDEFINES DATE-OF-HIRE.
              04 HIRE-MM                     PIC S9(002).
              04 HIRE-DD                     PIC S9(002).
              04 HIRE-YY                     PIC S9(002).
           03 FILLER                         PIC  X(009).
      *
       FD  WORKSDS
           RECORD CONTAINS 1024 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *
       01  REG-WORKSDS.
           03 WORKSDS-KEY.
              06 WORKSDS-REPORT             PIC  X(008).
              06 WORKSDS-STRING.
                 09 WORKSDS-SEQUENCE        PIC  9(017).
                 09 FILLER                   PIC  X(238).
           03 FILLER                         PIC  X(761).
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
           03 WRETC                      PIC  X.
           03 WRETCSUB                   PIC  X.
           03 WMACRO                     PIC  X(8).
           03 WRC                        PIC  9(9)   COMP.
           03 WRS                        PIC  9(9)   COMP.
       01  WFUNCAO                       PIC  X(5)   VALUE 'AMB'.
       01  WDADOS.
           03 WAMB                       PIC  X(3)   VALUE SPACES.
           03 WCPUID                     PIC  X(4)   VALUE SPACES.
      *
      *----------------------------------------------------------------*
      * CAMPOS DE DATA E HORA                                          *
      *----------------------------------------------------------------*
      *
       01  WDATAAAMMDD.
           03 WDATAAA                    PIC  X(04)  VALUE SPACES.
           03 WDATAMM                    PIC  X(02)  VALUE SPACES.
           03 WDATADD                    PIC  X(02)  VALUE SPACES.
      *
       01  WS-DATA-HORA.
           03  WS-DATACURRENT.
               05 WS-ANO-DATA            PIC  X(04)  VALUE SPACES.
               05 WS-MES-DATA            PIC  X(02)  VALUE SPACES.
               05 WS-DIA-DATA            PIC  X(02)  VALUE SPACES.
           03  WS-HORACURRENT.
               05 WS-HOR-HORA            PIC  X(02)  VALUE SPACES.
               05 WS-MIN-HORA            PIC  X(02)  VALUE SPACES.
               05 WS-SEG-HORA            PIC  X(02)  VALUE SPACES.
               05 WS-CEN-HORA            PIC  X(02)  VALUE SPACES.
       01  FILLER REDEFINES     WS-DATA-HORA.
           03  SYSDATE-LONG              PIC  9(008).
           03  FILLER REDEFINES SYSDATE-LONG.
               05 FILLER                 PIC  X(002).
               05 SYSDATE                PIC  9(006).
           03  SYSTIME                   PIC  9(008).
      *
       01  WS-DATA-HORADISP.
           03  WS-DATADISP.
               05  WS-DIA-DISP           PIC  X(02)  VALUE SPACES.
               05  FILLER                PIC  X(01)  VALUE '/'.
               05  WS-MES-DISP           PIC  X(02)  VALUE SPACES.
               05  FILLER                PIC  X(01)  VALUE '/'.
               05  WS-ANO-DISP           PIC  X(04)  VALUE SPACES.
               05  FILLER                PIC  X(02)  VALUE SPACES.
           03  WS-HORACURRENTP.
               05  WS-HOR-DISP           PIC  X(02)  VALUE SPACES.
               05  FILLER                PIC  X(01)  VALUE ':'.
               05  WS-MIN-DISP           PIC  X(02)  VALUE SPACES.
               05  FILLER                PIC  X(01)  VALUE ':'.
               05  WS-SEG-DISP           PIC  X(02)  VALUE SPACES.
      *
      *
      *----------------------------------------------------------------*
      **** VARIAVEIS DE CONTROLE DOS RELATORIOS                        *
      *----------------------------------------------------------------*
      *
       01  WS-CONTROLE-RELATORIOS.
       02  WS-DISPLAYS.
       02  WS-COMPACTADOS COMP-3.
           03 SALARY                      PIC S9(004)V99 VALUE ZEROS.
           03 BONUS                       PIC S9(004)V99 VALUE ZEROS.
           03 RAISE                       PIC S9(004)V99 VALUE ZEROS.
           03 SERVICE                     PIC S9(002)    VALUE ZEROS.
           03 BREAK-LEVEL                 PIC  9(003).
           03 LEVEL                       PIC  9(003).
           03 LINE-COUNT                  PIC  9(018).
              88 REPORT-CLOSED                           VALUE ZERO.
           03 DETAIL-COUNT                PIC  9(018).
              88 FIRST-DETAIL                            VALUE 0.
      *
      *----------------------------------------------------------------*
      *        AREA DE VARIAVES AUXILIARES                             *
      *----------------------------------------------------------------*
      *
       01  WS-VARIAVEIS.
           03 TRACOS                         PIC  X(118)    VALUE
              ALL '-'.
           03 PGMID                          PIC  X(009)    VALUE
              'ûSAMPLE1.'.
           03 WS-DISPLAY                     PIC  -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           03 CURR-DATE                   PIC  9(006)    VALUE ZEROS.
           03 REDEFINES CURR-DATE.
              04 CURR-MM                  PIC  9(002).
              04 CURR-DD                  PIC  9(002).
              04 CURR-YY                  PIC  9(002).
           03 FL-STA-PERSNL PIC  X(002)   VALUE ZEROS.
              88 FL-PERSNL-OK                             VALUE '00'.
              88 FL-PERSNL-EOF                            VALUE '10'.
           03 WS-ULT-LIDO-PERSNL PIC  X(150)   VALUE SPACE.
           03 WS-LIDOS-PERSNL COMP-3 PIC S9(017)   VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *--> AREA MONTAR TABELAS                                         *
      *----------------------------------------------------------------*
      *
       01  WS-TABELA-FIXA.
      *
        03  WS-OCOR1-MENSAGEM.
          05  WS-OCOR1-LAYOUT PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR1-SECTION-PGM PIC  X(032) VALUE
          'ABRIR-ENTRADA-PERSNL'.
          05  WS-OCOR1-COD-MSG PIC  X(004) VALUE '0001'.
          05  WS-OCOR1-COMPL PIC  X(100) VALUE
          'ERRO DE ABERTURA DO ARQUIVO PERSNL'.
          05  WS-OCOR1-QSAM-MSG.
            07  WS-OCOR1-QSAM-STAT PIC  X(002) VALUE SPACES.
            07  WS-OCOR1-QSAM-DDN PIC  X(008) VALUE 'SYS010'.
          05  FILLER PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR2-MENSAGEM.
          05  WS-OCOR2-LAYOUT PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR2-SECTION-PGM PIC  X(032) VALUE
          'RT-LER-PERSNL'.
          05  WS-OCOR2-COD-MSG PIC  X(004) VALUE '0002'.
          05  WS-OCOR2-COMPL PIC  X(100) VALUE
          'ERRO DE LEITURA DO ARQUIVO PERSNL'.
          05  WS-OCOR2-QSAM-MSG.
            07  WS-OCOR2-QSAM-STAT PIC  X(002) VALUE SPACES.
            07  WS-OCOR2-QSAM-DDN PIC  X(008) VALUE 'SYS010'.
          05  FILLER PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR3-MENSAGEM.
          05  WS-OCOR3-LAYOUT PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR3-SECTION-PGM PIC  X(032) VALUE
          'RT-FECHAR-PERSNL'.
          05  WS-OCOR3-COD-MSG PIC  X(004) VALUE '0003'.
          05  WS-OCOR3-COMPL PIC  X(100) VALUE
          'ERRO AO FECHAR O ARQUIVO PERSNL'.
          05  WS-OCOR3-QSAM-MSG.
            07  WS-OCOR3-QSAM-STAT PIC  X(002) VALUE SPACES.
            07  WS-OCOR3-QSAM-DDN PIC  X(008) VALUE 'SYS010'.
          05  FILLER PIC  X(182) VALUE SPACES.
      *
       01  WS-TABELA-ALTER.
        03  WS-TABELA-ALTERX   OCCURS 3 TIMES INDEXED BY INDALT.
         05 WS-ALTER-LAYOUT           PIC  X(004).
         05 WS-ALTER-SECTION-PGM      PIC  X(032).
         05 WS-ALTER-COD-MSG          PIC  X(004).
         05 WS-ALTER-COMPL            PIC  X(100).
         05 WS-ALTER-QSAM-MSG         PIC  X(190).
      *
      *----------------------------------------------------------------*
       COPY 'SC5LDIS1'.
      *================================================================*
       01  FILLER                     PIC  X(32)  VALUE
                                     'FFF  FIM DAWORKING-STORAGE  FFF'.
      *================================================================*
      *                                                                *
      *================================================================*
       LINKAGE SECTION.

       SCREEN SECTION.

       PROCEDURE DIVISION.
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
      *
           PERFORM RT-OBTER-HORA
      *
           DISPLAY PGMID '001I' TRACOS
           DISPLAY PGMID '001I- INICIO PROC: ' WS-DATA-HORADISP
           DISPLAY PGMID '001I' TRACOS
      *
       RT-INICIARX.                                EXIT.
      *----------------------------------------------------------------*
      *               ROTINA PARA PROCESSAMENTO PRINCIPAL              *
      *----------------------------------------------------------------*
       RT-PROCESSAR                                SECTION.
      *
      *
      *
      *
           PERFORM RT-ABRIR-ENTRADA-PERSNL
      *
      *
           PERFORM RT-ENTRADA-PERSNL
                   UNTIL FL-PERSNL-EOF
      *
      *
      *
           PERFORM RT-FECHAR-PERSNL
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
           DISPLAY PGMID '900I' TRACOS
      *
           DISPLAY PGMID '002I- ULTIMO LIDO DE PERSNL:'
                   WS-ULT-LIDO-PERSNL
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
           DISPLAY PGMID '999I' TRACOS.
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
          END-IF
      *
       RT-ABRIR-ENTRADA-PERSNLX.                   EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA MONTAR AS MENSAGENS DA TABELA DE ERRO       *
      *----------------------------------------------------------------*
       RT-MONTA-MSG                                SECTION.
      *
           MOVE FL-STA-PERSNL           TO WS-OCOR1-QSAM-STAT
                                           WS-OCOR2-QSAM-STAT
                                           WS-OCOR3-QSAM-STAT
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
      *----------------------------------------------------------------*
      *            ROTINA ENTRADA-PERSNL (DE JOB EASYTRIEVE)           *
      *----------------------------------------------------------------*
       RT-ENTRADA-PERSNL                           SECTION.
      *
           PERFORM RT-LER-PERSNL
           IF  FL-PERSNL-EOF
               EXIT SECTION
           END-IF
[ezt]      * Macro "GETDATE.ESY" carregada OK.
[ezt]      CURR-DATE = SYSDATE
[ezt]      * Fim da macro GETDATE.ESY
[ezt]      SALARY = PAY-GROSS * 52
[ezt]      PERFORM SERVICE-CALC
[ezt]      IF SERVICE LT 1
[ezt]         GO TO JOB
[ezt]      END-IF
[ezt]      PERFORM RAISE-CALC
[ezt]      BONUS = 0
[ezt]      IF SERVICE GT 14
[ezt]         PERFORM BONUS-CALC
[ezt]      END-IF
[ezt]      SALARY = SALARY + RAISE + BONUS
[ezt]      PRINT UPD-RPT
[ezt]      *
[ezt]      SERVICE-CALC
[ezt]      PROC
[ezt]      SERVICE = CURR-YY - HIRE-YY
[ezt]      IF CURR-MM < HIRE-MM
[ezt]         SERVICE = SERVICE - 1
[ezt]      END-IF
[ezt]      IF CURR-MM NE HIRE-MM
[ezt]         GOTO QUIT-SERV-CALC
[ezt]      END-IF
[ezt]      IF CURR-DD < HIRE-DD
[ezt]         SERVICE = SERVICE - 1
[ezt]      END-IF
[ezt]      QUIT-SERV-CALC
[ezt]      END-PROC
[ezt]      *
[ezt]      RAISE-CALC
[ezt]      PROC
[ezt]      IF DEPT LT 940
[ezt]         RAISE = SALARY * 0.1
[ezt]      ELSE
[ezt]         RAISE = SALARY * 0.15
[ezt]      END-IF
[ezt]      END-PROC
[ezt]      *
[ezt]      BONUS-CALC
[ezt]      PROC
[ezt]      IF SALARY GT 29999
[ezt]         DISPLAY ERRPRINT LAST-NAME +5 'INELIGIBLE°FOR°BONUS'
[ezt]         GOTO QUIT-BONUS
[ezt]      END-IF
[ezt]      IF SERVICE GT 19
[ezt]         BONUS = 2000
[ezt]      ELSE
[ezt]         BONUS = 1000
[ezt]      END-IF
[ezt]      PRINT BONUSRPT
[ezt]      QUIT-BONUS
[ezt]      END-PROC
[ezt]      *
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
