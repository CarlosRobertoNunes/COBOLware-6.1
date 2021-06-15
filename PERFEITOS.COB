      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PERFEITOS.
       AUTHOR.        Traduzido de Easytrieve (macro esy.kex v.0.10).
       DATE-WRITTEN.  21/02/2021 21:20:58 21:20:59.
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
           SELECT PESSOAL ASSIGN TO PESSOAL
                  ORGANIZATION   IS SEQUENTIAL
                  FILE STATUS    IS FL-STA-PESSOAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  PESSOAL
           RECORDING MODE IS F
           RECORD CONTAINS 5 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *
       01  REG-PESSOAL.
           03 NUMPERF                        PIC S9(005).
      *
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
      *----------------------------------------------------------------*
      **** VARIAVEIS DE CONTROLE DOS RELATORIOS                        *
      *----------------------------------------------------------------*
      *
       01  WS-CONTROLE-RELATORIOS.
       02  WS-COMPACTADOS COMP-3.
           03 DIVIDENDO                   PIC S9(005)    VALUE 1.
           03 NUM                         PIC S9(005)    VALUE 0.
           03 DIVISOR                     PIC S9(005)    VALUE 1.
           03 SOMA                        PIC S9(005)    VALUE 0.
           03 QUOCIENTE                   PIC S9(005)    VALUE 0.
      *
      *----------------------------------------------------------------*
      *        AREA DE VARIAVES AUXILIARES                             *
      *----------------------------------------------------------------*
      *
       01  WS-VARIAVEIS.
           03 TRACOS                         PIC  X(116)    VALUE
              ALL '-'.
           03 PGMID                          PIC  X(011)    VALUE
              '#PERFEITOS.'.
           03 WS-DISPLAY                     PIC  -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           03 FL-STA-PESSOAL                 PIC  X(002)    VALUE ZEROS.
              88 FL-PESSOAL-EOF                             VALUE '10'.
              88 FL-PESSOAL-OK                              VALUE '00'.
           03 WS-LIDOS-PESSOAL        COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-ULT-LIDO-PESSOAL            PIC  X(005)    VALUE SPACE.
      *
      *----------------------------------------------------------------*
      *--> AREA MONTAR TABELAS                                         *
      *----------------------------------------------------------------*
      *
       01  WS-TABELA-FIXA.
      *
        03  WS-OCOR1-MENSAGEM.
          05  WS-OCOR1-LAYOUT         PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR1-SECTION-PGM    PIC  X(032) VALUE
          'ABRIR-ENTRADA-PESSOAL'.
          05  WS-OCOR1-COD-MSG        PIC  X(004) VALUE '0001'.
          05  WS-OCOR1-COMPL          PIC  X(100) VALUE
          'ERRO DE ABERTURA DO ARQUIVO PESSOAL'.
          05  WS-OCOR1-QSAM-MSG.
            07  WS-OCOR1-QSAM-STAT    PIC  X(002) VALUE SPACES.
            07  WS-OCOR1-QSAM-DDN     PIC  X(008) VALUE 'PESSOAL'.
          05  FILLER                  PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR2-MENSAGEM.
          05  WS-OCOR2-LAYOUT         PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR2-SECTION-PGM    PIC  X(032) VALUE
          'RT-FECHAR-PESSOAL'.
          05  WS-OCOR2-COD-MSG        PIC  X(004) VALUE '0002'.
          05  WS-OCOR2-COMPL          PIC  X(100) VALUE
          'ERRO AO FECHAR O ARQUIVO PESSOAL'.
          05  WS-OCOR2-QSAM-MSG.
            07  WS-OCOR2-QSAM-STAT    PIC  X(002) VALUE SPACES.
            07  WS-OCOR2-QSAM-DDN     PIC  X(008) VALUE 'PESSOAL'.
          05  FILLER                  PIC  X(182) VALUE SPACES.
      *
        03  WS-OCOR3-MENSAGEM.
          05  WS-OCOR3-LAYOUT         PIC  X(004) VALUE 'QSAM'.
          05  WS-OCOR3-SECTION-PGM    PIC  X(032) VALUE
          'RT-LER-PESSOAL'.
          05  WS-OCOR3-COD-MSG        PIC  X(004) VALUE '0003'.
          05  WS-OCOR3-COMPL          PIC  X(100) VALUE
          'ERRO DE LEITURA DO ARQUIVO PESSOAL'.
          05  WS-OCOR3-QSAM-MSG.
            07  WS-OCOR3-QSAM-STAT    PIC  X(002) VALUE SPACES.
            07  WS-OCOR3-QSAM-DDN     PIC  X(008) VALUE 'PESSOAL'.
          05  FILLER                  PIC  X(182) VALUE SPACES.
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
                   UNTIL FL-PESSOAL-EOF
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
           PERFORM RT-LER-PESSOAL
           IF  FL-PESSOAL-EOF
               EXIT SECTION
           END-IF.
      *
           PERFORM RT-ABRIR-ENTRADA-PESSOAL.
      *
       RT-INICIARX.                                EXIT.
      *----------------------------------------------------------------*
      *               ROTINA PARA PROCESSAMENTO PRINCIPAL              *
      *----------------------------------------------------------------*
       RT-PROCESSAR                                SECTION.
      *
       CALCULA.
      *
[   ]         COMPUTE QUOCIENTE = DIVIDENDO / DIVISOR
[   ]         IF QUOCIENTE * DIVISOR EQUAL DIVIDENDO
[   ]            ADD DIVISOR TO SOMA.
      *
[   ]      IF SOMA > DIVIDENDO
[   ]         GO  TO  PROXIMO-NUM
[   ]      END-IF
      *
[   ]      ADD 1 TO DIVISOR
      *
[   ]      IF DIVISOR LESS DIVIDENDO
[   ]         GO TO CALCULA
[   ]      END-IF
      *
[   ]      IF (SOMA EQUAL DIVIDENDO) AND (DIVIDENDO NOT EQUAL 1)
[   ]         ADD 1 TO NUM
[   ]         DISPLAY PGMID '003I-  PERFEITO ' NUM ': ' DIVIDENDO.
      *
       PROXIMO-NUM.
      *
[   ]         ADD 1 TO DIVIDENDO
      *
[   ]         IF NUM LESS NUMPERF
[   ]            MOVE 1 TO DIVISOR
[   ]            MOVE ZERO TO SOMA
[   ]            GO TO CALCULA.
      *
       RT-PROCESSARX.                              EXIT.
      *----------------------------------------------------------------*
      *             OBTER AMBIENTE DE EXECUCAO DO PERFEITOS            *
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
           MOVE    WS-LIDOS-PESSOAL                        TO WS-DISPLAY
           DISPLAY PGMID '003I- LIDOS DE PESSOAL...........:' WS-DISPLAY
           DISPLAY PGMID '900I' TRACOS
      *
           DISPLAY PGMID '003I- ULTIMO LIDO DE PESSOAL:'
                   WS-ULT-LIDO-PESSOAL
           CALL SB-CANCEL.
      *
       RT-CANCELARX.                               EXIT.
      *----------------------------------------------------------------*
      *           ROTINA DE FINALIZACAO DO PROGRAMA PERFEITOS          *
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
           MOVE    WS-LIDOS-PESSOAL                        TO WS-DISPLAY
           DISPLAY PGMID '003I- LIDOS DE PESSOAL...........:' WS-DISPLAY
           DISPLAY PGMID '999I' TRACOS.
      *
           PERFORM RT-FECHAR-PESSOAL.
      *
       RT-FINALIZARX.                              EXIT.
      *----------------------------------------------------------------*
      *                     ABRIR O ARQUIVO PESSOAL                    *
      *----------------------------------------------------------------*
       RT-ABRIR-ENTRADA-PESSOAL                    SECTION.
      *
          OPEN INPUT PESSOAL
          IF  NOT FL-PESSOAL-OK
              SET INDALT TO 1
              PERFORM RT-MONTA-MSG
          END-IF.
      *
       RT-ABRIR-ENTRADA-PESSOALX.                  EXIT.
      *----------------------------------------------------------------*
      *                    FECHAR O ARQUIVO PESSOAL                    *
      *----------------------------------------------------------------*
       RT-FECHAR-PESSOAL                           SECTION.
      *
          CLOSE PESSOAL
          IF  NOT FL-PESSOAL-OK
              SET INDALT TO 2
              PERFORM RT-MONTA-MSG
          END-IF.
      *
       RT-FECHAR-PESSOALX.                         EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA LEITURA SEQUENCIAL DO ARQUIVO PESSOAL       *
      *----------------------------------------------------------------*
       RT-LER-PESSOAL                              SECTION.
      *
          READ PESSOAL INTO WS-ULT-LIDO-PESSOAL
      *
          IF  FL-PESSOAL-OK
              ADD 1 TO WS-LIDOS-PESSOAL
          ELSE
              IF  NOT FL-PESSOAL-EOF
                  SET INDALT TO 3
                  PERFORM RT-MONTA-MSG
              ELSE
                  IF  FL-PESSOAL-EOF
                  AND WS-LIDOS-PESSOAL EQUAL ZERO
                      DISPLAY PGMID '003I  *=========================*'
                      DISPLAY PGMID '003I  * ARQUIVO "PESSOAL" VAZIO *'
                      DISPLAY PGMID '003I  *=========================*'
                      DISPLAY PGMID '003I  DDN = PESSOAL'
                  END-IF
              END-IF
          END-IF.
      *
       RT-LER-PESSOALX.                            EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA MONTAR AS MENSAGENS DA TABELA DE ERRO       *
      *----------------------------------------------------------------*
       RT-MONTA-MSG                                SECTION.
      *
           MOVE FL-STA-PESSOAL          TO WS-OCOR1-QSAM-STAT
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
           MOVE 'PERFEITOS'             TO SC5LDIS1-PGM.
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
