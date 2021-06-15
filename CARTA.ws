       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CARTA.
       AUTHOR.        Traduzido de Easytrieve (macro esy.kex v.0.10).
       DATE-WRITTEN.  21/02/2021 21:42:18.
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
           SELECT CARTAO ASSIGN TO CARD.
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
       FD  CARTAO
           RECORDING MODE IS F
           RECORD CONTAINS 080 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *
       01  REG-CARTAO.
           03 CARTAO-EMP                     PIC S9(005).
           03 FILLER                         PIC  X(001).
           03 RAISE-PERCENT                  PIC S9(002).
           03 FILLER                         PIC  X(072).
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
           03 TRACOS                         PIC  X(120)    VALUE
              ALL '-'.
           03 PGMID                          PIC  X(007)    VALUE
              'ûCARTA.'.
           03 WS-DISPLAY                     PIC  -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
      *
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
       RT-PROCESSARX.                              EXIT.
      *----------------------------------------------------------------*
      *               OBTER AMBIENTE DE EXECUCAO DO CARTA              *
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
           DISPLAY PGMID '900I' TRACOS
      *
           CALL SB-CANCEL.
      *
       RT-CANCELARX.                               EXIT.
      *----------------------------------------------------------------*
      *             ROTINA DE FINALIZACAO DO PROGRAMA CARTA            *
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
           DISPLAY PGMID '999I' TRACOS.
      *
       RT-FINALIZARX.                              EXIT.
