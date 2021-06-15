       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CARTA.
       AUTHOR.        Traduzido de Easytrieve (macro esy.kex v.0.10).
       DATE-WRITTEN.  13/06/2021 00:16:42 00:16:44.
       SECURITY.      *************************************************
                      *************************************************
                      *                                          HOST *
                      *    Microsoft Windows [versÆo 10.0.19043.1052] *
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
           SELECT LISTA ASSIGN TO LISTA.
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
           03 CARTAO-EMP                     PIC  9(005).
           03 FILLER                         PIC  X(001).
           03 RAISE-PERCENT                  PIC  9(002).
           03 FILLER                         PIC  X(072).
      *
       FD  LISTA
           LABEL RECORD IS OMITTED.
      *
       01  REG-LISTA                         PIC  X(030).
      *
       FD  WORKSDS
           RECORD CONTAINS 1024 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *
       01  REG-WORKSDS.
           03 WORKSDS-KEY.
              06 WORKSDS-STRING.
                 09 WORKSDS-SEQUENCE         PIC  9(017).
                 09 FILLER                   PIC  X(238).
           03 FILLER                         PIC  X(769).
      *
       01  REG-WORKSDS-LISTA.
           03 WORKSDS-LISTA.
              06 LISTA-CARTAO-EMP            PIC 9(005).
              06 LISTA-RAISE-PERCENT         PIC 9(002).
              06 FILLER                      PIC  X(1000).
      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*
       77  FILLER                     PIC  X(31)  VALUE
                                     'III WORKING-STORAGE SECTION III'.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *        SUBPROGRAMAS                                            *
      *----------------------------------------------------------------*
      *
       77  SB-CANCEL                     PIC  X(08)  VALUE '$CANCEL'.
       77  SB-SC5CP                      PIC  X(08)  VALUE '$SC5CP'.
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
      *        LINHAS DE IMPRESSAO DO RELATORIO LISTA                  *
      *----------------------------------------------------------------*
      *
       01  LISTA-LAYOUT.
       02  LISTA-TOP1.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(013) VALUE
              "EMP# RAISE-PE".
           05 FILLER                         PIC  X(002) VALUE "RC".
           05 FILLER                         PIC  X(003) VALUE "ENT".
       02  LISTA-DET1.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 LISTA-DET1-CARTAO-EMP          PIC  9(005) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 LISTA-DET1-RAISE-PERCENT       PIC  9(002) VALUE ZEROS.
      *----------------------------------------------------------------*
      **** VARIAVEIS DE CONTROLE DOS RELATORIOS                        *
      *----------------------------------------------------------------*
      *
       01  WS-CONTROLE-RELATORIOS.
       02  WS-COMPACTADOS COMP-3.
           03 BREAK-LEVEL                    PIC  9(003).
           03 LEVEL                          PIC  9(003).
           03 LINE-COUNT                     PIC  9(018).
              88 REPORT-CLOSED                           VALUE ZERO.
           03 DETAIL-COUNT                   PIC  9(018).
              88 FIRST-DETAIL                            VALUE 0.
           03 PAGE-COUNT                     PIC  9(018).
      *
      *----------------------------------------------------------------*
      *        AREA DE VARIAVES AUXILIARES                             *
      *----------------------------------------------------------------*
      *
       01  WS-VARIAVEIS.
           03 TRACOS                         PIC  X(109)    VALUE
              ALL '-'.
           03 PGMID                          PIC  X(007)    VALUE
              '#CARTA.'.
           03 WS-DISPLAY                     PIC  -ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           03 FL-STA-CARTAO                  PIC  X(002)    VALUE ZEROS.
              88 FL-CARTAO-EOF                              VALUE '10'.
              88 FL-CARTAO-OK                               VALUE '00'.
           03 FL-STA-WORKSDS                 PIC  X(002)    VALUE ZEROS.
              88 FL-WORKSDS-DUPALT                          VALUE '02'.
              88 FL-WORKSDS-DUPKEY                          VALUE '22'.
              88 FL-WORKSDS-EOF                             VALUE '10'.
              88 FL-WORKSDS-NEW                             VALUE '05'.
              88 FL-WORKSDS-OK                              VALUE '00'.
           03 BUFFER                         PIC  X(030)    VALUE ZEROS.
           03 WS-GRAVADOS-WORKSDS     COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-LIDOS-CARTAO         COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-LIDOS-WORKSDS        COMP-3 PIC S9(017)    VALUE ZEROS.
           03 WS-ULT-LIDO-CARTAO             PIC  X(080)    VALUE SPACE.
           03 WS-ULT-LIDO-WORKSDS            PIC  X(1024)   VALUE SPACE.
      *
      *----------------------------------------------------------------*
      *--> AREA MONTAR TABELAS                                         *
      *----------------------------------------------------------------*
      *
       01  WS-TABELA-FIXA.
      *
        03 WS-OCOR1-MENSAGEM.
           05 WS-OCOR1-LAYOUT                PIC  X(004) VALUE 'QSAM'.
           05 WS-OCOR1-SECTION-PGM           PIC  X(032) VALUE
           'ABRIR-ATUALIZAR-WORKSDS'.
           05 WS-OCOR1-COD-MSG               PIC  X(004) VALUE '0001'.
           05 WS-OCOR1-COMPL                 PIC  X(100) VALUE
           'ERRO DE ABERTURA DO ARQUIVO WORKSDS'.
           05 WS-OCOR1-QSAM-MSG.
              07 WS-OCOR1-QSAM-STAT          PIC  X(002) VALUE SPACES.
              07 WS-OCOR1-QSAM-DDN           PIC  X(008) VALUE
                 'WORKSDS'.
           05 FILLER                         PIC  X(180) VALUE SPACES.
      *
        03 WS-OCOR2-MENSAGEM.
           05 WS-OCOR2-LAYOUT                PIC  X(004) VALUE 'QSAM'.
           05 WS-OCOR2-SECTION-PGM           PIC  X(032) VALUE
           'RT-LER-WORKSDS'.
           05 WS-OCOR2-COD-MSG               PIC  X(004) VALUE '0002'.
           05 WS-OCOR2-COMPL                 PIC  X(100) VALUE
           'ERRO DE LEITURA DO ARQUIVO WORKSDS'.
           05 WS-OCOR2-QSAM-MSG.
              07 WS-OCOR2-QSAM-STAT          PIC  X(002) VALUE SPACES.
              07 WS-OCOR2-QSAM-DDN           PIC  X(008) VALUE
                 'WORKSDS'.
           05 FILLER                         PIC  X(180) VALUE SPACES.
      *
        03 WS-OCOR3-MENSAGEM.
           05 WS-OCOR3-LAYOUT                PIC  X(004) VALUE 'QSAM'.
           05 WS-OCOR3-SECTION-PGM           PIC  X(032) VALUE
           'RT-GRAVAR-WORKSDS'.
           05 WS-OCOR3-COD-MSG               PIC  X(004) VALUE '0003'.
           05 WS-OCOR3-COMPL                 PIC  X(100) VALUE
           'ERRO DE GRAVACAO NO ARQUIVO WORKSDS'.
           05 WS-OCOR3-QSAM-MSG.
              07 WS-OCOR3-QSAM-STAT          PIC  X(002) VALUE SPACES.
              07 WS-OCOR3-QSAM-DDN           PIC  X(008) VALUE
                 'WORKSDS'.
            07 FILLER                        PIC  X(015) VALUE
               ' WORKSDS-KEY = '.
            07  WS-OCOR3-WORKSDS-KEY         PIC  X(015) VALUE SPACES.
           05 FILLER                         PIC  X(150) VALUE SPACES.
      *
       01  WS-TABELA-ALTER.
        03 WS-TABELA-ALTERX   OCCURS 3 TIMES INDEXED BY INDALT.
           05 WS-ALTER-LAYOUT                PIC  X(004).
           05 WS-ALTER-SECTION-PGM           PIC  X(032).
           05 WS-ALTER-COD-MSG               PIC  X(004).
           05 WS-ALTER-COMPL                 PIC  X(100).
           05 WS-ALTER-QSAM-MSG              PIC  X(190).
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
           OPEN INPUT CARTAO
      *
           PERFORM UNTIL   FL-CARTAO-EOF
                   PERFORM RT-LER-CARTAO
                   IF   FL-CARTAO-OK
                        PERFORM RT-CARREGA-LISTA
                   END-IF
           END-PERFORM
      *
           MOVE HIGH-VALUES         TO WORKSDS-KEY
           PERFORM RT-GRAVAR-WORKSDS
      *
           PERFORM RT-GERA-RELATORIOS.
      *
       RT-PROCESSARX.                              EXIT.
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
           MOVE    WS-LIDOS-WORKSDS                        TO WS-DISPLAY
           DISPLAY PGMID '002I- LIDOS DE WORKSDS...........:' WS-DISPLAY
           MOVE    WS-LIDOS-CARTAO                         TO WS-DISPLAY
           DISPLAY PGMID '000I- LIDOS DE CARTAO............:' WS-DISPLAY
           MOVE    WS-GRAVADOS-WORKSDS                     TO WS-DISPLAY
           DISPLAY PGMID '003I- GRAVADOS EM WORKSDS........:' WS-DISPLAY
           DISPLAY PGMID '900I' TRACOS
      *
           DISPLAY PGMID '002I- ULTIMO LIDO DE WORKSDS:'
                   WS-ULT-LIDO-WORKSDS
           DISPLAY PGMID '000I- ULTIMO LIDO DE CARTAO:'
                   WS-ULT-LIDO-CARTAO
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
           MOVE    WS-LIDOS-WORKSDS                        TO WS-DISPLAY
           DISPLAY PGMID '002I- LIDOS DE WORKSDS...........:' WS-DISPLAY
           MOVE    WS-LIDOS-CARTAO                         TO WS-DISPLAY
           DISPLAY PGMID '000I- LIDOS DE CARTAO............:' WS-DISPLAY
           MOVE    WS-GRAVADOS-WORKSDS                     TO WS-DISPLAY
           DISPLAY PGMID '003I- GRAVADOS EM WORKSDS........:' WS-DISPLAY
           DISPLAY PGMID '999I' TRACOS.
      *
           CLOSE WORKSDS.
      *
       RT-FINALIZARX.                              EXIT.
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
               SET INDALT TO 1
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
                   PERFORM RT-GERAR-RELATORIO-LISTA
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
                   SET INDALT TO 2
                   PERFORM RT-MONTA-MSG
               ELSE
                   IF  FL-WORKSDS-EOF
                   AND WS-LIDOS-WORKSDS EQUAL ZERO
                       DISPLAY PGMID '002I *=========================*'
                       DISPLAY PGMID '002I * ARQUIVO "WORKSDS" VAZIO *'
                       DISPLAY PGMID '002I *=========================*'
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
                    SET INDALT TO 3
                    PERFORM RT-MONTA-MSG
               END-IF
           END-IF.
      *
       RT-GRAVAR-WORKSDSX.                         EXIT.
      *----------------------------------------------------------------*
      *                   GERACAO DO RELATORIO LISTA                   *
      *----------------------------------------------------------------*
       RT-GERAR-RELATORIO-LISTA                    SECTION.
      *
           IF   WORKSDS-STRING EQUAL HIGH-VALUES
                CLOSE LISTA
                INITIALIZE WS-CONTROLE-RELATORIOS
                EXIT SECTION
           END-IF
      *
           MOVE    LISTA-CARTAO-EMP    TO LISTA-DET1-CARTAO-EMP
           MOVE    LISTA-RAISE-PERCENT TO LISTA-DET1-RAISE-PERCENT
           MOVE    LISTA-DET1          TO BUFFER
           PERFORM RT-IMPRIMIR-LINHA-LISTA
      *
           ADD     1                         TO DETAIL-COUNT.
      *
       RT-GERAR-RELATORIO-LISTAX.                  EXIT.
      *----------------------------------------------------------------*
      *        ROTINA DE IMPRESSAO DE LINHA DO LISTA E CABECALHO       *
      *----------------------------------------------------------------*
       RT-IMPRIMIR-LINHA-LISTA                     SECTION.
      *
           IF REPORT-CLOSED
              OPEN OUTPUT LISTA
              MOVE 59               TO LINE-COUNT
           END-IF
      *
           IF  LINE-COUNT NOT LESS 58
               ADD   1              TO PAGE-COUNT
               WRITE REG-LISTA    FROM LISTA-TOP1 AFTER 2
               MOVE  SPACES         TO REG-LISTA
               WRITE REG-LISTA                    AFTER 1
               MOVE  3              TO LINE-COUNT
               SET   FIRST-DETAIL   TO TRUE
           END-IF.
      *
           ADD   1                  TO LINE-COUNT
           WRITE REG-LISTA FROM BUFFER            AFTER 1.
      *.
       RT-IMPRIMIR-LINHA-LISTAX.                   EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA LEITURA SEQUENCIAL DO ARQUIVO CARTAO        *
      *----------------------------------------------------------------*
       RT-LER-CARTAO                               SECTION.
      *
           READ CARTAO
             AT END
                CLOSE CARTAO
                SET FL-CARTAO-EOF TO TRUE
                IF  WS-LIDOS-CARTAO EQUAL ZERO
                    DISPLAY PGMID '000I  *=========================*'
                    DISPLAY PGMID '000I  * NAO HA CARTOES PARA LER *'
                    DISPLAY PGMID '000I  *=========================*'
                END-IF
             NOT AT END
                 SET FL-CARTAO-OK TO TRUE
                 ADD 1 TO WS-LIDOS-CARTAO
           END-READ.
      *
       RT-LER-CARTAOX.                             EXIT.
      *----------------------------------------------------------------*
      *       GRAVAR DADOS DA LINHA DO RELATORIO LISTA NO WORKSDS      *
      *----------------------------------------------------------------*
       RT-CARREGA-LISTA                            SECTION.
      *
           ADD  1                    TO WS-GRAVADOS-WORKSDS
           MOVE 'LISTA'              TO REG-WORKSDS
           MOVE WS-GRAVADOS-WORKSDS  TO WORKSDS-SEQUENCE
           MOVE CARTAO-EMP           TO LISTA-CARTAO-EMP
           MOVE RAISE-PERCENT        TO LISTA-RAISE-PERCENT
           PERFORM RT-GRAVAR-WORKSDS.
      *
       RT-CARREGA-LISTAX.                          EXIT.
      *----------------------------------------------------------------*
      *        ROTINA PARA MONTAR AS MENSAGENS DA TABELA DE ERRO       *
      *----------------------------------------------------------------*
       RT-MONTA-MSG                                SECTION.
      *
           MOVE WORKSDS-KEY             TO WS-OCOR3-WORKSDS-KEY
      *
           MOVE FL-STA-WORKSDS          TO WS-OCOR1-QSAM-STAT
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
           MOVE 'CARTA'                 TO SC5LDIS1-PGM.
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
