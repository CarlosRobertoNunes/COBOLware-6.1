       COPY CWSQLC.CPY.

       01  FS-CWCONF PIC  X(002) VALUE "00".
       01  FS-CWGRPS PIC  X(002) VALUE "00".
       01  KCO       PIC  X(030) VALUE "CHAVE".
       01  PCO       PIC  X(002) VALUE SPACE.
       01  KGR       PIC  X(030) VALUE "CHAVE".
       01  PGR       PIC  X(002) VALUE SPACE.

       01  CWCONF-REG.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO                        PIC  X(002).
              10 CWCONF-ELEMENTO                    PIC  X(030).
            05 CWCONF-RESTO                         PIC X(1976).

       01  CWCONF-REGTY redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-TTY                            PIC  X(030).
           05 CWCONF-DATE                           PIC  X(006).
           05 CWCONF-TIME                           PIC  X(006).

       01  CWCONF-REG00 redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-QUADRO                         PIC  9(001).
           05 CWCONF-EJECT-MODE-OLD                 PIC  X(002).
           05 CWCONF-FATOR-00-U              COMP-X PIC  9(002).
           05 CWCONF-SIZE-U                  COMP-X PIC  9(002).
           05 CWCONF-USUARIO                        PIC  X(030).
           05 CWCONF-FATOR-00-UP             COMP-X PIC  9(002).
           05 CWCONF-SIZE-UP                 COMP-X PIC  9(002).
           05 CWCONF-USUARIO-P                      PIC  X(030).
           05 CWCONF-APLICATIVO.
              10 CWCONF-FATOR-00-S           COMP-X PIC  9(002).
              10 CWCONF-SIZE-S               COMP-X PIC  9(002).
              10 CWCONF-SISTEMA                     PIC  X(030).
              10 CWCONF-FATOR-00-SP          COMP-X PIC  9(002).
              10 CWCONF-SIZE-SP              COMP-X PIC  9(002).
              10 CWCONF-SISTEMA-P                   PIC  X(030).

       01  CWCONF-REG02 redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-LABEL-file.
              10 CWCONF-LABEL-1                     PIC  X(051).
              10 CWCONF-LABEL-2                     PIC  X(051).
              10 CWCONF-LABEL-3                     PIC  X(051).
              10 CWCONF-LABEL-4                     PIC  X(051).
              10 CWCONF-LABEL-5                     PIC  X(051).

       01  CWCONF-REG03 redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-ARQUIVO                        PIC  X(030).
           05 CWCONF-LABEL                          PIC  X(050).
           05 CWCONF-EJECT-MODE                     PIC  X(002).
           05 CWCONF-ASCII                          PIC  X(001).
           05 CWCONF-CADEIA-ASCII-INICIAL.
              10 CWCONF-ASCII-I    OCCURS 50 COMP-X PIC  9(002).
           05 CWCONF-CADEIA-ASCII-FINAL.
              10 CWCONF-ASCII-F    OCCURS 50 COMP-X PIC  9(002).
           05 CWCONF-ESTILO                         PIC  X(030).
           05 CWCONF-FORMATO                        PIC  X(001).
              88 CWCONF-FORMATO-DOS    VALUE "D".
              88 CWCONF-FORMATO-UNIX   VALUE "U".
              88 CWCONF-FORMATO-NATIVO VALUE "N".
              88 CWCONF-FORMATO-OK     VALUE "D" "N" "U".
           05 CWCONF-CODEPAGE                       PIC  X(001).
              88 CWCONF-CODEPAGE-001     VALUE "0".
              88 CWCONF-CODEPAGE-OFF     VALUE "O".
              88 CWCONF-CODEPAGE-WINDOWS VALUE "W".
              88 CWCONF-CODEPAGE-850     VALUE "8".
              88 CWCONF-CODEPAGE-OK      VALUE "0" "O" "W" "8".

       01  CWCONF-REG92 redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-BEEP                           PIC  X(001).
              88 CWCONF-BEEP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CWCONF-STOP                           PIC  X(001).
              88 CWCONF-STOP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CWCONF-ERASE                          PIC  X(001).
              88 CWCONF-ERASE-ON VALUE "S" "s"
                                       "Y" "y".
           05 CWCONF-TIMER                          PIC  9(002).

       01  CWCONF-REG94 redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-RELATORIO                      PIC  X(007).
           05 CWCONF-NAME-REPORT                    PIC  X(023).
           05 CWCONF-PROGRAMA-PRINTER.
              10 CWCONF-PROGRAMA-ASCII1      COMP-X PIC  9(002).
              10 CWCONF-PROGRAMA-ASCII2      COMP-X PIC  9(002).
           05 CWCONF-DESPROGRAMA.
              10 CWCONF-DESPROGRAMA-ASCII1   COMP-X PIC  9(002).
              10 CWCONF-DESPROGRAMA-ASCII2   COMP-X PIC  9(002).
           05 CWCONF-TIPO-FORM                      PIC  X(020).
           05 CWCONF-SIZE-PAGE                      PIC  9(003).
           05 CWCONF-SAIDA.
              10 CWCONF-BYTE-SAIDA OCCURS 15 COMP-X PIC  9(002).
           05 CWCONF-TITLE.
              10 CWCONF-TITLE-1                     PIC  X(043).
              10 CWCONF-TITLE-2                     PIC  X(043).
           05 CWCONF-SUB-TITLE.
              10 CWCONF-SUB-TITLE-1                 PIC  X(043).
              10 CWCONF-SUB-TITLE-2                 PIC  X(043).
           05 CWCONF-CAMPOS-AP.
              10 CWCONF-CAMPOS-APAGADOS OCCURS 6.
                 15 CWCONF-INICIO-AP                PIC  9(003).
                 15 CWCONF-FIM-AP                   PIC  9(003).
           05 CWCONF-CAMPOS-TB.
              10 CWCONF-CAMPOS-TABULADOS OCCURS 6.
                 15 CWCONF-INICIO-TB                PIC  9(003).
                 15 CWCONF-FIM-TB                   PIC  9(003).
           05 CWCONF-EMPRESA-ALT                    PIC  X(030).
           05 CWCONF-SISTEMA-ALT                    PIC  X(030).
           05 CWCONF-KEEP                           PIC  X(001).
           05 CWCONF-QUIET                          PIC  X(001).

           66 CWCONF-DADOS RENAMES CWCONF-PROGRAMA-ASCII1
                              THRU CWCONF-QUIET.

       01  CWCONF-REG99 redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-PAGINA                         PIC  9(004).
           05 FILLER                                PIC  X(026).
           05 CWCONF-OPCOES.
              10 CWCONF-OPCAO OCCURS 26.
                 15 CWCONF-NIVEL                    PIC  9(001).
                 15 CWCONF-CHECK                    PIC  X(001).
                 15 CWCONF-FATOR-P-99        COMP-X PIC  9(002).
                 15 CWCONF-FATOR-S-99        COMP-X PIC  9(002).
                 15 CWCONF-PROG                     PIC  X(008).
                 15 CWCONF-SIZE-P-99         COMP-X PIC  9(002).
                 15 CWCONF-SIZE-S-99         COMP-X PIC  9(002).
                 15 CWCONF-PASS                     PIC  X(006).
                 15 CWCONF-HELP                     PIC  X(020).
                 15 CWCONF-NO-OPCAO                 PIC  9(002).
                 15 CWCONF-NM-OPCAO                 PIC  X(034).

       01  CWCONF-REGMD redefines CWCONF-REG.
           05 FILLER                                PIC  X(008).
           05 CWCONF-TIPO-MODULO                    PIC  X(002).
           05 FILLER                                PIC  X(022).
           05 CWCONF-MODULO                         PIC  X(015).
           05 FILLER                                PIC  X(010).

       01  CWCONF-REGAT redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-ATRIBUTOS.
              10 CWCONF-CORES OCCURS 20.
                 15 CWCONF-COR-TEXTO                PIC  9(001).
                 15 CWCONF-COR-FUNDO                PIC  9(001).

       01  CWCONF-REGJB redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-JOB                            PIC  X(030).
           05 CWCONF-JOB-MODULO                     PIC  X(050).
           05 CWCONF-JOB-TIPO                       PIC  X(001).
              88 CWCONF-JOB-BINARIO                      VALUE "1".
              88 CWCONF-JOB-COBOL                        VALUE "2".
              88 CWCONF-JOB-WINDOWS                      VALUE "3".
              88 CWCONF-JOB-TIPO-OK                      VALUE
                                                      "1" "2" "3".
           05 CWCONF-JOB-PARAMETRO                  PIC  X(060).
           05 CWCONF-JOB-PROXIMO-RC-OK              PIC  X(007).
           05 CWCONF-JOB-PROXIMO-NAO-OK             PIC  X(007).
           05 CWCONF-JOB-MENSAGEM                   PIC  X(050).

       01  CWCONF-REGLG redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-OPCOES-LOG.
              10 CWCONF-MASTER                      PIC  X(008).
              10 CWCONF-END                         PIC  X(008).
              10 CWCONF-LOGIN                       PIC  X(008).
              10 CWCONF-LOGOUT                      PIC  X(008).
              10 CWCONF-CALLIN                      PIC  X(008).
              10 CWCONF-CALLOUT                     PIC  X(008).
              10 CWCONF-HELPDIR-D                   PIC  X(050).
              10 CWCONF-HELPDIR-U                   PIC  X(050).
              10 CWCONF-TIMEOUT                     PIC  9(004).
              10 CWCONF-AUTOPASS                    PIC  9(001).
              10 CWCONF-CLOCK                       PIC  9(001).
              10 CWCONF-LOG                         PIC  9(001).
              10 CWCONF-MOUSE                       PIC  9(001).
              10 CWCONF-DIR                         PIC  9(001).
              10 CWCONF-HIGH                        PIC  9(001).
              10 CWCONF-SPOOL                       PIC  9(001).
              10 CWCONF-CODELOG                     PIC  9(001).
              10 CWCONF-PRTPOSIT                    PIC  9(001).
              10 CWCONF-RETRY                       PIC  9(004).
              10 CWCONF-FILLER                      PIC  X(002).
              10 CWCONF-LOGDIR                      PIC  X(050).
              10 CWCONF-FILESHARE.
                 15 CWCONF-FSSERVER                 PIC  X(015).
                 15 CWCONF-CCITCP2                  PIC  X(015).
                 15 CWCONF-FSUSERNAME               PIC  X(020).
                 15 CWCONF-FSPASSWORD               PIC  X(020).
              10 CWCONF-MIN-SENHA                   PIC  9(002).
              10 CWCONF-EXPIRE                      PIC  9(003).
              10 CWCONF-REUSE                       PIC  9(003).
              10 CWCONF-FSCHANGED                   PIC  9(001).

       01  CWCONF-REGPS redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-NOME                           PIC  X(030).
           05 CWCONF-NIVEL-PS                COMP-X PIC  9(002).
           05 CWCONF-SIZE-PS                 COMP-X PIC  9(002).
           05 CWCONF-FATOR-PS                COMP-X PIC  9(002).
           05 CWCONF-SENHA                          PIC  X(030).
           05 CWCONF-GRUPO                          PIC  X(022).
           05 CWCONF-MODO-MENU                      PIC  9(001).
           05 CWCONF-PATH-SPOOL                     PIC  X(030).
           05 CWCONF-PRINTER-DEFAULT                PIC  X(008).
           05 CWCONF-QUADRO-PS                      PIC  9(001).
           05 CWCONF-SPOOL-OPTIONS.
              10 CWCONF-ZEBRA                       PIC  9(001).
              10 CWCONF-FULL                        PIC  9(001).
              10 CWCONF-COLUNA-SORT                 PIC  9(001).
              10 CWCONF-OPCAO-SORT                  PIC  9(001).
           05 CWCONF-RELATOR-IN                     PIC  9(009).
           05 CWCONF-RELATOR-OUT                    PIC  9(009).
           05 CWCONF-ESTILO-FLAG                    PIC  9(001).
           05 CWCONF-AUTOVIEW                       PIC  9(001).
           05 CWCONF-PRINTER-DEFAULT2               PIC  X(022).
           05 CWCONF-E-MAIL                         PIC  X(042).
           05 CWCONF-SMTP                           PIC  X(042).
           05 CWCONF-SMTP-SIZE               COMP-X PIC  9(002).
           05 CWCONF-SMTP-FATOR              COMP-X PIC  9(002).
           05 CWCONF-SMTP-PASSWORD                  PIC  X(020).
           05 CWCONF-DATA-SENHA              COMP-3 PIC  9(008).
           05 CWCONF-LOGIN-ERRO                     PIC  9(001).
           05 CWCONF-LOGIN-LAST              COMP-3 PIC  9(008).
           05 CWCONF-BLOQUEADO                      PIC  X(001).
           05 CWCONF-ESQUECI-SIZE            COMP-X PIC  9(002).
           05 CWCONF-ESQUECI-FATOR           COMP-X PIC  9(002).
           05 CWCONF-ESQUECI.
              10 CWCONF-PERGUNTA                    PIC  X(030).
              10 CWCONF-RESPOSTA                    PIC  X(030).
           05 CWCONF-PAI                            PIC  X(030).
           05 CWCONF-SMTP-PORT                      PIC  X(005).
           05 CWCONF-SSL                            PIC  X(001).
           05 CWCONF-AUTENTICACAO                   PIC  X(001).

       01  CWCONF-REGGU redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-NOME-GRUPO                     PIC  X(022).
           05 CWCONF-PROG-GRUPO                     PIC  X(008).
           05 CWCONF-GRUPO-ID.
              10 CWCONF-ACESSO-GRUPO                PIC  X(001).
              10 CWCONF-ALTERACAO-GRUPO             PIC  X(001).
              10 CWCONF-CONSULTA-GRUPO              PIC  X(001).
              10 CWCONF-EXCLUSAO-GRUPO              PIC  X(001).
              10 CWCONF-INCLUSAO-GRUPO              PIC  X(001).
           05 CWCONF-ADM                            PIC  X(001).

       01  CWCONF-REGGX redefines CWCONF-REG.
           05 FILLER                                PIC  X(030).
           05 CWCONF-GRUPO-SEQ                      PIC  9(005).

       01  CWCONF-REGGI redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-GU-ID                          PIC  9(005).
           05 CWCONF-IMPRESSORA                     PIC  X(025).

       01  CWCONF-REGRT redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-TOT-ROTINAS                    PIC  9(002).
           05 CWCONF-ROTINAS.
              10 CWCONF-ROTINA            OCCURS 84 PIC  X(008).

       01  CWCONF-REGRV redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-VERSAO                         PIC  99V99.
           05 CWCONF-REVISAO                        PIC  999.

       01  CWCONF-REGVD redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-OLD-VALIDADE                   PIC  9(006) COMP-3.
           05 CWCONF-TRAVADO                        PIC  X(001).
           05 CWCONF-SENHA-ATIVACAO                 PIC  9(006) COMP-3.
           05 CWCONF-FLAG-2000                      PIC  9(001).
           05 CWCONF-VALIDADE                       PIC  9(008) COMP-3.
           05 CWCONF-VALIDADE-10                    PIC  9(008) COMP-3.
           05 CWCONF-VALIDADE-15                    PIC  9(008) COMP-3.
           05 CWCONF-VALIDADE-20                    PIC  9(008) COMP-3.
           05 CWCONF-ULTIMO-LOGIN.
              10 CWCONF-ULTIMO-LOGIN-DATA           PIC  9(008) COMP-3.
              10 CWCONF-ULTIMO-LOGIN-HORA           PIC  9(006) COMP-3.
           05 CWCONF-EXIBE-LICENCA                  PIC  X(001).
           05 CWCONF-DESTRAVA                       PIC  X(001).
           05 CWCONF-TENTATIVAS                     PIC  9(001).

       01  CWCONF-REGMX redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-MAXUSERS                       PIC  9(006) COMP-3.

       01  CWCONF-REGES redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-ESTILOS.
              10 OCCURS 16.
                 15 OCCURS 16.
                    20 CWCONF-ASC                   PIC  9(003).

       01  CWCONF-REGTK redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-TASK OCCURS 9                  PIC  9(009) COMP-3.

       01  CWCONF-REGLC redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-DATA-X                         PIC  9(008) COMP-X.
           05 CWCONF-LICENCA-SALVA.
              10 CWCONF-LICENCA-X                   PIC  9(010) COMP-X.
              10 CWCONF-LICENCIADO                  PIC  X(040).

       01  CWCONF-REGRL redefines CWCONF-REG.
           05 FILLER                                PIC  X(032).
           05 CWCONF-RELATOR                        PIC  X(050).
           05 CWCONF-RELATOR-ROLL                   PIC  X(001).
           05 CWCONF-RELATOR-COUNT                  PIC  X(001).
           05 CWCONF-RELATOR-YEAR                   PIC  9(002).

       01  CWCONF-REGSX redefines CWCONF-REG.
           05 CWCONF-CHAVEX.
              10 CWCONF-S                          PIC  X(001).
              10 CWCONF-SEQ                 COMP-X PIC  9(002).
              10                                   PIC  X(030).
           05 CWCONF-FATOR                  COMP-X PIC  9(002).
           05 CWCONF-SIZE                   COMP-X PIC  9(002).
           05 CWCONF-PASSWORD                      PIC  X(030).

       01  CWCONF-REGSQ redefines CWCONF-REG.
           05 FILLER                               PIC  X(002).
           05 CWCONF-SEQUENCE                      PIC  X(030).
           05 CWCONF-VALUE                         PIC  9(018).
           05 CWCONF-INCREMENT                     PIC  9(018).

       01  CWGRPS-REG.
           05 CWGRPS-CHAVE.
              10 CWGRPS-TIPO                        PIC  X(002).
              10 CWGRPS-ELEMENTO                    PIC  X(030).
            05 CWGRPS-RESTO                         PIC X(1976).

       01  CWGRPS-REGGU redefines CWGRPS-REG.
           05 FILLER                                PIC  X(002).
           05 CWGRPS-NOME-GRUPO                     PIC  X(022).
           05 CWGRPS-PROG-GRUPO                     PIC  X(008).
           05 CWGRPS-GRUPO-ID.
              10 CWGRPS-ACESSO-GRUPO                PIC  X(001).
              10 CWGRPS-ALTERACAO-GRUPO             PIC  X(001).
              10 CWGRPS-CONSULTA-GRUPO              PIC  X(001).
              10 CWGRPS-EXCLUSAO-GRUPO              PIC  X(001).
              10 CWGRPS-INCLUSAO-GRUPO              PIC  X(001).
           05 CWGRPS-ADM                            PIC  X(001).

