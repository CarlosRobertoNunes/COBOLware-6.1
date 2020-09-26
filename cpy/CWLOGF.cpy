
       FD  CWLOGF
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWLOGF.

       01  CWLOGF-REG.
           02 CWLOGF-DADOS.
              05 CWLOGF-DATA-DE-HOJE          PIC  X(011).
              05 CWLOGF-HORA                  PIC  X(009).
              05 CWLOGF-PROGRAMA              PIC  X(008)B.
              05 CWLOGF-RESTO                 PIC  X(080).
              05 REDEFINES CWLOGF-RESTO.
                 10 CWLOGF-PARTE.
                    15 CWLOGF-FUNCAO-PROGRAMA PIC  X(035).
                    15 CWLOGF-OPERADOR        PIC  X(030).
                 10 REDEFINES CWLOGF-PARTE.
                    15 CWLOGF-MENSAGEM        PIC  X(049).
                    15 CWLOGF-FIM             PIC  X(016).
                 10 CWLOGF-TASK               PIC  9(006).
                 10 CWLOGF-FATOR              PIC  9(003).
                 10 FILLER                    PIC  X(006).
           02 CWLOGF-SEQUENCIA COMP-3         PIC  9(018).

