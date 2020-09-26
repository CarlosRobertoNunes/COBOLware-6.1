       FD  CWDIRS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWDIRS.

       01  CWDIRS-REG.
           05 CWDIRS-SPOOL.
              10 CWDIRS-CW             PIC  X(002).
              10 CWDIRS-NUMERO         PIC  9(006).
              10 CWDIRS-PONTO          PIC  X(001).
              10 CWDIRS-EXT            PIC  X(003).
           05 CWDIRS-COMANDO           PIC  X(007).
           05 CWDIRS-IMPRESSO          PIC  X(001).
           05 CWDIRS-CODIGO            PIC  X(007).
           05 CWDIRS-TIPO              PIC  X(001).
           05 CWDIRS-PROGRAMA          PIC  X(002).
           05 CWDIRS-FOLHAS            PIC  9(006).
           05 CWDIRS-LINHAS            PIC  9(010).
           05 CWDIRS-EMISSAO.
              10 CWDIRS-DATA           PIC  9(008).
              10 CWDIRS-HORA           PIC  9(002).
              10 CWDIRS-MINUTO         PIC  9(002).
           05 CWDIRS-USUARIO           PIC  X(010).
           05 CWDIRS-NOTA              PIC  X(020).
           05 CWDIRS-ESTILO            PIC  X(001).
           05 CWDIRS-TITULO            PIC  X(077).
           05 CWDIRS-WIDTH             PIC  9(003).

