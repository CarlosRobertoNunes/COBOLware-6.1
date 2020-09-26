
      ***********************************************************
      *    COBOLware 5.0 Lay-out de arquivo texto               *
      ***********************************************************

       FD  spool
           RECORD CONTAINS 0234 CHARACTERS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "spool\spool.seq".

       01  spool-REG.
           05 CODIGO                         PIC   X(007).
           05 PAGINAS                        PIC   9(006).
           05 LINHAS                         PIC   9(006).
           05 LARGURA                        PIC   9(003).
           05 GERACAO                        PIC   X(010).
           05 HORA                           PIC   X(008).
           05 PROPRIETARIO                   PIC   X(010).
           05 TITULO                         PIC   X(077).
           05 ARQUIVO                        PIC   X(080).
           05 OBSERVACAO                     PIC   X(020).
           05 CONTROLE                       PIC   X(007).

