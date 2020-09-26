       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWDCEI.
       AUTHOR.        Digifred Informatica Ltda.
       DATE-WRITTEN.  25/08/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *   Calculo de validade de C.E.I.               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 DV                       PIC  9(006) VALUE ZERO.
           05 FILLER REDEFINES DV.
              10 DV-LIXO               PIC  9(004).
              10 DV-DEZENA             PIC  9(001).
              10 DV-UNIDADE            PIC  9(001).

       LINKAGE SECTION.

       01  PARAMETROS-CWDCEI.
           05 CEI                         PIC  9(012).
           05 FILLER REDEFINES CEI.
              10 CEI-01                   PIC  9(001).
              10 CEI-02                   PIC  9(001).
              10 CEI-03                   PIC  9(001).
              10 CEI-04                   PIC  9(001).
              10 CEI-05                   PIC  9(001).
              10 CEI-06                   PIC  9(001).
              10 CEI-07                   PIC  9(001).
              10 CEI-08                   PIC  9(001).
              10 CEI-09                   PIC  9(001).
              10 CEI-10                   PIC  9(001).
              10 CEI-11                   PIC  9(001).
              10 CEI-12                   PIC  9(001).
           05 CWDCEI-RETORNO.
              10 RETORNO-1                PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-CWDCEI.

       010-PROCESSAMENTO.

           COMPUTE DV   = CEI-01 *  7
                        + CEI-02 *  4
                        + CEI-03 *  1
                        + CEI-04 *  8
                        + CEI-05 *  5
                        + CEI-06 *  2
                        + CEI-07 *  1
                        + CEI-08 *  6
                        + CEI-09 *  3
                        + CEI-10 *  7
                        + CEI-11 *  4

           COMPUTE DV = DV-DEZENA + DV-UNIDADE
           COMPUTE DV = 10 - DV-UNIDADE

           IF CEI-12 EQUAL DV-UNIDADE
              MOVE "0" TO CWDCEI-RETORNO
           ELSE
              MOVE "1" TO CWDCEI-RETORNO
           END-IF.

       010-99-FIM. EXIT PROGRAM.

       END PROGRAM CWDCEI.
