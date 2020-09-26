       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWDPIS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/10/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *   Calculo de validade de PIS/PASEP            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 RESTO     PIC 99   VALUE 0.
           05 RESULTADO PIC 9(4) VALUE 0.
           05 DV        PIC 99   VALUE 0.
           05 QUOCIENTE PIC 99   VALUE 0.

       LINKAGE SECTION.

       01  PARAMETROS-CWDPIS.
           05 CWDPIS-PIS            PIC 9(011).
              88 PIS-INVALIDO             VALUE 111111111 0
                                              99999999999.
           05 FILLER REDEFINES CWDPIS-PIS.
              10 D01                PIC 9(001).
              10 D02                PIC 9(001).
              10 D03                PIC 9(001).
              10 D04                PIC 9(001).
              10 D05                PIC 9(001).
              10 D06                PIC 9(001).
              10 D07                PIC 9(001).
              10 D08                PIC 9(001).
              10 D09                PIC 9(001).
              10 D10                PIC 9(001).
              10 DV-INFORMADO       PIC 9(001).
           05 CWDPIS-PIS-PASEP-ED   PIC X(014).
           05 CWDPIS-RETORNO        PIC 9(001).

       PROCEDURE DIVISION USING PARAMETROS-CWDPIS.

       010-PROCESSAMENTO.

           MOVE 1         TO CWDPIS-RETORNO
           MOVE ALL X"B0" TO CWDPIS-PIS-PASEP-ED

           IF   NOT PIS-INVALIDO
                COMPUTE RESULTADO = (D01 * 3) + (D02 * 2) +
                                    (D03 * 9) + (D04 * 8) +
                                    (D05 * 7) + (D06 * 6) +
                                    (D07 * 5) + (D08 * 4) +
                                    (D09 * 3) + (D10 * 2)
                DIVIDE RESULTADO BY 11 GIVING QUOCIENTE
                                    REMAINDER RESTO
                IF   RESTO = 0
                     MOVE 0 TO DV
                ELSE
                     COMPUTE DV = 11 - RESTO
                END-IF
                IF  (DV NOT = 10)
                AND  DV = DV-INFORMADO
                     MOVE 0      TO CWDPIS-RETORNO
                     MOVE SPACES TO CWDPIS-PIS-PASEP-ED
                     STRING CWDPIS-PIS  (1: 3) DELIMITED BY SIZE
                            "."                DELIMITED BY SIZE
                            CWDPIS-PIS  (4: 4) DELIMITED BY SIZE
                            "."                DELIMITED BY SIZE
                            CWDPIS-PIS  (8: 2) DELIMITED BY SIZE
                            "."                DELIMITED BY SIZE
                            CWDPIS-PIS (10: 2) DELIMITED BY SIZE
                               INTO CWDPIS-PIS-PASEP-ED
                END-IF
           END-IF.

       010-99-FIM. EXIT PROGRAM.

       END PROGRAM CWDPIS.
