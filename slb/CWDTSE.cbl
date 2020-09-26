       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWDTSE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/10/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *   Calculo de validade do titulo de eleitor    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 SOMA                           PIC 9(008) VALUE 0.
           05 RESTO                          PIC 9(008) VALUE 0.
           05 LIXO                           PIC 9(008) VALUE 0.
           05 DIGVER.
              10 DIGVER1                     PIC 9(001) VALUE 0.
              10 DIGVER2                     PIC 9(001) VALUE 0.

       LINKAGE SECTION.

       01  PARAMETROS-CWDTSE.
           05 CWDTSE-TITULO                  PIC  9(012).
           05 REDEFINES CWDTSE-TITULO.
              10 D1                          PIC  9(001).
              10 D2                          PIC  9(001).
              10 D3                          PIC  9(001).
              10 D4                          PIC  9(001).
              10 D5                          PIC  9(001).
              10 D6                          PIC  9(001).
              10 D7                          PIC  9(001).
              10 D8                          PIC  9(001).
              10 UF.
                 15 UF1                      PIC  9(001).
                 15 UF2                      PIC  9(001).
              10 DV                          PIC  9(002).
              10 REDEFINES DV.
                 15 DV1                      PIC  9(001).
                 15 DV2                      PIC  9(001).
           05 CWDTSE-TITULO-ED               PIC  X(013).
           05 CWDTSE-RETORNO                 PIC  9(001).

       PROCEDURE DIVISION USING PARAMETROS-CWDTSE.

       000-INICIO.

           MOVE 1             TO CWDTSE-RETORNO
           MOVE ALL X"B0"     TO CWDTSE-TITULO-ED
           IF   CWDTSE-TITULO NUMERIC
           AND (CWDTSE-TITULO NOT = ZEROS)
                IF   UF = "01" OR "02"
                     COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7)
                                  + (D4 * 6) + (D5 * 5) + (D6 * 4)
                                  + (D7 * 3) + (D8 * 2)
                     IF   SOMA < 11
                          MOVE SOMA TO RESTO
                     ELSE
                          DIVIDE SOMA BY 11 GIVING LIXO REMAINDER RESTO
                     END-IF
                     IF   RESTO = 0
                          MOVE 1 TO DIGVER1
                     ELSE
                          IF   RESTO = 1
                               MOVE 0 TO DIGVER1
                          ELSE
                               COMPUTE DIGVER1 = 11 - RESTO
                          END-IF
                     END-IF
                     COMPUTE SOMA = (UF1 * 4) + (UF2 * 3)
                                              + (DIGVER1 * 2)
                     IF   SOMA < 11
                          MOVE SOMA TO RESTO
                     ELSE
                          DIVIDE SOMA BY 11 GIVING LIXO REMAINDER RESTO
                     END-IF
                     IF   RESTO = 0
                          MOVE 1 TO DIGVER2
                     ELSE
                          IF   RESTO = 1
                               MOVE 0 TO DIGVER2
                          ELSE
                               COMPUTE DIGVER2 = 11 - RESTO
                          END-IF
                     END-IF
                ELSE
                     COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7)
                                  + (D4 * 6) + (D5 * 5) + (D6 * 4)
                                  + (D7 * 3) + (D8 * 2)
                     DIVIDE SOMA BY 11 GIVING LIXO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0 TO DIGVER1
                     ELSE
                          COMPUTE DIGVER1 = 11 - RESTO
                     END-IF
                     COMPUTE SOMA = (UF1 * 4) + (UF2 * 3)
                                              + (DIGVER1 * 2)
                     DIVIDE SOMA BY 11 GIVING LIXO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0 TO DIGVER2
                     ELSE
                          COMPUTE DIGVER2 = 11 - RESTO
                     END-IF
                END-IF
                IF   DV = DIGVER
                     MOVE 0 TO CWDTSE-RETORNO
                     STRING CWDTSE-TITULO (1: 10) DELIMITED BY SIZE
                            "-"                   DELIMITED BY SIZE
                            CWDTSE-TITULO (11: 2) DELIMITED BY SIZE
                     INTO CWDTSE-TITULO-ED
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWDTSE.
