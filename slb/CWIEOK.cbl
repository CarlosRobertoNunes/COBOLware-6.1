       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWIEOK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/05/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Validade e edicao de Inscricao Estadual      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO VALUE ALL "0".
           05 CONTA-N                       PIC 9(002).
           05 I                             PIC 9(002).
           05 Y                             PIC 9(002).
           05 IE0                           PIC X(015).
           05 SOMA                          PIC 9(004).
           05 INTEIRO                       PIC 9(004).
           05 RESTO                         PIC 9(004).
           05 DIG-1                         PIC 9(004).
           05 DIG-1-R REDEFINES DIG-1.
              10 FILLER                     PIC 9(003).
              10 DIG-1-4                    PIC 9(001).
           05 DIG-2                         PIC 9(004).
           05 DIG-2-R REDEFINES DIG-2.
              10 FILLER                     PIC 9(003).
              10 DIG-2-4                    PIC 9(001).
           05 W-D-RO                        PIC X(005).
           05 W-D.
              10 D1                         PIC 9(001).
              10 D1-R REDEFINES D1          PIC X(001).
              10 D2                         PIC 9(001).
              10 D3                         PIC 9(001).
              10 D4                         PIC 9(001).
              10 D5                         PIC 9(001).
              10 D6                         PIC 9(001).
              10 D7                         PIC 9(001).
              10 D8                         PIC 9(001).
              10 D9                         PIC 9(001).
              10 D10                        PIC 9(001).
              10 D11                        PIC 9(001).
              10 D12                        PIC 9(001).
              10 D13                        PIC 9(001).
              10 D14                        PIC 9(001).
           05 W-DR REDEFINES W-D            PIC X(014).

       01  AREAS-DE-TRABALHO-MG VALUE ALL "0".
           05 DMG                           PIC 9(001).
           05 W-D1.
              10 W-D1-1                     PIC 9(001).
              10 W-D1-2                     PIC 9(001).
           05 W-D1-R REDEFINES W-D1         PIC 9(002).
           05 W-D2.
              10 W-D2-1                     PIC 9(001).
              10 W-D2-2                     PIC 9(001).
           05 W-D2-R REDEFINES W-D2         PIC 9(002).
           05 W-D3.
              10 W-D3-1                     PIC 9(001).
              10 W-D3-2                     PIC 9(001).
           05 W-D3-R REDEFINES W-D3         PIC 9(002).
           05 W-D4.
              10 W-D4-1                     PIC 9(001).
              10 W-D4-2                     PIC 9(001).
           05 W-D4-R REDEFINES W-D4         PIC 9(002).
           05 W-D5.
              10 W-D5-1                     PIC 9(001).
              10 W-D5-2                     PIC 9(001).
           05 W-D5-R REDEFINES W-D5         PIC 9(002).
           05 W-D6.
              10 W-D6-1                     PIC 9(001).
              10 W-D6-2                     PIC 9(001).
           05 W-D6-R REDEFINES W-D6         PIC 9(002).
           05 W-D7.
              10 W-D7-1                     PIC 9(001).
              10 W-D7-2                     PIC 9(001).
           05 W-D7-R REDEFINES W-D7         PIC 9(002).
           05 W-D8.
              10 W-D8-1                     PIC 9(001).
              10 W-D8-2                     PIC 9(001).
           05 W-D8-R REDEFINES W-D8         PIC 9(002).
           05 W-D9.
              10 W-D9-1                     PIC 9(001).
              10 W-D9-2                     PIC 9(001).
           05 W-D9-R REDEFINES W-D9         PIC 9(002).
           05 W-D10.
              10 W-D10-1                    PIC 9(001).
              10 W-D10-2                    PIC 9(001).
           05 W-D10-R REDEFINES W-D10       PIC 9(002).
           05 W-D11.
              10 W-D11-1                    PIC 9(001).
              10 W-D11-2                    PIC 9(001).
           05 W-D11-R REDEFINES W-D11       PIC 9(002).
           05 ALGARISMO                     PIC 9(002).
           05 ALGARISMOS.
              10 ALGARIMOS-1                PIC 9(001).
              10 ALGARIMOS-2                PIC 9(001).
              10 ALGARIMOS-3                PIC 9(001).
              10 ALGARIMOS-4                PIC 9(001).
           05 ALGARISMOS-R REDEFINES ALGARISMOS
                                            PIC 9(004).
           05 PRODUTOS                      PIC 9(004).
           05 DEZENA                        PIC 9(002).

       01  AREAS-DE-TRABALHO-AP-GO.
           05 P                             PIC 9(001) VALUE 0.
           05 D                             PIC 9(001) VALUE 0.
           05 INSCRICAO-AP-GO               PIC 9(008) VALUE 0.

       01  AREAS-DE-TRABALHO-RS.
           05 INSCRICAO-RS                  PIC 9(003) VALUE 0.

       01  AREAS-DE-TRABALHO-RR.
           05 INSCRICAO-RR                  PIC 9(002) VALUE 0.

       01  AREAS-DE-TRABALHO-TO.
           05 INSCRICAO-TO                  PIC 9(002) VALUE 0.

       01  AREAS-DE-TRABALHO-XX.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       LINKAGE SECTION.

       01  PARAMETROS-SGIEOK.
           05 UF             PIC X(002).
           05 IE             PIC X(018).
           05 IE-ED          PIC X(018).
           05 RETORNO        PIC 9(001).

       PROCEDURE DIVISION USING PARAMETROS-SGIEOK.
       000-INICIO.

           MOVE IE TO W-DR
           INSPECT W-DR CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE SPACES TO IE-ED

           IF   W-DR = "ISENTO" OR "I S E N T O"
                MOVE IE TO IE-ED
                MOVE 0  TO RETORNO
                GOBACK
           END-IF

           MOVE SPACES TO W-DR
           MOVE 0      TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 18
                   IF   IE (I: 1) = "P"
                        MOVE "P-" TO UF
                   END-IF
                   IF   IE (I: 1) NUMERIC
                   OR ((IE (I: 1) NOT NUMERIC)
                   AND (I = 1)
                   AND (UF = "SP"))
                       IF   Y < 14
                            ADD  1         TO Y
                            MOVE IE (I: 1) TO W-DR (Y: 1)
                       END-IF
                   END-IF
           END-PERFORM

           MOVE    Y TO CONTA-N
           MOVE W-DR TO IE0
           INSPECT IE0 CONVERTING SPACE TO "0"

           MOVE 0 TO RETORNO

           IF    IE0 = ALL "0"
                 MOVE 1 TO RETORNO
           ELSE
                 EVALUATE UF
                     WHEN "ES" PERFORM 010-VALIDA-ES THRU 010-99-FIM
                     WHEN "MG" PERFORM 020-VALIDA-MG THRU 020-99-FIM
                     WHEN "BA" PERFORM 030-VALIDA-BA THRU 030-99-FIM
                     WHEN "RJ" PERFORM 040-VALIDA-RJ THRU 040-99-FIM
                     WHEN "DF" PERFORM 050-VALIDA-DF THRU 050-99-FIM
                     WHEN "SP" PERFORM 060-VALIDA-SP THRU 060-99-FIM
                     WHEN "AC" PERFORM 070-VALIDA-AC THRU 070-99-FIM
                     WHEN "AL" PERFORM 080-VALIDA-AL THRU 080-99-FIM
                     WHEN "AP" PERFORM 090-VALIDA-AP THRU 090-99-FIM
                     WHEN "AM" PERFORM 100-VALIDA-AM THRU 100-99-FIM
                     WHEN "CE" PERFORM 110-VALIDA-CE THRU 110-99-FIM
                     WHEN "GO" PERFORM 120-VALIDA-GO THRU 120-99-FIM
                     WHEN "MS" PERFORM 130-VALIDA-MS THRU 130-99-FIM
                     WHEN "PA" PERFORM 140-VALIDA-PA THRU 140-99-FIM
                     WHEN "MA" PERFORM 150-VALIDA-MA THRU 150-99-FIM
                     WHEN "MT" PERFORM 160-VALIDA-MT THRU 160-99-FIM
                     WHEN "PB" PERFORM 170-VALIDA-PB THRU 170-99-FIM
                     WHEN "PR" PERFORM 180-VALIDA-PR THRU 180-99-FIM
                     WHEN "PE" PERFORM 190-VALIDA-PE THRU 190-99-FIM
                     WHEN "PI" PERFORM 200-VALIDA-PI THRU 200-99-FIM
                     WHEN "RN" PERFORM 210-VALIDA-RN THRU 210-99-FIM
                     WHEN "RS" PERFORM 220-VALIDA-RS THRU 220-99-FIM
                     WHEN "RO" PERFORM 230-VALIDA-RO THRU 230-99-FIM
                     WHEN "RR" PERFORM 240-VALIDA-RR THRU 240-99-FIM
                     WHEN "SC" PERFORM 250-VALIDA-SC THRU 250-99-FIM
                     WHEN "SE" PERFORM 260-VALIDA-SE THRU 260-99-FIM
                     WHEN "TO" PERFORM 270-VALIDA-TO THRU 270-99-FIM
                     WHEN "P-" PERFORM 280-VALIDA-P  THRU 280-99-FIM
                     WHEN OTHER
                               MOVE 1 TO RETORNO
                 END-EVALUATE
           END-IF

           IF   RETORNO = 0
                MOVE W-DR TO IE0
                PERFORM 900-EDIT-IE THRU 900-99-FIM
           END-IF

           GOBACK.

       010-VALIDA-ES.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-1
           ELSE
                COMPUTE DIG-1 = RESTO - 11
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       010-99-FIM. EXIT.

       020-VALIDA-MG.

           IF   Y <> 13
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   D13 NOT NUMERIC
                MOVE 1 TO RETORNO
                GO TO 020-99-FIM
           END-IF

           COMPUTE W-D1-R  = D1  * 1
           COMPUTE W-D2-R  = D2  * 2
           COMPUTE W-D3-R  = D3  * 1
           COMPUTE W-D4-R  = D4  * 1
           COMPUTE W-D5-R  = D5  * 2
           COMPUTE W-D6-R  = D6  * 1
           COMPUTE W-D7-R  = D7  * 2
           COMPUTE W-D8-R  = D8  * 1
           COMPUTE W-D9-R  = D9  * 2
           COMPUTE W-D10-R = D10 * 1
           COMPUTE W-D11-R = D11 * 2

           COMPUTE ALGARISMOS-R = W-D1-1 + W-D1-2  + W-D2-1  + W-D2-2 +
                                  W-D3-1 + W-D3-2  + W-D4-1  + W-D4-2 +
                                  W-D5-1 + W-D5-2  + W-D6-1  + W-D6-2 +
                                  W-D7-1 + W-D7-2  + W-D8-1  + W-D8-2 +
                                  W-D9-1 + W-D9-2  + W-D10-1 + W-D10-2 +
                                  W-D11-1 + W-D11-2

           COMPUTE DIG-1 = 10 - ALGARIMOS-4

           COMPUTE SOMA = (D1 *  3) + (D2  * 2) + (D3  * 11) +
                          (D4 * 10) + (D5  * 9) + (D6  *  8) +
                          (D7  * 7) + (D8  * 6) + (D9  *  5) +
                          (D10 * 4) + (D11 * 3) + (D12 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-2
           ELSE
                COMPUTE DIG-2 = RESTO - 11
           END-IF

           IF  (DIG-1-4 NOT = D12)
           OR  (DIG-2   NOT = D13)
                MOVE 1 TO RETORNO
           END-IF.

       020-99-FIM. EXIT.

       030-VALIDA-BA.

           IF   Y < 8
           OR   Y > 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   Y = 9
                IF   D1 = 0 OR 1 OR 2 OR 3 OR 4 OR 5 OR 8
                     COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) +
                                    (D4 * 5) + (D5 * 4) + (D6 * 3) +
                                    (D7 * 2)
                     DIVIDE SOMA BY 10 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0
                          MOVE 0    TO DIG-2
                     ELSE
                          COMPUTE DIG-2 = RESTO - 10
                     END-IF
                     COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                                    (D4 * 6) + (D5 * 5) + (D6 * 4) +
                                    (D7 * 3) + (D9 * 2)
                     DIVIDE SOMA BY 10 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0
                          MOVE 0    TO DIG-1
                     ELSE
                          COMPUTE DIG-1 = RESTO - 10
                     END-IF
                     IF  (DIG-1 NOT = D8)
                     OR  (DIG-2 NOT = D9)
                          MOVE 1 TO RETORNO
                          IF   D1 = 0
                          AND  IE-ED = SPACES
                               MOVE 0          TO RETORNO
                               MOVE 8          TO Y
                               MOVE W-D        TO IE-ED
                               MOVE IE-ED (2:) TO W-D
                               GO TO 030-VALIDA-BA
                          END-IF
                     END-IF
                ELSE
                     COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) +
                                    (D4 * 5) + (D5 * 4) + (D6 * 3) +
                                    (D7 * 2)
                     DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0    TO DIG-2
                     ELSE
                          COMPUTE DIG-2 = RESTO - 11
                     END-IF
                     COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                                    (D4 * 6) + (D5 * 5) + (D6 * 4) +
                                    (D7 * 3) + (D9 * 2)
                     DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0    TO DIG-1
                     ELSE
                          COMPUTE DIG-1 = RESTO - 11
                     END-IF
                     IF  (DIG-1 NOT = D8)
                     OR  (DIG-2 NOT = D9)
                          MOVE 1 TO RETORNO
                     END-IF
                END-IF
           ELSE
                IF   D1 = 0 OR 1 OR 2 OR 3 OR 4 OR 5 OR 8
                     COMPUTE SOMA = (D1 * 7) + (D2 * 6) + (D3 * 5) +
                                    (D4 * 4) + (D5 * 3) + (D6 * 2)
                     DIVIDE SOMA BY 10 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0
                          MOVE 0    TO DIG-2
                     ELSE
                          COMPUTE DIG-2 = RESTO - 10
                     END-IF
                     COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) +
                                    (D4 * 5) + (D5 * 4) + (D6 * 3) +
                                    (D8 * 2)
                     DIVIDE SOMA BY 10 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0
                          MOVE 0    TO DIG-1
                     ELSE
                          COMPUTE DIG-1 = RESTO - 10
                     END-IF
                     IF  (DIG-1 NOT = D7)
                     OR  (DIG-2 NOT = D8)
                          MOVE 1 TO RETORNO
                     END-IF
                ELSE
                     COMPUTE SOMA = (D1 * 7) + (D2 * 6) + (D3 * 5) +
                                    (D4 * 4) + (D5 * 3) + (D6 * 2)
                     DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0    TO DIG-2
                     ELSE
                          COMPUTE DIG-2 = RESTO - 11
                     END-IF
                     COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) +
                                    (D4 * 5) + (D5 * 4) + (D6 * 3) +
                                    (D8 * 2)
                     DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                     IF   RESTO = 0 OR 1
                          MOVE 0    TO DIG-1
                     ELSE
                          COMPUTE DIG-1 = RESTO - 11
                     END-IF
                     IF  (DIG-1 NOT = D7)
                     OR  (DIG-2 NOT = D8)
                          MOVE 1 TO RETORNO
                     END-IF
                END-IF
                IF   RETORNO = 1
                AND (IE-ED NOT = SPACES)
                    MOVE IE-ED TO W-D
                    MOVE 9     TO Y
                    MOVE 0     TO RETORNO
                    GO TO 030-VALIDA-BA
                END-IF
           END-IF

           MOVE SPACES TO IE-ED.

       030-99-FIM. EXIT.

       040-VALIDA-RJ.

           IF   Y <> 8
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 2) + (D2 * 7) + (D3 * 6) + (D4 * 5) +
                          (D5 * 4) + (D6 * 3) + (D7 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-1
           ELSE
                COMPUTE DIG-1 = RESTO - 11
           END-IF

           IF   DIG-1 NOT = D8
                MOVE 1 TO RETORNO
           END-IF.

       040-99-FIM. EXIT.

       050-VALIDA-DF.

           IF   Y <> 13
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 4) + (D2 * 3) + (D3 * 2) + (D4 * 9) +
                          (D5 * 8) + (D6 * 7) + (D7 * 6) + (D8 * 5) +
                          (D9 * 4) + (D10 * 3) + (D11 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           COMPUTE DIG-2 = RESTO - 11

           IF   DIG-2 = 10 OR 11
                MOVE 0 TO DIG-2
           END-IF

           COMPUTE SOMA = (D1 * 5) + (D2 * 4) + (D3 * 3) + (D4 * 2) +
                          (D5 * 9) + (D6 * 8) + (D7 * 7) + (D8 * 6) +
                          (D9 * 5) + (D10 * 4) + (D11 * 3) + (D12 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           COMPUTE DIG-1 = RESTO - 11

           IF   DIG-1 = 10 OR 11
                MOVE 0    TO DIG-1
           END-IF

           IF  (DIG-1 NOT = D13)
           OR  (DIG-2 NOT = D12)
                MOVE 1 TO RETORNO
           END-IF.

       050-99-FIM. EXIT.

       060-VALIDA-SP.

           IF   Y > 13
           OR   (Y < 12 AND Y > 0 AND W-D (1: Y) NUMERIC)
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   D1-R NOT NUMERIC
                COMPUTE SOMA = (D2 * 1) + (D3 * 3) + (D4 * 4) +
                               (D5 * 5) + (D6 * 6) + (D7 * 7) +
                               (D8 * 8) + (D9 * 10)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              MOVE RESTO TO DIG-1
              IF   DIG-1-4 NOT = D10
                   MOVE 1 TO RETORNO
              ELSE
                   IF   D1-R NOT = "P"
                        MOVE 1 TO RETORNO
                   END-IF
              END-IF
           ELSE
              COMPUTE SOMA = (D1 * 1) + (D2 * 3) + (D3 * 4) + (D4 * 5)
                           + (D5 * 6) + (D6 * 7) + (D7 * 8) + (D8 * 10)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              MOVE RESTO TO DIG-1
              COMPUTE SOMA = (D1 * 3) + (D2 * 2) + (D3 * 10) +
                             (D4 * 9) + (D5 * 8) + (D6 *  7) +
                             (D7 * 6) + (D8 * 5) + (D9 *  4) +
                              (D10 * 3) + (D11 * 2)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              MOVE RESTO TO DIG-2
              IF  (DIG-1-4 NOT = D9)
              OR  (DIG-2-4 NOT = D12)
                   MOVE 1 TO RETORNO
              END-IF
         END-IF.

       060-99-FIM. EXIT.

       070-VALIDA-AC.

           IF   Y <> 13
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (4 * D1) + (3 * D2)  + (2 * D3) + (9 * D4) +
                          (8 * D5) + (7 * D6)  + (6 * D7) + (5 * D8) +
                          (4 * D9) + (3 * D10) + (2 * D11)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-1
           ELSE
                COMPUTE DIG-1 = 11 - RESTO
           END-IF

           COMPUTE SOMA = (5 * D1) + (4 * D2)  + (3 * D3) + (2 * D4) +
                          (9 * D5) + (8 * D6)  + (7 * D7) + (6 * D8) +
                          (5 * D9) + (4 * D10) + (3 * D11) + (2 * DIG-1)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-2
           ELSE
                COMPUTE DIG-2 = 11 - RESTO
           END-IF

           IF  (DIG-1 NOT = D12)
           OR  (DIG-2 NOT = D13)
           OR  (D1    NOT = 0)
           OR  (D2    NOT = 1)
                MOVE 1 TO RETORNO
           END-IF.

       070-99-FIM. EXIT.

       080-VALIDA-AL.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           COMPUTE SOMA = SOMA * 10
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 10
                MOVE 0     TO DIG-1
           ELSE
                MOVE RESTO TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       080-99-FIM. EXIT.

       090-VALIDA-AP.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           STRING D1 D2 D3 D4 D5 D6 D7 D8 DELIMITED BY SIZE
                  INTO INSCRICAO-AP-GO

           EVALUATE TRUE
               WHEN INSCRICAO-AP-GO < 03000001
                    MOVE 1 TO RETORNO
               WHEN INSCRICAO-AP-GO < 03017001
                    MOVE 5     TO P
                    MOVE 0     TO D
               WHEN INSCRICAO-AP-GO < 03019023
                    MOVE 9     TO P
                    MOVE 1     TO D
               WHEN OTHER
                    MOVE 0     TO P
                                  D
           END-EVALUATE
           COMPUTE SOMA = P + (D1 * 9) + (D2 * 8) + (D3 * 7) +
                              (D4 * 6) + (D5 * 5) + (D6 * 4) +
                              (D7 * 3) + (D8 * 2)
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
           COMPUTE DIG-1 = 11 - RESTO
           IF   DIG-1 = 10
                MOVE 0     TO DIG-1
           ELSE
                IF   DIG-1 = 11
                     MOVE D TO DIG-1
                END-IF
           END-IF
           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       090-99-FIM. EXIT.

       100-VALIDA-AM.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           IF   SOMA < 11
                COMPUTE DIG-1 = 11 - SOMA
           ELSE
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                IF   RESTO <= 1
                     MOVE 0  TO DIG-1
                ELSE
                     COMPUTE DIG-1 = RESTO - 11
                END-IF
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       100-99-FIM. EXIT.

       110-VALIDA-CE.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           COMPUTE DIG-1 = RESTO - 11

           IF   DIG-1 = 10 OR 11
                MOVE 0 TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       110-99-FIM. EXIT.

       120-VALIDA-GO.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           STRING D1 D2 D3 D4 D5 D6 D7 D8 DELIMITED BY SIZE
                  INTO INSCRICAO-AP-GO

           IF  (D1 = 1 AND D2 = 0) OR (D1 = 1 AND D2 = 1) OR
               (D1 = 1 AND D2 = 5)
               COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                              (D4 * 6) + (D5 * 5) + (D6 * 4) +
                              (D7 * 3) + (D8 * 2)
               DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
               IF   RESTO = 0
                    MOVE 0 TO DIG-1
               ELSE
                  IF   RESTO = 1
                       IF  (INSCRICAO-AP-GO NOT < 10103105)
                       AND (INSCRICAO-AP-GO NOT > 10119997)
                           MOVE 1 TO DIG-1
                       ELSE
                           MOVE 0 TO DIG-1
                       END-IF
                  END-IF
               END-IF
               IF   RESTO > 1
                    COMPUTE DIG-1 = RESTO - 11
               END-IF
               IF   DIG-1 NOT = D9
                    MOVE 1 TO RETORNO
                    IF   INSCRICAO-AP-GO = 11094402
                         MOVE 0 TO RETORNO
                    END-IF
               END-IF
           ELSE
               MOVE 1 TO RETORNO
           END-IF.

       120-99-FIM. EXIT.

       130-VALIDA-MS.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   (D1 = 2 AND D2 = 8)
                COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                               (D4 * 6) + (D5 * 5) + (D6 * 4) +
                               (D7 * 3) + (D8 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                IF   RESTO = 0
                     MOVE 0 TO DIG-1
                ELSE
                     IF   RESTO > 0
                          COMPUTE DIG-1 = RESTO - 11
                          IF   DIG-1 > 9
                               MOVE 0 TO DIG-1
                          END-IF
                     END-IF
                END-IF
                IF   DIG-1 NOT = D9
                     MOVE 1 TO RETORNO
                END-IF
           ELSE
                MOVE 1 TO RETORNO
           END-IF.

       130-99-FIM. EXIT.

       140-VALIDA-PA.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   D1 = 1 AND D2 = 5
                COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                               (D4 * 6) + (D5 * 5) + (D6 * 4) +
                               (D7 * 3) + (D8 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                IF   RESTO = 0 OR 1
                     MOVE 0    TO DIG-1
                ELSE
                     COMPUTE DIG-1 = RESTO - 11
                END-IF
                IF   DIG-1 NOT = D9
                     MOVE 1 TO RETORNO
                END-IF
           ELSE
                MOVE 1 TO RETORNO
           END-IF.

       140-99-FIM. EXIT.

       150-VALIDA-MA.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   D1 NOT = 1 OR D2 NOT = 2
                MOVE 1 TO RETORNO
           ELSE
                COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                               (D4 * 6) + (D5 * 5) + (D6 * 4) +
                               (D7 * 3) + (D8 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                IF   RESTO = 0 OR 1
                     MOVE 0 TO DIG-1
                ELSE
                     COMPUTE DIG-1 = RESTO - 11
                END-IF
                IF   DIG-1 NOT = D9
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       150-99-FIM. EXIT.

       160-VALIDA-MT.

           IF   Y > 10
           OR   Y < 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           SUBTRACT 1 FROM CONTA-N

           IF      CONTA-N = 8
                   COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                                  (D4 * 6) + (D5 * 5) + (D6 * 4) +
                                  (D7 * 3) + (D8 * 2)
           END-IF
           IF      CONTA-N = 9
                   COMPUTE SOMA = (D1 * 2) + (D2 * 9) + (D3 * 8) +
                                  (D4 * 7) + (D5 * 6) + (D6 * 5) +
                                  (D7 * 4) + (D8 * 3) + (D9 * 2)
           END-IF
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0 TO DIG-1
           ELSE
                COMPUTE DIG-1 = RESTO - 11
           END-IF

           IF   CONTA-N = 8
                IF  DIG-1 NOT = D9
                    MOVE 1 TO RETORNO
                END-IF
           END-IF
           IF   CONTA-N = 9
                IF  DIG-1 NOT = D10
                    MOVE 1 TO RETORNO
                END-IF
           END-IF.

       160-99-FIM. EXIT.

       170-VALIDA-PB.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-1
           ELSE
                COMPUTE DIG-1 = RESTO - 11
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       170-99-FIM. EXIT.

       180-VALIDA-PR.

           IF   Y <> 10
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 3) + (D2 * 2) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           IF   RESTO = 0 OR 1
                MOVE 0    TO DIG-1
           ELSE
                COMPUTE DIG-1 = RESTO - 11
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           ELSE
                COMPUTE SOMA = (D1 * 4) + (D2 * 3) + (D3 * 2) +
                               (D4 * 7) + (D5 * 6) + (D6 * 5) +
                               (D7 * 4) + (D8 * 3) + (D9 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                IF   RESTO = 0 OR 1
                     MOVE 0    TO DIG-1
                ELSE
                     COMPUTE DIG-1 = RESTO - 11
                END-IF
                IF   DIG-1 NOT = D10
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       180-99-FIM. EXIT.

       190-VALIDA-PE.

           IF  (Y <> 14)
           AND (Y <> 9)
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF Y = 14
              COMPUTE SOMA = (D1 * 5) + (D2 * 4) + (D3 * 3) + (D4 * 2) +
                             (D5 * 1) + (D6 * 9) + (D7 * 8) + (D8 * 7) +
                             (D9 * 6) + (D10 * 5) + (D11 * 4) +
                             (D12 * 3) + (D13 * 2)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              COMPUTE DIG-1 = RESTO - 11
              IF   DIG-1 > 9
                   COMPUTE DIG-1 = DIG-1 - 10
              END-IF
              IF   DIG-1 NOT = D14
                   MOVE 1 TO RETORNO
              ELSE
                   MOVE D7  TO D1
                   MOVE D8  TO D2
                   MOVE D9  TO D3
                   MOVE D10 TO D4
                   MOVE D11 TO D5
                   MOVE D12 TO D6
                   MOVE D13 TO D7
                   MOVE 0   TO D8 D9 D10 D11 D12 D13 D14
                   COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) +
                                  (D4 * 5) + (D5 * 4) + (D6 * 3) +
                                  (D7 * 2)
                   DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                   COMPUTE DIG-1 = RESTO - 11
                   IF   DIG-1  >  09
                        MOVE 0     TO D8
                   ELSE
                        MOVE DIG-1 TO D8
                   END-IF
                   COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                                  (D4 * 6) + (D5 * 5) + (D6 * 4) +
                                  (D7 * 3) + (D8 * 2)
                   DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                   COMPUTE DIG-1 = RESTO - 11
                   IF   DIG-1  >  09
                        MOVE 0     TO D9
                   ELSE
                        MOVE DIG-1 TO D9
                   END-IF
              END-IF
           END-IF

           IF Y = 9
              COMPUTE SOMA = (D1 * 8) + (D2 * 7) + (D3 * 6) + (D4 * 5) +
                             (D5 * 4) + (D6 * 3) + (D7 * 2)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              COMPUTE DIG-1 = RESTO - 11
              IF   DIG-1  >  09
                   MOVE 0 TO DIG-1
              END-IF
              IF   DIG-1 NOT = D8
                   MOVE 1 TO RETORNO
              END-IF
              COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                             (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)
              DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
              COMPUTE DIG-1 = RESTO - 11
              IF   DIG-1  >  09
                   MOVE 0 TO DIG-1
              END-IF
              IF   DIG-1 NOT = D9
                   MOVE 2 TO RETORNO
              END-IF
           END-IF.

       190-99-FIM. EXIT.

       200-VALIDA-PI.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
           COMPUTE DIG-1 = RESTO - 11

           IF   DIG-1  >  09
                MOVE 0 TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       200-99-FIM. EXIT.

       210-VALIDA-RN.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           COMPUTE SOMA = SOMA * 10

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           MOVE RESTO TO DIG-1

           IF   DIG-1 >= 10
                MOVE 0 TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       210-99-FIM. EXIT.

       220-VALIDA-RS.

           IF   Y <> 10
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           STRING D1 D2 D3 DELIMITED BY SIZE
                  INTO INSCRICAO-RS

           IF   INSCRICAO-RS = 0
      *    OR   INSCRICAO-RS > 467
                MOVE 1 TO RETORNO
           ELSE
                COMPUTE SOMA = (D1 * 2) + (D2 * 9) + (D3 * 8) +
                               (D4 * 7) + (D5 * 6) + (D6 * 5) +
                               (D7 * 4) + (D8 * 3) + (D9 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                COMPUTE DIG-1 = RESTO - 11
                IF   DIG-1 NOT < 10
                     MOVE 0 TO DIG-1
                END-IF
                IF   DIG-1 NOT = D10
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       220-99-FIM. EXIT.

       230-VALIDA-RO.

           IF   NOT (Y = 9 OR 14)
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   Y = 9
                COMPUTE SOMA = (D4 * 6) + (D5 * 5) + (D6 * 4) +
                               (D7 * 3) + (D8 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                COMPUTE DIG-1 = RESTO - 11
                IF   DIG-1 >= 10
                     COMPUTE DIG-1 = DIG-1 - 10
                END-IF
                IF   DIG-1 NOT = D9
                     MOVE 1 TO RETORNO
                END-IF
                IF   RETORNO = 0
                     MOVE W-D (4: 5) TO W-D-RO
                     MOVE ZEROS      TO W-D
                     MOVE W-D-RO     TO W-D (9: 5)
                     COMPUTE SOMA = (D9 * 6) + (D10 * 5) + (D11 * 4) +
                                   (D12 * 3) + (D13 * 2)
                     DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                     COMPUTE DIG-1 = RESTO - 11
                     IF   DIG-1 >= 10
                          COMPUTE DIG-1 = DIG-1 - 10
                     END-IF
                     MOVE DIG-1 TO D14
                     MOVE 14    TO Y
                END-IF
           END-IF

           IF   Y = 14
                COMPUTE SOMA = (D1 * 6) +  (D2 * 5) +  (D3 * 4) +
                               (D4 * 3) +  (D5 * 2) +  (D6 * 9) +
                               (D7 * 8) +  (D8 * 7) +  (D9 * 6) +
                              (D10 * 5) + (D11 * 4) + (D12 * 3) +
                              (D13 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                COMPUTE DIG-1 = RESTO - 11
                IF   DIG-1 >= 10
                     COMPUTE DIG-1 = DIG-1 - 10
                END-IF
                IF   DIG-1 NOT = D14
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       230-99-FIM. EXIT.

       240-VALIDA-RR.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           STRING D1 D2 DELIMITED BY SIZE
                  INTO INSCRICAO-RR

           IF   INSCRICAO-RR NOT = 24
                MOVE 1 TO RETORNO
           ELSE
                COMPUTE SOMA = (D1 * 1) + (D2 * 2) + (D3 * 3) +
                               (D4 * 4) + (D5 * 5) + (D6 * 6) +
                               (D7 * 7) + (D8 * 8)
                DIVIDE SOMA BY 9 GIVING INTEIRO REMAINDER RESTO
                MOVE RESTO TO DIG-1
                IF   DIG-1 NOT = D9
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       240-99-FIM. EXIT.

       250-VALIDA-SC.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           COMPUTE DIG-1 = 11 - RESTO

           IF   DIG-1 > 9
                MOVE 0 TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       250-99-FIM. EXIT.

       260-VALIDA-SE.

           IF   Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) + (D4 * 6) +
                          (D5 * 5) + (D6 * 4) + (D7 * 3) + (D8 * 2)

           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO

           COMPUTE DIG-1 = RESTO - 11

           IF   DIG-1 >= 10
                MOVE 0 TO DIG-1
           END-IF

           IF   RESTO = 1
                MOVE 0 TO DIG-1
           END-IF

           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       260-99-FIM. EXIT.

       270-VALIDA-TO.

           IF   Y <> 11
           AND  Y <> 9
                MOVE 2 TO RETORNO
                GOBACK
           END-IF

           IF   Y =  9
                GO TO NOVO-TO
           END-IF

           STRING D3 D4 DELIMITED BY SIZE
                  INTO INSCRICAO-TO

           IF  (INSCRICAO-TO NOT = 01)
           AND (INSCRICAO-TO NOT = 02)
           AND (INSCRICAO-TO NOT = 03)
           AND (INSCRICAO-TO NOT = 99)
                MOVE 1 TO RETORNO
           ELSE
                COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D5 * 7) +
                               (D6 * 6) + (D7 * 5) + (D8 * 4) +
                               (D9 * 3) + (D10 * 2)
                DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
                COMPUTE DIG-1 = 11 - RESTO
                IF   RESTO < 2
                     MOVE 0 TO DIG-1
                ELSE
                     COMPUTE DIG-1 = 11 - RESTO
                END-IF
                IF   DIG-1 NOT = D11
                     MOVE 1 TO RETORNO
                END-IF
           END-IF.

       NOVO-TO.

           COMPUTE SOMA = (D1 * 9) + (D2 * 8) + (D3 * 7) +
                          (D4 * 6) + (D5 * 5) + (D6 * 4) +
                          (D7 * 3) + (D8 * 2)
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
           IF   RESTO < 2
                MOVE 0 TO DIG-1
           ELSE
                COMPUTE DIG-1 = 11 - RESTO
           END-IF
           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       270-99-FIM. EXIT.

       280-VALIDA-P.

           COMPUTE SOMA = (D1 * 1) + (D2 * 3) + (D3 * 4) + (D4 * 5)
                        + (D5 * 6) + (D6 * 7) + (D7 * 8) + (D8 * 10)
           DIVIDE SOMA BY 11 GIVING INTEIRO REMAINDER RESTO
           MOVE 0            TO DIG-1
           MOVE RESTO (4: 1) TO DIG-1 (4: 1)
           IF   DIG-1 NOT = D9
                MOVE 1 TO RETORNO
           END-IF.

       280-99-FIM. EXIT.

       900-EDIT-IE.

           MOVE IE0    TO W-DR

           EVALUATE UF
               WHEN "ES" STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "RJ" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "MG" STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 D9 "."
                                D10 D11 D12 D13
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "BA" IF CONTA-N = 8
                            STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8
                                   DELIMITED BY SIZE INTO IE-ED
                         ELSE
                            STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 D9
                                   DELIMITED BY SIZE INTO IE-ED
                         END-IF
               WHEN "MG" STRING D1 D2 D3 D4 D5 D6 D7 D8 D9 "-"
                                D10 D11 D12 D13
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "SP" IF   D1-R NOT NUMERIC
                              STRING D1-R "-" D2 D3 D4 D5 D6 D7 D8 D9
                                     D10 D11 D12 D13
                                     DELIMITED BY SIZE INTO IE-ED
                         ELSE
                              IF   D13 NOT NUMERIC
                                   STRING D1 D2 D3 "."
                                          D4 D5 D6 "."
                                          D7 D8 D9 "."
                                          D10 D11 D12
                                     DELIMITED BY SIZE INTO IE-ED
                              ELSE
                                     STRING D1 D2 D3 "."
                                            D4 D5 D6 D7 D8 D9 D10 "."
                                            D11 D12 D13
                                     DELIMITED BY SIZE INTO IE-ED
                              END-IF
                         END-IF
               WHEN "AC" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8 "/" D9
                                D10 D11 "-" D12 D13
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "AL" STRING D1 D2 "." D3 D4 "." D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "AP" STRING D1 D2 D3 D4 D5 D6 D7 D8 D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "AM" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "CE" STRING *>  D1 D2 "." D3 D4 "." D5 D6 D7 D8 "-" D9
                                D1 D2 "." D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "GO" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "MS" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "PA" STRING D1 D2 "." D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "MA" STRING D1 D2 "." D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "MT" IF  CONTA-N = 8
                             STRING D1 D2 "."
                                    D3 D4 D5 "."
                                    D6 D7 D8 "-"
                                    D9
                             DELIMITED BY SIZE INTO IE-ED
                         END-IF
                         IF  CONTA-N = 9
                             STRING D1 D2 D3 "."
                                    D4 D5 D6 "."
                                    D7 D8 D9 "-"
                                    D10
                             DELIMITED BY SIZE INTO IE-ED
                         END-IF
               WHEN "PB" STRING D1 D2 D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "PR" STRING D1 D2 D3 D4 D5 D6 D7 D8 "-" D9 D10
                                DELIMITED BY SIZE INTO IE-ED
      *        WHEN "PE" STRING D1 D2 "." D3 "." D4 D5 D6 "." D7 D8 D9
      *                         D10 D11 D12 D13 "-" D14
      *                         DELIMITED BY SIZE INTO IE-ED
               WHEN "PE" STRING D1 D2 D3 D4 D5 D6 D7 "-" D8 D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "DF" STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 "." D9
                                D10 D11 "-" D12 D13
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "PI" STRING D1 D2 D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "RN" STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "RS" STRING D1 D2 D3 "/" D4 D5 D6 D7 D8 D9 D10
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "RO" STRING D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12
                                D13 "-" D14
                                DELIMITED BY SIZE INTO IE-ED
      *                  STRING D1 D2 D3 "." D4 D5 D6 D7 D8 "-" D9
      *                         DELIMITED BY SIZE INTO IE-ED
               WHEN "RR" STRING D1 D2 "." D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "SC" STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "SE" STRING D1 D2 D3 D4 D5 D6 D7 D8 "-" D9
                                DELIMITED BY SIZE INTO IE-ED
               WHEN "TO" IF Y = 11
                            STRING D1 D2 D3 "." D4 D5 D6 "." D7 D8 D9
                                "-" D10 D11
                                DELIMITED BY SIZE INTO IE-ED
                         ELSE
                             STRING D1 D2 "." D3 D4 D5 "." D6 D7 D8
                             "-" D9
                                DELIMITED BY SIZE INTO IE-ED
                         END-IF
               WHEN "P-" STRING "P-" D1 D2 D3 D4 D5 D6 D7 D8
                                "." D9 "/" D10 D11 D12
                                DELIMITED BY SIZE INTO IE-ED
               WHEN OTHER
                                MOVE IE   TO      IE-ED
           END-EVALUATE.

       900-99-FIM. EXIT.
       END PROGRAM CWIEOK.
