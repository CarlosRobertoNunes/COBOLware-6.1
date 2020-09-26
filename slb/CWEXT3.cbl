       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEXT3.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/04/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Extenso em 3 colunas e separacao de silabas  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  I                           PIC 999              VALUE 0.
       77  IX-LC                       PIC 99               VALUE 0.
       77  IX-LS                       PIC 999              VALUE 0.
       77  WS-ERRO                     PIC 9                VALUE 0.
       77  WS-TOT                      PIC S9(07)   COMP-3  VALUE 0.
       77  TAM-NOME                    PIC S99              VALUE 0.
       77  TAM-PARC-LC                 PIC S99              VALUE 0.
       77  TAM-TOT-LC                  PIC 99               VALUE 0.
       77  TAM-TOT-LS                  PIC S999     COMP-3  VALUE 0.
       77  TAM-TOT-LINHAS              PIC S999     COMP-3  VALUE 0.
       77  MAX-LINHAS                  PIC S9               VALUE 3.
       77  MAX-SILABAS                 PIC S9               VALUE 5.
       77  SIN-DIF                     PIC 9                VALUE 0.
       77  MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
       77  MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       77  VIRGULA    PIC X(01) VALUE SPACE.

       01  WS-NOME                  PIC X(020).
       01  WS-TESTE                 PIC X(020).

       01  DIVERSOS.
           05 TAM-LINHAS.
              07 TAM-L1                PIC S99              VALUE 0.
              07 TAM-L2                PIC S99              VALUE 0.
              07 TAM-L3                PIC S99              VALUE 0.
           05 FILLER REDEFINES TAM-LINHAS.
              07 TAM-LINHA             PIC S99
                 OCCURS 3 TIMES INDEXED BY IX-TAM-LINHA.

           05 WS-VALOR                 PIC 9(12)V99.

       01 DIVERSOS3.
           05 TAB-DICIONARIO.
              07 FILLER                   PIC  X(44) VALUE
                 "um            um   2                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "dois          dois 4                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "tres          tres 4                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "quatro        qua  3tro  3                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "cinco         cin  3co   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "seis          seis 4                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "sete          se   2te   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "oito          oi   2to   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "nove          no   2ve   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "dez           dez  3                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "onze          on   2ze   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "doze          do   2ze   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "treze         tre  3ze   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "quatorze      qua  3tor  3ze   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "quinze        quin 4ze   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "dezesseis     de   2zes  3seis 4            ".
              07 FILLER                   PIC  X(44) VALUE
                 "dezessete     de   2zes  3se   2te   2      ".
              07 FILLER                   PIC  X(44) VALUE
                 "dezoito       de   2zoi  3to   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "dezenove      de   2ze   2no   2ve   2      ".
              07 FILLER                   PIC  X(44) VALUE
                 "vinte         vin  3te   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "trinta        trin 4ta   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "quarenta      qua  3ren  3ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "cinquenta     cin  3quen 4ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "sessenta      ses  3sen  3ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "setenta       se   2ten  3ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "oitenta       oi   2ten  3ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "noventa       no   2ven  3ta   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "cem           cem  3                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "cento         cen  3to   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "duzentos      du   2zen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "trezentos     tre  3zen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "quatrocentos  qua  3tro  3cen  3tos  3      ".
              07 FILLER                   PIC  X(44) VALUE
                 "quinhentos    qui  3nhen 4tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "seiscentos    seis 4cen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "setecentos    sete 4cen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "oitocentos    oito 4cen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "novecentos    nove 4cen  3tos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "mil           mil  3                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "milhao        mi   2lhao 4                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "milhoes       mi   2lhoes5                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "Real          Re   2al   2                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "Reais         Re   2ais  3                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "centavo       cen  3ta   2vo   2            ".
              07 FILLER                   PIC  X(44) VALUE
                 "centavos      cen  3ta   2vos  3            ".
              07 FILLER                   PIC  X(44) VALUE
                 "de            de   2                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "e             e    1                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "^             ^    1                        ".
              07 FILLER                   PIC  X(44) VALUE
                 "bilhao        bi   2lhao 4                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "bilhoes       bi   2lhoes5                  ".
              07 FILLER                   PIC  X(44) VALUE
                 "_             _    1                        ".
           05 FILLER REDEFINES TAB-DICIONARIO.
              07 TB-DICIONARIO OCCURS 50 TIMES INDEXED BY IX-DIC.
                 10 TBDI-PALAVRA          PIC X(14).
                 10 TBDI-SILABAS          OCCURS 5 TIMES
                                          INDEXED BY IX-SILABA.
                    15 TBDI-SILABA        PIC X(05).
                    15 TBDI-TAM-SILABA    PIC 9.

       01  SIN-LINHAS                     PIC X(240).

       COPY CWEXTE.

       LINKAGE SECTION.

       01  SIN-TUDO.
           05 SIN-VAL-TOT     PIC S9(12)V99 COMP-3.
           05 SIN-OPCAO       PIC X.
           05 SIN-ENCHEDOR    PIC X.
           05 SIN-LINHA-1     PIC X(099).
           05 SIN-TAM-L1      PIC 99.
           05 SIN-LINHA-2     PIC X(099).
           05 SIN-TAM-L2      PIC 99.
           05 SIN-LINHA-3     PIC X(099).
           05 SIN-TAM-L3      PIC 99.

       PROCEDURE DIVISION USING SIN-TUDO.
      *-----------------------------------*
       020-INICIO.
           MOVE ZEROS       TO TAM-TOT-LS
                               TAM-PARC-LC
                               TAM-L1
                               TAM-L2
                               TAM-L3
                               WS-ERRO
           MOVE SPACES      TO SIN-LINHAS
           MOVE SIN-VAL-TOT TO WS-VALOR

      *    IF   NOT (SIN-OPCAO EQUAL "M" OR "m")
      *         COMPUTE WS-VALOR = WS-VALOR * 100
      *    END-IF

           IF   SIN-TAM-L1 NOT EQUAL  0
                MOVE SIN-TAM-L1 TO TAM-L1
                IF   SIN-TAM-L2 NOT EQUAL 0
                     MOVE SIN-TAM-L2 TO TAM-L2
                     IF   SIN-TAM-L3 NOT EQUAL 0
                          MOVE SIN-TAM-L3 TO TAM-L3
                     END-IF
                END-IF
           END-IF

           COMPUTE TAM-TOT-LINHAS = TAM-L1 + TAM-L2 + TAM-L3
           SET  IX-TAM-LINHA TO 1
           MOVE TAM-L1       TO TAM-TOT-LC
           MOVE WS-VALOR     TO CWEXTE-VALOR-BASE
           CALL "CWEXTE"  USING PARAMETROS-CWEXTE

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF CWEXTE-VALOR-EXTENSO
                   IF   CWEXTE-VALOR-EXTENSO (I: 1) NOT = SPACE
                        MOVE SPACES TO WS-NOME
                        MOVE 0      TO TAM-NOME
                        PERFORM UNTIL CWEXTE-VALOR-EXTENSO(I: 1) = SPACE
                                ADD 1 TO TAM-NOME
                                MOVE CWEXTE-VALOR-EXTENSO (I: 1)
                                  TO WS-NOME (TAM-NOME: 1)
                                ADD 1 TO I
                        END-PERFORM
                        IF NOT (SIN-OPCAO EQUAL "M" OR "m")
                        AND (WS-NOME = "Reais" OR "Real")
                            MOVE SPACE TO VIRGULA
                        ELSE
                            MOVE SPACE TO VIRGULA
                            PERFORM 900-INSERE THRU 900-99-FIM
                            IF   VIRGULA = ","
                                 MOVE SPACE           TO VIRGULA
                                 MOVE    "^ "         TO WS-NOME
                                 MOVE    2            TO TAM-NOME
                                 PERFORM 900-INSERE THRU 900-99-FIM
                            ELSE
                                 MOVE    "_"          TO WS-NOME
                                 MOVE    1            TO TAM-NOME
                                 PERFORM 900-INSERE THRU 900-99-FIM
                            END-IF
                        END-IF
                     END-IF
           END-PERFORM

           PERFORM VARYING TAM-TOT-LS FROM 240 BY -1
                   UNTIL SIN-LINHAS(TAM-TOT-LS: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           PERFORM VARYING IX-LS FROM TAM-TOT-LS BY 1
                   UNTIL IX-LS > LENGTH OF SIN-LINHAS
                   MOVE SIN-ENCHEDOR TO SIN-LINHAS(IX-LS: 1)
           END-PERFORM

           GO TO 999-FIM.

       900-INSERE.

           MOVE WS-NOME TO WS-TESTE
           INSPECT WS-NOME CONVERTING "," TO " "
           IF   WS-NOME NOT = WS-TESTE
                MOVE "," TO VIRGULA
                SUBTRACT 1 FROM TAM-NOME WS-TOT
           END-IF

           COMPUTE WS-TOT = TAM-NOME + TAM-PARC-LC

           IF   WS-TOT GREATER TAM-TOT-LC
                PERFORM 970-NOVA-LINHA THRU 981-EXIT
           ELSE
                PERFORM 992-MONTA-LS THRU 995-S-MONTA-LS
           END-IF.

       900-99-FIM. EXIT.

       970-NOVA-LINHA.

           SET IX-DIC               TO 1.
           SEARCH TB-DICIONARIO   AT END
                  MOVE 1 TO WS-ERRO
                  GO TO 999-FIM
           WHEN TBDI-PALAVRA (IX-DIC) EQUAL WS-NOME NEXT SENTENCE.
           SET IX-SILABA              TO 1.

       980-SEPARA-SILABAS.

           IF   IX-SILABA GREATER MAX-SILABAS
                GO TO 981-EXIT
           ELSE
              IF   TBDI-SILABAS (IX-DIC, IX-SILABA) EQUAL SPACES
                   GO TO 981-EXIT
              END-IF
           END-IF

           COMPUTE WS-TOT = TBDI-TAM-SILABA (IX-DIC, IX-SILABA)
                          + TAM-PARC-LC.

           IF   WS-TOT LESS TAM-TOT-LC
                MOVE TBDI-TAM-SILABA (IX-DIC, IX-SILABA) TO TAM-NOME
                MOVE TBDI-SILABA (IX-DIC, IX-SILABA) TO WS-NOME
                PERFORM 992-MONTA-LS THRU 995-S-MONTA-LS
                SET IX-SILABA            UP BY 1
                GO TO 980-SEPARA-SILABAS
           END-IF

           IF IX-SILABA NOT = 1
              MOVE 1                  TO TAM-NOME
              MOVE "-"                TO WS-NOME
              PERFORM 992-MONTA-LS THRU 995-S-MONTA-LS
           END-IF

           COMPUTE SIN-DIF = TAM-TOT-LC - TAM-PARC-LC
           ADD  SIN-DIF               TO TAM-TOT-LS
           MOVE ZEROS                 TO TAM-PARC-LC
           SET IX-TAM-LINHA           UP BY 1

           IF   IX-TAM-LINHA NOT LESS MAX-LINHAS
           AND (TAM-LINHA (IX-TAM-LINHA) NOT EQUAL ZEROS)
                MOVE TAM-LINHA (IX-TAM-LINHA) TO TAM-TOT-LC
           END-IF

           GO TO 980-SEPARA-SILABAS.

       981-EXIT. EXIT.

       992-MONTA-LS.

           MOVE TAM-TOT-LS TO IX-LS
           MOVE 0          TO IX-LC.

       993-LOOP-LS.

           ADD 1 TO IX-LC.

           IF   IX-LC > TAM-NOME
                GO TO 994-ACTOT
           END-IF

           ADD  1                  TO IX-LS
           MOVE WS-NOME (IX-LC: 1) TO SIN-LINHAS(IX-LS: 1)
           GO TO 993-LOOP-LS.

       994-ACTOT.

           ADD TAM-NOME     TO TAM-TOT-LS
           ADD TAM-NOME     TO TAM-PARC-LC.

       995-S-MONTA-LS. EXIT.

       999-FIM.

           IF   WS-ERRO NOT EQUAL ZEROS
                MOVE SPACES TO SIN-LINHAS
           ELSE
                INSPECT SIN-LINHAS CONVERTING "_^" TO " ,"
           END-IF

           MOVE SPACES TO SIN-LINHA-1
                          SIN-LINHA-2
                          SIN-LINHA-3
           IF    TAM-L1 NOT = 0
                 MOVE 1                      TO I
                 MOVE SIN-LINHAS (I: TAM-L1) TO SIN-LINHA-1
                 ADD  TAM-L1  TO I
                 IF   TAM-L2 NOT = 0
                      IF   SIN-LINHAS (I: 1) = SPACE OR ","
                           ADD 1 TO I
                           IF   SIN-LINHAS (I: 1) = SPACE
                                ADD 1 TO I
                           END-IF
                      END-IF
                      MOVE SIN-LINHAS (I: TAM-L2) TO SIN-LINHA-2
                      ADD  TAM-L2                 TO I
                      IF   TAM-L3 NOT = 0
                           IF   SIN-LINHAS (I: 1) = SPACE OR ","
                                ADD 1 TO I
                                IF   SIN-LINHAS (I: 1) = SPACE
                                     ADD 1 TO I
                                END-IF
                           END-IF
                           MOVE SIN-LINHAS (I: TAM-L3) TO SIN-LINHA-3
                      END-IF
                 END-IF
           END-IF

           INSPECT SIN-LINHA-1 (1: 1)
                   CONVERTING MINUSCULAS TO MAIUSCULAS.

       1000-EXIT. EXIT PROGRAM.

       END PROGRAM CWEXT3.
