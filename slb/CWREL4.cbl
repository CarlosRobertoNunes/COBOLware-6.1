      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL4.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/06/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 4 - Apoio a CWBOXF para tabela de     *
                      *             Datanames                         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAMPOS.SEL.

       DATA DIVISION.
       FILE SECTION.

       COPY CAMPOS.FD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 RELATOR-RESTRICT    PIC  X(003) VALUE SPACES.
           05 ATT                 PIC  X(020)        VALUE SPACES.
           05 RELDIR              PIC  X(050) VALUE SPACES.
           05 ORDENADOR           PIC  9(001) VALUE 0.
           05 I                   PIC  9(001) VALUE 0.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-CAMPOS.
              10 FS-CAMPOS        PIC  X(002) VALUE "00".
              10 LB-CAMPOS        PIC  X(255) VALUE SPACES.

       COPY CWUNIX.
       COPY CWCONF.

       01  MAPA-ORDENACAO VALUE "0".
           05 COLUNAS-ORDENADAS        PIC 9(001).
           05 COLUNA-ORDENADA OCCURS 7 PIC X(030).

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                               VALUE "O" "o".
           88 CLOSE-FILE                              VALUE "C" "c".
           88 BEGIN-FILE                              VALUE "B" "b".
           88 END-FILE                                VALUE "E" "e".
           88 AT-END                                  VALUE "*".
           88 READ-NEXT                               VALUE "N" "n".
           88 READ-PREVIOUS                           VALUE "P" "p".
           88 NOT-LESS                                VALUE ">".
           88 NOT-GREATER                             VALUE "<".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(080).
       01  STRING-2                           PIC  X(080).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA.
           05 WORK-OPCAO                      PIC  X(001).
           05 PROVEDOR                        PIC  X(008).
           05 RELATORIO                       PIC  X(007).
           05 FILLER                          PIC  X(033).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA.
       DECLARATIVES.

       DECLARATIVES-CAMPOS SECTION.

           USE AFTER ERROR PROCEDURE ON CAMPOS.

       CHECK-CAMPOS-TRAVADO.

           IF   FS-CAMPOS = "99" OR "9D"
                CALL "CWISAM" USING ER-CAMPOS
           END-IF.

       END DECLARATIVES.

       000 SECTION.
       000-INICIO.

           CALL "CWREL9"   USING MAPA-ORDENACAO
           IF   VEZ = 1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                MOVE 2 TO VEZ
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "RL" TO CWCONF-CHAVE
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "10"
                     MOVE CWCONF-RELATOR TO RELDIR
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   RELDIR = SPACES
                     MOVE "relator" TO RELDIR
                END-IF
                STRING RELDIR    DELIMITED BY SPACE
                       "\COLUNAS" DELIMITED BY SIZE
                         INTO LB-CAMPOS
                INSPECT LB-CAMPOS CONVERTING "\" TO "/"
                DISPLAY "RELATOR-RESTRICT" UPON ENVIRONMENT-NAME
                ACCEPT RELATOR-RESTRICT    FROM ENVIRONMENT-VALUE
                IF   RELATOR-RESTRICT = SPACES
                     DISPLAY "RELATOR_RESTRICT" UPON ENVIRONMENT-NAME
                     ACCEPT RELATOR-RESTRICT    FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT RELATOR-RESTRICT CONVERTING "of" TO "OF"
           END-IF

           IF   RELATOR-RESTRICT = "OFF"
                MOVE SPACES TO WORK-OPCAO
           END-IF

           PERFORM TEST AFTER UNTIL (FS-CAMPOS NOT = "99")
                                AND (FS-CAMPOS NOT = "9D")
           EVALUATE TRUE
               WHEN OPEN-FILE
                    OPEN INPUT CAMPOS
                    IF   FS-CAMPOS > "09"
                         CALL "CWISAM" USING ER-CAMPOS
                    END-IF
                    MOVE 1         TO CAMPOS-COLUNA
                    MOVE 0         TO REGISTROS
                    MOVE PROVEDOR  TO CAMPOS-PROVEDOR
                    MOVE SPACES    TO CAMPOS-RELATORIO
                    START CAMPOS KEY NOT < CAMPOS-CHAVE
                    PERFORM TEST AFTER UNTIL FS-CAMPOS > "09"
                                          OR AT-END
                                          OR REGISTROS = VERTICAL-LENGTH
                            PERFORM LER-CAMPOS THRU LEU-CAMPOS
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CLOSE CAMPOS
               WHEN BEGIN-FILE
                    INITIALIZE CAMPOS-REG
                    MOVE PROVEDOR  TO CAMPOS-PROVEDOR
                    MOVE RELATORIO TO CAMPOS-RELATORIO
                    EVALUATE ORDER-X
                        WHEN 1
                             START CAMPOS KEY NOT < CAMPOS-CHAVE
                        WHEN 2
                             START CAMPOS KEY NOT < CAMPOS-PRONAMREL
                        WHEN OTHER
                             START CAMPOS KEY NOT < CAMPOS-CHAVE
                    END-EVALUATE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CAMPOS-REG
                    MOVE PROVEDOR   TO CAMPOS-PROVEDOR
                    MOVE RELATORIO  TO CAMPOS-RELATORIO
                    EVALUATE ORDER-X
                        WHEN 1
                             START CAMPOS KEY NOT > CAMPOS-CHAVE
                        WHEN 2
                             START CAMPOS KEY NOT > CAMPOS-PRONAMREL
                        WHEN OTHER
                             START CAMPOS KEY NOT > CAMPOS-CHAVE
                    END-EVALUATE
               WHEN READ-NEXT
                    PERFORM LER-CAMPOS THRU FIM-LER-CAMPOS
               WHEN READ-PREVIOUS
                    PERFORM LER-CAMPOS THRU FIM-LER-CAMPOS
               WHEN NOT-LESS
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CAMPOS-CHAVE
                             START CAMPOS KEY NOT < CAMPOS-CHAVE
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                        WHEN 2
                             MOVE STRING-2 TO CAMPOS-DATANAME
                             MOVE PROVEDOR TO CAMPOS-PROVEDOR
                             MOVE RELATORIO TO CAMPOS-RELATORIO
                             START CAMPOS KEY NOT < CAMPOS-PRONAMREL
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                    END-EVALUATE
               WHEN NOT-GREATER
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CAMPOS-CHAVE
                             START CAMPOS KEY NOT > CAMPOS-CHAVE
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                        WHEN 2
                             MOVE STRING-2  TO CAMPOS-DATANAME
                             MOVE PROVEDOR  TO CAMPOS-PROVEDOR
                             MOVE RELATORIO TO CAMPOS-RELATORIO
                             START CAMPOS KEY NOT > CAMPOS-PRONAMREL
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                    END-EVALUATE
           END-EVALUATE
           END-PERFORM

           MOVE CAMPOS-COLUNA   TO STRING-1
           MOVE CAMPOS-DATANAME TO STRING-2
           GOBACK.

       LER-CAMPOS.

           IF   FS-CAMPOS = '49' OR '23'
                SET AT-END TO TRUE
           END-IF
           IF   AT-END
                GO TO FIM-LER-CAMPOS
           END-IF

           IF   READ-NEXT OR OPEN-FILE
                READ CAMPOS NEXT RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO FIM-LER-CAMPOS
                END-READ
           ELSE
                READ CAMPOS PREVIOUS RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO FIM-LER-CAMPOS
                END-READ
           END-IF

           IF    PROVEDOR NOT = CAMPOS-PROVEDOR
                 SET AT-END TO TRUE
                 GO TO FIM-LER-CAMPOS
           END-IF

           IF   WORK-OPCAO = "E"
           AND  CAMPOS-DATANAME (1: 8) = "Relator-"
                GO TO LER-CAMPOS
           END-IF

           IF  (CAMPOS-RELATORIO NOT = SPACES)
           AND (RELATORIO NOT = CAMPOS-RELATORIO)
           OR   CAMPOS-COLUNA = 0
                GO TO LER-CAMPOS
           END-IF

           IF   WORK-OPCAO = "V"
                IF ((NOT CAMPOS-VALOR) AND (NOT CAMPOS-NUMERICO))
                OR   CAMPOS-ACUMULADOR = "+"
                OR   CAMPOS-DATANAME (1: 8) = "Relator-"
                     GO TO LER-CAMPOS
                END-IF
           END-IF

           IF   WORK-OPCAO = "O"
                IF   CAMPOS-DATANAME (1: 7) = "Prompt-"
                OR   CAMPOS-DATANAME (1: 8) = "Relator-"
                OR   CAMPOS-ACUMULADOR = "+"
                     GO TO LER-CAMPOS
                END-IF
           END-IF

           IF   WORK-OPCAO = "I"
                IF  (CAMPOS-DATANAME (1: 8) = "Relator-"
                AND (CAMPOS-DATANAME NOT = "Relator-DATA"))
                OR   CAMPOS-ACUMULADOR = "+"
                     GO TO LER-CAMPOS
                END-IF
           END-IF

           MOVE 0 TO ORDENADOR
           PERFORM VARYING I
                      FROM 1 BY 1
                      UNTIL I > COLUNAS-ORDENADAS
                   IF   COLUNA-ORDENADA (I) = CAMPOS-DATANAME
                        MOVE 1 TO ORDENADOR
                   END-IF
           END-PERFORM

           IF   WORK-OPCAO = "T"
                IF   CAMPOS-ACUMULADOR = "+"
                OR   CAMPOS-DATANAME (1: 8) = "Relator-"
                     GO TO FIM-LER-CAMPOS
                ELSE
                     GO TO LER-CAMPOS
                END-IF
           END-IF

           IF   WORK-OPCAO = "q"
                IF  (CAMPOS-VALOR
                AND  CAMPOS-ACUMULADOR = " ")
                     GO TO LER-CAMPOS
                ELSE
                     GO TO FIM-LER-CAMPOS
                END-IF
           END-IF

           IF   WORK-OPCAO = "C"
                IF  (CAMPOS-VALOR
                AND  CAMPOS-ACUMULADOR = "+")
                OR  (ORDENADOR = 0 AND (NOT CAMPOS-CAB))
                     GO TO LER-CAMPOS
                ELSE
                     GO TO FIM-LER-CAMPOS
                END-IF
           END-IF.

       FIM-LER-CAMPOS. EXIT.

           IF   FS-CAMPOS < "10"
           AND  OPEN-FILE
                ADD 1 TO REGISTROS
           END-IF.

       LEU-CAMPOS. EXIT.

       END PROGRAM CWREL4.
