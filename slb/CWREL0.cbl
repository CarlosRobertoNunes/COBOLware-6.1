      $Set CALLFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL0.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/08/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 10 - Apoio a CWBOXF para tabela de    *
                      *             Provedores                        *
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
           05 ADM                 PIC  X(001) VALUE SPACES.
           05 GRUPO               PIC  X(022) VALUE SPACES.
           05 NOME                PIC  X(030) VALUE "LOGON".
           05 TASK                PIC  9(006) VALUE ZERO.
           05 PROGRAMA            PIC  X(008) VALUE "CWMENU".
           05 ATT                 PIC  X(020) VALUE SPACES.
           05 RELDIR              PIC  X(255) VALUE SPACES.
           05 I                   PIC  9(001) VALUE 0.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-CAMPOS.
              10 FS-CAMPOS        PIC  X(002) VALUE "00".
              10 LB-CAMPOS        PIC  X(255) VALUE SPACES.

       COPY CWUNIX.
       COPY CWCONF.

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
       01  STRING-1                           PIC  X(040).
       01  STRING-2                           PIC  X(040).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA                          PIC  X(050).

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
           END-IF

           PERFORM TEST AFTER UNTIL (FS-CAMPOS NOT = "99")
                                AND (FS-CAMPOS NOT = "9D")
           EVALUATE TRUE
               WHEN OPEN-FILE
                    OPEN INPUT CAMPOS
                    IF   FS-CAMPOS > "09"
                         CALL "CWISAM" USING ER-CAMPOS
                    END-IF
                    SET CWSQLC-OPEN TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF   FS-CWCONF NOT EQUAL "00"
                         CALL "CWCONF" USING "ISAM"
                    END-IF
                    CALL "CWGETU" USING NOME TASK PROGRAMA "?"
                    MOVE "PS" TO CWCONF-REGPS
                    MOVE NOME TO CWCONF-NOME
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF   FS-CWCONF = "23"
                    OR   CWCONF-GRUPO = SPACES
                    OR   CWCONF-GRUPO = "Acesso sem restri‡”es"
                    OR   CWCONF-GRUPO = "Acesso sem restri‡äes"
                    OR   CWCONF-GRUPO = "Acesso sem restricoes"
                    OR   CWCONF-GRUPO = "Acesso irrestrito"
                         MOVE SPACES TO GRUPO
                    ELSE
                         MOVE CWCONF-GRUPO    TO GRUPO
                         MOVE "GU"            TO CWCONF-REG
                         MOVE GRUPO           TO CWCONF-NOME-GRUPO
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
                         SET CWSQLC-IGNORE-LOCK TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         IF   FS-CWCONF < "09"
                              MOVE CWCONF-ADM TO ADM
                         ELSE
                              MOVE SPACES TO GRUPO
                         END-IF
                    END-IF
                    INITIALIZE CAMPOS-REG
                    START CAMPOS KEY NOT < CAMPOS-CHAVE
                    PERFORM TEST AFTER UNTIL FS-CAMPOS > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            PERFORM LER-CAMPOS THRU FIM-LER-CAMPOS
                            IF   FS-CAMPOS < "10"
                                 ADD 1 TO REGISTROS
                            END-IF
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CLOSE CAMPOS
                    SET CWSQLC-CLOSE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN BEGIN-FILE
                    INITIALIZE CAMPOS-REG
                    START CAMPOS KEY NOT < CAMPOS-CHAVE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CAMPOS-REG
                    START CAMPOS KEY NOT > CAMPOS-CHAVE
               WHEN READ-NEXT
                    PERFORM LER-CAMPOS THRU FIM-LER-CAMPOS
               WHEN READ-PREVIOUS
                    PERFORM LER-CAMPOS THRU FIM-LER-CAMPOS
               WHEN NOT-LESS
                    MOVE STRING-1 TO CAMPOS-PROVEDOR
                    MOVE 0        TO CAMPOS-COLUNA
                    START CAMPOS KEY NOT < CAMPOS-CHAVE
                          INVALID KEY SET AT-END TO TRUE
                    END-START
                    MOVE SPACES TO CAMPOS-PROVEDOR
               WHEN NOT-GREATER
                    MOVE STRING-1  TO CAMPOS-PROVEDOR
                    MOVE 9999      TO CAMPOS-COLUNA
                    START CAMPOS KEY NOT > CAMPOS-CHAVE
                          INVALID KEY SET AT-END TO TRUE
                    END-START
                    MOVE SPACES TO CAMPOS-PROVEDOR
           END-EVALUATE
           END-PERFORM

           MOVE CAMPOS-PROVEDOR TO STRING-1
           MOVE CAMPOS-MASCARA  TO STRING-2
           GOBACK.

       LER-CAMPOS.

           IF   AT-END
                GO TO FIM-LER-CAMPOS
           END-IF

           IF   READ-NEXT OR OPEN-FILE
                READ CAMPOS NEXT RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO FIM-LER-CAMPOS
                END-READ
                IF   CAMPOS-COLUNA NOT = 0
                     MOVE 9999 TO CAMPOS-COLUNA
                     START CAMPOS KEY NOT < CAMPOS-CHAVE
                           INVALID KEY SET AT-END TO TRUE
                           GO TO FIM-LER-CAMPOS
                     END-START
                     GO TO LER-CAMPOS
                END-IF
           ELSE
                READ CAMPOS PREVIOUS RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO FIM-LER-CAMPOS
                END-READ
                IF   CAMPOS-COLUNA NOT = 0
                     MOVE 0000 TO CAMPOS-COLUNA
                     START CAMPOS KEY NOT > CAMPOS-CHAVE
                           INVALID KEY SET AT-END TO TRUE
                           GO TO FIM-LER-CAMPOS
                     END-START
                     GO TO LER-CAMPOS
                END-IF
           END-IF
           IF   GRUPO NOT = SPACES
                MOVE "GU"              TO CWCONF-REGGU
                MOVE GRUPO             TO CWCONF-NOME-GRUPO
                MOVE CAMPOS-PROVEDOR   TO CWCONF-PROG-GRUPO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF  (FS-CWCONF < "09"
                AND  CWCONF-ACESSO-GRUPO NOT = SPACE
                AND  ADM                 NOT = "I")
                OR  (FS-CWCONF = "23"
                AND  ADM                     = "I")
                     GO TO LER-CAMPOS
                END-IF
           END-IF.

       FIM-LER-CAMPOS. EXIT.

       END PROGRAM CWREL0.
