      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL6.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/09/1999.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 6 - Programa de apoio a CWBOXF para   *
                      *             acesso a tabela de relatorios     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY FORMATOS.SEL.

       DATA DIVISION.
       FILE SECTION.

       COPY FORMATOS.FD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ADM                 PIC  X(001) VALUE SPACES.
           05 GRUPO               PIC  X(022) VALUE SPACES.
           05 NOME                PIC  X(030) VALUE "LOGON".
           05 TASK                PIC  9(006) VALUE ZERO.
           05 PROGRAMA            PIC  X(008) VALUE "CWMENU".
           05 ATT                 PIC  X(020) VALUE SPACES.
           05 RELDIR              PIC  X(255) VALUE SPACES.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-FORMATOS.
              10 FS-FORMATOS      PIC  X(002) VALUE "00".
              10 LB-FORMATOS      PIC  X(255) VALUE SPACES.
           05 SZ-FORMATOS         PIC  9(003) VALUE 0.

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

       DECLARATIVES-FORMATOS SECTION.

           USE AFTER ERROR PROCEDURE ON FORMATOS.

       CHECK-FORMATOS-TRAVADO.

           IF   (FS-FORMATOS NOT = "10")
           AND  (FS-FORMATOS NOT = "23")
           AND  (FS-FORMATOS NOT = "30")
           AND  (FS-FORMATOS NOT = "35")
           AND  (FS-FORMATOS NOT = "42")
           AND  (FS-FORMATOS NOT = "9)")
                CALL "CWISAM" USING ER-FORMATOS
           END-IF.

       END DECLARATIVES.

       000 SECTION.
       000-INICIO.

           IF   VEZ = 1
                MOVE 2           TO VEZ
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
                STRING RELDIR      DELIMITED BY SPACE
                       "\FORMATOS" DELIMITED BY SIZE
                              INTO LB-FORMATOS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF  CWUNIX-ON
                    INSPECT LB-FORMATOS CONVERTING "\" TO "/"
                END-IF
           END-IF

           PERFORM TEST AFTER UNTIL (FS-FORMATOS NOT = "99")
                                AND (FS-FORMATOS NOT = "9D")
           EVALUATE TRUE
               WHEN OPEN-FILE
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
                    PERFORM TEST AFTER UNTIL FS-FORMATOS NOT = "9)"
                            OPEN INPUT FORMATOS
                    END-PERFORM
                    IF   FS-FORMATOS = "30" OR "35"
                         OPEN OUTPUT FORMATOS
                         MOVE SPACES           TO FORMATOS-REG
                         MOVE "1"              TO FORMATOS-TIPO-REG
                         MOVE ALL X"00"        TO FORMATOS-REPORT
                         MOVE 0                TO FORMATOS-OBJETO
                         MOVE "Criar um novo relat¢rio"
                                               TO FORMATOS-DATANAME
                         MOVE 37               TO SZ-FORMATOS
                         WRITE FORMATOS-REG
                         CLOSE FORMATOS
                         OPEN INPUT FORMATOS
                    END-IF
                    IF   FS-FORMATOS > "09"
                         CALL "CWISAM" USING ER-FORMATOS
                    END-IF
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-FORMATOS > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            PERFORM LER-FORMATOS THRU FIM-LER-FORMATOS
                            IF   FS-FORMATOS < "10"
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
                    CLOSE FORMATOS
                    SET CWSQLC-CLOSE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN BEGIN-FILE
                    INITIALIZE FORMATOS-REG
                    START FORMATOS KEY NOT < FORMATOS-CHAVE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO FORMATOS-REG
                    START FORMATOS KEY NOT > FORMATOS-CHAVE
               WHEN READ-NEXT
                    PERFORM LER-FORMATOS THRU FIM-LER-FORMATOS
               WHEN READ-PREVIOUS
                    PERFORM LER-FORMATOS THRU FIM-LER-FORMATOS
               WHEN NOT-LESS
                    MOVE "1"      TO FORMATOS-CHAVE
                    MOVE STRING-1 TO FORMATOS-REPORT
                    START FORMATOS KEY NOT < FORMATOS-CHAVE
                          INVALID KEY
                                  SET AT-END TO TRUE
                    END-START
               WHEN NOT-GREATER
                    MOVE "1"      TO FORMATOS-CHAVE
                    MOVE STRING-1 TO FORMATOS-REPORT
                    START FORMATOS KEY NOT > FORMATOS-CHAVE
                          INVALID KEY
                                  SET AT-END TO TRUE
                    END-START
           END-EVALUATE
           END-PERFORM

           MOVE FORMATOS-REPORT        TO STRING-1
           MOVE FORMATOS-DATANAME      TO STRING-2
           INSPECT STRING-2 CONVERTING "_" TO SPACE
           GOBACK.

       LER-FORMATOS.

           IF   AT-END
                MOVE "10" TO FS-FORMATOS
                GO TO FIM-LER-FORMATOS
           END-IF

           IF   READ-NEXT OR OPEN-FILE
                READ FORMATOS NEXT RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO LER-FORMATOS
                END-READ
                IF    FORMATOS-TIPO-REG > "1"
                      SET AT-END TO TRUE
                      GO TO LER-FORMATOS
                END-IF
           ELSE
                READ FORMATOS PREVIOUS RECORD IGNORE LOCK
                    AT END
                       SET AT-END TO TRUE
                       GO TO LER-FORMATOS
                END-READ
           END-IF

           IF   WORK-AREA (1: 1) = "R"
           AND  FORMATOS-REPORT = LOW-VALUES
                GO TO LER-FORMATOS
           END-IF

           IF   GRUPO NOT = SPACES
                MOVE "GU"              TO CWCONF-REGGU
                MOVE GRUPO             TO CWCONF-NOME-GRUPO
                MOVE FORMATOS-PROVEDOR TO CWCONF-PROG-GRUPO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF  (FS-CWCONF < "09"
                AND  CWCONF-ACESSO-GRUPO NOT = SPACE
                AND  ADM                 NOT = "I")
                OR  (FS-CWCONF = "23"
                AND  ADM                     = "I")
                     GO TO LER-FORMATOS
                END-IF
           END-IF.

       FIM-LER-FORMATOS. EXIT.

       END PROGRAM CWREL6.
