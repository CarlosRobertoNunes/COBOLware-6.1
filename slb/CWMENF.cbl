       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/07/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Apoio a CWBOXF                               *
                      *                                               *
                      *  para a tabela dependendo do TIPO(WORK-AREA)  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                   PIC  9(006) VALUE 0.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 IGNORA              PIC  9(001) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 MEU-GRUPO           PIC  X(022) VALUE SPACES.

       COPY CWCONF.

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                            VALUE "O" "o".
           88 CLOSE-FILE                           VALUE "C" "c".
           88 BEGIN-FILE                           VALUE "B" "b".
           88 END-FILE                             VALUE "E" "e".
           88 AT-END                               VALUE "*".
           88 READ-NEXT                            VALUE "N" "n".
           88 READ-PREVIOUS                        VALUE "P" "p".
           88 NOT-LESS                             VALUE ">".
           88 NOT-GREATER                          VALUE "<".
           88 EDIT-KEY                             VALUE "$".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(080).
       01  STRING-2                           PIC  X(080).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA.
           05 TIPO                            PIC  X(002).
           05 RESTO                           PIC  X(048).
           05 FILLER REDEFINES RESTO.
              10 ELEMENTO                     PIC  X(030).
              10 CLASSE                       PIC  9(001).
              10 FILLER                       PIC  X(001).
                 88 SKIP-IRRESTRITO VALUE '='.
              10 FILLER                       PIC  X(016).
       01  CWBOXF-EDIT                        PIC  9(003).
       01  CWBOXF-OPTION                      PIC  X(076).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA
                                        CWBOXF-EDIT CWBOXF-OPTION.

       000-INICIO.

           PERFORM TEST AFTER UNTIL (FS-CWCONF NOT = "99")
                                AND (FS-CWCONF NOT = "9D")
           EVALUATE TRUE
               WHEN EDIT-KEY
                    CONTINUE *> Tecla de funçao em CWBOXF-EDIT
      *             exec cobolware
      *             send message "Tecla de edi‡Æo" end-exec
      *             perform varying i from 1 by 1 until i > 10000
      *                     display i at 0310
      *             end-perform
               WHEN OPEN-FILE
                    EXEC COBOLware GetSystem
                         Group;MEU-GRUPO
                    END-EXEC
                    SET CWSQLC-OPEN TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF   FS-CWCONF = "41"
                         SET CWSQLC-CLOSE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         SET CWSQLC-OPEN TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                    END-IF
                    IF   FS-CWCONF > "09"
                         CALL "CWCONF" USING "ISAM"
                    END-IF
                    IF   WORK-AREA = "03"
                         MOVE "03Spool" TO CWCONF-REG03
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
=>                       SET CWSQLC-IGNORE-LOCK TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         IF  FS-CWCONF = "00"
                             SET CWSQLC-CLOSE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-UPDATE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-DELETE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-OPEN TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                         END-IF
                         MOVE "03<Spool>" TO CWCONF-REG03
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
=>                       SET CWSQLC-IGNORE-LOCK TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         IF  FS-CWCONF = "23"
                             SET CWSQLC-CLOSE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-UPDATE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-WRITE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-OPEN TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                         END-IF
                         MOVE "03<Default>" TO CWCONF-REG03
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
=>                       SET CWSQLC-IGNORE-LOCK TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         IF  FS-CWCONF = "23"
                             SET CWSQLC-CLOSE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-UPDATE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-WRITE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             SET CWSQLC-OPEN TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                         END-IF
                    END-IF
                    MOVE TIPO     TO CWCONF-REG
                    MOVE SPACES   TO CWCONF-ELEMENTO
                    IF  TIPO = "GI" OR "GS"
                        MOVE ELEMENTO(1: 5) TO CWCONF-GU-ID
                    END-IF
                    SET CWSQLC-START TO TRUE
                    SET CWSQLC-NOT-LESS TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                                  OR IGNORA = 0
                                    MOVE SPACES TO CWCONF-REG
                                    SET CWSQLC-READ TO TRUE
                                    SET CWSQLC-NEXT TO TRUE
                                    SET CWSQLC-IGNORE-LOCK TO TRUE
                                    CALL "CWCONF" USING CWSQLC
                                                        CWCONF-REG
                                                        FS-CWCONF
                                                        KCO PCO
                                    IF   FS-CWCONF < "10"
                                         PERFORM CHECK-IGNORA
                                    END-IF
                            END-PERFORM
                            IF   FS-CWCONF < "10"
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
                    SET CWSQLC-CLOSE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN BEGIN-FILE
                    MOVE TIPO TO CWCONF-REG
                    IF  TIPO = "GI" OR "GS"
                        MOVE ELEMENTO(1: 5) TO CWCONF-GU-ID
                    END-IF
                    SET CWSQLC-START TO TRUE
                    SET CWSQLC-NOT-LESS TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CWCONF-REG
                    MOVE TIPO       TO CWCONF-REG (1: 2)
                    IF  TIPO = "GI" OR "GS"
                        MOVE ELEMENTO(1: 5) TO CWCONF-GU-ID
                    END-IF
                    SET CWSQLC-START TO TRUE
                    SET CWSQLC-NOT-GREATER TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN READ-NEXT
                    PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                          OR IGNORA = 0
                            MOVE SPACES TO CWCONF-REG
                            SET CWSQLC-READ TO TRUE
                            SET CWSQLC-NEXT TO TRUE
                            SET CWSQLC-IGNORE-LOCK TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                            IF   FS-CWCONF < "10"
                                 PERFORM CHECK-IGNORA
                            ELSE
                                 SET AT-END TO TRUE
                            END-IF
                    END-PERFORM
               WHEN READ-PREVIOUS
                    PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                          OR IGNORA = 0
                            MOVE SPACES TO CWCONF-REG
                            SET CWSQLC-READ TO TRUE
                            SET CWSQLC-PREVIOUS TO TRUE
                            SET CWSQLC-IGNORE-LOCK TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                            IF   FS-CWCONF < "10"
                                 PERFORM CHECK-IGNORA
                            ELSE
                                 SET AT-END TO TRUE
                            END-IF
                    END-PERFORM
               WHEN NOT-LESS
                    MOVE TIPO     TO CWCONF-REG
                    MOVE STRING-1 TO CWCONF-ELEMENTO
                    IF  TIPO = "GI" OR "GS"
                        MOVE ELEMENTO(1: 5) TO CWCONF-GU-ID
                        MOVE STRING-1       TO CWCONF-IMPRESSORA
                    END-IF
                    SET CWSQLC-START TO TRUE
                    SET CWSQLC-NOT-LESS TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF FS-CWCONF > "09"
                       SET AT-END TO TRUE
                    END-IF
               WHEN NOT-GREATER
                    MOVE TIPO     TO CWCONF-REG
                    MOVE STRING-1 TO CWCONF-ELEMENTO
                    IF  TIPO = "GI" OR "GS"
                        MOVE ELEMENTO(1: 5) TO CWCONF-GU-ID
                        MOVE STRING-1       TO CWCONF-IMPRESSORA
                    END-IF
                    SET CWSQLC-START TO TRUE
                    SET CWSQLC-NOT-GREATER TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    IF FS-CWCONF > "09"
                       SET AT-END TO TRUE
                    END-IF
           END-EVALUATE
           END-PERFORM

           MOVE CWCONF-ELEMENTO TO STRING-1
           IF  TIPO = "GI" OR "GS"
               MOVE CWCONF-ELEMENTO (6:) TO STRING-1
           END-IF
           IF AT-END OR FS-CWCONF > "09"
              MOVE SPACES TO STRING-1
           END-IF
           GOBACK.

       CHECK-IGNORA.

           MOVE 0 TO IGNORA
           EVALUATE TRUE
               WHEN CWCONF-TIPO NOT = TIPO
                    MOVE "10" TO FS-CWCONF
                    IF   NOT OPEN-FILE
                         SET AT-END TO TRUE
                    END-IF
               WHEN (CWCONF-TIPO = "GI" OR "GS")
                AND (CWCONF-GU-ID NOT = ELEMENTO (1: 5))
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "GU"
                AND (CWCONF-PROG-GRUPO NOT = SPACE)
                AND (CWCONF-PROG-GRUPO NOT = LOW-VALUES)
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "GU"
                AND CLASSE < 5
      *         AND CWCONF-NOME-GRUPO (1: 7) = "Acesso "
                AND (CWCONF-NOME-GRUPO NOT = MEU-GRUPO)
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "GU"
                AND SKIP-IRRESTRITO
                AND CWCONF-NOME-GRUPO (1: 7) = "Acesso "
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "PS"
                AND ((CWCONF-NIVEL-PS NOT LESS CLASSE)
                    AND (CWCONF-NOME  NOT = ELEMENTO))
                AND (CLASSE NOT = "9")
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "94"
                AND (CWCONF-NAME-REPORT NOT = WORK-AREA (3: 23))
                    MOVE 1 TO IGNORA
               WHEN CWCONF-TIPO = "03"
                AND CLASSE = 1
                AND(CWCONF-ARQUIVO = "<Spool>"
                 OR CWCONF-ARQUIVO = "<Default>")
                    MOVE 1 TO IGNORA
           END-EVALUATE.

       FIM-CHECK-IGNORA. EXIT.

       END PROGRAM CWMENF.
