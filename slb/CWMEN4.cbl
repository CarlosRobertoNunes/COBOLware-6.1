       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN4.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/02/2005.
       SECURITY.      *************************************************
                      *                                               *
                      * Manuten‡ao de impressoras e estacoes do Grupo *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHOA-1. COPY CWCASE.
           05 TECLA2                   PIC  9(003) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 MSG                      PIC  X(080) VALUE SPACES.
           05 SZ                       PIC  9(002) VALUE 0.
           05 IMPRESSORA               PIC  X(025) VALUE SPACES.
           05 FILTRO.
              10 TIPO                  PIC  X(002) VALUE SPACES.
              10 G-ID                  PIC  X(005) VALUE SPACES.
           05 IMP.
              10                       PIC  X(032) VALUE "03".
              10                       PIC  X(005) VALUE "1".

       COPY CWCONF.
       COPY CWOBJE.

       LINKAGE SECTION.

       01  TECLA                       PIC  9(003). COPY CWKEYS.
       01  GRUPO-ID                    PIC  X(005).
       01  GRUPO                       PIC  X(022).

       PROCEDURE DIVISION USING TECLA GRUPO-ID GRUPO.

       000-INICIO.

           IF  GRUPO-ID NUMERIC
           AND GRUPO = SPACES
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               MOVE "GI"  TO TIPO PERFORM 020-REMOVER
               MOVE "GS"  TO TIPO PERFORM 020-REMOVER
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               GOBACK
           END-IF

           IF  GRUPO = SPACES
               GOBACK
           END-IF

           IF  F2
               MOVE "GI" TO TIPO
               MOVE 15   TO SZ
           ELSE
               MOVE "GS" TO TIPO
               MOVE 25   TO SZ
           END-IF
           MOVE TECLA TO TECLA2
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  GRUPO-ID NOT NUMERIC
               PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                       MOVE "GX"  TO CWCONF-REG
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-EQUAL TO TRUE
                       SET CWSQLC-LOCK TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF  FS-CWCONF = "23"
                           MOVE 0 TO CWCONF-GRUPO-SEQ
                           SET CWSQLC-WRITE TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                           SET CWSQLC-READ TO TRUE
                           SET CWSQLC-EQUAL TO TRUE
                           SET CWSQLC-LOCK TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                       END-IF
                       IF  FS-CWCONF = "9D"
                           CALL "CWCONF" USING "ISAM"
                       ELSE
                           ADD 1 TO CWCONF-GRUPO-SEQ
                           SET CWSQLC-REWRITE TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                           MOVE CWCONF-GRUPO-SEQ TO GRUPO-ID
                           SET CWSQLC-UNLOCK TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                       END-IF
               END-PERFORM
           END-IF
           MOVE GRUPO-ID TO G-ID
           EXEC COBOLware BoxWindow OPEN
                Line 07 COLUMN 08
                HEIGHT 15 WIDTH 47
                COLOR-FRAME 112
                COLOR-BORDER 112
           END-EXEC
           MOVE SPACES TO MSG
           IF  TIPO = "GI"
               STRING " Impressoras do grupo "
                       GRUPO DELIMITED BY SIZE INTO MSG
           ELSE
               STRING " Esta‡äes do grupo "
                       GRUPO DELIMITED BY SIZE INTO MSG
           END-IF
           PERFORM VARYING I FROM 80 BY -1 UNTIL MSG(I: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           ADD 1 TO I
           DISPLAY MSG LINE 7 COLUMN 10 WITH SIZE I
           PERFORM 010-PRIMEIRA
           PERFORM TEST AFTER UNTIL ESC
                   EXEC COBOLware Object LIST-BOX
                        LINE 09 COLUMN 11 HEIGHT 13 WIDTH SZ
                        LEFT-WIDTH SZ RETURN-LEFT ORDER-LEFT
                        OPTION IMPRESSORA
                        PROGRAM "CWMENF" WORK-AREA FILTRO
                   END-EXEC
                   EXEC COBOLware OBJECT PUSH-BUTTON
                        LINE 10 COLUMN 43 WIDTH 11
                        CAPTION "~Adcionar"
                        KEY F2
                   END-EXEC
                   EXEC COBOLware OBJECT PUSH-BUTTON
                        LINE 13 COLUMN 43 WIDTH 11
                        CAPTION "~Remover"
                        KEY F3
                   END-EXEC
                   EXEC COBOLware OBJECT PUSH-BUTTON
                        LINE 16 COLUMN 43 WIDTH 11
                        CAPTION "~Fechar"
                        KEY ESC
                   END-EXEC
                   DISPLAY IMPRESSORA
                          LINE 09 COLUMN 11 WITH SIZE SZ
                   ACCEPT IMPRESSORA
                          LINE 09 COLUMN 11 WITH SIZE SZ UPDATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   EXEC COBOLware Object
                        Drop
                   END-EXEC
                   EVALUATE TRUE
                       WHEN F2
                            IF TIPO = "GS"
                               EXEC COBOLware BoxDialog
                                    Header "Incluir esta‡Æo"
                                    Caption(1) "Esta‡Æo"
                                    Size(1) SZ
                                    Data(1) SPACES;CWCONF-IMPRESSORA
                               END-EXEC
                            END-IF
                            IF TIPO = "GI"
                               EXEC COBOLware BOXFile
                                    HEADER "Impressoras"
                                    LINE 08 COLUMN 33
                                    HEIGHT 08 WIDTH 15
                                    PROGRAM "CWMENF"
                                    OPTION SPACES;CWCONF-IMPRESSORA
                                    ORDER 1 RETURN 1
                                    STRING-1-LENGTH 15
                                    WORK-AREA IMP
                               END-EXEC
                            END-IF
                            IF  CWCONF-IMPRESSORA NOT = SPACES
                                INSPECT CWCONF-IMPRESSORA
                                CONVERTING MINUSCULAS TO MAIUSCULAS
                                MOVE TIPO       TO CWCONF-TIPO
                                MOVE GRUPO-ID TO CWCONF-GU-ID
                                SET CWSQLC-READ TO TRUE
                                SET CWSQLC-EQUAL TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                                IF  FS-CWCONF = '23'
                                    SET CWSQLC-WRITE TO TRUE
                                    CALL "CWCONF" USING CWSQLC
                                                        CWCONF-REG
                                                        FS-CWCONF
                                                        KCO PCO
                                    PERFORM 010-PRIMEIRA
                                END-IF
                            END-IF
                       WHEN F3
                            MOVE TIPO     TO CWCONF-REGGI
                            MOVE GRUPO-ID TO CWCONF-GU-ID
                            MOVE IMPRESSORA TO CWCONF-IMPRESSORA
                            SET CWSQLC-READ TO TRUE
                            SET CWSQLC-EQUAL TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                            IF   FS-CWCONF < "10"
                                 SET CWSQLC-DELETE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                            END-IF
                            PERFORM 010-PRIMEIRA
                       WHEN ESC
                            SET CWSQLC-REWRITE TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                   END-EVALUATE
           END-PERFORM
           EXEC COBOLware BoxWindow CLOSE END-EXEC
           MOVE TECLA2 TO TECLA
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-FIM. EXIT PROGRAM.

       010-PRIMEIRA.

           MOVE SPACES   TO IMPRESSORA
           MOVE TIPO     TO CWCONF-REGGI
           MOVE GRUPO-ID TO CWCONF-GU-ID
           SET CWSQLC-START TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF < "10"
               SET CWSQLC-READ TO TRUE
               SET CWSQLC-NEXT TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF  FS-CWCONF < "10"
               AND TIPO = CWCONF-TIPO
               AND GRUPO-ID = CWCONF-GU-ID
                   MOVE CWCONF-IMPRESSORA TO IMPRESSORA
               END-IF
           END-IF.

       010-99-FIM. EXIT.

       020-REMOVER.

           MOVE TIPO     TO CWCONF-REGGI
           MOVE GRUPO-ID TO CWCONF-GU-ID
           SET CWSQLC-START TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM UNTIL FS-CWCONF > "09"
                      OR (TIPO NOT = CWCONF-TIPO)
               SET CWSQLC-READ TO TRUE
               SET CWSQLC-NEXT TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF  FS-CWCONF < "10"
               AND TIPO = CWCONF-TIPO
               AND GRUPO-ID = CWCONF-GU-ID
                   SET CWSQLC-DELETE TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
               END-IF
           END-PERFORM.

       020-99-FIM. EXIT.
       END PROGRAM CWMEN4.
