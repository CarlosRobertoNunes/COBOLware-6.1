       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWIAEF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/05/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *  Menu padrao para opcoes de cadastramento     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 CWRUN                   PIC  X(001) VALUE SPACE.
           05 ENCERRA                 PIC  X(001) VALUE SPACE.
           05 NIVEL                   PIC  9(001) VALUE ZERO.
           05 CRT-STATUS              PIC  X(003) VALUE SPACES.
           05 FLAGS.
              10 SEM-ALTERACAO        PIC  X(001) VALUE SPACE.
              10 SEM-CONSULTA         PIC  X(001) VALUE SPACE.
              10 SEM-EXCLUSAO         PIC  X(001) VALUE SPACE.
              10 SEM-INCLUSAO         PIC  X(001) VALUE SPACE.
           05 FLAGS-2.
              10 OFF-ALTERACAO        PIC  X(001) VALUE SPACE.
              10 OFF-CONSULTA         PIC  X(001) VALUE SPACE.
              10 OFF-EXCLUSAO         PIC  X(001) VALUE SPACE.
              10 OFF-INCLUSAO         PIC  X(001) VALUE SPACE.
           05 FUNCAO-X                PIC  9(002) COMP-X.
           05 Y                       PIC  9(001) VALUE ZERO.
           05 K                       PIC  9(001) VALUE ZERO.
           05 YT                      PIC  9(001) VALUE ZERO.
           05 OBS                     PIC  X(029) VALUE SPACES.
           05 TASK                    PIC  9(006) VALUE ZERO.
           05 PROGRAMA                PIC  X(008) VALUE "CWMENU".
           05 NOME                    PIC  X(030) VALUE "LOGON".
           05 SET-LOG                 PIC  X(001) VALUE "?".
           05 PROGRAMA-A              PIC  X(008) VALUE SPACES.
           05 NOME-A                  PIC  X(030) VALUE SPACES.
           05 GRUPO                   PIC  X(022) VALUE SPACES.
           05 FUNCAO-ANTERIOR         PIC  9(002) VALUE ZERO.
           05 MODO-MENU               PIC  9(001) VALUE 0.
           05 CWUSER-PROGRAM          PIC  X(030) VALUE SPACES.
           05 CWMENU-PROGRAM          PIC  X(030) VALUE SPACES.

       COPY CWSTAT.
       COPY CWBOXS.
       COPY CWSEND.
       COPY CWSPWS.
       COPY CWCONF.

       LINKAGE SECTION.

       01  FUNCAO                            PIC  X(001).
           88 FUNCAO-OK VALUE "A" "C" "E" "I".
           88 ALTERACAO VALUE "A".
           88 CONSULTA  VALUE "C".
           88 EXCLUSAO  VALUE "E".
           88 INCLUSAO  VALUE "I".

       PROCEDURE DIVISION USING FUNCAO.

       INICIO.

           DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWMENU-PROGRAM
           ACCEPT  CWMENU-PROGRAM   FROM ENVIRONMENT-VALUE
           CALL "CWUSERPGM" USING "G" CWUSER-PROGRAM
ZP    *    CALL "CWUSER" USING X"FF"
           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           CALL "CWCRTS" USING "G" CRT-STATUS
           EVALUATE TRUE
               WHEN CRT-STATUS = X"FFFFFF"
                    MOVE ENCERRA TO FUNCAO
                    GOBACK
               WHEN FUNCAO > X"00" AND FUNCAO < X"10"
                    MOVE FUNCAO TO FUNCAO-X (1: 1)
                    IF   FUNCAO-X NOT < 8
                         MOVE "*" TO OFF-INCLUSAO
                         SUBTRACT 8 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 4
                         MOVE "*" TO OFF-EXCLUSAO
                         SUBTRACT 4 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 2
                         MOVE "*" TO OFF-CONSULTA
                         SUBTRACT 2 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 1
                         MOVE "*" TO OFF-ALTERACAO
                         SUBTRACT 1 FROM FUNCAO-X
                    END-IF
                    GOBACK
               WHEN FUNCAO > X"10" AND FUNCAO < X"20"
                    MOVE FUNCAO TO FUNCAO-X (1: 1)
                    SUBTRACT 16 FROM FUNCAO-X
                    IF   FUNCAO-X NOT < 8
                         MOVE " " TO OFF-INCLUSAO
                         SUBTRACT 8 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 4
                         MOVE " " TO OFF-EXCLUSAO
                         SUBTRACT 4 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 2
                         MOVE " " TO OFF-CONSULTA
                         SUBTRACT 2 FROM FUNCAO-X
                    END-IF
                    IF   FUNCAO-X NOT < 1
                         MOVE " " TO OFF-ALTERACAO
                         SUBTRACT 1 FROM FUNCAO-X
                    END-IF
                    GOBACK
           END-EVALUATE
           COPY CWSPPD.
           INSPECT FUNCAO CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE "?"         TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG

           IF ((PROGRAMA NOT = PROGRAMA-A)
           OR  (NOME     NOT = NOME-A))
           AND (NOME     NOT = "DESENVOLVIMENTO (CWRUN)")
                MOVE SPACES TO FLAGS
                MOVE ZERO   TO FUNCAO-ANTERIOR
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                        MOVE "PS" TO CWCONF-REG
                        MOVE NOME TO CWCONF-NOME
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-EQUAL TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                        IF   FS-CWCONF = "9D"
                             CALL "CWCONF" USING "ISAM"
                        END-IF
                END-PERFORM
                IF   FS-CWCONF NOT EQUAL "00"
                     CALL "CWCONF" USING "ISAM"
                     MOVE "V" TO FUNCAO
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     EXIT PROGRAM
                END-IF
                MOVE CWCONF-MODO-MENU TO MODO-MENU
                MOVE CWCONF-GRUPO     TO GRUPO
                MOVE CWCONF-NIVEL-PS  TO NIVEL
                PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                        MOVE "GU"     TO CWCONF-REG
                        MOVE GRUPO    TO CWCONF-NOME
                        MOVE PROGRAMA TO CWCONF-PROG-GRUPO
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-EQUAL TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                        IF   FS-CWCONF = "9D"
                             CALL "CWCONF" USING "ISAM"
                        END-IF
                END-PERFORM
                IF   FS-CWCONF < "10"
                     MOVE CWCONF-ALTERACAO-GRUPO TO SEM-ALTERACAO
                     MOVE CWCONF-CONSULTA-GRUPO  TO SEM-CONSULTA
                     MOVE CWCONF-EXCLUSAO-GRUPO  TO SEM-EXCLUSAO
                     MOVE CWCONF-INCLUSAO-GRUPO  TO SEM-INCLUSAO
                     IF   CWCONF-ACESSO-GRUPO NOT = SPACE
                     AND  CWCONF-ADM = "1"
                          MOVE ALL "*" TO FLAGS
                     END-IF
                END-IF
                MOVE PROGRAMA TO PROGRAMA-A
                MOVE NOME     TO NOME-A
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   OFF-ALTERACAO = "*" MOVE "*" TO SEM-ALTERACAO END-IF
           IF   OFF-CONSULTA  = "*" MOVE "*" TO SEM-CONSULTA  END-IF
           IF   OFF-EXCLUSAO  = "*" MOVE "*" TO SEM-EXCLUSAO  END-IF
           IF   OFF-INCLUSAO  = "*" MOVE "*" TO SEM-INCLUSAO  END-IF

           IF   ALL "*" = FLAGS
                MOVE "V" TO FUNCAO
                GOBACK
           END-IF

           IF ((SEM-ALTERACAO NOT = SPACE) AND ALTERACAO)
           OR ((SEM-CONSULTA  NOT = SPACE) AND CONSULTA)
           OR ((SEM-EXCLUSAO  NOT = SPACE) AND EXCLUSAO)
           OR ((SEM-INCLUSAO  NOT = SPACE) AND INCLUSAO)
               MOVE SPACE TO FUNCAO
           END-IF

           IF   FUNCAO-OK
                EVALUATE TRUE
                    WHEN ALTERACAO
                         MOVE "Altera" TO OBS
                    WHEN CONSULTA
                         MOVE "Consulta" TO OBS
                    WHEN EXCLUSAO
                         MOVE "Exclui" TO OBS
                    WHEN INCLUSAO
                         MOVE "Inclui" TO OBS
                END-EVALUATE
                PERFORM CHECK-STATUS THRU FIM-CHECK-STATUS
                GOBACK
           END-IF

           MOVE "Fun‡äes"            TO CWBOXS-TITLE
           MOVE "^Selecione fun‡Æo:" TO CWSEND-MSG
           MOVE SPACES               TO CWBOXS-ITENS CWSEND-SCREENS
           MOVE 11                   TO CWBOXS-LINE
           MOVE 34                   TO CWBOXS-COLUMN
           MOVE 0                    TO Y

           IF   PROGRAMA = "CWMEN8"
           AND  NIVEL < 4
                MOVE "*" TO SEM-INCLUSAO
                            SEM-EXCLUSAO
           END-IF

           IF  SEM-ALTERACAO = SPACE
               ADD  1             TO Y
               MOVE "~Altera‡Æo " TO CWBOXS-TEXT   (Y)
                                     CWSEND-SCREEN (Y)
           END-IF

           IF  SEM-CONSULTA = SPACE
               ADD  1            TO Y
               MOVE "~Consulta  " TO CWBOXS-TEXT   (Y)
                                     CWSEND-SCREEN (Y)
           END-IF

           IF  SEM-EXCLUSAO = SPACE
               ADD  1            TO Y
               MOVE "~ExclusÆo  " TO CWBOXS-TEXT   (Y)
                                     CWSEND-SCREEN (Y)
           END-IF

           IF  SEM-INCLUSAO = SPACE
               ADD  1            TO Y
               MOVE "~InclusÆo  " TO CWBOXS-TEXT   (Y)
                                     CWSEND-SCREEN (Y)
           END-IF

               ADD  1             TO Y

           IF  (CWUSER-PROGRAM NOT = SPACES)
           AND (CWUSER-PROGRAM NOT = CWMENU-PROGRAM)
               MOVE "~Retornar " TO CWBOXS-TEXT   (Y)
                                    CWSEND-SCREEN (Y)
               MOVE "V"           TO ENCERRA
CYRO           ADD  1             TO Y
CYRO           MOVE "~Finalizar " TO CWBOXS-TEXT   (Y)
CYRO                                 CWSEND-SCREEN (Y)
           ELSE
               MOVE "~Finalizar " TO CWBOXS-TEXT   (Y)
                                     CWSEND-SCREEN (Y)
               DISPLAY 'CWRUN' UPON ENVIRONMENT-NAME
               ACCEPT   CWRUN  FROM ENVIRONMENT-VALUE
               IF  CWRUN NOT = '1'
                   ADD  1             TO Y
                   MOVE Y             TO YT
                   MOVE "~Menu"       TO CWBOXS-TEXT   (Y)
                                         CWSEND-SCREEN (Y)
               END-IF
               MOVE "F"           TO ENCERRA
           END-IF

           MOVE Y             TO K
           IF   FUNCAO-ANTERIOR = 0
                MOVE Y TO FUNCAO-ANTERIOR
           END-IF

           MOVE FUNCAO-ANTERIOR TO CWBOXS-OPTION CWSEND-OPTION

           IF   MODO-MENU < 2
                SET CWBOXS-TIMEOUT-ENABLE TO TRUE
                CALL "CWBOXS"     USING PARAMETROS-CWBOXS
           ELSE
                MOVE CWSEND-OPTION   TO CWBOXS-OPTION
                SET CWSEND-TIMEOUT-ENABLE TO TRUE
                DISPLAY 'CWIAEF'   UPON ENVIRONMENT-NAME
                DISPLAY 'ON'       UPON ENVIRONMENT-VALUE
                CALL "CWSEND"     USING PARAMETROS-CWSEND
                DISPLAY 'CWIAEF'   UPON ENVIRONMENT-NAME
                DISPLAY 'OFF'      UPON ENVIRONMENT-VALUE
                MOVE CWSEND-OPTION   TO CWBOXS-OPTION
           END-IF

           CALL "CWCRTS" USING "G" CRT-STATUS
           IF   CRT-STATUS = X"FFFFFF"
                MOVE ENCERRA TO FUNCAO
                GOBACK
           END-IF

           IF   CWBOXS-TIMEOUT-ON
           OR   CWSEND-TIMEOUT-ON
                MOVE YT TO CWBOXS-OPTION
           END-IF

           IF   CWBOXS-OPTION = 27
           OR   CWSEND-OPTION = 9
                MOVE ENCERRA      TO FUNCAO
                MOVE 0 TO CWBOXS-OPTION
                          CWSEND-OPTION
                GOBACK
           END-IF
           IF   CWBOXS-OPTION = 0
                MOVE "Volta menu" TO OBS
                MOVE K            TO CWBOXS-OPTION
                                     FUNCAO-ANTERIOR
                MOVE "V"          TO FUNCAO
                IF  (CWUSER-PROGRAM NOT = SPACES)
                AND (CWUSER-PROGRAM NOT = CWMENU-PROGRAM)
                     MOVE "Retorno" TO OBS
                END-IF
           ELSE
                MOVE CWBOXS-OPTION TO FUNCAO-ANTERIOR
                IF   MODO-MENU < 2
                     MOVE CWBOXS-TEXT   (CWBOXS-OPTION) TO OBS
                     MOVE CWBOXS-CHAR   (CWBOXS-OPTION) TO FUNCAO
                ELSE
                     MOVE CWSEND-SCREEN (CWBOXS-OPTION) (2: ) TO OBS
                                                                 FUNCAO
                END-IF
                INSPECT OBS CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF

           IF   FUNCAO = "M" OR 'R'
                MOVE "V" TO FUNCAO
           END-IF

           IF   FUNCAO-OK
                PERFORM CHECK-STATUS THRU FIM-CHECK-STATUS
           END-IF

           GOBACK.

       CHECK-STATUS.

           MOVE LOW-VALUES TO SP2-PD-DATA
           MOVE "CWMENU"   TO SP2-PD-NAME
           CALL SP2   USING SP2-GET-PANEL-DEF  SP2-PANEL-DEF
           MOVE SP2-PD-MSG-TEXT TO STATUS-DEF
           IF   CURDIR (36: 1) = SPACE
                SUBTRACT 60 FROM DLEN
           END-IF
           MOVE SPACES TO CURDIR (36: )
           STRING "/60/y/" OBS DELIMITED BY SPACE
                  INTO CURDIR (36: )
           MOVE STATUS-DEF  TO SP2-PD-MSG-TEXT
           CALL "CWRESE"
           CALL SP2      USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.

           MOVE LOW-VALUES      TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
           CALL "CWSPID" USING SP2-FD-ID "FInsert"
           MOVE 1               TO SP2-FD-FLD-NUM
                                   SP2-FD-TAB-NUM
           MOVE "e"             TO SP2-FD-CTRL-TYPE
           MOVE X"00"           TO SP2-FD-CTRL-TYPE
           MOVE 1               TO SP2-FD-PROG-LEN
                                   SP2-FD-MAX-LEN
                                   SP2-FD-INITIAL-LEN
                                   SP2-FD-VAR-LEN
           MOVE "a"             TO SP2-FD-PROG-CTRL
           MOVE "y"             TO SP2-FD-CURS-SKIP
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           MOVE SP2-FD-ID       TO SP2-CD-NEXT-FLD-ID
           move "k" to sp2-cd-wait-sw
           CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
           CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
           CALL "CWSPID" USING SP2-FD-ID "FDelete".

       FIM-CHECK-STATUS. EXIT.

       END PROGRAM CWIAEF.
