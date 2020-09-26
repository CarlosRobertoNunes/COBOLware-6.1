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

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
frango     05 JANPOS.
frango        10 JANLIN               PIC  9(002) VALUE 0.
frango        10 JANCOL               PIC  9(002) VALUE 0.
           05 cwframe                 PIC  X(018) VALUE SPACES.
           05 FROMGUI                 PIC  X(006) VALUE SPACE.
           05 CWRUN                   PIC  X(001) VALUE SPACE.
           05 STOPRUN                 PIC  9(001) VALUE 0.
           05 AJUSTE                  PIC  X(001) VALUE SPACE.
           05 CWIAEF-DISPLAY          PIC  X(003) VALUE SPACE.
           05 LEN                     PIC  9(002) VALUE ZERO.
           05 NIVEL                   PIC  9(001) VALUE ZERO.
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
           05 OBSR                    PIC  X(029) VALUE SPACES.
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

       COPY CWBOXS.
       COPY CWBOXW.
       COPY CWLINE.
       COPY CWNCOR.
       COPY CWCONF.

       LINKAGE SECTION.

       01  FUNCAO                            PIC  X(001).
           88 FUNCAO-OK VALUE "A" "C" "E" "I".
           88 ALTERACAO VALUE "A".
           88 CONSULTA  VALUE "C".
           88 EXCLUSAO  VALUE "E".
           88 INCLUSAO  VALUE "I".
           88 MOLDURA   VALUE X"CD" X"C4" X"CD" X"C4" X"B0" X"B1" X"B2"
                              X"2D" X"24".

       PROCEDURE DIVISION USING FUNCAO.

       INICIO.

           ON 1
              DISPLAY "CWFRAMEPGM" UPON ENVIRONMENT-NAME
              ACCEPT   CWFRAME  FROM ENVIRONMENT-VALUE.
           MOVE 0 TO STOPRUN
           DISPLAY 'CWFROMGUI' UPON ENVIRONMENT-NAME
           ACCEPT  FROMGUI     FROM ENVIRONMENT-VALUE
           DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWMENU-PROGRAM
           ACCEPT  CWMENU-PROGRAM   FROM ENVIRONMENT-VALUE
           CALL "CWUSERPGM" USING "G" CWUSER-PROGRAM
           DISPLAY "CWIAEF-DISPLAY" UPON ENVIRONMENT-NAME
           ACCEPT CWIAEF-DISPLAY    FROM ENVIRONMENT-VALUE
           IF     CWIAEF-DISPLAY    = SPACES
                  DISPLAY "CWIAEF_DISPLAY" UPON ENVIRONMENT-NAME
                  ACCEPT CWIAEF-DISPLAY    FROM ENVIRONMENT-VALUE
           END-IF
           INSPECT CWIAEF-DISPLAY CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWIAEF" UPON ENVIRONMENT-NAME
           ACCEPT AJUSTE    FROM ENVIRONMENT-VALUE
           EVALUATE TRUE
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
                CALL "CWCONF" USING CWSQLC
                                    CWCONF-REG
                                    FS-CWCONF
                                    KCO PCO
           END-IF

           IF   OFF-ALTERACAO = "*" MOVE "*" TO SEM-ALTERACAO END-IF
           IF   OFF-CONSULTA  = "*" MOVE "*" TO SEM-CONSULTA  END-IF
           IF   OFF-EXCLUSAO  = "*" MOVE "*" TO SEM-EXCLUSAO  END-IF
           IF   OFF-INCLUSAO  = "*" MOVE "*" TO SEM-INCLUSAO  END-IF

           IF   PROGRAMA = "CWMEN8"
           AND  NIVEL < 4
                MOVE "*" TO SEM-INCLUSAO
                            SEM-EXCLUSAO
           END-IF

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
                GOBACK
           END-IF

frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango     DISPLAY '0000'        UPON ENVIRONMENT-VALUE

           MOVE 01             TO CWBOXW-HORIZONTAL-LENGTH
           MOVE "Fun‡äes"      TO CWBOXS-TITLE
           MOVE SPACES         TO CWBOXS-ITENS CWLINE-SCREENS
           MOVE RED-BROWN-HIGH TO CWBOXS-COLOR-FRAME
                                  CWBOXS-COLOR-BORDER
           MOVE 11             TO CWBOXS-LINE
           MOVE 34             TO CWBOXS-COLUMN
           MOVE 33             TO CWBOXW-COLUMN
           MOVE 35             TO CWLINE-COLUMN
           MOVE 0              TO Y

           IF  SEM-ALTERACAO = SPACE
               ADD  1             TO Y
               MOVE "~Altera‡Æo " TO CWBOXS-TEXT   (Y)
               MOVE "~Altera"     TO CWLINE-SCREEN (Y)
               ADD 11 TO CWBOXW-HORIZONTAL-LENGTH
               SUBTRACT 5 FROM CWBOXW-COLUMN CWLINE-COLUMN
           END-IF

           IF  SEM-CONSULTA = SPACE
               ADD  1           TO Y
               MOVE "~Consulta " TO CWBOXS-TEXT   (Y)
                                    CWLINE-SCREEN (Y)
               ADD 14 TO CWBOXW-HORIZONTAL-LENGTH
               SUBTRACT 6 FROM CWBOXW-COLUMN CWLINE-COLUMN
           END-IF

           IF  SEM-EXCLUSAO = SPACE
               ADD  1            TO Y
               MOVE "~ExclusÆo  " TO CWBOXS-TEXT   (Y)
               MOVE "~Exclui"     TO CWLINE-SCREEN (Y)
               ADD 12 TO CWBOXW-HORIZONTAL-LENGTH
               SUBTRACT 5 FROM CWBOXW-COLUMN CWLINE-COLUMN
           END-IF

           IF  SEM-INCLUSAO = SPACE
               ADD  1            TO Y
               MOVE "~InclusÆo  " TO CWBOXS-TEXT   (Y)
               MOVE "~Inclui"     TO CWLINE-SCREEN (Y)
               ADD 12 TO CWBOXW-HORIZONTAL-LENGTH
               SUBTRACT 5 FROM CWBOXW-COLUMN CWLINE-COLUMN
           END-IF

           IF  (CWUSER-PROGRAM NOT = SPACES)
           AND (CWUSER-PROGRAM NOT = CWMENU-PROGRAM)
               ADD  1            TO Y
               MOVE "~Retornar " TO CWBOXS-TEXT   (Y)
                                    CWLINE-SCREEN (Y)
               ADD 14 TO CWBOXW-HORIZONTAL-LENGTH
               SUBTRACT 6 FROM CWBOXW-COLUMN CWLINE-COLUMN
cyro           ADD  1             TO Y
cyro           MOVE "~Finalizar " TO CWBOXS-TEXT   (Y)
cyro           MOVE "~Fim"        TO CWLINE-SCREEN (Y)
cyro           ADD 9  TO CWBOXW-HORIZONTAL-LENGTH
cyro           SUBTRACT 3 FROM CWBOXW-COLUMN CWLINE-COLUMN
           ELSE
               ADD  1             TO Y
               IF FROMGUI NOT = SPACES
                  MOVE 1             TO STOPRUN
                  MOVE Y             TO YT
                  MOVE "~Volta menu" TO CWBOXS-TEXT   (Y)
                  MOVE "~Menu"       TO CWLINE-SCREEN (Y)
                  ADD 10 TO CWBOXW-HORIZONTAL-LENGTH
                  SUBTRACT 3 FROM CWBOXW-COLUMN CWLINE-COLUMN
               ELSE
                  MOVE "~Finalizar " TO CWBOXS-TEXT   (Y)
                  MOVE "~Fim"        TO CWLINE-SCREEN (Y)
                  ADD 09 TO CWBOXW-HORIZONTAL-LENGTH
                  SUBTRACT 2 FROM CWBOXW-COLUMN CWLINE-COLUMN
                  DISPLAY 'CWRUN' UPON ENVIRONMENT-NAME
                  ACCEPT   CWRUN  FROM ENVIRONMENT-VALUE
                  IF  CWRUN NOT = '1'
                      ADD  1             TO Y
                      MOVE Y             TO YT
                      MOVE "~Volta menu" TO CWBOXS-TEXT   (Y)
                      MOVE "~Menu"       TO CWLINE-SCREEN (Y)
                      ADD 10 TO CWBOXW-HORIZONTAL-LENGTH
                      SUBTRACT 3 FROM CWBOXW-COLUMN CWLINE-COLUMN
                  END-IF
               END-IF
           END-IF

           MOVE Y             TO K

           IF   FUNCAO-ANTERIOR = 0
                MOVE Y TO FUNCAO-ANTERIOR
           END-IF

           MOVE FUNCAO-ANTERIOR TO CWBOXS-OPTION CWLINE-OPTION

           IF   MODO-MENU < 2
                SET CWBOXS-TIMEOUT-ENABLE TO TRUE
                CALL "CWBOXS"     USING PARAMETROS-CWBOXS
           ELSE
                MOVE CWLINE-OPTION   TO CWBOXS-OPTION
                MOVE 11              TO CWBOXW-LINE
                MOVE "OPEN"          TO CWBOXW-FUNCTION
                MOVE 112             TO CWBOXW-COLOR-FRAME
                MOVE 112             TO CWBOXW-COLOR-BORDER
                MOVE 3               TO CWBOXW-VERTICAL-LENGTH
                CALL "CWBOXW"     USING PARAMETROS-CWBOXW
                MOVE 12              TO CWLINE-LINE
                SET CWLINE-TIMEOUT-ENABLE TO TRUE
                CALL "CWLINE"     USING PARAMETROS-CWLINE
                MOVE CWLINE-OPTION   TO CWBOXS-OPTION
                MOVE "CLOSE"         TO CWBOXW-FUNCTION
                CALL "CWBOXW"     USING PARAMETROS-CWBOXW
           END-IF

           IF   CWBOXS-TIMEOUT-ON
           OR   CWLINE-TIMEOUT-ON
                MOVE YT TO CWBOXS-OPTION
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
                     MOVE CWLINE-SCREEN (CWBOXS-OPTION) TO OBS
                     MOVE CWLINE-CHAR   (CWBOXS-OPTION) TO FUNCAO
                     IF   FUNCAO = "M" OR 'R'
                          MOVE "V" TO FUNCAO
                     END-IF
                END-IF
                CALL    "CWTEXT" USING OBS LENGTH OF OBS
           END-IF

           IF   CWIAEF-DISPLAY NOT = 'OFF'
           AND  CWFRAME = SPACES
                IF  AJUSTE NOT = SPACE
                    MOVE 29 TO LEN
                    CALL "CBL_READ_SCR_CHARS" USING X"0428"
                                              OBSR
                                              LEN
                    PERFORM VARYING LEN FROM 29 BY -1 UNTIL LEN = 1
                                 OR OBSR (LEN: 1) = AJUSTE
                            CONTINUE
                    END-PERFORM
                    INSPECT OBS (1: LEN)
                            CONVERTING SPACE TO AJUSTE
                END-IF
      *         DISPLAY OBS AT 0541
                CALL "CBL_WRITE_SCR_CHARS" USING X"0428"
                                                 OBS
                                                 X"001C"
MDBA  *                                          X"001D"
           END-IF
           IF   FUNCAO = 'M'
           AND  STOPRUN = 1
                MOVE "F" TO FUNCAO
           END-IF

mollo      IF  CWFRAME NOT = SPACES
mollo          CALL CWFRAME USING 'f' OBS
           END-IF

frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     DISPLAY JANPOS        UPON ENVIRONMENT-VALUE
           GOBACK.

       END PROGRAM CWIAEF.
