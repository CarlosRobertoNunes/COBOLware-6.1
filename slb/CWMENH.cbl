       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/09/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *   Configura atributos de cor                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 MULTI-USER               PIC  9(001) VALUE 0.
           05 TMP                      PIC  X(003) VALUE SPACES.
           05 CONTA-LOOP               PIC  9(004) VALUE 0.
           05 COR-F5                   PIC  9(001) VALUE ZERO.
           05 COR-F6                   PIC  9(001) VALUE ZERO.
           05 COR-F7                   PIC  9(001) VALUE ZERO.
           05 COR-F8                   PIC  9(001) VALUE ZERO.
           05 ATTRIBUTE-F5      COMP-X PIC  9(002) VALUE ZERO.
           05 ATTRIBUTE-F6      COMP-X PIC  9(002) VALUE ZERO.
           05 ATTRIBUTE-F7      COMP-X PIC  9(002) VALUE ZERO.
           05 ATTRIBUTE-F8      COMP-X PIC  9(002) VALUE ZERO.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
           05 NOME                     PIC  X(030) VALUE "LOGON".
           05 NOVAS.
              10 CWHELP                PIC  9(001) VALUE ZERO.
              10 CWBOXW                PIC  9(001) VALUE ZERO.
              10 CWBOXC                PIC  9(001) VALUE ZERO.
              10 CWLOGW                PIC  9(001) VALUE ZERO.
           05 CBL-READ-SCR-CHARS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE            PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE     PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME     PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL      PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE         PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-HANDLE            PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X.
           05 KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
           05 CBL-SET-CSR-POS.
              10 CURSOR-POSITION.
                 15 ROW-CURSOR        PIC  9(002) COMP-X VALUE 00.
                 15 COLUMN-CURSOR     PIC  9(002) COMP-X VALUE 00.
           05 CLASSE                  PIC  X(001) VALUE SPACE.
           05 CX               COMP-X PIC  9(004) VALUE 0.
           05 LX               COMP-X PIC  9(004) VALUE 0.
           05 I                COMP-X PIC  9(004) VALUE 0.
           05 Y                COMP-X PIC  9(004) VALUE 0.
           05 Y-A              COMP-X PIC  9(004) VALUE 0.
           05 Z                COMP-X PIC  9(004) VALUE 0.
           05 DADOS.
              10 CARACTER             PIC  X(001).
              10 CARACTER-X REDEFINES CARACTER PIC 9(002) COMP-X.
              10 TECLA                PIC  9(003). COPY CWEDIT.
           05 APAGA-RODAPE             PIC  X(080) VALUE SPACES.
           05 RODAPE                   PIC  X(080) VALUE
             " <>^v F5-Texto-(7-1) F6-Texto-(7+1) F7-Fundo-(0-1) F8-Fund
      -      "o-(0+1) Esc-Sa¡da".
           05 RODAPE-MOUSE             PIC  X(080) VALUE
             "      ?????????????? @@@@@@@@@@@@@@ AAAAAAAAAAAAAA BBBBBBB
      -      "BBBBBBB 111111111".

       COPY CWGETL.
       COPY CWUNIX.
       COPY CWCONF.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01                          PIC  X(080) VALUE ALL "e".
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "effffffffffffffffffffffffffffffffffffffpppppppppppppppp".
           05 FILLER                         PIC  X(025) VALUE
              "pppppppprgggshhhhhhhhhhte".
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "eiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiqqqqqqqqqqqqqqqq".
           05 FILLER                         PIC  X(025) VALUE
              "qqqqqqqqqqqqqqujjjjjjjjve".
       02  LINHA-04                          PIC  X(080) VALUE ALL "e".
       02  LINHA-05.
           05 FILLER                         PIC  X(055) VALUE
              "ekkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkllllllllllllllll".
           05 FILLER                         PIC  X(025) VALUE
              "lllllllllllllllllllllllle".
       02  LINHA-06                          PIC  X(080) VALUE ALL "e".
       02  LINHA-07.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-08.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-09.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-10.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-11.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-12.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-13.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-14.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-15.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-16.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-17.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-18.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-19.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-20.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-21.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-22                          PIC  X(080) VALUE ALL "e".
       02  LINHA-23.
           05 FILLER                         PIC  X(055) VALUE
              "ennnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn".
           05 FILLER                         PIC  X(025) VALUE
              "nnnnnnnnnnnnnnnnwooooooxe".
       02  LINHA-24                          PIC  X(080) VALUE ALL "e".

       01  REDEFINES LINHAS-DE-IMPRESSAO-CLIC.
           05 CLASSE-C OCCURS 1920 PIC 9(002) COMP-X.

       LINKAGE SECTION.

       01   PARAMETROS-CWACOR.
            05 CORES OCCURS 20.
               10 CWACOR-F                      PIC  9(001).
               10 CWACOR-B                      PIC  9(001).
            05 FUNCAO                           PIC  X(001).
               88 CONFIGURAR  VALUE "C" "c" "U" "u".
               88 POR-USUARIO VALUE "U" "u".

       PROCEDURE DIVISION USING PARAMETROS-CWACOR.

       000-INICIO.

           DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   TMP (1: 2) = "ON"
                MOVE 1 TO MULTI-USER
           END-IF

           CALL "CWUNIX" USING PARAMETROS-CWUNIX

           IF   CWUNIX-ON
                MOVE 1 TO MULTI-USER
           END-IF

           CALL "CWACOR" USING PARAMETROS-CWACOR
           CALL "CWGETL" USING PARAMETROS-CWGETL

           IF   CWUNIX-OFF
                MOVE X"1B" TO RODAPE (2: 1)
                MOVE X"12" TO RODAPE (3: 1)
                MOVE X"1A" TO RODAPE (4: 1)
                MOVE SPACE TO RODAPE (5: 1)
           ELSE
                CALL "CWTEXT" USING RODAPE LENGTH OF RODAPE
           END-IF

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF NOT = "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           CALL "CWGETU" USING NOME TASK PROGRAMA '?'
           IF   NOME = "LOGON"
                MOVE SPACES TO NOME
           END-IF

           MOVE "AT"  TO CWCONF-REGAT
           MOVE NOME  TO CWCONF-ELEMENTO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "23"
                MOVE "AT"  TO CWCONF-REGAT
                MOVE NOME  TO CWCONF-ELEMENTO
                MOVE PARAMETROS-CWACOR TO CWCONF-REGAT (33: )
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   FS-CWCONF = "00"
                MOVE CWCONF-REGAT (33: ) TO PARAMETROS-CWACOR (1: 40)
           ELSE
                CALL "CWCONF" USING "ISAM"
           END-IF

           IF   NOT POR-USUARIO
                MOVE SPACES TO NOME
                PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                        MOVE "AT"  TO CWCONF-REGAT
                        MOVE NOME  TO CWCONF-ELEMENTO
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
                MOVE CWCONF-REGAT (33: ) TO PARAMETROS-CWACOR (1: 40)
                PERFORM VARYING CX FROM 1 BY 1
                           UNTIL CX > 80
                        PERFORM VARYING LX FROM 1 BY 1
                                UNTIL LX > 24
                                COMPUTE I = (LX * 80) - 80 + CX
                                COMPUTE Y = CLASSE-C (I) - 100
                                PERFORM CWMENHA
                        END-PERFORM
                END-PERFORM
           END-IF
           IF   CWGETL-MOUSE = 1
                CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF
           MOVE 1 TO Y
           CALL "CBL_WRITE_SCR_CHARS" USING X"1800"
                                            RODAPE
                                            X"0050"
           PERFORM NUMEROS
           MOVE 23                   TO ROW-CURSOR
           MOVE 0                    TO COLUMN-CURSOR
           CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
           PERFORM TEST AFTER UNTIL EDIT-ESC
              PERFORM TEST AFTER UNTIL KEY-STATUS > 0
                                    OR MULTI-USER = 1
                 MOVE 0 TO TECLA
                 IF   CWGETL-MOUSE = 1
                      CALL "CBL_GET_MOUSE_POSITION"
                            USING MOUSE-HANDLE
                                  MOUSE-POSITION
                        ON OVERFLOW
                           CONTINUE
                        END-CALL
                      IF   ROW-MOUSE < 24
                      AND  COLUMN-MOUSE < 79
                           MOVE ROW-MOUSE    TO ROW-CURSOR
                           MOVE COLUMN-MOUSE TO COLUMN-CURSOR
                           COMPUTE I = ((ROW-CURSOR + 1) * 80)
                                     - 80 + COLUMN-CURSOR + 1
                           COMPUTE Y = CLASSE-C (I) - 100
                           IF   Y NOT = Y-A
                                MOVE Y TO Y-A
                                PERFORM NUMEROS
                           END-IF
                           CALL "CBL_SET_CSR_POS"
                                USING CURSOR-POSITION
                      END-IF
                 END-IF
                 CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                 IF   CWGETL-MOUSE = 1
                      CALL "CBL_READ_MOUSE_EVENT" USING
                                                  MOUSE-HANDLE
                                                  MOUSE-DATA
                                                  MOUSE-READ-TYPE
                             ON   OVERFLOW
                                  CONTINUE
                      END-CALL
                      IF   MOUSE-EVENT-TYPE = 2 OR 3
                           SET EDIT-F6 TO TRUE
                           MOVE 2 TO KEY-STATUS
                      END-IF
                      IF   MOUSE-EVENT-TYPE = 4 OR 5
                           SET EDIT-F8 TO TRUE
                           MOVE 2 TO KEY-STATUS
                      END-IF
                 END-IF
              END-PERFORM
              IF   KEY-STATUS = 1
              OR   MULTI-USER = 1
                   CALL "CWKBDC" USING "0000" CARACTER TECLA
              END-IF
              EVALUATE TRUE
                 WHEN EDIT-CURSOR-UP
                      IF   ROW-CURSOR > 0
                           SUBTRACT 1 FROM ROW-CURSOR
                      ELSE
                           MOVE 23      TO ROW-CURSOR
                      END-IF
                      CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                 WHEN EDIT-CURSOR-DOWN
                      IF   ROW-CURSOR < 23
                           ADD      1   TO ROW-CURSOR
                      ELSE
                           MOVE 0       TO ROW-CURSOR

                      END-IF
                      CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                 WHEN EDIT-CURSOR-LEFT
                      IF   COLUMN-CURSOR > 0
                           SUBTRACT 1 FROM COLUMN-CURSOR
                      ELSE
                           MOVE 79      TO COLUMN-CURSOR

                      END-IF
                      CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                 WHEN EDIT-CURSOR-RIGHT
                      IF   COLUMN-CURSOR < 79
                           ADD      1   TO COLUMN-CURSOR
                      ELSE
                           MOVE 0       TO COLUMN-CURSOR
                      END-IF
                      CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
              END-EVALUATE
              IF   EDIT-F5
              OR   EDIT-F6
              OR   EDIT-F7
              OR   EDIT-F8
              OR   EDIT-CURSOR-UP
              OR   EDIT-CURSOR-DOWN
              OR   EDIT-CURSOR-LEFT
              OR   EDIT-CURSOR-RIGHT
                   COMPUTE I = ((ROW-CURSOR + 1) * 80)
                             - 80 + COLUMN-CURSOR + 1
                   COMPUTE Y = CLASSE-C (I) - 100
                   EVALUATE TRUE
                            WHEN EDIT-F5
                                 IF   CWACOR-F (Y) = 0
                                      MOVE 7 TO CWACOR-F (Y)
                                 ELSE
                                      SUBTRACT 1 FROM CWACOR-F (Y)
                                 END-IF
                            WHEN EDIT-F6
                                 IF   CWACOR-F (Y) = 7
                                      MOVE 0 TO CWACOR-F (Y)
                                 ELSE
                                      ADD  1 TO CWACOR-F (Y)
                                 END-IF
                            WHEN EDIT-F7
                                 IF   CWACOR-B (Y) = 0
                                      MOVE 7 TO CWACOR-B (Y)
                                 ELSE
                                      SUBTRACT 1 FROM CWACOR-B (Y)
                                 END-IF
                            WHEN EDIT-F8
                                 IF   CWACOR-B (Y) = 7
                                      MOVE 0 TO CWACOR-B (Y)
                                 ELSE
                                      ADD  1 TO CWACOR-B (Y)
                                 END-IF
                   END-EVALUATE
                   PERFORM NUMEROS
                   CALL "CBL_SET_CSR_POS" USING X"FFFF"
                   IF   EDIT-F6 OR EDIT-F8
                        PERFORM VARYING CX FROM 1 BY 1
                                   UNTIL CX > 80
                           PERFORM VARYING LX FROM 1 BY 1
                                   UNTIL LX > 24
                                   COMPUTE I = (LX * 80) - 80 + CX
                                   COMPUTE Z = CLASSE-C (I) - 100
                                   IF   Z = Y
                                        PERFORM CWMENHA
                                        CALL "CBL_SET_CSR_POS"
                                       USING X"FFFF"
                                   END-IF
                           END-PERFORM
                        END-PERFORM
                   END-IF
                   IF   EDIT-F5 OR EDIT-F7
                        PERFORM VARYING CX FROM 80 BY -1
                                   UNTIL CX < 1
                           PERFORM VARYING LX FROM 24 BY -1
                                   UNTIL LX < 1
                                   COMPUTE I = (LX * 80) - 80 + CX
                                   COMPUTE Z = CLASSE-C (I) - 100
                                   IF   Z = Y
                                        PERFORM CWMENHA
                                   END-IF
                           END-PERFORM
                        END-PERFORM
                   END-IF
                   PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                           MOVE "AT"  TO CWCONF-REGAT
                           MOVE NOME  TO CWCONF-ELEMENTO
                           SET CWSQLC-READ TO TRUE
                           SET CWSQLC-EQUAL TO TRUE
                           SET CWSQLC-LOCK TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                           IF   FS-CWCONF = "9D"
                                CALL "CWCONF" USING "ISAM"
                           END-IF
                   END-PERFORM
                   MOVE PARAMETROS-CWACOR TO CWCONF-REGAT (33: )
                   SET CWSQLC-REWRITE TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   SET CWSQLC-UNLOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
              END-IF
           END-PERFORM
           CALL "CBL_WRITE_SCR_CHARS" USING X"1800"
                                            APAGA-RODAPE
                                            X"0050"
           IF   CWGETL-MOUSE = 1
                CALL "CBL_TERM_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           CANCEL "CWACOR"
           GOBACK.

       FIM-0500-3000. EXIT.

       CWMENHA.

           COMPUTE ROW-NUMBER    = LX - 1
           COMPUTE COLUMN-NUMBER = CX - 1
           COMPUTE ATTRIBUTE = CWACOR-F (Y) + (CWACOR-B (Y) * 16)
           CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                             ATTRIBUTE
                                             X"0001".


       NUMEROS.

           MOVE CWACOR-F (Y) TO COR-F5 COR-F6
           MOVE CWACOR-B (Y) TO COR-F7 COR-F8
           IF   COR-F5 = 0
                MOVE 7 TO COR-F5
           ELSE
                SUBTRACT 1 FROM COR-F5
           END-IF
           IF   COR-F6 = 7
                MOVE 0 TO COR-F6
           ELSE
                ADD  1 TO COR-F6
           END-IF
           IF   COR-F7 = 0
                MOVE 7 TO COR-F7
           ELSE
                SUBTRACT 1 FROM COR-F7
           END-IF
           IF   COR-F8 = 7
                MOVE 0 TO COR-F8
           ELSE
                ADD  1 TO COR-F8
           END-IF
           COMPUTE ATTRIBUTE-F5 = COR-F5 + (CWACOR-B (Y) * 16)
           COMPUTE ATTRIBUTE-F6 = COR-F6 + (CWACOR-B (Y) * 16)
           COMPUTE ATTRIBUTE-F7 = CWACOR-F (Y) + (COR-F7 * 16)
           COMPUTE ATTRIBUTE-F8 = CWACOR-F (Y) + (COR-F8 * 16)
           CALL "CBL_WRITE_SCR_CHARS" USING X"1810"
                                            CWACOR-F (Y)
                                            X"0001"
           CALL "CBL_WRITE_SCR_CHARS" USING X"181F"
                                            CWACOR-F (Y)
                                            X"0001"
           CALL "CBL_WRITE_SCR_CHARS" USING X"182E"
                                            CWACOR-B (Y)

                                            X"0001"
           CALL "CBL_WRITE_SCR_CHARS" USING X"183D"
                                            CWACOR-B (Y)
                                            X"0001"

           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1806"
                                             ATTRIBUTE-F5
                                             X"000E"

           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1815"
                                             ATTRIBUTE-F6
                                             X"000E"

           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1824"
                                             ATTRIBUTE-F7
                                             X"000E"

           CALL "CBL_WRITE_SCR_N_ATTR" USING X"1833"
                                             ATTRIBUTE-F8
                                             X"000E".

       END PROGRAM CWMENH.

