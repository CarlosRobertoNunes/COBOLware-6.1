       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXS INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/11/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre janela-menu e retorna a op‡Æo           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
frango     05 JANPOS.
frango        10 JANLIN               PIC  9(002) VALUE 0.
frango        10 JANCOL               PIC  9(002) VALUE 0.
           05 MULTI-COLOR             PIC  X(098) VALUE SPACEs.
           05 COLOR-CHAR              PIC  X(080) VALUE SPACEs.
           05 COLOR-TAB.
              10 COR-TAB              PIC  9(003) OCCURS 21.
           05 COR-N                   PIC  9(002) COMP-X.
           05 LETRA                   PIC  X(001) VALUE SPACE.
           05 CWBOXS-EXIT-CHAR        PIC  X(001) VALUE SPACE.
           05 COLOR-BARR              PIC  9(003) VALUE 0.
           05 NUMERO                  PIC  9(018) VALUE 0.
           05 CWNUMERO                PIC  X(018) VALUE SPACES.
           05 II                      PIC  9(004) VALUE 0.
           05 NX                      PIC  9(002) COMP-X.
           05 MXS.
              10 MX OCCURS 21         PIC  9(001) VALUE 0.
           05 MYS.
              10 MY OCCURS 21         PIC  9(001) VALUE 0.
           05 MULTI-USER              PIC  9(001) VALUE 0.
           05 MORE                    PIC  X(001) VALUE SPACE.
           05 TMP                     PIC  X(003) VALUE SPACE.
           05 VEZ                     PIC  9(001) VALUE 1.
           05 ARROW-OFF               PIC  9(001) VALUE 0.
           05 REVERSO                 PIC  9(002) COMP-X VALUE 0.
           05 HORA-A                  PIC  X(006) VALUE SPACES.
           05 HORA-B                  PIC  X(006) VALUE SPACES.
           05 DESTACADOS.
              10 DESTACADO            PIC  X(001) OCCURS 24.
           05 LX                      PIC  9(002) VALUE 0.
           05 SEGUNDOS                PIC  9(004) VALUE 0.
           05 TITULO                              VALUE SPACES.
              10 CWBOXS-T OCCURS 78   PIC  X(001).
           05 MOLDURA-DEFAULT         PIC  9(001) VALUE 0.
           05 C                       PIC  9(003) VALUE 0.
           05 ATT-H                   PIC  X(001) VALUE X"6A".
           05 ATT-HN REDEFINES ATT-H  PIC  9(002) COMP-X.
              88 SAME-FB VALUE 000 008 017 025 034 042 051 059
                               068 076 085 093 102 110 119 127.
              88 LOW-FB VALUE  000 001 002 003 004 005 006 007
                               016 017 018 019 020 021 022 023
                               032 033 034 035 036 037 038 039
                               048 049 050 051 052 053 054 055
                               064 065 066 067 068 069 070 071
                               080 081 082 083 084 085 086 087
                               096 097 098 099 100 101 102 103
                               112 113 114 115 116 117 118 119.
           05 ATT-T                   PIC  X(001) VALUE X"1B".
           05 ATT-B                   PIC  X(001) VALUE X"1B".
           05 ATT-HB                  PIC  X(001) VALUE X"1B".
           05 SOMBRA                  PIC  X(001) VALUE X"08".
           05 REVERTE-VIDEO           PIC  X(001) VALUE X"70".
           05 REVERTE-BLINK           PIC  9(002) COMP-X VALUE 0.
           05 KEY-STATUS              PIC  9(002) COMP-X VALUE 0.
           05 T                       PIC  9(004) COMP-X VALUE 0.
           05 SIZE-BARR-MENU          PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-HANDLE            PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION-S        PIC  X(008).
           05 MOUSE-POSITION-A        PIC  X(008).
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE     PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME     PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL      PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE         PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MAX              PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MIN              PIC  9(002) COMP-X VALUE 0.
           05 ROW-MIN                 PIC  9(002) COMP-X VALUE 0.
           05 ROW-MAX                 PIC  9(002) COMP-X VALUE 0.
           05 BYTE-MOUSE              PIC  X(001) VALUE SPACE.
           05 BYTE-MOUSE-LENGTH       PIC  9(004) COMP-X VALUE 1.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BARR       PIC  X(001) VALUE SPACES.
              10 ATTRIBUTE-BARR-X REDEFINES
                 ATTRIBUTE-BARR       PIC  9(002) COMP-X.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 0.
              10 STRING-START         PIC  9(004) COMP-X VALUE 0.
              10 SCREEN-POSITION-MOUSE.
                 15 ROW-NUMBER-2      PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-2   PIC  9(002) COMP-X VALUE 0.
           05 SALVA-CURSOR.
              15                      PIC  9(002) COMP-X VALUE 0.
              15                      PIC  9(002) COMP-X VALUE 0.
           05 OPCOES-CHAR                         VALUE SPACES.
              10 CHAR OCCURS 21       PIC  X(001).
           05 CURPOS.
              10 CURPOS-LIN            PIC  9(002) VALUE ZERO.
              10 CURPOS-COL            PIC  9(002) VALUE ZERO.
           05 CARACTER                PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                      PIC  9(002) COMP-X.
           05 TECLA                   PIC  9(003). COPY CWEDIT.
           05 MAX-OP           COMP-X PIC  9(002) VALUE 0.
           05 RETORNO          COMP-X PIC  9(004) VALUE 0.
           05 NADA                    PIC  X(001) VALUE SPACE.
           05 NEW-ITEM                PIC  X(080) VALUE SPACES.
           05 CHECK-ITEM              PIC  X(080) VALUE SPACES.
           05 REDEFINES CHECK-ITEM.
              10 BYTE-ITEM OCCURS 80  PIC  X(001).
           05 MAIOR            COMP-X PIC  9(002) VALUE ZERO.
           05 MAIOR-TIT        COMP-X PIC  9(002) VALUE ZERO.
           05 LINHA            COMP-X PIC  9(002) VALUE ZERO.
           05 LINHA-2          COMP-X PIC  9(002) VALUE ZERO.
           05 LINHA-I          COMP-X PIC  9(002) VALUE ZERO.
           05 COLUNA-2         COMP-X PIC  9(002) VALUE ZERO.
           05 CC               COMP-X PIC  9(002) VALUE ZERO.
           05 KC               COMP-X PIC  9(002) VALUE ZERO.
           05 SPC.
              15 RN                   PIC  9(002) COMP-X VALUE 0.
              15 CN                   PIC  9(002) COMP-X VALUE 0.
           05 COLUNA-I         COMP-X PIC  9(002) VALUE ZERO.
           05 COLUNA           COMP-X PIC  9(002) VALUE ZERO.
           05 TAMANHO          COMP-X PIC  9(002) VALUE ZERO.
           05 UM               COMP-X PIC  9(002) VALUE 1.
           05 I                COMP-X PIC  9(002) VALUE ZERO.
           05 E                COMP-X PIC  9(002) VALUE ZERO.
           05 E-ANT            COMP-X PIC  9(002) VALUE ZERO.
           05 Y                COMP-X PIC  9(008) VALUE ZERO.
           05 K                       PIC  9(002) VALUE ZERO.
           05 K2                      PIC  9(002) VALUE ZERO.
           05 T-A                     PIC  X(2000) VALUE SPACES.
           05 A-A                     PIC  X(2000) VALUE SPACES.
           05 TELA.
              10             OCCURS 25.
                 15 BYTE     OCCURS 80 PIC X(001).
           05 ATRIBUTOS.
              10 S-ATT       OCCURS 25.
                 15 ATRIBUTO OCCURS 80 PIC X(001).
           05 WINDOW.
              10 OCCURS 25.
                 15 ESQUERDA PIC X.
                 15 ITEM.
                    20 BYTE-JANE OCCURS 79 PIC X.
           05 REDEFINES WINDOW.
              10           OCCURS 25.
                 15 BYTE-J OCCURS 80 PIC X.
           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 MOLDURA                              VALUE SPACES.
              10 M-201   PIC  X(001).
              10 M-205   PIC  X(001).
              10 M-187   PIC  X(001).
              10 M-186   PIC  X(001).
              10 M-204   PIC  X(001).
              10 M-185   PIC  X(001).
              10 M-200   PIC  X(001).
              10 M-188   PIC  X(001).
           05 USUARIO    PIC  X(030) VALUE SPACES.
           05 TASK       PIC  9(006) VALUE 0.
           05 PROGRAMA   PIC  X(008) VALUE SPACES.
           05 SET-LOG    PIC  X(001) VALUE SPACE.

       COPY CWGETL.
       COPY CWUNIX.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXS.
           05 CWBOXS-LINE                 PIC  9(002).
           05 CWBOXS-COLUMN               PIC  9(002).
           05 CWBOXS-TYPE                 PIC  9(001).
           05 CWBOXS-OPTION               PIC  9(002).
           05 CWBOXS-OPTION-CHAR          PIC  X(001).
              88 CWBOXS-LEAVING                          VALUE "?".
           05 CWBOXS-TITLE                PIC  X(078).
           05 CWBOXS-ITENS.
              10 CWBOXS-CHARS.
                 15 CWBOXS-CHAR OCCURS 21 PIC  X(001).
              10 CWBOXS-TEXT    OCCURS 21 PIC  X(078).
           05 CWBOXS-KEY-ON               PIC  X(001).
           05 CWBOXS-KEY                  PIC  9(002).
           05 TECLA-X REDEFINES CWBOXS-KEY
                                   COMP-5 PIC S9(004).
           05 CWBOXS-COLOR-FRAME          PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-BORDER         PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-SHADE          PIC  9(002) COMP-X.
           05 CWBOXS-COLOR-BARR-MENU      PIC  9(002) COMP-X.
           05 CWBOXS-ARROW                PIC  X(001).
           05 CWBOXS-ERASE                PIC  X(001).
           05 CWBOXS-TIMEOUT-STATUS       PIC  9(001).
              88 CWBOXS-TIMEOUT-ENABLE                VALUE 1.
              88 CWBOXS-TIMEOUT-DISABLE               VALUE 0.
           05 CWBOXS-TIMEOUT-RETURN       PIC  9(001).
              88 CWBOXS-TIMEOUT-ON                    VALUE 1.
              88 CWBOXS-TIMEOUT-OFF                   VALUE 0.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXS.

       000-INICIO.

frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango     add JANLIN to CWBOXS-LINE
frango     add JANCOL to CWBOXS-COLUMN
           DISPLAY "CWBOXS-COLORS" UPON ENVIRONMENT-NAME
           INITIALIZE MULTI-COLOR COLOR-TAB
           ACCEPT  MULTI-COLOR FROM ENVIRONMENT-VALUE
           IF MULTI-COLOR NOT = SPACES
              MOVE 1        TO T
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH MULTI-COLOR
                      IF MULTI-COLOR (I:1) NOT NUMERIC
                         IF COR-TAB (T) NOT = 0
                            ADD  1 TO T
                         END-IF
                      ELSE
                         IF COR-TAB (T) NOT = 0
                            MOVE COR-TAB (T) (2: 2)
                              TO COR-TAB (T) (1: 2)
                         END-IF
                         MOVE MULTI-COLOR (I:1) TO COR-TAB (T) (3:1)
                      END-IF
              END-PERFORM
              MOVE 0        TO T
           END-IF

           IF   VEZ = 1
                DISPLAY "CWCOLOR-BARR"   UPON ENVIRONMENT-NAME
                PERFORM AJUSTA MOVE NUMERO TO COLOR-BARR
                DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 2) = "ON"
                     MOVE 1 TO MULTI-USER
                END-IF
                DISPLAY "CWARROW"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 3) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 3) = "OFF"
                     MOVE 1 TO ARROW-OFF
                END-IF
                MOVE 2            TO VEZ
                CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE 1     TO MULTI-USER
                     MOVE ">"   TO MORE
                ELSE
                     MOVE X"10" TO MORE
                END-IF
           END-IF

           IF CWBOXS-COLOR-BARR-MENU = 112
           AND (COLOR-BARR NOT = 0)
               MOVE COLOR-BARR TO CWBOXS-COLOR-BARR-MENU
           END-IF

           CALL "CWGETL" USING PARAMETROS-CWGETL

           IF   CWGETL-MOUSE = 1
                CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON EXCEPTION
                        CONTINUE
                END-CALL
                CALL "CBL_GET_MOUSE_POSITION"
                      USING MOUSE-HANDLE
                            MOUSE-POSITION
                  ON   EXCEPTION
                      CONTINUE
                END-CALL
                MOVE MOUSE-POSITION   TO MOUSE-POSITION-A
                                         MOUSE-POSITION-S
           END-IF

           COMPUTE C = CWBOXS-COLOR-FRAME     + 1
           MOVE COR (C) TO ATT-T

           COMPUTE C = CWBOXS-COLOR-SHADE     + 1
           MOVE COR (C) TO SOMBRA

           IF   CWBOXS-COLOR-BARR-MENU = 112
                COMPUTE REVERSO = CWBOXS-COLOR-FRAME - 16
                COMPUTE C = REVERSO + 1
           ELSE
                COMPUTE C = CWBOXS-COLOR-BARR-MENU + 1
           END-IF

           IF   CWGETL-HIGH = 1
                MOVE COR (C) TO REVERTE-VIDEO
           END-IF

=>PDS      MOVE REVERTE-VIDEO TO ATT-H
=>PDS      IF  LOW-FB
=>PDS          ADD 8 TO ATT-HN
=>PDS      ELSE
=>PDS           SUBTRACT 8 FROM ATT-HN
=>PDS      END-IF
=>PDS      IF SAME-FB ADD 1 TO ATT-HN.
=>PDS      MOVE ATT-H TO ATT-HB

           COMPUTE C = CWBOXS-COLOR-BORDER    + 1
           MOVE COR (C) TO ATT-B
=>PDS                      ATT-H
      *    SUBTRACT 16 FROM C
      *    IF  C = 0 MOVE 1 TO C END-IF
      *    MOVE COR (C) TO ATT-H

=>PDS      IF  LOW-FB
=>PDS          ADD 8 TO ATT-HN
=>PDS      ELSE
=>PDS           SUBTRACT 8 FROM ATT-HN
=>PDS      END-IF
=>PDS      IF SAME-FB ADD 1 TO ATT-HN.

           IF   CWBOXS-TYPE = 0
                MOVE "m"             TO SET-LOG
                CALL "CWGETU"     USING USUARIO TASK PROGRAMA SET-LOG
                MOVE TASK            TO MOLDURA-DEFAULT
                MOVE MOLDURA-DEFAULT TO CWBOXS-TYPE
           END-IF

           COMPUTE I = CWBOXS-TYPE + 1
           MOVE BASE-MOLDURA (I) TO MOLDURA

           IF   CWBOXS-LINE = 0
           OR   CWBOXS-LINE NOT NUMERIC
                MOVE 1 TO CWBOXS-LINE
           END-IF

           IF   CWBOXS-COLUMN = 0
           OR   CWBOXS-COLUMN NOT NUMERIC
                MOVE 1 TO CWBOXS-COLUMN
           END-IF

           COMPUTE T = ((CWBOXS-LINE + 1) * 80) - 80 + CWBOXS-COLUMN + 1

           MOVE 2000                      TO STRING-LENGTH
           MOVE 0                         TO ROW-NUMBER
                                             COLUMN-NUMBER
           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH


           MOVE CARACTER-BUFFER         TO T-A      TELA
           MOVE ATTRIBUTE-BUFFER        TO A-A ATRIBUTOS
           MOVE ZERO                    TO MAIOR
                                           MAX-OP
                                           MAIOR-TIT
           MOVE 1                       TO TAMANHO
           MOVE LOW-VALUES              TO WINDOW
           MOVE SPACES                  TO OPCOES-CHAR
           IF   ARROW-OFF = 0
                IF   CWUNIX-OFF
                     MOVE X"1B"              TO TITULO
                ELSE
                     MOVE "<"                TO TITULO
                END-IF
           END-IF
           MOVE CWBOXS-TITLE            TO TITULO (2: )
           MOVE TITULO                  TO CHECK-ITEM
           PERFORM VARYING MAIOR FROM 78 BY -1
                    UNTIL MAIOR = 0
                       OR BYTE-ITEM (MAIOR) NOT = SPACE
                   CONTINUE
           END-PERFORM

           MOVE MAIOR TO MAIOR-TIT
           MOVE ZEROS TO MXS MYS

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
              IF   CWBOXS-TEXT (I) NOT = SPACES
                   CALL "CWVARX" USING CWBOXS-TEXT (I)
                             LENGTH OF CWBOXS-TEXT (I)
                   IF  CWBOXS-TEXT (I) (77: 2) = X"1010"
                       MOVE SPACES TO CWBOXS-TEXT (I) (77: 2)
                       MOVE 1  TO MX (I)
                   ELSE
                       MOVE 0  TO MX (I)
                   END-IF
                   ADD  1                 TO TAMANHO
                                             MAX-OP
                   MOVE CWBOXS-TEXT   (I) TO CHECK-ITEM
                   IF   BYTE-ITEM (1) NOT = SPACE
                        MOVE SPACE             TO CHECK-ITEM
                        MOVE CWBOXS-TEXT   (I) TO CHECK-ITEM (2: )
                   END-IF
                   PERFORM VARYING Y FROM 78 BY -1
                           UNTIL Y = 0
                              OR BYTE-ITEM (Y) NOT = SPACE
                           CONTINUE
                   END-PERFORM
                   MOVE SPACES TO NEW-ITEM
                   MOVE 0      TO K2
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > Y
                           IF   BYTE-ITEM (K) = X"7E"
                                ADD 1 TO K
                                INSPECT BYTE-ITEM (K)
                                     CONVERTING MINUSCULAS TO MAIUSCULAS
                                MOVE BYTE-ITEM (K) TO CWBOXS-CHAR (I)
                           END-IF
                           ADD 1 TO K2
                           MOVE BYTE-ITEM (K) TO NEW-ITEM (K2: 1)
                           IF   K2 > MAIOR
                           AND  BYTE-ITEM (K) NOT = SPACE
                                MOVE K2 TO MAIOR
                           END-IF
                   END-PERFORM
txt   *            CALL "CWTEXT" USING NEW-ITEM Y
                   MOVE NEW-ITEM        TO ITEM (TAMANHO)
                   MOVE NEW-ITEM (2: )  TO CWBOXS-TEXT (I)
                   INSPECT CWBOXS-CHAR (I)
                           CONVERTING MINUSCULAS TO MAIUSCULAS
                   MOVE CWBOXS-CHAR (I) TO CHAR (TAMANHO)
                   IF   MX (I) = 1
                        MOVE X"1010" TO CWBOXS-TEXT (I) (77: 2)
                        MOVE 1       TO MY          (TAMANHO)
                   END-IF
              END-IF
           END-PERFORM
           IF   MYS NOT = ZEROS
                ADD 2 TO MAIOR
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                        IF  MY (I) = 1
                            MOVE MORE TO ITEM (I) (MAIOR: 1)
                        END-IF
                END-PERFORM
           END-IF
           ADD  1        TO TAMANHO MAIOR
           MOVE M-201    TO ESQUERDA (1)
           MOVE M-200    TO ESQUERDA (TAMANHO)

           IF   MAIOR < MAIOR-TIT
                ADD  1 TO MAIOR
           END-IF

           IF   ARROW-OFF = 0
                IF   CWUNIX-OFF
                     MOVE X"1A"  TO CWBOXS-T (MAIOR)
                ELSE
                     MOVE ">"    TO CWBOXS-T (MAIOR)
                END-IF
           END-IF

           ADD  1           TO MAIOR

           MOVE M-187    TO BYTE-JANE (1 MAIOR)
           MOVE M-188    TO BYTE-JANE (TAMANHO MAIOR)
txt   *    CALL "CWTEXT" USING TITULO LENGTH OF TITULO
           INSPECT TITULO CONVERTING SPACE TO M-205
           INSPECT TITULO CONVERTING X"5F" TO SPACE

           PERFORM VARYING LINHA FROM 1 BY 1 UNTIL LINHA > TAMANHO
                   IF   LINHA = 1 OR TAMANHO
                        PERFORM VARYING COLUNA FROM 1 BY 1
                                  UNTIL COLUNA = MAIOR
                           IF   LINHA = 1
                                MOVE CWBOXS-T (COLUNA) TO BYTE-JANE
                                             (LINHA COLUNA)
                                IF   BYTE-JANE (LINHA COLUNA) = "_"
                                     MOVE SPACE TO BYTE-JANE
                                                   (LINHA COLUNA)
                                END-IF
                           ELSE
                                MOVE M-205 TO BYTE-JANE (LINHA COLUNA)
                           END-IF
                        END-PERFORM
                   ELSE
                        MOVE M-186 TO ESQUERDA  (LINHA)
                        MOVE M-186 TO BYTE-JANE (LINHA MAIOR)
                   END-IF
           END-PERFORM

           MOVE SPACE        TO CWBOXS-T (MAIOR - 1)
           MOVE CWBOXS-LINE  TO LINHA-2
           ADD  1            TO MAIOR
           MOVE ALL "0"      TO DESTACADOS
           PERFORM VARYING LINHA FROM 1 BY 1
                           UNTIL LINHA > TAMANHO
                   MOVE CWBOXS-COLUMN TO COLUNA-2
                   PERFORM VARYING COLUNA FROM 1 BY 1
                           UNTIL COLUNA > MAIOR
                           MOVE BYTE-J (LINHA COLUNA)
                             TO BYTE (LINHA-2 COLUNA-2)
                           IF   BYTE (LINHA-2 COLUNA-2) = "_"
                                MOVE SPACE TO BYTE (LINHA-2 COLUNA-2)
                           END-IF
                           IF  (LINHA  = 1)
                           OR  (COLUNA = 1)
                           OR  (COLUNA = MAIOR)
                           OR  (LINHA  = TAMANHO)
                                MOVE ATT-B TO ATRIBUTO
                                                 (LINHA-2 COLUNA-2)
                           ELSE
                                IF COR-TAB (LINHA) = 0
                                   MOVE ATT-T TO ATRIBUTO
                                                    (LINHA-2 COLUNA-2)
                                ELSE
                                   MOVE COR-TAB (LINHA) TO COR-N
                                   MOVE COR-N(1:1) TO ATRIBUTO
                                                    (LINHA-2 COLUNA-2)
                                END-IF
                                IF  BYTE (LINHA-2 COLUNA-2) =
                                    CHAR (LINHA)
                                AND(CHAR (LINHA) NOT = SPACE)
                                AND DESTACADO (LINHA) = "0"
                                AND (CWGETL-HIGH = 1)
                                     MOVE ATT-H TO ATRIBUTO
                                                    (LINHA-2 COLUNA-2)
                                     MOVE "1" TO DESTACADO (LINHA)
                                END-IF
                           END-IF
                           ADD 1 TO COLUNA-2
                           IF   COLUNA-2 > 80
                                MOVE 1 TO COLUNA-2
                           END-IF
                           IF ((COLUNA = MAIOR
                           AND  COLUNA-2 < 80)
                           OR  (LINHA = TAMANHO
                           AND  LINHA-2  < 25
                           AND  COLUNA-2 < 80))
                           AND (COLUNA-2 NOT = CWBOXS-COLUMN + 1)
                                MOVE SOMBRA TO ATRIBUTO
                                           (LINHA-2 + 1 COLUNA-2)
                                              ATRIBUTO
                                           (LINHA-2 + 1 COLUNA-2 + 1)
                           END-IF
                   END-PERFORM
                   ADD  1 TO LINHA-2
                   IF   LINHA-2 > 25
                        MOVE 1 TO LINHA-2
                   END-IF
           END-PERFORM

           MOVE TELA                      TO CARACTER-BUFFER
           MOVE ATRIBUTOS                 TO ATTRIBUTE-BUFFER
           MOVE 2000                      TO STRING-LENGTH
           MOVE 0                         TO ROW-NUMBER
                                             COLUMN-NUMBER
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH

           COMPUTE LINHA         = CWBOXS-LINE   + 1
           MOVE    CWBOXS-LINE  TO ROW-MIN
           COMPUTE ROW-MAX       = ROW-MIN + MAX-OP - 1

           COMPUTE COLUNA         = CWBOXS-COLUMN + 1
           MOVE    COLUNA        TO COLUMN-MIN
           COMPUTE SIZE-BARR-MENU = MAIOR - 2
           COMPUTE COLUMN-MAX     = COLUMN-MIN
                                  + SIZE-BARR-MENU - 1

           MOVE ZERO TO TECLA

           IF   CWBOXS-OPTION > MAX-OP
                MOVE MAX-OP TO CWBOXS-OPTION
           END-IF

           IF   CWBOXS-OPTION = 0
                MOVE 2    TO E
           ELSE
                COMPUTE E = CWBOXS-OPTION + 1
                COMPUTE LINHA = LINHA + CWBOXS-OPTION - 1
           END-IF

           CALL "CBL_GET_CSR_POS" USING SCREEN-POSITION
           MOVE SCREEN-POSITION       TO SALVA-CURSOR
           MOVE 255                   TO ROW-NUMBER
                                         COLUMN-NUMBER
           CALL "CBL_SET_CSR_POS"  USING SCREEN-POSITION
           MOVE 0                     TO CWBOXS-KEY
           SET  CWBOXS-TIMEOUT-OFF TO TRUE
           IF   CWGETL-TIMEOUT NOT = 0
           AND  CWBOXS-TIMEOUT-ENABLE
                ACCEPT HORA-A FROM TIME
                MOVE 0 TO SEGUNDOS
           END-IF

           MOVE spaces TO CWBOXS-EXIT-CHAR
           PERFORM UNTIL EDIT-ESC
                      OR EDIT-ENTER
                      OR EDIT-CURSOR-LEFT
                      OR EDIT-CURSOR-RIGHT
                      OR CWBOXS-KEY NOT = 0
                      OR(CWBOXS-EXIT-CHAR NOT = SPACES)
                   MOVE ZERO           TO TECLA
                   MOVE ITEM (E)       TO CARACTER-BUFFER
                   MOVE E              TO E-ANT
                   MOVE COLUNA         TO COLUMN-NUMBER
                   MOVE LINHA          TO ROW-NUMBER
                   SUBTRACT 1        FROM COLUMN-NUMBER
                                          ROW-NUMBER
                   MOVE REVERTE-VIDEO             TO ATTRIBUTE-BARR
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                     ATTRIBUTE-BARR
                                                     SIZE-BARR-MENU
                   perform acende-na-barra
                   PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                                            OR MOUSE-EVENT-TYPE > 1
                                            OR MULTI-USER = 1
                           CALL "CWATCH"
                           CALL "CBL_GET_KBD_STATUS" USING KEY-STATUS
                           IF   CWGETL-MOUSE = 1
                                CALL "CBL_GET_MOUSE_POSITION"
                                      USING MOUSE-HANDLE
                                            MOUSE-POSITION
                                  ON   EXCEPTION
                                       CONTINUE
                                END-CALL
                                IF MOUSE-POSITION-A NOT = MOUSE-POSITION
                                   MOVE 0 TO SEGUNDOS
                                END-IF
                                MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                                CALL "CBL_READ_MOUSE_EVENT" USING
                                                            MOUSE-HANDLE
                                                            MOUSE-DATA
                                                         MOUSE-READ-TYPE
                                ON   EXCEPTION
                                     CONTINUE
                                END-CALL
                                IF MOUSE-EVENT-TYPE = 2 OR 3
                                   MOVE ROW-MOUSE    TO ROW-NUMBER-2
                                   MOVE COLUMN-MOUSE TO COLUMN-NUMBER-2
                                   CALL "CBL_READ_SCR_CHARS"
                                    USING SCREEN-POSITION-MOUSE
                                                 BYTE-MOUSE
                                                 BYTE-MOUSE-LENGTH
                                       ON EXCEPTION
                                          CONTINUE
                                   END-CALL
                                END-IF
                           END-IF
                           IF (((COLUMN-MOUSE NOT < (COLUMN-MIN - 1))
                           AND  (COLUMN-MOUSE NOT > (COLUMN-MAX - 1))
                           AND  (ROW-MOUSE    NOT < ROW-MIN)
                           AND  (ROW-MOUSE    NOT > ROW-MAX))
                           OR  (BYTE-MOUSE = X"1B" OR X"1A"))
                           and (MOUSE-POSITION not = MOUSE-POSITION-s
                             or MOUSE-EVENT-TYPE = 2 OR 3)
                                COMPUTE E = ROW-MOUSE - ROW-MIN + 2
                                IF  (E NOT = E-ANT)
                                AND (BYTE-MOUSE NOT = X"1B")
                                AND (BYTE-MOUSE NOT = X"1A")
                                    MOVE E     TO E-ANT
                                    MOVE 0     TO SEGUNDOS
                                    MOVE ATT-T TO ATTRIBUTE-BARR
                                    CALL "CBL_WRITE_SCR_ATTRS"
                                           USING SCREEN-POSITION
                                                     S-ATT (LINHA)
                                              (COLUNA: STRING-LENGTH)
                                                     STRING-LENGTH
                                    MOVE ROW-MOUSE TO LINHA
                                                      ROW-NUMBER
                                    ADD  1       TO LINHA
                                    MOVE ITEM(E) TO CARACTER-BUFFER
                                    MOVE REVERTE-VIDEO TO ATTRIBUTE-BARR
                                    CALL "CBL_WRITE_SCR_N_ATTR"
                                          USING SCREEN-POSITION
                                                ATTRIBUTE-BARR
                                                SIZE-BARR-MENU
                                    perform acende-na-barra
                                END-IF
                            ELSE
                                IF   MOUSE-EVENT-TYPE > 1
                                     MOVE 4 TO MOUSE-EVENT-TYPE
                                ELSE
                                     MOVE 0 TO MOUSE-EVENT-TYPE
                                END-IF
                           END-IF
                           move MOUSE-POSITION to MOUSE-POSITION-s
                           IF   CWGETL-TIMEOUT NOT = 0
                           AND  CWBOXS-TIMEOUT-ENABLE
                                ACCEPT HORA-B FROM TIME
                                IF   HORA-B NOT = HORA-A
                                     MOVE HORA-B TO HORA-A
                                     ADD 1 TO SEGUNDOS
                                     IF   SEGUNDOS NOT < CWGETL-TIMEOUT
                                          MOVE 1 TO KEY-STATUS
                                          SET CWBOXS-TIMEOUT-ON TO TRUE
                                          SET EDIT-ESC TO TRUE
                                     END-IF
                                END-IF
                           END-IF
                   END-PERFORM
                   IF   MOUSE-EVENT-TYPE = 0
                   AND  CWBOXS-TIMEOUT-OFF
                        MOVE 0 TO SEGUNDOS
                        COMPUTE CURPOS-LIN = ROW-NUMBER    + 1
                        COMPUTE CURPOS-COL = COLUMN-NUMBER + 1
<SCM> *                 CALL "CWKBDC" USING CURPOS CARACTER TECLA
                        CALL "CWKBDC" USING "0000" CARACTER TECLA
                        PERFORM 030-ALT THRU 030-99-FIM
                        INSPECT CARACTER
                                CONVERTING MINUSCULAS TO MAIUSCULAS
                        IF   CARACTER = X"00"
                             MOVE SPACE      TO CARACTER
                             IF   CWBOXS-KEY-ON = "Y" OR "y"
                                               OR "S" OR "s"
                                  EVALUATE TRUE
                                     WHEN EDIT-F1  MOVE 02 TO CWBOXS-KEY
                                     WHEN EDIT-F2  MOVE 03 TO CWBOXS-KEY
                                     WHEN EDIT-F3  MOVE 04 TO CWBOXS-KEY
                                     WHEN EDIT-F4  MOVE 05 TO CWBOXS-KEY
                                     WHEN EDIT-F5  MOVE 06 TO CWBOXS-KEY
                                     WHEN EDIT-F6  MOVE 07 TO CWBOXS-KEY
                                     WHEN EDIT-F7  MOVE 08 TO CWBOXS-KEY
                                     WHEN EDIT-F8  MOVE 09 TO CWBOXS-KEY
                                     WHEN EDIT-F9  MOVE 10 TO CWBOXS-KEY
                                     WHEN EDIT-F10 MOVE 11 TO CWBOXS-KEY
                                  END-EVALUATE
                                  IF   CWBOXS-KEY NOT = 0
                                       SET EDIT-ENTER TO TRUE
                                  END-IF
                             ELSE
                                  IF   CWBOXS-KEY-ON = "*"
                                  AND (NOT EDIT-CURSOR-DOWN)
                                  AND (NOT EDIT-CURSOR-UP)
                                  AND (NOT EDIT-ESC)
                                  AND (NOT EDIT-CURSOR-LEFT)
                                  AND (NOT EDIT-CURSOR-RIGHT)
                                  AND (NOT EDIT-ENTER)
                                       MOVE TECLA TO TECLA-X
                                       SET EDIT-ENTER TO TRUE
                                  END-IF
                             END-IF
                        END-IF
                   ELSE
                        IF   MOUSE-EVENT-TYPE = 2 OR 3
                             SET EDIT-ENTER TO TRUE
                        ELSE
                             SET EDIT-ESC TO TRUE
                        END-IF
                   END-IF
                   IF   BYTE-MOUSE = X"1A" OR "<"
                        SET EDIT-CURSOR-RIGHT TO TRUE
                   END-IF
                   IF   BYTE-MOUSE = X"1B" OR ">"
                        SET EDIT-CURSOR-LEFT  TO TRUE
                   END-IF
                   MOVE ATT-T                     TO ATTRIBUTE-BARR
                   CALL "CBL_WRITE_SCR_ATTRS"  USING SCREEN-POSITION
                                                     S-ATT (LINHA)
                                              (COLUNA: STRING-LENGTH)
                                                     STRING-LENGTH
                   IF  (NOT (EDIT-ESC
                         OR EDIT-ENTER
                         OR EDIT-CURSOR-LEFT
                         OR EDIT-CURSOR-RIGHT))
                   AND  CARACTER NOT = SPACE
                        PERFORM VARYING I FROM 1 BY 1
                                UNTIL I > 21
                                IF   CARACTER = CHAR (I)
                                     MOVE I  TO E
                                     SET EDIT-ENTER TO TRUE
                                     MOVE 23 TO I
                                END-IF
                        END-PERFORM
                      IF CWBOXS-LEAVING
                      AND (NOT EDIT-ENTER)
                          MOVE CARACTER TO CWBOXS-EXIT-CHAR
                      END-IF
                   ELSE
                        IF   EDIT-ENTER
                             MOVE CHAR (E) TO CARACTER
                        END-IF
                   END-IF
                   IF   EDIT-CURSOR-DOWN
                        ADD 1 TO E
                                 LINHA
                        IF    LINHA > 25
                              MOVE 1 TO LINHA
                        ELSE
                             IF   LINHA = 0
                                  MOVE 25 TO LINHA
                             END-IF
                        END-IF
                        IF    E = TAMANHO
                              MOVE 2  TO E
                              COMPUTE LINHA = CWBOXS-LINE  + 1
                              IF    LINHA > 25
                                    MOVE 1 TO LINHA
                              ELSE
                                   IF   LINHA = 0
                                        MOVE 25 TO LINHA
                                   END-IF
                              END-IF
                        END-IF
                   END-IF
                   IF   EDIT-CURSOR-UP
                        SUBTRACT 1 FROM E LINHA
                        IF    E = 1
                              COMPUTE E = TAMANHO - 1
                              COMPUTE LINHA = CWBOXS-LINE
                                         + TAMANHO - 2
                              IF    LINHA > 25
                                    MOVE 1 TO LINHA
                              ELSE
                                   IF   LINHA = 0
                                        MOVE 25 TO LINHA
                                   END-IF
                              END-IF
                        END-IF
                  END-IF
           END-PERFORM

           MOVE SPACE TO CWBOXS-ARROW

           IF   EDIT-ESC
           OR   EDIT-CURSOR-LEFT
           OR   EDIT-CURSOR-RIGHT
                MOVE ZERO  TO CWBOXS-OPTION
                MOVE SPACE TO CWBOXS-OPTION-CHAR
                IF   EDIT-CURSOR-RIGHT
                     MOVE ">" TO CWBOXS-ARROW
                ELSE
                     IF   EDIT-CURSOR-LEFT
                          MOVE "<" TO CWBOXS-ARROW
                     END-IF
                END-IF
           ELSE
                COMPUTE CWBOXS-OPTION = E - 1
                MOVE CHAR (E) TO CWBOXS-OPTION-CHAR
           END-IF

           IF CWBOXS-ERASE = "Y" OR "y" OR "s" OR "S"
              COMPUTE STRING-START = (CWBOXS-LINE * 80)
                                    - 80 + CWBOXS-COLUMN
              COMPUTE STRING-LENGTH = 2000 - STRING-START
              MOVE T-A (STRING-START: STRING-LENGTH) TO CARACTER-BUFFER
              MOVE A-A (STRING-START: STRING-LENGTH) TO ATTRIBUTE-BUFFER
              COMPUTE ROW-NUMBER               = CWBOXS-LINE   - 1
              COMPUTE COLUMN-NUMBER            = CWBOXS-COLUMN - 1
              CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 STRING-LENGTH
           ELSE
                IF CWBOXS-OPTION NOT = 0
                   COMPUTE ROW-NUMBER = CWBOXS-LINE + CWBOXS-OPTION - 1
                   MOVE REVERTE-VIDEO TO REVERTE-BLINK (1: 1)
                   ADD 128 TO REVERTE-BLINK
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                     REVERTE-BLINK
                                                     SIZE-BARR-MENU
                END-IF
           END-IF

           MOVE SALVA-CURSOR         TO SCREEN-POSITION
           CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION

           IF   CWGETL-MOUSE = 1
                CALL "CBL_TERM_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON EXCEPTION
                        CONTINUE
                END-CALL
           END-IF
           CALL "CWATCH".
           IF  CWBOXS-EXIT-CHAR NOT = SPACES
               MOVE CWBOXS-EXIT-CHAR TO CWBOXS-OPTION-CHAR
               MOVE 99               TO CWBOXS-OPTION
           END-IF

frango     subtract JANLIN FROM CWBOXS-LINE
frango     subtract JANCOL FROM CWBOXS-COLUMN.

       000-99-FIM. EXIT PROGRAM.

       acende-na-barra.

==>PDS     IF CWGETL-HIGH = 1
==>PDS        MOVE COLUNA          TO CC
==>PDS        MOVE SCREEN-POSITION TO SPC
==>PDS        PERFORM VARYING KC FROM 1 BY 1
==>PDS                  UNTIL KC > SIZE-BARR-MENU
==>PDS                IF  BYTE (LINHA CC) = CHAR (E)
==>PDS                AND(CHAR (E) NOT = SPACE)
==>PDS                    CALL "CBL_WRITE_SCR_N_ATTR"
==>PDS                         USING SPC ATT-HB X"0001"
==>PDS                    EXIT PERFORM
==>PDS                END-IF
==>PDS                ADD 1 TO CN CC
==>PDS        END-PERFORM
==>PDS     END-IF.

       fim-avende-na-barra. exit.
       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.
       030-ALT.

           MOVE SPACE TO LETRA
           EVALUATE TRUE
               WHEN EDIT-ALT-0           MOVE "0" TO LETRA
               WHEN EDIT-ALT-1           MOVE "1" TO LETRA
               WHEN EDIT-ALT-2           MOVE "2" TO LETRA
               WHEN EDIT-ALT-3           MOVE "3" TO LETRA
               WHEN EDIT-ALT-4           MOVE "4" TO LETRA
               WHEN EDIT-ALT-5           MOVE "5" TO LETRA
               WHEN EDIT-ALT-6           MOVE "6" TO LETRA
               WHEN EDIT-ALT-7           MOVE "7" TO LETRA
               WHEN EDIT-ALT-8           MOVE "8" TO LETRA
               WHEN EDIT-ALT-9           MOVE "9" TO LETRA
               WHEN EDIT-ALT-A           MOVE "A" TO LETRA
               WHEN EDIT-ALT-B           MOVE "B" TO LETRA
               WHEN EDIT-ALT-C           MOVE "C" TO LETRA
               WHEN EDIT-ALT-D           MOVE "D" TO LETRA
               WHEN EDIT-ALT-E           MOVE "E" TO LETRA
               WHEN EDIT-ALT-EQUAL       MOVE "=" TO LETRA
               WHEN EDIT-ALT-F           MOVE "F" TO LETRA
               WHEN EDIT-ALT-G           MOVE "G" TO LETRA
               WHEN EDIT-ALT-H           MOVE "H" TO LETRA
               WHEN EDIT-ALT-I           MOVE "I" TO LETRA
               WHEN EDIT-ALT-J           MOVE "J" TO LETRA
               WHEN EDIT-ALT-K           MOVE "K" TO LETRA
               WHEN EDIT-ALT-L           MOVE "L" TO LETRA
               WHEN EDIT-ALT-M           MOVE "M" TO LETRA
               WHEN EDIT-ALT-TRACE       MOVE "-" TO LETRA
               WHEN EDIT-ALT-N           MOVE "N" TO LETRA
               WHEN EDIT-ALT-O           MOVE "O" TO LETRA
               WHEN EDIT-ALT-P           MOVE "P" TO LETRA
               WHEN EDIT-ALT-Q           MOVE "Q" TO LETRA
               WHEN EDIT-ALT-R           MOVE "R" TO LETRA
               WHEN EDIT-ALT-S           MOVE "S" TO LETRA
               WHEN EDIT-ALT-T           MOVE "T" TO LETRA
               WHEN EDIT-ALT-U           MOVE "U" TO LETRA
               WHEN EDIT-ALT-V           MOVE "V" TO LETRA
               WHEN EDIT-ALT-W           MOVE "W" TO LETRA
               WHEN EDIT-ALT-X           MOVE "X" TO LETRA
               WHEN EDIT-ALT-Y           MOVE "Y" TO LETRA
               WHEN EDIT-ALT-Z           MOVE "Z" TO LETRA
           END-EVALUATE

           IF LETRA NOT = SPACE
              MOVE SPACE TO CARACTER
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                      IF LETRA = CHAR (I)
                         MOVE LETRA TO CARACTER
                         EXIT PERFORM
                      END-IF
              END-PERFORM
              IF SPACE = CARACTER
                 CALL X"E5"
              END-IF
           END-IF.

       030-99-FIM. EXIT.
       END PROGRAM CWBOXS.
