       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEXMN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/03/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Extrai definiáao de menus para cwmenu.mnu    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MENUS ASSIGN  TO DISK
                  ORGANIZATION  LINE SEQUENTIAL
                  FILE STATUS   IS FS-MENUS.

           SELECT BIN   ASSIGN  TO DISK
                  ORGANIZATION  BINARY SEQUENTIAL
                  FILE STATUS   IS FS-MENUS.

       DATA DIVISION.
       FILE SECTION.

       FD  MENUS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-MENUS.

       01  MENUS-REG.
           05 MENUS-SISTEMA     PIC  X(002).
           05 MENUS-PONTO       PIC  X(001).
           05 MENUS-ITEM.
              10 MENUS-menu     PIC  X(001).
              10 FILLER         PIC  X(001).
              10 MENUS-OPCAO    PIC  9(002).
           05 FILLER            PIC  X(001).
           05 MENUS-PROGRAMA    PIC  X(008).
           05 MENUS-PLUS        PIC  X(001).
           05 MENUS-MODULO      PIC  X(001).
           05 FILLER            PIC  X(001).
           05 MENUS-DESCRICAO   PIC  X(034).
           05 FILLER            PIC  X(001).
           05 MENUS-HELP        PIC  X(020).
           05 MENUS-NIVEL       PIC  Z(001).
           05 MENUS-CHECK       PIC  X(001).
           05 MENUS-PASS        PIC  X(006).

       FD  BIN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-MENUS.

       01  BIN-REG.
           05 BIN-REGISTRO PIC X(082).
           05 BIN-FATOR    PIC 9(002) COMP-X.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 BIN-LENGTH             PIC  9(002) COMP-X.
           05 OP                     PIC  9(001) VALUE 0.
           05 LETRA           COMP-X PIC  9(002) VALUE 0.
           05 OPCAO                  PIC  9(002) VALUE ZERO.
           05 check-n                PIC  9(002) VALUE ZERO.
           05 check-opcao            PIC  9(002) VALUE ZERO.
           05 check-plus             PIC  X(001) VALUE SPACE.
           05 check-reg              PIC  X(007) VALUE SPACE.
           05 salva                  PIC  X(082) VALUE spaces.
           05 OPCAO-K                PIC  9(002) VALUE ZERO.
           05 PORTAL                 PIC  9(001) VALUE 1.
           05 PAGINAX                PIC  9(004) VALUE ZERO.
           05 U                      PIC  9(004) VALUE ZERO.
           05 TP                     PIC  X(001) VALUE SPACE.
           05 IP                     PIC  9(002) VALUE 0.
           05 YP                     PIC  9(002) VALUE 0.
           05 SM-FIL                 PIC  9(001) VALUE 0.
           05 SUB-CWCONF.
              10 SM-ATT              PIC  9(004) OCCURS 7.
           05 SUB-CWCONF-2           PIC  X(028) VALUE SPACES.
           05 MINUSCULAS             PIC  X(049) VALUE
              "abcdefghijklmnopqrstuvwxyz†Ç°¢£ÏÖäçïóÑâîÅÉàåìñ∆‰á".
           05 MAIUSCULAS             PIC  X(049) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZµê÷‡ÈÌ∑‘Î„Îé”ôö∂“◊‚Í«ÂÄ".
           05 N                        PIC  9(002) VALUE 0.
           05 N2                       PIC  9(002) VALUE 0.
           05 SALVAS.
              10 SAVEX OCCURS 6.
                 15 SAVE-CHAVE PIC X(32).
                 15 SAVE-I     PIC 9(02).
                 15 SAVE-Y     PIC 9(02).
           05 ER-MENUS.
              10 FS-MENUS              PIC  X(002) VALUE "00".
              10 LB-MENUS              PIC  X(255) VALUE "cwmenu.txt".
           05 ER-SEGURANCA.
              10 FS-SEGURANCA          PIC  X(002) VALUE "00".
              10 LB-SEGURANCA          PIC  X(255) VALUE "exim####".
           05 MODULO                   PIC  X(025) VALUE SPACES.
           05 CANCELAR                 PIC  X(001) VALUE SPACES.
           05 PAGINA                   PIC  9(004) VALUE 0.
           05 PAGINAS                              VALUE ALL "0".
              10 PG OCCURS 10          PIC  9(004).
           05 SISTEMAS                 PIC  9(002) VALUE 0.
           05 NIVEL                    PIC  9(001) VALUE 0.
           05 OPTION                   PIC  9(001) VALUE 1.
           05 PROCESSAR                PIC  X(001) VALUE SPACE.
           05 PROCESSAR-A              PIC  X(001) VALUE SPACE.
           05 SISTEMA-A                PIC  X(002) VALUE SPACE.
           05 menu-A                   PIC  X(001) VALUE SPACES.
           05 WK-MENU                  PIC  X(001) VALUE SPACES.
           05 X                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 Z                        PIC  9(003) VALUE 0.
           05 I                        PIC  9(004) VALUE 0.
           05 I2                       PIC  9(002) VALUE 0.
           05 I3                       PIC  9(002) VALUE 0.
           05 P                        PIC  9(002) VALUE 0.
           05 RETURN-STATUS            PIC  9(002) COMP-5 VALUE 0.
           05 SIZE-OLD-DIR             PIC  9(002) COMP-5 VALUE 50.
           05 OLD-DRIVE                PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY            PIC  X(255) VALUE SPACES.

       01  FS-CWCONF PIC  X(002) VALUE "00".
       01  KCO       PIC  X(030) VALUE "CHAVE".
       01  PCO       PIC  X(002) VALUE SPACE.
       01  KGR       PIC  X(030) VALUE "CHAVE".
       01  PGR       PIC  X(002) VALUE SPACE.

       01  CWCONF-REG99.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO                        PIC  X(002).
              10 CWCONF-PAGINA                      PIC  9(004).
              10 FILLER                             PIC  X(026).
           05 CWCONF-OPCOES.
              10 CWCONF-OPCAO OCCURS 26.
                 15 CWCONF-NIVEL                    PIC  9(001).
                 15 CWCONF-CHECK                    PIC  X(001).
                 15 CWCONF-FATOR-P-99        COMP-X PIC  9(002).
                 15 CWCONF-FATOR-S-99        COMP-X PIC  9(002).
                 15 CWCONF-PROG                     PIC  X(008).
                 15 CWCONF-SIZE-P-99         COMP-X PIC  9(002).
                 15 CWCONF-SIZE-S-99         COMP-X PIC  9(002).
                 15 CWCONF-PASS                     PIC  X(006).
                 15 CWCONF-HELP                     PIC  X(020).
                 15 CWCONF-NO-OPCAO                 PIC  9(002).
                 15 CWCONF-NM-OPCAO                 PIC  X(034).

       COPY CWPATH.
       COPY CWUNIX.
       COPY CWSEND.
       COPY CWSQLC.

       PROCEDURE DIVISION.

       000-INICIO.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           MOVE 046 TO CWPATH-COLOR-FRAME
                       CWPATH-COLOR-BORDER
           MOVE 126 TO CWPATH-COLOR-BARR-MENU
           SET  CWPATH-WITH-DIR      TO TRUE
           SET  CWPATH-WITH-DRIVES   TO TRUE
           SET  CWPATH-WITH-NEWDIR   TO TRUE
           SET  CWPATH-WITH-NEWFILE  TO TRUE
           MOVE "_Exportar_para:"    TO CWPATH-TITLE
           MOVE "cwmenu.mnu"         TO CWPATH-DEFAULT
           MOVE SPACES               TO CWPATH-FILE
                                        CWPATH-PATH
           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
           IF  CWUNIX-OFF
               CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                           RETURN-STATUS
               STRING OLD-DRIVE DELIMITED BY SPACE
                      ":\"           DELIMITED BY SPACE
                      OLD-DIRECTORY  DELIMITED BY SPACE
                      "\*.CW"       DELIMITED BY SIZE
               INTO CWPATH-PATH
           ELSE
               STRING OLD-DIRECTORY  DELIMITED BY SPACE
                      "/*.CW"       DELIMITED BY SIZE
               INTO CWPATH-PATH
           END-IF
           CALL "CWPATH" USING PARAMETROS-CWPATH.
           IF CWPATH-FILE = SPACES
              GOBACK
           END-IF
           MOVE CWPATH-FILE TO LB-MENUS
           OPEN INPUT MENUS
           IF   FS-MENUS < "10"
                MOVE SPACES TO CWSEND-MSG
                CLOSE MENUS
                STRING "Arquivo: " DELIMITED BY SIZE
                   LB-MENUS        DELIMITED BY SPACE
                   " j† existe !"  DELIMITED BY SIZE
                      INTO CWSEND-MSG
                MOVE "~Sobrepor "   TO CWSEND-SCREEN (1)
                MOVE "~Outro"       TO CWSEND-SCREEN (2)
                MOVE "~Cancelar"    TO CWSEND-SCREEN (3)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                EVALUATE CWSEND-OPTION-CHAR
                         WHEN "S" CONTINUE
                         WHEN "O" GO TO 000-INICIO
                         WHEN OTHER GOBACK
                END-EVALUATE
           END-IF

           EXEC COBOLware Send Message 'Formato:'
                Caption(1) '~Texto'
                Caption(2) '~Bin†rio'
                Caption(3) '~Cancelar'
                Option;OP
           END-EXEC

           EVALUATE OP
                 WHEN 1 OPEN OUTPUT MENUS
                 WHEN 2 OPEN OUTPUT BIN
                 WHEN OTHER GOBACK
           END-EVALUATE

           IF   FS-MENUS NOT = "00"
                CALL "CWISAM" USING ER-MENUS
                GOBACK
           END-IF

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
           MOVE '99' TO CWCONF-CHAVE
           SET CWSQLC-START     TO TRUE
           SET CWSQLC-NOT-LESS  TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                 OR CWCONF-TIPO > "99"
                                 OR CANCELAR = 'Y'
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-NEXT TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                       FS-CWCONF KCO PCO
                   IF   FS-CWCONF < '10'
                   AND  CWCONF-TIPO = "99"
                        MOVE 0 TO OPCAO-K
                        PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 26
                        IF  CWCONF-PROG (I) NOT = SPACES
                            IF N = 0
                               ADD 1 TO SISTEMAS
                               MOVE 1 TO OPCAO
                            END-IF
                            CALL "CWCODE" USING "D"
                                CWCONF-SIZE-P-99  (I)
                                CWCONF-FATOR-P-99 (I)
                                CWCONF-PROG       (I)
                            INSPECT CWCONF-PROG (I)
                                    CONVERTING MINUSCULAS
                                            TO MAIUSCULAS
                            IF  CWCONF-PASS (I) NOT = SPACES
                                CALL "CWCODE" USING "D"
                                      CWCONF-SIZE-S-99  (I)
                                      CWCONF-FATOR-S-99 (I)
                                      CWCONF-PASS       (I)
                            END-IF
                            MOVE SISTEMAS TO MENUS-REG
                            MOVE CWCONF-NIVEL(I) TO MENUS-NIVEL
                            MOVE CWCONF-CHECK(I) TO MENUS-CHECK
                            MOVE CWCONF-PASS (I) TO MENUS-PASS
                            MOVE CWCONF-NM-OPCAO (I)
                              TO MENUS-DESCRICAO
                            IF CWCONF-HELP (I) (5:1) = ':'
                               MOVE CWCONF-HELP (I) (06:2)
                                 TO MENUS-PROGRAMA (1:2)
                               MOVE CWCONF-HELP (I) (09:2)
                                 TO MENUS-PROGRAMA (3:2)
                               MOVE CWCONF-HELP (I) (12:2)
                                 TO MENUS-PROGRAMA (5:2)
                               MOVE CWCONF-HELP (I) (15:2)
                                 TO MENUS-PROGRAMA (7:2)
                               MOVE '*' TO MENUS-PONTO
                               MOVE 65  TO LETRA
                            ELSE
                               IF LETRA = 0
                               OR (PORTAL = 0 AND OPCAO-K = 0)
                                  MOVE 0   TO PORTAL
                                  MOVE 1   TO OPCAO-K
                                  MOVE 97  TO LETRA
                                  MOVE '*' TO MENUS-PONTO
                               END-IF
                               MOVE '.'   TO MENUS-PONTO
                               IF N = 2
                                  MOVE I TO OPCAO
                               END-IF
                            END-IF
                            IF  CWCONF-PROG(I) = 'CWBOXS' OR 'GRBOXS'
      *                                      OR  'CWPAGE' OR 'GRPAGE'
                            AND N < 5
                                IF  CWCONF-HELP (I) (5:1) NOT = ':'
                                    MOVE '+'        TO MENUS-PLUS
                                    MOVE LETRA(1:1) TO MENUS-menu
                                    IF N = 2
                                       MOVE '.' TO MENUS-ITEM (2:1)
                                       MOVE OPCAO TO MENUS-OPCAO
                                    END-IF
                                END-IF
                                IF PORTAL = 0
                                   MOVE '.' TO MENUS-ITEM (2:1)
                                   MOVE I     TO MENUS-OPCAO
                                END-IF
                                PERFORM GRAVA-MENU
                                ADD  1            TO N
                                MOVE CWCONF-CHAVE TO SAVE-CHAVE(N)
                                PERFORM TOPAGINA
                                PERFORM VARYING Y FROM 26 BY -1
                                        UNTIL CWCONF-PROG(Y)
                                              NOT = SPACES
                                            OR Y = 1
                                        CONTINUE
                                END-PERFORM
                                MOVE Y TO SAVE-Y (N)
                            ELSE
                                IF N > 0
                                   IF I = SAVE-Y (N)
                                      MOVE '-' TO MENUS-PLUS
                                   END-IF
                                END-IF
                                IF  PORTAL = 0
                                    MOVE I     TO MENUS-OPCAO
                                ELSE
                                    MOVE OPCAO TO MENUS-OPCAO
                                END-IF
                                MOVE LETRA(1:1) TO MENUS-menu
                                MOVE '.' TO MENUS-ITEM (2:1)
                                MOVE CWCONF-PROG(I)  TO MENUS-PROGRAMA
                                MOVE CWCONF-HELP(I)  TO MENUS-HELP
                                PERFORM GRAVA-MENU
                                EXEC COBOLware Process
                                     MSG LB-MENUS
                                     CANCEL;CANCELAR
                                END-EXEC
                            END-IF
                        END-IF
                        IF I = 26
                           IF N > 0
                              MOVE SAVE-CHAVE(N) TO CWCONF-REG99
                              MOVE SAVE-I (N)    TO I
                              SUBTRACT 1 FROM N
                              SET CWSQLC-READ TO TRUE
                              SET CWSQLC-EQUAL TO TRUE
                              CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                                  FS-CWCONF KCO PCO
                              IF N = PORTAL
                                 ADD 1 TO LETRA
                              END-IF
                           END-IF
                        END-IF
                   END-IF
           END-PERFORM
           CLOSE MENUS
           EXEC COBOLware Process Close
           END-EXEC
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO.

       000-99-FIM. GOBACK.

       TOPAGINA.

           MOVE 0        TO PAGINAX
           MOVE 7        TO SM-FIL
           MOVE 5        TO YP
           INITIALIZE SUB-CWCONF
           PERFORM VARYING IP FROM 20 BY -1
                   UNTIL IP = 0
                      OR SM-FIL < 1
                   IF IP = 20
                   AND cwconf-help(i) (5:1) = ':'
                       MOVE 4 TO IP
                   END-IF
                   MOVE cwconf-help(i) (IP: 1) TO TP
                   IF TP NUMERIC
                   AND YP > 1
                       SUBTRACT 1 FROM YP
                       MOVE     TP  TO PAGINAX (YP: 1)
                   ELSE
                       IF   TP = ","
                            MOVE PAGINAX TO SM-ATT (SM-FIL)
                            MOVE 0       TO PAGINAX
                            MOVE 5       TO YP
                            SUBTRACT 1 FROM SM-FIL
                       END-IF
                   END-IF
           END-PERFORM

           IF   SM-FIL NOT = 7
                MOVE PAGINAX TO SM-ATT (SM-FIL)
                IF  SM-FIL NOT = 1
                    COMPUTE U = (SM-FIL - 1) * 4 + 1
                    MOVE SUB-CWCONF (U: )      TO SUB-CWCONF-2
                    MOVE SUB-CWCONF-2          TO SUB-CWCONF
                    INSPECT SUB-CWCONF CONVERTING SPACE
                                               TO ZERO
                END-IF
           END-IF
           IF   SUB-CWCONF NOT = ZEROS
                MOVE SM-ATT (1) TO PAGINAX
           END-IF
           IF CWCONF-PROG(I) = 'CWBOXS' OR 'GRBOXS'
              MOVE 'SM' TO CWCONF-REG99
           END-IF
           MOVE PAGINAX TO CWCONF-PAGINA
           MOVE I TO SAVE-I (N)
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO
           IF  FS-CWCONF > '09'
               SUBTRACT 1 FROM N
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING I FROM 26 BY -1
                   UNTIL CWCONF-PROG(I)
                         NOT = SPACES
                       OR I = 1
                   CONTINUE
           END-PERFORM
      *    IF CWCONF-PROG(I) = 'CWPAGE' OR 'CWBOXS'
      *       ADD 1 TO I
      *    END-IF
           MOVE I TO SAVE-Y (N)
           MOVE 0 TO I.

       GRAVA-MENU.
           if  check-opcao > 0
           and MENUS-PLUS = '+'
           and check-plus = '-'
           and menus-opcao > check-opcao
           and check-n = 4
               MOVE MENUS-REG to salva
               move check-reg to menus-reg
               move '-+'      to MENUS-opcao(1:2)
               move '-'       to MENUS-PLUS
               compute n2 = n - 1
               perform n2 times
                   if op = 1
                      write menus-reg
                   else
                      move menus-reg to bin-registro
                      MOVE LENGTH OF MENUS-REG TO BIN-LENGTH
                      CALL "CWCODE" USING "D"
                                        BIN-LENGTH
                                        BIN-FATOR
                                        BIN-REGISTRO
                      end-perform
               MOVE salva     to MENUS-REG
           end-if
           move menus-opcao to check-opcao
           move MENUS-PLUS  to check-plus
           move MENUS-REG   to check-reg
           move n           to check-n
           IF OP = 1
              WRITE MENUS-REG
           ELSE
              MOVE MENUS-REG TO BIN-REGISTRO
              MOVE LENGTH OF MENUS-REG TO BIN-LENGTH
              CALL "CWCODE" USING "D"
                                BIN-LENGTH
                                BIN-FATOR
                                BIN-REGISTRO
              WRITE BIN-REG
           END-IF
           EXEC COBOLware Process
                MSG LB-MENUS
                CANCEL;CANCELAR
           END-EXEC
       END PROGRAM CWEXMN.

