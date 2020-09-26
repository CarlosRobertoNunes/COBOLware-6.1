       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWIMMN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/03/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa defini‡ao de menus de cwmenu.mnu     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BIN   ASSIGN  TO DISK
                  ORGANIZATION  BINARY SEQUENTIAL
                  FILE STATUS   IS FS-MENUS.


           SELECT MENUS ASSIGN  TO DISK
                  ORGANIZATION  LINE SEQUENTIAL
                  FILE STATUS   IS FS-MENUS.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT SEGURANCA ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SEGURANCA-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-SEGURANCA.

           SELECT MENU99   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS MENU99-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-MENU99.

           SELECT VELHOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS VELHOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-VELHOS.

           SELECT NOVOS     ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS NOVOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-NOVOS.

           SELECT GRUPOS    ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS GRUPOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-GRUPOS.

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
              10 MENUS-OPCAO    PIC  X(002).
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

       FD  SEGURANCA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SEGURANCA.

       01  SEGURANCA-REG.
           05 SEGURANCA-CHAVE.
              10 SEGURANCA-PROG            PIC  X(008).
              10 SEGURANCA-NM-OPCAO        PIC  X(034).
           05 SEGURANCA-NIVEL              PIC  9(001).
           05 SEGURANCA-CHECK              PIC  X(001).
           05 SEGURANCA-PASS               PIC  X(006).
           05 SEGURANCA-HELP               PIC  X(020).

       FD  MENU99
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-MENU99.

       01  MENU99-REG.
           05 MENU99-CHAVE       PIC  9(008).
           05 TEXTO-99           PIC  X(034).
           05 PAGINA-99          PIC  9(004).
           05 SISTEMA-99         PIC  X(002).
           05 PROGRAMA-99        PIC  X(008).
           05 NIVEL-99           PIC  9(001).
           05 SENHA-99           PIC  X(006).
           05 HELP-99            PIC  X(020).
           05 CHECK-99           PIC  X(001).
           05 POSIT-99           PIC  XXBXXBXXBXX.

       FD  VELHOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-VELHOS.

       01  VELHOS-REG.
           05 VELHOS-CHAVE       PIC  X(008).

       FD  NOVOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-NOVOS.

       01  NOVOS-REG.
           05 NOVOS-CHAVE       PIC  X(008).

       FD  GRUPOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GRUPOS.

       01  GRUPOS-REG.
           05 GRUPOS-CHAVE       PIC  X(022).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LOCKNEW                  PIC  X(003) VALUE SPACES.
           05 WK-MENU                  PIC  X(001) VALUE SPACES.
           05 BIN-LENGTH               PIC  9(002) COMP-X.
           05 MENU-quebra              PIC  X(001) VALUE SPACES.
           05 OP                       PIC  9(001) VALUE 0.
           05 PGW                      PIC  9(004) VALUE 0.
           05 T                        PIC  9(002) VALUE 0.
           05 D                        PIC  9(002) VALUE 0.
           05 TEXTO                    PIC  X(050) VALUE SPACES.
           05 MINUSCULAS               PIC  X(049) VALUE
              "abcdefghijklmnopqrstuvwxyz ‚¡¢£ì…Š•—„‰”ƒˆŒ“–Æä‡".
           05 MAIUSCULAS               PIC  X(049) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZµÖàéí·ÔëãëŽÓ™š¶Ò×âêÇå€".
           05 ER-MENUS.
              10 FS-MENUS              PIC  X(002) VALUE "00".
              10 LB-MENUS              PIC  X(255) VALUE "cwmenu.mnu".
           05 ER-SEGURANCA.
              10 FS-SEGURANCA          PIC  X(002) VALUE "00".
              10 LB-SEGURANCA          PIC  X(255) VALUE "eximA###".
           05 ER-MENU99.
              10 FS-MENU99             PIC  X(002) VALUE "00".
              10 LB-MENU99             PIC  X(255) VALUE "eximB###".
           05 ER-VELHOS.
              10 FS-VELHOS             PIC  X(002) VALUE "00".
              10 LB-VELHOS             PIC  X(255) VALUE "eximV###".
           05 ER-NOVOS.
              10 FS-NOVOS              PIC  X(002) VALUE "00".
              10 LB-NOVOS              PIC  X(255) VALUE "eximN###".
           05 ER-GRUPOS.
              10 FS-GRUPOS             PIC  X(002) VALUE "00".
              10 LB-GRUPOS             PIC  X(255) VALUE "eximG###".
           05 MODULO                   PIC  X(025) VALUE SPACES.
           05 menu-noportal            PIC  X(001) VALUE SPACES.
           05 CANCELAR                 PIC  X(001) VALUE SPACES.
           05 LIDOS                    PIC  9(004) VALUE 0.
           05 PAGINA                   PIC  9(004) VALUE 0.
           05 PAGINAS                              VALUE ALL "0".
              10 PG OCCURS 99          PIC  9(004).
           05 SISTEMAS                 PIC  9(002) VALUE 0.
           05 NIVEL                    PIC  9(002) VALUE 0.
           05 PORTAL                   PIC  9(001) VALUE 1.
           05 OPTION                   PIC  9(001) VALUE 1.
           05 PROCESSAR                PIC  X(001) VALUE SPACE.
           05 PROCESSAR-A              PIC  X(001) VALUE SPACE.
           05 SISTEMA-A                PIC  X(002) VALUE SPACE.
           05 menu-A                   PIC  X(001) VALUE SPACES.
           05 X                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 Z                        PIC  9(003) VALUE 0.
           05 I                        PIC  9(004) VALUE 0.
           05 I2                       PIC  9(002) VALUE 0.
           05 I3                       PIC  9(002) VALUE 0.
           05 P                        PIC  9(002) VALUE 0.
           05 R-99                     PIC  9(008) VALUE 0.

       COPY CWPATH.
       COPY CWUNIX.
       COPY CWSEND.
       COPY CWCONF.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWLOCKNEW' UPON ENVIRONMENT-NAME
           ACCEPT     LOCKNEW  FROM ENVIRONMENT-VALUE
           INSPECT LOCKNEW
                   CONVERTING MINUSCULAS
                           TO MAIUSCULAS
           MOVE 110 TO CWPATH-COLOR-FRAME
                       CWPATH-COLOR-BORDER
           MOVE 112 TO CWPATH-COLOR-BARR-MENU
           SET  CWPATH-WITH-DIR        TO TRUE
           SET  CWPATH-WITH-DRIVES     TO TRUE
           SET  CWPATH-WITHOUT-NEWDIR  TO TRUE
           SET  CWPATH-WITHOUT-NEWFILE TO TRUE
           MOVE "_Importar_de:"        TO CWPATH-TITLE
           MOVE 'cwmenu.mnu' TO CWPATH-DEFAULT CWPATH-FILE
           MOVE SPACES               TO CWPATH-PATH
           CALL "CWPATH" USING PARAMETROS-CWPATH
           IF  CWPATH-FILE = SPACES
               GOBACK
           ELSE
               MOVE CWPATH-FILE TO LB-MENUS
           END-IF
           OPEN INPUT BIN
           IF   FS-MENUS NOT = "00"
                CALL "CWISAM" USING ER-MENUS
                GOBACK
           END-IF
           READ BIN
           IF  BIN-REG (8: 1) = SPACE
           AND (BIN-REG (17: 1) = SPACE OR '+')
              MOVE 1 TO OP
              CLOSE BIN
              OPEN INPUT MENUS
           ELSE
              MOVE 2 TO OP
              CLOSE BIN
              OPEN INPUT BIN
           END-IF
           EXEC COBOLware Send
              Message "Este procedimento ir  sobrescrever o menu atual"
              Caption(1) "   ~OK"
              Caption(2) "~Cancelar"
              Option OPTION;OPTION
           END-EXEC

           IF  OPTION = 1
               IF LOCKNEW = 'ON'
                  OPEN I-O VELHOS NOVOS GRUPOS
               END-IF
               OPEN I-O SEGURANCA
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                           FS-CWCONF KCO PCO
                       IF FS-CWCONF < '10'
                       AND (CWCONF-TIPO = "99" OR "SM")
                            SET CWSQLC-DELETE TO TRUE
                            CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                                FS-CWCONF KCO PCO
                            PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > 26
                            IF  CWCONF-PROG (I) NOT = SPACES
                               CALL "CWCODE" USING "D"
                                 CWCONF-SIZE-P-99  (I)
                                 CWCONF-FATOR-P-99 (I)
                                 CWCONF-PROG       (I)
                               INSPECT CWCONF-PROG (I)
                                       CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
                                IF LOCKNEW = 'ON'
                                   MOVE CWCONF-PROG (I) TO VELHOS-CHAVE
                                   READ VELHOS
                                   IF FS-VELHOS = '23'
                                      WRITE VELHOS-REG
                                   END-IF
                                END-IF
                            END-IF
                            IF  CWCONF-PASS (I) NOT = SPACES
                                CALL "CWCODE" USING "D"
                                      CWCONF-SIZE-S-99  (I)
                                      CWCONF-FATOR-S-99 (I)
                                      CWCONF-PASS       (I)
                            END-IF
                            IF((CWCONF-PASS (I) NOT = SPACES)
                            OR (CWCONF-NIVEL(I) NOT = 0)
                            OR (CWCONF-CHECK(I) NOT = SPACES))
                            OR((CWCONF-PROG(I) NOT = 'CWMENU')
                            AND(CWCONF-PROG(I) NOT = 'CWBOXS')
                            AND(CWCONF-HELP(I) NOT = SPACES))
                                MOVE CWCONF-PROG (I) TO SEGURANCA-PROG
                                MOVE CWCONF-NIVEL(I) TO SEGURANCA-NIVEL
                                MOVE CWCONF-CHECK(I) TO SEGURANCA-CHECK
                                MOVE CWCONF-PASS (I) TO SEGURANCA-PASS
                                MOVE CWCONF-HELP (I) TO SEGURANCA-HELP
                                PERFORM 117-LIMPA-NM THRU 117-99-FIM
                                WRITE SEGURANCA-REG
                            END-IF
                       END-IF
               END-PERFORM
               PERFORM UNTIL FS-MENUS > '09'
                   MOVE 0 TO R-99 LIDOS NIVEL
                   OPEN I-O MENU99
                   PERFORM LER-MENUS UNTIL FS-MENUS > "09"
                                        OR CANCELAR = "Y"
                   MOVE 0 TO MENU99-CHAVE
                   MOVE "99" TO CWCONF-REG99
                   ADD  1    TO PGW
                   MOVE PGW  TO CWCONF-PAGINA
                   INITIALIZE CWCONF-OPCOES
                   START MENU99 KEY NOT LESS MENU99-CHAVE
                   PERFORM VARYING I FROM 1 BY 1 UNTIL FS-MENU99 > '09'
                          READ MENU99 NEXT RECORD
                          IF FS-MENU99 > '09'
                             EXIT PERFORM
                          END-IF
                          IF I > 26
                             SET CWSQLC-WRITE TO TRUE
                             CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                                 FS-CWCONF KCO PCO
                             MOVE "99" TO CWCONF-REG99
                             ADD  1    TO PGW
                             MOVE PGW  TO CWCONF-PAGINA
                             MOVE 1    TO I
                             INITIALIZE CWCONF-OPCOES
                          END-IF
                          MOVE TEXTO-99 TO CWCONF-NM-OPCAO (I)
                          MOVE I        TO CWCONF-NO-OPCAO (I)
                          IF PROGRAMA-99 = 'CWBOXS'
                             MOVE "CWBOXS"   TO CWCONF-PROG (I)
                             MOVE PAGINA-99  TO CWCONF-HELP (I)
                             IF   POSIT-99  NOT = SPACES
                                  MOVE ":" TO CWCONF-HELP (I) (5:1)
                                  MOVE POSIT-99 TO CWCONF-HELP (I) (6:)
                                  INSPECT CWCONF-HELP (I) (6: 11)
                                          CONVERTING SPACE TO ","
                             END-IF
                          ELSE
                             MOVE PROGRAMA-99 TO CWCONF-PROG (I)
                             MOVE HELP-99     TO CWCONF-HELP (I)
                          END-IF
                          MOVE NIVEL-99       TO CWCONF-NIVEL(I)
                          MOVE SENHA-99       TO CWCONF-PASS (I)
                          MOVE CHECK-99       TO CWCONF-CHECK(I)
                          PERFORM KRIPTO
                   END-PERFORM
                   SET CWSQLC-WRITE TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                       FS-CWCONF KCO PCO
                   IF   FS-MENUS = '11'
                        MOVE '01' TO FS-MENUS
                   END-IF
                   CLOSE MENU99
               END-PERFORM
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                   FS-CWCONF KCO PCO
           END-IF

           IF OP = 1
              CLOSE MENUS
           ELSE
              CLOSE BIN
           END-IF

           IF  LOCKNEW = 'ON'
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               MOVE 'GU' TO CWCONF-CHAVE
               SET CWSQLC-START TO TRUE
               SET CWSQLC-NOT-LESS TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
               PERFORM UNTIL FS-CWCONF > '09'
                       SET CWSQLC-IGNORE-LOCK TO TRUE
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF CWCONF-TIPO NOT = 'GU'
                          EXIT PERFORM
                       ELSE
                          IF CWCONF-NOME-GRUPO NOT = GRUPOS-CHAVE
                             MOVE CWCONF-NOME-GRUPO TO GRUPOS-CHAVE
                             WRITE GRUPOS-REG
                          END-IF
                       END-IF
               END-PERFORM
               MOVE LOW-VALUES TO GRUPOS-CHAVE
               START GRUPOS KEY NOT LESS GRUPOS-CHAVE
               PERFORM UNTIL FS-GRUPOS > '09'
                       READ GRUPOS NEXT RECORD
                       IF FS-GRUPOS = '00'
                          MOVE LOW-VALUES TO NOVOS-CHAVE
                          START NOVOS KEY NOT LESS NOVOS-CHAVE
                          PERFORM UNTIL FS-NOVOS > '09'
                             READ NOVOS NEXT RECORD
                             IF FS-NOVOS < '10'
                                MOVE 'GU' TO CWCONF-REG
                                MOVE GRUPOS-CHAVE TO CWCONF-NOME-GRUPO
                                MOVE NOVOS-CHAVE  TO CWCONF-PROG-GRUPO
                                MOVE '1'          TO CWCONF-ACESSO-GRUPO
                                SET  CWSQLC-WRITE TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                             END-IF
                          END-PERFORM
                       END-IF
               END-PERFORM
               CLOSE VELHOS NOVOS GRUPOS
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG99 FS-CWCONF KCO PCO
           END-IF
           CLOSE SEGURANCA
           EXEC COBOLware Process Close END-EXEC.

       000-99-FIM. GOBACK.

       117-LIMPA-NM.

           MOVE SPACES TO SEGURANCA-NM-OPCAO
           MOVE 1 TO I2
           IF  CWCONF-NM-OPCAO (I) (I2: 1) = "/"
               ADD 1 TO I2
           END-IF
           MOVE 0 TO I3
           PERFORM VARYING I2 FROM I2 BY 1 UNTIL I2 > 34
                   IF  (CWCONF-NM-OPCAO (I) (I2: 1) NOT = "~")
                   AND (CWCONF-NM-OPCAO (I) (I2: 1) NOT = SPACE)
                        ADD 1 TO I3
                        MOVE CWCONF-NM-OPCAO (I) (I2: 1)
                          TO SEGURANCA-NM-OPCAO (I3: 1)
                   END-IF
           END-PERFORM
           INSPECT SEGURANCA-NM-OPCAO
                   CONVERTING MINUSCULAS TO MAIUSCULAS.

       117-99-FIM. EXIT.
       KRIPTO.

           PERFORM 117-LIMPA-NM THRU 117-99-FIM
           INSPECT CWCONF-PROG (I) CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE CWCONF-PROG (I) TO SEGURANCA-PROG
           READ SEGURANCA
           IF   FS-SEGURANCA < "10"
                IF CWCONF-CHECK (I) = SPACES
                   MOVE SEGURANCA-CHECK TO CWCONF-CHECK (I)
                END-IF
                IF CWCONF-NIVEL (I) = 0
                   MOVE SEGURANCA-NIVEL TO CWCONF-NIVEL (I)
                END-IF
                IF CWCONF-HELP  (I) = SPACES
                   MOVE SEGURANCA-HELP  TO CWCONF-HELP  (I)
                END-IF
                IF CWCONF-PASS (I) = SPACES
                AND (SEGURANCA-PASS NOT = SPACES)
                   MOVE SEGURANCA-PASS TO CWCONF-PASS (I)
                END-IF
           END-IF
           MOVE 8 TO CWCONF-SIZE-P-99 (I)
           CALL "CWCODE" USING "C" CWCONF-SIZE-P-99  (I)
                                   CWCONF-FATOR-P-99 (I)
                                   CWCONF-PROG       (I)
           MOVE 6 TO CWCONF-SIZE-S-99 (I)
           CALL "CWCODE" USING "C" CWCONF-SIZE-S-99  (I)
                                   CWCONF-FATOR-S-99 (I)
                                   CWCONF-PASS       (I)
           IF LOCKNEW = 'ON'
              MOVE SEGURANCA-PROG TO VELHOS-CHAVE
              IF  (VELHOS-CHAVE NOT = SPACES)
              AND (VELHOS-CHAVE NOT = 'CWBOXS')
              AND (VELHOS-CHAVE NOT = 'CWMENU')
              AND (VELHOS-CHAVE NOT = 'GRBOXS')
              AND (VELHOS-CHAVE NOT = 'GRMENU')
                  READ VELHOS
                  IF FS-VELHOS = '23'
                     MOVE SEGURANCA-PROG TO NOVOS-CHAVE
                     READ NOVOS
                     IF FS-NOVOS = '23'
                        WRITE NOVOS-REG
                     END-IF
                  END-IF
              END-IF
           END-IF.

       FIM-KRIPTO. EXIT.

       LER-MENUS.

           IF   FS-MENUS = '01'
                MOVE '00'       TO FS-MENUS
                MOVE MENUS-menu TO menu-quebra
           ELSE
                IF OP = 1
                   READ MENUS
                ELSE
                   READ BIN
                   NOT AT END
                       MOVE LENGTH BIN-REGISTRO TO BIN-LENGTH
                       CALL "CWCODE" USING "D" BIN-LENGTH
                                               BIN-FATOR
                                               BIN-REGISTRO
                       MOVE BIN-REGISTRO TO MENUS-REG
                   END-READ
                END-IF
           END-IF
           IF   FS-MENUS < '10'
           AND (MENUS-menu NOT = SPACES)
           AND  menu-quebra > MENUS-menu
                MOVE '11' TO FS-MENUS
           END-IF
           IF   FS-MENUS < '10'
                EXEC COBOLware Process
                     MSG LB-MENUS
                     CANCEL;CANCELAR
                END-EXEC
                ADD 1 TO LIDOS
                INSPECT MENUS-REG CONVERTING X"10" TO SPACE
                IF  menus-menu not = space
                    MOVE MENUS-menu TO menu-quebra
                END-IF
                IF  MENUS-menu = SPACE
                OR (MENUS-PROGRAMA = SPACES
                AND MENUS-PLUS = '+'
                AND(MENUS-menu not = menu-noportal)
                AND (PORTAL = 0 OR LIDOS = 1))
                    IF MENUS-PROGRAMA = SPACES AND LIDOS = 1
                       MOVE 0 TO PORTAL
                    END-IF
                    PERFORM TOR99
                    MOVE 1              TO NIVEL
                    MOVE ZEROS          TO PAGINAS
                    IF   MENUS-PROGRAMA NUMERIC
                         MOVE MENUS-PROGRAMA TO POSIT-99
                         MOVE SPACE          TO menu-quebra
                    END-IF
                    MOVE 'CWBOXS'       TO PROGRAMA-99
                    WRITE MENU99-REG
                ELSE
                    IF  MENUS-menu NOT = WK-MENU
                        IF   NIVEL > 1
                             MOVE 1     TO NIVEL
                             MOVE ZEROS TO PAGINAS(5: )
                        END-IF
                        MOVE MENUS-menu TO WK-MENU
                    END-IF
                    IF   NIVEL = 0
                         PERFORM TOR99
                         MOVE MENUS-PROGRAMA TO PROGRAMA-99
                         WRITE MENU99-REG
                    ELSE
                         IF   PG (NIVEL) = 0
                              ADD  1 TO PAGINA
                              MOVE PAGINA TO PG(NIVEL)
                         END-IF
                         MOVE "SM"         TO CWCONF-REG99
                         MOVE PG (NIVEL)   TO CWCONF-PAGINA
                         SET CWSQLC-READ  TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
                         CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                             FS-CWCONF KCO PCO
                         IF  FS-CWCONF = "23"
                             IF  PAGINA-99 = 0
                                 MOVE PG (NIVEL)  TO PAGINA-99
                                 REWRITE MENU99-REG
                             END-IF
                             INITIALIZE CWCONF-OPCOES
                             SET CWSQLC-WRITE TO TRUE
                             CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                                 FS-CWCONF KCO PCO
                         END-IF
                         PERFORM VARYING I FROM 1 BY 1
                                           UNTIL I = 26
                                       OR CWCONF-NM-OPCAO (I) = SPACES
                                 CONTINUE
                         END-PERFORM
                         MOVE MENUS-DESCRICAO TO CWCONF-NM-OPCAO (I)
                         MOVE I               TO CWCONF-NO-OPCAO (I)
                         MOVE MENUS-NIVEL     TO CWCONF-NIVEL(I)
                         MOVE MENUS-CHECK     TO CWCONF-CHECK(I)
                         MOVE MENUS-PASS      TO CWCONF-PASS (I)
                         IF   MENUS-PLUS      = "+"
                              MOVE "CWBOXS"   TO CWCONF-PROG     (I)
                              ADD 1 TO NIVEL
                                       PAGINA
                              MOVE PAGINA TO PG(NIVEL)
                                             CWCONF-HELP(I)
                              MOVE MENUS-MODULO TO MODULO
                              CALL "CWMODE" USING "S" PAGINA MODULO
                                                      CWCONF-TIPO
                         ELSE
                              MOVE MENUS-PROGRAMA  TO CWCONF-PROG (I)
                              MOVE MENUS-HELP      TO CWCONF-HELP (I)
                              IF   MENUS-PLUS      = "-"
                                   MOVE 0 TO PG(NIVEL)
                                   SUBTRACT 1 FROM NIVEL
                              END-IF
                         END-IF
                         IF   MENUS-PLUS = "9"
                              MOVE "F9"     TO CWCONF-HELP (I)
                              MOVE "RTCWEM" TO CWCONF-PROG (I)
                         END-IF
                         PERFORM KRIPTO
                         SET CWSQLC-REWRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                             FS-CWCONF KCO PCO
                    END-IF
                END-IF
           END-IF.

       FIM-LER-MENU. EXIT.

       TOR99.

           ADD  1               TO R-99
           INITIALIZE MENU99-REG
           MOVE R-99            TO MENU99-CHAVE
           MOVE MENUS-NIVEL     TO NIVEL-99
           MOVE MENUS-CHECK     TO CHECK-99
           MOVE MENUS-PASS      TO SENHA-99
           MOVE MENUS-HELP      TO HELP-99
           move menus-menu      to menu-noportal
           MOVE MENUS-DESCRICAO TO TEXTO-99
           MOVE SPACES TO TEXTO
           MOVE 0      TO T
           MOVE 1      TO D
           IF MENUS-DESCRICAO = '/'
              ADD 1 TO D
           END-IF
           PERFORM VARYING D FROM D BY 1 UNTIL
                   D > LENGTH MENUS-DESCRICAO
               IF MENUS-DESCRICAO(D:1) <> '~'
                  ADD 1 TO T
                  MOVE MENUS-DESCRICAO(D:1)
                   TO TEXTO(T:1)
               END-IF
           END-PERFORM
           MOVE MENUS-SISTEMA   TO SISTEMA-99.

       FIM-TOR99. EXIT.
       END PROGRAM CWIMMN.

