       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LOADMENU.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/03/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa defini‡ao de menus                   *
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
           05 FILLER            PIC  X(020).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ER-MENUS.
              10 FS-MENUS              PIC  X(002) VALUE "00".
              10 LB-MENUS              PIC  X(255) VALUE "cwmenu.mnu".
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
           05 P                        PIC  9(002) VALUE 0.
           05 R-99                     PIC  9(002) VALUE 0.
           05 TECLA                    PIC  9(003) VALUE 0.
           05 DESCRICOES                          VALUE SPACES.
              10 DESCRICAO                  PIC  X(034) OCCURS 16.
           05 CHECKS.
              10 CWCHECK-01            PIC  X(001) VALUE "1".
              10 CWCHECK-02            PIC  X(001) VALUE "1".
              10 CWCHECK-03            PIC  X(001) VALUE "1".
              10 CWCHECK-04            PIC  X(001) VALUE "1".
              10 CWCHECK-05            PIC  X(001) VALUE "1".
              10 CWCHECK-06            PIC  X(001) VALUE "1".
              10 CWCHECK-07            PIC  X(001) VALUE "1".
              10 CWCHECK-08            PIC  X(001) VALUE "1".
              10 CWCHECK-09            PIC  X(001) VALUE "1".
              10 CWCHECK-10            PIC  X(001) VALUE "1".
              10 CWCHECK-11            PIC  X(001) VALUE "1".
              10 CWCHECK-12            PIC  X(001) VALUE "1".
              10 CWCHECK-13            PIC  X(001) VALUE "1".
              10 CWCHECK-14            PIC  X(001) VALUE "1".
              10 CWCHECK-15            PIC  X(001) VALUE "1".
              10 CWCHECK-16            PIC  X(001) VALUE "1".
           05 REDEFINES CHECKS.
              10 CHECK OCCURS 16       PIC  X(001).
           05 SIGLAS.
              10 SELECAO OCCURS 16.
                 15 SISTEMA            PIC  X(002).
                 15 PRODUTO            PIC  X(001).
           05 TEXTOS.
              10 OCCURS 26.
                 15 TEXTO-99           PIC  X(034).
                 15 PAGINA-99          PIC  9(004).
                 15 SISTEMA-99         PIC  X(002).
                 15 POSIT-99           PIC  XXBXXBXXBXX.
       COPY CWSQLC.

       01  FS-CWCONF PIC  X(002) VALUE "00".
       01  FS-CWGRPS PIC  X(002) VALUE "00".
       01  KCO       PIC  X(030) VALUE "CHAVE".
       01  PCO       PIC  X(002) VALUE SPACE.
       01  KGR       PIC  X(030) VALUE "CHAVE".
       01  PGR       PIC  X(002) VALUE SPACE.

       01  CWCONF-REG.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO                        PIC  X(002).
              10 CWCONF-ELEMENTO                    PIC  X(030).
            05 CWCONF-RESTO                         PIC X(1976).

       01  CWCONF-REG99 redefines CWCONF-REG.
           05 FILLER                                PIC  X(002).
           05 CWCONF-PAGINA                         PIC  9(004).
           05 FILLER                                PIC  X(026).
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

       SCREEN SECTION.

       01  CTAC-LIT-MENU.
           05 LINE 03 COLUMN 15 PIC X(034) FROM DESCRICAO(01).
           05 LINE 04 COLUMN 15 PIC X(034) FROM DESCRICAO(02).
           05 LINE 05 COLUMN 15 PIC X(034) FROM DESCRICAO(03).
           05 LINE 06 COLUMN 15 PIC X(034) FROM DESCRICAO(04).
           05 LINE 07 COLUMN 15 PIC X(034) FROM DESCRICAO(05).
           05 LINE 08 COLUMN 15 PIC X(034) FROM DESCRICAO(06).
           05 LINE 10 COLUMN 15 PIC X(034) FROM DESCRICAO(07).
           05 LINE 12 COLUMN 15 PIC X(034) FROM DESCRICAO(08).
           05 LINE 13 COLUMN 15 PIC X(034) FROM DESCRICAO(09).
           05 LINE 14 COLUMN 15 PIC X(034) FROM DESCRICAO(10).
           05 LINE 15 COLUMN 15 PIC X(034) FROM DESCRICAO(11).
           05 LINE 16 COLUMN 15 PIC X(034) FROM DESCRICAO(12).
           05 LINE 17 COLUMN 15 PIC X(034) FROM DESCRICAO(13).
           05 LINE 18 COLUMN 15 PIC X(034) FROM DESCRICAO(14).
           05 LINE 19 COLUMN 15 PIC X(034) FROM DESCRICAO(15).
           05 LINE 20 COLUMN 15 PIC X(034) FROM DESCRICAO(16).
           05 LINE 21 COLUMN 15 PIC X(034) FROM DESCRICAO(00).

       01  CTAC-VAR-MENU.
           05 LINE 03 COLUMN 12 PIC X(001) using CWCHECK-01.
           05 LINE 04 COLUMN 12 PIC X(001) using CWCHECK-02.
           05 LINE 05 COLUMN 12 PIC X(001) using CWCHECK-03.
           05 LINE 06 COLUMN 12 PIC X(001) using CWCHECK-04.
           05 LINE 07 COLUMN 12 PIC X(001) using CWCHECK-05.
           05 LINE 08 COLUMN 12 PIC X(001) using CWCHECK-06.
           05 LINE 12 COLUMN 12 PIC X(001) using CWCHECK-07.
           05 LINE 13 COLUMN 12 PIC X(001) using CWCHECK-08.
           05 LINE 14 COLUMN 12 PIC X(001) using CWCHECK-09.
           05 LINE 15 COLUMN 12 PIC X(001) using CWCHECK-10.
           05 LINE 16 COLUMN 12 PIC X(001) using CWCHECK-11.
           05 LINE 17 COLUMN 12 PIC X(001) using CWCHECK-12.
           05 LINE 18 COLUMN 12 PIC X(001) using CWCHECK-13.
           05 LINE 19 COLUMN 12 PIC X(001) using CWCHECK-14.
           05 LINE 20 COLUMN 12 PIC X(001) using CWCHECK-15.
           05 LINE 21 COLUMN 12 PIC X(001) using CWCHECK-16.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY SPACES AT 0101 ERASE EOS
           INITIALIZE TEXTOS
           OPEN INPUT MENUS
           IF   FS-MENUS NOT = "00"
                CALL "CWISAM" USING ER-MENUS
                GOBACK
           END-IF
           MOVE 0 TO SISTEMAS
           PERFORM UNTIL FS-MENUS > '10'
                   READ MENUS
                   IF FS-MENUS < '10'
                   AND (MENUS-PONTO NOT = '.')
                      IF MENUS-PONTO = SPACE  OR '*'
                         ADD  1               TO SISTEMAS
                         MOVE MENUS-SISTEMA   TO SISTEMA (SISTEMAS)
                         MOVE MENUS-DESCRICAO TO DESCRICAO(SISTEMAS)
                         MOVE MENUS-PONTO     TO PRODUTO(SISTEMAS)
                      ELSE
                         IF PRODUTO(SISTEMAS) NOT = '*'
                            IF PRODUTO(SISTEMAS) = SPACE
                               MOVE MENUS-menu TO PRODUTO(SISTEMAS)
                            ELSE
                               ADD 1 TO SISTEMAS
                               MOVE MENUS-SISTEMA
                                 TO SISTEMA (SISTEMAS)
                               MOVE MENUS-DESCRICAO
                                 TO DESCRICAO(SISTEMAS)
                            END-IF
                         END-IF
                      END-IF
                   END-IF
           END-PERFORM
           CLOSE MENUS
           OPEN INPUT MENUS
           EXEC COBOLware OBJECT (PUSH-BUTTON)
                     LINE 09 COLUMN 51 WIDTH 10
                      CAPTION "~Aplicar"
                    KEY 100 (TAB-OFF)
           END-EXEC
           EXEC COBOLware OBJECT (PUSH-BUTTON)
                     LINE 15 COLUMN 51 WIDTH 10
                      CAPTION "~Fechar"
                    KEY 101 (TAB-OFF)
           END-EXEC
           DISPLAY CTAC-LIT-MENU
                   CTAC-VAR-MENU
           PERFORM UNTIL TECLA NOT = 0
                   ACCEPT CTAC-VAR-MENU
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           IF  TECLA NOT = 100
               GOBACK
           END-IF
           EXEC COBOLware Send
              Message "Este procedimento ir  sobrescrever o menu atual"
              Caption(1) "~OK" Caption(2) "~Cancelar"
              Option OPTION;OPTION
           END-EXEC

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16
                   IF  CHECK (I) = "0"
                       MOVE LOW-VALUES TO SELECAO(I)
                   END-IF
           END-PERFORM

           IF  OPTION = 1
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               PERFORM TEST AFTER UNTIL FS-CWCONF > "10"
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-NEXT TO TRUE
                       CALL "CWCONF" USING CWSQLC CWCONF-REG
                                           FS-CWCONF KCO PCO
                       IF   FS-CWCONF < '10'
                       AND (CWCONF-TIPO = "99" OR "SM")
                            SET CWSQLC-DELETE TO TRUE
                            CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                FS-CWCONF KCO PCO
                       END-IF
               END-PERFORM
               PERFORM LER-MENUS UNTIL FS-MENUS > "09"
                                    OR CANCELAR = "Y"
               MOVE "99" TO CWCONF-REG99
               MOVE 1    TO CWCONF-PAGINA
               INITIALIZE CWCONF-OPCOES
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > R-99
                      MOVE TEXTO-99 (I)  TO CWCONF-NM-OPCAO (I)
                      MOVE I             TO CWCONF-NO-OPCAO (I)
                      MOVE "CWBOXS"      TO CWCONF-PROG (I)
                      MOVE PAGINA-99 (I) TO CWCONF-HELP (I)
                      IF   POSIT-99 (I) NOT = SPACES
                           MOVE ":" TO CWCONF-HELP (I) (5:1)
                           MOVE POSIT-99 (I) TO CWCONF-HELP (I) (6:)
                           INSPECT CWCONF-HELP (I) (6: 11)
                                   CONVERTING SPACE TO ","
                      END-IF
                      PERFORM KRIPTO
               END-PERFORM
               SET CWSQLC-WRITE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG
                                   FS-CWCONF KCO PCO
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG
                                   FS-CWCONF KCO PCO
           END-IF
           CLOSE MENUS.

       000-99-FIM. GOBACK.

       KRIPTO.

           MOVE 8 TO CWCONF-SIZE-P-99 (I)
           CALL "CWCODE" USING "C" CWCONF-SIZE-P-99  (I)
                                   CWCONF-FATOR-P-99 (I)
                                   CWCONF-PROG       (I)
           MOVE 6 TO CWCONF-SIZE-S-99 (I)
           CALL "CWCODE" USING "C" CWCONF-SIZE-S-99  (I)
                                   CWCONF-FATOR-S-99 (I)
                                   CWCONF-PASS       (I).

       FIM-KRIPTO. EXIT.

       LER-MENUS.

           MOVE "0" TO PROCESSAR
           READ MENUS
                NOT END
                IF   MENUS-SISTEMA =  SISTEMA-A
                AND  MENUS-menu  =  menu-A
                     MOVE PROCESSAR-A TO PROCESSAR
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16
                                                 OR PROCESSAR = "1"
                       IF  MENUS-SISTEMA = SISTEMA (I)
                       AND(MENUS-menu    = PRODUTO (I)
                        OR MENUS-menu    = SPACE
                        OR PRODUTO (I)   = "*")
                           MOVE "1" TO PROCESSAR
                       END-IF
                END-PERFORM
                MOVE PROCESSAR     TO PROCESSAR-A
                MOVE MENUS-SISTEMA TO SISTEMA-A
                MOVE MENUS-menu    TO menu-A
                IF   PROCESSAR = "0"
                AND (MENUS-PROGRAMA NOT NUMERIC)
                     GO TO LER-MENUS
                END-IF
                INSPECT MENUS-REG CONVERTING X"10" TO SPACE
                IF  MENUS-menu = SPACE
                    ADD  1               TO R-99
                    MOVE MENUS-DESCRICAO TO TEXTO-99  (R-99)
                    MOVE MENUS-SISTEMA   TO SISTEMA-99(R-99)
                    MOVE 1               TO NIVEL
                    MOVE ZEROS           TO PAGINAS
                    IF   MENUS-PROGRAMA NUMERIC
                         MOVE MENUS-PROGRAMA TO POSIT-99 (R-99)
                    END-IF
                    EXEC COBOLware Process (Close) END-EXEC
                ELSE
                    EXEC COBOLware Process
                         MSG TEXTO-99(R-99)
                         CANCEL;CANCELAR
                    END-EXEC
                    IF  MENUS-menu NOT = WK-MENU
                        IF   NIVEL > 1
                             MOVE 1     TO NIVEL
                             MOVE ZEROS TO PAGINAS(5: )
                        END-IF
                        MOVE MENUS-menu TO WK-MENU
                    END-IF
                    IF   PG (NIVEL) = 0
                         ADD  1 TO PAGINA
                         MOVE PAGINA TO PG(NIVEL)
                    END-IF
                    MOVE "SM"         TO CWCONF-REG99
                    MOVE PG (NIVEL)   TO CWCONF-PAGINA
                    SET CWSQLC-READ  TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    CALL "CWCONF" USING CWSQLC CWCONF-REG
                                            FS-CWCONF KCO PCO
                    IF  FS-CWCONF = "23"
                        IF  PAGINA-99 (R-99) = 0
                            MOVE PG (NIVEL)  TO PAGINA-99 (R-99)
                        END-IF
                        INITIALIZE CWCONF-OPCOES
                        SET CWSQLC-WRITE TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG
                                            FS-CWCONF KCO PCO
                    END-IF
                    PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I = 26
                                  OR CWCONF-NM-OPCAO (I) = SPACES
                            CONTINUE
                    END-PERFORM
                    MOVE MENUS-DESCRICAO TO CWCONF-NM-OPCAO (I)
                    MOVE I               TO CWCONF-NO-OPCAO (I)
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
                         IF   MENUS-PLUS      = "-"
                              MOVE 0 TO PG(NIVEL)
                              SUBTRACT 1 FROM NIVEL
                         END-IF
                    END-IF
                    IF   MENUS-PLUS      = "9"
                         MOVE "F9" TO CWCONF-HELP (I)
                         MOVE "RTCWEM" TO CWCONF-PROG (I)
                    END-IF
                    PERFORM KRIPTO
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC CWCONF-REG
                END-IF
           END-READ.

       FIM-LER-MENU. EXIT.

       END PROGRAM LOADMENU.

