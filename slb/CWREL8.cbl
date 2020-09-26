       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL8.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/07/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 8 - Rolamento de eveito visual        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 OPT                     PIC X(004) VALUE SPACES.
           05 CORES                   PIC X(256) VALUE SPACES.
           05 MOLDURAS                PIC X(072) VALUE SPACES.
           05 MSG                     PIC X(030) VALUE SPACES.
           05 LM                      PIC 9(002) VALUE 22.
           05 L                       PIC 9(002) VALUE 0.
           05 Y                       PIC 9(002) VALUE 0.
           05 I                       PIC 9(002) VALUE 0.
           05 K                       PIC 9(002) VALUE 0.
           05 RELROW                  PIC X(001) VALUE "1".
           05 RELCONT                 PIC X(001) VALUE "1".
           05 LINHA                              VALUE SPACES.
              15 LINHA-1              PIC X(020).
              15 LINHA-2              PIC X(038).
              15 LINHA-3              PIC X(020).
           05 TELA                          VALUE SPACES.
              10 LINHA-TAB OCCURS 16  PIC X(999).
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.

       COPY CWUNIX.
       COPY CWCONF.

       LINKAGE SECTION.

       01  TEXTO                      PIC X(999).
       01  C                          PIC 9(003).
       01  OPCAO                      PIC X(001).

       PROCEDURE DIVISION USING TEXTO C OPCAO.

       000-INICIO.

           ON   1
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "RL" TO CWCONF-CHAVE
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "10"
                     MOVE CWCONF-RELATOR-COUNT TO RELCONT
                     MOVE CWCONF-RELATOR-ROLL  TO RELROW
                END-IF
                DISPLAY "CWRELDISPLAY" UPON ENVIRONMENT-NAME
                MOVE SPACES TO OPT
                ACCEPT  OPT         FROM ENVIRONMENT-VALUE
                IF  OPT = SPACES
                    DISPLAY "cwreldisplay" UPON ENVIRONMENT-NAME
                    ACCEPT  OPT         FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT OPT CONVERTING MINUSCULAS TO MAIUSCULAS
                IF  OPT = "ON" OR "1"
                    MOVE "1" TO RELROW
                ELSE
                    IF  OPT = "OFF" OR "0"
                        MOVE "0" TO RELROW
                    END-IF
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC
                                    CWCONF-REG
                                    FS-CWCONF
                                    KCO PCO.

           IF   RELROW NOT = "1"
                GOBACK
           END-IF

           IF   K < 15
                ADD  1     TO K
                MOVE TEXTO TO LINHA-TAB (K)
                GOBACK
           END-IF

           ON   1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE "[<]-Esquerda [>]-Direita"
                       TO MSG
                ELSE
                     STRING "[" X"1B" "]-Esquerda ["
                             X"1A" "]-Direita" DELIMITED BY SIZE
                          INTO MSG
                END-IF
                CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                           MSG
                                           X"001E".

           IF   OPCAO = "R"
                MOVE    1                 TO I
                PERFORM 010-EXIBE-LINHA THRU 010-99-FIM
                        VARYING L FROM 07 BY 1 UNTIL L > 21
                GOBACK
           END-IF

           MOVE    1                 TO I
           PERFORM 010-EXIBE-LINHA THRU 010-99-FIM
                   VARYING L FROM 07 BY 1 UNTIL L > 20
           MOVE    TEXTO             TO LINHA-TAB (I)
           PERFORM 010-EXIBE-LINHA THRU 010-99-FIM.

       000-99-FIM. GOBACK.

       010-EXIBE-LINHA.

           IF   OPCAO = "R"
                MOVE  LINHA-TAB (I) (C: 78) TO  LINHA
                ADD 1 TO I
           ELSE
                ADD 1 TO I
                COMPUTE Y = I - 1
                IF  I < 16
                    MOVE LINHA-TAB (I) TO LINHA-TAB (Y)
                END-IF
                MOVE  LINHA-TAB (Y) (C: 78)   TO  LINHA
           END-IF
           COMPUTE ROW-NUMBER = L - 1
           MOVE 1                        TO COLUMN-NUMBER

           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                            LINHA-1
                                            X"0014"

           IF   L < 10
           OR   L > 18
           OR  (RELCONT NOT = "1")
                MOVE 21                       TO COLUMN-NUMBER
                CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                 LINHA-2
                                                 X"0026"
           END-IF

           MOVE 59                       TO COLUMN-NUMBER
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                            LINHA-3
                                            X"0014".

       010-99-FIM. EXIT.

       END PROGRAM CWREL8.
