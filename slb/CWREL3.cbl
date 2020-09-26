      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL3.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/06/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 3 - Cataloga dicionario               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAMPOS.SEL.

       DATA DIVISION.
       FILE SECTION.

       COPY CAMPOS.FD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 DIC-BYTE             PIC  X(001) VALUE SPACE.
           05 A                    PIC  X(050) VALUE SPACES.
           05 C                    PIC  X(050) VALUE SPACES.
           05 RELOGO               PIC  X(003) VALUE SPACES.
           05 DESCRICAO            PIC  X(030) VALUE SPACES.
           05 COL-W                PIC S9(003) VALUE 0.
           05 CORES                PIC  X(256)        VALUE SPACES.
           05 MOLDURAS             PIC  X(072)        VALUE SPACES.
           05 X91-RESULT           PIC  9(002) COMP-X VALUE 0.
           05 X91-F16              PIC  9(002) COMP-X VALUE 16.
           05 X91-F15              PIC  9(002) COMP-X VALUE 15.
           05 X91-PARAMETER        PIC  9(002) COMP-X VALUE 0.
           05 RELDIR               PIC  X(050) VALUE SPACES.
           05 Y                    PIC  9(003) VALUE 0.
           05 D                    PIC  9(003) VALUE 0.
           05 I                    PIC  9(003) VALUE 0.
           05 X1                   PIC  9(003) VALUE 0.
           05 X2                   PIC  9(003) VALUE 0.
           05 ERRO                 PIC  9(001) VALUE 0.
           05 MINUSCULAS  PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS  PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ACENTOS     PIC X(36) VALUE
              "†Ç°¢£ÖäçïóÑîÅÉàìáÜêãüñëíò©ùéôöèâåÄ‰∆".
           05 SEM-ACENTOS PIC X(36) VALUE
              "AEIOUAEIOUAOUAEOCAEIOUAEIOUAOUAEOCOA".
           05 PROVEDOR              PIC  X(008) VALUE SPACES.
           05 X91-PROVEDOR.
              10 LEN-PROVEDOR      PIC  9(002) VALUE 0 COMP-X.
              10 PROG              PIC  X(065) VALUE SPACES.
           05 ER-CAMPOS.
              10 FS-CAMPOS         PIC  X(002) VALUE "00".
              10 LB-CAMPOS         PIC  X(255) VALUE SPACES.
           05 ER-DIC.
              10 FS-DIC            PIC  X(002) VALUE "00".
              10 LB-DIC            PIC  X(100) VALUE SPACES.
           05 K                    PIC  9(004) VALUE 0.
           05 K-ED                 PIC  Z(004) VALUE 0.
           05 OBS                  PIC  X(053) VALUE SPACES.
           05 COLUNAS              PIC  9(004) VALUE 0.
           05 MAX-NAME             PIC  9(002) VALUE 30.
           05 LENAME               PIC  9(002) VALUE 30.
           05 MAIOR-NOME           PIC  9(002) VALUE 8.
           05 VARIAVEIS-RELATOR.
              10 PIC X(30) VALUE "SELECIONADOS  09 V".
              10 PIC X(30) VALUE "PAGINA        06 V".
              10 PIC X(30) VALUE "FOLHA         06 V".
              10 PIC X(30) VALUE "DATA          08 D DD/MM/AAAA".
              10 PIC X(30) VALUE "HORA          06 H HH:MM:SS".
              10 PIC X(30) VALUE "LINHA         09 V".
              10 PIC X(30) VALUE "EMPRESA       30 X".
              10 PIC X(30) VALUE "SISTEMA       30 X".
              10 PIC X(30) VALUE "USUARIO       30 X".
              10 PIC X(30) VALUE "DIA-SEMANA    07 X".
              10 PIC X(30) VALUE "CODIGO        07 X".
              10 PIC X(30) VALUE "DESCRICAO     30 X".
              10 PIC X(30) VALUE "QUEBRA(1)     06 V".
              10 PIC X(30) VALUE "QUEBRA(2)     06 V".
              10 PIC X(30) VALUE "QUEBRA(3)     06 V".
              10 PIC X(30) VALUE "QUEBRA(4)     06 V".
              10 PIC X(30) VALUE "QUEBRA(5)     06 V".
              10 PIC X(30) VALUE "QUEBRA(6)     06 V".
              10 PIC X(30) VALUE "QUEBRA(7)     06 V".
           05 REDEFINES VARIAVEIS-RELATOR.
              10 OCCURS 19.
                 15 RELATOR-DATANAME PIC  X(014).
                 15 RELATOR-TAMANHO  PIC  9(002).
                 15                  PIC  X(001).
                 15 RELATOR-TIPO     PIC  X(001).
                 15                  PIC  X(001).
                 15 RELATOR-MASCARA  PIC  X(011).
           05 TECLA                PIC  9(002) VALUE 0. COPY CWKEYS.
           05 VARS.
              10                   PIC X(255) VALUE "RELATOR".
              10 COBDIR            PIC X(255) VALUE "COBDIR".
              10                   PIC X(255) VALUE "SLB".
              10                   PIC X(255) VALUE "TMP".
              10                   PIC X(255) VALUE "TEMP".
           05 REDEFINES VARS.
              10 VAR OCCURS 5      PIC X(255).
           05 VAR-X                PIC X(255) VALUE SPACES.
           05 ER-DIC-X.
              15 FS-DIC-X          PIC X(002) VALUE SPACES.
              15 LB-DIC-X          PIC X(100) VALUE SPACES.

      *01  CONTROL-COLORS            PIC X(38) VALUE
      *    "BACKGROUND-COLOR X,FOREGROUND-COLOR X".
      *01  REDEFINES CONTROL-COLORS.
      *    10                        PIC X(017).
      *    10 B                      PIC 9(001).
      *    10                        PIC X(018).
      *    10 F                      PIC 9(001).
      *
       COPY CWSEND.
       COPY CWBOXS.
       COPY CWUNIX.
       COPY CWCONF.
       COPY CWACOR.

       01  DIC-REG.
           05 DIC-POSIT              PIC  9(005).
           05 FILLER                 PIC  X(001).
           05 DIC-TAMANHO            PIC  9(003).
           05 FILLER                 PIC  X(001).
           05 DIC-DEC                PIC  9(002).
           05 FILLER                 PIC  X(001).
           05 DIC-TIPO               PIC  X(001).
           05 FILLER                 PIC  X(001).
           05 DIC-DATANAME           PIC  X(030).
           05 FILLER                 PIC  X(001).
           05 DIC-MASCARA            PIC  X(030).

       LINKAGE SECTION.

       01  COMMAREA PIC X(2000).

       PROCEDURE DIVISION USING COMMAREA.

       000-INICIO.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
      *    IF   CWUNIX-ON
      *         MOVE "COBPATH" TO COBDIR
      *    END-IF
           CALL X"91" USING X91-RESULT X91-F16 X91-PARAMETER
           IF   X91-PARAMETER < 1
                MOVE "Rodar sob o CWMENU(COBOLware 6.1)" TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                GOBACK
           END-IF

      *    CALL "CWACOR"         USING PARAMETROS-CWACOR
      *    MOVE CWACOR-F (9)        TO F
      *    MOVE CWACOR-B (9)        TO B
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "RL" TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-RELATOR TO RELDIR
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   RELDIR = SPACES
                MOVE "relator" TO RELDIR
           END-IF
           CALL "CBL_CREATE_DIR" USING RELDIR
           STRING RELDIR     DELIMITED BY SPACE
                  "\COLUNAS" DELIMITED BY SIZE
                    INTO LB-CAMPOS
           INSPECT LB-CAMPOS CONVERTING "\" TO "/".

       RETRY.

           DISPLAY "Provedor: " LINE 08 COLUMN 03
      *                         WITH CONTROL CONTROL-COLORS
           PERFORM TEST AFTER UNTIL ERRO = 0
              MOVE 0 TO ERRO
              ACCEPT  PROVEDOR LINE 08 COLUMN 13 WITH UPDATE PROMPT
      *                        CONTROL CONTROL-COLORS
              ACCEPT  TECLA FROM ESCAPE KEY
              INSPECT PROVEDOR CONVERTING MINUSCULAS
                                       TO MAIUSCULAS
              DISPLAY PROVEDOR LINE 08 COLUMN 13
      *                        WITH CONTROL CONTROL-COLORS
              MOVE PROVEDOR TO PROG
              IF  NOT ESC
                  IF   PROVEDOR (1: 5) = "CWMEN"
                  AND  PROVEDOR (7: 2) = SPACES
                  AND (PROVEDOR (3: 4) NOT = SPACES)
                       MOVE "Nome de provedor inadequado"
                         TO CWSEND-MSG
                       CALL "CWSEND" USING PARAMETROS-CWSEND
                       MOVE 1 TO ERRO
                  ELSE
                       PERFORM VARYING LEN-PROVEDOR FROM 8 BY -1
                                      UNTIL LEN-PROVEDOR = 0
                                         OR (PROG (LEN-PROVEDOR: 1)
                                            NOT = SPACE)
                              CONTINUE
                       END-PERFORM
                       CALL X"91" USING X91-RESULT X91-F15 X91-PROVEDOR
                       IF   X91-RESULT NOT = 0
                            MOVE "Programa provedor n∆o existe"
                              TO CWSEND-MSG
                            CALL "CWSEND" USING PARAMETROS-CWSEND
                            MOVE 1 TO ERRO
                       END-IF
              END-IF
           END-PERFORM
           IF  ESC
               GOBACK
           END-IF

           MOVE LENGTH OF CAMPOS-DATANAME TO LENAME
           MOVE 0 TO Y
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 5
                   IF   I = 0
                        MOVE SPACES TO LB-DIC-X
                        STRING PROVEDOR         DELIMITED BY SPACE
                               ".DIC"           DELIMITED BY SIZE
                               INTO LB-DIC-X
                        PERFORM 002-ISOLA-DIRETORIO THRU 002-99-FIM
                   ELSE
                        IF   I = 1
                             MOVE RELDIR TO VAR (1)
                        ELSE
                             DISPLAY VAR (I) UPON ENVIRONMENT-NAME
                             ACCEPT  VAR (I) FROM ENVIRONMENT-VALUE
                        END-IF
                        IF   VAR (I) = SPACES
                        AND  I = 1
                             MOVE SPACES TO LB-DIC-X
                             STRING "RELATOR\"       DELIMITED BY SIZE
                               PROVEDOR         DELIMITED BY SPACE
                               ".DIC"           DELIMITED BY SIZE
                               INTO LB-DIC-X
                              PERFORM 002-ISOLA-DIRETORIO
                                 THRU 002-99-FIM
                        ELSE
                             MOVE VAR (I) TO VAR-X
                             MOVE SPACES  TO VAR (I)
                             MOVE 0       TO X2
                             PERFORM VARYING X1 FROM 1 BY 1
                                     UNTIL X1 > 255
                                        OR VAR-X (X1: 1) = SPACE
                                     IF VAR-X (X1: 1) = ";"
                                     OR (CWUNIX-ON
                                      AND VAR-X (X1: 1) = ":")
                                         PERFORM 001-ADD-X
                                            THRU 001-99-FIM
                                     ELSE
                                          ADD 1 TO X2
                                          MOVE VAR-X (X1: 1)
                                            TO VAR (I) (X2: 1)
                                     END-IF
                             END-PERFORM
                             IF   VAR (I) NOT = SPACES
                                  PERFORM 001-ADD-X THRU 001-99-FIM
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           IF   Y = 0
                MOVE SPACES TO LB-DIC
                STRING PROVEDOR         DELIMITED BY SPACE
                       ".DIC"           DELIMITED BY SIZE
                       INTO LB-DIC
                DISPLAY "Entrada : " LINE 10 COLUMN 03
      *                              WITH CONTROL CONTROL-COLORS
                ACCEPT  LB-DIC       LINE 10 COLUMN 13
                                          WITH UPDATE PROMPT SIZE 50
      *                              CONTROL CONTROL-COLORS
                ACCEPT  TECLA FROM ESCAPE KEY
                IF  ESC
                    GOBACK
                END-IF
           ELSE
                IF   Y = 1
                     MOVE CWBOXS-TEXT   (Y) TO LB-DIC
                     DISPLAY "Entrada : " LINE 10 COLUMN 03 LB-DIC
      *                                   WITH CONTROL CONTROL-COLORS
                ELSE
                     MOVE "_Selecione_entrada:_" TO CWBOXS-TITLE
                     MOVE 10                     TO CWBOXS-LINE
                     MOVE 13                     TO CWBOXS-COLUMN
                     MOVE "N"                    TO CWBOXS-ERASE
                     CALL "CWBOXS"            USING PARAMETROS-CWBOXS
                     IF   CWBOXS-OPTION NOT = 0
                          MOVE CWBOXS-TEXT   (CWBOXS-OPTION) TO LB-DIC
                     ELSE
                          GOBACK
                     END-IF
                END-IF
           END-IF

           CALL "CWBINF" USING 'I' FS-DIC LB-DIC
           IF   FS-DIC NOT = "00"
                CALL "CWISAM" USING ER-DIC
                GO TO RETRY
           END-IF
           MOVE SPACES TO OBS
           STRING "0000 Removendo dicion†rio " DELIMITED BY SIZE
                              PROVEDOR         DELIMITED BY SPACE
                              "..."            DELIMITED BY SIZE
                       INTO OBS
           OPEN I-O CAMPOS
           CALL "CWMSGW" USING "230353" OBS
           MOVE PROVEDOR TO CAMPOS-PROVEDOR
           MOVE 0 TO K      CAMPOS-COLUNA
           START CAMPOS KEY NOT LESS CAMPOS-CHAVE
           PERFORM UNTIL (CAMPOS-PROVEDOR NOT = PROVEDOR)
                      OR FS-CAMPOS > "09"
                   CALL "CWATCH"
                   READ CAMPOS NEXT RECORD
                   IF   FS-CAMPOS < "10"
                   AND  CAMPOS-PROVEDOR = PROVEDOR
                        ADD  1 TO K
                        MOVE K TO K-ED
                        CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                         K-ED
                                                         X"0004"
                        DELETE CAMPOS RECORD
                   END-IF
           END-PERFORM
           MOVE 0 TO K
           MOVE SPACES TO OBS
           STRING "0000 Carregando dicion†rio " DELIMITED BY SIZE
                  PROVEDOR                      DELIMITED BY SPACE
                  ".DIC..."                DELIMITED BY SIZE
             INTO OBS
           CALL "CWMSGW" USING "230353" OBS
           PERFORM UNTIL FS-DIC NOT = "00"
              CALL "CWATCH"
              MOVE SPACES TO DIC-REG
              MOVE 0      TO D
              PERFORM UNTIL FS-DIC > '09'
                      CALL "CWBINF" USING 'R' FS-DIC DIC-BYTE
                      IF  FS-DIC < '10'
                      AND(DIC-BYTE NOT = X'0D')
                          IF DIC-BYTE = X'0A'
                             EXIT PERFORM
                          END-IF
                          IF  D < LENGTH OF DIC-REG
                              ADD 1 TO D
                              MOVE DIC-BYTE TO DIC-REG (D: 1)
                          END-IF
                      END-IF
              END-PERFORM
              IF   FS-DIC < "10"
                   ADD  1 TO K
                   MOVE K TO K-ED
                   CALL "CBL_WRITE_SCR_CHARS" USING X"1602"
                                                    K-ED
                                                    X"0004"
                   IF   DIC-POSIT NUMERIC
                        INSPECT DIC-DATANAME CONVERTING MINUSCULAS
                                                     TO MAIUSCULAS
                        INSPECT DIC-DATANAME CONVERTING ACENTOS
                                                     TO SEM-ACENTOS
                        IF  FS-DIC = "00"
                        AND DIC-TAMANHO NUMERIC
                        AND DIC-TAMANHO > 0
                            MOVE SPACES        TO CAMPOS-ACUMULADOR
                                                  CAMPOS-RELATORIO
                            MOVE DIC-DATANAME  TO CAMPOS-DATANAME
                            MOVE DIC-POSIT     TO CAMPOS-POSIT
                            MOVE DIC-TAMANHO   TO CAMPOS-TAMANHO
                            MOVE DIC-DEC       TO CAMPOS-DEC
                            MOVE DIC-TIPO      TO CAMPOS-TIPO
                            MOVE DIC-MASCARA   TO CAMPOS-MASCARA
                            PERFORM 010-GRAVA-COLUNA THRU 010-99-FIM
                            IF   CAMPOS-VALOR
                                 PERFORM VARYING I
                                            FROM LENAME
                                              BY -1
                                           UNTIL CAMPOS-DATANAME (I: 1)
                                                 NOT = SPACE
                                         CONTINUE
                                 END-PERFORM
                                 ADD  1 TO I
                                 IF   I > LENAME
                                      SUBTRACT 3 FROM I
                                 END-IF
                                 ADD    4   TO CAMPOS-TAMANHO
                                 MOVE "[+]" TO CAMPOS-DATANAME (I: )
                                 MOVE "+"   TO CAMPOS-ACUMULADOR
                                 PERFORM 010-GRAVA-COLUNA
                                   THRU 010-99-FIM
                            END-IF
                        END-IF
                   ELSE
                        MOVE DIC-REG TO DESCRICAO
                   END-IF
              END-IF
           END-PERFORM

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 18
                   INITIALIZE CAMPOS-REG
                   MOVE "Relator-"           TO CAMPOS-DATANAME
                   MOVE RELATOR-DATANAME (Y) TO CAMPOS-DATANAME (9: )
                   MOVE RELATOR-TAMANHO  (Y) TO CAMPOS-TAMANHO
                   MOVE RELATOR-TIPO     (Y) TO CAMPOS-TIPO
                   MOVE RELATOR-MASCARA  (Y) TO CAMPOS-MASCARA
                   IF   Y < 13
                        PERFORM 010-GRAVA-COLUNA THRU 010-99-FIM
                   END-IF
                   IF   CAMPOS-VALOR
                   AND (Y = 1 OR Y > 12)
                        PERFORM VARYING I
                                   FROM LENAME
                                     BY -1
                                  UNTIL CAMPOS-DATANAME (I: 1)
                                        NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        ADD  1 TO I
                        IF   I > LENAME
                             SUBTRACT 3 FROM I
                        END-IF
                        ADD    4   TO CAMPOS-TAMANHO
                        MOVE "[+]" TO CAMPOS-DATANAME (I: )
                        MOVE "+"   TO CAMPOS-ACUMULADOR
                        PERFORM 010-GRAVA-COLUNA THRU 010-99-FIM
                   END-IF
           END-PERFORM
           INITIALIZE CAMPOS-REG
           MOVE  PROVEDOR   TO CAMPOS-PROVEDOR
           MOVE  0          TO CAMPOS-COLUNA
           MOVE  MAIOR-NOME TO CAMPOS-TAMANHO
           MOVE  DESCRICAO  TO CAMPOS-MASCARA
           WRITE CAMPOS-REG
           CALL "CWBINF" USING 'C' FS-DIC
           CLOSE CAMPOS.

       000-99-FIM. GOBACK.

       001-ADD-X.

           IF   VAR (I) (X2: 1) = "\" OR "/"
                MOVE SPACE TO VAR (I) (X2: 1)
                SUBTRACT 1 FROM X2
           END-IF

            MOVE SPACES TO LB-DIC-X
            STRING VAR (I) DELIMITED BY SPACE
                   "\"        DELIMITED BY SIZE
                   PROVEDOR   DELIMITED BY SPACE
                   ".DIC"     DELIMITED BY SIZE
                   INTO LB-DIC-X

             PERFORM 002-ISOLA-DIRETORIO THRU 002-99-FIM
             MOVE SPACES TO VAR (I)
             MOVE 0      TO X2.

       001-99-FIM. EXIT.

       002-ISOLA-DIRETORIO.

           IF   CWUNIX-ON
                INSPECT LB-DIC-X CONVERTING "\" TO "/"
           END-IF
           MOVE ER-DIC-X TO ER-DIC
           CALL "CWBINF" USING 'I' FS-DIC LB-DIC
           MOVE ER-DIC   TO ER-DIC-X
           MOVE ALL "X"  TO A
           IF   Y NOT = 0
                MOVE LB-DIC-X          TO A
                INSPECT A CONVERTING MINUSCULAS TO MAIUSCULAS
                PERFORM VARYING K FROM 1 BY 1 UNTIL K > Y
                        MOVE CWBOXS-TEXT   (K) TO C
                        INSPECT C CONVERTING MINUSCULAS TO MAIUSCULAS
                        IF   A = C
                             MOVE SPACES TO A
                        END-IF
                END-PERFORM
          END-IF
          IF  (A NOT = SPACES)
          AND  FS-DIC = "00"
          AND  Y < 15
               ADD  1        TO Y
               MOVE LB-DIC-X TO CWBOXS-TEXT   (Y)
          END-IF
          CALL "CWBINF" USING 'C' FS-DIC.

       002-99-FIM. EXIT.

       010-GRAVA-COLUNA.

           ADD   1        TO COLUNAS
           MOVE  PROVEDOR TO CAMPOS-PROVEDOR
           MOVE  COLUNAS  TO CAMPOS-COLUNA
           MOVE  SPACES   TO CAMPOS-RELATORIO
           WRITE CAMPOS-REG
           PERFORM VARYING MAX-NAME
                      FROM LENAME BY -1
                     UNTIL MAX-NAME = MAIOR-NOME
                        OR MAX-NAME = 1
                        OR CAMPOS-DATANAME(MAX-NAME: 1)
                           NOT = SPACE
                   CONTINUE
           END-PERFORM
           IF  MAX-NAME > MAIOR-NOME
               MOVE MAX-NAME TO MAIOR-NOME
           END-IF.

       010-99-FIM. EXIT.

       END PROGRAM CWREL3.
