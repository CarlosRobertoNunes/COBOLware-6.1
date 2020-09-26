       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEXIM INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/02/2001.
       SECURITY.      *************************************************
                      *                                               *
                      * Sequencia/Indexa CWCONF/CWCONF.TXT            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TEXTO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  RESERVE       NO ALTERNATE AREA
                  FILE STATUS   IS FS-TEXTO.

           SELECT CWNEWS ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CWNEWS.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT SEGURANCA ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SEGURANCA-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-SEGURANCA.

       DATA DIVISION.
       FILE SECTION.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG                  PIC  X(2008).

       FD  CWNEWS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWNEWS.

       01  CWNEWS-REG                 PIC  X(080).

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

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 ER-TEXTO.
              10 FS-TEXTO              PIC  X(002) VALUE "00".
              10 LB-TEXTO              PIC  X(255) VALUE "CWCONF.TXT".
           05 ER-CWNEWS.
              10 FS-CWNEWS             PIC  X(002) VALUE "00".
              10 LB-CWNEWS             PIC  X(255) VALUE "CWNEWS".
           05 ER-SEGURANCA.
              10 FS-SEGURANCA          PIC  X(002) VALUE "00".
              10 LB-SEGURANCA          PIC  X(255) VALUE "exim####".
           05 I                        PIC  9(002) VALUE 0.
           05 I2                       PIC  9(002) VALUE 0.
           05 I3                       PIC  9(002) VALUE 0.
           05 VOLTOU                   PIC  9(001) VALUE 0.
           05 COM-99                   PIC  9(001) VALUE 0.
           05 COM-SM                   PIC  9(001) VALUE 0.
           05 COM-RT                   PIC  9(001) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 0.
           05 RETURN-STATUS            PIC  9(002) COMP-5 VALUE 0.
           05 SIZE-OLD-DIR             PIC  9(002) COMP-5 VALUE 50.
           05 OLD-DRIVE                PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY            PIC  X(050) VALUE SPACES.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       COPY CWPATH.
       COPY CWUNIX.
       COPY CWSEND.
       COPY CWHELP.
       COPY CWCONF.

       LINKAGE SECTION.

       01  FUNCAO   PIC X(001).
       01  TIPO     PIC X(002).
       01  ELEMENTO PIC X(030).
       01  ARQUIVO  PIC X(050).

       PROCEDURE DIVISION USING FUNCAO TIPO ELEMENTO ARQUIVO.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 0
                GOBACK
           END-IF

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           EVALUATE FUNCAO
               WHEN "I" OPEN I-O SEGURANCA
                        PERFORM 010-IMPORTAR THRU 010-99-FIM
                        CLOSE SEGURANCA
               WHEN "E" PERFORM 020-EXPORTAR THRU 020-99-FIM
           END-EVALUATE.

       000-99-FIM. GOBACK.

       010-IMPORTAR.

           CALL "CWFILE" USING LB-SEGURANCA
           IF   X91-PARAMETER > 3
                IF   ARQUIVO NOT = SPACES
                     MOVE ARQUIVO TO CWPATH-FILE
                     MOVE SPACES  TO ARQUIVO
                     GO TO 010-IMPORTAR-FILE
                END-IF
           END-IF
           MOVE 110 TO CWPATH-COLOR-FRAME
                       CWPATH-COLOR-BORDER
           MOVE 112 TO CWPATH-COLOR-BARR-MENU
           SET  CWPATH-WITH-DIR        TO TRUE
           SET  CWPATH-WITH-DRIVES     TO TRUE
           SET  CWPATH-WITHOUT-NEWDIR  TO TRUE
           SET  CWPATH-WITHOUT-NEWFILE TO TRUE
           MOVE "_Importar_de:"        TO CWPATH-TITLE
           MOVE SPACES                 TO CWPATH-DEFAULT
                                          CWPATH-PATH
           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                  SIZE-OLD-DIR
           IF   CWUNIX-OFF
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            RETURN-STATUS
                STRING OLD-DRIVE DELIMITED BY SPACE
                       ":\"           DELIMITED BY SPACE
                       OLD-DIRECTORY  DELIMITED BY SPACE
                       "\*.CW"        DELIMITED BY SIZE
                  INTO CWPATH-PATH
           ELSE
                STRING OLD-DIRECTORY  DELIMITED BY SPACE
                       "/*.CW"        DELIMITED BY SIZE
                  INTO CWPATH-PATH
           END-IF
           CALL "CWPATH" USING PARAMETROS-CWPATH.

       010-IMPORTAR-FILE.

           IF   CWPATH-FILE NOT = SPACES
                MOVE CWPATH-FILE TO LB-TEXTO
                OPEN INPUT TEXTO
                IF   FS-TEXTO = "30" OR "35"
                     MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                     STRING "Arquivo: " DELIMITED BY SIZE
                              LB-TEXTO  DELIMITED BY SPACE
                        " nÆo existe !" DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                     GO TO 010-99-FIM
                END-IF
           ELSE
                 GO TO 010-99-FIM
           END-IF

           MOVE 0 TO COM-99
           MOVE 0 TO COM-SM
           MOVE 0 TO COM-RT

           PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                                 OR ELEMENTO = SPACES
                   READ TEXTO IGNORE LOCK
                   NOT AT END
                       EVALUATE TEXTO-REG (1: 2)
                         WHEN "99" MOVE 1 TO COM-99
                         WHEN "SM" MOVE 1 TO COM-SM
                         WHEN "RT" MOVE 1 TO COM-RT
                       END-EVALUATE
           END-PERFORM
           CLOSE TEXTO
           OPEN INPUT TEXTO
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   TIPO = SPACES
           OR   COM-99 = 1
           OR   COM-SM = 1
           OR   COM-RT = 1
                PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                    AND (FS-CWCONF NOT = "9D")
                        SET CWSQLC-READ TO TRUE
                        SET CWSQLC-NEXT TO TRUE
                        SET CWSQLC-LOCK TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG
                                            FS-CWCONF KCO PCO
                        IF   FS-CWCONF < "10"
                        AND  (CWCONF-TIPO = "99" OR "SM" OR "RT")
                             IF  TIPO = SPACES
                             OR  (COM-99 = 1 AND CWCONF-TIPO = "99")
                             OR  (COM-SM = 1 AND CWCONF-TIPO = "SM")
                             OR  (COM-RT = 1 AND CWCONF-TIPO = "RT")
                                PERFORM VARYING I FROM 1 BY 1
                                        UNTIL I > 26
                                  IF((CWCONF-PASS (I) NOT = SPACES)
                                  OR (CWCONF-NIVEL(I) NOT = 0)
                                  OR (CWCONF-CHECK(I) NOT = SPACES))
                                  AND(CWCONF-TIPO = "99" OR "SM")
                                    CALL "CWCODE" USING "D"
                                          CWCONF-SIZE-P-99  (I)
                                          CWCONF-FATOR-P-99 (I)
                                          CWCONF-PROG       (I)
                                     IF  CWCONF-PASS (I) NOT = SPACES
                                         CALL "CWCODE" USING "D"
                                               CWCONF-SIZE-S-99  (I)
                                               CWCONF-FATOR-S-99 (I)
                                               CWCONF-PASS       (I)
                                      END-IF
                                      MOVE CWCONF-PROG (I)
                                        TO SEGURANCA-PROG
                                      MOVE CWCONF-NIVEL(I)
                                        TO SEGURANCA-NIVEL
                                      MOVE CWCONF-CHECK(I)
                                        TO SEGURANCA-CHECK
                                      MOVE CWCONF-PASS (I)
                                        TO SEGURANCA-PASS
                                      PERFORM 117-LIMPA-NM
                                         THRU 117-99-FIM
                                      WRITE SEGURANCA-REG
                                  END-IF
                                END-PERFORM
                                SET CWSQLC-DELETE TO TRUE
                                CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                    FS-CWCONF KCO PCO
                             END-IF
                        END-IF
                END-PERFORM
           END-IF

           PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                   READ TEXTO IGNORE LOCK
                   NOT AT END
                       EVALUATE TEXTO-REG (1: 2)
                         WHEN TIPO PERFORM 115-IMPORTA-GENERICO
                                      THRU 115-99-FIM
                         WHEN "__" MOVE "00" TO CWCONF-REG00
                                   SET CWSQLC-READ  TO TRUE
                                   SET CWSQLC-LOCK  TO TRUE
                                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                      FS-CWCONF KCO PCO
                                   IF   FS-CWCONF < "10"
                                        MOVE TEXTO-REG (3: )
                                          TO CWCONF-APLICATIVO
                                        SET CWSQLC-WRITE  TO TRUE
                                        CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG FS-CWCONF KCO PCO
                                   END-IF
                         WHEN "99"  SET CWSQLC-WRITE  TO TRUE
                                    MOVE TEXTO-REG TO CWCONF-REG
                                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG FS-CWCONF KCO PCO
                                   PERFORM 116-CHECK-SEGURANCA
                                      THRU 116-99-FIM
                         WHEN "SM"  SET CWSQLC-WRITE  TO TRUE
                                    MOVE TEXTO-REG TO CWCONF-REG
                                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG FS-CWCONF KCO PCO
                                   PERFORM 116-CHECK-SEGURANCA
                                      THRU 116-99-FIM
                         WHEN "RT"  SET CWSQLC-WRITE  TO TRUE
                                    MOVE TEXTO-REG TO CWCONF-REG
                                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG FS-CWCONF KCO PCO
                         WHEN "JB"
                                   SET CWSQLC-READ  TO TRUE
                                   SET CWSQLC-LOCK  TO TRUE
                                   MOVE TEXTO-REG TO CWCONF-REGJB
                                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                      FS-CWCONF KCO PCO
                                   IF   FS-CWCONF = "23"
                                        SET CWSQLC-WRITE TO TRUE
                                   ELSE
                                        SET CWSQLC-REWRITE TO TRUE
                                   END-IF
                                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                      FS-CWCONF KCO PCO
                       END-EVALUATE
           END-PERFORM
           CLOSE TEXTO
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           CALL "CWFILE" USING LB-CWNEWS
           OPEN INPUT CWNEWS
           CLOSE CWNEWS
           IF   FS-CWNEWS < "09"
                MOVE LB-CWNEWS TO CWHELP-FILE
                CALL "CWHELP" USING PARAMETROS-CWHELP
           END-IF.

       010-99-FIM. EXIT.

       115-IMPORTA-GENERICO.

           MOVE TEXTO-REG TO CWCONF-REG
           SET CWSQLC-WRITE  TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF = "22"
                SET CWSQLC-REWRITE  TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   TEXTO-REG (1: 2) = '99' OR 'SM'
                PERFORM 116-CHECK-SEGURANCA THRU 116-99-FIM
           END-IF.

       115-99-FIM. EXIT.

       116-CHECK-SEGURANCA.

           IF   FS-CWCONF > "10"
                GO TO 116-99-FIM
           END-IF

           MOVE 0 TO VOLTOU

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 26
             IF  CWCONF-PROG (I) NOT = SPACES
                 MOVE CWCONF-PROG (I) TO SEGURANCA-PROG
                 CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                         CWCONF-FATOR-P-99 (I)
                                         SEGURANCA-PROG
                 PERFORM 117-LIMPA-NM THRU 117-99-FIM
                 READ SEGURANCA
                    IF   FS-SEGURANCA < "10"
                         MOVE 1 TO VOLTOU
                         IF CWCONF-CHECK (I) = SPACES
                            MOVE SEGURANCA-CHECK TO CWCONF-CHECK (I)
                         END-IF
                         IF CWCONF-NIVEL (I) = 0
                            MOVE SEGURANCA-NIVEL TO CWCONF-NIVEL (I)
                         END-IF
                         IF CWCONF-PASS (I) = SPACES
                         AND (SEGURANCA-PASS NOT = SPACES)
                            MOVE SEGURANCA-PASS TO CWCONF-PASS (I)
                            CALL "CWCODE" USING "C"
                                               CWCONF-SIZE-S-99  (I)
                                               CWCONF-FATOR-S-99 (I)
                                               CWCONF-PASS       (I)
                         END-IF
                    END-IF
             END-IF
           END-PERFORM

           IF  VOLTOU = 1
               SET CWSQLC-REWRITE  TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF.

       116-99-FIM. EXIT.

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

       020-EXPORTAR.

           IF   X91-PARAMETER > 3
                IF   ARQUIVO NOT = SPACES
                     MOVE ARQUIVO TO CWPATH-FILE
                     MOVE SPACES  TO ARQUIVO
                     GO TO 020-EXPORTAR-FILE
                END-IF
           END-IF
           MOVE 046 TO CWPATH-COLOR-FRAME
                       CWPATH-COLOR-BORDER
           MOVE 126 TO CWPATH-COLOR-BARR-MENU
           SET  CWPATH-WITH-DIR      TO TRUE
           SET  CWPATH-WITH-DRIVES   TO TRUE
           SET  CWPATH-WITH-NEWDIR   TO TRUE
           SET  CWPATH-WITH-NEWFILE  TO TRUE
           MOVE "_Exportar_para:"    TO CWPATH-TITLE
           MOVE "CWCONF.CW"          TO CWPATH-DEFAULT
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

       020-EXPORTAR-FILE.

           IF   CWPATH-FILE = SPACES
                GO TO 020-99-FIM
           ELSE
                MOVE CWPATH-FILE TO LB-TEXTO
                OPEN INPUT TEXTO
                IF   FS-TEXTO = "30" OR "35" OR X"3909"
                     OPEN OUTPUT TEXTO
                     IF   FS-TEXTO > "09"
                          CALL "CWISAM" USING ER-TEXTO
                          GO TO 020-99-FIM
                     END-IF
                ELSE
                     IF   FS-TEXTO < "10"
                          MOVE SPACES TO CWSEND-MSG
                          STRING "Arquivo: " DELIMITED BY SIZE
                             LB-TEXTO        DELIMITED BY SPACE
                             " j  existe !"  DELIMITED BY SIZE
                                INTO CWSEND-MSG
                          MOVE 0 TO I
                          IF   X91-PARAMETER < 3
                               ADD 1 TO I
                               MOVE "~Extender" TO CWSEND-SCREEN (I)
                          END-IF
                          ADD 1 TO I
                          MOVE "~Sobrepor "   TO CWSEND-SCREEN (I)
                          ADD 1 TO I
                          MOVE "~Outro"       TO CWSEND-SCREEN (I)
                          ADD 1 TO I
                          MOVE "~Cancelar"    TO CWSEND-SCREEN (I)
                          CLOSE TEXTO
                          IF   TIPO NOT = SPACES
                          AND  X91-PARAMETER > 3
                               OPEN EXTEND TEXTO
                          ELSE
                               CALL "CWSEND" USING PARAMETROS-CWSEND
                               EVALUATE CWSEND-OPTION-CHAR
                                        WHEN "S" OPEN OUTPUT TEXTO
                                        WHEN "O" GO TO 020-EXPORTAR
                                        WHEN "E" OPEN EXTEND TEXTO
                                        WHEN OTHER
                                             GO TO 020-99-FIM
                               END-EVALUATE
                          END-IF
                     END-IF
                END-IF
           END-IF
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  TIPO NOT = SPACES
               MOVE 0 TO VEZ
               PERFORM 030-EXPORTA-GENERICO THRU 030-99-FIM
           ELSE
                IF   TIPO = SPACES
                     MOVE "00"              TO CWCONF-REG00
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                                 KCO PCO
                     MOVE "__"              TO TEXTO-REG
                     MOVE CWCONF-APLICATIVO TO TEXTO-REG (3: )
                     WRITE TEXTO-REG
                END-IF
                PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                   SET CWSQLC-READ        TO TRUE
                   SET CWSQLC-NEXT        TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                       FS-CWCONF KCO PCO
                   IF   FS-CWCONF < "10"
                        EVALUATE CWCONF-TIPO
                             WHEN "99" WRITE TEXTO-REG FROM CWCONF-REG99
                             WHEN "SM" WRITE TEXTO-REG FROM CWCONF-REG99
                             WHEN "JB" WRITE TEXTO-REG FROM CWCONF-REGJB
                             WHEN "RT" WRITE TEXTO-REG FROM CWCONF-REGRT
                        END-EVALUATE
                   END-IF
                END-PERFORM
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       020-99-FIM. EXIT.

       030-EXPORTA-GENERICO.

           IF   ELEMENTO = SPACES
                IF  VEZ = 0
                AND (TIPO NOT = SPACES)
                    IF  TIPO (1: 1) = "G"
                        MOVE "G" TO CWCONF-REG00
                    ELSE
                        MOVE TIPO TO CWCONF-REG00
                    END-IF
                    MOVE 1    TO VEZ
                    SET CWSQLC-START    TO TRUE
                    SET CWSQLC-NOT-LESS TO TRUE
                    CALL "CWCONF" USING CWSQLC CWCONF-REG
                          FS-CWCONF KCO PCO
                END-IF
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-NEXT        TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF  TIPO (1: 1) = "G"
                    IF   CWCONF-TIPO (1: 1) NOT = "G"
                         MOVE "10" TO FS-CWCONF
                    END-IF
                ELSE
                    IF  (TIPO NOT = CWCONF-TIPO)
                    AND (TIPO NOT = SPACES)
                         MOVE "10" TO FS-CWCONF
                    END-IF
                END-IF
           ELSE
                MOVE TIPO     TO CWCONF-TIPO
                MOVE ELEMENTO TO CWCONF-ELEMENTO
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-EQUAL       TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   FS-CWCONF < "10"
                EVALUATE CWCONF-TIPO
                    WHEN "00" WRITE TEXTO-REG FROM CWCONF-REG00
                    WHEN "02" WRITE TEXTO-REG FROM CWCONF-REG02
                    WHEN "03" WRITE TEXTO-REG FROM CWCONF-REG03
                    WHEN "92" WRITE TEXTO-REG FROM CWCONF-REG92
                    WHEN "94" WRITE TEXTO-REG FROM CWCONF-REG94
                    WHEN "99" WRITE TEXTO-REG FROM CWCONF-REG99
                    WHEN "AT" WRITE TEXTO-REG FROM CWCONF-REGAT
                    WHEN "ES" WRITE TEXTO-REG FROM CWCONF-REGES
                    WHEN "GU" WRITE TEXTO-REG FROM CWCONF-REGGU
                    WHEN "GX" WRITE TEXTO-REG FROM CWCONF-REGGX
                    WHEN "GI" WRITE TEXTO-REG FROM CWCONF-REGGI
                    WHEN "GS" WRITE TEXTO-REG FROM CWCONF-REGGI
                    WHEN "JB" WRITE TEXTO-REG FROM CWCONF-REGJB
                    WHEN "LC" WRITE TEXTO-REG FROM CWCONF-REGLC
                    WHEN "L5" WRITE TEXTO-REG FROM CWCONF-REGLC
                    WHEN "LG" WRITE TEXTO-REG FROM CWCONF-REGLG
                    WHEN "PS" WRITE TEXTO-REG FROM CWCONF-REGPS
                    WHEN "RL" WRITE TEXTO-REG FROM CWCONF-REGRL
                    WHEN "RT" WRITE TEXTO-REG FROM CWCONF-REGRT
                    WHEN "SM" WRITE TEXTO-REG FROM CWCONF-REG99
                    WHEN "TK" WRITE TEXTO-REG FROM CWCONF-REGTK
                END-EVALUATE
                IF   ELEMENTO = SPACES
                     GO TO 030-EXPORTA-GENERICO
                END-IF
           END-IF.

       030-99-FIM. EXIT.

       END PROGRAM CWEXIM.
