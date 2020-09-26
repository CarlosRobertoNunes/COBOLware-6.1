       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Valida nome de senhas para CWMEN8            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HELP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-HELP.

       DATA DIVISION.
       FILE SECTION.

       FD  HELP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HELP.

       01  HELP-REG.
           05 HELP-TEXTO             PIC  X(047).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 REUSE                    PIC  9(003) VALUE 0.
           05 SARBANE                              VALUE SPACES.
              10 FLAG-LETRAS           PIC  X(001).
              10 FLAG-NUMEROS          PIC  X(001).
              10 FLAG-ESPECIAIS        PIC  X(001).
           05 FS-PASSWD                PIC  X(001) VALUE SPACE.
           05 TESTE                    PIC  X(001) VALUE SPACE.
              88 TESTE-LETRAS VALUE "A" THRU "Z".
           05 MINIMO                   PIC  9(002) VALUE 0.
           05 LINHAS                   PIC  9(002) VALUE 0.
           05 TESTE-1                  PIC  X(002) VALUE SPACES.
           05 TESTE-2                  PIC  X(001) VALUE SPACE.
           05 MSG-MINIMO               PIC  X(050) VALUE SPACES.
           05 NOME              PIC  X(030) VALUE SPACES.
           05 OK                       PIC  X(001) VALUE SPACE.
           05 MAX                      PIC  9(003) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 SEQ                      PIC  9(003) VALUE 0.
           05 LETRAS                   PIC  9(002) VALUE 0.
           05 NUMEROS                  PIC  9(002) VALUE 0.
           05 ESPECIAIS                PIC  9(002) VALUE 0.
           05 IGUAIS                   PIC  9(002) VALUE 0.
           05 REPETE                   PIC  9(002) VALUE 0.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ER-HELP.
              10 FS-HELP      PIC  X(002) VALUE "00".
              10 LB-HELP      PIC  X(255) VALUE "$TEMP/cwmenk.tmp".
      *                          1 proibir 2 exigir
       01  TEXTOS.
          5 PIC X(41) VALUE "   ".
          5 PIC X(41) VALUE "   Visando maior seguran‡a de acesso,".
          5 PIC X(41) VALUE "   a senha pessoal nÆo pode:".
          5 PIC X(41) VALUE "    ".
          5 PIC X(41) VALUE "1..- Conter letras;".
          5 PIC X(41) VALUE ".1.- Conter n£meros;".
          5 PIC X(41) VALUE "..1- Conter caracteres especiais;".
          5 PIC X(41) VALUE "2..- Deixar de ter letras;".
          5 PIC X(41) VALUE ".2.- Deixar de ter n£meros;".
          5 PIC X(41) VALUE "..2- Deixar de ter caracteres especiais;".
          5 PIC X(41) VALUE "   - Ser igual ao nome do usu rio;".
          5 PIC X(41) VALUE "   - Ser repetitiva como A1A1A1 e AAAAAA;".
          5 PIC X(41) VALUE "   ".
        01 REDEFINES TEXTOS.
           05 OCCURS 13.
              10 FLAGS.
                 15 FLAG-1 PIC X.
                 15 FLAG-2 PIC X.
                 15 FLAG-3 PIC X.
              10 LINHA PIC X(38).

       COPY CWCONF.

       LINKAGE SECTION.

       01   ERRO           PIC 9 VALUE 0.
       01   SENHAS.
            05 SENHA1      PIC X(030).
            05 SENHA2      PIC X(030).

       PROCEDURE DIVISION USING ERRO SENHAS.

       000-INICIO.

           DISPLAY "CWSARBANE" UPON ENVIRONMENT-NAME
           ACCEPT     SARBANE  FROM ENVIRONMENT-VALUE
           INSPECT SENHAS CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   SENHA1 = SPACES
           AND  SENHA2 = SPACES
                EXEC COBOLware SEND
                     MSG "Falta a senha "
                END-EXEC
                MOVE 1 TO ERRO
           ELSE
                IF   SENHA1 NOT = SENHA2
                     MOVE 1   TO ERRO
                     EXEC COBOLware SEND
                               MSG "Senha nÆo confere"
                     END-EXEC
                ELSE
                     MOVE 0   TO ERRO
                     SET CWSQLC-OPEN TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     MOVE "LG"        TO CWCONF-REG
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   FS-CWCONF = "00"
                          MOVE CWCONF-MIN-SENHA TO MINIMO
                          IF   CWCONF-REUSE NOT NUMERIC
                               MOVE 0 TO CWCONF-REUSE
                          END-IF
                          MOVE CWCONF-REUSE TO REUSE
                     END-IF
                     DISPLAY "CWMENK" UPON ENVIRONMENT-NAME
                     ACCEPT NOME FROM ENVIRONMENT-VALUE
                     PERFORM VARYING SEQ
                                FROM 1
                                  BY 1
                                  UNTIL FS-PASSWD > "0"
                                     OR REUSE = 0
                             MOVE "S"         TO CWCONF-S
                             MOVE SEQ         TO CWCONF-SEQ
                             MOVE NOME        TO CWCONF-NOME
                             CALL "CWFSPW" USING CWCONF-CHAVE
                                                 CWCONF-PASSWORD
                                                 CWCONF-SIZE
                                                 CWCONF-FATOR
                                                 FS-PASSWD
                             IF   FS-PASSWD = "0"
                                  CALL "CWCODE" USING "D" CWCONF-SIZE
                                                          CWCONF-FATOR
                                                       CWCONF-PASSWORD
                                  IF   CWCONF-PASSWORD = SENHA1
                                       MOVE 1 TO ERRO
                                       MOVE "99" TO FS-CWCONF
                                  END-IF
                             END-IF
                     END-PERFORM
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF  ERRO = 1
                         EXEC COBOLware SEND
                            MSG "Senha utilizada anteriormente"
                         END-EXEC
                     ELSE
                         PERFORM 010-MEDE-SENHA THRU 010-99-FIM
                         IF (NUMEROS + LETRAS + ESPECIAIS) < MINIMO
                             MOVE SPACES TO MSG-MINIMO
                             STRING "Senha cont‚m menos de "
                                    MINIMO " caracteres"
                                    DELIMITED BY SIZE
                                      INTO MSG-MINIMO
                             EXEC COBOLware SEND
                                MSG MSG-MINIMO
                             END-EXEC
                             MOVE 1 TO ERRO
                         END-IF
                     END-IF
                END-IF
           END-IF

           IF  ERRO = 0
           AND SARBANE NUMERIC
               IF  SENHA1 = NOME
                   MOVE 1 TO ERRO
               END-IF
               IF (LETRAS = 0    AND FLAG-LETRAS    = "2")
               OR (NUMEROS = 0   AND FLAG-NUMEROS   = "2")
               OR (ESPECIAIS = 0 AND FLAG-ESPECIAIS = "2")
               OR (LETRAS > 0    AND FLAG-LETRAS    = "1")
               OR (NUMEROS > 0   AND FLAG-NUMEROS   = "1")
               OR (ESPECIAIS > 0 AND FLAG-ESPECIAIS = "1")
               OR ((NUMEROS + LETRAS + ESPECIAIS) / 2) = (IGUAIS + 1)
               OR (NUMEROS + LETRAS + ESPECIAIS) < MINIMO
               OR (NUMEROS + LETRAS + ESPECIAIS) = REPETE
                  MOVE 1 TO ERRO
               END-IF
               IF   ERRO = 1
                    PERFORM TEST AFTER UNTIL OK NOT = "D"
                    EXEC COBOLware SEND
                       MSG "Perfil de senha rejeitado"
                       Screen(1) "   ~OK"
                       Screen(2) "~Detalhes"
                       Option 1
                       OPTION-CHAR;OK
                    END-EXEC
                    IF  OK = "D"
                        OPEN OUTPUT HELP
                        MOVE 0 TO LINHAS
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 13
                                MOVE SPACES   TO HELP-REG
                                IF  FLAGS (I) = SPACES
                                OR  FLAG-1 (I) = FLAG-LETRAS
                                OR  FLAG-2 (I) = FLAG-NUMEROS
                                OR  FLAG-3 (I) = FLAG-ESPECIAIS
                                    MOVE LINHA(I) TO HELP-REG(2: )
                                    ADD 1 TO LINHAS
                                    WRITE HELP-REG
                                END-IF
                        END-PERFORM
                        CLOSE HELP
                        EXEC COBOLware Help
                             File LB-HELP
                             LINE 07
                             COLUMN 10
                             WIDTH 47
                             HEIGHT LINHAS
                        END-EXEC
                        DELETE FILE HELP
                    END-IF
                    END-PERFORM
               END-IF
           END-IF.

       000-99-FIM. GOBACK.

       010-MEDE-SENHA.

           MOVE 0 TO LETRAS
           MOVE 0 TO NUMEROS
           MOVE 0 TO ESPECIAIS
           MOVE 0 TO IGUAIS
           MOVE SPACE TO TESTE-1
           PERFORM VARYING MAX FROM LENGTH OF SENHA1
                                 BY -1
                                 UNTIL MAX < 2
                      OR SENHA1(MAX: 1) NOT = SPACE
                    CONTINUE
           END-PERFORM
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > MAX
                           OR ERRO = 1
                   IF  (SENHA1 (I: 1) NOT = SPACE)
                   OR  (TESTE-1 NOT = SPACE)
                       MOVE SENHA1 (I: 1) TO TESTE
                       IF  TESTE-1 = SPACE
                           MOVE SENHA1 (I: 2) TO TESTE-1
                                                 TESTE-2
                           MOVE 1 TO REPETE
                       ELSE
                           IF   TESTE-1 = SENHA1 (I: 2)
                                ADD 1 TO IGUAIS
                           END-IF
                           IF   TESTE-2 = SENHA1 (I: 1)
                                ADD 1 TO REPETE
                           END-IF
                       END-IF
                       EVALUATE TRUE
                           WHEN TESTE NUMERIC
                                ADD 1 TO NUMEROS
                           WHEN TESTE-LETRAS
                                ADD 1 TO LETRAS
                           WHEN OTHER
                                ADD 1 TO ESPECIAIS
                       END-EVALUATE
                   END-IF
           END-PERFORM.

       010-99-FIM. EXIT.

       END PROGRAM CWMENK.
