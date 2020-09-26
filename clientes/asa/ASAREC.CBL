       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ASAREC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/03/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Reconstroi CWDIRS                            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWCONF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWCONF-CHAVE
                  FILE STATUS   IS FS-CWCONF.

           SELECT PASTAS ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-PASTAS.

           SELECT CWDIRS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWDIRS-SPOOL
                  ALTERNATE RECORD KEY IS CWDIRS-FOLHAS  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-NOTA    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-USUARIO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-CODIGO  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CWDIRS-EMISSAO WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CWDIRS.

           SELECT PRNTEX ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-PRINTS.

           SELECT PRNTE1 ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-PRINTS
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-PRINTS.

           SELECT PRNTE2 ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-PRINTS
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-PRINTS.

           SELECT PRNTE3 ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-PRINTS
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-PRINTS.

       DATA DIVISION.
       FILE SECTION.

       COPY PRCONF.

       FD  PASTAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PASTAS.

       01  PASTAS-REG.
           05 PASTAS-FN-16              PIC  X(008).
           05 FILLER                    PIC  X(001).
           05 PASTAS-FT-16              PIC  X(003).
           05 FILLER                    PIC  X(034).
           05 PASTAS-NOME-32            PIC  X(050).
           05 FILLER                    PIC  X(050).

       FD  CWDIRS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWDIRS.

       01  CWDIRS-REG.
           05 CWDIRS-SPOOL.
              10 FILLER                PIC  X(002).
              10 CWDIRS-NUMERO         PIC  9(006).
              10 FILLER                PIC  X(004).
           05 CWDIRS-COMANDO           PIC  X(007).
           05 CWDIRS-IMPRESSO          PIC  X(001).
           05 CWDIRS-CODIGO            PIC  X(007).
           05 CWDIRS-TIPO              PIC  X(001).
           05 CWDIRS-PROGRAMA          PIC  X(002).
           05 CWDIRS-FOLHAS            PIC  9(006).
           05 CWDIRS-LINHAS            PIC  9(010).
           05 CWDIRS-EMISSAO.
              10 CWDIRS-DATA           PIC  9(008).
              10 CWDIRS-HORA           PIC  9(002).
              10 CWDIRS-MINUTO         PIC  9(002).
           05 CWDIRS-USUARIO           PIC  X(010).
           05 CWDIRS-NOTA              PIC  X(020).
           05 CWDIRS-ESTILO            PIC  X(001).
           05 CWDIRS-TITULO            PIC  X(080).

       FD  PRNTEx
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PRINTS.

       01  PRNTEX-REG                  PIC  X(001).

       FD  PRNTE1
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PRINTS.

       01  PRNTE1-REG.
           10 FILLER                   PIC  X(135).

       FD  PRNTE2
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PRINTS.

       01  PRNTE2-REG.
           10 FILLER                   PIC  X(083).

       FD  PRNTE3
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PRINTS.

       01  PRNTE3-REG.
           10 FILLER                   PIC  X(223).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 LINHA               PIC X(500) VALUE SPACES.
           05 PRIMEIRA            PIC X(500) VALUE SPACES.
           05 I                   PIC 9(003) VALUE 0.
           05 Y                   PIC 9(003) VALUE 0.
           05 POSBARRA            PIC 9(003) VALUE 0.
           05 TAMANHO             PIC 9(003) VALUE 0.
           05 PADRAO              PIC 9(001) VALUE 0.
           05 TECLA               PIC 9(002) VALUE 0. COPY CWKEYS.
           05 ER-CWCONF.
              10 FS-CWCONF             PIC  X(002) VALUE "00".
              10 LB-CWCONF             PIC  X(050) VALUE "cwconf".
           05 ER-CWDIRS.
              10 FS-CWDIRS             PIC  X(002) VALUE "00".
              10 LB-CWDIRS             PIC  X(050) VALUE "CWDIRS".
           05 RK-PRINTS           COMP PIC  9(010) VALUE ZERO.
           05 ER-PRINTS.
              10 FS-PRINTS             PIC  X(002) VALUE "00".
              10 LB-PRINTS                         VALUE "PRINTS".
                 15 BYTE-LB-PRINTS OCCURS 50 PIC X.
           05 PATH-SPOOL               PIC  X(030) VALUE SPACES.
           05 OPERADOR                 PIC  X(030).
           05 TASK                     PIC  9(006) VALUE 0.
           05 CWMENU                   PIC  X(001) VALUE SPACE.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 OLD-DRIVE                PIC  X(001) VALUE SPACE.
           05 RETURN-STATUS            PIC  9(002) COMP-5 VALUE 0.
           05 ER-PASTAS.
              10 FS-PASTAS         PIC  X(002) VALUE "00".
              10 LB-PASTAS         PIC  X(050) VALUE SPACES.

       COPY CWEXEC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FS-PASTAS > "09"
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           CALL "CWATCH".
           READ PASTAS
                NOT AT END
                    INSPECT PASTAS-REG CONVERTING
                            MINUSCULAS TO MAIUSCULAS
                    MOVE PASTAS-REG TO CWDIRS-SPOOL
                    READ CWDIRS IGNORE LOCK
                         INVALID KEY
                         PERFORM 110-RECUPERA THRU 110-99-FIM
                    END-READ
           END-READ.

       100-99-FIM. EXIT.

       110-RECUPERA.

           MOVE ALL "0" TO CWDIRS-EMISSAO
           MOVE SPACES  TO CWDIRS-COMANDO
                           CWDIRS-IMPRESSO
                           CWDIRS-PROGRAMA
                           CWDIRS-NOTA
                           CWDIRS-ESTILO
                           CWDIRS-TITULO
           MOVE "?"     TO CWDIRS-USUARIO
                           CWDIRS-CODIGO
           MOVE 1       TO CWDIRS-FOLHAS
           MOVE 0       TO CWDIRS-LINHAS
                           Y
                           TAMANHO PADRAO
           MOVE SPACES TO LB-PRINTS
           STRING PATH-SPOOL   DELIMITED BY SPACE
                  "\"          DELIMITED BY SIZE
                  CWDIRS-SPOOL DELIMITED BY SIZE
             INTO LB-PRINTS

           OPEN INPUT PRNTEX
           MOVE SPACE TO CWDIRS-TIPO
           DISPLAY "Verificando tamanho: " AT 0803
                   TAMANHO AT 0824
                   "            Linhas : " AT 1003
                   "            P ginas: " AT 1203
           PERFORM UNTIL FS-PRINTS > "09"
                      OR CWDIRS-TIPO NOT = SPACE
                   READ PRNTEX
                   ADD 1 TO TAMANHO
                   DISPLAY TAMANHO AT 0824
                   IF  PRNTEX-REG = X"0D"
                       EVALUATE TRUE
                         WHEN TAMANHO > 220
                              MOVE "3" TO CWDIRS-TIPO
                         WHEN TAMANHO > 132
                              MOVE "1" TO CWDIRS-TIPO
                         WHEN TAMANHO > 80
                              MOVE "2" TO CWDIRS-TIPO
                       END-EVALUATE
                   END-IF
           END-PERFORM
           CLOSE PRNTEX
           EVALUATE CWDIRS-TIPO
                  WHEN "1"  OPEN INPUT PRNTE1
                            MOVE 119 TO POSBARRA
                  WHEN "2"  OPEN INPUT PRNTE2
                            MOVE 067 TO POSBARRA
                  WHEN "3"  OPEN INPUT PRNTE3
                            MOVE 207 TO POSBARRA
           END-EVALUATE
           MOVE 0 TO RK-PRINTS
           PERFORM UNTIL FS-PRINTS > "09"
                   ADD 1 TO RK-PRINTS
                   EVALUATE CWDIRS-TIPO
                          WHEN "1" READ PRNTE1 INTO LINHA
                          WHEN "2" READ PRNTE2 INTO LINHA
                          WHEN "3" READ PRNTE3 INTO LINHA
                   END-EVALUATE
                   IF   FS-PRINTS < "10"
                        MOVE RK-PRINTS TO CWDIRS-LINHAS
                        DISPLAY CWDIRS-LINHAS AT 1024
                   END-IF
                   IF  RK-PRINTS = 1
                       MOVE LINHA TO PRIMEIRA
                   END-IF
                   IF  RK-PRINTS = 2
                       PERFORM 120-CHECK-TIME THRU 120-99-FIM
                   END-IF
                   IF  RK-PRINTS = 3
                   AND PADRAO = 1
                       MOVE LINHA (POSBARRA + 6: 7) TO CWDIRS-CODIGO
                   END-IF
                   IF LINHA (1: 1) = X"0C"
                   OR LINHA (2: 1) = X"0C"
                   OR LINHA (3: 1) = X"0C"
                      ADD 1 TO CWDIRS-FOLHAS
                        DISPLAY CWDIRS-FOLHAS AT 1224
                   END-IF
           END-PERFORM
           WRITE CWDIRS-REG
           EVALUATE CWDIRS-TIPO
                  WHEN "1" CLOSE PRNTE1
                  WHEN "2" CLOSE PRNTE2
                  WHEN "3" CLOSE PRNTE3
           END-EVALUATE.

       110-99-FIM. EXIT.

       120-CHECK-TIME.

           IF  LINHA (POSBARRA: 1) = "/"
           AND LINHA (POSBARRA + 3: 1) = "/"
           AND LINHA (POSBARRA + 11: 1) = ":"
               MOVE LINHA (POSBARRA +  4: 4) TO CWDIRS-DATA (1: 4)
               MOVE LINHA (POSBARRA +  1: 2) TO CWDIRS-DATA (5: 2)
               MOVE LINHA (POSBARRA -  2: 2) TO CWDIRS-DATA (7: 2)
               MOVE LINHA (POSBARRA +  9: 2) TO CWDIRS-HORA
               MOVE LINHA (POSBARRA + 12: 2) TO CWDIRS-MINUTO
               MOVE 1 TO PADRAO
               MOVE 0 TO Y
               PERFORM VARYING I FROM 35 BY 1 UNTIL I = POSBARRA
                                                 OR Y = 80
                       IF   Y = 0
                            IF   PRIMEIRA (I: 1) NOT = SPACE
                                 MOVE 1 TO Y
                            END-IF
                       ELSE
                            ADD 1 TO Y
                       END-IF
                       IF   Y NOT = 0
                            MOVE PRIMEIRA (I: 1) TO CWDIRS-TITULO (Y: 1)
                       END-IF
               END-PERFORM
               MOVE CWDIRS-TITULO TO CWDIRS-NOTA
           END-IF.

       120-99-FIM. EXIT.

       800-INICIAIS.

           MOVE  "?"              TO CWMENU
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     CWMENU

           MOVE SPACES TO LB-PASTAS
           STRING "CW"            DELIMITED BY SIZE
                  TASK            DELIMITED BY SIZE
                  ".DIR"  DELIMITED BY SIZE
                 INTO LB-PASTAS.

           OPEN INPUT CWCONF
           MOVE "PS"     TO CWCONF-TIPO
           MOVE OPERADOR TO CWCONF-NOME
           READ CWCONF IGNORE LOCK

           IF   FS-CWCONF = "23"
                MOVE "SPOOL"  TO CWCONF-PATH-SPOOL
           ELSE
               IF   CWCONF-PATH-SPOOL = SPACES
                    MOVE "SPOOL"  TO CWCONF-PATH-SPOOL
               ELSE
                    IF   CWCONF-PATH-SPOOL (1: 1) = "\"
                         CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                     RETURN-STATUS
                         MOVE SPACES TO PATH-SPOOL
                         STRING OLD-DRIVE ":" CWCONF-PATH-SPOOL
                                              DELIMITED BY SPACE
                                INTO PATH-SPOOL
                         MOVE PATH-SPOOL TO CWCONF-PATH-SPOOL
                    END-IF
               END-IF
           END-IF

           MOVE CWCONF-PATH-SPOOL TO PATH-SPOOL

           MOVE SPACES TO CWEXEC-COMANDO
           STRING "DIR "          DELIMITED BY SIZE
                  PATH-SPOOL      DELIMITED BY SPACE
                  "\*.SPL /B"     DELIMITED BY SIZE
                  "> "            DELIMITED BY SIZE
                  LB-PASTAS       DELIMITED BY SPACE
                 INTO CWEXEC-COMANDO
           CALL "CWEXEC" USING PARAMETROS-CWEXEC

          MOVE SPACES TO LB-CWDIRS
          STRING PATH-SPOOL DELIMITED BY SPACE
                 "\CWDIRS"  DELIMITED BY SIZE
                 INTO LB-CWDIRS
          OPEN I-O CWDIRS
          OPEN INPUT PASTAS.

       800-99-FIM. EXIT.

       900-FINAIS.

            CLOSE CWDIRS PASTAS
            DELETE FILE PASTAS.

       900-99-FIM. EXIT.

       END PROGRAM ASAREC.
