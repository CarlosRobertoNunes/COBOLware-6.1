      $SET CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPAGE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/07/2013.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica se pagina de menu est  habilitada   *
                      *  para o grupo                                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT PAGINAS  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS PAGINAS-CHAVE
                  ALTERNATE KEY IS PAGINAS-FLAG WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-PAGINAS.

       DATA DIVISION.
       FILE SECTION.

       FD  PAGINAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CWPAGE$$".

       01  PAGINAS-REG.
           05 PAGINAS-CHAVE PIC X(8).
           05 PAGINAS-FLAG  PIC X(1).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 OPCOES-VALIDAS           PIC  9(004) VALUE 0.
           05 FL-CWCONF                PIC  X(001) VALUE "C".
           05 TP                       PIC  X(001) VALUE SPACE.
           05 IP                       PIC  9(002) VALUE 0.
           05 DP                       PIC  9(002) VALUE 0.
           05 YP                       PIC  9(002) VALUE 0.
           05 FS-PAGINAS               PIC  X(002) VALUE "00".
           05 PAGINA                   PIC  9(004) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 SAVE                     PIC  X(008) VALUE SPACE.
           05 SHOWCOMENT               PIC  X(002) VALUE SPACES.
           05 SP2                      PIC  X(005) VALUE SPACES.

       COPY CWCONF.

       LINKAGE SECTION.

       01  PROG                   PIC  X(008).
       01  GRUPO                  PIC  X(022).
       01  NIVEL                  PIC  9(001).
       01  PROG-OPT               PIC  X(008).

       SCREEN SECTION.

       PROCEDURE DIVISION USING PROG GRUPO NIVEL PROG-OPT.

       000-INICIO.

           IF   PROG (1:1) = '!'
                IF FL-CWCONF = "O"
                   SET CWSQLC-CLOSE TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                       KCO PCO
                   MOVE "C" TO FL-CWCONF
                END-IF
                GOBACK
           END-IF

           IF  (GRUPO NOT = SPACES)
           AND (GRUPO NOT = "Acesso sem restri‡”es")
           AND (GRUPO NOT = "Acesso sem restri‡äes")
           AND (GRUPO NOT = "Acesso sem restricoes")
           AND (GRUPO NOT = "Acesso irrestrito")
               CONTINUE
           ELSE
               GOBACK
           END-IF

           ON   1
                DISPLAY "SP2END" UPON ENVIRONMENT-NAME
                ACCEPT  SP2      FROM ENVIRONMENT-VALUE
                IF  SP2 = "6"
                    DISPLAY "CWMENUCOMMENT" UPON ENVIRONMENT-NAME
                    ACCEPT  SHOWCOMENT      FROM ENVIRONMENT-VALUE
                    INSPECT SHOWCOMENT
                            CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   PROG (1:1) = ':'
           AND  (SHOWCOMENT NOT = 'ON')
                IF FL-CWCONF = "C"
                   MOVE "O" TO FL-CWCONF
                   SET CWSQLC-OPEN TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                              KCO PCO
                END-IF
                MOVE 0                 TO OPCOES-VALIDAS
                MOVE "GU"              TO CWGRPS-REG
                MOVE GRUPO             TO CWGRPS-NOME-GRUPO
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-EQUAL       TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KCO PCO
                IF   FS-CWGRPS < "09"
                     MOVE CWGRPS-ADM    TO ADM
                     OPEN I-O PAGINAS
                     MOVE PROG          TO PAGINAS-CHAVE
                     MOVE '2'           TO PAGINAS-FLAG
                     WRITE PAGINAS-REG
                     PERFORM VERIFICA THRU FIM-VERIFICA
                     CLOSE PAGINAS
                END-IF
                IF  OPCOES-VALIDAS = ZERO
                    MOVE SPACES TO PROG-OPT
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       VERIFICA.

           MOVE '2'      TO PAGINAS-FLAG
           START PAGINAS KEY NOT LESS PAGINAS-FLAG
           IF FS-PAGINAS > '09'
              GO TO FIM-VERIFICA
           END-IF

           READ PAGINAS NEXT RECORD
           IF FS-PAGINAS > '09'
              GO TO FIM-VERIFICA
           END-IF
           MOVE PAGINAS-CHAVE TO SAVE
           MOVE 5             TO YP
           MOVE 0             TO PAGINA
                                 DP
           PERFORM VARYING IP FROM LENGTH PAGINAS-CHAVE BY -1
                   UNTIL IP = 0
                   MOVE PAGINAS-CHAVE (IP:1) TO TP
                   IF (TP = ':' AND IP > 1)
                   AND DP = 0
                       MOVE PAGINA TO DP
                       MOVE 0 TO PAGINA
                       MOVE 5 TO YP
                   END-IF
                   IF TP NUMERIC
                   AND YP > 1
                       SUBTRACT 1 FROM YP
                       MOVE TP TO PAGINA (YP: 1)
                   END-IF
           END-PERFORM
           MOVE "SM"    TO CWCONF-REG99
           MOVE PAGINA  TO CWCONF-PAGINA
           SET CWSQLC-READ        TO TRUE
           SET CWSQLC-EQUAL       TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC
                               CWCONF-REG
                               FS-CWCONF
                               KCO PCO
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 26
                      OR FS-CWCONF NOT = "00"
             IF  CWCONF-NIVEL (I) NOT NUMERIC
                 MOVE 0 TO CWCONF-NIVEL (I)
             END-IF
             IF (NIVEL NOT LESS CWCONF-NIVEL (I))
             AND (CWCONF-PROG (I) NOT = SPACES)
                MOVE "GU"            TO CWGRPS-REG
                MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                MOVE CWCONF-PROG (I) TO CWGRPS-PROG-GRUPO
                CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (I)
                                        CWCONF-FATOR-P-99 (I)
                                        CWGRPS-PROG-GRUPO
                CALL "CWCODE" USING CWGRPS-PROG-GRUPO
                          LENGTH OF CWGRPS-PROG-GRUPO
                INSPECT CWGRPS-REG CONVERTING MINUSCULAS TO MAIUSCULAS
                CALL "CWVARX" USING CWGRPS-PROG-GRUPO
                          LENGTH OF CWGRPS-PROG-GRUPO
                IF   CWGRPS-PROG-GRUPO = SPACES
                     MOVE SPACES TO CWCONF-PROG    (I)
                     EXIT PERFORM CYCLE
                END-IF
                IF   CWGRPS-PROG-GRUPO = "CWMENU" OR 'GRMENU'
                     ADD 1 TO OPCOES-VALIDAS
                     EXIT PERFORM
                END-IF
                IF   CWGRPS-PROG-GRUPO = "CWBOXS" OR "GRBOXS"
                                      OR "CWPAGE" OR 'GRPAGE'
                     IF CWGRPS-PROG-GRUPO = "CWPAGE" OR 'GRPAGE'
                        MOVE ";"          TO CWGRPS-PROG-GRUPO (1: 1)
                     ELSE
                        MOVE ":"          TO CWGRPS-PROG-GRUPO (1: 1)
                     END-IF
                     MOVE CWCONF-HELP (I) TO CWGRPS-PROG-GRUPO (2: )
                ELSE
                     IF   CWGRPS-PROG-GRUPO = "CWREL2"
                     AND (CWCONF-HELP (I) NOT = SPACES)
                          MOVE "*"  TO CWGRPS-PROG-GRUPO (1: 1)
                          MOVE CWCONF-HELP (I)
                            TO CWGRPS-PROG-GRUPO (2: )
                          INSPECT CWGRPS-PROG-GRUPO (2: )
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                END-IF
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWGRPS-REG FS-CWGRPS KCO PCO
                IF  (FS-CWGRPS < "09"
                AND  CWGRPS-ACESSO-GRUPO NOT = SPACE
                AND  ADM                 NOT = "I")
                OR  (FS-CWGRPS = "23"
                AND  ADM                     = "I")
                     MOVE SPACES TO CWGRPS-PROG-GRUPO
                ELSE
                     IF   FS-CWGRPS > "09"
                     AND  FS-CWGRPS NOT = "23"
                          CALL "CWCONF" USING "ISAM"
                     ELSE
                          IF CWGRPS-PROG-GRUPO (1: 1) = ':'
                             MOVE CWGRPS-PROG-GRUPO TO PAGINAS-CHAVE
                             MOVE '2'               TO PAGINAS-FLAG
                             WRITE PAGINAS-REG
                          ELSE
                             ADD 1 TO OPCOES-VALIDAS
                          END-IF
                     END-IF
                END-IF
             END-IF
           END-PERFORM
           MOVE SAVE TO PAGINAS-CHAVE
           READ PAGINAS
           MOVE '1' TO PAGINAS-FLAG
           REWRITE PAGINAS-REG
           IF OPCOES-VALIDAS = ZERO
              GO TO VERIFICA
           END-IF.

       FIM-VERIFICA. EXIT.
       END PROGRAM CWPAGE.
