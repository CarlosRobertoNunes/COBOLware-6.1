      $Set PreProcess(cobsql)
      $Set LinkCount(128) Gnt()
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ORACLE-FILENAME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/08/2017 11:57:05.
       SECURITY.      *************************************************
                      *                                               *
                      * SQL Handler FILENAME COBOL                    *
                      * Gerado por base no programa FNAME             *
                      *                      http://www.COBOLware.com *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WT-FILENAME-DESCRICAO.
           05 WS-DESCRICAO PIC X(30).
           05 WT-END-FILENAME-DESCRICAO.
              06 WS-CODIGO PIC 9(5).

       01  ISAM-CONTROL.
           05 ISAM-FREES                      VALUE SPACES.
              10 ISAM-FREE        PIC  X(001) OCCURS 98.
           05 ISAM-LEVEL          PIC  9(002) VALUE 0.

       77  WS-CWSQLC              PIC  X(020) VALUE SPACES.
       77  WS-CREATE-INDEX        PIC  9(001) VALUE 0.
       77  WS-LOAD                PIC  X(030) VALUE SPACES.
           88 VERIFY-OFF VALUE "oracle-filename" "OFF".

       01  KEYTABLE.
           05 PIC X(30) VALUE "CHAVE".
           05 PIC X(30) VALUE "DESCRICAO".
       01  REDEFINES KEYTABLE.
           05 KEYNAME             PIC  X(030) OCCURS 2 TIMES.

       01  MENSAGEM-LONGA         PIC  X(255) VALUE SPACES.
       01  CWSQLISAM              PIC  X(003) VALUE "OFF".
       01  CWSAVEPOINT            PIC  X(003) VALUE "OFF".
       01  CHECK-POINT            PIC  9(001) VALUE 0.
           88 CHECK-POINT-OFF                 VALUE 0.
           88 CHECK-POINT-ON                  VALUE 1.
       01  SQLMSG                 PIC  X(050) VALUE SPACES.
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-POSITION           PIC  X(254).
       01  SQL-LAST-ROWID         PIC  X(018).
       01  SQL-CWID               PIC  X(018).
       01  SQL-RECORD.
           02 SQL-CODIGO    PIC S9(5) COMP-3.
           02 SQL-DESCRICAO PIC  X(30).
           02 SQL-PRECO     PIC S9(8)V9(2) COMP-3.
           02 SQL-TIPO      PIC S9(1) COMP-3.
           02 SQL-IMPORTADO PIC S9(1) COMP-3.
           02 SQL-GARANTIA  PIC S9(1) COMP-3.
           02 SQL-DURAVEL   PIC S9(1) COMP-3.
       01  TG-FILENAME-DESCRICAO PIC X(35).
       01  SQL-ENDECLARE          PIC  X(001).
           EXEC SQL END   DECLARE SECTION END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  KEYSTRU.
           02 KEYSTRU-LENGTH PIC 9(4) COMP-X VALUE   66.
           02                PIC X(8)        VALUE X'0200000000020000'.
           02                PIC X(8)        VALUE X'000000000001002E'.
           02                PIC X(8)        VALUE X'1000002000000000'.
           02                PIC X(8)        VALUE X'0000000000010038'.
           02                PIC X(8)        VALUE X'4000002000000000'.
           02                PIC X(8)        VALUE X'0000000000000000'.
           02                PIC X(8)        VALUE X'0000000000050000'.
           02                PIC X(8)        VALUE X'000000050000001E'.

       LINKAGE SECTION.

       COPY CWSQLC.

       01  FD-FILENAME.
           05 FILENAME-CHAVE.
              10 FILENAME-CODIGO PIC 9(005).
           05 FILENAME-DESCRICAO PIC X(030).
           05 FILENAME-PRECO PIC 9(008)V99.
           05 FILENAME-TIPO PIC 9(001).
           05 FILENAME-OPCOES.
              10 FILENAME-IMPORTADO PIC 9(001).
              10 FILENAME-GARANTIA PIC 9(001).
              10 FILENAME-DURAVEL PIC 9(001).

       01  ISAM-FILENAME.
           05 CWSQLC-FS              PIC X(002).
              88 CWSQLC-OK  VALUE "22" "23" "9D".
              88 CWSQLC-NORMAL
                 VALUE "10" "22" "23" "35" "39" "42" "9A" "9D".

       01  POINTER-FILENAME.
           05 POINTER-NUMBER      PIC  9(003).
           05                     PIC  X(001).
           05 POINTER-KEY         PIC  X(030).
           05 FROM-STATUS         PIC  X(002).
              88 FROM-STATUS-OFF       VALUE SPACES.
              88 FROM-EQUAL            VALUE "=".
              88 FROM-LESS             VALUE "<".
              88 FROM-GREATER          VALUE ">".
              88 FROM-NOT-LESS         VALUE ">=".
              88 FROM-NOT-GREATER      VALUE "<=".
              88 FROM-NEXT             VALUE "->".
              88 FROM-PREVIOUS         VALUE "<-".
              88 FROM-START            VALUE "=" "<" ">" ">=" "<=".
              88 FROM-READ             VALUE "R".
           05 COMMIT-MODE         PIC  9(001).
              88 AUTO-COMMIT           VALUE 0.
              88 COMMIT-MANUAL         VALUE 1.
           05 MAX-REC-FLAG        PIC  X(001).
           05 MAX-REC-LENGTH      PIC  9(004) COMP-X.
           05 CURSOR-MODE         PIC  X(002).
           05 START-STATUS        PIC  9(001).
              88 FIRST-START-ON        VALUE 0.
              88 FIRST-START-OFF       VALUE 1.
           05 OPEN-MODE           PIC  9(001).
              88 OPENED-UPDATE         VALUE 1.
              88 OPENED-INQUIRY        VALUE 0.
           05 ACTIVE-KEY          PIC  X(030).
           05.
              10 SAVE-POSITION    PIC  X(255).
              10 SAVE-CWID        PIC  X(018).
              10 SAVE-ROWID       PIC  X(018).
              10 SAVE-LAST-ROWID  PIC  X(018).
           05 POINTER-VOLUME      PIC  9(018) COMP-3.

       01  LBS-NAME               PIC X(255).
       01  FCD-MAX-REC-LENGTH     PIC 9(4) COMP-X.
       01  FCD-RELATIVE-KEY       PIC 9(9) COMP-X.
       01  FCD-SESSION-ID         PIC 9(9) COMP-X.
       01  FCD-KEY-DEF-ADDRESS    USAGE POINTER.
       01  KEY-DEF                PIC X.

       PROCEDURE DIVISION USING CWSQLC
                                FD-FILENAME
                                ISAM-FILENAME
                                POINTER-FILENAME
                                LBS-NAME
                                FCD-MAX-REC-LENGTH
                                FCD-RELATIVE-KEY
                                FCD-SESSION-ID
                                FCD-KEY-DEF-ADDRESS.

           ON 1
              DISPLAY "CWSAVEPOINT" UPON ENVIRONMENT-NAME
              ACCEPT   CWSAVEPOINT  FROM ENVIRONMENT-VALUE
              INSPECT  CWSAVEPOINT  CONVERTING "on" TO "ON"
              DISPLAY "CWSQLISAM"   UPON ENVIRONMENT-NAME
              ACCEPT   CWSQLISAM    FROM ENVIRONMENT-VALUE
              INSPECT  CWSQLISAM    CONVERTING "on" TO "ON".

       000-INICIO.

           CALL "CWSQLT" USING "Get" COMMIT-MODE

           IF   CWSQLC-COMMIT-MANUAL
                SET COMMIT-MANUAL TO TRUE
                CALL "CWSQLT" USING "Set" COMMIT-MODE
                GOBACK
           END-IF

           IF   CWSQLC-COMMIT-AUTOMATIC
                SET AUTO-COMMIT TO TRUE
                CALL "CWSQLT" USING "Set" COMMIT-MODE
                GOBACK
           END-IF

           IF   CWSQLC-CREATE
           OR   CWSQLC-OPEN
           OR   CWSQLC-ISAM = 0
                IF   MAX-REC-FLAG = "!" OR "*"
                     IF  MAX-REC-LENGTH NOT = LENGTH FD-FILENAME
                         MOVE "39" TO CWSQLC-FS
                         GOBACK
                     END-IF
                END-IF
                IF   MAX-REC-FLAG = "!"
                     SET ADDRESS OF KEY-DEF TO FCD-KEY-DEF-ADDRESS
                     MOVE KEY-DEF(1:2) TO KEYSTRU-LENGTH(1:2)
                     IF   KEY-DEF(1:KEYSTRU-LENGTH) NOT = KEYSTRU
                          MOVE "39" TO CWSQLC-FS
                          GOBACK
                     END-IF
                END-IF
                IF   FD-FILENAME = LOW-VALUES
                OR   SPACES OR ZEROS
                     INITIALIZE FD-FILENAME
                END-IF
                IF  POINTER-FILENAME = SPACES
                    INITIALIZE POINTER-FILENAME
                END-IF
                PERFORM VARYING CWSQLC-ISAM
                           FROM 1 BY 1
                          UNTIL CWSQLC-ISAM > LENGTH ISAM-FREES
                        IF ISAM-FREE (CWSQLC-ISAM) = SPACE
                           MOVE "*" TO ISAM-FREE (CWSQLC-ISAM)
                           EXIT PERFORM
                        END-IF
                END-PERFORM
                INITIALIZE SQL-CWID SQL-LAST-ROWID
           END-IF

           IF   CWSQLC-OPEN OR CWSQLC-CREATE
                MOVE SPACES TO WS-LOAD
                DISPLAY "ORACLE-VERIFY" UPON ENVIRONMENT-NAME
                ACCEPT   WS-LOAD        FROM ENVIRONMENT-VALUE
                INSPECT WS-LOAD CONVERTING "of" TO "OF"
                IF  NOT VERIFY-OFF
                    DISPLAY "TABLE-LOAD" UPON ENVIRONMENT-NAME
                    ACCEPT   WS-LOAD     FROM ENVIRONMENT-VALUE
                END-IF
           END-IF

           INITIALIZE SQLCODE
           IF  (NOT CWSQLC-READ)
           AND (NOT CWSQLC-START)
           AND (NOT CWSQLC-UNLOCK)
           AND (NOT CWSQLC-ROLLBACK)
               SET FROM-STATUS-OFF TO TRUE
           END-IF

           IF   CWSQLC-KEY NOT = 0
                MOVE KEYNAME (CWSQLC-KEY) TO POINTER-KEY
           END-IF

           IF   POINTER-KEY = SPACES OR "MEMORDINATE"
                MOVE "CHAVE" TO POINTER-KEY
                IF   (ACTIVE-KEY NOT = SPACES)
                AND  (NOT CWSQLC-EQUAL)
                     MOVE ACTIVE-KEY TO POINTER-KEY
                END-IF
           END-IF

           IF  (CWSQLC-REWRITE OR CWSQLC-DELETE)
           AND  SAVE-ROWID = SPACES
                MOVE "23" TO CWSQLC-FS
                GOBACK
           END-IF

           MOVE SAVE-POSITION        TO SQL-POSITION
           MOVE SAVE-LAST-ROWID (1:) TO SQL-LAST-ROWID (1:)
           MOVE SAVE-ROWID      (1:) TO SQL-CWID       (1:)

           IF   CWSQLC-NO-DATA
                GO TO 000-METODOS
           END-IF

           IF   FILENAME-CODIGO(1:) = HIGH-VALUES
                MOVE 99999 TO SQL-CODIGO
           ELSE
                IF   FILENAME-CODIGO NOT NUMERIC
                     COMPUTE FILENAME-CODIGO
                           = FUNCTION NUMVAL(FILENAME-CODIGO)
                END-IF
                MOVE FILENAME-CODIGO TO SQL-CODIGO
           END-IF

           IF   FILENAME-DESCRICAO(1:) = LOW-VALUES
           AND (CWSQLC-OPEN OR CWSQLC-START)
                MOVE SPACES TO SQL-DESCRICAO
           ELSE
                MOVE FILENAME-DESCRICAO TO SQL-DESCRICAO
           END-IF

           IF   FILENAME-PRECO(1:) = HIGH-VALUES
                MOVE 99999999,99 TO SQL-PRECO
           ELSE
                IF   FILENAME-PRECO NOT NUMERIC
                     COMPUTE FILENAME-PRECO
                           = FUNCTION NUMVAL(FILENAME-PRECO)
                END-IF
                MOVE FILENAME-PRECO TO SQL-PRECO
           END-IF

           IF   FILENAME-TIPO(1:) = HIGH-VALUES
                MOVE 9 TO SQL-TIPO
           ELSE
                IF   FILENAME-TIPO NOT NUMERIC
                     COMPUTE FILENAME-TIPO
                           = FUNCTION NUMVAL(FILENAME-TIPO)
                END-IF
                MOVE FILENAME-TIPO TO SQL-TIPO
           END-IF

           IF   FILENAME-IMPORTADO(1:) = HIGH-VALUES
                MOVE 9 TO SQL-IMPORTADO
           ELSE
                IF   FILENAME-IMPORTADO NOT NUMERIC
                     COMPUTE FILENAME-IMPORTADO
                           = FUNCTION NUMVAL(FILENAME-IMPORTADO)
                END-IF
                MOVE FILENAME-IMPORTADO TO SQL-IMPORTADO
           END-IF

           IF   FILENAME-GARANTIA(1:) = HIGH-VALUES
                MOVE 9 TO SQL-GARANTIA
           ELSE
                IF   FILENAME-GARANTIA NOT NUMERIC
                     COMPUTE FILENAME-GARANTIA
                           = FUNCTION NUMVAL(FILENAME-GARANTIA)
                END-IF
                MOVE FILENAME-GARANTIA TO SQL-GARANTIA
           END-IF

           IF   FILENAME-DURAVEL(1:) = HIGH-VALUES
                MOVE 9 TO SQL-DURAVEL
           ELSE
                IF   FILENAME-DURAVEL NOT NUMERIC
                     COMPUTE FILENAME-DURAVEL
                           = FUNCTION NUMVAL(FILENAME-DURAVEL)
                END-IF
                MOVE FILENAME-DURAVEL TO SQL-DURAVEL
           END-IF.

       000-METODOS.

           IF   CWSQLC-START OR (CWSQLC-READ AND CWSQLC-EQUAL)
                MOVE FILENAME-DESCRICAO
                  TO WS-DESCRICAO IN WT-FILENAME-DESCRICAO
                MOVE FILENAME-CODIGO
                  TO WS-CODIGO    IN WT-FILENAME-DESCRICAO
           END-IF
           MOVE "00" TO CWSQLC-FS
           EVALUATE TRUE
               WHEN CWSQLC-CREATE
                    PERFORM 050-CREATE THRU 050-99-FIM
                    IF   CWSQLC-FS < "10"
                         SET OPENED-UPDATE TO TRUE
                    ELSE
                         MOVE SPACE TO ISAM-FREE (CWSQLC-ISAM)
                    END-IF
               WHEN CWSQLC-OPEN
                    IF  CWSQLC-UPDATE
                        SET OPENED-UPDATE  TO TRUE
                    ELSE
                        SET OPENED-INQUIRY TO TRUE
                    END-IF
                    MOVE    CWSQLC                TO WS-CWSQLC
                    PERFORM 051-OPEN-SEQUENTIAL THRU 051-99-FIM
                    IF   CWSQLC-FS > "09"
                         MOVE SPACE TO ISAM-FREE (CWSQLC-ISAM)
                    END-IF
               WHEN CWSQLC-CLOSE
                    IF   ISAM-LEVEL = 1
                         MOVE 0     TO ISAM-LEVEL
                         MOVE SPACE TO CWSQLC-OPTION
                    END-IF
                    IF   CWSQLC-CREATE-INDEXES
                    OR   WS-CREATE-INDEX = 1
                         MOVE 0 TO WS-CREATE-INDEX
                         PERFORM 065-CREATE-INDEX THRU 065-99-FIM
                    END-IF
                    IF   CWSQLC-EXCLUSIVE
                         PERFORM 043-COMMIT THRU 043-99-FIM
                    END-IF
                    IF   CWSQLC-ISAM = ZERO
                    OR   ISAM-FREE (CWSQLC-ISAM) = SPACE
                         MOVE "42" TO CWSQLC-FS
                    ELSE
                         MOVE SPACE TO ISAM-FREE (CWSQLC-ISAM)
                         IF   ISAM-FREES = SPACES
                              SET CWSQLC-CANCEL TO TRUE
                         END-IF
                         SET  OPENED-INQUIRY TO TRUE
                         IF   SQLCODE NOT = ZERO
                              MOVE "42" TO CWSQLC-FS
                         END-IF
                    END-IF
               WHEN CWSQLC-START
               OR  (CWSQLC-READ AND CWSQLC-EQUAL)
                    PERFORM 010-START  THRU 010-99-FIM
               WHEN CWSQLC-READ
                    PERFORM 030-READ   THRU 030-99-FIM
               WHEN CWSQLC-WRITE
                    PERFORM 040-INSERT THRU 040-99-FIM
               WHEN CWSQLC-REWRITE
                    PERFORM 041-UPDATE THRU 041-99-FIM
               WHEN CWSQLC-DELETE
                    PERFORM 042-ERASE  THRU 042-99-FIM
               WHEN CWSQLC-UNLOCK
                    PERFORM 043-COMMIT THRU 043-99-FIM
               WHEN CWSQLC-ROLLBACK
                    PERFORM 044-ROLLBACK THRU 044-99-FIM
               WHEN CWSQLC-KILL
                    EXEC SQL
                         DROP TABLE FILENAME
                    END-EXEC
                    IF   SQLCODE = -54
                         MOVE "9A" TO CWSQLC-FS
                    ELSE
                         PERFORM 043-COMMIT THRU 043-99-FIM
                    END-IF
               WHEN OTHER
                    MOVE "9d" TO CWSQLC-FS
           END-EVALUATE

           IF   CWSQLC-READ
           AND  CWSQLC-FS < "10"
                MOVE SQL-CODIGO
                  TO FILENAME-CODIGO
                MOVE SQL-DESCRICAO
                  TO FILENAME-DESCRICAO
                MOVE SQL-PRECO
                  TO FILENAME-PRECO
                MOVE SQL-TIPO
                  TO FILENAME-TIPO
                MOVE SQL-IMPORTADO
                  TO FILENAME-IMPORTADO
                MOVE SQL-GARANTIA
                  TO FILENAME-GARANTIA
                MOVE SQL-DURAVEL
                  TO FILENAME-DURAVEL
           END-IF

           IF   SQLCODE NOT = ZERO
                DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                DISPLAY  SQLERRMC  UPON ENVIRONMENT-VALUE
           END-IF

           MOVE SQL-POSITION        TO SAVE-POSITION
           MOVE SQL-LAST-ROWID (1:) TO SAVE-LAST-ROWID (1:)
           MOVE SQL-CWID       (1:) TO SAVE-ROWID      (1:).

       000-99-FIM. GOBACK.

       010-START.

           IF  (ACTIVE-KEY NOT = SPACES)
           AND (POINTER-KEY NOT = ACTIVE-KEY)
               SET FIRST-START-ON TO TRUE
           END-IF

           EVALUATE POINTER-KEY ALSO TRUE
               WHEN "CHAVE" ALSO CWSQLC-NOT-LESS
                    EXEC SQL
                         SELECT ROWID
                           INTO :SQL-CWID
                                 FROM FILENAME
                              WHERE ( CODIGO >=
                                 :SQL-CODIGO
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "CHAVE" ALSO CWSQLC-GREATER
                    EXEC SQL
                         SELECT ROWID
                           INTO :SQL-CWID
                                 FROM FILENAME
                              WHERE ( CODIGO >
                                 :SQL-CODIGO
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "CHAVE" ALSO CWSQLC-NOT-GREATER
                    EXEC SQL
                         SELECT CODIGO, ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                                WHERE CODIGO = (
                              SELECT MAX(CODIGO)
                                     FROM FILENAME
                                    WHERE CODIGO <=
                                 :SQL-CODIGO )
                    END-EXEC
               WHEN "CHAVE" ALSO CWSQLC-LESS
                    EXEC SQL
                         SELECT CODIGO, ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                                WHERE CODIGO = (
                              SELECT MAX(CODIGO)
                                     FROM FILENAME
                                    WHERE CODIGO <
                                 :SQL-CODIGO )
                    END-EXEC
               WHEN "CHAVE" ALSO CWSQLC-EQUAL
                    EXEC SQL
                         SELECT ROWID
                           INTO :SQL-CWID
                                 FROM FILENAME
                              WHERE CODIGO =
                                 :SQL-CODIGO
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-NOT-LESS
                    IF(( WT-FILENAME-DESCRICAO (1:30) NOT
                    = TG-FILENAME-DESCRICAO (1:30) )
           AND  WT-FILENAME-DESCRICAO (31:)
            = TG-FILENAME-DESCRICAO (31:) )
                    OR   FIRST-START-ON
                         INITIALIZE WT-END-FILENAME-DESCRICAO
                                 OF WT-FILENAME-DESCRICAO
                    END-IF
                    MOVE WT-FILENAME-DESCRICAO TO SQL-POSITION
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
                  /*+ INDEX(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                             || LPAD(ABS(CODIGO),5,0) >=
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-GREATER
                    IF(( WT-FILENAME-DESCRICAO (1:30) NOT
                    = TG-FILENAME-DESCRICAO (1:30) )
           AND  WT-FILENAME-DESCRICAO (31:)
            = TG-FILENAME-DESCRICAO (31:) )
                    OR   FIRST-START-ON
                         INITIALIZE WT-END-FILENAME-DESCRICAO
                                 OF WT-FILENAME-DESCRICAO
                    END-IF
                    MOVE WT-FILENAME-DESCRICAO TO SQL-POSITION
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
                  /*+ INDEX(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                              || LPAD(ABS(CODIGO),5,0) >
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-NOT-GREATER
                    IF(( WT-FILENAME-DESCRICAO (1:30) NOT
                    = TG-FILENAME-DESCRICAO (1:30) )
           AND  WT-FILENAME-DESCRICAO (31:)
            = TG-FILENAME-DESCRICAO (31:) )
                    OR   FIRST-START-ON
                         INITIALIZE WT-END-FILENAME-DESCRICAO
                                 OF WT-FILENAME-DESCRICAO
                               REPLACING NUMERIC DATA BY ALL "9"
                                         ALPHABETIC DATA BY ALL X"AD"
                    END-IF
                    MOVE WT-FILENAME-DESCRICAO TO SQL-POSITION
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
             /*+ INDEX_DESC(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                             || LPAD(ABS(CODIGO),5,0) <=
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                  AND ROWNUM = 1)
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-LESS
                    IF(( WT-FILENAME-DESCRICAO (1:30) NOT
                    = TG-FILENAME-DESCRICAO (1:30) )
           AND  WT-FILENAME-DESCRICAO (31:)
            = TG-FILENAME-DESCRICAO (31:) )
                    OR   FIRST-START-ON
                         INITIALIZE WT-END-FILENAME-DESCRICAO
                                 OF WT-FILENAME-DESCRICAO
                               REPLACING NUMERIC DATA BY ALL "9"
                                         ALPHABETIC DATA BY ALL X"AD"
                    END-IF
                    MOVE WT-FILENAME-DESCRICAO TO SQL-POSITION
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
             /*+ INDEX_DESC(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                              || LPAD(ABS(CODIGO),5,0) <
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                  AND ROWNUM = 1)
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-EQUAL
                    IF(( WT-FILENAME-DESCRICAO (1:30) NOT
                    = TG-FILENAME-DESCRICAO (1:30) )
           AND  WT-FILENAME-DESCRICAO (31:)
            = TG-FILENAME-DESCRICAO (31:) )
                    OR   FIRST-START-ON
                         INITIALIZE WT-END-FILENAME-DESCRICAO
                                 OF WT-FILENAME-DESCRICAO
                    END-IF
                    MOVE WT-FILENAME-DESCRICAO TO SQL-POSITION
                    EXEC SQL
                         SELECT RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( DESCRICAO =
                 NVL(RTRIM(:SQL-DESCRICAO,' '),' ')
                                AND  ROWNUM = 1 )
                    END-EXEC
           END-EVALUATE

           SET FIRST-START-OFF TO TRUE
           INITIALIZE SAVE-CWID

           IF   SQLCODE = 0
                MOVE CWSQLC-OPTION TO FROM-STATUS
                IF   CWSQLC-READ
                     PERFORM 060-SELECT THRU 060-99-FIM
                     MOVE SQL-CWID TO SAVE-CWID
                     SET FROM-READ TO TRUE
                END-IF
           ELSE
                MOVE "23" TO CWSQLC-FS
           END-IF

           MOVE POINTER-KEY TO ACTIVE-KEY.

       010-99-FIM. EXIT.

       030-READ.

           IF   FROM-NOT-GREATER
           AND  CWSQLC-NEXT
                SET CWSQLC-PREVIOUS TO TRUE
           END-IF

           IF   FROM-START
                MOVE CWSQLC-OPTION TO FROM-STATUS
                GO TO 030-GET
           END-IF

           EVALUATE POINTER-KEY ALSO TRUE
               WHEN "CHAVE" ALSO CWSQLC-NEXT
                    EXEC SQL
                         SELECT ROWID
                           INTO :SQL-CWID
                                 FROM FILENAME
                              WHERE ( CODIGO >
                                 :SQL-CODIGO
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "CHAVE" ALSO CWSQLC-PREVIOUS
                    EXEC SQL
                         SELECT CODIGO, ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                                WHERE CODIGO = (
                              SELECT MAX(CODIGO)
                                     FROM FILENAME
                                    WHERE CODIGO <
                                 :SQL-CODIGO )
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-NEXT
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
                  /*+ INDEX(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                              || LPAD(ABS(CODIGO),5,0) >
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                AND  ROWNUM = 1 )
                    END-EXEC
               WHEN "DESCRICAO" ALSO CWSQLC-PREVIOUS
                    MOVE SQL-POSITION TO TG-FILENAME-DESCRICAO
                    EXEC SQL
                         SELECT
             /*+ INDEX_DESC(FILENAME FILENAME_DESCRICAO) */
                                RPAD(DESCRICAO,30,' ')
                             || LPAD(ABS(CODIGO),5,0),
                             ROWID
                           INTO :SQL-POSITION, :SQL-CWID
                                 FROM FILENAME
                              WHERE ( RPAD(DESCRICAO,30,' ')
                                              || LPAD(ABS(CODIGO),5,0) <
                                     RPAD(:TG-FILENAME-DESCRICAO,35,' ')
                                  AND ROWNUM = 1)
                    END-EXEC
               WHEN OTHER
                    MOVE "39" TO CWSQLC-FS
                    GO TO 030-99-FIM
           END-EVALUATE.

       030-GET.

           IF   SQLCODE = 0
                PERFORM 060-SELECT THRU 060-99-FIM
                IF   SQL-CWID = SAVE-CWID
                AND((FROM-NEXT     AND CWSQLC-PREVIOUS)
                 OR (FROM-PREVIOUS AND CWSQLC-NEXT))
                     MOVE CWSQLC-OPTION TO FROM-STATUS
                     GO TO 030-READ
                END-IF
                MOVE SQL-CWID TO SAVE-CWID
                MOVE CWSQLC-OPTION TO FROM-STATUS
           ELSE
                MOVE "10" TO CWSQLC-FS
                SET FROM-EQUAL TO TRUE
           END-IF.

       030-99-FIM. EXIT.

       040-INSERT.

           INITIALIZE SQL-CWID
           EXEC SQL
                INSERT INTO FILENAME (
                     CODIGO,
                     DESCRICAO,
                     PRECO,
                     TIPO,
                     IMPORTADO,
                     GARANTIA,
                     DURAVEL
                     )
                VALUES (
                       NVL(:SQL-CODIGO,0),
                       NVL(RTRIM(:SQL-DESCRICAO,' '),' '),
                       NVL(:SQL-PRECO,0),
                       NVL(:SQL-TIPO,0),
                       NVL(:SQL-IMPORTADO,0),
                       NVL(:SQL-GARANTIA,0),
                       NVL(:SQL-DURAVEL,0)
                       )
           END-EXEC
           IF   SQLCODE = -1
                MOVE "22" TO CWSQLC-FS
           ELSE
                IF   SQLCODE NOT = ZERO
                     PERFORM 070-ERRO-SQL THRU 070-99-FIM
                ELSE
                     MOVE SQL-CODIGO TO FILENAME-CODIGO
                     IF   NOT COMMIT-MANUAL
                          PERFORM 043-COMMIT THRU 043-99-FIM
                     END-IF
                     IF   VERIFY-OFF
                     OR   COMMIT-MANUAL
                          EXIT PARAGRAPH
                     END-IF
                END-IF
           END-IF.

       040-99-FIM. EXIT.

       041-UPDATE.

           MOVE SAVE-CWID TO SQL-LAST-ROWID

           EXEC SQL
               UPDATE FILENAME SET
                       CODIGO    =
                  NVL(:SQL-CODIGO,0),
                       DESCRICAO =
                  NVL(RTRIM(:SQL-DESCRICAO,' '),' '),
                       PRECO     =
                  NVL(:SQL-PRECO,0),
                       TIPO      =
                  NVL(:SQL-TIPO,0),
                       IMPORTADO =
                  NVL(:SQL-IMPORTADO,0),
                       GARANTIA  =
                  NVL(:SQL-GARANTIA,0),
                       DURAVEL   =
                  NVL(:SQL-DURAVEL,0)
                              WHERE CODIGO =
                                 :SQL-CODIGO
           END-EXEC
           IF    SQLCODE NOT = ZERO
                 IF    SQLCODE = -1
                       MOVE "22" TO CWSQLC-FS
                 ELSE
                       IF    SQLCODE = -1410 OR +1403
                             MOVE "23" TO CWSQLC-FS
                       ELSE
                             MOVE "9D" TO CWSQLC-FS
                             PERFORM 070-ERRO-SQL THRU 070-99-FIM
                       END-IF
                 END-IF
           ELSE
                 IF   NOT COMMIT-MANUAL
                      PERFORM 043-COMMIT THRU 043-99-FIM
                 END-IF
                 MOVE SQL-LAST-ROWID TO SQL-CWID
                 PERFORM 060-SELECT THRU 060-99-FIM
           END-IF.

       041-99-FIM. EXIT.

       042-ERASE.

           EXEC SQL
                DELETE FROM FILENAME
                              WHERE CODIGO =
                                 :SQL-CODIGO
           END-EXEC
           IF   SQLCODE NOT = ZERO
                MOVE "23" TO CWSQLC-FS
           ELSE
                IF   NOT COMMIT-MANUAL
                     PERFORM 043-COMMIT THRU 043-99-FIM
                END-IF
           END-IF.

       042-99-FIM. EXIT.

       043-COMMIT.

           SET CHECK-POINT-OFF TO TRUE
           EXEC SQL
                COMMIT
           END-EXEC.

       043-99-FIM. EXIT.

       044-ROLLBACK.

           SET CHECK-POINT-OFF TO TRUE
           EXEC SQL
                ROLLBACK
           END-EXEC.

       044-99-FIM. EXIT.

       050-CREATE.

           EXEC SQL
              CREATE TABLE FILENAME
                   ( CODIGO    NUMBER(5),
                     DESCRICAO VARCHAR2(30),
                     PRECO     DECIMAL(10,2),
                     TIPO      NUMBER(1),
                     IMPORTADO NUMBER(1),
                     GARANTIA  NUMBER(1),
                     DURAVEL   NUMBER(1),
                CONSTRAINTS PK_FILENAME
                 PRIMARY KEY ( CODIGO )
                      USING INDEX ) TABLESPACE TS_USER
           END-EXEC
           IF   SQLCODE = -955
                EXEC SQL
                    DROP TABLE FILENAME
                END-EXEC
                IF   SQLCODE = -54
                     MOVE "9A" TO CWSQLC-FS
                ELSE
                     GO TO 050-CREATE
                END-IF
           ELSE
                IF   SQLCODE NOT = ZERO
                     PERFORM 070-ERRO-SQL THRU 070-99-FIM
                END-IF
           END-IF

      *    EXEC SQL
      *         DROP INDEX FILENAME_DESCRICAO
      *    END-EXEC
      *    EXEC SQL
      *         DROP INDEX FILENAME_DESCRICAO_key
      *    END-EXEC
      *
           MOVE 1 TO WS-CREATE-INDEX.

       050-99-FIM. EXIT.

       051-OPEN-SEQUENTIAL.

           MOVE    "CHAVE"     TO POINTER-KEY
           INITIALIZE SQL-RECORD
                      WT-FILENAME-DESCRICAO
           SET     CWSQLC-START    TO TRUE
           SET     CWSQLC-NOT-LESS TO TRUE
           PERFORM 010-START     THRU 010-99-FIM
           MOVE    WS-CWSQLC       TO CWSQLC
           MOVE    "00"            TO CWSQLC-FS

           IF   SQLCODE NOT = ZERO
           AND (SQLCODE NOT = -1003)
           AND (SQLCODE NOT = 1403)
               MOVE "35" TO CWSQLC-FS
               IF   NOT CWSQLC-NOT-OPTIONAL
                    PERFORM 050-CREATE THRU 050-99-FIM
                    IF   SQLCODE = ZERO
                         MOVE "05" TO CWSQLC-FS
                    END-IF
               END-IF
           END-IF

           IF   SQLCODE = -904 OR -932
                MOVE "39" TO CWSQLC-FS
           END-IF.

           IF   CWSQLC-EXCLUSIVE
           AND  CWSQLC-FS < '10'
                EXEC SQL
                     LOCK TABLE FILENAME
                       IN EXCLUSIVE MODE
                END-EXEC
           END-IF.

       051-99-FIM. EXIT.

       060-SELECT.

           IF CHECK-POINT-ON
              EXEC SQL
                   ROLLBACK TO UNLOCK_FILENAME
              END-EXEC
              SET CHECK-POINT-OFF TO TRUE
           END-IF

           IF   CWSQLISAM = "ON"
           OR   CWSQLC-IGNORE-LOCK
           OR   CWSQLC-NO-LOCK
                EXEC SQL
                SELECT NVL(CODIGO,0),
                       NVL(DESCRICAO,' '),
                       NVL(PRECO,0),
                       NVL(TIPO,0),
                       NVL(IMPORTADO,0),
                       NVL(GARANTIA,0),
                       NVL(DURAVEL,0)
                       INTO :SQL-CODIGO,
                       :SQL-DESCRICAO,
                       :SQL-PRECO,
                       :SQL-TIPO,
                       :SQL-IMPORTADO,
                       :SQL-GARANTIA,
                       :SQL-DURAVEL
                       FROM FILENAME
                            WHERE ROWID = :SQL-CWID
                END-EXEC
           ELSE
                IF  CWSAVEPOINT = "ON"
                AND(AUTO-COMMIT
                OR (NOT CWSQLC-MULTIPLE-RECORDS))
                    EXEC SQL
                         SAVEPOINT UNLOCK_FILENAME
                    END-EXEC
                    SET CHECK-POINT-ON TO TRUE
                END-IF
                EXEC SQL
                SELECT NVL(CODIGO,0),
                       NVL(DESCRICAO,' '),
                       NVL(PRECO,0),
                       NVL(TIPO,0),
                       NVL(IMPORTADO,0),
                       NVL(GARANTIA,0),
                       NVL(DURAVEL,0)
                       INTO :SQL-CODIGO,
                       :SQL-DESCRICAO,
                       :SQL-PRECO,
                       :SQL-TIPO,
                       :SQL-IMPORTADO,
                       :SQL-GARANTIA,
                       :SQL-DURAVEL
                       FROM FILENAME
                            WHERE ROWID = :SQL-CWID
                              FOR UPDATE NOWAIT
                END-EXEC
           END-IF

           MOVE SQL-CWID TO SQL-LAST-ROWID

           IF   SQLCODE = -54
                MOVE 0    TO SQLCODE
                IF NOT CWSQLC-IGNORE-LOCK
                   MOVE "9D" TO CWSQLC-FS
                END-IF
           END-IF

           IF   SQLCODE = -1410 OR +1403
                EXIT PARAGRAPH
           END-IF

           IF   SQLCODE NOT = ZERO
                PERFORM 070-ERRO-SQL THRU 070-99-FIM
           END-IF.

       060-99-FIM. EXIT.

       065-CREATE-INDEX.

           EXEC SQL
                CREATE UNIQUE INDEX FILENAME_DESCRICAO
                    ON FILENAME
                    ( RPAD(DESCRICAO,30,' ')
                   || LPAD(ABS(CODIGO),5,0) ) TABLESPACE TS_INDEX
           END-EXEC
           IF  (SQLCODE NOT = 0)
           AND (SQLCODE NOT = -955)
               PERFORM 070-ERRO-SQL THRU 070-99-FIM
           END-IF

           EXEC SQL
                CREATE INDEX FILENAME_DESCRICAO_key
                    ON FILENAME
                    ( DESCRICAO ) TABLESPACE TS_INDEX
           END-EXEC
           IF  (SQLCODE NOT = 0)
           AND (SQLCODE NOT = -955)
               PERFORM 070-ERRO-SQL THRU 070-99-FIM
           END-IF.

       065-99-FIM. EXIT.

       070-ERRO-SQL.

           ON 1
              DISPLAY 'SQLMSG' UPON ENVIRONMENT-NAME
              ACCEPT SQLMSG FROM ENVIRONMENT-VALUE.

           DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
           DISPLAY  SQLERRMC  UPON ENVIRONMENT-VALUE

           IF   CWSQLISAM = "ON"
                EXIT PARAGRAPH
           END-IF

           IF   SQLMSG NOT = SPACES
                CALL SQLMSG USING SQLCA Z"FILENAME"
                 ON EXCEPTION
                    MOVE SPACES TO SQLMSG
                END-CALL
           END-IF

           IF   SQLMSG = SPACES
                MOVE SPACES TO MENSAGEM-LONGA
                STRING "(FILENAME)" DELIMITED BY SIZE
                       SQLERRMC DELIMITED BY X"0A"
                              INTO MENSAGEM-LONGA
                CALL "CWLONG" USING CWSQLC-FUNCTION MENSAGEM-LONGA
                     ON EXCEPTION
                        DISPLAY MENSAGEM-LONGA AT 2401
                        STOP "[ENTER]"
                END-CALL
           END-IF

           IF   NOT CWSQLC-NORMAL
           AND (SQLCODE NOT = -1452)
                MOVE "9d" TO CWSQLC-FS
                GOBACK
           END-IF.

       070-99-FIM. EXIT.

       END PROGRAM ORACLE-FILENAME.
