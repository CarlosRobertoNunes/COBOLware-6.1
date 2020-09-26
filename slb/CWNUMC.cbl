       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWNUMC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/02/2013.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relat¢rio de critica de carregadores         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BADS  ASSIGN TO DISK
                  ORGANIZATION IS BINARY SEQUENTIAL
                  LOCK MODE    IS EXCLUSIVE
                  FILE STATUS  IS FS-BADS.

       DATA DIVISION.
       FILE SECTION.

       FD  BADS
           VALUE OF FILE-ID LB-BADS.

       01  BADS-REG PIC X.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LEN           COMP-X PIC   9(008) VALUE 0.
           05 I             COMP-X PIC   9(008) VALUE 0.
           05 H             COMP-X PIC   9(008) VALUE 0.
           05 ERRO                 PIC   9(004) VALUE 0.
           05 BAD                  PIC   9(012) VALUE 0.
           05 WS-P1.
              10 LB-POINTER        POINTER.
           05 LD-FILE              PIC   9(012) VALUE 0.
           05 LD-SAVE              PIC   9(012) VALUE 0.
           05 WS-LB                PIC   X(050) VALUE SPACES.
           05 BF-RECORD            PIC X(32768) VALUE SPACES.
           05 LE-RECORD     COMP-X PIC   9(008) VALUE 0.
           05 NM-FIELD             PIC   X(030) VALUE SPACES.
           05 LE-FIELD      COMP-X PIC   9(008) VALUE 0.
           05 ER-BADS.
              10 FS-BADS           PIC  X(002) VALUE "00".
              10 LB-BADS           PIC  X(255) VALUE SPACES.
           05 BUFFER               PIC X(32768) VALUE SPACES.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.
           05 CHAR          COMP-X PIC  9(002) VALUE 0.
           05 HEX-FLAG             PIC  9(001) VALUE 0.
           05 PWD                  PIC  X(005) VALUE SPACES.
           05 DOS                  PIC  X(005) VALUE SPACES.
           05 SP2                  PIC  X(005) VALUE SPACES.

       01  HEXTAB.
           10 PIC X(32) VALUE "000102030405060708090A0B0C0D0E0F".
           10 PIC X(32) VALUE "101112131415161718191A1B1C1D1E1F".
           10 PIC X(32) VALUE "202122232425262728292A2B2C2D2E2F".
           10 PIC X(32) VALUE "303132333435363738393A3B3C3D3E3F".
           10 PIC X(32) VALUE "404142434445464748494A4B4C4D4E4F".
           10 PIC X(32) VALUE "505152535455565758595A5B5C5D5E5F".
           10 PIC X(32) VALUE "606162636465666768696A6B6C6D6E6F".
           10 PIC X(32) VALUE "707172737475767778797A7B7C7D7E7F".
           10 PIC X(32) VALUE "808182838485868788898A8B8C8D8E8F".
           10 PIC X(32) VALUE "909192939495969798999A9B9C9D9E9F".
           10 PIC X(32) VALUE "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF".
           10 PIC X(32) VALUE "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF".
           10 PIC X(32) VALUE "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF".
           10 PIC X(32) VALUE "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF".
           10 PIC X(32) VALUE "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF".
           10 PIC X(32) VALUE "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
       01  REDEFINES HEXTAB.
           10 HEX OCCURS 256 PIC XX.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(010) VALUE
              "Registro: ".
           05 clic-lidos                     PIC  ZZZ.ZZZ.ZZZ.ZZ9.
       02  LINHA-02.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE
              "Imagem:".
       02  LINHA-03.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE
              "Campo: ".
           05  clic-dataname                 PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(012) VALUE
              " Ocorrencia ".
           05  clic-occurs                   PIC  9(004) VALUE ZEROS.
       02  LINHA-04.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(009) VALUE
              "Conteudo:".

       01  OS-INFORMATION.
           05 OS-PARAMETER-SIZE     PIC XX COMP-X VALUE 13.
           05 OS-TYPE               PIC 9(2) COMP-X.
           05 OS-VERSION            PIC XXXX COMP-X.
           05 OS-DBCS-SUPPORT       PIC X COMP-X.
           05 OS-CHAR-CODING        PIC 9(2) COMP-X.
           05 OS-COUNTRY-CODE       PIC XX COMP-X.
           05 OS-CODE-PAGE          PIC XX COMP-X.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  TIPO PIC X.
       01  P1 PIC X.
       01  P2 PIC X.
       01  P3 PIC X.
       01  P4 PIC 9(4).
       01  LB-FILE PIC X(50).

       PROCEDURE DIVISION USING TIPO P1 P2 P3 P4.

       000-INICIO.

           ON 1 CALL "CBL_GET_OS_INFO" USING OS-INFORMATION
                DISPLAY "PWD" UPON ENVIRONMENT-NAME
                ACCEPT  PWD   FROM ENVIRONMENT-VALUE
                IF PWD(1:1) = '/'
                   SET CWUNIX-ON  TO TRUE
                ELSE
                   IF   OS-TYPE < 3
                   OR  (OS-TYPE = 5 OR 131)
                        CONTINUE
                   ELSE
                        SET CWUNIX-ON  TO TRUE
                   END-IF
                END-IF.

           IF TIPO = 'R'
              MOVE P1(1:4) TO WS-P1
              SET ADDRESS LB-FILE TO LB-POINTER
              IF LB-FILE(255:1) = '*'
                 MOVE SPACE TO LB-FILE(255:1)
              END-IF
              MOVE P2(1:12) TO LD-FILE
              MOVE P4(1:4)  TO LE-RECORD(1:4)
              MOVE P3(1:LE-RECORD) TO BF-RECORD(1:LE-RECORD)
              MOVE 0        TO ERRO
           ELSE
              MOVE P2(1:4)  TO LE-FIELD(1:4)
              IF P1 (1: LE-FIELD) NOT NUMERIC
                 SET ADDRESS LB-FILE TO LB-POINTER
                 ADD 1       TO BAD
                 MOVE '*'    TO LB-FILE (255:1)
                 MOVE SPACES TO WS-LB
                 STRING LB-FILE DELIMITED BY '.'
                        INTO WS-LB
                 MOVE SPACES TO LB-BADS
                 STRING WS-LB   DELIMITED BY SPACE
                        ".lst" DELIMITED BY SIZE
                        INTO LB-BADS
                 PERFORM TEST AFTER UNTIL FS-BADS NOT = '9A'
                      IF BAD = 1
                         OPEN OUTPUT BADS
                      ELSE
                         OPEN EXTEND BADS
                      END-IF
                 END-PERFORM
                 IF ERRO = 0
                    COMPUTE LEN = LE-RECORD + LENGTH LINHA-02 - 1
                    MOVE ALL '-' TO BUFFER
                    PERFORM GRAVAR THRU FIM-GRAVAR
                    MOVE 1               TO ERRO
                    IF LD-FILE NOT = LD-SAVE
                       MOVE LD-FILE      TO clic-lidos
                                            LD-SAVE
                       MOVE LINHA-01     TO BUFFER
                       MOVE LENGTH LINHA-01 TO LEN
                       PERFORM GRAVAR THRU FIM-GRAVAR
                    END-IF
                    STRING LINHA-02 " "
                       BF-RECORD(1:LE-RECORD)  DELIMITED BY SIZE
                       INTO BUFFER
                    COMPUTE LEN = LE-RECORD + LENGTH LINHA-02 - 1
                    PERFORM GRAVAR THRU FIM-GRAVAR
                 END-IF
                 MOVE SPACES TO CLIC-DATANAME
                 PERFORM VARYING I FROM 1 BY 1 UNTIL P3(I:1) = X'00'
                         MOVE P3 (I:1) TO CLIC-DATANAME(I:1)
                 END-PERFORM
                 CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                 IF   X91-PARAMETER > 4
                      MOVE P4 TO CLIC-OCCURS
                      MOVE LENGTH LINHA-03 TO LEN
                 ELSE
                      MOVE 40 TO LEN
                 END-IF
                 MOVE LINHA-03 TO BUFFER
                 PERFORM GRAVAR THRU FIM-GRAVAR
                 STRING LINHA-04 ' "'
                        P1 (1: LE-FIELD)
                        '"' DELIMITED BY SIZE INTO BUFFER
                 COMPUTE LEN = LE-FIELD + 3 + LENGTH LINHA-04
                 MOVE 1 TO HEX-FLAG
                 PERFORM GRAVAR THRU FIM-GRAVAR
                 MOVE 0 TO HEX-FLAG
                 CLOSE BADS
              END-IF
           END-IF.

       000-99-FIM. GOBACK.

       GRAVAR.

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
                    IF BUFFER(I:1) < X'20' OR BUFFER(I:1) = X'FF'
                       WRITE BADS-REG FROM '?'
                    ELSE
                       WRITE BADS-REG FROM BUFFER(I:1)
                    END-IF
            END-PERFORM
            IF HEX-FLAG = 1
               WRITE BADS-REG FROM ','
               WRITE BADS-REG FROM 'X'
               WRITE BADS-REG FROM '"'
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > LE-FIELD
                       MOVE P1 (I:1) TO CHAR(1:1)
                       COMPUTE H = CHAR + 1
                       WRITE BADS-REG FROM HEX(H)(1:1)
                       WRITE BADS-REG FROM HEX(H)(2:1)
                       IF P1 (I:1) < X'20'
                          MOVE '?' TO P1 (I:1)
                       END-IF
               END-PERFORM
               WRITE BADS-REG FROM '"'
            END-IF
            MOVE SPACES TO BUFFER
            IF   CWUNIX-OFF
                 WRITE BADS-REG FROM X'0D'
            END-IF
            WRITE BADS-REG FROM X'0A'.

       FIM-GRAVAR. EXIT.

       END PROGRAM CWNUMC.
