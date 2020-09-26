       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEOL INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/06/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Converte arquivos texto entre Unix <> DOS    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ENTRADA ASSIGN TO DISK
                  ORGANIZATION   IS BINARY SEQUENTIAL
                  LOCK MODE      IS EXCLUSIVE
                  FILE STATUS    IS CWEOL-STATUS.

           SELECT SAIDA ASSIGN TO DISK
                  ORGANIZATION IS BINARY SEQUENTIAL
                  LOCK MODE    IS EXCLUSIVE
                  FILE STATUS  IS CWEOL-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA
           VALUE OF FILE-ID LB-ENTRADA.

       01  ENTRADA-REG PIC X.

       FD  SAIDA
           VALUE OF FILE-ID LB-SAIDA.

       01  SAIDA-REG PIC X.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.
           05 ER-ENTRADA.
              10 FS-ENTRADA   PIC X(002) VALUE "00".
              10 LB-ENTRADA   PIC X(255) VALUE SPACES.
           05 ER-SAIDA.
              10 FS-SAIDA     PIC X(002) VALUE "00".
              10 LB-SAIDA     PIC X(255) VALUE SPACES.
           05 ERRO            PIC 9(001) VALUE 0.
           05 D        COMP-5 PIC S9(04) VALUE 0.
           05 I        COMP-5 PIC S9(04) VALUE 0.
           05 MODO            PIC X(001) VALUE SPACE.
           05 FIM             PIC X(001) VALUE SPACE.
           05 MSG             PIC X(079) VALUE SPACE.

       LINKAGE SECTION.

       COPY CWEOL.

       PROCEDURE DIVISION USING PARAMETROS-CWEOL.

       000-INICIO.

          EVALUATE TRUE
              WHEN CWEOL-DOS
                   MOVE '1' TO MODO
              WHEN CWEOL-UNIX
                   MOVE '2' TO MODO
              WHEN OTHER
                   MOVE 1 TO RETURN-CODE
                   GOBACK
           END-EVALUATE

           MOVE CWEOL-FILE   TO LB-ENTRADA

           PERFORM TEST AFTER UNTIL FS-ENTRADA NOT = '9A'
                   OPEN INPUT ENTRADA
           END-PERFORM

           IF CWEOL-STATUS > '09'
              MOVE 2 TO RETURN-CODE
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH LB-ENTRADA
                    OR LB-ENTRADA (I: 1) = '.' OR SPACE
                   MOVE LB-ENTRADA (I: 1) TO LB-SAIDA (I: 1)
           END-PERFORM
           MOVE '.EOL$' TO LB-SAIDA (I: )
           OPEN OUTPUT SAIDA
           PERFORM UNTIL CWEOL-STATUS > '09'
                 READ ENTRADA
                  NOT AT END
                      MOVE 'N' TO FIM
                      EVALUATE TRUE
                          WHEN ENTRADA-REG = X'0D'
                               ADD 1 TO D
                          WHEN ENTRADA-REG = X'0A'
                               IF MODO = '1'
                                  WRITE SAIDA-REG FROM X'0D'
                               END-IF
                               WRITE SAIDA-REG FROM X'0A'
                               MOVE 'S' TO FIM
                               MOVE 0 TO D
                          WHEN OTHER
                               PERFORM D TIMES
                                    IF MODO = '1'
                                       WRITE SAIDA-REG FROM X'0D'
                                    END-IF
                                    WRITE SAIDA-REG FROM X'0A'
                                    MOVE 'S' TO FIM
                               END-PERFORM
                               MOVE 0 TO D
                               WRITE SAIDA-REG FROM ENTRADA-REG
                      END-EVALUATE
           END-PERFORM
           PERFORM D TIMES
                IF MODO = '1'
                   WRITE SAIDA-REG FROM X'0D'
                END-IF
                WRITE SAIDA-REG FROM X'0A'
                MOVE 'S' TO FIM
           END-PERFORM
           IF FIM = 'N'
              IF MODO = '1'
                 WRITE SAIDA-REG FROM X'0D'
              END-IF
              WRITE SAIDA-REG FROM X'0A'
              MOVE 'S' TO FIM
           END-IF
           CLOSE ENTRADA SAIDA
           DELETE FILE ENTRADA
           CALL "CBL_RENAME_FILE" USING LB-SAIDA LB-ENTRADA.

       000-99-FIM. GOBACK.
       END PROGRAM CWEOL.
