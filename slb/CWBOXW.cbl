       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/02/1992.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre/fecha janela free format                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TEXTO ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  LOCK MODE    IS EXCLUSIVE.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT WINDOW ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD  KEY   IS WINDOW-CHAVE
                  ACCESS MODE   IS RANDOM
                  FILE STATUS   IS FS-WINDOW.

       DATA DIVISION.
       FILE SECTION.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG        PIC X(017).

       FD  WINDOW
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINDOW.

       01  WINDOW-REG.
           05 WINDOW-CHAVE             PIC  9(0003).
           05 WINDOW-DATA              PIC  X(2000).
           05 WINDOW-ATTRIB            PIC  X(2000).
           05 WINDOW-AJUSTE            PIC  X(0001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
frango     05 JANPOS.
frango        10 XANLIN               PIC  9(002) VALUE 0.
frango        10 XANCOL               PIC  9(002) VALUE 0.
           05 CWATTW                  PIC  X(008) VALUE 'CWATTW'.
           05 WINWORK-CHAVE           PIC  X(002) VALUE SPACES.
           05 box-LINE                PIC  9(002) VALUE 0.
           05 box-COLUMN              PIC  9(002) VALUE 0.
           05 GUICOLOR                PIC  X(003) VALUE SPACES.
           05 CWLOGT                  PIC  X(002) VALUE SPACES.
           05 ERRO                    PIC  9(001) VALUE 0.
           05 LIST                    PIC  9(001) VALUE 0.
           05 LIXO                    PIC  X(001) VALUE SPACE.
           05 OPERADOR                PIC  X(030) VALUE SPACES.
           05 TASK                    PIC  9(006) VALUE 0.
           05 PROGRAMA                PIC  X(008) VALUE SPACES.
           05 SET-LOG                 PIC  X(001) VALUE "?".
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 MAXFRAME           COMP PIC  9(003) VALUE 0.
           05 ER-TEXTO.
              10 FS-TEXTO             PIC  X(002) VALUE "00".
              10 LB-TEXTO             PIC  X(255) VALUE "CWUSER.LOG".
           05 RK-WINDOW          COMP PIC  9(003) VALUE 0.
           05 ER-WINDOW.
              10 FS-WINDOW            PIC  X(002) VALUE "00".
              10 LB-WINDOW            PIC  X(255) VALUE 'CWBOXW$$$'.
           05 L                       PIC  9(004) COMP-X VALUE 0.
           05 C                       PIC  9(004) COMP-X VALUE 0.
           05 CC                      PIC  9(003) VALUE 0.
           05 ATT-T                   PIC  X(001) VALUE X"07".
           05 ATT-B                   PIC  X(001) VALUE X"07".
           05 SOMBRA                  PIC  X(001) VALUE X"08".
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER.
                 15         OCCURS 25.
                   20 BYTE  OCCURS 80 PIC X(001).
              10 ATTRIBUTE-BUFFER.
                 15 AAA         OCCURS 25.
                    20 ATRIBUTO OCCURS 80 PIC X(001).
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
           05 TABELA-CORES.
              10 COR PIC X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 MOLDURA                              VALUE SPACES.
              10 M-201                 PIC  X(001).
              10 M-205                 PIC  X(001).
              10 M-187                 PIC  X(001).
              10 M-186                 PIC  X(001).
              10 M-204                 PIC  X(001).
              10 M-185                 PIC  X(001).
              10 M-200                 PIC  X(001).
              10 M-188                 PIC  X(001).
           05 MOLDURA-DEFAULT          PIC  9(001) VALUE 0.
           05 JANELA.
               10 JANLIN                PIC  9(2)  VALUE 0.
               10 JANCOL                PIC  9(2)  VALUE 0.
               10 JANWID                PIC  9(2)  VALUE 80.
               10 JANHEI                PIC  9(2)  VALUE 25.
               10 JANMAX                PIC  9(2)  VALUE 80.
               10 JANLIMAX              PIC  9(2)  VALUE 25.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXW.
           05 CWBOXW-LINE                PIC  9(002).
           05 CWBOXW-COLUMN              PIC  9(002).
           05 CWBOXW-TYPE                PIC  9(001).
           05 CWBOXW-VERTICAL-LENGTH     PIC  9(002).
           05 CWBOXW-HORIZONTAL-LENGTH   PIC  9(002).
           05 CWBOXW-COLOR-FRAME         PIC  9(002) COMP-X.
           05 CWBOXW-COLOR-BORDER        PIC  9(002) COMP-X.
           05 CWBOXW-COLOR-SHADE         PIC  9(002) COMP-X.
           05 CWBOXW-FUNCTION            PIC  X(005).
              88 NOJAN     VALUE "OPEN".
              88 ABRE      VALUE "OPEN" "BOX" "ERASE" 'NOWIN'.
              88 BOX       VALUE "BOX" "ERASE" 'NOWIN'.
              88 NOERASE   VALUE "BOX" 'NOWIN'.
              88 NOWIN     VALUE 'NOWIN'.
              88 FECHA     VALUE "CLOSE" "POPUP".
              88 NAO-APAGA VALUE "POPUP".

       PROCEDURE DIVISION USING PARAMETROS-CWBOXW.

       INICIO.

           DISPLAY 'CWATTW' UPON ENVIRONMENT-NAME
           ACCEPT   CWATTW  FROM ENVIRONMENT-VALUE
           DISPLAY 'WINWORK'      UPON ENVIRONMENT-NAME
           ACCEPT   WINWORK-CHAVE FROM ENVIRONMENT-VALUE
      *    IF   CWBOXW-TYPE(1:1) = 'I'
                MOVE SPACES TO MOLDURA
      *    END-IF
           IF   CWBOXW-COLOR-SHADE = 255
                MOVE 0 TO CWBOXW-COLOR-SHADE
                MOVE 1 TO LIST
           ELSE
                MOVE 0 TO LIST
           END-IF

           ON 1
              DISPLAY "CWGUICOLOR" UPON ENVIRONMENT-NAME
              ACCEPT GUICOLOR FROM ENVIRONMENT-VALUE
              INSPECT GUICOLOR CONVERTING MINUSCULAS TO MAIUSCULAS
              MOVE "?"                 TO SET-LOG
              CALL "CWGETU"         USING OPERADOR TASK PROGRAMA SET-LOG
              MOVE TASK TO LB-TEXTO (8: )
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
              MOVE "m"                 TO SET-LOG
              CALL "CWGETU"         USING OPERADOR TASK PROGRAMA SET-LOG
              MOVE TASK                TO MOLDURA-DEFAULT
              CALL "CWGETU"         USING OPERADOR TASK PROGRAMA "?"
              DISPLAY "CWLOGT"     UPON ENVIRONMENT-NAME
              ACCEPT  CWLOGT       FROM ENVIRONMENT-VALUE
              INSPECT CWLOGT CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF   PARAMETROS-CWBOXW (1:1) = 'W' OR 'w'
                MOVE PARAMETROS-CWBOXW(2:8) TO JANELA (1:8)
                SUBTRACT 1  FROM JANLIN
                                 JANCOL
      *         COMPUTE JANMAX   = JANCOL + JANWID - 1
      *         COMPUTE JANLIMAX = JANLIN + JANHEI - 1
                GOBACK
           END-IF

           IF  CWLOGT = 'ON'
               PERFORM TEST AFTER UNTIL FS-TEXTO = '00'
                       OPEN EXTEND TEXTO
               END-PERFORM
               WRITE TEXTO-REG FROM PARAMETROS-CWBOXW
               CLOSE TEXTO
           END-IF

           IF   CWBOXW-TYPE = 0
                MOVE MOLDURA-DEFAULT TO CWBOXW-TYPE
           END-IF

           INSPECT CWBOXW-FUNCTION CONVERTING MINUSCULAS TO MAIUSCULAS
           COMPUTE JANMAX   = JANCOL + JANWID - 1
           COMPUTE JANLIMAX = JANLIN + JANHEI - 1

           IF   ABRE
                MOVE CWBOXW-LINE   TO box-LINE
                MOVE CWBOXW-COLUMN TO box-COLUMN
frango          IF  NOJAN
frango              DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango              ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
                    if janpos = '0000'
                       move 80 to JANMAX
                       move 25 to JANLIMAX
                    else
                       if xanlin > 0
                          subtract 1 from xanlin
                       end-if
                       if xancol > 0
                          subtract 1 from xancol
                       end-if
                    end-if
frango              IF JANPOS NUMERIC
frango                 add XANLIN to BOX-LINE
frango                 add XANCOL to BOX-COLUMN
frango              END-IF
                ELSE
                    IF BOX
                       SUBTRACT 1 FROM CWBOXW-VERTICAL-LENGTH
                                       CWBOXW-HORIZONTAL-LENGTH
                    ELSE
                       ADD JANLIN TO box-LINE
                       ADD JANCOL TO box-COLUMN
                    END-IF
                END-IF
                IF  (box-LINE   + CWBOXW-VERTICAL-LENGTH)   > JANLIMAX
                OR  (box-COLUMN + CWBOXW-HORIZONTAL-LENGTH) > JANMAX
                    CALL "CWMSGW" USING "230333"
                         "Erro de limites no uso da CWBOXW "
                    PERFORM PAUSA
                    CALL "CWATCH"
                    EXIT PROGRAM
                END-IF
                IF BOX
                   ADD 1 TO CWBOXW-VERTICAL-LENGTH
                            CWBOXW-HORIZONTAL-LENGTH
                END-IF
           END-IF

           IF   FECHA
                IF   RK-WINDOW = 0
                     CALL "CWMSGW" USING "230333"
                          "Erro de l¢gica no uso da CWBOXW  "
                     PERFORM PAUSA
                ELSE
                     PERFORM CLOSE-WINDOW
                END-IF
           ELSE
                IF   ABRE
                     PERFORM OPEN-WINDOW
                ELSE
                     CALL "CWMSGW" USING "230333"
                          "Erro de sintaxe no uso CWBOXW    "
                     PERFORM PAUSA
                END-IF
           END-IF

           IF NOT BOX
              CALL "CWATCH"
           END-IF
           EXIT PROGRAM.

       PAUSA.

          ACCEPT LIXO AT 2302.

       OPEN-WINDOW.

300715*    IF  CWUNIX-GUI
300715*    AND(NOT BOX)
300715*        CALL "CWSCRE" USING ">"
300715*    END-IF
300715*
           IF  CWUNIX-GUI
           AND GUICOLOR = "OFF"
               MOVE X'00' TO ATT-T
                             ATT-B
                             SOMBRA
               IF  (CWBOXW-COLOR-SHADE NOT = CWBOXW-COLOR-FRAME)
               AND (CWBOXW-COLOR-SHADE NOT = CWBOXW-COLOR-BORDER)
                    MOVE X"08" TO SOMBRA
               END-IF
           ELSE
               COMPUTE CC = CWBOXW-COLOR-FRAME + 1
               MOVE COR (CC) TO ATT-T
               COMPUTE CC = CWBOXW-COLOR-BORDER + 1
               MOVE COR (CC) TO ATT-B
               COMPUTE CC = CWBOXW-COLOR-SHADE + 1
               MOVE COR (CC) TO SOMBRA
           END-IF

           IF   CWBOXW-TYPE(1:1) NOT = 'I'
                MOVE BASE-MOLDURA (CWBOXW-TYPE + 1) TO MOLDURA
           END-IF

           IF   box-LINE = 0
           OR   box-LINE NOT NUMERIC
                MOVE 1 TO box-LINE
           END-IF

           IF   box-COLUMN = 0
           OR   box-COLUMN NOT NUMERIC
                MOVE 1 TO box-COLUMN
           END-IF

           IF   CWBOXW-VERTICAL-LENGTH = 0
           OR   CWBOXW-VERTICAL-LENGTH NOT NUMERIC
                MOVE 1 TO CWBOXW-VERTICAL-LENGTH
           END-IF

           IF   CWBOXW-HORIZONTAL-LENGTH = 0
           OR   CWBOXW-HORIZONTAL-LENGTH NOT NUMERIC
                MOVE 1 TO CWBOXW-HORIZONTAL-LENGTH
           END-IF

           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH

      *    IF   ERRO = 0
      *         CALL "CWSCRE" USING "+"
      *         ON EXCEPTION
      *             MOVE 1 TO ERRO
      *         END-CALL
      *    END-IF
           IF NOT BOX
              ADD  1                TO RK-WINDOW
              MOVE RK-WINDOW        TO WINDOW-CHAVE
              MOVE CARACTER-BUFFER  TO WINDOW-DATA
              MOVE ATTRIBUTE-BUFFER TO WINDOW-ATTRIB
           END-IF

           IF  (CWBOXW-TYPE(1:1) NOT = 'I')
MD    *    IF  (CWBOXW-COLOR-SHADE NOT = 0)
           OR   LIST = 1
                IF   LIST = 1
                     ADD      1   TO box-COLUMN
                     SUBTRACT 1 FROM CWBOXW-HORIZONTAL-LENGTH
                END-IF
                MOVE M-201  TO BYTE     (box-LINE box-COLUMN)
                MOVE ATT-B  TO ATRIBUTO (box-LINE box-COLUMN)
                MOVE box-COLUMN TO C
                PERFORM CWBOXW-HORIZONTAL-LENGTH TIMES
                        ADD 1      TO C
                        MOVE M-205 TO BYTE     (box-LINE C)
                        MOVE ATT-B TO ATRIBUTO (box-LINE C)
                END-PERFORM
                ADD  1           TO C
                MOVE M-187       TO BYTE     (box-LINE C)
                MOVE ATT-B       TO ATRIBUTO (box-LINE C)
                MOVE box-LINE TO L
           ELSE
                COMPUTE L = box-LINE - 1
                ADD      1   TO box-COLUMN
                SUBTRACT 1 FROM CWBOXW-HORIZONTAL-LENGTH
           END-IF

           PERFORM CWBOXW-VERTICAL-LENGTH TIMES
                   MOVE box-COLUMN TO C
                   ADD  1             TO L
                   IF L > 25 EXIT PERFORM END-IF
                   MOVE M-186         TO BYTE     (L C)
                   MOVE ATT-B         TO ATRIBUTO (L C)
                   PERFORM CWBOXW-HORIZONTAL-LENGTH TIMES
                           ADD  1     TO C
                           IF NOT NOERASE
                              MOVE SPACE TO BYTE     (L C)
                           END-IF
                           IF NOT NOWIN
                              MOVE ATT-T TO ATRIBUTO (L C)
                           END-IF
                   END-PERFORM
                   ADD 1       TO C
                   IF C > 80 EXIT PERFORM END-IF
                   MOVE M-186  TO BYTE     (L C)
                   MOVE ATT-B  TO ATRIBUTO (L C)
                   IF   C < 79
                   AND (CWBOXW-COLOR-SHADE NOT = 0)
                        MOVE SOMBRA TO ATRIBUTO (L C + 1)
                                       ATRIBUTO (L C + 2)
                   END-IF
           END-PERFORM

           IF   CWBOXW-COLOR-SHADE = 0
                SUBTRACT 1 FROM box-COLUMN
                ADD 1 TO CWBOXW-HORIZONTAL-LENGTH
           END-IF

           MOVE box-COLUMN TO C
           ADD  1          TO L
           IF   L < 26
                IF   CWBOXW-COLOR-SHADE NOT = 0
                     MOVE M-200         TO BYTE     (L C)
                     MOVE ATT-B         TO ATRIBUTO (L C)
                ELSE
                     ADD  1 TO C
                     MOVE M-200         TO BYTE     (L C)
                     MOVE ATT-B         TO ATRIBUTO (L C)
                     SUBTRACT 1 FROM CWBOXW-HORIZONTAL-LENGTH
                END-IF

                PERFORM CWBOXW-HORIZONTAL-LENGTH TIMES
                        ADD 1       TO C
                        MOVE M-205  TO BYTE     (L C)
                        MOVE ATT-B  TO ATRIBUTO (L C)
                        IF   (L + 1) < 25
                        AND  C < 81
                        AND (CWBOXW-COLOR-SHADE NOT = 0)
                             MOVE SOMBRA TO ATRIBUTO (L + 1 C)
                        END-IF
                END-PERFORM

                IF   CWBOXW-COLOR-SHADE = 0
                     ADD 1 TO CWBOXW-HORIZONTAL-LENGTH
                END-IF
                ADD 1       TO C
                MOVE M-188  TO BYTE     (L C)
                MOVE ATT-B  TO ATRIBUTO (L C)

                IF   L < 25
                AND (CWBOXW-COLOR-SHADE NOT = 0)
                     MOVE SOMBRA TO ATRIBUTO (L + 1 C)
                END-IF

                ADD  1      TO C
                IF   L < 25
                AND  C < 80
                AND (CWBOXW-COLOR-SHADE NOT = 0)
                     MOVE SOMBRA TO ATRIBUTO (L C)
                                    ATRIBUTO (L C + 1)
                                    ATRIBUTO (L + 1 C)
                                    ATRIBUTO (L + 1 C + 1)
                END-IF
           END-IF


           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH
           CALL CWATTW USING "S" ATTRIBUTE-BUFFER WINWORK-CHAVE

           IF   (NOT CWUNIX-GUI)
           AND  box-LINE = 5
           AND (BYTE (5 41) = M-205)
           AND (M-205 NOT = SPACES)
               DISPLAY "CWIAEF" UPON ENVIRONMENT-NAME
               DISPLAY M-205    UPON ENVIRONMENT-VALUE
               MOVE M-205 TO WINDOW-AJUSTE
           ELSE
               MOVE SPACE TO WINDOW-AJUSTE
           END-IF

           IF NOT BOX
              IF   RK-WINDOW = 1
                   OPEN I-O WINDOW
              END-IF
              IF   RK-WINDOW > MAXFRAME
                   MOVE RK-WINDOW TO MAXFRAME
                   WRITE WINDOW-REG
              ELSE
                   REWRITE WINDOW-REG
              END-IF
           END-IF.

       CLOSE-WINDOW.

      *    IF   ERRO = 0
      *         CALL "CWSCRE" USING "-"
      *         ON EXCEPTION
      *             MOVE 1 TO ERRO
      *         END-CALL
      *    END-IF

           IF   NAO-APAGA
                CONTINUE
Jenne           MOVE LOW-VALUES TO ATTRIBUTE-BUFFER
Jenne           CALL CWATTW USING "S" ATTRIBUTE-BUFFER WINWORK-CHAVE
           ELSE
      *         OPEN INPUT WINDOW
                READ WINDOW
                MOVE WINDOW-DATA                TO CARACTER-BUFFER
                MOVE WINDOW-ATTRIB              TO ATTRIBUTE-BUFFER
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
                CALL CWATTW USING "S" ATTRIBUTE-BUFFER WINWORK-CHAVE
           END-IF

           SUBTRACT 1 FROM RK-WINDOW
           MOVE RK-WINDOW TO WINDOW-CHAVE

           IF   RK-WINDOW = 0
                MOVE 0 TO MAXFRAME
                MOVE SPACES TO WINDOW-AJUSTE
                DELETE FILE WINDOW
                CLOSE WINDOW
                MOVE LOW-VALUES TO ATTRIBUTE-BUFFER
                CALL CWATTW USING "S" ATTRIBUTE-BUFFER WINWORK-CHAVE
           ELSE
                READ  WINDOW
           END-IF

           DISPLAY "CWIAEF"      UPON ENVIRONMENT-NAME
           DISPLAY WINDOW-AJUSTE UPON ENVIRONMENT-VALUE.

300715*    IF  CWUNIX-GUI
300715*        CALL "CWSCRE" USING "<"
300715*    END-IF.

       END PROGRAM CWBOXW.
