       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/11/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Trata parametros da CWBOXS com radio buttons *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 footline-t.
              10 footline             PIC  9(002) VALUE 23.
           05 STATIC                  PIC  X(001) VALUE X"70".
           05 ENTRY-ATTR              PIC  X(001) VALUE X"F0".
           05 DISABLE-ATTR            PIC  X(001) VALUE X"70".
           05 CURCOLOR                PIC  X(001) VALUE X"00".
           05 NUMERO              PIC 9(002) COMP-X.
           05 COR                 PIC 9(003) VALUE 0.
           05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.
           05 COMANDO.
              10 CMD              PIC X(001) VALUE SPACES.
              10 PROGRAMA         PIC X(008) VALUE SPACES.
              10 VIRGULA          PIC X(001) VALUE ",".
           05 TITULO              PIC X(078) VALUE SPACES.
           05 LL                  PIC 9(002) VALUE 0.
           05 I                   PIC 9(002) VALUE 0.
           05 Y                   PIC 9(002) VALUE 0.
           05 T                   PIC 9(002) VALUE 0.
           05 T1                  PIC 9(002) VALUE 0.
           05 T2                  PIC 9(002) VALUE 0.
           05 LC.
              10 L                PIC 9(002) VALUE 0.
              10 C                PIC 9(002) VALUE 0.
           05 TAMANHO-MATRIZ      PIC 9(008) COMP-X.
           05 ISX.
              10 IX OCCURS 21     PIC 9(002).

       01  TELA-TXBOXS.
           3 TELA OCCURS 21.
           05 RADIO-ATTR     PIC X(040).
           05 RADIO-PIC      PIC X(080).
           05 RADIO-DATANAME PIC X(030).
           05 RADIO          PIC X(001).
           05                PIC X(129).
           05 RADIO-SCREEN   PIC X(030).

           05 TEXTO-ATTR     PIC X(040).
           05 TEXTO-PIC      PIC X(080).
           05 TEXTO-DATANAME PIC X(030).
           05 TEXTO          PIC X(078).
           05                PIC X(052).
           05 TEXTO-SCREEN   PIC X(030).

       LINKAGE SECTION.

       COPY CWBOXS.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXS.

       000-INICIO.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           CALL "CWATTR" USING STATIC ENTRY-ATTR DISABLE-ATTR CURCOLOR
           EXEC COBOLware GetSystem
                PROGRAM;programa
           END-EXEC
           IF   CWBOXS-LINE = 0
           OR   CWBOXS-LINE NOT NUMERIC
                MOVE 1 TO CWBOXS-LINE
           END-IF

           IF   CWBOXS-COLUMN = 0
           OR   CWBOXS-COLUMN NOT NUMERIC
                MOVE 1 TO CWBOXS-COLUMN
           END-IF

           MOVE CWBOXS-LINE   TO L
           MOVE CWBOXS-COLUMN TO C
           ADD  1             TO L C

           MOVE 0 TO Y
           PERFORM VARYING T FROM 78 BY -1 UNTIL T = 1
                        OR CWBOXS-TITLE (T: 1) NOT = SPACE
                  CONTINUE
           END-PERFORM
           MOVE SPACES TO TELA-TXBOXS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                IF CWBOXS-TEXT(I) NOT = SPACES
                   INSPECT CWBOXS-CHAR (I)
                           CONVERTING MINUSCULAS
                                   TO MAIUSCULAS
                   ADD  1      TO Y
                   MOVE I      TO IX(Y)
                   MOVE 0      TO T2
                   MOVE "0"    TO RADIO (Y)
                   MOVE SPACES TO TEXTO (Y)
                   IF CWBOXS-OPTION = I
                      MOVE "1" TO RADIO (Y)
                   END-IF
                   MOVE "TELA-TXBOXS" TO RADIO-SCREEN (Y)
                                         TEXTO-SCREEN (Y)
                   MOVE "010101Ufbsbzebrahua01"
                     TO RADIO-ATTR (Y)
                   MOVE "0103..Ffbsbzebrahua.."
                     TO TEXTO-ATTR (Y)
                   MOVE LC  TO RADIO-ATTR (Y) (1: 4)
                   ADD  2   TO C
                   MOVE LC  TO TEXTO-ATTR (Y) (1: 4)
                   SUBTRACT 2 FROM C
                   MOVE "X" TO RADIO-PIC (Y)
                               TEXTO-PIC (Y)
                   MOVE "CWRADIO-XX" TO RADIO-DATANAME (Y)
                   MOVE "CWTEXTO-XX" TO TEXTO-DATANAME (Y)
                   MOVE LC           TO RADIO-DATANAME (Y) (9:4)
                                        TEXTO-DATANAME (Y) (13:2)
                   MOVE Y            TO RADIO-DATANAME (Y) (13:2)
                                        TEXTO-DATANAME (Y) (9:4)

                   PERFORM VARYING T1 FROM 1 BY 1 UNTIL T1 > 78
                           IF CWBOXS-TEXT(I) (T1: 1) NOT = "~"
                              ADD 1 TO T2
                              MOVE CWBOXS-TEXT(I) (T1: 1)
                                TO TEXTO(I) (T2: 1)
                              IF TEXTO(I) (T2: 1) = "_"
                                 MOVE SPACE TO TEXTO(I) (T2: 1)
                              END-IF
                              IF TEXTO(I) (T2: 1) NOT = SPACE
                              AND T2 > T
                                  MOVE T2 TO T
                              END-IF
                           ELSE
                              IF CWBOXS-CHAR (I) = SPACE
                                 ADD 1 TO T1
                                 MOVE CWBOXS-TEXT(I) (T1: 1)
                                   TO CWBOXS-CHAR (I)
                                 INSPECT CWBOXS-CHAR (I)
                                         CONVERTING MINUSCULAS
                                                 TO MAIUSCULAS
                                 SUBTRACT 1 FROM T1
                              END-IF
                           END-IF
                   END-PERFORM
                   MOVE T TO TEXTO-ATTR (Y) (5: 2)
                             TEXTO-ATTR (Y) (20: 2)
                END-IF
                ADD 1 TO L
           END-PERFORM
           COMPUTE TAMANHO-MATRIZ = 620 * Y
           add  1             to T
           EXEC COBOLware Object GROUP
                     LINE CWBOXS-LINE COLUMN CWBOXS-COLUMN
                          WIDTH T HEIGHT Y
                     CAPTION CWBOXS-TITLE
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE FOOTLINE COLUMN 3 WIDTH 9
                     CAPTION " ~OK "
                     KEY F2
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE FOOTLINE COLUMN 13 WIDTH 9
                     CAPTION " ~Cancelar "
                     KEY ESC
           END-EXEC

           MOVE "D" TO CMD
           CALL "CWUSER" USING COMANDO TELA-TXBOXS TAMANHO-MATRIZ
           MOVE "a" TO CMD
           CALL "CWUSER" USING COMANDO TELA-TXBOXS TAMANHO-MATRIZ
           ACCEPT TECLA FROM ESCAPE KEY
           MOVE 0     TO CWBOXS-OPTION
           MOVE SPACE TO CWBOXS-OPTION-CHAR
           IF NOT ESC
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > Y
                   IF RADIO(I) = "1"
                      MOVE IX(I) TO CWBOXS-OPTION
                      MOVE CWBOXS-CHAR (CWBOXS-OPTION)
                        TO CWBOXS-OPTION-CHAR
                   END-IF
              END-PERFORM
           END-IF
           exec cobolware object drop end-exec
           move static to numero(1:1)
           move numero to cor
           add 1 to t
           exec cobolware boxwindow Open
                LINE CWBOXS-LINE COLUMN CWBOXS-COLUMN
                     WIDTH T HEIGHT Y
                     COLOR-BORDER cor
           end-exec
           exec cobolware boxwindow PopUP
           end-exec

           MOVE CWBOXS-TITLE TO TITULO
           Inspect Titulo converting "_:" TO "  "
           PERFORM VARYING T FROM 78 BY -1 UNTIL T = 1
                        OR TITULO (T: 1) NOT = SPACE
                  CONTINUE
           END-PERFORM
           compute LL = CWBOXS-LINE + 0
           DISPLAY TITULO LINE LL
                          COLUMN CWBOXS-COLUMN WITH SIZE T
           MOVE "D" TO CMD
           CALL "CWUSER" USING COMANDO TELA-TXBOXS TAMANHO-MATRIZ.

       000-99-FIM. GOBACK.

       END PROGRAM CWBOXR.
