       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPERC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/02/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exibe barra de progresso percentual          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 PROGRESSO            PIC  X(050) VALUE SPACES.
           05 I                    PIC  9(003) VALUE 0.
           05 PERC                 PIC  9(003) VALUE 0.
           05 COR-FULL             PIC  X(001) VALUE SPACE.
           05 COR-EMPTY            PIC  X(001) VALUE SPACE.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER      PIC  9(002) COMP-5 VALUE 0.
                 15 COLUMN-NUMBER   PIC  9(002) COMP-5 VALUE 0.
              10 BARRA-PROGRESSO.
                 15 CARACTER-BUFFER PIC  X(042)  VALUE SPACES.
                 15 PERC-ED         PIC  ZZ9,999 VALUE 0.
                 15 FILLER          PIC  X(001)  VALUE "%".
              10 ATTRIBUTE-BUFFER   PIC  X(050) VALUE SPACES.
              10 STRING-LENGTH      PIC  9(004) COMP-X VALUE 50.
           05 C                     PIC  9(003) VALUE 0.
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

       LINKAGE SECTION.

       01  PARAMETROS-CWPERC.
           05 CWPERC-LINE                    PIC  9(002).
           05 CWPERC-COLUMN                  PIC  9(002).
           05 CWPERC-COLOR-FULL       COMP-X PIC  9(002).
           05 CWPERC-COLOR-EMPTY      COMP-X PIC  9(002).
           05 CWPERC-FULL             COMP-3 PIC  9(018).
           05 CWPERC-EMPTY            COMP-3 PIC  9(018).
           05 CWPERC-TEXTO                   PIC  X(042).

       PROCEDURE DIVISION USING PARAMETROS-CWPERC.

       INICIO.

           ON  1
               CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA.

           COMPUTE PERC    = CWPERC-FULL / CWPERC-EMPTY * 100
           COMPUTE PERC-ED = CWPERC-FULL / CWPERC-EMPTY * 100
           IF  PERC > 2 AND PERC < 101
               MOVE CWPERC-TEXTO TO CARACTER-BUFFER
               COMPUTE ROW-NUMBER    = CWPERC-LINE   - 1
               COMPUTE COLUMN-NUMBER = CWPERC-COLUMN - 1
               COMPUTE PERC = PERC / 2
               COMPUTE C = CWPERC-COLOR-EMPTY   + 1
               MOVE COR (C) TO COR-EMPTY
               COMPUTE C = CWPERC-COLOR-FULL    + 1
               MOVE COR (C) TO COR-FULL
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
                 IF  I > PERC
                     MOVE COR-EMPTY TO ATTRIBUTE-BUFFER (I: 1)
                 ELSE
                     MOVE COR-FULL  TO ATTRIBUTE-BUFFER (I: 1)
                 END-IF
               END-PERFORM
               CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           END-IF.

       FIM. GOBACK.

       END PROGRAM CWPERC.
