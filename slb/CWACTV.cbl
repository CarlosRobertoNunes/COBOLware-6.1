       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWACTV INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/05/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Gera chaves de ativacao                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 VALUE "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmno
      -             "pqrstuvwxyz!@#$%&*()-_=+\|:;<>,./?".
              06 LETRA OCCURS 85            PIC  X(001).
           05 SENHA.
              06 SENHA-X                    PIC  9(014) COMP-X.
           05 SENHA-N REDEFINES SENHA       PIC  9(006).
           05 VALIDADE                      PIC  X(016).
           05 VALIDADE-N REDEFINES VALIDADE PIC  9(008).
           05 REDEFINES VALIDADE.
              06 DIA                        PIC  9(002).
              06 MES                        PIC  9(002).
              06 ANO                        PIC  9(004).
           05 VALIDADE-99                   PIC  9(008).
           05 REDEFINES VALIDADE-99.
              06 DIA-99                     PIC  9(002).
              06 MES-99                     PIC  9(002).
              06 ANO-99                     PIC  9(002).
           05 CHAVE.
              06 CHAVE-X                    PIC  9(014) COMP-X.
           05 REDEFINES CHAVE.
              06 BYTE-CHAVE                 PIC  X(001) OCCURS 6.
           05 CHAVE-D                       PIC V9(014).
           05 CHAVE-N                       PIC  9(014).
           05 C                             PIC  9(002) VALUE 0.
           05 M                             PIC  9(002) VALUE 0.
           05 OUTRO                         PIC  9(002) VALUE 0.
           05 OK                            PIC  9(001) VALUE 0.

           05 I                        PIC 9(03) VALUE 0.
           05 ATIVACAO-X.
              10 BYTE-X                PIC 9(002) COMP-X OCCURS 6.
           05 ATIVACAO-N               PIC 9(006) VALUE 0.
           05 ATIVACAO-K               PIC 9(006) VALUE 0.
           05 REDEFINES ATIVACAO-K.
              10 BYTE-N                PIC 9(003) OCCURS 6.

       COPY CWTIME.

       LINKAGE SECTION.

       01  PARAMETROS-CWACTV.
           05 CWACTV-SENHA       PIC 9(006).
           05 CWACTV-VALIDADE    PIC 9(008).
           05 CWACTV-ATIVACAO    PIC X(006).
           05 CWACTV-VALIDADE-10 PIC 9(008).
           05 CWACTV-ATIVACAO-10 PIC X(006).
           05 CWACTV-VALIDADE-15 PIC 9(008).
           05 CWACTV-ATIVACAO-15 PIC X(006).
           05 CWACTV-VALIDADE-20 PIC 9(008).
           05 CWACTV-ATIVACAO-20 PIC X(006).

       PROCEDURE DIVISION USING PARAMETROS-CWACTV.

       000-INICIO.

           MOVE    CWACTV-SENHA      TO SENHA
           MOVE    CWACTV-VALIDADE   TO VALIDADE
           PERFORM 010-GERA-CHAVE  THRU 010-99-FIM
           MOVE    CHAVE             TO CWACTV-ATIVACAO

           SET     CWTIME-NORMAL     TO TRUE
           SET     CWTIME-ADD-DAYS   TO TRUE
           MOVE    CWACTV-VALIDADE   TO CWTIME-DATE
           MOVE    10                TO CWTIME-DAYS
           CALL    "CWTIME"       USING PARAMETROS-CWTIME
           MOVE    CWTIME-DATE-FINAL TO CWACTV-VALIDADE-10
                                        VALIDADE
           PERFORM 010-GERA-CHAVE  THRU 010-99-FIM
           MOVE    CHAVE             TO CWACTV-ATIVACAO-10

           SET     CWTIME-NORMAL     TO TRUE
           SET     CWTIME-ADD-DAYS   TO TRUE
           MOVE    CWACTV-VALIDADE   TO CWTIME-DATE
           MOVE    15                TO CWTIME-DAYS
           CALL    "CWTIME"       USING PARAMETROS-CWTIME
           MOVE    CWTIME-DATE-FINAL TO CWACTV-VALIDADE-15
                                        VALIDADE
           PERFORM 010-GERA-CHAVE  THRU 010-99-FIM
           MOVE    CHAVE             TO CWACTV-ATIVACAO-15

           SET     CWTIME-NORMAL     TO TRUE
           SET     CWTIME-ADD-DAYS   TO TRUE
           MOVE    CWACTV-VALIDADE   TO CWTIME-DATE
           MOVE    20                TO CWTIME-DAYS
           CALL    "CWTIME"       USING PARAMETROS-CWTIME
           MOVE    CWTIME-DATE-FINAL TO CWACTV-VALIDADE-20
                                        VALIDADE
           PERFORM 010-GERA-CHAVE  THRU 010-99-FIM
           MOVE    CHAVE             TO CWACTV-ATIVACAO-20.

       000-99-FIM. GOBACK.

       010-GERA-CHAVE.

           MOVE DIA TO DIA-99
           MOVE MES TO MES-99
           MOVE ANO TO ANO-99
           COMPUTE CHAVE-D = SENHA-N / VALIDADE-99
           MOVE CHAVE-D (1: 14) TO CHAVE-N
           MOVE CHAVE-N         TO CHAVE-X
           COMPUTE OUTRO = (MES * 2) + MES
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 6
                   MOVE 0 TO OK
                   PERFORM VARYING M FROM 1 BY 1 UNTIL M > 85
                                                    OR OK = 1
                           IF  BYTE-CHAVE (C) =  LETRA (M)
                               MOVE 1 TO OK
                           END-IF
                   END-PERFORM
                   IF   OK = 0
                        ADD 3 TO OUTRO
                        MOVE LETRA (OUTRO) TO BYTE-CHAVE (C)
                   END-IF
           END-PERFORM

           MOVE CHAVE TO ATIVACAO-X
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
                   MOVE BYTE-X (I) TO BYTE-N (I)
                   MOVE BYTE-N (I) (3: 1) TO ATIVACAO-N (I: 1)
           END-PERFORM

           MOVE ATIVACAO-N TO CHAVE.

       010-99-FIM. EXIT.

       END PROGRAM CWACTV.
