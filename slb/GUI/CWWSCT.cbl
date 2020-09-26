       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWSCT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_WRITE_SCR_TTY                     *
                      *  Write characters TTY-style                   *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 OC             PIC  9(03) VALUE 0.
           05 POS            PIC  9(04) VALUE 0.
           05 LEN            PIC  9(08) VALUE 0.
           05 TAMANHO-MATRIZ PIC  9(008) COMP-X.
           05 TEXTO.
              10 CWUSER OCCURS 25.
                 15 CWUSER-ATT.
                    20              PIC X(004).
                    20 CWUSER-LEN   PIC 9(002).
                    20              PIC X(034).
                 15 CWUSER-PIC      PIC X(080).
                 15 CWUSER-DATANAME PIC X(030).
                 15 CWUSER-DATA     PIC X(080).

       LINKAGE SECTION.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC  9(004) COMP-X.

       PROCEDURE DIVISION.

       000-INICIO.

            MOVE STRING-LENGTH TO LEN
            IF   LEN > 80
                 MOVE 80 TO LEN
            END-IF
            MOVE 1 TO POS
            PERFORM UNTIL LEN = 0
                    ADD  1                     TO OC
                    MOVE "000000TfbsbzebrahuA" TO CWUSER-ATT      (OC)
                    MOVE "X"                   TO CWUSER-PIC      (OC)
                    MOVE "TTY-style"           TO CWUSER-DATANAME (OC)
                    IF   LEN < 80
                         MOVE LEN TO CWUSER-LEN (OC)
                         MOVE 0   TO LEN
                    ELSE
                         MOVE 80       TO CWUSER-LEN (OC)
                         SUBTRACT 80 FROM LEN
                    END-IF
                    MOVE CHARACTER-BUFFER (POS: CWUSER-LEN (OC))
                      TO CWUSER-DATA (OC)
                    ADD  CWUSER-LEN (OC) TO POS
            END-PERFORM
            COMPUTE TAMANHO-MATRIZ = OC * LENGTH OF TEXTO
            CALL "CWUSER" USING "DCWWSCT  ," TEXTO TAMANHO-MATRIZ.

       000-99-FIM. GOBACK.
       END PROGRAM CWWSCT.
