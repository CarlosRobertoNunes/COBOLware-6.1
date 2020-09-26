      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOCK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/03/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Controlar usuario logado                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. call-convention 74 is staticWINAPI
                      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWUSED ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWUSED-CHAVE
                  ALTERNATE RECORD KEY IS CWUSED-LISTA
                                        = CWUSED-USUARIO CWUSED-TASK
                  reserve no alternate area
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CWUSED.

       DATA DIVISION.
       FILE SECTION.

       FD  CWUSED
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWUSED.

       01  CWUSED-REG.
           05 CWUSED-CHAVE.
              10 CWUSED-TASK               PIC  9(006) COMP-3.
           05 CWUSED-LOGIN-DATA            PIC  9(008) COMP-3.
           05 CWUSED-LOGIN-HORA            PIC  9(006) COMP-3.
           05 CWUSED-USUARIO               PIC  X(030).
           05 CWUSED-MODULO                PIC  X(008).
           05 CWUSED-COMPUTERNAME          PIC  X(025).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 LOCKOPT                  PIC  X(003) VALUE SPACES.
           05 TTY-NOME                 PIC  X(029) VALUE SPACES.
           05 ER-CWUSED.
              10 FS-CWUSED             PIC  X(002) VALUE "00".
              10 LB-CWUSED             PIC  X(255) VALUE "cwused".
           05 OP-CWUSED                PIC  9(001) VALUE 0.
           05 COMPUTERNAME             PIC  X(025) VALUE SPACES.
           05 TTY                      PIC  X(025) VALUE SPACES.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.

       78 HKEY-LOCAL-MACHINE                      value h"80000002".
       01 sub-key-name            pic x(59)       value
          Z"System\CurrentControlSet\Control\ComputerName\ComputerName".
       78 KEY-ALL-ACCESS                          value h"3F".
       01 key-handle              pic x(4) comp-5 value zeroes.
       01 key-value-buffer        pic x(100)      value spaces.
       01 key-value-type          pic x(4) comp-5 value zeroes.
       01 key-value-length        pic x(4) comp-5 value 100.
       01 my-ret-code             pic x(4) comp-5 value zeroes.

       COPY CWTIME.
       COPY CWUNIX.

       LINKAGE SECTION.

       01  FUNCAO PIC X(01).
       01  NOME   PIC X(30).
       01  TASK   PIC 9(06).
       01  MODULO PIC X(08).

       PROCEDURE DIVISION USING FUNCAO NOME TASK MODULO.

       000-INICIO.

           ON 1
              DISPLAY 'CWLOCK' UPON ENVIRONMENT-NAME
              ACCEPT  LOCKOPT  FROM ENVIRONMENT-VALUE
              INSPECT LOCKOPT  CONVERTING 'of' TO 'OF'.

           IF LOCKOPT = 'OFF'
              GOBACK
           END-IF

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 0
                PERFORM 010-CHECK-COMPUTER
                   THRU 010-99-FIM
                 GOBACK
           END-IF

           IF   X91-PARAMETER = 1
                IF  OP-CWUSED = 1
                    READ CWUSED WITH LOCK
                    IF   FS-CWUSED = "00"
                         DELETE CWUSED RECORD
                         IF FS-CWUSED = '00'
                            WRITE CWUSED-REG
                            READ CWUSED WITH LOCK
                         END-IF
                    END-IF
                END-IF
                GOBACK
           END-IF

           IF   NOME = 'LOGON'
                GOBACK
           END-IF

           IF   OP-CWUSED = 0
                MOVE "cwused"    TO LB-CWUSED
                DISPLAY 'CWUSED' UPON ENVIRONMENT-NAME
                ACCEPT   LB-CWUSED FROM  ENVIRONMENT-VALUE
                CALL "CWFILE" USING LB-CWUSED
                OPEN I-O CWUSED
                IF   FS-CWUSED > "09"
                     DELETE FILE CWUSED
                     OPEN I-O CWUSED
                END-IF
                IF   FS-CWUSED > "09"
                     IF  FUNCAO = "?"
                         MOVE "N" TO FUNCAO
                         GOBACK
                     ELSE
                         CALL "CWISAM" USING ER-CWUSED
                         IF   FS-CWUSED = "9%"
                              STOP RUN
                         END-IF
                     END-IF
                ELSE
                     MOVE 1 TO OP-CWUSED
                     IF  FUNCAO = "?"
                         MOVE "S" TO FUNCAO
                         GOBACK
                     END-IF
                END-IF
           END-IF

           EVALUATE TRUE
               WHEN FUNCAO = "R"
                    MOVE TASK TO CWUSED-TASK
                    READ CWUSED WITH LOCK
               WHEN FUNCAO = "M"
                    MOVE TASK   TO CWUSED-TASK
                    READ CWUSED WITH LOCK
                    MOVE MODULO TO CWUSED-MODULO
                    REWRITE CWUSED-REG
                    MOVE TASK TO CWUSED-TASK
                    READ CWUSED WITH LOCK
               WHEN FUNCAO = "L"
                    SET CWTIME-REVERSED     TO TRUE
                    SET CWTIME-TODAY        TO TRUE
                    CALL "CWTIME"        USING PARAMETROS-CWTIME
                    MOVE TASK               TO CWUSED-TASK
                    MOVE NOME               TO CWUSED-USUARIO
                    MOVE CWTIME-DATE-FINAL  TO CWUSED-LOGIN-DATA
                    MOVE CWTIME-TIME-FINAL  TO CWUSED-LOGIN-HORA
                    MOVE "CWMENU"           TO CWUSED-MODULO
                    MOVE COMPUTERNAME       TO CWUSED-COMPUTERNAME
                    WRITE CWUSED-REG
                    IF   FS-CWUSED = "22"
                         REWRITE CWUSED-REG
                    END-IF
                    MOVE TASK TO CWUSED-TASK
                    READ CWUSED WITH LOCK
               WHEN FUNCAO = "U"
                    DELETE CWUSED RECORD
                    CLOSE CWUSED
                    MOVE 0 TO OP-CWUSED
                    DELETE FILE CWUSED
           END-EVALUATE.

       000-99-FIM. GOBACK.

       010-CHECK-COMPUTER.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF  CWUNIX-ON
               DISPLAY "COMPUTERNAME"  UPON ENVIRONMENT-NAME
               ACCEPT  COMPUTERNAME    FROM ENVIRONMENT-VALUE
               IF   COMPUTERNAME = SPACES
                    DISPLAY "TTY"         UPON ENVIRONMENT-NAME
                    ACCEPT  TTY           FROM ENVIRONMENT-VALUE
                    MOVE    TTY (6: )       TO COMPUTERNAME
                    IF TTY EQUAL SPACES
                       DISPLAY "SSH_TTY" UPON ENVIRONMENT-NAME
                       ACCEPT  TTY       FROM ENVIRONMENT-VALUE
                    END-IF
                    IF   TTY (1: 5) = "/dev/"
                         MOVE TTY (6: ) TO TTY-NOME
                         MOVE TTY-NOME  TO TTY
                    END-IF
               ELSE
                    INSPECT COMPUTERNAME
                            CONVERTING MINUSCULAS TO MAIUSCULAS
               END-IF
           ELSE
               call staticWINAPI "RegOpenKeyExA"
                  using by value          HKEY-LOCAL-MACHINE
                        by reference      sub-key-name
                        by value          0
                        by value          KEY-ALL-ACCESS
                        by reference      key-handle
                  returning my-ret-code
               end-call
               if my-ret-code = 0
                  call staticWINAPI "RegQueryValueExA"
                     using by value          key-handle
                           by reference      z"ComputerName"
                           by value          0
                           by reference      key-value-type
                           by reference      key-value-buffer
                           by reference      key-value-length
                     returning my-ret-code
                  end-call
                  if my-ret-code = 0
                     MOVE SPACES TO COMPUTERNAME
                     STRING key-value-buffer DELIMITED BY X"00"
                        INTO COMPUTERNAME
                     INSPECT COMPUTERNAME
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                  end-if
               end-if
               call staticWINAPI "RegCloseKey"
                  using by value key-handle
                  returning my-ret-code
               end-call
           END-IF.

       010-99-FIM. EXIT.
       END PROGRAM CWLOCK.
