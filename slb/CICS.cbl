      $SET NoOsVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CICS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/09/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simulador do monitor transacional CICS       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PCT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS PCT-KEY
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-PCT.

           SELECT STARTS  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS STARTS-KEY
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-STARTS.

       DATA DIVISION.
       FILE SECTION.

       COPY STARTS.
       COPY PCT.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TRANSACTION         PIC  X(004) VALUE SPACES.
           05 BUFFER              PIC X(1920) VALUE LOW-VALUES.
           05 IBUFFER             PIC X(1920) VALUE LOW-VALUES.
           05 TRMID               PIC  X(004) VALUE SPACES.
           05 FS-PCT              PIC  X(002) VALUE '00'.
           05 LB-PCT              PIC  X(255) VALUE "cicsPCT".
           05 SZ-STARTS           pic  9(004) comp-5.
           05 FS-STARTS           PIC  X(002) VALUE "00".
           05 LB-STARTS           PIC  X(255) VALUE SPACES.
           05 SYSID               PIC  X(004) value 'DATA'.
           05 START-MODE          PIC  9(001) VALUE 0.
           05 T                   PIC  9(002) COMP-X.
           05 INDICADOR.
              10 FILLER           PIC  S9(8) COMP.
              10 ADDRESS-COMMAREA POINTER.

       COPY CWCICS.
       COPY DFHAID.

       LINKAGE SECTION.

       01 CWTRANSID PIC X(4).

       PROCEDURE DIVISION USING CWTRANSID.

       000-INICIO.

           DISPLAY "SYSID"      UPON ENVIRONMENT-NAME
           ACCEPT  SYSID        FROM ENVIRONMENT-VALUE
           CALL "FS_CREATE_DIR" USING SYSID

           OPEN I-O PCT
           IF FS-PCT > '09'
              EXEC COBOLware ISAMerr
                   STATUS FS-PCT
                   LABEL  LB-PCT
              END-EXEC
              GOBACK
           END-IF

           MOVE X"0000" TO CWCICS-FUNCTION
           perform 010-CICS THRU 010-99-FIM
           MOVE LOW-VALUES TO PARAMETROS-CWCICS
           DISPLAY "TRMID" UPON ENVIRONMENT-NAME
           ACCEPT  TRMID   FROM ENVIRONMENT-VALUE

           IF  CWTRANSID = '0000'
           OR  CWTRANSID = SPACES
               MOVE SPACES TO LB-STARTS
               STRING SYSID    DELIMITED BY SPACE
                      '/'      DELIMITED BY SIZE
                      'cicsSTART('   DELIMITED BY SIZE
                      TRMID    DELIMITED BY SPACE
                      ')'      DELIMITED BY SIZE
                 INTO LB-STARTS
               OPEN I-O STARTS
               IF FS-STARTS = '00'
                  READ STARTS NEXT RECORD
                  IF FS-STARTS = '00'
                     MOVE 1 TO START-MODE
                  END-IF
               ELSE
                  IF FS-STARTS > '09'
                     EXEC COBOLware ISAMerr
                          STATUS FS-STARTS
                          LABEL  LB-STARTS
                     END-EXEC
                     CLOSE PCT
                     GOBACK
                  END-IF
               END-IF
           END-IF

           PERFORM UNTIL TRANSACTION = 'BYE' OR 'END'
              IF CWTRANSID = '0000'
              OR CWTRANSID = SPACES
                 MOVE 1     TO STARTS-ENTRY
                 START STARTS KEY NOT LESS STARTS-KEY
                 IF FS-STARTS < '10'
                    READ STARTS NEXT RECORD
                    IF FS-STARTS < '10'
                        MOVE STARTS-INTERVAL  TO CWCICS-INTERVAL
                        MOVE STARTS-REQID     TO CWCICS-REQID
                        MOVE STARTS-RTERMID   TO CWCICS-RTERMID
                        MOVE STARTS-RTRANSID  TO CWCICS-RTRANSID
                        MOVE STARTS-TRANSID   TO CWCICS-TRANSID
                        MOVE STARTS-STARTCODE TO CWCICS-STARTCODE
                        MOVE STARTS-EIBAID    TO   CICS-EIBAID
                        MOVE STARTS-TECLA     TO CWCICS-AID
                        IF STARTS-LENGTH NOT = 0
                           MOVE STARTS-FROM(1:STARTS-LENGTH)
                             TO CWCICS-COMMAREA
                           MOVE STARTS-LENGTH TO CWCICS-LENGTH
      *                     SET CWCICS-SET TO ADDRESS OF CWCICS-COMMAREA
                        END-IF
                        DELETE STARTS RECORD
                    ELSE
                        MOVE '23' TO FS-STARTS
                    END-IF
                 END-IF
                 IF FS-STARTS > '09'
                    IF START-MODE = 1
                       CLOSE STARTS
                       CANCEL "CWCICS"
                       DELETE FILE STARTS
                       GOBACK
                    END-IF
                    CALL "CW3278" USING "S" EIBAID IBUFFER TRANSACTION T
                    MOVE LOW-VALUES TO BUFFER
                    CALL "CW3278" USING "G" EIBAID BUFFER  TRANSACTION T
                    IF  (TRANSACTION NOT = SPACES)
                    AND (TRANSACTION NOT = 'BYE')
                    AND (TRANSACTION NOT = 'END')
                    MOVE    TRANSACTION   TO CWCICS-TRANSID
                    MOVE    'TO'          TO CWCICS-STARTCODE
                    SET     CWCICS-START  TO TRUE
                    PERFORM VARYING CWCICS-LENGTH FROM LENGTH OF BUFFER
                                                    BY -1
                            UNTIL CWCICS-LENGTH = 0
                            OR(BUFFER(CWCICS-LENGTH:1) NOT = LOW-VALUE)
                            CONTINUE
                    END-PERFORM
                    MOVE BUFFER         (1:CWCICS-LENGTH)
                      TO CWCICS-COMMAREA(1:CWCICS-LENGTH)
                    MOVE TRANSACTION TO CWCICS-RTRANSID
                    MOVE TRMID       TO CWCICS-RTERMID
                    MOVE EIBAID      TO   CICS-EIBAID
                    MOVE T           TO CWCICS-AID
                    perform 010-CICS THRU 010-99-FIM
                    IF CWCICS-TRANSIDERR
                       MOVE
           'DFH2001 INVALID TRANSACTION IDENTIFICATION-PLEASE RESUBMIT'
                         TO IBUFFER(1:58)
                    END-IF
                    MOVE LOW-VALUES TO PARAMETROS-CWCICS
                    END-IF
                    EXIT PERFORM CYCLE
                 END-IF
              ELSE
                 MOVE CWTRANSID TO TRANSACTION
              END-IF
              IF TRANSACTION NOT =  SPACES
              AND TRANSACTION NOT = 'BYE'
              AND TRANSACTION NOT = 'END'
                  MOVE TRANSACTION TO PCT-TRANSACTION
                  MOVE 0           TO PCT-STEP
                  READ PCT
                  IF FS-PCT > '09'
                     EXEC COBOLware Send Message 'Transa‡Æo inv lida'
                     END-EXEC
                  ELSE
                     DISPLAY "TRNID"         UPON ENVIRONMENT-NAME
                     DISPLAY PCT-TRANSACTION UPON ENVIRONMENT-VALUE
                     DISPLAY 'TRANSPGM'      UPON ENVIRONMENT-NAME
                     DISPLAY PCT-PROGRAM     UPON ENVIRONMENT-VALUE
                     PERFORM TEST AFTER UNTIL FS-PCT > '09'
                           DISPLAY "CWCHAIN"  UPON ENVIRONMENT-NAME
                           DISPLAY SPACES     UPON ENVIRONMENT-VALUE
                        DISPLAY "CURRENTPROG" UPON ENVIRONMENT-NAME
                        DISPLAY PCT-PROGRAM   UPON ENVIRONMENT-VALUE
                        MOVE X"FFFF" TO CWCICS-FUNCTION
                        perform 010-CICS THRU 010-99-FIM
                        SET ADDRESS-COMMAREA TO ADDRESS OF BUFFER
                        CALL  PCT-PROGRAM USING INDICADOR
                        CANCEL PCT-PROGRAM
                        PERFORM TEST AFTER UNTIL PCT-PROGRAM = SPACES
                           MOVE LOW-VALUES TO PARAMETROS-CWCICS
                           DISPLAY "CWCHAIN"  UPON ENVIRONMENT-NAME
                           MOVE SPACES TO PCT-PROGRAM
                           ACCEPT PCT-PROGRAM FROM ENVIRONMENT-VALUE
                           IF PCT-PROGRAM NOT = SPACES
                              DISPLAY SPACES UPON ENVIRONMENT-VALUE
                              CALL "CWCHAIN" USING "GET"
                                                    CWCICS-COMMAREA
                                                    CWCICS-LENGTH
                              CANCEL "CWCHAIN"
                              CALL PCT-PROGRAM USING CWCICS-COMMAREA
                                                    (1:CWCICS-LENGTH)
                              CANCEL PCT-PROGRAM
                           END-IF
                        END-PERFORM
                        ADD 1 TO PCT-STEP
                        READ PCT
                     END-PERFORM
                  END-IF
              END-IF
              IF  (CWTRANSID NOT = '0000')
              AND (CWTRANSID NOT = SPACES)
                  MOVE 'BYE' TO TRANSACTION
              END-IF
           END-PERFORM
           CLOSE PCT STARTS
           CANCEL "CWCICS"
           DELETE FILE STARTS.

       000-99-FIM. GOBACK.

       010-CICS.

           CALL "CWCICS" USING PARAMETROS-CWCICS
           IF CICS-purge
              CANCEL "CWCICS"
              move low-values to CWCICS-ABCODE
              GO TO 010-CICS
           END-IF.

       010-99-FIM. EXIT.

       END PROGRAM CICS.
