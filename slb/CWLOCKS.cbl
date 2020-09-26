      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOCKS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/05/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Controle de registros travados p/investigar   *
                      * dead locks.                                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY LOCKS.sl.

       DATA DIVISION.
       FILE SECTION.

       COPY LOCKS.fd.

       WORKING-STORAGE SECTION.

       01  ER-LOCKS.
           05 FS-LOCKS           PIC X(002) VALUE SPACES.
           05 LB-LOCKS           PIC X(255) VALUE SPACES.

       77  TTY-NOME               PIC X(030) VALUE SPACES.
       77  USERNAME               PIC X(030) VALUE SPACES.
       77  DEVICE                 PIC X(030) VALUE SPACES.
       77  MESMO                  PIC 9(001) VALUE ZERO.
       77  APAGAR                 PIC 9(001) VALUE ZERO.
       77  I                      PIC S9(04) COMP-5 VALUE 0.
       77  STATUS-A               PIC X(002) VALUE SPACES.

       LINKAGE SECTION.

       01  OP PIC X(2).
           88 OP-CLOSE                     VALUE X"FA80" "X4".
           88 OP-DELETE                    VALUE X"FAF7".
           88 OP-OPEN-INPUT                VALUE X"FA00" "OQ".
           88 OP-OPEN-OUTPUT               VALUE X"FA01".
           88 OP-OPEN-IO                   VALUE X"FA02".
           88 OP-OPEN-EXTEND               VALUE X"FA03".
           88 OP-OPEN-INPUT-NR             VALUE X"FA04".
           88 OP-OPEN-OUTPUT-NR            VALUE X"FA05".
           88 OP-OPEN-INPUT-R              VALUE X"FA08".
           88 OP-OPEN-TABLE                VALUE "OT".
           88 OP-OPEN-QUEUE                VALUE "OQ".
           88 OP-OPEN                      VALUE X"FA00"
                                                 X"FA01"
                                                 X"FA02"
                                                 X"FA03"
                                                 X"FA04"
                                                 X"FA05"
                                                 X"FA08"
                                                 "OQ" "OT".
           88 OP-COMMIT                    VALUE X"FADC".
           88 OP-ROLLBACK                  VALUE X"FADD".
           88 OP-DELETE-FILE               VALUE X"FAF8".
           88 OP-READ-sequential-NO-LOCK   VALUE X"FA8D".
           88 OP-READ-sequential-LOCK      VALUE X"FAD8".
           88 OP-READ-sequential-KEPT-LOCK VALUE X"FAD9".
           88 OP-READ-sequential           VALUE X"FAF5".
           88 OP-READ-PREVIOUS-NO-LOCK     VALUE X"FA8C".
           88 OP-READ-PREVIOUS-LOCK        VALUE X"FADE".
           88 OP-READ-PREVIOUS-KEPT-LOCK   VALUE X"FADF".
           88 OP-READ-PREVIOUS             VALUE X"FAF9".
           88 OP-READ-random-NO-LOCK       VALUE X"FA8E".
           88 OP-READ-random-LOCK          VALUE X"FADA".
           88 OP-READ-random-KEPT-LOCK     VALUE X"FADB".
           88 OP-READ-random               VALUE X"FAF6".
           88 OP-READ-direct-NO-LOCK       VALUE X"FA8F".
           88 OP-READ-direct-LOCK          VALUE X"FAD6".
           88 OP-READ-direct-KEPT-LOCK     VALUE X"FAD7".
           88 OP-READ-direct               VALUE X"FAC9".
           88 OP-READ-position             VALUE X"FAF1".
           88 OP-READ                      VALUE X"FAD8" X"FAD9" X"FAF5"
                                                 X"FA8C" X"FADE" X"FADF"
                                                 X"FAF9" X"FA8E" X"FADA"
                                                 X"FADB" X"FAF6" X"FA8F"
                                                 X"FAD6" X"FAD7" X"FAC9"
                                                 X"FAF1".
           88 OP-START-EQUAL               VALUE X"FAE8" X"FAE9".
           88 OP-START-GREATER             VALUE X"FAEA".
           88 OP-START-NOT-LESS            VALUE X"FAEB".
           88 OP-START-LESS                VALUE X"FAFE".
           88 OP-START-NOT-GREATER         VALUE X"FAFF".
           88 OP-WRITE-BEFORE              VALUE X"FAE1".
           88 OP-WRITE-AFTER               VALUE X"FAE2".
           88 OP-WRITE                     VALUE X"FAF3".
           88 OP-WRITE-BEFORE-PAGE         VALUE X"FAE5".
           88 OP-WRITE-PAGE                VALUE X"FAE6".
           88 OP-WRITE-QUEUE               VALUE "WQ".
           88 OP-WRITE-TABLE               VALUE "WT".
           88 OP-REWRITE                   VALUE X"FAF4".
           88 OP-UNLOCK                    VALUE X"FA0E".

       01  FCD.
           COPY xfhfcd.

       01  LB     PIC X(255).
       01  TASK   PIC 9(006).

       COPY LOCKMODE.

       PROCEDURE DIVISION USING BY REFERENCE OP
                                BY REFERENCE FCD
                                LB TASK LOCK-MODE.

       000-INICIO.

           IF (FCD-FILE-STATUS > '09'
           AND (FCD-FILE-STATUS NOT = '9D')
           AND (FCD-FILE-STATUS NOT = '9A'))
           OR  FCD-FILE-STATUS = LOW-VALUES
           OR  FCD-HANDLE = NULL
               GOBACK
           END-IF

           ON  1
               DISPLAY 'COMPUTERNAME' UPON ENVIRONMENT-NAME
               ACCEPT  DEVICE    FROM ENVIRONMENT-VALUE
               IF  DEVICE = SPACES
                   DISPLAY "TTY"      UPON ENVIRONMENT-NAME
                   ACCEPT  DEVICE        FROM ENVIRONMENT-VALUE
                   IF DEVICE EQUAL SPACES
                      DISPLAY "SSH_TTY" UPON ENVIRONMENT-NAME
                      ACCEPT  DEVICE       FROM ENVIRONMENT-VALUE
                   END-IF
                   IF   DEVICE (1: 5) = "/dev/"
                        MOVE DEVICE (6: ) TO TTY-NOME
                        MOVE TTY-NOME  TO DEVICE
                   END-IF
               ELSE
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL TASK(I:1) NOT = '0'
                           CONTINUE
                   END-PERFORM
                   STRING DEVICE DELIMITED BY SPACE '('
                          TASK(I:) ')'  DELIMITED BY SIZE
                          INTO TTY-NOME
                   MOVE TTY-NOME TO DEVICE
                   MOVE 1        TO APAGAR
               END-IF
               DISPLAY 'USERNAME'     UPON ENVIRONMENT-NAME
               ACCEPT  USERNAME  FROM ENVIRONMENT-VALUE
               DISPLAY 'USER'         UPON ENVIRONMENT-NAME
               ACCEPT  USERNAME  FROM ENVIRONMENT-VALUE
               DISPLAY 'CWLOCKS' UPON ENVIRONMENT-NAME
               ACCEPT  LB-LOCKS FROM ENVIRONMENT-VALUE
               IF  LB-LOCKS NOT = SPACES
                   OPEN I-O LOCKS
                   IF  FS-LOCKS > '09'
                       DELETE FILE LOCKS
                       OPEN I-O LOCKS
                       IF  FS-LOCKS > '09'
                           MOVE SPACES TO LB-LOCKS
                       ELSE
                           CLOSE LOCKS
                           OPEN I-O LOCKS
                       END-IF
                   END-IF
               END-IF.

           IF  LB-LOCKS = SPACES
               GOBACK
           END-IF

           MOVE DEVICE       TO LOCKS-DEVICE
           SET  LOCKS-HANDLE TO LOCKS-HANDLE
           MOVE USERNAME     TO LOCKS-USERNAME

           IF  OP-COMMIT OR OP-ROLLBACK
               START LOCKS KEY NOT LESS LOCKS-COMMITS
               PERFORM UNTIL FS-LOCKS > '09'
                       READ LOCKS NEXT RECORD
                       IF FS-LOCKS < '10'
                          IF (LOCKS-USERNAME NOT = USERNAME)
                          OR (LOCKS-DEVICE   NOT = DEVICE)
                              EXIT PERFORM
                          END-IF
                          SET LOCKS-UNLOCKED TO TRUE
                          REWRITE LOCKS-REG
                       END-IF
               END-PERFORM
           ELSE
               READ LOCKS WITH LOCK
               IF  FS-LOCKS = '23'
                   SET  LOCKS-UNLOCKED TO TRUE
                   MOVE LB             TO LOCKS-LABEL
                   WRITE LOCKS-REG
                   READ LOCKS WITH LOCK
               END-IF
               EVALUATE TRUE
                   WHEN FCD-FILE-STATUS = '9D'
                     OR FCD-FILE-STATUS = '9A'
                        MOVE FCD-FILE-STATUS TO LOCKS-STATUS
                   WHEN OP-OPEN-OUTPUT
                     OR Lock-mode-EXCLUSIVE
                        SET LOCKS-FILE-LOCKED TO TRUE
                   WHEN OP-OPEN
                        SET LOCKS-OPENED TO TRUE
                   WHEN Lock-mode-AUTOMATIC
                    AND OP-READ
                    AND FCD-OPEN-MODE = 2
                        SET  LOCKS-RECORD-LOCKED TO TRUE
                   WHEN Lock-mode-MANUAL
                    AND FCD-OPEN-MODE = 2
                    AND(OP-READ-sequential-LOCK
                     OR OP-READ-sequential-KEPT-LOCK
                     OR OP-READ-PREVIOUS-LOCK
                     OR OP-READ-PREVIOUS-KEPT-LOCK
                     OR OP-READ-random-LOCK
                     OR OP-READ-random-KEPT-LOCK
                     OR OP-READ-direct-LOCK
                     OR OP-READ-direct-KEPT-LOCK)
                        SET  LOCKS-RECORD-LOCKED TO TRUE
                   WHEN(OP-WRITE
                     OR OP-REWRITE)
                    AND WRITELOCK-directive-enabled
                        SET  LOCKS-RECORD-LOCKED TO TRUE
                   WHEN OP-CLOSE
                        CONTINUE
                   WHEN OP-UNLOCK
                     OR OP-WRITE
                     OR OP-REWRITE
                     OR OP-DELETE
                     OR(OP-READ
                    AND FCD-OPEN-MODE = 2
                    AND (NOT Lock-on-multiple-records))
                        IF  FS-LOCKS < '10'
                            SET LOCKS-UNLOCKED TO TRUE
                        END-IF
               END-EVALUATE
               IF   OP-CLOSE
                    IF   APAGAR = 1
                         DELETE LOCKS RECORD
                         INITIALIZE LOCKS-REG
                    ELSE
                         SET  LOCKS-CLOSED TO TRUE
                         MOVE LB           TO LOCKS-LABEL
                         REWRITE LOCKS-REG
                         UNLOCK LOCKS RECORD
                    END-IF
               ELSE
                    MOVE LB TO LOCKS-LABEL
                    REWRITE LOCKS-REG
                    READ LOCKS WITH LOCK
               END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWLOCKS.
