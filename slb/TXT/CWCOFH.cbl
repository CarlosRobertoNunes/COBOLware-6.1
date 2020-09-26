       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCOFH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/09/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  I/O COBOL Standard de CWCONF                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWCONF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWCONF-CHAVE
                  LOCK MODE     IS MANUAL
                  RESERVE       NO ALTERNATE AREA
                  FILE STATUS   IS FS-CWCONF.


      *    SELECT optional LOG    ASSIGN TO DISK
      *           ORGANIZATION  IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.

       FD  CWCONF
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 32 TO 2008 DEPENDING ON SZ-CWCONF
           VALUE OF FILE-ID IS LB-CWCONF.

       01  CWCONF-REG.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO                        PIC  X(002).
              10 CWCONF-ELEMENTO                    PIC  X(030).
            05 CWCONF-RESTO                         PIC  X(001)
               OCCURS 1 TO 1976 DEPENDING ON SZ-RESTO.


      *FD  LOG
      *    VALUE OF FILE-ID IS 'cwconf.log'.
      *
      *01  log-reg.
      *    05 log-isam                              pic x(6).
      *    05 log-CWCONF-LK                         PIC X(2008).
      *    05 log-FS-LK                             PIC X(002).
      *    05 log-ISAM-KEY                          PIC X(030).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 SZ-CWCONF                     PIC  9(004) VALUE 0.
           05 SZ-RESTO                      PIC  9(004) VALUE 0.
           05 FIM                           PIC  9(004) VALUE 0.
           05 ER-CWCONF.
              10 FS-CWCONF                  PIC  X(002) VALUE "00".
              10 LB-CWCONF                  PIC  X(255) VALUE "cwconf".
           05 OPEN-MODE                     PIC  9(001) VALUE 3.
              88 OPENED-UPDATE                          VALUE 1.
              88 OPENED-INQUIRY                         VALUE 0.
              88 CLOSED                                 VALUE 3.

       LINKAGE SECTION.

       01  ISAM.
           05  ISAM-FUNCTION                 PIC X(001).
               88 ISAM-OPEN                               VALUE "O" "L".
               88 ISAM-UPDATE                             VALUE "L".
               88 ISAM-CREATE                             VALUE "N".
               88 ISAM-CLOSE                              VALUE "C".
               88 ISAM-COMMIT-MANUAL                      VALUE "M".
               88 ISAM-COMMIT-AUTOMATIC                   VALUE "A".
               88 ISAM-READ                               VALUE "R".
               88 ISAM-START                              VALUE "S".
               88 ISAM-WRITE                              VALUE "W".
               88 ISAM-REWRITE                            VALUE "U".
               88 ISAM-DELETE                             VALUE "D".
               88 ISAM-KILL                               VALUE "K".
               88 ISAM-COMMIT                             VALUE "T".
               88 ISAM-UNLOCK                             VALUE "t".
               88 ISAM-ROLLBACK                           VALUE "B".
               88 ISAM-MODIFY                     VALUE "K" "D" "U" "W".
           05  ISAM-OPTION                   PIC X(002).
               88 ISAM-NEXT                               VALUE "->".
               88 ISAM-PREVIOUS                           VALUE "<-".
               88 ISAM-EQUAL                              VALUE "=".
               88 ISAM-LESS                               VALUE "<".
               88 ISAM-GREATER                            VALUE ">".
               88 ISAM-NOT-LESS                           VALUE ">=".
               88 ISAM-NOT-GREATER                        VALUE "<=".
           05  ISAM-LOCK-MODE                PIC X(001).
               88 ISAM-AUTOMATIC                          VALUE "A".
               88 ISAM-LOCK                               VALUE "1".
               88 ISAM-NO-LOCK                            VALUE "0" "3".
               88 ISAM-IGNORE-LOCK                        VALUE "3".
           05  ISAM-CONTROL                  PIC X(002).

       01  CWCONF-LK                         PIC X(2008).
       01  FS-LK                             PIC X(002).
       01  ISAM-KEY                          PIC X(030).

       PROCEDURE DIVISION USING ISAM
                                CWCONF-LK
                                FS-LK
                                ISAM-KEY.

       000-INICIO.

           MOVE 2008 TO SZ-CWCONF
           MOVE 1976 TO SZ-RESTO

           MOVE CWCONF-LK                         TO CWCONF-REG
      *    open extend log
      *    move isam        to log-isam
      *    move CWCONF-LK   to log-CWCONF-LK
      *    move FS-LK       to log-FS-LK
      *    move ISAM-KEY    to log-ISAM-KEY
      *    write log-reg
           EVALUATE TRUE
               WHEN ISAM-CREATE
                    DISPLAY  "CWCONF" UPON ENVIRONMENT-NAME
                    ACCEPT  LB-CWCONF FROM ENVIRONMENT-VALUE
                    DISPLAY LB-CWCONF UPON ENVIRONMENT-VALUE
                    OPEN OUTPUT CWCONF
                    IF   FS-CWCONF < "10"
                         SET OPENED-UPDATE        TO TRUE
                    END-IF
               WHEN ISAM-OPEN
                    DISPLAY "CWCONF"  UPON ENVIRONMENT-NAME
                    ACCEPT LB-CWCONF  FROM ENVIRONMENT-VALUE
                    DISPLAY LB-CWCONF UPON ENVIRONMENT-VALUE
                    IF  ISAM-UPDATE
                        SET OPENED-UPDATE         TO TRUE
                        OPEN I-O CWCONF
                    ELSE
                        SET OPENED-INQUIRY        TO TRUE
                        OPEN INPUT CWCONF
                    END-IF
                    IF   FS-CWCONF = X"397C"
                         CALL "CWISAM" USING ER-CWCONF
                    END-IF
               WHEN ISAM-CLOSE
                    IF   NOT CLOSED
                         CLOSE CWCONF
                         SET CLOSED               TO TRUE
                    END-IF
               WHEN ISAM-START
                    EVALUATE TRUE
                        WHEN ISAM-EQUAL
                             START CWCONF KEY EQUAL CWCONF-CHAVE
                        WHEN ISAM-LESS
                             START CWCONF KEY LESS CWCONF-CHAVE
                        WHEN ISAM-GREATER
                             START CWCONF KEY GREATER CWCONF-CHAVE
                        WHEN ISAM-NOT-LESS
                             START CWCONF KEY NOT LESS CWCONF-CHAVE
                        WHEN ISAM-NOT-GREATER
                             START CWCONF KEY NOT GREATER CWCONF-CHAVE
                    END-EVALUATE
               WHEN ISAM-READ
                    MOVE 1976 TO SZ-RESTO
                    IF  (ISAM-NEXT OR ISAM-PREVIOUS)
                         IF   ISAM-NEXT
                              EVALUATE TRUE
                                  WHEN ISAM-IGNORE-LOCK
                                       READ CWCONF NEXT RECORD
                                                   IGNORE LOCK
                                  WHEN ISAM-NO-LOCK
                                    OR OPENED-INQUIRY
                                       READ CWCONF NEXT RECORD NO LOCK
                                  WHEN(ISAM-AUTOMATIC
                                    OR ISAM-LOCK)
                                   AND OPENED-UPDATE
                                       READ CWCONF  NEXT RECORD LOCK
                                  WHEN OTHER
                                       READ CWCONF NEXT RECORD
                              END-EVALUATE
                         ELSE
                              EVALUATE TRUE
                                  WHEN ISAM-IGNORE-LOCK
                                       READ CWCONF
                                            PREVIOUS RECORD IGNORE LOCK
                                  WHEN ISAM-NO-LOCK
                                    OR OPENED-INQUIRY
                                       READ CWCONF
                                            PREVIOUS RECORD NO LOCK
                                  WHEN(ISAM-AUTOMATIC
                                    OR ISAM-LOCK)
                                   AND OPENED-UPDATE
                                       READ CWCONF
                                            PREVIOUS RECORD LOCK
                                  WHEN OTHER
                                       READ CWCONF
                                            PREVIOUS RECORD
                              END-EVALUATE
                         END-IF
                    ELSE
                         EVALUATE TRUE
                             WHEN ISAM-IGNORE-LOCK
                                  READ CWCONF IGNORE LOCK
                                       KEY IS CWCONF-CHAVE
                             WHEN ISAM-NO-LOCK
                               OR OPENED-INQUIRY
                                  READ CWCONF NO LOCK
                                       KEY IS CWCONF-CHAVE
                             WHEN(ISAM-AUTOMATIC
                               OR ISAM-LOCK)
                              AND OPENED-UPDATE
                                  READ CWCONF LOCK KEY IS CWCONF-CHAVE
                             WHEN OTHER
                                  READ CWCONF KEY IS CWCONF-CHAVE
                         END-EVALUATE
                    END-IF
                    IF   FS-CWCONF = "9D"
                         CALL "CWISAM" USING ER-CWCONF
                         GO               TO 000-INICIO
                    END-IF
                    IF  FS-CWCONF < "10"
                        MOVE CWCONF-REG(1:SZ-CWCONF) TO CWCONF-LK
                    END-IF
      *             move all '*'     to log-isam
      *             move CWCONF-LK   to log-CWCONF-LK
      *             move FS-LK       to log-FS-LK
      *             move ISAM-KEY    to log-ISAM-KEY
      *             write log-reg
               WHEN ISAM-WRITE
                    MOVE 1976 TO SZ-RESTO
                    PERFORM VARYING SZ-CWCONF FROM 2008 BY -1
                            UNTIL SZ-CWCONF = 32
                               OR(CWCONF-RESTO (SZ-RESTO) NOT = SPACE)
                            SUBTRACT 1 FROM SZ-RESTO
                    END-PERFORM
                    WRITE CWCONF-REG
               WHEN ISAM-REWRITE
                    MOVE CWCONF-LK TO CWCONF-REG
                    PERFORM TEST AFTER UNTIL FS-CWCONF NOT = '9D'
                            READ CWCONF WITH LOCK
                            IF FS-CWCONF = '9D'
                               CALL "CWISAM" USING ER-CWCONF
                            END-IF
                    END-PERFORM
                    IF FS-CWCONF < '10'
                       DELETE CWCONF RECORD
                    END-IF
                    MOVE CWCONF-LK TO CWCONF-REG
                    MOVE 1976 TO SZ-RESTO
                    PERFORM VARYING SZ-CWCONF FROM 2008 BY -1
                              UNTIL SZ-CWCONF = 32
                               OR(CWCONF-RESTO (SZ-RESTO) NOT = SPACE)
                            SUBTRACT 1 FROM SZ-RESTO
                    END-PERFORM
                    WRITE CWCONF-REG
               WHEN ISAM-DELETE
                    DELETE CWCONF RECORD
               WHEN ISAM-UNLOCK
                    UNLOCK CWCONF
               WHEN ISAM-COMMIT
                    COMMIT
               WHEN ISAM-ROLLBACK
                    ROLLBACK
               WHEN ISAM-KILL
                    DELETE FILE CWCONF
           END-EVALUATE

      *    close log
           MOVE SPACES    TO ISAM-KEY
           MOVE FS-CWCONF TO FS-LK.

       000-99-FIM. GOBACK.

       END PROGRAM CWCOFH.
