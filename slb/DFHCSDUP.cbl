      $SET NoOsVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DFHCSDUP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/09/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Interpreta e indexa as definicoes de DFHCDUP *
                      *  transa‡äes e programas para cicsPCT/cicsTCT  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT JOB ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-JOB.

           SELECT PCT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS PCT-KEY
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-PCT.

           SELECT PPT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS PPT-PROGRAM
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-PPT.

       DATA DIVISION.
       FILE SECTION.

       FD  JOB
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-JOB.

       01  JOB-RECORD            PIC X(080).

       FD  PCT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PCT.

       01  PCT-RECORD.
           05 PCT-KEY.
              06 PCT-TRANSACTION  PIC X(04).
              06 PCT-STEP         PIC 9(04) COMP-X.
           05 PCT-PROGRAM         PIC X(08).
           05 PCT-GROUP           PIC X(08).
           05 PCT-DESCRIPTION     PIC X(50).

       COPY PPT.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 I                   PIC 9(008) VALUE 0 COMP-X.
           05 Y                   PIC 9(008) VALUE 0 COMP-X.
           05 RECORDTYPE          PIC X(003) VALUE SPACES.
           05 DEFINE              PIC 9(001) VALUE 0.
           05 WORD                PIC X(050) VALUE SPACES.
           05 PARM                PIC X(050) VALUE SPACES.
           05 END-WORD            PIC X(001) VALUE SPACE.
           05 FS-JOB              PIC X(002) VALUE '00'.
           05 LB-JOB              PIC X(255) VALUE "DFHCSDUP.JCL".
           05 FS-PCT              PIC X(002) VALUE '00'.
           05 LB-PCT              PIC X(255) VALUE "cicsPCT".
           05 FS-PPT              PIC X(002) VALUE '00'.
           05 LB-PPT              PIC X(255) VALUE "cicsPPT".

       PROCEDURE DIVISION.

       000-INICIO.

           OPEN INPUT JOB
           IF FS-JOB = '30' OR '35'
              MOVE "DFHCSDUP.jcl" TO LB-JOB
              OPEN INPUT JOB
              IF FS-JOB = '30' OR '35'
                 MOVE "dfhcsdup.JCL" TO LB-JOB
                 OPEN INPUT JOB
                 IF FS-JOB = '30' OR '35'
                    MOVE "dfhcsdup.jcl" TO LB-JOB
                    OPEN INPUT JOB
                 END-IF
              END-IF
           END-IF

           IF FS-JOB > '09'
              EXEC COBOLware ISAMerr
                   STATUS FS-JOB
                   LABEL  LB-JOB
              END-EXEC
              GOBACK
           END-IF

           DELETE FILE PCT
           OPEN I-O PCT
           IF FS-PCT > '09'
              EXEC COBOLware ISAMerr
                   STATUS FS-PCT
                   LABEL  LB-PCT
              END-EXEC
              CLOSE JOB
              GOBACK
           END-IF

           DELETE FILE PPT
           OPEN I-O PPT
           IF FS-PPT > '09'
              EXEC COBOLware ISAMerr
                   STATUS FS-PPT
                   LABEL  LB-PPT
              END-EXEC
              CLOSE JOB PPT
              GOBACK
           END-IF

           MOVE SPACES TO WORD

           PERFORM UNTIL FS-JOB > '09'
            READ JOB
            IF FS-JOB < '10'
            AND JOB-RECORD(1:1) NOT = '*'
               INITIALIZE WORD END-WORD PARM Y DEFINE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
                    IF  (JOB-RECORD(I:1) NOT = SPACE)
                    OR  END-WORD = ')'
                        EVALUATE TRUE
                        WHEN WORD = 'DEFINE'
                             INITIALIZE WORD END-WORD PARM Y
                             MOVE 1 TO DEFINE
                             SUBTRACT 1 FROM I
                        WHEN JOB-RECORD(I:1) = '('
                             MOVE 0   TO Y
                             MOVE ')' TO END-WORD
                        WHEN JOB-RECORD(I:1) = ')'
                             PERFORM 010-CHECK-WORD THRU 010-99-FIM
                             INITIALIZE WORD END-WORD PARM Y DEFINE
                        WHEN END-WORD = ')'
                             ADD 1 TO Y
                             MOVE JOB-RECORD(I:1) TO PARM(Y:1)
                        WHEN OTHER
                             ADD 1 TO Y
                             MOVE JOB-RECORD(I:1) TO WORD(Y:1)
                        END-EVALUATE
                    END-IF
               END-PERFORM
            END-IF
           END-PERFORM

           CLOSE JOB.

       000-99-FIM. GOBACK.

       010-CHECK-WORD.

           IF DEFINE = 1
              IF WORD(1:4) = 'PROG'
                 INITIALIZE PPT-RECORD
                 MOVE PARM TO PPT-PROGRAM
                 WRITE PPT-RECORD
                 MOVE 'PPT' TO RECORDTYPE
              END-IF
              IF WORD(1:5) = 'TRANS'
                 INITIALIZE PCT-RECORD
                 MOVE PARM TO PCT-TRANSACTION
                 WRITE PCT-RECORD
                 MOVE 'PCT' TO RECORDTYPE
              END-IF
           ELSE
             EVALUATE TRUE
                 WHEN WORD(1:4) = 'PROG'
                      PERFORM TEST AFTER UNTIL FS-PCT NOT = '22'
                              MOVE PARM TO PCT-PROGRAM
                              IF PCT-STEP NOT = 0
                                 MOVE SPACES TO PCT-GROUP
                                                PCT-DESCRIPTION
                                 WRITE PCT-RECORD
                              ELSE
                                 REWRITE PCT-RECORD
                              END-IF
                              ADD 1 TO PCT-STEP
                      END-PERFORM
                 WHEN WORD(1:4) = 'DESC'
                      IF RECORDTYPE = 'PCT'
                         MOVE 0 TO PCT-STEP
                         READ PCT
                         IF FS-PCT < '10'
                            MOVE PARM TO PCT-DESCRIPTION
                            REWRITE PCT-RECORD
                         END-IF
                      ELSE
                         MOVE PARM TO PPT-DESCRIPTION
                         REWRITE PPT-RECORD
                      END-IF
                 WHEN WORD(1:4) = 'LANG'
                      MOVE PARM TO PPT-LANGUAGE
                      REWRITE PPT-RECORD
                 WHEN WORD(1:4) = 'G'
                      IF RECORDTYPE = 'PCT'
                         MOVE 0 TO PCT-STEP
                         READ PCT
                         IF FS-PCT < '10'
                            MOVE PARM TO PCT-GROUP
                            REWRITE PCT-RECORD
                         END-IF
                      ELSE
                         MOVE PARM TO PPT-GROUP
                         REWRITE PPT-RECORD
                      END-IF
             END-EVALUATE
           END-IF.

       010-99-FIM. EXIT.
       END PROGRAM DFHCSDUP.
