       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SQLMSG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/02/2013.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava erro SQLCODE no log de carga           *
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

       77  CWOBS                         PIC X(5000) VALUE SPACES.
       77  I                      COMP-X PIC  9(004) VALUE 0.
       77  Y                      COMP-X PIC  9(004) VALUE 0.

       LINKAGE SECTION.

       01 SQLCA.
           05  SQLCAID         PIC X(8)         VALUE "SQLCA   ".
           05  SQLCABC         PIC S9(9) COMP-5 VALUE 136.
           05  SQLCODE         PIC S9(9) COMP-5 VALUE 0.
           05  SQLERRM.
               49  SQLERRML    PIC S9(4) COMP-5.
               49  SQLERRMC    PIC X(70).
           05  SQLERRP         PIC X(8).
           05  SQLERRD         PIC S9(9) COMP-5 OCCURS 6 VALUE 0.
           05  SQLWARN.
               10  SQLWARN0    PIC X.
               10  SQLWARN1    PIC X.
               10  SQLWARN2    PIC X.
               10  SQLWARN3    PIC X.
               10  SQLWARN4    PIC X.
               10  SQLWARN5    PIC X.
               10  SQLWARN6    PIC X.
               10  SQLWARN7    PIC X.
           05  SQLEXT.
               10  SQLWARN8    PIC X.
               10  SQLWARN9    PIC X.
               10  SQLWARN10   PIC X.
               10  SQLWARNA    PIC X REDEFINES SQLWARN10.
               10  SQLSTATE    PIC X(5).

       01 DATASET PIC X.

       PROCEDURE DIVISION USING SQLCA DATASET.

       000-INICIO.


            MOVE '('    TO CWOBS
            MOVE 1      TO Y
            PERFORM VARYING I FROM 1 BY 1 UNTIL DATASET(I:1) = X'00'
                    ADD 1 TO Y
                    MOVE DATASET(I:1) TO CWOBS(Y:1)
            END-PERFORM
            ADD 1    TO Y
            MOVE ')' TO CWOBS(Y:1)
            ADD 1    TO Y
            PERFORM VARYING I FROM 1 BY 1
                             UNTIL I > LENGTH SQLERRMC
                                OR SQLERRMC (I:1) = X"0D"
                                OR SQLERRMC (I:1) = X"0A"
                    ADD 1               TO Y
                    MOVE SQLERRMC (I:1) TO CWOBS(Y:1)
            END-PERFORM
            CALL "CWCLOG" USING " " CWOBS
              ON EXCEPTION
                 CONTINUE
            END-CALL.

       000-99-FIM. GOBACK.

       END PROGRAM SQLMSG.
