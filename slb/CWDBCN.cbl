       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWDBCN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/11/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Conecta ao Banco DB2                         *
                      *                                               *
                      *************************************************
       SPECIAL-NAMES.
         CALL-CONVENTION 74 IS DB2API.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SQLDA-ID pic 9(4) comp-5.
       01  SQLDSIZE pic 9(4) comp-5.
       01  SQL-STMT-ID pic 9(4) comp-5.
       01  SQLVAR-INDEX pic 9(4) comp-5.
       01  SQL-DATA-TYPE pic 9(4) comp-5.
       01  SQL-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-LITERAL pic X(258).
       01  SQL-LITERAL1 pic X(130).
       01  SQL-LITERAL2 pic X(130).
       01  SQL-LITERAL3 pic X(130).
       01  SQL-LITERAL4 pic X(130).
       01  SQL-LITERAL5 pic X(130).
       01  SQL-LITERAL6 pic X(130).
       01  SQL-LITERAL7 pic X(130).
       01  SQL-LITERAL8 pic X(130).
       01  SQL-LITERAL9 pic X(130).
       01  SQL-LITERAL10 pic X(130).
       01  SQL-IS-LITERAL pic 9(4) comp-5 value 1.
       01  SQL-IS-INPUT-HVAR pic 9(4) comp-5 value 2.
       01  SQL-CALL-TYPE pic 9(4) comp-5.
       01  SQL-SECTIONUMBER pic 9(4) comp-5.
       01  SQL-INPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-OUTPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQLA-PROGRAM-ID.
           05 SQL-PART1 pic 9(4) COMP-5 value 172.
           05 SQL-PART2 pic X(6) value "AEAMAI".
           05 SQL-PART3 pic X(24) value "MAitQIEZ01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 13.
           05 SQL-PART5 pic X(13) value "JORGEOLIVEIRA".
           05 SQL-PART6 pic X(115) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "CWDBCN  ".
           05 SQL-PART9 pic X(120) value LOW-VALUES.


       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 SQLMSG          PIC X(50) VALUE SPACES.
           05 MSG             PIC  X(068) VALUE SPACES.
           05 ONOFF           PIC  9(001) VALUE 3.
           05 BUFFER-SIZE     PIC S9(4) COMP-5 VALUE 1024.
           05 LINE-WIDTH      PIC S9(4) COMP-5 VALUE 80.
           05 ERROR-BUFFER    PIC X(1024).
       COPY CWSEND.

      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  USERNAME          PIC X(10).
       01  PASSWD            PIC X(10).
       01  DATABASE          PIC X(10).

      *EXEC SQL END DECLARE SECTION END-EXEC


      *EXEC SQL INCLUDE SQLCA END-EXEC
      * SQL Communication Area - SQLCA
       COPY 'sqlca.cbl'.

       LINKAGE SECTION.

       01 DB2-LOGIN.
          05 DB2-USERNAME PIC X(10).
          05 DB2-PASSWORD PIC X(10).

       PROCEDURE DIVISION USING DB2-LOGIN.

       000-INICIO.

           ON 1
              DISPLAY 'SQLMSG' UPON ENVIRONMENT-NAME
              ACCEPT SQLMSG FROM ENVIRONMENT-VALUE.

           IF DB2-LOGIN (1:6) = 'COMMIT'

      *EXEC SQL COMMIT
      *        END-EXEC
           CALL DB2API "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID
           MOVE 0 TO SQL-INPUT-SQLDA-ID
           MOVE 0 TO SQL-SECTIONUMBER
           MOVE 21 TO SQL-CALL-TYPE

           CALL DB2API "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL DB2API "sqlgstop" USING
            BY VALUE 0

              GOBACK
           END-IF

           IF DB2-LOGIN (1:8) = 'ROLLBACK'

      *EXEC SQL ROLLBACK
      *        END-EXEC
           CALL DB2API "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID
           MOVE 0 TO SQL-INPUT-SQLDA-ID
           MOVE 0 TO SQL-SECTIONUMBER
           MOVE 28 TO SQL-CALL-TYPE

           CALL DB2API "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL DB2API "sqlgstop" USING
            BY VALUE 0

              GOBACK
           END-IF

           IF   ONOFF = 1
                MOVE 2 TO ONOFF
CEDAE *         DISPLAY MSG AT 2501

      *EXEC SQL CONNECT RESET END-EXEC
           CALL DB2API "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID
           MOVE 0 TO SQL-INPUT-SQLDA-ID
           MOVE 3 TO SQL-SECTIONUMBER
           MOVE 29 TO SQL-CALL-TYPE

           CALL DB2API "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL DB2API "sqlgstop" USING
            BY VALUE 0

                EXIT PROGRAM
           END-IF
           DISPLAY "DB2_DATABASE" UPON ENVIRONMENT-NAME
           ACCEPT DATABASE   FROM ENVIRONMENT-VALUE
           IF   DATABASE = SPACES
                DISPLAY "DB2-DATABASE" UPON ENVIRONMENT-NAME
                ACCEPT DATABASE   FROM ENVIRONMENT-VALUE
           END-IF
           IF  (DATABASE = SPACES)
           OR  (DATABASE = LOW-VALUES)
               MOVE "Banco n∆o especificado na vari†vel DB2-DATABASE"
                    TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                ON EXCEPTION
                    DISPLAY CWSEND-MSG
                END-CALL
                STOP RUN
           END-IF

           MOVE DB2-USERNAME TO USERNAME
           MOVE DB2-PASSWORD TO PASSWD
           MOVE SPACES       TO MSG

           STRING "Conectando " DELIMITED BY SIZE
                   USERNAME     DELIMITED BY SPACE
                   " ao banco " DELIMITED BY SIZE
                   DATABASE     DELIMITED BY SPACE
                   "..."        DELIMITED BY SIZE
              INTO MSG
CEDAE *    DISPLAY MSG AT 2501
           INSPECT DATABASE CONVERTING MAIUSCULAS TO MINUSCULAS

      *EXEC SQL CONNECT TO  :DATABASE
      *                USER  :USERNAME
      *                USING :PASSWD
      *     END-EXEC
           CALL DB2API "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 1 TO SQL-STMT-ID
           MOVE 3 TO SQLDSIZE
           MOVE 2 TO SQLDA-ID

           CALL DB2API "sqlgaloc" USING
               BY VALUE SQLDA-ID
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL DB2API "sqlgstlv" USING
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE DATABASE
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL DB2API "sqlgstlv" USING
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE USERNAME
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL DB2API "sqlgstlv" USING
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE PASSWD
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID
           MOVE 2 TO SQL-INPUT-SQLDA-ID
           MOVE 5 TO SQL-SECTIONUMBER
           MOVE 29 TO SQL-CALL-TYPE

           CALL DB2API "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL DB2API "sqlgstop" USING
            BY VALUE 0

           MOVE SPACES TO MSG
           STRING "Desconectando " DELIMITED BY SIZE
                   USERNAME     DELIMITED BY SPACE
                   " do banco " DELIMITED BY SIZE
                   DATABASE     DELIMITED BY SPACE
                   "..."        DELIMITED BY SIZE
              INTO MSG

           IF   SQLCODE NOT = 0
                CALL DB2API "sqlgintp" USING
                                      BY VALUE     BUFFER-SIZE
                                      BY VALUE     LINE-WIDTH
                                      BY REFERENCE SQLCA
                                      BY REFERENCE ERROR-BUFFER
                MOVE ERROR-BUFFER TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                   ON EXCEPTION
                      if sqlmsg not = spaces
                         CALL SQLMSG USING SQLERRM
                          ON EXCEPTION
                             display CWSEND-MSG
                         END-CALL
                      else
                         display CWSEND-MSG
                      end-if
                STOP RUN
           ELSE
                MOVE 0       TO RETURN-CODE
                MOVE 1       TO ONOFF
           END-IF

           MOVE SPACES TO MSG.
CEDAE *    DISPLAY MSG AT 2501.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM CWDBCN.
