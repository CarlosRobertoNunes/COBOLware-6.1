      $Set NoMs PreProcess(cobsql)
      $Set LinkCount(128)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSQLAT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  27/07/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Rollback                                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           EXEC SQL END   DECLARE SECTION END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       000-INICIO.

           DISPLAY "COMMIT"    UPON ENVIRONMENT-NAME
           DISPLAY "AUTOMATIC" UPON ENVIRONMENT-VALUE

           EXEC SQL
                ROLLBACK
           END-EXEC.

       END PROGRAM CWSQLAT.
