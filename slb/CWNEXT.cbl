       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWNEXT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/03/2013.
       SECURITY.      *************************************************
                      *                                               *
                      *  Fornece NEXTVAL de sequenciar por chave      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 NOW.
              10 CURDATE   PIC 9(16).
           05 I            PIC 9(02) VALUE 0.

       COPY CWCONF.

       LINKAGE SECTION.

       COPY CWNEXT.

       PROCEDURE DIVISION USING PARAMETROS-CWNEXT.

           ON 1
              SET CWSQLC-UPDATE TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

           MOVE '00' TO CWNEXT-STATUS

           IF FS-CWCONF > '09'
              MOVE FUNCTION CURRENT-DATE TO NOW
              MOVE CURDATE TO CWNEXT-VALUE
              MOVE FS-CWCONF TO CWNEXT-STATUS
              GOBACK
           END-IF

           MOVE 'SQ'   TO CWCONF-REG

           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE

           IF CWNEXT-DROP
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              IF FS-CWCONF = '00'
                 SET CWSQLC-DELETE TO TRUE
                 CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              END-IF
              MOVE FS-CWCONF TO CWNEXT-STATUS
              MOVE '00' TO FS-CWCONF
              MOVE ZERO TO CWNEXT-VALUE
              GOBACK
           END-IF

           IF CWNEXT-SET
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              IF CWNEXT-INCREMENT > 1
                 MOVE CWNEXT-INCREMENT TO CWCONF-INCREMENT
              ELSE
                 IF   FS-CWCONF > '09'
                      MOVE 1 TO CWCONF-INCREMENT
                 END-IF
              END-IF
              IF   FS-CWCONF = '00'
                   SET CWSQLC-REWRITE TO TRUE
              ELSE
                   SET CWSQLC-WRITE TO TRUE
              END-IF
              MOVE CWNEXT-VALUE TO CWCONF-VALUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              MOVE FS-CWCONF TO CWNEXT-STATUS
              MOVE '00' TO FS-CWCONF
              GOBACK
           END-IF

           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF FS-CWCONF = '23'
              IF   CWNEXT-INCREMENT > 1
                   MOVE CWNEXT-INCREMENT TO CWCONF-INCREMENT
              ELSE
                   MOVE 1 TO CWCONF-INCREMENT
              END-IF
              MOVE 0 TO CWCONF-VALUE CWNEXT-VALUE
              SET CWSQLC-WRITE TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              SET CWSQLC-READ TO TRUE
              SET CWSQLC-EQUAL TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF FS-CWCONF NOT = "00"
              MOVE FUNCTION CURRENT-DATE TO NOW
              MOVE CURDATE TO CWNEXT-VALUE
              MOVE FS-CWCONF TO CWNEXT-STATUS
              MOVE '00' TO FS-CWCONF
              GOBACK
           END-IF

           ADD  CWCONF-INCREMENT TO CWCONF-VALUE
           MOVE CWCONF-VALUE     TO CWNEXT-VALUE
           IF  FS-CWCONF < '10'
               SET CWSQLC-REWRITE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
              MOVE FUNCTION CURRENT-DATE TO NOW
              MOVE CURDATE TO CWNEXT-VALUE
           END-IF
           MOVE FS-CWCONF TO CWNEXT-STATUS
           MOVE '00' TO FS-CWCONF
           GOBACK.

       END PROGRAM CWNEXT.
