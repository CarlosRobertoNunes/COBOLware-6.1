      $Set LinkCount"128"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCONF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/09/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Seletor de manipulador do CWCONF             *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  CWCONF-FS PIC X(003) VALUE SPACES.
       01  FHCONF.
           05 FHP    PIC X(006) VALUE SPACES.
           05 NIVEL  PIC 9(001) VALUE 0.

       01  ER-ISAM.
           05 FS PIC X(002) VALUE "00".
           05 LB PIC X(050) VALUE SPACES.

       01  NIVEIS VALUE SPACES.
           05 LIVRE OCCURS 9 PIC X.

       LINKAGE SECTION.

       COPY CWSQLC.
       01  CWCONF-LK                         PIC X(2008).
       01  FS-LK                             PIC  X(002).
       01  ISAM-KEY                          PIC  X(030).
       01  ISAM-PT                           PIC  X(001).

       PROCEDURE DIVISION USING CWSQLC
                                CWCONF-LK
                                FS-LK
                                ISAM-KEY
                                ISAM-PT.

       000-INICIO.

           IF  FHP = SPACES
               DISPLAY "CWCONF_FS" UPON ENVIRONMENT-NAME
               ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
               IF CWCONF-FS = SPACES
                  DISPLAY "CWCONF-FS" UPON ENVIRONMENT-NAME
                  ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
               END-IF
               IF CWCONF-FS = "ON" OR "On" OR "on" OR "oN"
                  MOVE "CWCOFS0" TO FHCONF
               ELSE
                  MOVE "CWCOFH0" TO FHCONF
               END-IF
           END-IF.

           IF  CWSQLC(1:4) = "ISAM"
               MOVE "cwconf" TO LB
               DISPLAY "CWCONF" UPON ENVIRONMENT-NAME
               ACCEPT  LB       FROM ENVIRONMENT-VALUE
               CALL "CWISAM" USING ER-ISAM
               GOBACK
           END-IF

           IF  CWSQLC-OPEN
               SET CWSQLC-UPDATE TO TRUE
           END-IF

           IF  ISAM-PT = SPACE
               PERFORM VARYING NIVEL FROM 1 BY 1 UNTIL NIVEL = 0
                       IF LIVRE (NIVEL) = SPACE
                          MOVE NIVEL TO LIVRE (NIVEL)
                          EXIT PERFORM
                       END-IF
               END-PERFORM
               MOVE NIVEL   TO ISAM-PT
           ELSE
               MOVE ISAM-PT TO NIVEL
           END-IF
           CALL FHCONF USING CWSQLC CWCONF-LK FS-LK ISAM-KEY
           IF CWSQLC-CLOSE
              CANCEL FHCONF
              MOVE SPACE TO LIVRE(NIVEL)
                            ISAM-PT
           END-IF
           SET CWSQLC-NO-LOCK TO TRUE
           MOVE FS-LK TO FS.

       000-99-FIM. GOBACK.
       END PROGRAM CWCONF.
