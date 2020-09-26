       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMAIL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/01/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Enviar Email                                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
           CALL-CONVENTION 66 IS WIN32.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL INI ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-INI
                  LOCK MODE     IS EXCLUSIVE.

           SELECT LISTA ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-LISTA
                  LOCK MODE     IS EXCLUSIVE.

       DATA DIVISION.
       FILE SECTION.

       FD  INI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-INI.

       01  INI-REG    PIC X(3000).

       FD  LISTA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LISTA.

       01  LISTA-REG    PIC X(050).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 RESTO                    PIC  X(050) VALUE SPACES.
           05 AMBIENTE                 PIC  X(050) VALUE SPACES.
           05 COMANDO                  PIC  X(255) VALUE SPACES.
           05 PROGRAMA                 PIC  X(050) VALUE SPACES.
           05 ATTACH                   PIC  X(250) VALUE SPACES.
           05 POSTDIR                  PIC  X(100) VALUE SPACES.
           05 LB-INFILE                PIC  X(100) VALUE SPACES.
           05 BARRA                    PIC  X(001) VALUE "\".
           05 SMTP                     PIC  9(001) VALUE 0.
           05 NR                       PIC  9(001) VALUE 0.
           05 DATE-W                   PIC  9(008) VALUE 0.
           05 TIME-W                   PIC  9(006) VALUE 0.
           05 TASK                     PIC  9(006) VALUE 0.
           05 SEQ                      PIC  9(006) VALUE 0.
           05 SEQ2                     PIC  9(006) VALUE 0.
           05 UNICO                    PIC  9(001) VALUE 0.
           05 I                        PIC  9(004) VALUE 0.
           05 T                        PIC  9(004) VALUE 0.
           05 Y                        PIC  9(004) VALUE 0.
           05 E                        PIC  9(004) VALUE 0.
           05 L                        PIC  9(004) VALUE 0.
           05 BUFFER-INI               PIC  X(255) VALUE SPACES.
           05 ER-INI.
              10 FS-INI                PIC  X(002) VALUE "00".
              10 LB-INI                PIC  X(255) VALUE SPACES.
           05 TEMP                     PIC  X(256) VALUE SPACES.
           05 MAILPATH                 PIC  X(256) VALUE SPACES.
           05 NEW                      PIC  X(256) VALUE SPACES.
           05 COBOLWARE                PIC  X(256) VALUE SPACES.
           05 CMDLINE                  PIC  X(256) VALUE SPACES.
           05 CMDSHOW                  PIC  9(004) COMP-5.
           05 CMDSTATUS                PIC  9(004) COMP-5.
           05 RESULTADO                PIC  9(002) COMP-X VALUE 0.
           05 ER-LISTA.
              10 FS-LISTA              PIC  X(002) VALUE "00".
              10 LB-LISTA              PIC  X(255) VALUE SPACES.

       01  CBL-READ-WRITE-SCR-CHARS-ATTR.
           10 SCREEN-POSITION.
              15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
              15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
           10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
           10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
           10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.

       COPY CWUNIX.
       COPY CWGETS.
       COPY CWCONF.
       COPY CWEXEC.

       LINKAGE SECTION.

       COPY CWMAIL.

       PROCEDURE DIVISION USING PARAMETROS-CWMAIL.

       000-INICIO.

            MOVE 0           TO SMTP
            CALL "CWGETS" USING PARAMETROS-CWGETS
            INSPECT PARAMETROS-CWMAIL CONVERTING LOW-VALUE TO SPACE
            ON 1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                DISPLAY "COBOLWARE"  UPON ENVIRONMENT-NAME
                ACCEPT   COBOLWARE   FROM ENVIRONMENT-VALUE
                DISPLAY "TEMP"       UPON ENVIRONMENT-NAME
                ACCEPT   TEMP        FROM ENVIRONMENT-VALUE
                DISPLAY "CWMAILPATH" UPON ENVIRONMENT-NAME
                ACCEPT   MAILPATH    FROM ENVIRONMENT-VALUE.

           IF   CWMAIL-TEXT     = SPACES
           AND  CWMAIL-SUBJECT  = SPACES
           AND  CWMAIL-REPORT   = SPACES
           AND  CWMAIL-TO       = SPACES
                GOBACK
           END-IF

           IF   CWMAIL-REPORT NOT = SPACES
                CALL "CWFILE" USING CWMAIL-REPORT
                IF CWMAIL-REPORT (1:1) = '$'
                   MOVE 0 TO Y
                   MOVE SPACES TO AMBIENTE RESTO
                   PERFORM VARYING I FROM 2 BY 1
                             UNTIL I > LENGTH CWMAIL-REPORT
                                OR CWMAIL-REPORT (I:1) = SPACE
                           IF  CWMAIL-REPORT (I:1) = '/' OR '\'
                               MOVE CWMAIL-REPORT (I:) TO RESTO
                               EXIT PERFORM
                           ELSE
                               ADD 1 TO Y
                               MOVE CWMAIL-REPORT (I:1)
                                 TO AMBIENTE      (Y:1)
                           END-IF
                   END-PERFORM
                   DISPLAY AMBIENTE UPON ENVIRONMENT-NAME
                   MOVE SPACES TO AMBIENTE
                   ACCEPT AMBIENTE FROM ENVIRONMENT-VALUE
                   MOVE SPACES TO CWMAIL-REPORT
                   STRING AMBIENTE RESTO DELIMITED BY SPACE
                          INTO CWMAIL-REPORT
                END-IF
           END-IF

           IF  CWUNIX-ON
           OR  (MAILPATH NOT = SPACES)
               MOVE SPACES TO LB-INI
               IF   CWUNIX-ON
                    MOVE '/'    TO BARRA
               END-IF
               IF  MAILPATH NOT = SPACES
                   ADD 1 TO SEQ
                   STRING MAILPATH DELIMITED BY SPACE
                          BARRA "poster-" CWGETS-TASK "-" SEQ ".ini"
                          DELIMITED BY SIZE
                          INTO LB-INI
               ELSE
                   STRING TEMP DELIMITED BY SPACE
                          BARRA "mail-" CWGETS-TASK ".pl"
                          DELIMITED BY SIZE
                          INTO LB-INI
                   STRING "perl " DELIMITED BY SIZE
                          LB-INI DELIMITED BY SIZE INTO CMDLINE
               END-IF
           END-IF

           IF  CWUNIX-ON
               CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 STRING-LENGTH
           END-IF

           IF   CWMAIL-TO = SPACES OR "@"
                EXEC COBOLware BoxDialog
                     LINE 11 COLUMN 12
                     HEADER "Destinat rio do e-mail"
                     Caption(1) "E-mail"
                     Size(1) 50
                     Data(1) SPACES;CWMAIL-TO
                END-EXEC
           END-IF

           IF   CWMAIL-TO = SPACES
                IF   CWMAIL-REPORT NOT = SPACES
                     CALL "CBL_DELETE_FILE" USING CWMAIL-REPORT
                END-IF
                GOBACK
           END-IF

           MOVE 0 TO UNICO
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > LENGTH OF CWMAIL-TO
                   IF   CWMAIL-TO (I: 1) = "@"
                        MOVE 1 TO UNICO
                   END-IF
           END-PERFORM

           IF  UNICO = 1
               PERFORM ENVIA
           ELSE
               IF   CWMAIL-TO (1: 1) = "@"
                    MOVE CWMAIL-TO(2:) TO LB-LISTA
               ELSE
                    MOVE CWMAIL-TO TO LB-LISTA
               END-IF
               IF  LB-LISTA NOT = SPACES
                   IF LB-LISTA (1:1) = '&'
                      MOVE LB-LISTA (2:) To CWMAIL-TO
                      DISPLAY CWMAIL-TO UPON ENVIRONMENT-NAME
                      MOVE SPACES TO CWMAIL-TO
                      ACCEPT CWMAIL-TO FROM ENVIRONMENT-VALUE
                      IF  CWMAIL-TO NOT = SPACES
                          PERFORM ENVIA
                      END-IF
                   ELSE
                      IF LB-LISTA (1:1) = "*"
                         MOVE LB-LISTA (2: ) TO COMANDO
                         IF COMANDO = SPACES
                             MOVE "@" TO CWMAIL-TO
                             GO TO 000-INICIO
                         END-IF
                         MOVE 0 TO Y
                         MOVE SPACES TO LB-LISTA
                         PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > LENGTH COMANDO
                           EVALUATE TRUE
                                WHEN COMANDO (I:1) = SPACE
                                 AND PROGRAMA = SPACE
                                 AND LB-LISTA = SPACE
                                     CONTINUE
                               WHEN LB-LISTA NOT = SPACE
                                    ADD 1 TO Y
                                    MOVE COMANDO(I:1) TO LB-LISTA(Y:1)
                                WHEN(COMANDO (I:1) NOT = SPACE)
                                 AND PROGRAMA = SPACE
                                     ADD 1 TO Y
                                     MOVE COMANDO(I:1) TO PROGRAMA(Y:1)
                                WHEN(COMANDO (I:1) NOT = SPACE)
                                 AND PROGRAMA = SPACE
                                     ADD 1 TO Y
                                     MOVE COMANDO(I:1) TO PROGRAMA(Y:1)
                                WHEN COMANDO (I:1) = SPACE
                                 AND (PROGRAMA NOT = SPACE)
                                 AND (LB-LISTA = SPACE)
                                     MOVE ZERO TO Y
                                     MOVE X"00"
                                       TO LB-LISTA (LENGTH LB-LISTA: 1)
                                WHEN OTHER
                                     ADD 1 TO Y
                                     MOVE COMANDO(I:1) TO PROGRAMA(Y:1)
                           END-EVALUATE
                         END-PERFORM
                         MOVE SPACE TO LB-LISTA (LENGTH LB-LISTA: 1)
                         CALL PROGRAMA USING LB-LISTA
                           ON EXCEPTION
                              MOVE SPACES TO LB-LISTA
                              STRING 'M¢dulo seletor de e-mail "'
                                               DELIMITED BY SIZE
                                      PROGRAMA DELIMITED BY SPACE
                                '" nÆo encontrado' DELIMITED BY SIZE
                                INTO LB-LISTA
                              EXEC COBOLware Send MSG LB-LISTA END-EXEC
                              MOVE "@" TO CWMAIL-TO
                              GO TO 000-INICIO
                         END-CALL
                      END-IF
                      OPEN INPUT LISTA
                      PERFORM UNTIL FS-LISTA NOT = "00"
                              READ LISTA INTO CWMAIL-TO
                                IF  FS-LISTA = "00"
                                AND (CWMAIL-TO NOT = SPACES)
                                    PERFORM ENVIA
                                END-IF
                      END-PERFORM
                      CLOSE LISTA
                      DELETE FILE LISTA
                   END-IF
               END-IF
      *        IF  NOT CWUNIX-ON
      *        AND ( CWMAIL-REPORT NOT = SPACES )
      *              CALL "CBL_DELETE_FILE" USING CWMAIL-REPORT
      *              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
      *                IF CWMAIL-ATTACH (I) NOT = SPACES
      *                   CALL "CBL_DELETE_FILE" USING CWMAIL-ATTACH (I)
      *                END-IF
      *              END-PERFORM
      *        END-IF
           END-IF

           IF   CWUNIX-ON
                IF  (CWMAIL-REPORT NOT = SPACES)
                AND  MAILPATH = SPACES
                     MOVE SPACES TO CMDLINE
                     STRING "rm -rf " DELIMITED BY SIZE
                            CWMAIL-REPORT DELIMITED BY SPACE
                            INTO CMDLINE
                     CALL "system" USING CMDLINE
                END-IF
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   CARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
           ELSE
                IF   MAILPATH = SPACES
                     MOVE SPACES TO POSTDIR
                     STRING TEMP      DELIMITED BY SPACE
                            "\poster" DELIMITED BY SIZE
                        INTO POSTDIR
                     CALL "CBL_DELETE_DIR" USING POSTDIR
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       ENVIA.

           IF   NOT CWUNIX-ON
                CALL "CWTASK" USING "5" TASK
                IF   MAILPATH = SPACES
                     MOVE SPACES TO LB-INI
                     STRING TEMP DELIMITED BY SPACE
                            "\poster" TASK (5: 2) ".ini"
                            DELIMITED BY SIZE
                            INTO LB-INI
                     MOVE SPACES TO CMDLINE
                     IF   CWUNIX-DOS16
                     OR   CWUNIX-WIN16
                          STRING COBOLWARE DELIMITED BY SPACE
                                 "\poster.exe " DELIMITED BY SIZE
                                 LB-INI DELIMITED BY SIZE INTO CMDLINE
                     ELSE
                          STRING COBOLWARE DELIMITED BY SPACE
                                 "\poster.exe " DELIMITED BY SIZE
                                 LB-INI DELIMITED BY SPACE
                                 X"00" DELIMITED BY SIZE
                                 INTO CMDLINE
                     END-IF
                END-IF
                IF  UNICO = 0
      *         OR (CWMAIL-ATTACHES NOT = SPACES)
                    MOVE SPACES TO POSTDIR
                    STRING TEMP      DELIMITED BY SPACE
                           "\poster" DELIMITED BY SIZE
                       INTO POSTDIR
                    CALL "CBL_CREATE_DIR" USING POSTDIR
                    MOVE SPACES TO POSTDIR
                    STRING TEMP       DELIMITED BY SPACE
                           "\poster\"
                           TASK       DELIMITED BY SIZE
                       INTO POSTDIR
                    CALL "CBL_CREATE_DIR" USING POSTDIR
                END-IF
           END-IF

           OPEN OUTPUT INI
           MOVE 1 TO NR
           IF   CWUNIX-ON
           AND  MAILPATH = SPACES
                WRITE INI-REG FROM "use MIME::Entity;"
                WRITE INI-REG FROM
                  "$top = MIME::Entity->build(Type =>'multipart/mixed',"
                WRITE INI-REG FROM "Type => 'multipart/mixed',"
           ELSE
                WRITE INI-REG FROM "[Config]"
                IF CWMAIL-QUIET
                   WRITE INI-REG FROM "Quiet=Yes"
                END-IF
           END-IF

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "PS" TO CWCONF-REGPS
           CALL "CWGETS" USING PARAMETROS-CWGETS
           MOVE CWGETS-USUARIO TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   CWMAIL-SERVER = SPACES
           AND  CWCONF-SMTP   = SPACES
                DISPLAY "SMTPSERVER" UPON ENVIRONMENT-NAME
                ACCEPT CWMAIL-SERVER FROM ENVIRONMENT-VALUE
                IF   CWMAIL-SERVER NOT =  SPACES
                     DISPLAY "SMTPUSERNAME"  UPON ENVIRONMENT-NAME
                     ACCEPT CWMAIL-USER      FROM ENVIRONMENT-VALUE
                     IF   CWMAIL-FROM =  SPACES
                          MOVE CWMAIL-USER
                            TO CWMAIL-FROM
                          MOVE 0 TO NR
                     END-IF
                     DISPLAY "SMTPPASSWORD"  UPON ENVIRONMENT-NAME
                     ACCEPT CWMAIL-PASSWORD  FROM ENVIRONMENT-VALUE
                     DISPLAY "SMTPSSL"       UPON ENVIRONMENT-NAME
                     ACCEPT CWMAIL-SSL-FLAG  FROM ENVIRONMENT-VALUE
                     DISPLAY "SMTPAUT"       UPON ENVIRONMENT-NAME
                     ACCEPT CWMAIL-AUT-FLAG  FROM ENVIRONMENT-VALUE
                END-IF
           END-IF

           IF   CWMAIL-PORT = SPACES
                DISPLAY "SMTPPORT"      UPON ENVIRONMENT-NAME
                ACCEPT CWMAIL-PORT      FROM ENVIRONMENT-VALUE
           END-IF

           IF   CWMAIL-FROM NOT =  SPACES
                MOVE 0      TO NR
                MOVE SPACES TO INI-REG
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     STRING "From => '" DELIMITED BY SIZE
                            CWMAIL-FROM  DELIMITED BY SPACE
                           "',"          DELIMITED BY SIZE
                                    INTO INI-REG
                     WRITE INI-REG
                ELSE
                     IF FS-CWCONF = "00"
Borghe*              AND (CWCONF-E-MAIL NOT = SPACES)
Borghe               AND (CWCONF-E-MAIL = CWMAIL-FROM)
                        perform GRAVA-SMTP
                     ELSE
                        MOVE SPACES TO INI-REG
                        STRING "Sender=" CWMAIL-FROM
                                       DELIMITED BY SIZE
                        INTO INI-REG
                        WRITE INI-REG
                     END-IF
                END-IF
           ELSE
                IF FS-CWCONF = "00"
                   IF  (CWCONF-E-MAIL NOT = SPACES)
                   AND (CWCONF-E-MAIL(1: 1) NOT = X"00")
                       MOVE 0 TO NR
                       IF   CWUNIX-ON
                       AND  MAILPATH = SPACES
                            STRING "From => '"   DELIMITED BY SIZE
                                   CWCONF-E-MAIL DELIMITED BY SPACE
                                   "',"          DELIMITED BY SIZE
                                            INTO INI-REG
                            WRITE INI-REG
                       ELSE
                            perform GRAVA-SMTP
                       END-IF
                   ELSE
                       IF   CWUNIX-ON
                       AND  MAILPATH = SPACES
                            WRITE INI-REG FROM
                                 "From => 'spool@COBOLware.com',"          DELIMITED BY SIZE
                       END-IF
                   END-IF
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   CWMAIL-TO NOT =  SPACES
                MOVE SPACES TO INI-REG
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     STRING "To => '" DELIMITED BY SIZE
                            CWMAIL-TO DELIMITED BY SPACE
                            "',"      DELIMITED BY SIZE
                                 INTO INI-REG
                ELSE
                     STRING "OutFile=" CWMAIL-TO DELIMITED BY SIZE
                                 INTO INI-REG
                END-IF
                WRITE INI-REG
           END-IF

           IF   NR = 1
                EXEC COBOLware Pack
                     String CWMAIL-SUBJECT
                     WIDTH T
                END-EXEC
                ADD 1 TO T
                MOVE " (nÆo responder)"  TO CWMAIL-SUBJECT(T:)
           END-IF

           IF   CWMAIL-SUBJECT NOT =  SPACES
                MOVE SPACES TO INI-REG
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     PERFORM VARYING E FROM LENGTH OF CWMAIL-SUBJECT
                                  BY -1
                             UNTIL CWMAIL-SUBJECT(E: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     STRING "Subject => '" DELIMITED BY SIZE
                            CWMAIL-SUBJECT(1: E) DELIMITED BY SIZE
                            "');"      DELIMITED BY SIZE
                                 INTO INI-REG
                ELSE
                     STRING "Subject=" CWMAIL-SUBJECT DELIMITED BY SIZE
                            INTO INI-REG
                END-IF
                INSPECT INI-REG CONVERTING ACENTOS-850
                                        TO ACENTOS-WINDOWS
                WRITE INI-REG
           ELSE
                WRITE INI-REG FROM "Subject='Listagem em anexo',"
           END-IF

           IF   CWMAIL-TEXT NOT =  SPACES
                MOVE SPACES TO INI-REG
                PERFORM VARYING E FROM LENGTH OF CWMAIL-TEXT
                             BY -1
                        UNTIL CWMAIL-TEXT(E: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     STRING "$message ='" CWMAIL-TEXT(1: E)
                            "';" DELIMITED BY SIZE
                            INTO INI-REG
                ELSE
                     STRING "Text=" CWMAIL-TEXT(1: E) DELIMITED BY SIZE
                               INTO INI-REG
                END-IF
                INSPECT INI-REG CONVERTING ACENTOS-850
                                        TO ACENTOS-WINDOWS
                WRITE INI-REG
           END-IF

           IF  (NOT CWUNIX-ON) OR (MAILPATH NOT = SPACES)
           AND  SMTP = 0
                IF   CWMAIL-SERVER NOT =  SPACES
                     MOVE SPACES TO INI-REG
                     STRING "SmtpServer="
                            CWMAIL-SERVER DELIMITED BY SIZE
                                 INTO INI-REG WRITE INI-REG
                END-IF
                IF   CWMAIL-USER NOT =  SPACES
                     MOVE SPACES TO INI-REG
                     STRING "User=" CWMAIL-USER DELIMITED BY SIZE
                                 INTO INI-REG WRITE INI-REG
                END-IF
                IF   CWMAIL-PASSWORD NOT =  SPACES
                     MOVE SPACES TO INI-REG
                     STRING "Password="
                            CWMAIL-PASSWORD DELIMITED BY SIZE
                                 INTO INI-REG WRITE INI-REG
                END-IF
                IF   CWMAIL-PORT NOT =  SPACES
                     MOVE SPACES TO INI-REG
                     STRING "SmtpPort="
                            CWMAIL-PORT DELIMITED BY SIZE
                                 INTO INI-REG WRITE INI-REG
                END-IF
                IF   CWMAIL-SSL
                     WRITE INI-REG FROM "SSL=Yes"
                END-IF
                IF   CWMAIL-AUTHENTICATION
                     WRITE INI-REG FROM "Authentication=Yes"
                END-IF
                IF   CWMAIL-SERVER   = SPACES
                AND  CWMAIL-USER     = SPACES
                AND  CWMAIL-PASSWORD = SPACES
                AND  CWMAIL-PORT     = SPACES
                AND  CWMAIL-SSL-FLAG = SPACE
                AND  CWMAIL-AUT-FLAG = SPACE
                     IF  CWMAIL-FROM = SPACES
                         WRITE INI-REG FROM "Sender=relator@outlook.com"
                     END-IF
                     WRITE INI-REG FROM "SmtpServer=smtp.live.com"
                     WRITE INI-REG FROM "User=relator@outlook.com"
                     WRITE INI-REG FROM "Password=COBOLware"
                     WRITE INI-REG FROM "SmtpPort=587"
                     WRITE INI-REG FROM "SSL=Yes"
                     WRITE INI-REG FROM "Authentication=Yes"
                END-IF
           END-IF

           IF   CWMAIL-REPORT NOT = SPACES
                MOVE SPACES TO INI-REG
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     IF CWMAIL-50
                        IF CWMAIL-ATTACHES-50 = SPACES
                           STRING "$top->attach(Path=>'"
                             DELIMITED BY SIZE
                            CWMAIL-REPORT DELIMITED BY SPACE
                            "');" DELIMITED BY SIZE
                                 INTO INI-REG
                           WRITE INI-REG
                        END-IF
                     ELSE
                        IF CWMAIL-ATTACHES = SPACES
                           STRING "$top->attach(Path=>'"
                             DELIMITED BY SIZE
                            CWMAIL-REPORT DELIMITED BY SPACE
                            "');" DELIMITED BY SIZE
                                 INTO INI-REG
                           WRITE INI-REG
                        END-IF
                     END-IF
                ELSE
                     IF  UNICO = 1
                         STRING "InFile="    DELIMITED BY SIZE
                                CWMAIL-REPORT DELIMITED BY SPACE
                                     INTO INI-REG
                         PERFORM CHECK-PATH
                     ELSE
                         MOVE SPACES TO LB-INFILE
                         PERFORM VARYING I FROM LENGTH OF CWMAIL-REPORT
                                 BY -1
                                 UNTIL I = 0
                                    OR CWMAIL-REPORT (I: 1) = "\" OR "/"
                                       CONTINUE
                         END-PERFORM
                         ADD 1 TO I
                         STRING POSTDIR DELIMITED BY SPACE
                                BARRA   DELIMITED BY SIZE
                                CWMAIL-REPORT (I: ) DELIMITED BY SPACE
                                INTO LB-INFILE
                         CALL "CBL_COPY_FILE" USING CWMAIL-REPORT
                                                    LB-INFILE
                         STRING "InFile="    DELIMITED BY SIZE
                                LB-INFILE    DELIMITED BY SPACE
                                     INTO INI-REG
                         PERFORM CHECK-PATH
                     END-IF
                     WRITE INI-REG
                     WRITE INI-REG FROM "OutPutFormat=PDF"
                END-IF
           END-IF

           IF (CWMAIL-50 AND (CWMAIL-ATTACHES-50 NOT = SPACES))
           OR (((NOT CWMAIL-50) AND (CWMAIL-ATTACHES NOT = SPACES)))
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     MOVE 20        TO E
                     MOVE "$top->attach(Path=>'" TO INI-REG
                     IF   CWMAIL-REPORT NOT = SPACES
                          MOVE CWMAIL-REPORT TO INI-REG (E: )
                          PERFORM VARYING E FROM 1 BY 1
                                  UNTIL INI-REG (E: 1) = SPACE
                                  CONTINUE
                          END-PERFORM
                          MOVE ";" TO INI-REG (E: 1)
                          ADD  1   TO E
                     END-IF
                ELSE
                     MOVE 8         TO E
                     MOVE "Attach=" TO INI-REG
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                        IF (CWMAIL-50
                            AND (CWMAIL-ATTACH-50 (I) NOT = SPACES))
                        OR (NOT CWMAIL-50
                            AND (CWMAIL-ATTACH (I) NOT = SPACES))
                             IF I > 1
                                 ADD 1 TO E
                             END-IF
                             IF CWMAIL-50
                                MOVE CWMAIL-ATTACH-50 (I) TO ATTACH
                             ELSE
                                MOVE CWMAIL-ATTACH    (I) TO ATTACH
                             END-IF
                             INSPECT ATTACH CONVERTING '0' TO X'01'
                             EXEC COBOLware Pack
                                  String ATTACH
                                  WIDTH T
                             END-EXEC
                             INSPECT ATTACH CONVERTING X'01' TO '0'
                             MOVE ATTACH (1: T) TO INI-REG (E: T)
                             ADD  T             TO E
                             IF   CWUNIX-ON
                             AND  I < 10
                                ADD 1 TO I
                                IF (CWMAIL-50
                                AND (CWMAIL-ATTACH-50 (I) NOT = SPACES))
                                  OR (NOT CWMAIL-50
                                  AND (CWMAIL-ATTACH (I) NOT = SPACES))
                                       ADD 1 TO E
                                END-IF
                                SUBTRACT 1 FROM I
                             END-IF
                        END-IF
                END-PERFORM
                IF   CWUNIX-ON
                AND  MAILPATH = SPACES
                     ADD 1 TO E
                     MOVE "');" TO INI-REG (E: )
                END-IF
                WRITE INI-REG
           END-IF

           IF   CWUNIX-ON
           AND  MAILPATH = SPACES
                WRITE INI-REG FROM "open MAIL, '| /usr/lib/sendmail -t -
      -         "oi -oem' or die 'open: $!';"
                WRITE INI-REG FROM "$top->print(\*MAIL);"
                WRITE INI-REG FROM "close MAIL;"
           END-IF
           CLOSE INI

           IF  MAILPATH = SPACES
               IF  CWUNIX-ON
                   CALL "system" USING CMDLINE
                   delete file INI
               ELSE
                   IF   CWUNIX-DOS16
                   OR   CWUNIX-WIN16
                        DISPLAY CMDLINE UPON COMMAND-LINE
                        CALL X"91" USING RESULTADO X"23" X"00"
                   ELSE
                        IF  UNICO = 0
                            MOVE 0 TO CMDSHOW
                        ELSE
                            MOVE 7 TO CMDSHOW
                        END-IF
                        CALL WIN32 "WinExec"
                                USING BY REFERENCE CMDLINE
                                      BY VALUE     CMDSHOW
                                      RETURNING    CMDSTATUS
                   END-IF
                   IF  UNICO = 0
                       PERFORM TEST AFTER UNTIL CMDSHOW = 0
                               CALL "CBL_DELETE_DIR" USING POSTDIR
                                    RETURNING CMDSHOW
                       END-PERFORM
                   END-IF
               END-IF
           END-IF.

       FIM-ENVIA. EXIT.

       GRAVA-SMTP.

          STRING "Sender=" CWCONF-E-MAIL
                            DELIMITED BY SIZE
             INTO INI-REG
          WRITE INI-REG
          IF  (CWMAIL-SERVER   NOT =  SPACES)
          AND (CWMAIL-USER     NOT =  SPACES)
          AND (CWMAIL-PASSWORD NOT =  SPACES)
               MOVE 1 TO SMTP
               move spaces to ini-reg
               STRING "SmtpServer=" CWMAIL-SERVER
                                DELIMITED BY SIZE
                           INTO INI-REG WRITE INI-REG
               move spaces to ini-reg
               STRING "User=" CWMAIL-USER
                                DELIMITED BY SIZE
                           INTO INI-REG WRITE INI-REG
               move spaces to ini-reg
               STRING "Password=" CWMAIL-PASSWORD
                                DELIMITED BY SIZE
                           INTO INI-REG WRITE INI-REG
               IF CWMAIL-PORT NOT = SPACES
                  move spaces to ini-reg
                  STRING "SmtpPort=" CWMAIL-PORT
                                     DELIMITED BY SIZE
                                INTO INI-REG WRITE INI-REG
               END-IF
               IF   CWMAIL-SSL
                    WRITE INI-REG FROM "SSL=Yes"
               END-IF
               IF   CWMAIL-AUTHENTICATION
                    WRITE INI-REG FROM "Authentication=Yes"
               END-IF
          ELSE
               IF   (CWCONF-SMTP NOT = SPACES)
               AND  (CWCONF-SMTP (1: 1) NOT = X"00")
                    move spaces to ini-reg
                    STRING "SmtpServer=" CWCONF-SMTP
                                     DELIMITED BY SIZE
                                INTO INI-REG WRITE INI-REG
                    move spaces to ini-reg
                    STRING "User=" CWCONF-E-MAIL
                                 DELIMITED BY SIZE
                            INTO INI-REG WRITE INI-REG
                    IF (CWCONF-SMTP-PASSWORD NOT = SPACES)
                    AND(CWCONF-SMTP-PASSWORD (1: 1) NOT = X"00")
                    AND (CWCONF-SMTP-SIZE NOT = 32)
                        CALL "CWCODE" USING "D"
                                      CWCONF-SMTP-SIZE
                                      CWCONF-SMTP-FATOR
                                      CWCONF-SMTP-PASSWORD
                    END-IF
                    move spaces to ini-reg
                    STRING "Password=" CWCONF-SMTP-PASSWORD
                           DELIMITED BY SIZE
                                  INTO INI-REG WRITE INI-REG
                    IF (CWCONF-SMTP-PORT NOT = SPACES)
                    AND (CWCONF-SMTP-PORT(1:1) NOT = X'00')
                       move spaces to ini-reg
                       STRING "SmtpPort=" CWCONF-SMTP-PORT
                                          DELIMITED BY SIZE
                                     INTO INI-REG WRITE INI-REG
                    END-IF
                    IF   CWCONF-SSL = '1'
                         WRITE INI-REG FROM "SSL=Yes"
                    END-IF
                    IF   CWCONF-AUTENTICACAO = '1'
                         WRITE INI-REG FROM "Authentication=Yes"
                    END-IF
               ELSE
                    WRITE INI-REG FROM "SmtpServer=smtp.live.com"
                    WRITE INI-REG FROM "User=relator@outlook.com"
                    WRITE INI-REG FROM "Password=COBOLware"
                    WRITE INI-REG FROm "SmtpPort=587"
                    WRITE INI-REG FROM "SSL=Yes"
                    WRITE INI-REG FROM "Authentication=Yes"
               END-IF
          END-IF.

       FIM-GRAVA-SMTP. EXIT.

       CHECK-PATH.

           IF  MAILPATH NOT = SPACES
               MOVE SPACES TO CMDLINE
               ADD 1 TO SEQ2
               MOVE SPACES TO NEW
               STRING      MAILPATH     DELIMITED BY SPACE
                           BARRA "poster-" CWGETS-TASK "-" SEQ2 ".lst"
                                        DELIMITED BY SIZE
                                 INTO NEW
               IF   CWUNIX-ON
                    STRING "mv -f "     DELIMITED BY SIZE
                           INI-REG (8:) DELIMITED BY SPACE
                           " "          DELIMITED BY SIZE
                           NEW          DELIMITED BY SPACE
                    INTO CMDLINE
               ELSE
                    STRING "move /y "   DELIMITED BY SIZE
                           INI-REG (8:) DELIMITED BY SPACE
                           " "          DELIMITED BY SIZE
                           NEW          DELIMITED BY SPACE
                    INTO CMDLINE
               END-IF
               SET CWEXEC-HIDE  TO TRUE
               MOVE CMDLINE     TO CWEXEC-COMMAND
               CALL "CWEXE2" USING PARAMETROS-CWEXEC
               PERFORM VARYING I FROM LENGTH NEW BY -1 UNTIL I = 0
                        OR NEW (I:1) = '\' OR '/'
                        CONTINUE
               END-PERFORM
               ADD 1 TO I
               MOVE NEW (I:) TO INI-REG (8:)
           END-IF.

       FIM-CHECK-PATH. EXIT.
       END PROGRAM CWMAIL.
