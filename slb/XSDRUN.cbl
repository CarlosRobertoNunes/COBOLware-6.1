       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDRUN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Runtime modulo principal                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SETUP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-SETUP.

       DATA DIVISION.
       FILE SECTION.

       FD  SETUP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SETUP.

       01  SETUP-REG           PIC  X(255).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 FUNCAO-PROGRAMA         PIC  X(034) VALUE SPACES.
           05 OBS                     PIC  X(035) VALUE SPACES.
           05 I                       PIC  9(003) VALUE 0.
           05 Y                       PIC  9(003) VALUE 0.
           05 SAVE                    PIC  9(001) VALUE 3.
           05 TECLA                   PIC  9(003) VALUE 0. COPY CWKEYS.
           05 OPERADOR                PIC  X(030).
           05 TASK                    PIC  9(006) VALUE 0.
           05 PROGRAMA                PIC  X(008) VALUE SPACES.
           05 ER-SQL.
              10 FS-SETUP             PIC  X(002) VALUE '00'.
              10 LB-SETUP             PIC  X(255) VALUE 'setup.cnf'.
           05 HOJE                    PIC  9(008) VALUE 0.
           05 HORA                    PIC  X(008) VALUE SPACES.
           05 DATA-DE-HOJE            PIC  X(010) VALUE SPACES.
       77  GLB-WORK                   PIC X(2000) VALUE SPACES.
       77  GLB-CALL                   PIC  X(005) VALUE SPACES.
       77  GLB-REQUEST                PIC  X(001) VALUE SPACES.
       77  GLB-TEACH                  PIC  X(005) VALUE SPACES.
       77  GLB-LSN                    PIC  X(005) VALUE SPACES.
       77  GLB-STN                    PIC  X(017) VALUE SPACES.
       77  GLB-USERCODE               PIC  X(012) VALUE SPACES.
       77  MENSAGEM                   PIC  X(070) VALUE SPACES.
       77  ANTERIOR                   PIC  X(005) VALUE SPACES.
       77  ANTERIOR-OK                PIC  X(005) VALUE SPACES.
       77  VARNAME                    PIC  X(030) VALUE SPACES.

       COPY CWTIME.
       COPY CWGETL.

       LINKAGE SECTION.

       01   LK-ISPEC     PIC X(007).

       PROCEDURE DIVISION USING LK-ISPEC.

           OPEN INPUT SETUP
           IF   FS-SETUP > '09'
                MOVE 'SETUP.CNF' TO LB-SETUP
                OPEN INPUT SETUP
           ELSE
                MOVE 2 TO SAVE
           END-IF
           DISPLAY 'FIREUP-ISPEC' UPON ENVIRONMENT-NAME
           ACCEPT   GLB-CALL      FROM ENVIRONMENT-VALUE
           INSPECT LK-ISPEC CONVERTING LOW-VALUES TO SPACES
           IF LK-ISPEC NOT = SPACES
              MOVE LK-ISPEC TO GLB-CALL
           END-IF
           IF GLB-CALL NOT = SPACES
              MOVE 0 TO SAVE
           END-IF
           PERFORM TEST AFTER UNTIL FS-SETUP > '09'
                READ SETUP
                IF   FS-SETUP < '10'
                     IF SAVE = 0 AND GLB-CALL = SPACES
                        MOVE 1 TO SAVE
                     END-IF
                     IF   SETUP-REG (1: 14) = 'FIREUP-ISPEC; '
                     AND  GLB-CALL = SPACES
                          MOVE SETUP-REG (15:) TO GLB-CALL
                     END-IF
                     PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > LENGTH VARNAME
                                          OR I > LENGTH SETUP-REG
                             IF SETUP-REG (I:1) = ';'
                                DISPLAY VARNAME UPON ENVIRONMENT-NAME
                                MOVE SPACES TO MENSAGEM
                                ACCEPT MENSAGEM FROM ENVIRONMENT-VALUE
                                IF  MENSAGEM = SPACES
                                    PERFORM TEST AFTER
                                       UNTIL SETUP-REG (I:1) NOT = SPACE
                                         OR I > LENGTH SETUP-REG
                                            ADD 1 TO I
                                    END-PERFORM
                                    MOVE SETUP-REG(I:) TO MENSAGEM
                                    DISPLAY MENSAGEM
                                       UPON ENVIRONMENT-VALUE
                                END-IF
                                EXIT PERFORM
                             ELSE
                                ADD 1 TO Y
                                MOVE SETUP-REG (I:1) TO VARNAME(Y:1)
                             END-IF
                     END-PERFORM
                END-IF
           END-PERFORM
           CLOSE SETUP
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     "?"

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 68 WIDTH 5
                     CAPTION " ~Bye "
                     KEY ESC
                     TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 73 WIDTH 5
                     CAPTION " ~Xmt "
                     KEY PAGE-DOWN
                     TAB-OFF
           END-EXEC.

       000-INICIO.

           IF   GLB-CALL = SPACES
                DISPLAY "Ispec inicial ?       (ou BYE)"
                        LINE 10 POSITION 10
                PERFORM UNTIL GLB-CALL NOT = SPACES
                        ACCEPT GLB-CALL LINE 10 POSITION 26 WITH UPDATE
                        ACCEPT TECLA FROM ESCAPE KEY
                        IF ESC
                           STOP RUN
                        END-IF
                        INSPECT GLB-CALL
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                END-PERFORM
                IF   GLB-CALL = "BYE" OR "FIM" OR "END"
                     GOBACK
                END-IF
           ELSE
                MOVE 0 TO SAVE
           END-IF

      *    DISPLAY (1, 1) ERASE
           MOVE GLB-CALL  TO ANTERIOR-OK
           MOVE OPERADOR  TO GLB-STN
           MOVE OPERADOR  TO GLB-USERCODE
           CALL "XSDTASK" USING TASK
           MOVE TASK(2:)  TO GLB-LSN
           EXEC COBOLware Object Drop END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 68 WIDTH 5
                     CAPTION " ~Pg2 "
                     KEY ESC
                     TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE 25 COLUMN 73 WIDTH 5
                     CAPTION " ~Xmt "
                     KEY PAGE-DOWN
                     TAB-OFF
           END-EXEC
           PERFORM TEST AFTER UNTIL GLB-CALL = "BYE"
                                            OR "FIM"
                                            OR "END"
                                            OR "%%%"
                   MOVE GLB-CALL TO ANTERIOR
                   MOVE SPACE    TO GLB-REQUEST
                   CALL GLB-CALL USING GLB-WORK
                                       GLB-CALL
                                       GLB-REQUEST
                                       GLB-TEACH
                                       GLB-LSN
                                       GLB-STN
                                       GLB-USERCODE
                         ON EXCEPTION
                            MOVE SPACES TO MENSAGEM
                            STRING 'Ispec "' DELIMITED BY SIZE
                                   GLB-CALL  DELIMITED BY SPACE
                                   '" n∆o encontrado'
                                             DELIMITED BY SIZE
                                    INTO MENSAGEM
                            EXEC COBOLware Send
                                 Message MENSAGEM
                            END-EXEC
                            IF   ANTERIOR-OK = GLB-CALL
                                 MOVE "%%%" TO GLB-CALL
                            ELSE
                                 MOVE ANTERIOR-OK TO GLB-CALL
                            END-IF
                        NOT ON EXCEPTION
                            MOVE ANTERIOR TO ANTERIOR-OK
                            IF SAVE NOT = 0
                               IF SAVE = 1 OR 3
                                  MOVE 'setup.cnf' TO LB-SETUP
                               END-IF
                               IF SAVE = 2
                                  MOVE 'SETUP.CNF' TO LB-SETUP
                               END-IF
                               OPEN EXTEND SETUP
                               MOVE SPACES TO SETUP-REG
                               STRING 'FIREUP-ISPEC; '
                                       ANTERIOR
                                       DELIMITED BY SIZE
                                 INTO SETUP-REG
                               WRITE SETUP-REG
                               CLOSE SETUP
                               MOVE 0 TO SAVE
                            END-IF
                  END-CALL
                  CANCEL ANTERIOR
                  INSPECT GLB-CALL CONVERTING MINUSCULAS TO MAIUSCULAS
                  IF GLB-CALL NOT = ANTERIOR
                     MOVE "Processamento encerrado" TO FUNCAO-PROGRAMA
                     PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                  IF NOT (GLB-CALL = "BYE" OR "FIM" OR "END" OR "%%%")
                     MOVE SPACES TO FUNCAO-PROGRAMA
                     MOVE GLB-CALL TO PROGRAMA
                     STRING 'XSDRUN: de ' ANTERIOR
                       DELIMITED BY SIZE INTO FUNCAO-PROGRAMA
                     CALL "CWGETU" USING OPERADOR TASK PROGRAMA "#"
                     PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                  END-IF
                  END-IF
           END-PERFORM

           IF   GLB-CALL = "%%%"
                MOVE SPACES TO GLB-CALL
                EXEC COBOLware Object Drop END-EXEC
                GO TO 000-INICIO
           END-IF

           DISPLAY (1, 1) ERASE.

       000-99-FIM. GOBACK.

       130-GRAVA-CWLOGF.

           CALL "CWGETL" USING PARAMETROS-CWGETL

           IF   CWGETL-LOG = 0
                EXIT PARAGRAPH
           END-IF

           PERFORM 131-DATE-TIME THRU 131-99-FIM

           MOVE FUNCAO-PROGRAMA    TO OBS
           CALL "CWLOGW" USING "#" OBS.

       130-99-FIM. EXIT.

       131-DATE-TIME.

           SET CWTIME-REVERSED     TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL  TO HOJE
           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL  TO CWTIME-TIME
           SET CWTIME-EDIT         TO TRUE
           MOVE CWTIME-DATE-FINAL  TO CWTIME-DATE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED TO DATA-DE-HOJE
           MOVE CWTIME-TIME-EDITED TO HORA.

       131-99-FIM. EXIT.

       END PROGRAM XSDRUN.
