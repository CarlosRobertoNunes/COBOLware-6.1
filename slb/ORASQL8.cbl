       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ORASQL8.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/02/2013.
       SECURITY.      *************************************************
                      *                                               *
                      * Ajusta chamada aos componentes da Oracle      *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 FL-ORACLE                PIC 9(001) VALUE 0.
           05 ANO                      PIC 9(002) VALUE 1.
           05 HOJE                     PIC X(002) VALUE SPACES.
           05 ORASQL                              VALUE SPACES.
              10                       PIC X(006).
              10 ORACLEVER             PIC 9(002).
              10                       PIC X(247).
           05 TEST-LOAD.
              15 LOAD-ORASQL     USAGE PROCEDURE-POINTER.

       01  OS-INFORMATION.
           05 OS-PARAMETER-SIZE     PIC XX COMP-X VALUE 13.
           05 OS-TYPE               PIC 9(2) COMP-X.
           05 OS-VERSION            PIC XXXX COMP-X.
           05 OS-DBCS-SUPPORT       PIC X COMP-X.
           05 OS-CHAR-CODING        PIC 9(2) COMP-X.
           05 OS-COUNTRY-CODE       PIC XX COMP-X.
           05 OS-CODE-PAGE          PIC XX COMP-X.

       COPY CWUNIX.

       PROCEDURE DIVISION.

       000-INICIO.

           ON 1
              PERFORM CARREGA-DLL THRU FIM-CARREGA-DLL.

       000-99-FIM. GOBACK.

       CARREGA-DLL.

           DISPLAY 'ORASQL' UPON ENVIRONMENT-NAME
           ACCEPT  ORASQL   FROM ENVIRONMENT-VALUE
           CALL "CBL_GET_OS_INFO" USING OS-INFORMATION

           IF   ORASQL NOT = SPACES
                IF   OS-TYPE < 3
                OR  (OS-TYPE = 5 OR 131)
                   SET LOAD-ORASQL TO ENTRY ORASQL
                   IF (TEST-LOAD NOT = LOW-VALUES)
                   AND (TEST-LOAD NOT = X'90E30160')
                       GOBACK
                   END-IF
                ELSE
                   CALL ORASQL
                     ON EXCEPTION
                        CONTINUE
                     NOT ON EXCEPTION
                         GOBACK
                     END-CALL
                END-IF
           END-IF

           IF   OS-TYPE < 3
           OR  (OS-TYPE = 5 OR 131)
                MOVE 'ORASQL00.DLL' TO ORASQL
           ELSE
                MOVE 'ORASQL00'     TO ORASQL
           END-IF

           MOVE 0 TO FL-ORACLE
           ACCEPT HOJE FROM DATE
           PERFORM VARYING ORACLEVER FROM 10
                               BY 1 UNTIL ORACLEVER > 98
                                       OR HOJE < ANO
                   IF   OS-TYPE < 3
                   OR  (OS-TYPE = 5 OR 131)
                       SET LOAD-ORASQL TO ENTRY ORASQL
                       IF (TEST-LOAD NOT = LOW-VALUES)
                       AND (TEST-LOAD NOT = X'90E30160')
                           MOVE 1 TO FL-ORACLE
                           EXIT PERFORM
                       END-IF
                   ELSE
                       CALL ORASQL
                            ON EXCEPTION
                               CONTINUE
                            NOT ON EXCEPTION
                                MOVE 1 TO FL-ORACLE
                                EXIT PERFORM
                       END-CALL
                   END-IF
                   ADD 3 TO ANO
           END-PERFORM

           IF FL-ORACLE = 0
              IF   OS-TYPE < 3
              OR  (OS-TYPE = 5 OR 131)
                  SET LOAD-ORASQL TO ENTRY 'ORASQL9.DLL'
                  IF (TEST-LOAD NOT = LOW-VALUES)
                  AND (TEST-LOAD NOT = X'90E30160')
                      MOVE 1 TO FL-ORACLE
                  END-IF
              ELSE
                  CALL 'ORASQL9'
                       ON EXCEPTION
                          CONTINUE
                       NOT ON EXCEPTION
                           MOVE 1 TO FL-ORACLE
                  END-CALL
              END-IF
           END-IF

           IF FL-ORACLE = 0
              IF   OS-TYPE < 3
              OR  (OS-TYPE = 5 OR 131)
                  SET LOAD-ORASQL TO ENTRY 'ORASQL8.DLL'
                  IF (TEST-LOAD NOT = LOW-VALUES)
                  AND (TEST-LOAD NOT = X'90E30160')
                      MOVE 1 TO FL-ORACLE
                  END-IF
           END-IF

           IF FL-ORACLE = 0
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
                ON EXCEPTION
                   display "Cliente Oracle nÆo instalado/configurado"
                NOT ON EXCEPTION
                    EXEC COBOLWARE Send
                      MESSAGE "Cliente Oracle nÆo instalado/configurado"
                    END-EXEC
              END-CALL
              STOP RUN
           END-IF.

       FIM-CARREGA-DLL. EXIT.

       END PROGRAM ORASQL8.
