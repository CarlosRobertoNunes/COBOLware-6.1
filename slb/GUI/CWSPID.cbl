      $Set Remove"OBJECT" Remove"OBJECT-ID"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSPID.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Identificador de objetos                     *
                      *                                               *
                      *                                           SP2 *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT FREES  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FREES-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-FREES.

           SELECT OBJECT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJECT-CHAVE
                  ALTERNATE KEY IS OBJECT-FREE = OBJECT-FLAG
                                                 OBJECT-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-OBJECT.

       DATA DIVISION.
       FILE SECTION.

       FD  FREES
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FREES.

       01  FREES-REG.
           05 FREES-ID PIC S9(4) COMP-5.

       FD  OBJECT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJECT.

       01  OBJECT-REG.
           05 OBJECT-CHAVE.
              10 OBJECT-TYPE             PIC  X(001).
              10 OBJECT-ID               PIC  9(005).
           05 OBJECT-FLAG                PIC  9(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MODO                    PIC  X(3) VALUE SPACES.
           05 SEQUENCIA               PIC S9(4) COMP-5 VALUE 0.
           05 ER-FREES.
              10 FS-FREES             PIC  X(002) VALUE "00".
              10 LB-FREES             PIC  X(255) VALUE "$TEMP/cwspidF".
           05 ER-OBJECT.
              10 FS-OBJECT            PIC  X(002) VALUE "00".
              10 LB-OBJECT            PIC  X(255) VALUE "$TEMP/cwspidO".

       LINKAGE SECTION.

       01  OBJ-ID      PIC S9(4) COMP-5.
       01  ACTION.
           05 TYPE-OBJ PIC  X(1).
           05 OPTION   PIC  X(1).

       PROCEDURE DIVISION USING OBJ-ID ACTION.

       INICIO.

           ON 1
              DISPLAY 'CWSPID' UPON ENVIRONMENT-NAME
              ACCEPT MODO FROM ENVIRONMENT-VALUE
              INSPECT MODO CONVERTING 'seq' TO 'SEQ'.

           IF MODO NOT = 'SEQ' GO TO MULTI.

           ON 1 OPEN I-O FREES.
           IF   OPTION = "I" OR "i"
                MOVE LOW-VALUES TO FREES-REG
                START FREES KEY NOT LESS FREES-ID
                READ FREES NEXT RECORD
                IF   FS-FREES > '09'
                     ADD 1 TO SEQUENCIA
                     MOVE SEQUENCIA TO OBJ-ID
                ELSE
                     MOVE FREES-ID TO OBJ-ID
                     DELETE FREES RECORD
                END-IF
           ELSE
                MOVE OBJ-ID TO FREES-ID
                WRITE FREES-REG
           END-IF
           GO TO FIM.

       MULTI.

           ON 1 OPEN I-O OBJECT.
           IF   OPTION = "I" OR "i"
                MOVE 1           TO OBJECT-FLAG
                MOVE TYPE-OBJ    TO OBJECT-TYPE
                MOVE 0           TO OBJECT-ID
                START OBJECT KEY NOT LESS OBJECT-FREE
                      INVALID KEY
                              PERFORM INSERT
                      NOT INVALID KEY
                          READ OBJECT NEXT RECORD
                          IF   FS-OBJECT < "10"
                          AND  OBJECT-FLAG = 1
                          AND  OBJECT-TYPE = TYPE-OBJ
                               MOVE 0      TO OBJECT-FLAG
                               MOVE OBJECT-ID TO OBJ-ID
                               REWRITE OBJECT-REG
                          ELSE
                               PERFORM INSERT
                          END-IF
                END-START
           ELSE
                MOVE OBJ-ID   TO OBJECT-ID
                MOVE TYPE-OBJ TO OBJECT-TYPE
                READ OBJECT
                IF   FS-OBJECT < "10"
                     MOVE 1 TO OBJECT-FLAG
                     REWRITE OBJECT-REG
                END-IF
           END-IF.

       FIM. GOBACK.

       INSERT.

           MOVE TYPE-OBJ    TO OBJECT-TYPE
           MOVE 32767       TO OBJECT-ID
           START OBJECT KEY NOT GREATER OBJECT-CHAVE
           IF   FS-OBJECT < "10"
                READ OBJECT PREVIOUS RECORD
           END-IF

           IF   FS-OBJECT = "00"
           AND  OBJECT-TYPE = TYPE-OBJ
                ADD 1  TO OBJECT-ID
           ELSE
                MOVE 1 TO OBJECT-ID
           END-IF

           MOVE TYPE-OBJ   TO OBJECT-TYPE
           MOVE 0          TO OBJECT-FLAG
           MOVE OBJECT-ID  TO OBJ-ID
           WRITE OBJECT-REG.

       FIM-INSERT. EXIT.
       END PROGRAM CWSPID.
