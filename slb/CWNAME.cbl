       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWNAME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/04/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Muda o nome do arquivo no diretorio         *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                 COMP-X PIC  9(002) VALUE 0.
           05 T1                COMP-X PIC  9(002) VALUE 0.
           05 T2                COMP-X PIC  9(002) VALUE 0.
           05 TESTE                    PIC  X(001) VALUE SPACE.
           05 ER-FILE.
              10 RT-NUMBER-1    COMP-X PIC  9(002).
              10 RT-NUMBER-2    COMP-X PIC  9(002).
              10 LB-FILE               PIC  X(255) VALUE SPACES.

       LINKAGE SECTION.

       01 OLD-FILENAME                 PIC  X.
       01 NEW-FILENAME                 PIC  X.

       PROCEDURE DIVISION USING OLD-FILENAME NEW-FILENAME.

           PERFORM VARYING T1
                      FROM 1 BY 1 UNTIL OLD-FILENAME(T1:1) = SPACE
           END-PERFORM
           PERFORM VARYING T2
                      FROM 1 BY 1 UNTIL NEW-FILENAME(T2:1) = SPACE
           END-PERFORM
           CALL "FS_RENAME_FILE" USING OLD-FILENAME(1:T1)
                                       NEW-FILENAME(1:T2)
                             RETURNING RETURN-CODE

           IF   RETURN-CODE NOT EQUAL ZERO
                CALL "CBL_RENAME_FILE" USING OLD-FILENAME(1:T1)
                                             NEW-FILENAME(1:T2)
                             RETURNING RETURN-CODE
           END-IF

           IF   RETURN-CODE NOT EQUAL ZERO
                MOVE 57           TO RT-NUMBER-1
                MOVE RETURN-CODE  TO RT-NUMBER-2
                MOVE OLD-FILENAME TO LB-FILE
                PERFORM TEST AFTER VARYING I FROM LENGTH LB-FILE BY -1
                                   UNTIL TESTE NOT = SPACE
                                      OR I = 0
                        MOVE LB-FILE (I: 1) TO TESTE
                END-PERFORM
                IF   I < 44
                     ADD  2        TO I
                     MOVE "(Ren)"  TO LB-FILE (I: )
                END-IF
                CALL "CWISAM"  USING ER-FILE
                MOVE 0            TO RETURN-CODE
           ELSE
                MOVE 30           TO RT-NUMBER-1
                                     RT-NUMBER-2.

           EXIT PROGRAM.

       END PROGRAM CWNAME.
