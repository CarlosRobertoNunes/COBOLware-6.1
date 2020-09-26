       WORKING-STORAGE SECTION.
       77  I PIC 99 VALUE 0.

       COPY CWACOR.

       LINKAGE SECTION.

       01 PARAMETROS-G3ACOR.
          05 G3ACOR-CORES OCCURS 11.
             10 G3ACOR-F                   PIC  9(002).
             10 G3ACOR-B                   PIC  9(002).
          05 G3ACOR-FUNCAO                 PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-G3ACOR.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                   PERFORM UNTIL G3ACOR-F (I) < 8
                           SUBTRACT 7 FROM G3ACOR-F (I)
                   END-PERFORM
                   PERFORM UNTIL G3ACOR-B (I) < 8
                           SUBTRACT 7 FROM G3ACOR-B (I)
                   END-PERFORM
                   MOVE G3ACOR-F (I) TO CWACOR-F (I)
                   MOVE G3ACOR-B (I) TO CWACOR-B (I)
           END-PERFORM

           CALL "CWACOR" USING PARAMETROS-CWACOR

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                   MOVE CWACOR-F (I) TO G3ACOR-F (I)
                   MOVE CWACOR-B (I) TO G3ACOR-B (I)
           END-PERFORM
