       LINKAGE SECTION.

       01  OLD                          PIC  X(050).
       01  NEW                          PIC  X(050).

       PROCEDURE DIVISION USING OLD NEW.

           CALL "CWNAME" USING OLD NEW.
