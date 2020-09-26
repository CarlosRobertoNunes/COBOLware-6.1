       LINKAGE SECTION.

       01  FUNCAO                          PIC  X(001).
       01  OBS                             PIC  X(035).

       PROCEDURE DIVISION USING FUNCAO OBS.

           CALL "CWLOGW" USING FUNCAO OBS.
