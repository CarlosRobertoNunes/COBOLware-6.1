       COPY SP2 REPLACING ==X(80).==    BY ==X(300).==
            ==100.== BY ==0 TO 4000 DEPENDING ON SP2-MD-OPTION-CNT.==
                          ==+80.==      BY ==+300.==
      *                   ==OCCURS 20== BY ==OCCURS 89==
                          ==OCCURS 20== BY ==OCCURS 140==
                          ==2000==      BY ==20000==
                          ==OCCURS 10== BY ==OCCURS 2000==
         ==SP2-MDO-TEXT PIC X(30)== BY ==SP2-MDO-TEXT PIC X(34)==
         ==SP2-FO-NAME         PIC X(30)==
          BY ==SP2-FO-NAME         PIC X(60)==.

