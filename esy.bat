@Echo Off
If Not Exist %1.esy Echo N�o encontrado: %1.esy
If Not Exist %1.esy GoTo :Fim
kedit %1.esy (profile esy.kex
call co %1
:Fim
