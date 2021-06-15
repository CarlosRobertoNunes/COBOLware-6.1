@Echo Off
cobol %1,,%1.lst xnim;
if errorlevel 12 kedit %1.lst (nolock
if exist %1.int del exist %1.int
if exist %1.obj del exist %1.obj
if exist %1.idy del exist %1.idy
