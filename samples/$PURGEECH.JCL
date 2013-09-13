//JPURGEC  JOB ENCHAIN,'Purge des fichiers ECHANTILLON',CLASS=Z,MSGCLASS=9,
//*          RESTART=ECHANTILLON
//*         RESTART=JOB19
//*----------------------------------------------------------------
//* Purge des répértoires
//*     Hors donnees sources
//*----------------------------------------------------------------
//STEPECH  EXEC PGM=BAT
DEL /Q F:\PREMALLIANCE\Ech\Ano\*.*
DEL /Q /S F:\Premalliance\Ech\CIBLE\*.*
DEL /Q F:\PREMALLIANCE\Ech\CPT\*.*
DEL /Q E:\Premalliance\Ech\INTER\*.*
DEL /Q F:\Premalliance\Ech\TRACE\*.*
/*
