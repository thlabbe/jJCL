//JPURGRE  JOB ENCHAIN,'Purge des fichiers REELLE',CLASS=Z,MSGCLASS=9,
//*          RESTART=REEL
//*         RESTART=JOB19
//*----------------------------------------------------------------
//* Purge des répértoires
//*     Hors donnees sources
//*----------------------------------------------------------------
//STEPREEL  EXEC PGM=BAT
DEL /Q F:\PREMALLIANCE\REELLE\Ano\*.*
DEL /Q /S F:\Premalliance\REELLE\CIBLE\*.*
DEL /Q F:\PREMALLIANCE\REELLE\CPT\*.*
DEL /Q E:\Premalliance\REELLE\INTER\*.*
DEL /Q F:\Premalliance\REELLE\TRACE\*.*
/*
