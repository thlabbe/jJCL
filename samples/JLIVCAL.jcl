//JBATLIV  JOB UTI00TX0,'BATCH PREPARATION FICHIER',CLASS=Z,MSGCLASS=9
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.CREPA.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*Batch de livraison
//*        ATTENTION, il faut changer la date
//*----------------------------------------------------------------
//STEPBAT1  EXEC PGM=BAT
SET VDATE=%DATE:~6,4%%DATE:~3,2%%DATE:~0,2%
SET LOT=CAL
SET DOSSIER=%LOT%%VDATE%
COPY "C:\Projets\4_Premalliance\modèle\Livraison" "C:\Projets\4_Premalliance\Livraisons\%DOSSIER%"

7za a "C:\Projets\4_Premalliance\Livraisons\%DOSSIER%.zip" "C:\Projets\4_Premalliance\Livraisons\%DOSSIER%"
