//JLIVIND  JOB UTI00TX0,'PR LIV IND',CLASS=Z,MSGCLASS=X,
//*        RESTART=SORTTR04,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*Batch concatenantion Export compteur
//*----------------------------------------------------------------
//STEPBAT  EXEC PGM=BAT
CALL &LIVBAT IND BREF &SOURCE
