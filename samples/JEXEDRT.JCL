//JEXEDRT  JOB UTI00TX0,'PR DROITS',CLASS=Z,MSGCLASS=X
//*          RESTART=SORT0045,
//*         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//* JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//* MEMBER   INCLUDE MEMBER=$$VHE
//*----------------------------------------------------------------
//**    LANCEMENT DES PROGRAMMES
//JOB01  EXEC JCL=J9PRDNS
//JOB01B EXEC JCL=JCVDNS01
//JOB02  EXEC JCL=J9PRADR
//JOB02B EXEC JCL=JCVADR01
//JOB03  EXEC JCL=J9PRTCI
//JOB03B EXEC JCL=JCVTCI01
//JOB04  EXEC JCL=J9PRIBL
//JOB01B EXEC JCL=JCVIBL01
//JOB04  EXEC JCL=J9PRBFD
//JOB04B EXEC JCL=JCVBFD01
//*
//* FIN DU JCL DE MIGRATION