//JEXEBAC  JOB UTI00TX0,'PR BAC',CLASS=Z,MSGCLASS=X
//*          RESTART=SORT0045,
//*         RESTART=PVHIND05,
//*         RESTART=SORT1009,
//*         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//* JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//* MEMBER   INCLUDE MEMBER=$$VHE
//*----------------------------------------------------------------
//**    LANCEMENT DES PROGRAMMES
//** BAC ==>
//*Bref-059 - AJo le 20/08/2013 - modification de la chaine BAC => voir JEXEBREF !
//*JOB09  EXEC JCL=J9PRAD1
//*JOB10  EXEC JCL=J9PRAD2
//*JOB10B EXEC JCL=JCVADH01
//*JOB11  EXEC JCL=J9PRAUE
//*JOB11B EXEC JCL=JCVAUE01
//*
//*
//* FIN DU JCL DE MIGRATION