//JCJTXARR   JOB UTI00TX0,'MEF TXARRCO',CLASS=Z,MSGCLASS=9,
//         RESTART=DEL0001,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.APICIL.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DU FICHIER TACPCTR
//*----------------------------------------------------------------
//DEL0001      EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STXARRCO DD DSN=&PARTXA.TXARRCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*
//*----------------------------------------------------------------
//*TRI DU FICHIER TXARRCO
//*----------------------------------------------------------------
//SORT0001 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TXARRCO
//SORTIN   DD DISP=SHR,DSN=&PARTXA.TXARRCO..CSV
//SORTOUT  DD DSN=&PARTXA.TXARRCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
  *EXPAND=CSV
  *CSVENTETE=NON
  *LINESEQIN
  *FIXEOUT
/*