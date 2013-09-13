//J9VHCR3  JOB UTI00TX0,'VH GRECCO G02-CRE',CLASS=Z,MSGCLASS=X,         JOB00836
//*        RESTART=DEL3402,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHT
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE99
//*----------------------------------------------------------------
//DEL9901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPCREAS  DD DSN=&MIG.CRE99.PCREAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCRENS  DD DSN=&MIG.CRE99.PCRENS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER PCREAS
//*----------------------------------------------------------------
//SORT9901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPCREAS SORT AS-PCRE-IDTECADH ASC [16:20—
//* SPCREAS SORT AS-PCRE-NOORDADS ASC [36:3—
//* SPCREAS SORT AS-PCRE-NOORDUEC ASC [39:5—
//* SPCREAS SORT AS-PCRE-TYINS ASC [44:1—
//* SPCREAS SORT AS-PCRE-NOINS ASC [45:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=PIHURM.MOVE.VH.CIBLE.ECH.G19.PCRE
//SORTIN   DD DISP=SHR,DSN=PIHURM.MOVE.VH.CIBLE.ECH.G19T.PCRE
//SORTOUT  DD DSN=&MIG.CRE99.PCREAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               44,1,CH,A,
               45,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PCRENS
//*----------------------------------------------------------------
//SORT9901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPCRENS SORT NS-PCRE-IDTECADH ASC [16:20—
//* SPCRENS SORT NS-PCRE-NOORDADS ASC [36:3—
//* SPCRENS SORT NS-PCRE-NOORDUEC ASC [39:5—
//* SPCRENS SORT NS-PCRE-TYINS ASC [44:1—
//* SPCRENS SORT NS-PCRE-NOINS ASC [45:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PCRE
//SORTOUT  DD DSN=&MIG.CRE99.PCRENS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               44,1,CH,A,
               45,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL9902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKPCRE  DD DSN=&MIG.CRE99.WKPCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL9903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE99
//*----------------------------------------------------------------
//PVHCRE99 EXEC PGM=PVHCRE99,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SPCREAS  DD DISP=SHR,DSN=&MIG.CRE99.PCREAS
//SPCRENS  DD DISP=SHR,DSN=&MIG.CRE99.PCRENS
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKPCRE  DD DSN=&MIG.CRE99.WKPCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
