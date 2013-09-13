//JPRPTF05 JOB UTI00TX0,'EXEC PPRPTF05',CLASS=Y,MSGCLASS=Y,
//*         RESTART=DEL0501,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.APICIL.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0501  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SZENTRCG DD DSN=&MIG.PTF05.ZENTRCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENADH DD DSN=&MIG.PTF05.FPTFADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER ZENTRCG
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* ZENTRCG SORT ZENTRCG-ID-PERS (4:9) ASC [5:9]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&TAB.ZENTRCG
//COPY     DD DISP=SHR,DSN=&CPYSRC.ZENTRCG
//SORTOUT  DD DSN=&MIG.PTF05.ZENTRCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(5,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENADH
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TRENADH SORT TRENADH-ID-TECH-UR-ADHR (1:9) ASC [1:9]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&TAB.FPTFADH
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPTFADH
//SORTOUT  DD DSN=&MIG.PTF05.FPTFADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STBGPPTF DD DSN=&CIB.TBGPPTF,
//*STBGPPTF DD DSN=&MIG.PTF05.TBGPPTF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZZENTRCG DD DSN=&RED.PTF05.ZENTRCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPTFADH DD DSN=&RED.PTF05.FPTFADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTBGPPTF DD DSN=&RED.PTF05.TBGPPTF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPTF05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPTF05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPTF05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRPTF05 EXEC PGM=PPRPTF05
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPTF05,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPTF05,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRPTF05,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//SZENTRCG DD DISP=SHR,DSN=&MIG.PTF05.ZENTRCG
//SFPTFADH DD DISP=SHR,DSN=&MIG.PTF05.FPTFADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STBGPPTF DD DSN=&CIB.TBGPPTF,
//*STBGPPTF DD DSN=&MIG.PTF05.TBGPPTF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZZENTRCG DD DSN=&RED.PTF05.ZENTRCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZFPTFADH DD DSN=&RED.PTF05.FPTFADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTBGPPTF DD DSN=&RED.PTF05.TBGPPTF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*
//* FIN DU JCL DE MIGRATION
