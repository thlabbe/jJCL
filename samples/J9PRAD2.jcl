//J9PRAD2  JOB UTI00TX0,'PR BAC    B02-ADH',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL4301,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH15
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADHS DD DSN=&MIG.ADH15.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH2   DD DSN=&MIG.ADH15.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADHS SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STABADHS SORT ADHSADH-C-ADH ASC [95:9—
//* STABADHS SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADH15.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-GRPT-GEST-S ASC [1:1—
//* SFADH2 SORT FADH2-C-ADH-S ASC [2:9—
//* SFADH2 SORT FADH2-C-ORDRE-S ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH15.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH15  DD DSN=&MIG.ADH15.FADH15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.ADH15.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH15.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH15  DD DSN=&MIG.ADH15.FADH15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH15
//*----------------------------------------------------------------
//PPRADH15 EXEC PGM=PPRADH15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADHS DD DISP=SHR,DSN=&MIG.ADH15.TADHADH
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH15.FADH2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH15  DD DSN=&MIG.ADH15.FADH15,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.ADH15.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH15.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH15  DD DSN=&MIG.ADH15.FADH15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH17
//*----------------------------------------------------------------
//DEL1701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH15  DD DSN=&MIG.ADH17.FADH15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SK02COAU DD DSN=&MIG.ADH17.K02COAU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH15
//*----------------------------------------------------------------
//SORT1701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH15 SORT FADH15-C-CONTRAT-AURA ASC [174:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH15
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH15.FADH15
//SORTOUT  DD DSN=&MIG.ADH17.FADH15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(174,17,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER K02COAU
//*----------------------------------------------------------------
//SORT1702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SK02COAU SORT K02-CONT-AURA-C-CONTRAT-AURA ASC [1:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.K02COAUR
//SORTIN   DD DISP=SHR,DSN=&SRC.K02COAUR
//SORTOUT  DD DSN=&MIG.ADH17.K02COAU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH16  DD DSN=&MIG.ADH17.FADH16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH15  DD DSN=&MIG.ADH17.FADH15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZK02COAU DD DSN=&MIG.ADH17.K02COAU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH16  DD DSN=&MIG.ADH17.FADH16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH17
//*----------------------------------------------------------------
//PPRADH17 EXEC PGM=PPRADH17,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH15  DD DISP=SHR,DSN=&MIG.ADH17.FADH15
//SK02COAU DD DISP=SHR,DSN=&MIG.ADH17.K02COAU
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH16  DD DSN=&MIG.ADH17.FADH16,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH15  DD DSN=&MIG.ADH17.FADH15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZK02COAU DD DSN=&MIG.ADH17.K02COAU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH16  DD DSN=&MIG.ADH17.FADH16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH16  DD DSN=&MIG.ADH16.FADH16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH16
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH16 SORT FADH16-ID-TECH-UR-ADHR ASC [21:20—
//* SFADH16 SORT FADH16-NO-ORD-ADHS ASC [41:3—
//* SFADH16 SORT FADH16-C-GRPT-GEST ASC [94:1—
//* SFADH16 SORT FADH16-C-ADH ASC [95:9—
//* SFADH16 SORT FADH16-C-ORDRE ASC [104:4—
//* SFADH16 SORT FADH16-C-ID-CONTRAT-S ASC [133:12—
//* SFADH16 SORT FADH16-C-ID-INST ASC [145:7—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH16
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH17.FADH16
//SORTOUT  DD DSN=&MIG.ADH16.FADH16,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A,
               133,12,CH,A,
               145,7,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGACNT DD DSN=&CIB.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCTR DD DSN=&CIB.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZL01INST DD DSN=&MIG.ADH16.L01INST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH16  DD DSN=&MIG.ADH16.FADH16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&MIG.ADH16.TAGACNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.ADH16.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH16
//*----------------------------------------------------------------
//PPRADH16 EXEC PGM=PPRADH16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//L01INST  DD DISP=SHR,DSN=&SRC.L01INSTI
//TACPCTR  DD DISP=SHR,DSN=&TAB.TACPCTR
//SFADH16  DD DISP=SHR,DSN=&MIG.ADH16.FADH16
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGACNT DD DSN=&CIB.TAGACNT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STCNTCTR DD DSN=&CIB.TCNTCTR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZL01INST DD DSN=&MIG.ADH16.L01INST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH16  DD DSN=&MIG.ADH16.FADH16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACNT DD DSN=&MIG.ADH16.TAGACNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.ADH16.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH41
//*----------------------------------------------------------------
//DEL4101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTCTR DD DSN=&MIG.ADH41.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH2   DD DSN=&MIG.ADH41.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03PCTR DD DSN=&MIG.ADH41.A03PCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT4101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-C-ID-CONTRAT ASC [155:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&MIG.ADH41.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(155,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT4102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-ID-CONTRAT-S ASC [97:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH41.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(97,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03PCTR
//*----------------------------------------------------------------
//SORT4103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03PCTR SORT A03-POPUL-CONTR-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03POPCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03POPCO
//SORTOUT  DD DSN=&MIG.ADH41.A03PCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH41  DD DSN=&MIG.ADH41.FADH41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH42  DD DSN=&MIG.ADH41.FADH42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH44  DD DSN=&MIG.ADH41.FADH44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.ADH41.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH41.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03PCTR DD DSN=&MIG.ADH41.A03PCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH41  DD DSN=&MIG.ADH41.FADH41.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH42  DD DSN=&MIG.ADH41.FADH42.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH44  DD DSN=&MIG.ADH41.FADH44.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH41
//*----------------------------------------------------------------
//PPRADH41 EXEC PGM=PPRADH41,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTCTR DD DISP=SHR,DSN=&MIG.ADH41.TCNTCTR
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH41.FADH2
//SA03PCTR DD DISP=SHR,DSN=&MIG.ADH41.A03PCTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH41  DD DSN=&MIG.ADH41.FADH41,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADH42  DD DSN=&MIG.ADH41.FADH42,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADH44  DD DSN=&MIG.ADH41.FADH44,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.ADH41.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH41.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03PCTR DD DSN=&MIG.ADH41.A03PCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH41  DD DSN=&MIG.ADH41.FADH41.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH42  DD DSN=&MIG.ADH41.FADH42.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH44  DD DSN=&MIG.ADH41.FADH44.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH42
//*----------------------------------------------------------------
//DEL4201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH41  DD DSN=&MIG.ADH42.FADH41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH41
//*----------------------------------------------------------------
//SORT4201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH41 SORT FADH41-ID-TECH-UR-ADHR ASC [1:20—
//* SFADH41 SORT FADH41-NO-ORD-ADHS ASC [21:3—
//* SFADH41 SORT FADH41-NO-ORD-CTR ASC [24:3—
//* SFADH41 SORT FADH41-C-ID-INST ASC [27:7—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH41
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH41.FADH41
//SORTOUT  DD DSN=&MIG.ADH42.FADH41,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,7,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGAIOC DD DSN=&MIG.ADH42.TAGAIOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPADH1  DD DSN=&CIB.FPADH1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.ADH42.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTFININS DD DSN=&MIG.ADH42.TFININS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH41  DD DSN=&MIG.ADH42.FADH41.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAIOC DD DSN=&MIG.ADH42.TAGAIOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADH1  DD DSN=&MIG.ADH42.FPADH1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH42
//*----------------------------------------------------------------
//PPRADH42 EXEC PGM=PPRADH42,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//TFININS  DD DISP=SHR,DSN=&TAB.TFININS
//SFADH41  DD DISP=SHR,DSN=&MIG.ADH42.FADH41
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGAIOC DD DSN=&MIG.ADH42.TAGAIOC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPADH1  DD DSN=&CIB.FPADH1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.ADH42.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH41  DD DSN=&MIG.ADH42.FADH41.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAIOC DD DSN=&MIG.ADH42.TAGAIOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADH1  DD DSN=&MIG.ADH42.FPADH1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH55
//*----------------------------------------------------------------
//DEL5501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGAIOC DD DSN=&MIG.ADH55.TAGAIOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACNT DD DSN=&MIG.ADH55.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAIOC
//*----------------------------------------------------------------
//SORT5501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGAIOC SORT TAGAIOC-ID-TECH-UR-ADHR ASC [1:20—
//* STAGAIOC SORT TAGAIOC-NO-ORD-ADHS ASC [21:3—
//* STAGAIOC SORT TAGAIOC-NO-ORD-CTR ASC [24:3—
//* STAGAIOC SORT TAGAIOC-DT-DEFF-INST-ORIG DSC [27:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAIOC
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH42.TAGAIOC
//SORTOUT  DD DSN=&MIG.ADH55.TAGAIOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,10,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACNT
//*----------------------------------------------------------------
//SORT5502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGACNT SORT TAGACNT-ID-TECH-UR-ADHR ASC [1:20—
//* STAGACNT SORT TAGACNT-NO-ORD-ADHS ASC [21:3—
//* STAGACNT SORT TAGACNT-NO-ORD-CTR ASC [24:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACNT
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACNT
//SORTOUT  DD DSN=&MIG.ADH55.TAGACNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCTAGAIO DD DSN=&CIB.TAGAIOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH55,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH55,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH55,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAIOC DD DSN=&MIG.ADH55.TAGAIOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&MIG.ADH55.TAGACNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTAGAIO DD DSN=&MIG.ADH55.CTAGAIO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH55
//*----------------------------------------------------------------
//PPRADH55 EXEC PGM=PPRADH55,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH55,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH55,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH55,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STAGAIOC DD DISP=SHR,DSN=&MIG.ADH55.TAGAIOC
//STAGACNT DD DISP=SHR,DSN=&MIG.ADH55.TAGACNT
//*--------------<FICHIERS CIBLES>---------------------------------
//SCTAGAIO DD DSN=&CIB.TAGAIOC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAIOC DD DSN=&MIG.ADH55.TAGAIOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACNT DD DSN=&MIG.ADH55.TAGACNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTAGAIO DD DSN=&MIG.ADH55.CTAGAIO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH43
//*----------------------------------------------------------------
//DEL4301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//TRFCPRO  DD DSN=&MIG.ADH43.TRFCPRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH42  DD DSN=&MIG.ADH43.FADH42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRFCPRO TAB_CPRO_CPI_CPN
//*----------------------------------------------------------------
//SORT4301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TCPICPN
//SORTIN   DD DISP=SHR,DSN=&TAB.TCPICPN
//SORTOUT  DD DSN=&MIG.ADH43.TRFCPRO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH42
//*----------------------------------------------------------------
//SORT4302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH42 SORT FADH42-ID-TECH-UR-ADHR ASC [48:20—
//* SFADH42 SORT FADH42-NO-ORD-ADHS ASC [68:3—
//* SFADH42 SORT FADH42-C-CPI ASC [13:3—
//* SFADH42 SORT FADH42-D-DEB-EFFET ASC [16:8—
//* SFADH42 SORT FADH42-C-ID-CONTRAT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH42
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH41.FADH42
//SORTOUT  DD DSN=&MIG.ADH43.FADH42,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(48,20,CH,A,
               68,3,CH,A,
               13,3,CH,A,
               16,8,CH,A,
               24,8,CH,D,
               1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGACAT DD DSN=&CIB.TAGACAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCATCPI DD DSN=&CIB.TCATCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH43  DD DSN=&MIG.ADH43.FADH43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCPI DD DSN=&CIB.TCNTCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRFCPRO DD DSN=&MIG.ADH43.TRFCPRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH42  DD DSN=&MIG.ADH43.FADH42.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACAT DD DSN=&MIG.ADH43.TAGACAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCATCPI DD DSN=&MIG.ADH43.TCATCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH43  DD DSN=&MIG.ADH43.FADH43.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCPI DD DSN=&MIG.ADH43.TCNTCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH43
//*----------------------------------------------------------------
//PPRADH43 EXEC PGM=PPRADH43,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TRFCPRO  DD DISP=SHR,DSN=&MIG.ADH43.TRFCPRO
//SFADH42  DD DISP=SHR,DSN=&MIG.ADH43.FADH42
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGACAT DD DSN=&CIB.TAGACAT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STCATCPI DD DSN=&CIB.TCATCPI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADH43  DD DSN=&MIG.ADH43.FADH43,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STCNTCPI DD DSN=&CIB.TCNTCPI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRFCPRO DD DSN=&MIG.ADH43.TRFCPRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH42  DD DSN=&MIG.ADH43.FADH42.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACAT DD DSN=&MIG.ADH43.TAGACAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCATCPI DD DSN=&MIG.ADH43.TCATCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH43  DD DSN=&MIG.ADH43.FADH43.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH44
//*----------------------------------------------------------------
//DEL4401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH43  DD DSN=&MIG.ADH44.FADH43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH44  DD DSN=&MIG.ADH44.FADH44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH43
//*----------------------------------------------------------------
//SORT4401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH43 SORT FADH43-ID-TECH-UR-ADHR ASC [1:20—
//* SFADH43 SORT FADH43-NO-ORD-ADHS ASC [21:3—
//* SFADH43 SORT FADH43-NO-ORD-CTR ASC [24:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH43
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH43.FADH43
//SORTOUT  DD DSN=&MIG.ADH44.FADH43,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH44
//*----------------------------------------------------------------
//SORT4402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH44 SORT FADH44-ID-TECH-UR-ADHR ASC [21:20—
//* SFADH44 SORT FADH44-NO-ORD-ADHS ASC [41:3—
//* SFADH44 SORT FADH44-NO-ORD-CTR ASC [64:3—
//* SFADH44 SORT FADH44-CONTRAT-FERME ASC [203:1—
//* SFADH44 SORT FADH44-C-CONTRAT DESC [167:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH44
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH41.FADH44
//SORTOUT  DD DSN=&MIG.ADH44.FADH44,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A,
               203,1,CH,A,
               167,3,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH45  DD DSN=&MIG.ADH44.FADH45,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH43  DD DSN=&MIG.ADH44.FADH43.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH44  DD DSN=&MIG.ADH44.FADH44.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH45  DD DSN=&MIG.ADH44.FADH45.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH44
//*----------------------------------------------------------------
//PPRADH44 EXEC PGM=PPRADH44,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH43  DD DISP=SHR,DSN=&MIG.ADH44.FADH43
//SFADH44  DD DISP=SHR,DSN=&MIG.ADH44.FADH44
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH45  DD DSN=&MIG.ADH44.FADH45,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH43  DD DSN=&MIG.ADH44.FADH43.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH44  DD DSN=&MIG.ADH44.FADH44.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH45  DD DSN=&MIG.ADH44.FADH45.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH45
//*----------------------------------------------------------------
//DEL4501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH45  DD DSN=&MIG.ADH45.FADH45,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SK02AURA DD DSN=&MIG.ADH45.K02AURA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH45
//*----------------------------------------------------------------
//SORT4501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH45 SORT FADH45-NO-REFI-CTR-AURA ASC [30:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH45
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH44.FADH45
//SORTOUT  DD DSN=&MIG.ADH45.FADH45,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(30,17,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER K02AURA
//*----------------------------------------------------------------
//SORT4502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SK02AURA SORT K02-CONT-AURA-C-CONTRAT-AURA ASC [1:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.K02COAUR
//SORTIN   DD DISP=SHR,DSN=&SRC.K02COAUR
//SORTOUT  DD DSN=&MIG.ADH45.K02AURA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGALCC DD DSN=&CIB.TAGALCC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH45,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH45,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH45,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH45  DD DSN=&MIG.ADH45.FADH45.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZK02AURA DD DSN=&MIG.ADH45.K02AURA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGALCC DD DSN=&MIG.ADH45.TAGALCC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH45
//*----------------------------------------------------------------
//PPRADH45 EXEC PGM=PPRADH45,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH45,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH45,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH45,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH45  DD DISP=SHR,DSN=&MIG.ADH45.FADH45
//SK02AURA DD DISP=SHR,DSN=&MIG.ADH45.K02AURA
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGALCC DD DSN=&CIB.TAGALCC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH45  DD DSN=&MIG.ADH45.FADH45.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZK02AURA DD DSN=&MIG.ADH45.K02AURA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGALCC DD DSN=&MIG.ADH45.TAGALCC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH18
//*----------------------------------------------------------------
//DEL1801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FGARGAR  DD DSN=&MIG.ADH18.FGARGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPGAR  DD DSN=&MIG.ADH18.TACPGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPCOG  DD DSN=&MIG.ADH18.TACPCOG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPCOM  DD DSN=&MIG.ADH18.TACPCOM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPCOF  DD DSN=&MIG.ADH18.TACPCOF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPZTL  DD DSN=&MIG.ADH18.TACPZTL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TACPCOP  DD DSN=&MIG.ADH18.TACPCOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3SITCO DD DSN=&MIG.ADH18.A3SITCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3GARCO DD DSN=&MIG.ADH18.A3GARCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCTR DD DSN=&MIG.ADH18.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH2   DD DSN=&MIG.ADH18.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FGARGAR
//*----------------------------------------------------------------
//SORT1801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.FGARGAR
//SORTIN   DD DISP=SHR,DSN=&TAB.TGARGAR
//SORTOUT  DD DSN=&MIG.ADH18.FGARGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,5,CH,A)
/*
//*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPGAR
//*----------------------------------------------------------------
//SORT1802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPGAR
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPGAR
//SORTOUT  DD DSN=&MIG.ADH18.TACPGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPCOG
//*----------------------------------------------------------------
//SORT1803 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* TACPCOG SORT TACPCOG-ID-GARA-REF ASC [1:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPCOG
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPCOG
//SORTOUT  DD DSN=&MIG.ADH18.TACPCOG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPCOM
//*----------------------------------------------------------------
//SORT1804 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* TACPCOM SORT TACPCOM-ID-GARA-REF ASC [1:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPCOM
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPCOM
//SORTOUT  DD DSN=&MIG.ADH18.TACPCOM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPCOF
//*----------------------------------------------------------------
//SORT1805 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* TACPCOF SORT TACPCOF-ID-GARA-REF ASC [1:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPCOF
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPCOF
//SORTOUT  DD DSN=&MIG.ADH18.TACPCOF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPZTL
//*----------------------------------------------------------------
//SORT1806 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPZTL
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPZTL
//SORTOUT  DD DSN=&MIG.ADH18.TACPZTL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TACPCOP
//*----------------------------------------------------------------
//SORT1807 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* TACPCOP SORT TACPCOP-ID-GARA-REF ASC [1:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TACPCOP
//SORTIN   DD DISP=SHR,DSN=&TAB.TACPCOP
//SORTOUT  DD DSN=&MIG.ADH18.TACPCOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A3SITCO
//*----------------------------------------------------------------
//SORT1808 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA3SITCO SORT A03-SIT-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA3SITCO SORT A03-SIT-CONTR-C-SIT-CONTR ASC [15:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03SITCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03SITCO
//SORTOUT  DD DSN=&MIG.ADH18.A3SITCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               15,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A3GARCO
//*----------------------------------------------------------------
//SORT1809 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA3GARCO SORT A03-GAR-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA3GARCO SORT A03-GAR-CONTR-C-SIT-CONTR ASC [15:3—
//* SA3GARCO SORT A03-GAR-CONTR-C-GAR ASC [18:5—
//* SA3GARCO SORT A03-GAR-CONTR-C-CADRE ASC [23:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03GARCO
//SORTIN   DD DISP=SHR,DSN=&MIG.A03GARCO
//SORTOUT  DD DSN=&MIG.ADH18.A3GARCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               15,3,CH,A,
               18,5,CH,A,
               23,1,CH,A)
  *MEMSIZE=1000000000
/*
//*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT1810 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-C-ID-CONTRAT ASC [155:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&MIG.ADH18.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(155,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT1811 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-ID-CONTRAT-S ASC [97:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH18.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(97,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH18  DD DSN=&MIG.ADH18.FADH18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFWGFAMD DD DSN=&MIG.ADH18.FWGFAMD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFWGPAMD DD DSN=&MIG.ADH18.FWGPAMD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGARGAR DD DSN=&MIG.ADH18.FGARGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPGAR DD DSN=&MIG.ADH18.TACPGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPCOG DD DSN=&MIG.ADH18.TACPCOG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPCOM DD DSN=&MIG.ADH18.TACPCOM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPCOF DD DSN=&MIG.ADH18.TACPCOF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPZTL DD DSN=&MIG.ADH18.TACPZTL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTACPCOP DD DSN=&MIG.ADH18.TACPCOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA3SITCO DD DSN=&MIG.ADH18.A3SITCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA3GARCO DD DSN=&MIG.ADH18.A3GARCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.ADH18.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH18.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH18  DD DSN=&MIG.ADH18.FADH18.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWGFAMD DD DSN=&MIG.ADH18.FWGFAMD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWGPAMD DD DSN=&MIG.ADH18.FWGPAMD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH18
//*-------------------------_--------------------------------------
//PPRADH18 EXEC PGM=PPRADH18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FGARGAR  DD DISP=SHR,DSN=&MIG.ADH18.FGARGAR
//TACPGAR  DD DISP=SHR,DSN=&MIG.ADH18.TACPGAR
//TACPCOG  DD DISP=SHR,DSN=&MIG.ADH18.TACPCOG
//TACPCOM  DD DISP=SHR,DSN=&MIG.ADH18.TACPCOM
//TACPCOF  DD DISP=SHR,DSN=&MIG.ADH18.TACPCOF
//TACPZTL  DD DISP=SHR,DSN=&MIG.ADH18.TACPZTL
//TACPCOP  DD DISP=SHR,DSN=&MIG.ADH18.TACPCOP
//*GCA modifié pour passage  TPARETA  DD DISP=SHR,DSN=&TAB.TPARTETA
//* AJO BAC-032 - On rebranche la TPARTETA//TPARETA  DD DUMMY
//TPARETA  DD DISP=SHR,DSN=&TAB.TPARTETA
//SA3SITCO DD DISP=SHR,DSN=&MIG.ADH18.A3SITCO
//SA3GARCO DD DISP=SHR,DSN=&MIG.ADH18.A3GARCO
//STCNTCTR DD DISP=SHR,DSN=&MIG.ADH18.TCNTCTR
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH18.FADH2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH18  DD DSN=&MIG.ADH18.FADH18,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFWGFAMD DD DSN=&MIG.ADH18.FWGFAMD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFWGPAMD DD DSN=&MIG.ADH18.FWGPAMD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGARGAR DD DSN=&MIG.ADH18.FGARGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPGAR DD DSN=&MIG.ADH18.TACPGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPCOG DD DSN=&MIG.ADH18.TACPCOG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPCOM DD DSN=&MIG.ADH18.TACPCOM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPCOF DD DSN=&MIG.ADH18.TACPCOF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPZTL DD DSN=&MIG.ADH18.TACPZTL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTACPCOP DD DSN=&MIG.ADH18.TACPCOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA3SITCO DD DSN=&MIG.ADH18.A3SITCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA3GARCO DD DSN=&MIG.ADH18.A3GARCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.ADH18.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH18.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH18  DD DSN=&MIG.ADH18.FADH18.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWGFAMD DD DSN=&MIG.ADH18.FWGFAMD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWGPAMD DD DSN=&MIG.ADH18.FWGPAMD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH18  DD DSN=&MIG.ADH19.FADH18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH18
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH18 SORT FADH18-C-ID-CONTRAT ASC [1:12—
//* SFADH18 SORT FADH18-C-GAR ASC [16:5—
//* SFADH18 SORT FADH18-C-CADRE ASC [21:1—
//* SFADH18 SORT FADH18-ID-GARA-REF ASC [46:4—
//* SFADH18 SORT FADH18-SIT-DEB-EFFET ASC [30:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH18
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH18.FADH18
//SORTOUT  DD DSN=&MIG.ADH19.FADH18,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               16,5,CH,A,
               21,1,CH,A,
               46,4,CH,A,
               30,8,CH,A)
  *MEMSIZE=1000000000
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFWGAR   DD DSN=&MIG.ADH19.FWGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH18  DD DSN=&MIG.ADH19.FADH18.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWGAR   DD DSN=&MIG.ADH19.FWGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH19
//*----------------------------------------------------------------
//PPRADH19 EXEC PGM=PPRADH19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH18  DD DISP=SHR,DSN=&MIG.ADH19.FADH18
//*--------------<FICHIERS CIBLES>---------------------------------
//SFWGAR   DD DSN=&MIG.ADH19.FWGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH18  DD DSN=&MIG.ADH19.FADH18.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWGAR   DD DSN=&MIG.ADH19.FWGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH27
//*----------------------------------------------------------------
//DEL2701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTCTR DD DSN=&MIG.ADH27.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCPI DD DSN=&MIG.ADH27.TCNTCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3POPCO DD DSN=&MIG.ADH27.A3POPCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT2701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-C-ID-CONTRAT ASC [155:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&MIG.ADH27.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(155,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCPI
//*----------------------------------------------------------------
//SORT2702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCPI SORT CNTCPI-C-ID-CONTRAT ASC [120:12—
//* STCNTCPI SORT CNTCPI-C-CPI ASC [151:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCPI
//SORTOUT  DD DSN=&MIG.ADH27.TCNTCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,12,CH,A,
               151,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A3POPCO
//*----------------------------------------------------------------
//SORT2703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA3POPCO SORT A03-POPUL-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA3POPCO SORT A03-POPUL-CONTR-C-CPI ASC [14:3—
//* FNC 1711 AJOUT TRI SUR A03-POPUL-CONTR-D-DEB-EFFET
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03POPCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03POPCO
//SORTOUT  DD DSN=&MIG.ADH27.A3POPCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               14,3,CH,A,
               17,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFWPOP   DD DSN=&MIG.ADH27.FWPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.ADH27.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCPI DD DSN=&MIG.ADH27.TCNTCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA3POPCO DD DSN=&MIG.ADH27.A3POPCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWPOP   DD DSN=&MIG.ADH27.FWPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH27
//*----------------------------------------------------------------
//PPRADH27 EXEC PGM=PPRADH27,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTCTR DD DISP=SHR,DSN=&MIG.ADH27.TCNTCTR
//STCNTCPI DD DISP=SHR,DSN=&MIG.ADH27.TCNTCPI
//SA3POPCO DD DISP=SHR,DSN=&MIG.ADH27.A3POPCO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFWPOP   DD DSN=&MIG.ADH27.FWPOP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.ADH27.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCPI DD DSN=&MIG.ADH27.TCNTCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA3POPCO DD DSN=&MIG.ADH27.A3POPCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWPOP   DD DSN=&MIG.ADH27.FWPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH28
//*----------------------------------------------------------------
//DEL2801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFWPOP   DD DSN=&MIG.ADH28.FWPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFWGAR   DD DSN=&MIG.ADH28.FWGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FWPOP
//*----------------------------------------------------------------
//SORT2801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFWPOP SORT FWPOP-C-ID-CONTRAT ASC [50:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FWPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH27.FWPOP
//SORTOUT  DD DSN=&MIG.ADH28.FWPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FWGAR
//*----------------------------------------------------------------
//SORT2802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFWGAR SORT FWGAR-C-ID-CONTRAT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FWGAR
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH19.FWGAR
//SORTOUT  DD DSN=&MIG.ADH28.FWGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFGARPOP DD DSN=&MIG.ADH28.FGARPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWPOP   DD DSN=&MIG.ADH28.FWPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFWGAR   DD DSN=&MIG.ADH28.FWGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGARPOP DD DSN=&MIG.ADH28.FGARPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH28
//*----------------------------------------------------------------
//PPRADH28 EXEC PGM=PPRADH28,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFWPOP   DD DISP=SHR,DSN=&MIG.ADH28.FWPOP
//SFWGAR   DD DISP=SHR,DSN=&MIG.ADH28.FWGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFGARPOP DD DSN=&MIG.ADH28.FGARPOP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWPOP   DD DSN=&MIG.ADH28.FWPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFWGAR   DD DSN=&MIG.ADH28.FWGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGARPOP DD DSN=&MIG.ADH28.FGARPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH29
//*----------------------------------------------------------------
//DEL2901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFGARPOP DD DSN=&MIG.ADH29.FGARPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FGARPOP
//*----------------------------------------------------------------
//SORT2901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFGARPOP SORT FGARPOP-ID-TECH-UR-ADHR ASC [1:20—
//* SFGARPOP SORT FGARPOP-NO-ORD-ADHS ASC [21:3—
//* SFGARPOP SORT FGARPOP-NO-ORD-CTR ASC [24:3—
//* SFGARPOP SORT FGARPOP-NO-ORD-CPRO ASC [27:3—
//* SFGARPOP SORT FGARPOP-ID-CPRO-REF ASC [30:4—
//* SFGARPOP SORT FGARPOP-ID-GARA-REF ASC [68:4—
//* SFGARPOP SORT FGARPOP-D-DEB-EFFET ASC [34:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FGARPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH28.FGARPOP
//SORTOUT  DD DSN=&MIG.ADH29.FGARPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,4,CH,A,
               68,4,CH,A,
               34,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTMPGAR DD DSN=&MIG.ADH29.FTMPGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFGPREG  DD DSN=&MIG.ADH29.FGPREG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGARPOP DD DSN=&MIG.ADH29.FGARPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTMPGAR DD DSN=&MIG.ADH29.FTMPGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGPREG  DD DSN=&MIG.ADH29.FGPREG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH29
//*----------------------------------------------------------------
//PPRADH29 EXEC PGM=PPRADH29,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFGARPOP DD DISP=SHR,DSN=&MIG.ADH29.FGARPOP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFTMPGAR DD DSN=&MIG.ADH29.FTMPGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFGPREG  DD DSN=&MIG.ADH29.FGPREG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGARPOP DD DSN=&MIG.ADH29.FGARPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTMPGAR DD DSN=&MIG.ADH29.FTMPGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGPREG  DD DSN=&MIG.ADH29.FGPREG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH36
//*----------------------------------------------------------------
//DEL3601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTMPGAR DD DSN=&MIG.ADH36.FTMPGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFGPREG  DD DSN=&MIG.ADH36.FGPREG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FTMPGAR
//*----------------------------------------------------------------
//SORT3601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFTMPGAR SORT FTMPGAR-ID-TECH-UR-ADHR ASC [1:20—
//* SFTMPGAR SORT FTMPGAR-NO-ORD-ADHS ASC [21:3—
//* SFTMPGAR SORT FTMPGAR-NO-ORD-CTR ASC [24:3—
//* SFTMPGAR SORT FTMPGAR-NO-ORD-CPRO ASC [27:3—
//* SFTMPGAR SORT FTMPGAR-NO-ORD-GARA ASC [30:3—
//* SFTMPGAR SORT FTMPGAR-ID-GARA-REF ASC [64:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FTMPGAR
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH29.FTMPGAR
//SORTOUT  DD DSN=&MIG.ADH36.FTMPGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A,
               64,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FGPREG
//*----------------------------------------------------------------
//SORT3602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFGPREG SORT FGPREG-ID-TECH-UR-ADHR ASC [1:20—
//* SFGPREG SORT FGPREG-NO-ORD-ADHS ASC [21:3—
//* SFGPREG SORT FGPREG-NO-ORD-CTR ASC [24:3—
//* SFGPREG SORT FGPREG-NO-ORD-CPRO ASC [27:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FGPREG
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH29.FGPREG
//SORTOUT  DD DSN=&MIG.ADH36.FGPREG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGAGAR DD DSN=&CIB.TAGAGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTFINGAR DD DSN=&MIG.ADH36.TFINGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTMPGAR DD DSN=&MIG.ADH36.FTMPGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGPREG  DD DSN=&MIG.ADH36.FGPREG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAGAR DD DSN=&MIG.ADH36.TAGAGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH36
//*----------------------------------------------------------------
//PPRADH36 EXEC PGM=PPRADH36,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//* GCA mise en dummu TFINGAR  DD DISP=SHR,DSN=&TAB.TFINGAR
//TFINGAR  DD DUMMY
//SFTMPGAR DD DISP=SHR,DSN=&MIG.ADH36.FTMPGAR
//SFGPREG  DD DISP=SHR,DSN=&MIG.ADH36.FGPREG
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGAGAR DD DSN=&CIB.TAGAGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTFINGAR DD DSN=&MIG.ADH36.TFINGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTMPGAR DD DSN=&MIG.ADH36.FTMPGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGPREG  DD DSN=&MIG.ADH36.FGPREG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAGAR DD DSN=&MIG.ADH36.TAGAGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH35
//*----------------------------------------------------------------
//DEL3501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGAGAR DD DSN=&MIG.ADH35.TAGAGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCPI DD DSN=&MIG.ADH35.TCNTCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCTR DD DSN=&MIG.ADH35.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFGARPOP DD DSN=&MIG.ADH35.FGARPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAGAR
//*----------------------------------------------------------------
//SORT3501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGAGAR SORT TAGAGAR-ID-TECH-UR-ADHR ASC [1:20—
//* STAGAGAR SORT TAGAGAR-NO-ORD-ADHS ASC [21:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-CTR ASC [24:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-CPRO ASC [27:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-GARA ASC [30:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAGAR
//SORTOUT  DD DSN=&MIG.ADH35.TAGAGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCPI
//*----------------------------------------------------------------
//SORT3502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCPI SORT CNTCPI-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTCPI SORT CNTCPI-NO-ORD-ADHS ASC [41:3—
//* STCNTCPI SORT CNTCPI-NO-ORD-CTR ASC [44:3—
//* STCNTCPI SORT CNTCPI-NO-ORD-CPRO ASC [75:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCPI
//SORTOUT  DD DSN=&MIG.ADH35.TCNTCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               44,3,CH,A,
               75,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT3503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTCTR SORT CNTCTR-NO-ORD-ADHS ASC [41:3—
//* STCNTCTR SORT CNTCTR-NO-ORD-CTR ASC [64:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&MIG.ADH35.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FGARPOP
//*----------------------------------------------------------------
//SORT3504 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFGARPOP SORT FGARPOP-ID-TECH-UR-ADHR ASC [1:20—
//* SFGARPOP SORT FGARPOP-NO-ORD-ADHS ASC [21:3—
//* SFGARPOP SORT FGARPOP-NO-ORD-CTR ASC [24:3—
//* SFGARPOP SORT FGARPOP-NO-ORD-CPRO ASC [27:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FGARPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH28.FGARPOP
//SORTOUT  DD DSN=&MIG.ADH35.FGARPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTGAR DD DSN=&CIB.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAGAR DD DSN=&MIG.ADH35.TAGAGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCPI DD DSN=&MIG.ADH35.TCNTCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.ADH35.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGARPOP DD DSN=&MIG.ADH35.FGARPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTGAR DD DSN=&MIG.ADH35.TCNTGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH35
//*----------------------------------------------------------------
//PPRADH35 EXEC PGM=PPRADH35,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STAGAGAR DD DISP=SHR,DSN=&MIG.ADH35.TAGAGAR
//STCNTCPI DD DISP=SHR,DSN=&MIG.ADH35.TCNTCPI
//STCNTCTR DD DISP=SHR,DSN=&MIG.ADH35.TCNTCTR
//SFGARPOP DD DISP=SHR,DSN=&MIG.ADH35.FGARPOP
//*--------------<FICHIERS CIBLES>---------------------------------
//STCNTGAR DD DSN=&CIB.TCNTGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAGAR DD DSN=&MIG.ADH35.TAGAGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCPI DD DSN=&MIG.ADH35.TCNTCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.ADH35.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGARPOP DD DSN=&MIG.ADH35.FGARPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTGAR DD DSN=&MIG.ADH35.TCNTGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH46
//*----------------------------------------------------------------
//DEL4601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWGFAMD  DD DSN=&MIG.ADH46.WGFAMD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWGPAMD  DD DSN=&MIG.ADH46.WGPAMD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWPOP    DD DSN=&MIG.ADH46.WPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER WGFAMD
//*----------------------------------------------------------------
//SORT4601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWGFAMD SORT WGFORAMD-C-ID-CONTRAT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.WGFAMD
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH18.FWGFAMD
//SORTOUT  DD DSN=&MIG.ADH46.WGFAMD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WGPAMD
//*----------------------------------------------------------------
//SORT4602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWGPAMD SORT WGPROAMD-C-ID-CONTRAT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.WGPAMD
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH18.FWGPAMD
//SORTOUT  DD DSN=&MIG.ADH46.WGPAMD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WPOP
//*----------------------------------------------------------------
//SORT4603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWPOP SORT FWPOP-C-ID-CONTRAT ASC [50:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FWPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH27.FWPOP
//SORTOUT  DD DSN=&MIG.ADH46.WPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWGFPOP  DD DSN=&MIG.ADH46.WGFPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWGPPOP  DD DSN=&MIG.ADH46.WGPPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH46,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH46,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH46,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGFAMD  DD DSN=&MIG.ADH46.WGFAMD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGPAMD  DD DSN=&MIG.ADH46.WGPAMD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWPOP    DD DSN=&MIG.ADH46.WPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGFPOP  DD DSN=&MIG.ADH46.WGFPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGPPOP  DD DSN=&MIG.ADH46.WGPPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH46
//*----------------------------------------------------------------
//PPRADH46 EXEC PGM=PPRADH46,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH46,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH46,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH46,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SWGFAMD  DD DISP=SHR,DSN=&MIG.ADH46.WGFAMD
//SWGPAMD  DD DISP=SHR,DSN=&MIG.ADH46.WGPAMD
//SWPOP    DD DISP=SHR,DSN=&MIG.ADH46.WPOP
//*--------------<FICHIERS CIBLES>---------------------------------
//SWGFPOP  DD DSN=&MIG.ADH46.WGFPOP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SWGPPOP  DD DSN=&MIG.ADH46.WGPPOP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGFAMD  DD DSN=&MIG.ADH46.WGFAMD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGPAMD  DD DSN=&MIG.ADH46.WGPAMD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWPOP    DD DSN=&MIG.ADH46.WPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGFPOP  DD DSN=&MIG.ADH46.WGFPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGPPOP  DD DSN=&MIG.ADH46.WGPPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH47
//*----------------------------------------------------------------
//DEL4701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWGFPOP  DD DSN=&MIG.ADH47.WGFPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER WGFPOP
//*----------------------------------------------------------------
//SORT4701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWGFPOP SORT WGFORPOP-ID-TECH-UR-ADHR ASC [1:20—
//* SWGFPOP SORT WGFORPOP-NO-ORD-ADHS ASC [21:3—
//* SWGFPOP SORT WGFORPOP-NO-ORD-CTR ASC [24:3—
//* SWGFPOP SORT WGFORPOP-NO-ORD-CPRO ASC [27:3—
//* SWGFPOP SORT WGFORPOP-ID-CPRO-REF ASC [30:4—
//* SWGFPOP SORT WGFORPOP-ID-GARA-REF ASC [68:4—
//* SWGFPOP SORT WGFORPOP-NB-VALR-FORF-AMND ASC [72:11—
//* SWGFPOP SORT WGFORPOP-TX-PPAT-FORF-AMND ASC [83:7—
//* SWGFPOP SORT WGFORPOP-TX-PSAL-FORF-AMND ASC [90:7—
//* SWGFPOP SORT WGFORPOP-TX-TXAP-COTF-AMND ASC [97:7—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.WGFPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH46.WGFPOP
//SORTOUT  DD DSN=&MIG.ADH47.WGFPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,4,CH,A,
               68,4,CH,A,
               72,11,CH,A,
               83,7,CH,A,
               90,7,CH,A,
               97,7,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH47  DD DSN=&MIG.ADH47.FADH47,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH47,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH47,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH47,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGFPOP  DD DSN=&MIG.ADH47.WGFPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH47  DD DSN=&MIG.ADH47.FADH47.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH47
//*----------------------------------------------------------------
//PPRADH47 EXEC PGM=PPRADH47,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH47,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH47,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH47,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SWGFPOP  DD DISP=SHR,DSN=&MIG.ADH47.WGFPOP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH47  DD DSN=&MIG.ADH47.FADH47,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGFPOP  DD DSN=&MIG.ADH47.WGFPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH47  DD DSN=&MIG.ADH47.FADH47.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH48
//*----------------------------------------------------------------
//DEL4801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWGPPOP  DD DSN=&MIG.ADH48.WGPPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER WGPPOP
//*----------------------------------------------------------------
//SORT4801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWGPPOP SORT WGPROPOP-ID-TECH-UR-ADHR ASC [1:20—
//* SWGPPOP SORT WGPROPOP-NO-ORD-ADHS ASC [21:3—
//* SWGPPOP SORT WGPROPOP-NO-ORD-CTR ASC [24:3—
//* SWGPPOP SORT WGPROPOP-NO-ORD-CPRO ASC [27:3—
//* SWGPPOP SORT WGPROPOP-ID-CPRO-REF ASC [30:4—
//* SWGPPOP SORT WGPROPOP-ID-GARA-REF ASC [68:4—
//* SWGPPOP SORT WGPROPOP-TX-STD-PRPR-AMND ASC [72:7—
//* SWGPPOP SORT WGPROPOP-TX-PPAT-PRPR-AMND ASC [79:7—
//* SWGPPOP SORT WGPROPOP-TX-PSAL-PRPR-AMND ASC [86:7—
//* SWGPPOP SORT WGPROPOP-TX-TXAP-COTP-AMND ASC [93:7—
//* FNC2557 AJOUT TRI SUR TX-PART-ETAT 100:8
//* FNC 421 AJOUT TRI SUR D-DEB-EFFET 34:8
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.WGPPOP
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH46.WGPPOP
//SORTOUT  DD DSN=&MIG.ADH48.WGPPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,4,CH,A,
               68,4,CH,A,
               72,7,CH,A,
               79,7,CH,A,
               86,7,CH,A,
               93,7,CH,A,
               100,8,CH,A,
               34,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH48  DD DSN=&MIG.ADH48.FADH48,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH48,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH48,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH48,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWGPPOP  DD DSN=&MIG.ADH48.WGPPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH48  DD DSN=&MIG.ADH48.FADH48.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH48
//*----------------------------------------------------------------
//PPRADH48 EXEC PGM=PPRADH48,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH48,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH48,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH48,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SWGPPOP  DD DISP=SHR,DSN=&MIG.ADH48.WGPPOP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH48  DD DSN=&MIG.ADH48.FADH48,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZWGPPOP  DD DSN=&MIG.ADH48.WGPPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH48  DD DSN=&MIG.ADH48.FADH48.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH49
//*----------------------------------------------------------------
//DEL4901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH47  DD DSN=&MIG.ADH49.FADH47,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH48  DD DSN=&MIG.ADH49.FADH48,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.ADH49.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH47
//*----------------------------------------------------------------
//SORT4901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH47 SORT FADH47-ID-TECH-UR-ADHR ASC [1:20—
//* SFADH47 SORT FADH47-NO-ORD-ADHS ASC [21:3—
//* SFADH47 SORT FADH47-NO-ORD-CTR ASC [24:3—
//* SFADH47 SORT FADH47-NO-ORD-CPRO ASC [27:3—
//* SFADH47 SORT FADH47-ID-GARA-REF ASC [30:4—
//* FNC612 AJOUT TRI SUR FADH47-DT-DEFF-COTF-AMND
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH47
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH47.FADH47
//SORTOUT  DD DSN=&MIG.ADH49.FADH47,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,4,CH,A,
               34,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH48
//*----------------------------------------------------------------
//SORT4902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH48 SORT FADH48-ID-TECH-UR-ADHR ASC [1:20—
//* SFADH48 SORT FADH48-NO-ORD-ADHS ASC [21:3—
//* SFADH48 SORT FADH48-NO-ORD-CTR ASC [24:3—
//* SFADH48 SORT FADH48-NO-ORD-CPRO ASC [27:3—
//* SFADH48 SORT FADH48-ID-GARA-REF ASC [30:4—
//* FNC612 AJOUT TRI SUR FADH48-DT-DEFF-COTP-AMND
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH48
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH48.FADH48
//SORTOUT  DD DSN=&MIG.ADH49.FADH48,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,4,CH,A,
               54,8,CH,A,
               62,8,CH,A,
               70,8,CH,A,
               78,8,CH,A,
               34,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT4903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTGAR SORT CNTGAR-NO-ORD-ADHS ASC [41:3—
//* STCNTGAR SORT CNTGAR-NO-ORD-CTR ASC [64:3—
//* STCNTGAR SORT CNTGAR-NO-ORD-CPRO ASC [95:3—
//* STCNTGAR SORT CNTGAR-ID-GARA-REF ASC [113:4—
//* FNC 421 AJOUT TRI SUR NO-ORD-GARA 102,3
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.ADH49.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A,
               95,3,CH,A,
               113,4,CH,A,
               102,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGACGF DD DSN=&CIB.TAGACGF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACGP DD DSN=&CIB.TAGACGP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH49,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH49,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH49,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH47  DD DSN=&MIG.ADH49.FADH47.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH48  DD DSN=&MIG.ADH49.FADH48.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTGAR DD DSN=&MIG.ADH49.TCNTGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGF DD DSN=&MIG.ADH49.TAGACGF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGP DD DSN=&MIG.ADH49.TAGACGP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH49
//*----------------------------------------------------------------
//PPRADH49 EXEC PGM=PPRADH49,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH49,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH49,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH49,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH47  DD DISP=SHR,DSN=&MIG.ADH49.FADH47
//SFADH48  DD DISP=SHR,DSN=&MIG.ADH49.FADH48
//STCNTGAR DD DISP=SHR,DSN=&MIG.ADH49.TCNTGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGACGF DD DSN=&CIB.TAGACGF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAGACGP DD DSN=&CIB.TAGACGP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH47  DD DSN=&MIG.ADH49.FADH47.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH48  DD DSN=&MIG.ADH49.FADH48.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTGAR DD DSN=&MIG.ADH49.TCNTGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGF DD DSN=&MIG.ADH49.TAGACGF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGP DD DSN=&MIG.ADH49.TAGACGP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABSURP DD DSN=&MIG.ADH20.TABSURP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03GARC DD DSN=&MIG.ADH20.A03GARC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABSURP
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABSURP SORT TAB-SURP-C-GAR-SUR ASC [1:5—
//* STABSURP SORT TAB-SURP-C-PROD-SUR ASC [6:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TABSURP
//SORTIN   DD DISP=SHR,DSN=&TAB.TABSURP
//SORTOUT  DD DSN=&MIG.ADH20.TABSURP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,5,CH,A,
               6,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03GARC
//*----------------------------------------------------------------
//SORT2002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03GARC SORT A03-GAR-CONTR-C-GAR ASC [18:5—
//* SA03GARC SORT A03-GAR-CONTR-C-PROD ASC [24:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03GARCO
//SORTIN   DD DISP=SHR,DSN=&MIG.A03GARCO
//SORTOUT  DD DSN=&MIG.ADH20.A03GARC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(18,5,CH,A,
               24,8,CH,A)
  *MEMSIZE=1000000000
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFISURP1 DD DSN=&MIG.ADH20.FISURP1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABSURP DD DSN=&MIG.ADH20.TABSURP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03GARC DD DSN=&MIG.ADH20.A03GARC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP1 DD DSN=&MIG.ADH20.FISURP1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH20
//*----------------------------------------------------------------
//PPRADH20 EXEC PGM=PPRADH20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABSURP DD DISP=SHR,DSN=&MIG.ADH20.TABSURP
//SA03GARC DD DISP=SHR,DSN=&MIG.ADH20.A03GARC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFISURP1 DD DSN=&MIG.ADH20.FISURP1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABSURP DD DSN=&MIG.ADH20.TABSURP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03GARC DD DSN=&MIG.ADH20.A03GARC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP1 DD DSN=&MIG.ADH20.FISURP1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH21
//*----------------------------------------------------------------
//DEL2101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFISURP1 DD DSN=&MIG.ADH21.FISURP1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03SICO DD DSN=&MIG.ADH21.A03SICO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.ADH21.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FISURP1
//*----------------------------------------------------------------
//SORT2101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFISURP1 SORT FISURP1-C-ID-CONTRAT ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FISURP1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH20.FISURP1
//SORTOUT  DD DSN=&MIG.ADH21.FISURP1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03SICO
//*----------------------------------------------------------------
//SORT2102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03SICO SORT A03-SIT-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA03SICO SORT A03-SIT-CONTR-C-SIT-CONTR ASC [15:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03SITCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03SITCO
//SORTOUT  DD DSN=&MIG.ADH21.A03SICO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               15,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT2103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CONT SORT A03-CONTRAT-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ADH21.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFISURP2 DD DSN=&MIG.ADH21.FISURP2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP1 DD DSN=&MIG.ADH21.FISURP1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03SICO DD DSN=&MIG.ADH21.A03SICO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ADH21.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP2 DD DSN=&MIG.ADH21.FISURP2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH21
//*----------------------------------------------------------------
//PPRADH21 EXEC PGM=PPRADH21,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFISURP1 DD DISP=SHR,DSN=&MIG.ADH21.FISURP1
//SA03SICO DD DISP=SHR,DSN=&MIG.ADH21.A03SICO
//SA03CONT DD DISP=SHR,DSN=&MIG.ADH21.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFISURP2 DD DSN=&MIG.ADH21.FISURP2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP1 DD DSN=&MIG.ADH21.FISURP1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03SICO DD DSN=&MIG.ADH21.A03SICO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ADH21.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP2 DD DSN=&MIG.ADH21.FISURP2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH22
//*----------------------------------------------------------------
//DEL2201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//TABSURP  DD DSN=&MIG.ADH22.TABSURP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFISURP2 DD DSN=&MIG.ADH22.FISURP2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.ADH22.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FISURP2
//*----------------------------------------------------------------
//SORT2202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFISURP2 SORT FISURP2-C-ID-CONTRAT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FISURP2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH21.FISURP2
//SORTOUT  DD DSN=&MIG.ADH22.FISURP2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT2203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-ID-CONTRAT ASC [175:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.ADH22.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(175,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFISURP3 DD DSN=&MIG.ADH22.FISURP3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABSURP DD DSN=&MIG.ADH22.TABSURP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP2 DD DSN=&MIG.ADH22.FISURP2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTGAR DD DSN=&MIG.ADH22.TCNTGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP3 DD DSN=&MIG.ADH22.FISURP3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH22
//*----------------------------------------------------------------
//PPRADH22 EXEC PGM=PPRADH22,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TABSURP  DD DISP=SHR,DSN=&TAB.TABSURP
//SFISURP2 DD DISP=SHR,DSN=&MIG.ADH22.FISURP2
//STCNTGAR DD DISP=SHR,DSN=&MIG.ADH22.TCNTGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFISURP3 DD DSN=&MIG.ADH22.FISURP3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABSURP DD DSN=&MIG.ADH22.TABSURP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP2 DD DSN=&MIG.ADH22.FISURP2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTGAR DD DSN=&MIG.ADH22.TCNTGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP3 DD DSN=&MIG.ADH22.FISURP3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH23
//*----------------------------------------------------------------
//DEL2301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFISURP3 DD DSN=&MIG.ADH23.FISURP3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FISURP3
//*----------------------------------------------------------------
//SORT2301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFISURP3 SORT FISURP3-ID-TECH-UR-ADHR ASC ¬1:20¦
//* SFISURP3 SORT FISURP3-NO-ORD-ADHS ASC ¬21:3¦
//* SFISURP3 SORT FISURP3-NO-ORD-CTR ASC ¬24:3¦
//* SFISURP3 SORT FISURP3-NO-ORD-CPRO ASC ¬27:3¦
//* SFISURP3 SORT FISURP3-NO-ORD-GARA ASC ¬30:3¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FISURP3
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH22.FISURP3
//SORTOUT  DD DSN=&MIG.ADH23.FISURP3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A,
               33,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGATRS DD DSN=&CIB.TAGATRS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFISURP3 DD DSN=&MIG.ADH23.FISURP3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGATRS DD DSN=&MIG.ADH23.TAGATRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH23
//*----------------------------------------------------------------
//PPRADH23 EXEC PGM=PPRADH23,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFISURP3 DD DISP=SHR,DSN=&MIG.ADH23.FISURP3
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGATRS DD DSN=&CIB.TAGATRS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFISURP3 DD DSN=&MIG.ADH23.FISURP3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGATRS DD DSN=&MIG.ADH23.TAGATRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH24
//*----------------------------------------------------------------
//DEL2401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//TABGAR   DD DSN=&MIG.ADH24.TABGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CREV DD DSN=&MIG.ADH24.A03CREV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH2   DD DSN=&MIG.ADH24.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03GARC DD DSN=&MIG.ADH24.A03GARC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CREV
//*----------------------------------------------------------------
//SORT2402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CREV SORT A03-CONT-REV-A-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CTREV
//SORTIN   DD DISP=SHR,DSN=&SRC.A03CTREV
//SORTOUT  DD DSN=&MIG.ADH24.A03CREV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT2404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-ID-CONTRAT-S ASC [97:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH24.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(97,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03GARC
//*----------------------------------------------------------------
//SORT2405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03GARC SORT A03-GAR-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA03GARC SORT A03-GAR-CONTR-C-GAR ASC [18:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03GARCO
//SORTIN   DD DISP=SHR,DSN=&MIG.A03GARCO
//SORTOUT  DD DSN=&MIG.ADH24.A03GARC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               18,5,CH,A)
  *MEMSIZE=1000000000
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFIREV1  DD DSN=&MIG.ADH24.FIREV1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABGAR  DD DSN=&MIG.ADH24.TABGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CREV DD DSN=&MIG.ADH24.A03CREV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ADH24.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH24.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03GARC DD DSN=&MIG.ADH24.A03GARC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIREV1  DD DSN=&MIG.ADH24.FIREV1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH24
//*----------------------------------------------------------------
//PPRADH24 EXEC PGM=PPRADH24,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TABGAR   DD DISP=SHR,DSN=&SRC.A02GARAN
//SA03CREV DD DISP=SHR,DSN=&MIG.ADH24.A03CREV
//SA03CONT DD DISP=SHR,DSN=&MIG.ADH21.A03CONT
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH24.FADH2
//SA03GARC DD DISP=SHR,DSN=&MIG.ADH24.A03GARC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFIREV1  DD DSN=&MIG.ADH24.FIREV1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABGAR  DD DSN=&MIG.ADH24.TABGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CREV DD DSN=&MIG.ADH24.A03CREV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ADH24.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH24.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03GARC DD DSN=&MIG.ADH24.A03GARC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIREV1  DD DSN=&MIG.ADH24.FIREV1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH25
//*----------------------------------------------------------------
//DEL2501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFIREV1  DD DSN=&MIG.ADH25.FIREV1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.ADH25.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FIREV1
//*----------------------------------------------------------------
//SORT2501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFIREV1 SORT FIREV1-C-ID-CONTRAT ASC [1:12—
//* SFIREV1 SORT FIREV1-C-GAR ASC [60:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FIREV1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH24.FIREV1
//SORTOUT  DD DSN=&MIG.ADH25.FIREV1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               60,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT2502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-ID-CONTRAT ASC [175:12—
//* STCNTGAR SORT CNTGAR-C-GAR ASC [223:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.ADH25.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(175,12,CH,A,
               223,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFIREV2  DD DSN=&MIG.ADH25.FIREV2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIREV1  DD DSN=&MIG.ADH25.FIREV1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTGAR DD DSN=&MIG.ADH25.TCNTGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIREV2  DD DSN=&MIG.ADH25.FIREV2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH25
//*----------------------------------------------------------------
//PPRADH25 EXEC PGM=PPRADH25,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFIREV1  DD DISP=SHR,DSN=&MIG.ADH25.FIREV1
//STCNTGAR DD DISP=SHR,DSN=&MIG.ADH25.TCNTGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFIREV2  DD DSN=&MIG.ADH25.FIREV2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIREV1  DD DSN=&MIG.ADH25.FIREV1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTGAR DD DSN=&MIG.ADH25.TCNTGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIREV2  DD DSN=&MIG.ADH25.FIREV2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH26
//*----------------------------------------------------------------
//DEL2601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFIREV2  DD DSN=&MIG.ADH26.FIREV2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FIREV2
//*----------------------------------------------------------------
//SORT2601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFIREV2 SORT FIREV2-ID-TECH-UR-ADHR ASC [1:20—
//* SFIREV2 SORT FIREV2-NO-ORD-ADHS ASC [21:3—
//* SFIREV2 SORT FIREV2-NO-ORD-CTR ASC [24:3—
//* SFIREV2 SORT FIREV2-NO-ORD-CPRO ASC [27:3—
//* SFIREV2 SORT FIREV2-NO-ORD-GARA ASC [30:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FIREV2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH25.FIREV2
//SORTOUT  DD DSN=&MIG.ADH26.FIREV2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGATVD DD DSN=&CIB.TAGATVD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIREV2  DD DSN=&MIG.ADH26.FIREV2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGATVD DD DSN=&MIG.ADH26.TAGATVD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH26
//*----------------------------------------------------------------
//PPRADH26 EXEC PGM=PPRADH26,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFIREV2  DD DISP=SHR,DSN=&MIG.ADH26.FIREV2
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGATVD DD DSN=&CIB.TAGATVD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIREV2  DD DSN=&MIG.ADH26.FIREV2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGATVD DD DSN=&MIG.ADH26.TAGATVD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH98
//*----------------------------------------------------------------
//DEL9801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL9802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBACBNA  DD DSN=&CIB.BACBNA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL9803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH98,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH98,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH98,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGABNA DD DSN=&MIG.ADH98.TAGABNA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBACBNA  DD DSN=&MIG.ADH98.BACBNA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH98
//*----------------------------------------------------------------
//PPRADH98 EXEC PGM=PPRADH98,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH98,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH98,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH98,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TAGABNA  DD DISP=SHR,DSN=&CIB.TAGABNA
//*--------------<FICHIERS CIBLES>---------------------------------
//SBACBNA  DD DSN=&CIB.BACBNA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGABNA DD DSN=&MIG.ADH98.TAGABNA.R,
//            DISP=(NEW,CATLG,CATLG),                                                                                        tl
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBACBNA  DD DSN=&MIG.ADH98.BACBNA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*BAC-063 - AJO le 20/08/2013
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL9A01  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STAGAADN DD DSN=&MIG.ADH9A.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACNT DD DSN=&MIG.ADH9A.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAHMA DD DSN=&MIG.ADH9A.TAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAADN
//*----------------------------------------------------------------
//SORT9A01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAGAADN SORT TAGAADN-ID-TECH-UR-ADHR ASC [1:20]
//* TAGAADN SORT TAGAADN-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH53.TAGAADN
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAADN
//SORTOUT  DD DSN=&MIG.ADH9A.TAGAADN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACNT
//*----------------------------------------------------------------
//SORT9A02 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAGACNT SORT TAGACNT-ID-TECH-UR-ADHR ASC [1:20]
//* TAGACNT SORT TAGACNT-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACNT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACNT
//SORTOUT  DD DSN=&MIG.ADH9A.TAGACNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAHMA
//*----------------------------------------------------------------
//SORT9A03 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAGAHMA SORT TAGAHMA-ID-TECH-UR-ADHR ASC [1:20]
//* TAGAHMA SORT TAGAHMA-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH13.TAGAHMA
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAHMA
//SORTOUT  DD DSN=&MIG.ADH9A.TAGAHMA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL9A02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SSAGAADN DD DSN=&CIB.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SSAGAHMA DD DSN=&CIB.TAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAADN DD DSN=&RED.ADH9A.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&RED.ADH9A.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAHMA DD DSN=&RED.ADH9A.TAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSAGAADN DD DSN=&RED.ADH9A.SAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSAGAHMA DD DSN=&RED.ADH9A.SAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL9A03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH9A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH9A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH9A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRADH9A EXEC PGM=PPRADH9A
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH9A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH9A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH9A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//STAGAADN DD DISP=SHR,DSN=&MIG.ADH9A.TAGAADN
//STAGACNT DD DISP=SHR,DSN=&MIG.ADH9A.TAGACNT
//STAGAHMA DD DISP=SHR,DSN=&MIG.ADH9A.TAGAHMA
//*--------------<FICHIERS CIBLES>---------------------------------
//SSAGAADN DD DSN=&CIB.TAGAADN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//SSAGAHMA DD DSN=&CIB.TAGAHMA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZTAGAADN DD DSN=&RED.ADH9A.TAGAADN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTAGACNT DD DSN=&RED.ADH9A.TAGACNT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTAGAHMA DD DSN=&RED.ADH9A.TAGAHMA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZSAGAADN DD DSN=&RED.ADH9A.SAGAADN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZSAGAHMA DD DSN=&RED.ADH9A.SAGAHMA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH99
//*----------------------------------------------------------------
//DEL9901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL9902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBACADH  DD DSN=&CIB.BACADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL9903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH99,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAADN DD DSN=&MIG.ADH99.TAGAADN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACAT DD DSN=&MIG.ADH99.TAGACAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGF DD DSN=&MIG.ADH99.TAGACGF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGP DD DSN=&MIG.ADH99.TAGACGP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&MIG.ADH99.TAGACNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAGAR DD DSN=&MIG.ADH99.TAGAGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAHMA DD DSN=&MIG.ADH99.TAGAHMA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAHSA DD DSN=&MIG.ADH99.TAGAHSA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAIOC DD DSN=&MIG.ADH99.TAGAIOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGALCC DD DSN=&MIG.ADH99.TAGALCC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGATRS DD DSN=&MIG.ADH99.TAGATRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGATVD DD DSN=&MIG.ADH99.TAGATVD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBACADH  DD DSN=&MIG.ADH99.BACADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH99
//*----------------------------------------------------------------
//PPRADH99 EXEC PGM=PPRADH99,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH99,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TAGAADN  DD DISP=SHR,DSN=&CIB.TAGAADN
//TAGACAT  DD DISP=SHR,DSN=&CIB.TAGACAT
//TAGACGF  DD DISP=SHR,DSN=&CIB.TAGACGF
//TAGACGP  DD DISP=SHR,DSN=&CIB.TAGACGP
//TAGACNT  DD DISP=SHR,DSN=&CIB.TAGACNT
//TAGAGAR  DD DISP=SHR,DSN=&CIB.TAGAGAR
//TAGAHMA  DD DISP=SHR,DSN=&CIB.TAGAHMA
//TAGAHSA  DD DISP=SHR,DSN=&CIB.TAGAHSA
//TAGAIOC  DD DISP=SHR,DSN=&CIB.TAGAIOC
//TAGALCC  DD DISP=SHR,DSN=&CIB.TAGALCC
//TAGATRS  DD DISP=SHR,DSN=&CIB.TAGATRS
//*BAC-055 - AJO le 05/08/2013 - TAGATVD en dummy
//*TAGATVD  DD DISP=SHR,DSN=&CIB.TAGATVD
//TAGATVD  DD DUMMY
//*--------------<FICHIERS CIBLES>---------------------------------
//SBACADH  DD DSN=&CIB.BACADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAADN DD DSN=&MIG.ADH99.TAGAADN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACAT DD DSN=&MIG.ADH99.TAGACAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGF DD DSN=&MIG.ADH99.TAGACGF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGP DD DSN=&MIG.ADH99.TAGACGP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACNT DD DSN=&MIG.ADH99.TAGACNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAGAR DD DSN=&MIG.ADH99.TAGAGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAHMA DD DSN=&MIG.ADH99.TAGAHMA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAHSA DD DSN=&MIG.ADH99.TAGAHSA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAIOC DD DSN=&MIG.ADH99.TAGAIOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGALCC DD DSN=&MIG.ADH99.TAGALCC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGATRS DD DSN=&MIG.ADH99.TAGATRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGATVD DD DSN=&MIG.ADH99.TAGATVD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBACADH  DD DSN=&MIG.ADH99.BACADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*=========================================
//* Conversion en ebcdic des fichiers cibles
//*=========================================
//*
//*JOB90  EXEC JCL=JCVADH01
//*JOB91  EXEC JCL=JCVADH03
//*JOB92  EXEC JCL=JCVADH02
//*
//* FIN DU JCL DE MIGRATION