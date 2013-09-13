//J9PRMAN  JOB UTI00TX0,'PR GRECCO G93-MAN',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL0102,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PRREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRMAN01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05OPER DD DSN=&MIG.MAN01.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SURCOMTI DD DSN=&MIG.MAN01.URCOMTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [53:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.MAN01.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               43,9,CH,A,
               53,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER URCOMTI
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SURCOMTI SORT URCOMTIE-C-GRPT-GEST ASC [49:1—
//* SURCOMTI SORT URCOMTIE-C-ADH ASC [50:9—
//* SURCOMTI SORT URCOMTIE-C-ORDRE ASC [59:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.URCOMTIE
//SORTIN   DD DISP=SHR,DSN=&TAB.URCOMTI
//SORTOUT  DD DSN=&MIG.MAN01.URCOMTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,1,CH,A,
               50,9,CH,A,
               59,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCA     DD DSN=&CIB.FCA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRMAN01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRMAN01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRMAN01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05OPER DD DSN=&MIG.MAN01.B05OPER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZURCOMTI DD DSN=&MIG.MAN01.URCOMTI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCA     DD DSN=&MIG.MAN01.FCA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRMAN01
//*----------------------------------------------------------------
//PPRMAN01 EXEC PGM=PPRMAN01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRMAN01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRMAN01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRMAN01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05OPER DD DISP=SHR,DSN=&MIG.MAN01.B05OPER
//SURCOMTI DD DISP=SHR,DSN=&MIG.MAN01.URCOMTI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCA     DD DSN=&CIB.FCA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05OPER DD DSN=&MIG.MAN01.B05OPER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZURCOMTI DD DSN=&MIG.MAN01.URCOMTI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCA     DD DSN=&MIG.MAN01.FCA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRMAN50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SMANDAT  DD DSN=&CIB.MANDAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRMAN50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRMAN50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRMAN50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCA     DD DSN=&MIG.MAN50.FCA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZMANDAT  DD DSN=&MIG.MAN50.MANDAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRMAN50
//*----------------------------------------------------------------
//PPRMAN50 EXEC PGM=PPRMAN50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRMAN50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRMAN50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRMAN50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCA      DD DISP=SHR,DSN=&CIB.FCA
//*--------------<FICHIERS CIBLES>---------------------------------
//SMANDAT  DD DSN=&CIB.MANDAT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCA     DD DSN=&MIG.MAN50.FCA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZMANDAT  DD DSN=&MIG.MAN50.MANDAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
