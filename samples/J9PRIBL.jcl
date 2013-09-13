//J9PRIBL  JOB UTI00TX0,'PR C05-IBL',CLASS=Z,MSGCLASS=X,                JOB07692
//*        RESTART=DEL0102,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRIBL01
//*----------------------------------------------------------------
//DEL0001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SD01IFCI DD DSN=&MIG.IBL00.D01IFCI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER D01IFCI
//*----------------------------------------------------------------
//SORT0001 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01IDENT
//SORTIN   DD DISP=SHR,DSN=&SRC.D01IDENT
//SORTOUT  DD DSN=&MIG.IBL00.D01IFCI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS01IFCI DD DSN=&MIG.IBL00.S01IFCI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRIBL00,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRIBL00,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRIBL00,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRIBL01
//*----------------------------------------------------------------
//PPRIBL00 EXEC PGM=PPRIBL00,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRIBL00,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRIBL00,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRIBL00,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SD01IFCI DD DISP=SHR,DSN=&MIG.IBL00.D01IFCI
//*--------------<FICHIERS CIBLES>---------------------------------
//SS01IFCI DD DSN=&MIG.IBL00.S01IFCI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRIBL01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SD01IFCI DD DSN=&MIG.IBL01.D01IFCI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03EPBL DD DSN=&MIG.IBL01.J03EPBL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER D01IFCI
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SD01IFCI SORT D01-IDENT-FCI-C-ID-PERS ASC [19:12—
//* SD01IFCI SORT D01-IDENT-FCI-C-ID-FCI ASC [2:12—
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01IDENT
//SORTIN   DD DISP=SHR,DSN=&MIG.IBL00.S01IFCI
//SORTOUT  DD DSN=&MIG.IBL01.D01IFCI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A,
               2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03EPBL
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03EPBL SORT J03-ETAT-PER-BL-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03ETAT
//SORTIN   DD DISP=SHR,DSN=&SRC.J03ETAT
//SORTOUT  DD DSN=&MIG.IBL01.J03EPBL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STIBLSVI DD DSN=&CIB.TIBLSVI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLRJI DD DSN=&CIB.TIBLRJI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLSVP DD DSN=&CIB.TIBLSVP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLRJP DD DSN=&CIB.TIBLRJP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRIBL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRIBL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRIBL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.IBL01.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZD01IFCI DD DSN=&MIG.IBL01.D01IFCI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03EPBL DD DSN=&MIG.IBL01.J03EPBL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADR1  DD DSN=&MIG.IBL01.FPADR1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBLSVI DD DSN=&MIG.IBL01.TIBLSVI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBLRJI DD DSN=&MIG.IBL01.TIBLRJI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBLSVP DD DSN=&MIG.IBL01.TIBLSVP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBLRJP DD DSN=&MIG.IBL01.TIBLRJP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRIBL01
//*----------------------------------------------------------------
//PPRIBL01 EXEC PGM=PPRIBL01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRIBL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRIBL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRIBL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//SD01IFCI DD DISP=SHR,DSN=&MIG.IBL01.D01IFCI
//SJ03EPBL DD DISP=SHR,DSN=&MIG.IBL01.J03EPBL
//SFPADR1  DD DISP=SHR,DSN=&MIG.TCI01.FPADR01
//*--------------<FICHIERS CIBLES>---------------------------------
//STIBLSVI DD DSN=&CIB.TIBLSVI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STIBLRJI DD DSN=&CIB.TIBLRJI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STIBLSVP DD DSN=&CIB.TIBLSVP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STIBLRJP DD DSN=&CIB.TIBLRJP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.IBL01.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZD01IFCI DD DSN=&MIG.IBL01.D01IFCI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03EPBL DD DSN=&MIG.IBL01.J03EPBL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADR1  DD DSN=&MIG.IBL01.FPADR1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBLSVI DD DSN=&MIG.IBL01.TIBLSVI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBLRJI DD DSN=&MIG.IBL01.TIBLRJI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBLSVP DD DSN=&MIG.IBL01.TIBLSVP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBLRJP DD DSN=&MIG.IBL01.TIBLRJP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*