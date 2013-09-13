//J9PRRTI  JOB UTI00TX0,'PR BREF A03-RTI',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0103,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRRTI01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01ENT  DD DSN=&MIG.RTI01.A01ENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01HADR DD DSN=&MIG.RTI01.A01HADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01RIBF DD DSN=&MIG.RTI01.A01RIBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENT SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.RTI01.A01ENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01HADR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01HADR SORT A01-HIST-ADRESSE-C-ID-PERS ASC [17:12—
//* SA01HADR SORT A01-HIST-ADRESSE-C-TYPE-ADR ASC [29:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01HISTA
//SORTIN   DD DISP=SHR,DSN=&SRC.A01HISTA
//SORTOUT  DD DSN=&MIG.RTI01.A01HADR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A,
               29,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01RIBF
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01RIBF SORT A01-RIB-FRANCE-C-ID-PERS ASC [94:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01RIBFR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01RIBFR
//SORTOUT  DD DSN=&MIG.RTI01.A01RIBF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRTITIE DD DSN=&CIB.TRTITIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRTIROL DD DSN=&CIB.TRTIROL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADT DD DSN=&CIB.TRCOADT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOMPT DD DSN=&CIB.TRCOMPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPIT DD DSN=&CIB.TRCOPIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPRT DD DSN=&CIB.TRCOPRT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADRS  DD DSN=&CIB.CHADR2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRRTI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRRTI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRRTI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRE  DD DSN=&MIG.RTI01.CHADRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPAYS   DD DSN=&MIG.RTI01.CPAYS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTBRTIER DD DSN=&MIG.RTI01.TBRTIER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCOZCA  DD DSN=&MIG.RTI01.TCOZCA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZCB DD DSN=&MIG.RTI01.TRCOZCB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZBI DD DSN=&MIG.RTI01.TRCOZBI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENT  DD DSN=&MIG.RTI01.A01ENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01HADR DD DSN=&MIG.RTI01.A01HADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01RIBF DD DSN=&MIG.RTI01.A01RIBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRTITIE DD DSN=&MIG.RTI01.TRTITIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRTIROL DD DSN=&MIG.RTI01.TRTIROL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADT DD DSN=&MIG.RTI01.TRCOADT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOMPT DD DSN=&MIG.RTI01.TRCOMPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPIT DD DSN=&MIG.RTI01.TRCOPIT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPRT DD DSN=&MIG.RTI01.TRCOPRT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRS  DD DSN=&MIG.RTI01.CHADRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRRTI01
//*----------------------------------------------------------------
//PPRRTI01 EXEC PGM=PPRRTI01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRRTI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRRTI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRRTI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CHADRE   DD DISP=SHR,DSN=&CIB.CHADR1
//CPAYS    DD DISP=SHR,DSN=&TAB.CPAY
//TBRTIER  DD DISP=SHR,DSN=&TAB.TBRTIER
//TCOZCA   DD DISP=SHR,DSN=&TAB.TRCOZCA
//TRCOZCB  DD DISP=SHR,DSN=&TAB.TRCOZCB
//*TRCOZBI  DD DISP=SHR,DSN=&TAB.TRCOZBI
//TRCOZBI  DD DUMMY
//SA01ENT  DD DISP=SHR,DSN=&MIG.RTI01.A01ENT
//SA01HADR DD DISP=SHR,DSN=&MIG.RTI01.A01HADR
//SA01RIBF DD DISP=SHR,DSN=&MIG.RTI01.A01RIBF
//*--------------<FICHIERS CIBLES>---------------------------------
//STRTITIE DD DSN=&CIB.TRTITIE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRTIROL DD DSN=&CIB.TRTIROL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOADT DD DSN=&CIB.TRCOADT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOMPT DD DSN=&CIB.TRCOMPT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOPIT DD DSN=&CIB.TRCOPIT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOPRT DD DSN=&CIB.TRCOPRT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHADRS  DD DSN=&CIB.CHADR2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRE  DD DSN=&MIG.RTI01.CHADRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPAYS   DD DSN=&MIG.RTI01.CPAYS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTBRTIER DD DSN=&MIG.RTI01.TBRTIER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCOZCA  DD DSN=&MIG.RTI01.TCOZCA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZCB DD DSN=&MIG.RTI01.TRCOZCB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZBI DD DSN=&MIG.RTI01.TRCOZBI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENT  DD DSN=&MIG.RTI01.A01ENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01HADR DD DSN=&MIG.RTI01.A01HADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01RIBF DD DSN=&MIG.RTI01.A01RIBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRTITIE DD DSN=&MIG.RTI01.TRTITIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRTIROL DD DSN=&MIG.RTI01.TRTIROL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADT DD DSN=&MIG.RTI01.TRCOADT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOMPT DD DSN=&MIG.RTI01.TRCOMPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPIT DD DSN=&MIG.RTI01.TRCOPIT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPRT DD DSN=&MIG.RTI01.TRCOPRT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRS  DD DSN=&MIG.RTI01.CHADRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRRTI50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBREFRTI DD DSN=&CIB.BREFRTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRRTI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRRTI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRRTI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRTITIE DD DSN=&MIG.RTI50.TRTITIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRTIROL DD DSN=&MIG.RTI50.TRTIROL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADT DD DSN=&MIG.RTI50.TRCOADT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOMPT DD DSN=&MIG.RTI50.TRCOMPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPIT DD DSN=&MIG.RTI50.TRCOPIT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPRT DD DSN=&MIG.RTI50.TRCOPRT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBREFRTI DD DSN=&MIG.RTI50.BREFRTI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRRTI50
//*----------------------------------------------------------------
//PPRRTI50 EXEC PGM=PPRRTI50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRRTI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRRTI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRRTI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TRTITIE  DD DISP=SHR,DSN=&CIB.TRTITIE
//TRTIROL  DD DISP=SHR,DSN=&CIB.TRTIROL
//TRCOADT  DD DISP=SHR,DSN=&CIB.TRCOADT
//TRCOMPT  DD DISP=SHR,DSN=&CIB.TRCOMPT
//TRCOPIT  DD DISP=SHR,DSN=&CIB.TRCOPIT
//TRCOPRT  DD DISP=SHR,DSN=&CIB.TRCOPRT
//*--------------<FICHIERS CIBLES>---------------------------------
//SBREFRTI DD DSN=&CIB.BREFRTI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRTITIE DD DSN=&MIG.RTI50.TRTITIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRTIROL DD DSN=&MIG.RTI50.TRTIROL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADT DD DSN=&MIG.RTI50.TRCOADT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOMPT DD DSN=&MIG.RTI50.TRCOMPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPIT DD DSN=&MIG.RTI50.TRCOPIT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPRT DD DSN=&MIG.RTI50.TRCOPRT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBREFRTI DD DSN=&MIG.RTI50.BREFRTI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*=========================================
//* Conversion en ebcdic des fichiers cibles
//*=========================================
//*
//*JOB99  EXEC JCL=JCVRTI01
//*
//* FIN DU JCL DE MIGRATION