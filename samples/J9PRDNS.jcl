//J9PRDNS  JOB UTI00TX0,'PR DN C02-DNS',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL0102,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRDNS01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SJ04DAS  DD DSN=&MIG.DNS01.J04DAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&MIG.DNS01.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STADHADH DD DSN=&MIG.DNS01.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ADH  DD DSN=&MIG.DNS01.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS SORT J04-DAS-C-GRPT-GEST ASC [20:1ó
//* SJ04DAS SORT J04-DAS-C-ADH ASC [22:9ó
//* SJ04DAS SORT J04-DAS-C-ORDRE ASC [32:4ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J04DAS
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.DNS01.J04DAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(20,1,CH,A,
               22,9,CH,A,
               32,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUEDADH
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-C-GRPT-GEST ASC ç101:1Ù
//* STUEDADH SORT TABUEDAD-C-ADH ASC ç102:9Ù
//* STUEDADH SORT TABUEDAD-C-ORDRE ASC ç111:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&MIG.DNS01.TUEDADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC ç94:1Ù
//* STADHADH SORT ADHSADH-C-ADH ASC ç95:9Ù
//* STADHADH SORT ADHSADH-C-ORDRE ASC ç104:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.DNS01.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-GRPT-GEST ASC ç1:1Ù
//* SA03ADH SORT A03-ADHERENT-C-ADH ASC ç3:9Ù
//* SA03ADH SORT A03-ADHERENT-C-ORDRE ASC ç13:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.DNS01.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STNKM0DS DD DSN=&CIB.TNKM0DS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STDNDAS  DD DSN=&CIB.TDNDAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRDNS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRDNS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRDNS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ04DAS  DD DSN=&MIG.DNS01.J04DAS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.DNS01.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.DNS01.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.DNS01.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTNKM0DS DD DSN=&MIG.DNS01.TNKM0DS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTDNDAS  DD DSN=&MIG.DNS01.TDNDAS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRDNS01
//*----------------------------------------------------------------
//PPRDNS01 EXEC PGM=PPRDNS01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRDNS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRDNS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRDNS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SJ04DAS  DD DISP=SHR,DSN=&MIG.DNS01.J04DAS
//STUEDADH DD DISP=SHR,DSN=&MIG.DNS01.TUEDADH
//STADHADH DD DISP=SHR,DSN=&MIG.DNS01.TADHADH
//SA03ADH  DD DISP=SHR,DSN=&MIG.DNS01.A03ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STNKM0DS DD DSN=&CIB.TNKM0DS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STDNDAS  DD DSN=&CIB.TDNDAS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ04DAS  DD DSN=&MIG.DNS01.J04DAS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.DNS01.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.DNS01.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.DNS01.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTNKM0DS DD DSN=&MIG.DNS01.TNKM0DS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTDNDAS  DD DSN=&MIG.DNS01.TDNDAS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SENCDNS  DD DSN=&CIB.ENCDNS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRDNS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRDNS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRDNS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTNKM0DS DD DSN=&MIG.DNS50.TNKM0DS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZENCDNS  DD DSN=&MIG.DNS50.ENCDNS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRDNS50
//*----------------------------------------------------------------
//PPRDNS50 EXEC PGM=PPRDNS50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRDNS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRDNS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRDNS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TNKM0DS  DD DISP=SHR,DSN=&CIB.TNKM0DS
//*--------------<FICHIERS CIBLES>---------------------------------
//SENCDNS  DD DSN=&CIB.ENCDNS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTNKM0DS DD DSN=&MIG.DNS50.TNKM0DS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZENCDNS  DD DSN=&MIG.DNS50.ENCDNS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION