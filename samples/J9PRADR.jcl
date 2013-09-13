//J9PRADR  JOB UTI00TX0,'PR DROITS C01-ADR',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0901,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIND1  DD DSN=&MIG.ADR01.FPIND1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01LIAI DD DSN=&MIG.ADR01.A01LIAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03PD DD DSN=&MIG.ADR01.J03PD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPIND1
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPIND1 SORT FPIND1-C-ID-PERS ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPIND1
//SORTIN   DD DISP=SHR,DSN=&CIB.FPIND1
//SORTOUT  DD DSN=&MIG.ADR01.FPIND1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01LIAI
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01LIAI SORT A01-LIAISON-C-ID-PERS ASC [19:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01LIAIS
//SORTIN   DD DISP=SHR,DSN=&SRC.A01LIAIS
//SORTOUT  DD DSN=&MIG.ADR01.A01LIAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI3 DU FICHIER J03PD
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03PD SORT J03-PER-DROITS-C-ID-PERS ASC [518:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY      DD DISP=SHR,DSN=&CPYSRC.J03PERDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERDR
//SORTOUT  DD DSN=&MIG.ADR01.J03PD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(500,100),RLSE)
//SYSIN    DD *
  SORT FIELDS=(518,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR01  DD DSN=&MIG.ADR01.FADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SFADR01D DD DSN=&MIG.ADR01.FADR01D,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03PD   DD DSN=&MIG.ADR01.J03PD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIND1  DD DSN=&MIG.ADR01.FPIND1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01LIAI DD DSN=&MIG.ADR01.A01LIAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR01  DD DSN=&MIG.ADR01.FADR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*ZFADR01D DD DSN=&MIG.ADR01.FADR01D.R,
//*           DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR01
//*----------------------------------------------------------------
//PPRADR01 EXEC PGM=PPRADR01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SJ03PD   DD DISP=SHR,DSN=&MIG.ADR01.J03PD
//SFPIND1  DD DISP=SHR,DSN=&MIG.ADR01.FPIND1
//SA01LIAI DD DISP=SHR,DSN=&MIG.IND05.LIAISON
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR01  DD DSN=&MIG.ADR01.FADR01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*SFADR01D DD DSN=&MIG.ADR01.FADR01D,
//*            DISP=(NEW,CATLG,CATLG),
//*            SPACE=(TRK,(200,100),RLSE)
//ZJ03PD   DD DSN=&MIG.ADR01.J03PD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIND1  DD DSN=&MIG.ADR01.FPIND1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01LIAI DD DSN=&MIG.ADR01.A01LIAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR01  DD DSN=&MIG.ADR01.FADR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*ZFADR01D DD DSN=&MIG.ADR01.FADR01D.R,
//*            DISP=(NEW,CATLG,CATLG),
//*            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL1B01  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SFADR01  DD DSN=&MIG.ADR1B.FADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPERAFFI DD DSN=&MIG.ADR1B.PERAFFI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR01
//*----------------------------------------------------------------
//SORT1B01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* FADR01 SORT FADR01-C-ID-PA ASC [110:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR01.FADR01
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR01
//SORTOUT  DD DSN=&MIG.ADR1B.FADR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(110,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03PERAF / PERAFFI
//*----------------------------------------------------------------
//SORT1B02 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* PERAFFI SORT J03-PER-AFFIL-C-ID-PA ASC [345:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERAF
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PERAF
//SORTOUT  DD DSN=&MIG.ADR1B.PERAFFI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(345,12,CH,A)
/*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1B02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SOFADR01 DD DSN=&MIG.ADR1B.OFADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1B03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRADR1B EXEC PGM=PPRADR1B
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR01  DD DISP=SHR,DSN=&MIG.ADR1B.FADR01
//SPERAFFI DD DISP=SHR,DSN=&MIG.ADR1B.PERAFFI
//*--------------<FICHIERS CIBLES>---------------------------------
//SOFADR01 DD DSN=&MIG.ADR1B.OFADR01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SJ03PERA DD DSN=&MIG.ADR02.J03PERA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTR DD DSN=&MIG.ADR02.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER J03PERA
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03PERA SORT J03-PER-AFFIL-C-ID-PERS-ENTR ASC ç40:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PERAF
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERAF
//SORTOUT  DD DSN=&MIG.ADR02.J03PERA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(40,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ADR02.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR02  DD DSN=&MIG.ADR02.FADR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03PERA DD DSN=&MIG.ADR02.J03PERA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.ADR02.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR02  DD DSN=&MIG.ADR02.FADR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR02
//*----------------------------------------------------------------
//PPRADR02 EXEC PGM=PPRADR02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SJ03PERA DD DISP=SHR,DSN=&MIG.ADR02.J03PERA
//SA01ENTR DD DISP=SHR,DSN=&MIG.ADR02.A01ENTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR02  DD DSN=&MIG.ADR02.FADR02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03PERA DD DSN=&MIG.ADR02.J03PERA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.ADR02.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR02  DD DSN=&MIG.ADR02.FADR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR01  DD DSN=&MIG.ADR03.FADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAUE01 DD DSN=&MIG.ADR03.FPAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&MIG.ADR03.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STADHADH DD DSN=&MIG.ADR03.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&MIG.ADR03.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR01
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR01 SORT FADR01-C-GRPT-GEST ASC ç23:1Ù
//* SFADR01 SORT FADR01-C-ADH ASC ç24:9Ù
//* SFADR01 SORT FADR01-C-ORDRE ASC ç33:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR01
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR1B.OFADR01
//SORTOUT  DD DSN=&MIG.ADR03.FADR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(23,1,CH,A,
               24,9,CH,A,
               33,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPAUE01
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPAUE01 SORT FPAUE01-C-GRPT-GEST ASC ç38:1Ù
//* SFPAUE01 SORT FPAUE01-C-ADH ASC ç39:9Ù
//* SFPAUE01 SORT FPAUE01-C-ORDRE ASC ç48:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPAUE01
//SORTIN   DD DISP=SHR,DSN=&CIB.FPAUE01
//SORTOUT  DD DSN=&MIG.ADR03.FPAUE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,1,CH,A,
               39,9,CH,A,
               48,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUEDADH
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-C-GRPT-GEST ASC ç101:1Ù
//* STUEDADH SORT TABUEDAD-C-ADH ASC ç102:9Ù
//* STUEDADH SORT TABUEDAD-C-ORDRE ASC ç111:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&MIG.ADR03.TUEDADH,
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
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC ç94:1Ù
//* STADHADH SORT ADHSADH-C-ADH ASC ç95:9Ù
//* STADHADH SORT ADHSADH-C-ORDRE ASC ç104:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADR03.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0305 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-C-GRPT-GEST ASC ç101:1Ù
//* STUECADH SORT TABUECAD-C-ADH ASC ç102:9Ù
//* STUECADH SORT TABUECAD-C-ORDRE ASC ç111:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUECADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUECADH
//SORTOUT  DD DSN=&MIG.ADR03.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03A DD DSN=&MIG.ADR03.FADR03A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR01  DD DSN=&MIG.ADR03.FADR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAUE01 DD DSN=&MIG.ADR03.FPAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.ADR03.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.ADR03.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.ADR03.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03A DD DSN=&MIG.ADR03.FADR03A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR03
//*----------------------------------------------------------------
//PPRADR03 EXEC PGM=PPRADR03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR01  DD DISP=SHR,DSN=&MIG.ADR03.FADR01
//SFPAUE01 DD DISP=SHR,DSN=&MIG.ADR03.FPAUE01
//STUEDADH DD DISP=SHR,DSN=&MIG.ADR03.TUEDADH
//STADHADH DD DISP=SHR,DSN=&MIG.ADR03.TADHADH
//STUECADH DD DISP=SHR,DSN=&MIG.ADR03.TUECADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR03A DD DSN=&MIG.ADR03.FADR03A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR01  DD DSN=&MIG.ADR03.FADR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAUE01 DD DSN=&MIG.ADR03.FPAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.ADR03.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.ADR03.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.ADR03.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03A DD DSN=&MIG.ADR03.FADR03A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR02  DD DSN=&MIG.ADR04.FADR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03PA   DD DSN=&MIG.ADR04.J03PA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SJ03BP   DD DSN=&MIG.ADR04.J03BP,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR03A DD DSN=&MIG.ADR04.FADR03A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03BLOC DD DSN=&MIG.ADR04.J03BLOC1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR02
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR02 SORT FADR02-C-ID-PA ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR02
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR02.FADR02
//SORTOUT  DD DSN=&MIG.ADR04.FADR02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03PA
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03PA SORT J03-PER-AFFIL-C-ID-PA ASC ç345:12Ù
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PERAF
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERAF
//SORTOUT  DD DSN=&MIG.ADR04.J03PA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(345,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03A
//*----------------------------------------------------------------
//SORT0404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03A SORT FADR03A-C-ID-PA ASC ç110:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03A
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR03.FADR03A
//SORTOUT  DD DSN=&MIG.ADR04.FADR03A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(110,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI1 DU FICHIER J03BP
//*----------------------------------------------------------------
//SORT0405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03BP SORT J03-BP-C-ID-PA ASC [357:12ó
//* SJ03BP SORT J03-BP-Q-EXE ASC [29:4ó
//* SJ03BP SORT J03-BP-D-DEB-PER ASC [33:8ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03BLOC
//SORTIN   DD DISP=SHR,DSN=&SRC.J03BLOC
//SORTOUT  DD DSN=&MIG.ADR04.J03BLOC1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(2000,5000),RLSE)
//SYSIN    DD *
  SORT FIELDS=(357,12,CH,A,
               29,4,CH,A,
               33,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR04.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR09A DD DSN=&MIG.ADR04.FADR09A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR02  DD DSN=&MIG.ADR04.FADR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03PA   DD DSN=&MIG.ADR04.J03PA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*ZJ03BP   DD DSN=&MIG.ADR04.J03BP.R,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03A DD DSN=&MIG.ADR04.FADR03A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR04.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR09A DD DSN=&MIG.ADR04.FADR09A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR04
//*----------------------------------------------------------------
//PPRADR04 EXEC PGM=PPRADR04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR02  DD DISP=SHR,DSN=&MIG.ADR04.FADR02
//SJ03PA   DD DISP=SHR,DSN=&MIG.ADR04.J03PA
//SJ03BP   DD DISP=SHR,DSN=&MIG.ADR04.J03BLOC1
//SFADR03A DD DISP=SHR,DSN=&MIG.ADR04.FADR03A
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR03B DD DSN=&MIG.ADR04.FADR03B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFADR09A DD DSN=&MIG.ADR04.FADR09A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR02  DD DSN=&MIG.ADR04.FADR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03PA   DD DSN=&MIG.ADR04.J03PA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*ZJ03BP   DD DSN=&MIG.ADR04.J03BP.R,
//*            DISP=(NEW,CATLG,CATLG),
//*            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*            SPACE=(TRK,(200,100),RLSE)
//ZFADR03A DD DSN=&MIG.ADR04.FADR03A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR04.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR09A DD DSN=&MIG.ADR04.FADR09A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SN04ATT  DD DSN=&MIG.ADR05.N04ATT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SN04ANO  DD DSN=&MIG.ADR05.N04ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER N04ATT
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SN04ATT SORT N04-ATT-INC-C-ID-ATT-INCAP ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.N04ATTIN
//SORTIN   DD DISP=SHR,DSN=&SRC.N04ATTIN
//SORTOUT  DD DSN=&MIG.ADR05.N04ATT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER N04ANO
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SN04ANO SORT N04-ANO-AI-C-ID-ATT-INCAP ASC [21:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.N04ANOAI
//SORTIN   DD DISP=SHR,DSN=&SRC.N04ANOAI
//SORTOUT  DD DSN=&MIG.ADR05.N04ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR04  DD DSN=&MIG.ADR05.FADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZN04ATT  DD DSN=&MIG.ADR05.N04ATT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZN04ANO  DD DSN=&MIG.ADR05.N04ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR04  DD DSN=&MIG.ADR05.FADR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR05
//*----------------------------------------------------------------
//PPRADR05 EXEC PGM=PPRADR05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SN04ATT  DD DISP=SHR,DSN=&MIG.ADR05.N04ATT
//SN04ANO  DD DISP=SHR,DSN=&MIG.ADR05.N04ANO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR04  DD DSN=&MIG.ADR05.FADR04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZN04ATT  DD DSN=&MIG.ADR05.N04ATT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZN04ANO  DD DSN=&MIG.ADR05.N04ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR04  DD DSN=&MIG.ADR05.FADR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR04  DD DSN=&MIG.ADR06.FADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR03B DD DSN=&MIG.ADR06.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR04
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR04 SORT FADR04-C-ID-PD-ARRCO ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR04
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR05.FADR04
//SORTOUT  DD DSN=&MIG.ADR06.FADR04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR06.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR05  DD DSN=&MIG.ADR06.FADR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR06  DD DSN=&MIG.ADR06.FADR06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR04  DD DSN=&MIG.ADR06.FADR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR06.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR05  DD DSN=&MIG.ADR06.FADR05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR06  DD DSN=&MIG.ADR06.FADR06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR06
//*----------------------------------------------------------------
//PPRADR06 EXEC PGM=PPRADR06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR04  DD DISP=SHR,DSN=&MIG.ADR06.FADR04
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR06.FADR03B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR05  DD DSN=&MIG.ADR06.FADR05,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADR06  DD DSN=&MIG.ADR06.FADR06,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR04  DD DSN=&MIG.ADR06.FADR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR06.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR05  DD DSN=&MIG.ADR06.FADR05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR06  DD DSN=&MIG.ADR06.FADR06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR04  DD DSN=&MIG.ADR07.FADR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR03B DD DSN=&MIG.ADR07.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR06  DD DSN=&MIG.ADR07.FADR06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR04
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR04 SORT FADR04-C-ID-PD-TB ASC ç13:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR04
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR05.FADR04
//SORTOUT  DD DSN=&MIG.ADR07.FADR04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR07.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR06
//*----------------------------------------------------------------
//SORT0703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR06 SORT FADR06-C-ID-PD-TB ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR06
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR06.FADR06
//SORTOUT  DD DSN=&MIG.ADR07.FADR06,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR07  DD DSN=&MIG.ADR07.FADR07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR04  DD DSN=&MIG.ADR07.FADR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR07.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR06  DD DSN=&MIG.ADR07.FADR06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR07  DD DSN=&MIG.ADR07.FADR07.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR07
//*----------------------------------------------------------------
//PPRADR07 EXEC PGM=PPRADR07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR04  DD DISP=SHR,DSN=&MIG.ADR07.FADR04
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR07.FADR03B
//SFADR06  DD DISP=SHR,DSN=&MIG.ADR07.FADR06
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR07  DD DSN=&MIG.ADR07.FADR07,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR04  DD DSN=&MIG.ADR07.FADR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR07.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR06  DD DSN=&MIG.ADR07.FADR06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR07  DD DSN=&MIG.ADR07.FADR07.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SJ06TRF  DD DSN=&MIG.ADR08.J06TRF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR03B DD DSN=&MIG.ADR08.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)

//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PERS ASC ç239:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR08.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(239,12,CH,A,
               329,7,CH,A,
               236,2,CH,A)
//*----------------------------------------------------------------
//*TRI DU FICHIER J06TRF
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ06TRF SORT J06-PROC-TRANSF-AG-C-ID-PERS ASC [16:12ó
//* SJ06TRF SORT J06-PROC-TRANSF-AG-C-SENS-ECH ASC [51:1ó
//* SJ06TRF SORT J06-PROC-TRANSF-AG-D-CRE-DEM DESC [53:8ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J06PROC 
//SORTIN   DD DISP=SHR,DSN=&SRC.J06PROC
//SORTOUT  DD DSN=&MIG.ADR08.J06TRF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               51,1,CH,A,
               53,8,CH,D)
/*

/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR08  DD DSN=&MIG.ADR08.FADR08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.ADR08.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ06TRF  DD DSN=&MIG.ADR08.J06TRF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR08.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR08  DD DSN=&MIG.ADR08.FADR08.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR08
//*----------------------------------------------------------------
//PPRADR08 EXEC PGM=PPRADR08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*FINST    DD DISP=SHR,DSN=&TAB.FINST
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//SJ06TRF  DD DISP=SHR,DSN=&MIG.ADR08.J06TRF
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR08.FADR03B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR08  DD DSN=&MIG.ADR08.FADR08,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.ADR08.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ06TRF  DD DSN=&MIG.ADR08.J06TRF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR08.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR08  DD DSN=&MIG.ADR08.FADR08.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*SJ03BP   DD DSN=&MIG.ADR09.J03BP,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&MIG.ADR09.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STADHADH DD DSN=&MIG.ADR09.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAUE01 DD DSN=&MIG.ADR09.FPAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03BLOC DD DSN=&MIG.ADR09.J03BLOC2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR03B DD DSN=&MIG.ADR09.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR08.FADR03B
//SORTOUT  DD DSN=&MIG.ADR09.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(23,1,CH,A,
               24,9,CH,A,
               33,4,CH,A)
  SUM FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUEDADH
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-C-GRPT-GEST ASC ç101:1Ù
//* STUEDADH SORT TABUEDAD-C-ADH ASC ç102:9Ù
//* STUEDADH SORT TABUEDAD-C-ORDRE ASC ç111:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&MIG.ADR09.TUEDADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPAUE01
//*-------------------------------------------------------------
//SORT0903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPAUE01 SORT FPAUE01-C-GRPT-GEST ASC +38:1Ÿ
//* SFPAUE01 SORT FPAUE01-C-ADH ASC +39:9Ÿ
//* SFPAUE01 SORT FPAUE01-C-ORDRE ASC +48:4Ÿ
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPAUE01
//SORTIN   DD DISP=SHR,DSN=&CIB.FPAUE01
//SORTOUT  DD DSN=&MIG.ADR09.FPAUE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,1,CH,A,
               39,9,CH,A,
               48,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0904 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC ç94:1Ù
//* STADHADH SORT ADHSADH-C-ADH ASC ç95:9Ù
//* STADHADH SORT ADHSADH-C-ORDRE ASC ç104:4Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADR09.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI2 DU FICHIER J03BLOC
//*----------------------------------------------------------------
//SORT0905 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03BP SORT J03-BP-C-GRPT-GEST ASC [340:1ó
//* SJ03BP SORT J03-BP-C-ADH ASC [342:9ó
//* SJ03BP SORT J03-BP-C-ORDRE ASC [352:4ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03BLOC
//SORTIN   DD DISP=SHR,DSN=&SRC.J03BLOC
//SORTOUT  DD DSN=&MIG.ADR09.J03BLOC2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(5000,2500),RLSE),UNIT=(SYSDA,20)
//*           SPACE=(CYL,(2500,2100),RLSE)
//SYSIN    DD *
  SORT FIELDS=(340,1,CH,A,
               342,9,CH,A,
               352,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR09  DD DSN=&MIG.ADR09.FADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*ZJ03BP   DD DSN=&MIG.ADR09.J03BP.R,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.ADR09.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.ADR09.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR09  DD DSN=&MIG.ADR09.FADR09.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAUE01 DD DSN=&MIG.ADR09.FPAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR09
//*----------------------------------------------------------------
//PPRADR09 EXEC PGM=PPRADR09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SJ03BP   DD DISP=SHR,DSN=&MIG.ADR09.J03BLOC2
//STUEDADH DD DISP=SHR,DSN=&MIG.ADR09.TUEDADH
//SFPAUE01 DD DISP=SHR,DSN=&MIG.ADR09.FPAUE01
//STADHADH DD DISP=SHR,DSN=&MIG.ADR09.TADHADH
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR09.FADR03B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR09  DD DSN=&MIG.ADR09.FADR09,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE)
//*ZJ03BP   DD DSN=&MIG.ADR09.J03BP.R,
//*            DISP=(NEW,CATLG,CATLG),
//*            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.ADR09.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAUE01 DD DSN=&MIG.ADR09.FPAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.ADR09.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR09  DD DSN=&MIG.ADR09.FADR09.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR10.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.ADR10.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-CONTRAT ASC ç55:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR10.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(55,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CONT SORT A03-CONTRAT-C-ID-CONTRAT ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ADR10.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR10  DD DSN=&MIG.ADR10.FADR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR10.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ADR10.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR10  DD DSN=&MIG.ADR10.FADR10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR10
//*----------------------------------------------------------------
//PPRADR10 EXEC PGM=PPRADR10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR10.FADR03B
//SA03CONT DD DISP=SHR,DSN=&MIG.ADR10.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR10  DD DSN=&MIG.ADR10.FADR10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR10.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ADR10.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR10  DD DSN=&MIG.ADR10.FADR10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SN04CALC DD DSN=&MIG.ADR11.N04CALC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR09  DD DSN=&MIG.ADR11.FADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER N04CALC
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SN04CALC SORT N04-BASE-CALC-A-C-ID-CALC-INC ASC [42:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.N04BASEC
//SORTIN   DD DISP=SHR,DSN=&SRC.N04BASEC
//SORTOUT  DD DSN=&MIG.ADR11.N04CALC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(42,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR09
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR09 SORT FADR09-C-ID-JUSTIF ASC ç134:12Ù
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR09
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR09.FADR09
//SORTOUT  DD DSN=&MIG.ADR11.FADR09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(3000,1000),RLSE)
//SYSIN    DD *
  SORT FIELDS=(134,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR11  DD DSN=&MIG.ADR11.FADR11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZN04CALC DD DSN=&MIG.ADR11.N04CALC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR09  DD DSN=&MIG.ADR11.FADR09.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR11  DD DSN=&MIG.ADR11.FADR11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR11
//*----------------------------------------------------------------
//PPRADR11 EXEC PGM=PPRADR11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SN04CALC DD DISP=SHR,DSN=&MIG.ADR11.N04CALC
//SFADR09  DD DISP=SHR,DSN=&MIG.ADR11.FADR09
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR11  DD DSN=&MIG.ADR11.FADR11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZN04CALC DD DSN=&MIG.ADR11.N04CALC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR09  DD DSN=&MIG.ADR11.FADR09.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR11  DD DSN=&MIG.ADR11.FADR11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR09  DD DSN=&MIG.ADR12.FADR09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR09A DD DSN=&MIG.ADR12.FADR09A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR11  DD DSN=&MIG.ADR12.FADR11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03ANO  DD DSN=&MIG.ADR12.J03ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR09
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR09 SORT FADR09-C-ID-BP ASC ç1:12Ù
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR09
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR09.FADR09
//SORTOUT  DD DSN=&MIG.ADR12.FADR09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(100,100),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR09A
//*----------------------------------------------------------------
//SORT1202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR09A SORT FADR09A-C-ID-BP ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR09A
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR09A
//SORTOUT  DD DSN=&MIG.ADR12.FADR09A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR11
//*----------------------------------------------------------------
//SORT1203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR11 SORT FADR11-C-ID-BP ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR11
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR11.FADR11
//SORTOUT  DD DSN=&MIG.ADR12.FADR11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03ANO
//*----------------------------------------------------------------
//SORT1204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03ANO SORT J03-ANOMALIE-BP-C-ID-BP ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03ANOBP
//SORTIN   DD DUMMY
//*SORTIN   DD DISP=SHR,DSN=&SRC.J03ANOBP
//SORTOUT  DD DSN=&MIG.ADR12.J03ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR12.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFANO DD DSN=&MIG.ADR12.FREFANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR09  DD DSN=&MIG.ADR12.FADR09.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR09A DD DSN=&MIG.ADR12.FADR09A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR11  DD DSN=&MIG.ADR12.FADR11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03ANO  DD DSN=&MIG.ADR12.J03ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR12.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR12
//*----------------------------------------------------------------
//PPRADR12 EXEC PGM=PPRADR12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FREFANO  DD DUMMY
//*FREFANO  DD DISP=SHR,DSN=&TABVH.J21RCAR
//SFADR09  DD DISP=SHR,DSN=&MIG.ADR12.FADR09
//SFADR09A DD DISP=SHR,DSN=&MIG.ADR12.FADR09A
//SFADR11  DD DISP=SHR,DSN=&MIG.ADR12.FADR11
//SJ03ANO  DD DISP=SHR,DSN=&MIG.ADR12.J03ANO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR12  DD DSN=&MIG.ADR12.FADR12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(CYL,(500,1000),RLSE)
//ZFREFANO DD DSN=&MIG.ADR12.FREFANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR09  DD DSN=&MIG.ADR12.FADR09.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR09A DD DSN=&MIG.ADR12.FADR09A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR11  DD DSN=&MIG.ADR12.FADR11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03ANO  DD DSN=&MIG.ADR12.J03ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR12.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR35
//*----------------------------------------------------------------
//DEL3501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR35.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR12A DD DSN=&MIG.ADR35.FADR12A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT3501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR35.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12A
//*----------------------------------------------------------------
//SORT3502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12A SORT FADR12-C-ID-PD ASC ç13:12Ù
//* SFADR12A SORT FADR12-D-DEB-PER ASC ç29:8Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR12.FADR12
//SORTOUT  DD DSN=&MIG.ADR35.FADR12A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(10000,10000),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               29,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR35.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR35.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12A DD DSN=&MIG.ADR35.FADR12A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR35.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR35
//*----------------------------------------------------------------
//PPRADR35 EXEC PGM=PPRADR35,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR35.FADR03B
//SFADR12A DD DISP=SHR,DSN=&MIG.ADR35.FADR12A
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR12  DD DSN=&MIG.ADR35.FADR12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR35.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12A DD DSN=&MIG.ADR35.FADR12A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR35.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR13.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PERS ASC ç239:12Ù
//* SFADR03B SORT FADR03B-C-SIREN ASC ç317:9Ù
//* SFADR03B SORT FADR03B-D-DEB-PER ASC ç68:8Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR13.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(239,12,CH,A,
               317,9,CH,A,
               76,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR13  DD DSN=&MIG.ADR13.FADR13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR13.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR13  DD DSN=&MIG.ADR13.FADR13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR13
//*----------------------------------------------------------------
//PPRADR13 EXEC PGM=PPRADR13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR13.FADR03B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR13  DD DSN=&MIG.ADR13.FADR13,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR13.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR13  DD DSN=&MIG.ADR13.FADR13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR25
//*----------------------------------------------------------------
//DEL2501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR25.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR12  DD DSN=&MIG.ADR25.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT2501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//*SORTIN   DD DISP=SHR,DSN=&MIG.ADR20.FADR21
//SORTOUT  DD DSN=&MIG.ADR25.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12
//*----------------------------------------------------------------
//SORT2502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12 SORT FADR12-C-ID-PD ASC ç13:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR35.FADR12
//*SORTIN   DD DISP=SHR,DSN=&MIG.ADR12.FADR12
//*SORTIN   DD DISP=SHR,DSN=&MIG.ADR20.FADR20
//SORTOUT  DD DSN=&MIG.ADR25.FADR12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR25A DD DSN=&MIG.ADR25.FADR25A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR25B DD DSN=&MIG.ADR25.FADR25B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR25.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR25.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25A DD DSN=&MIG.ADR25.FADR25A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25B DD DSN=&MIG.ADR25.FADR25B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR25
//*----------------------------------------------------------------
//PPRADR25 EXEC PGM=PPRADR25,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR25.FADR03B
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR25.FADR12
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR25A DD DSN=&MIG.ADR25.FADR25A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADR25B DD DSN=&MIG.ADR25.FADR25B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR25.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR25.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR25A DD DSN=&MIG.ADR25.FADR25A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR25B DD DSN=&MIG.ADR25.FADR25B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR26
//*----------------------------------------------------------------
//DEL3201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR32.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12
//*----------------------------------------------------------------
//SORT3201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12 SORT FADR12-C-ID-PD ASC  13:12ì
//* SFADR12 SORT FADR12-Q-EXE DESC 25:4ì
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR25.FADR12
//SORTOUT  DD DSN=&MIG.ADR32.FADR12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,25,4,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12C DD DSN=&MIG.ADR32.FADR12C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR32.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR32.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12C DD DSN=&MIG.ADR32.FADR12C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR32
//*----------------------------------------------------------------
//PPRADR32 EXEC PGM=PPRADR32,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR25.FADR03B
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR32.FADR12
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR12C DD DSN=&MIG.ADR32.FADR12C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR32.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR32.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12C DD DSN=&MIG.ADR32.FADR12C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR33
//*----------------------------------------------------------------
//DEL3301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR33A DD DSN=&MIG.ADR33.FADR33A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR33B DD DSN=&MIG.ADR33.FADR33B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR33C DD DSN=&MIG.ADR33.FADR33C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR33.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR33.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33A DD DSN=&MIG.ADR33.FADR33A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33B DD DSN=&MIG.ADR33.FADR33B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33C DD DSN=&MIG.ADR33.FADR33C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR33
//*----------------------------------------------------------------
//PPRADR33 EXEC PGM=PPRADR33,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR25.FADR03B
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR25.FADR12
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR33A DD DSN=&MIG.ADR33.FADR33A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADR33B DD DSN=&MIG.ADR33.FADR33B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFADR33C DD DSN=&MIG.ADR33.FADR33C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR33.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR33.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33A DD DSN=&MIG.ADR33.FADR33A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33B DD DSN=&MIG.ADR33.FADR33B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33C DD DSN=&MIG.ADR33.FADR33C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR34
//*----------------------------------------------------------------
//DEL3401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR33A DD DSN=&MIG.ADR34.FADR33A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR33B DD DSN=&MIG.ADR34.FADR33B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR33C DD DSN=&MIG.ADR34.FADR33C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR33A
//*----------------------------------------------------------------
//SORT3401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR33A SORT A-FADR25-C-ID-PERS ASC  1:12ì
//* SFADR33A SORT A-FADR25-C-SIREN ASC  25:9ì
//* SFADR33A SORT A-FADR25-C-NIC ASC  34:5ì
//* SFADR33A SORT A-FADR25-C-CPN ASC  39:3ì
//* SFADR33A SORT A-FADR25-EXE ASC  87:4ì
//* SFADR33A SORT A-FADR25-C-TRANCHE-LC ASC  70:1ì
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR33.FADR33A
//SORTOUT  DD DSN=&MIG.ADR34.FADR33A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,9,CH,A,
               34,5,CH,A,
               39,3,CH,A,
               87,4,CH,A,
               70,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR33B
//*----------------------------------------------------------------
//SORT3402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR33B SORT B-FADR25-C-ID-PERS ASC  1:12ì
//* SFADR33B SORT B-FADR25-C-SIREN ASC  25:9ì
//* SFADR33B SORT B-FADR25-C-NIC ASC  34:5ì
//* SFADR33B SORT B-FADR25-C-CPN ASC  39:3ì
//* SFADR33B SORT B-FADR25-EXE ASC  87:4ì
//* SFADR33B SORT B-FADR25-C-TRANCHE-LC ASC  70:1ì
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR33.FADR33B
//SORTOUT  DD DSN=&MIG.ADR34.FADR33B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,9,CH,A,
               34,5,CH,A,
               39,3,CH,A,
               87,4,CH,A,
               70,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR33C
//*----------------------------------------------------------------
//SORT3403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR33C SORT C-FADR25-C-ID-PERS ASC  1:12ì
//* SFADR33C SORT C-FADR25-C-SIREN ASC  25:9ì
//* SFADR33C SORT C-FADR25-C-NIC ASC  34:5ì
//* SFADR33C SORT C-FADR25-C-CPN ASC  39:3ì
//* SFADR33C SORT C-FADR25-EXE ASC  87:4ì
//* SFADR33C SORT C-FADR25-C-TRANCHE-LC ASC  70:1ì
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR33.FADR33C
//SORTOUT  DD DSN=&MIG.ADR34.FADR33C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,9,CH,A,
               34,5,CH,A,
               39,3,CH,A,
               87,4,CH,A,
               70,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR33D DD DSN=&MIG.ADR34.FADR33D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33A DD DSN=&MIG.ADR34.FADR33A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33B DD DSN=&MIG.ADR34.FADR33B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33C DD DSN=&MIG.ADR34.FADR33C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33D DD DSN=&MIG.ADR34.FADR33D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR34
//*----------------------------------------------------------------
//PPRADR34 EXEC PGM=PPRADR34,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR33A DD DISP=SHR,DSN=&MIG.ADR34.FADR33A
//SFADR33B DD DISP=SHR,DSN=&MIG.ADR34.FADR33B
//SFADR33C DD DISP=SHR,DSN=&MIG.ADR34.FADR33C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR33D DD DSN=&MIG.ADR34.FADR33D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33A DD DSN=&MIG.ADR34.FADR33A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33B DD DSN=&MIG.ADR34.FADR33B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33C DD DSN=&MIG.ADR34.FADR33C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33D DD DSN=&MIG.ADR34.FADR33D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR26
//*----------------------------------------------------------------
//DEL2601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR25A DD DSN=&MIG.ADR26.FADR25A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR25B DD DSN=&MIG.ADR26.FADR25B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR25A
//*----------------------------------------------------------------
//SORT2601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR25A SORT A-FADR25-C-ID-PERS ASC [1:12ó
//* SFADR25A SORT A-FADR25-C-SIREN ASC [25:9ó
//* SFADR25A SORT A-FADR25-C-NIC ASC [34:5ó
//* SFADR25A SORT A-FADR25-C-CPN ASC [39:3ó
//* SFADR25A SORT A-FADR25-EXE ASC [87:4ó
//* SFADR25A SORT A-FADR25-C-TRANCHE-LC ASC [70:1ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR25.FADR25A
//SORTOUT  DD DSN=&MIG.ADR26.FADR25A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,9,CH,A,
               34,5,CH,A,
               39,3,CH,A,
               87,4,CH,A,
               70,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR25B
//*----------------------------------------------------------------
//SORT2602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR25B SORT B-FADR25-C-ID-PERS ASC [1:12ó
//* SFADR25B SORT B-FADR25-C-SIREN ASC [25:9ó
//* SFADR25B SORT B-FADR25-C-NIC ASC [34:5ó
//* SFADR25B SORT B-FADR25-C-CPN ASC [39:3ó
//* SFADR25B SORT B-FADR25-EXE ASC [87:4ó
//* SFADR25B SORT B-FADR25-C-TRANCHE-LC ASC [70:1ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR25.FADR25B
//SORTOUT  DD DSN=&MIG.ADR26.FADR25B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,9,CH,A,
               34,5,CH,A,
               39,3,CH,A,
               87,4,CH,A,
               70,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR25C DD DSN=&MIG.ADR26.FADR25C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25A DD DSN=&MIG.ADR26.FADR25A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25B DD DSN=&MIG.ADR26.FADR25B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25C DD DSN=&MIG.ADR26.FADR25C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR26
//*----------------------------------------------------------------
//PPRADR26 EXEC PGM=PPRADR26,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR25A DD DISP=SHR,DSN=&MIG.ADR26.FADR25A
//SFADR25B DD DISP=SHR,DSN=&MIG.ADR26.FADR25B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR25C DD DSN=&MIG.ADR26.FADR25C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR25A DD DSN=&MIG.ADR26.FADR25A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR25B DD DSN=&MIG.ADR26.FADR25B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR25C DD DSN=&MIG.ADR26.FADR25C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR27
//*----------------------------------------------------------------
//DEL2701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR25C DD DSN=&MIG.ADR27.FADR25C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR33D DD DSN=&MIG.ADR27.FADR33D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR12A DD DSN=&MIG.ADR27.FADR12A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR25C
//*----------------------------------------------------------------
//SORT2701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR25C SORT C-FADR25-C-ID-BP ASC [58:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR26.FADR25C
//SORTOUT  DD DSN=&MIG.ADR27.FADR25C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(58,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR33D
//*----------------------------------------------------------------
//SORT2702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR33D SORT D-FADR25-C-ID-BP ASC  58:12ì
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR25
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR34.FADR33D
//SORTOUT  DD DSN=&MIG.ADR27.FADR33D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(58,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12A
//*----------------------------------------------------------------
//SORT2703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12A SORT FADR12-C-ID-BP ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR32.FADR12C
//SORTOUT  DD DSN=&MIG.ADR27.FADR12A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12B DD DSN=&MIG.ADR27.FADR12B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR25C DD DSN=&MIG.ADR27.FADR25C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR33D DD DSN=&MIG.ADR27.FADR33D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12A DD DSN=&MIG.ADR27.FADR12A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12B DD DSN=&MIG.ADR27.FADR12B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR27
//*----------------------------------------------------------------
//PPRADR27 EXEC PGM=PPRADR27,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR25C DD DISP=SHR,DSN=&MIG.ADR27.FADR25C
//SFADR33D DD DISP=SHR,DSN=&MIG.ADR27.FADR33D
//SFADR12A DD DISP=SHR,DSN=&MIG.ADR27.FADR12A
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR12B DD DSN=&MIG.ADR27.FADR12B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//ZFADR25C DD DSN=&MIG.ADR27.FADR25C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR33D DD DSN=&MIG.ADR27.FADR33D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12A DD DSN=&MIG.ADR27.FADR12A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12B DD DSN=&MIG.ADR27.FADR12B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR28
//*----------------------------------------------------------------
//DEL2801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR28.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12
//*----------------------------------------------------------------
//SORT2801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12 SORT FADR12-C-ID-PERS ASC ç178:12Ù
//* SFADR12 SORT FADR12-Q-EXE ASC ç25:4Ù
//* SFADR12 SORT FADR12-C-GRPT-GEST ASC ç152:1Ù
//* SFADR12 SORT FADR12-C-ADH ASC ç153:9Ù
//* SFADR12 SORT FADR12-C-ORDRE ASC ç162:4Ù
//* SFADR12 SORT FADR12-C-TYPE-TP ASC ç112:1Ù
//* SFADR12 SORT FADR12-D-DEB-PER ASC ç29:8Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR27.FADR12B
//SORTOUT  DD DSN=&MIG.ADR28.FADR12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(178,12,CH,A,
               25,4,CH,A,
               152,1,CH,A,
               153,9,CH,A,
               162,4,CH,A,
               112,1,CH,A,
               29,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR28  DD DSN=&MIG.ADR28.FADR28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR28.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR28  DD DSN=&MIG.ADR28.FADR28.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR28
//*----------------------------------------------------------------
//PPRADR28 EXEC PGM=PPRADR28,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR28.FADR12
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR28  DD DSN=&MIG.ADR28.FADR28,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR28.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR28  DD DSN=&MIG.ADR28.FADR28.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR30
//*----------------------------------------------------------------
//DEL3001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR30.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03PERA DD DSN=&MIG.ADR30.J03PERA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12
//*----------------------------------------------------------------
//SORT3001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12 SORT FADR12-C-ID-PA ASC ç166:12Ù
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR27.FADR12B
//SORTOUT  DD DSN=&MIG.ADR30.FADR12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(166,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03PERA
//*----------------------------------------------------------------
//SORT3002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03PERA SORT J03-PER-AFFIL-C-ID-PA ASC ç345:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PERAF
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERAF
//SORTOUT  DD DSN=&MIG.ADR30.J03PERA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(345,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR30  DD DSN=&MIG.ADR30.FADR30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR30.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03PERA DD DSN=&MIG.ADR30.J03PERA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR30  DD DSN=&MIG.ADR30.FADR30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR30
//*----------------------------------------------------------------
//PPRADR30 EXEC PGM=PPRADR30,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR30.FADR12
//SJ03PERA DD DISP=SHR,DSN=&MIG.ADR30.J03PERA
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR30  DD DSN=&MIG.ADR30.FADR30,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR30.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03PERA DD DSN=&MIG.ADR30.J03PERA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR30  DD DSN=&MIG.ADR30.FADR30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR31
//*----------------------------------------------------------------
//DEL3101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR30  DD DSN=&MIG.ADR31.FADR30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR30
//*----------------------------------------------------------------
//SORT3101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR30 SORT FADR30-C-ID-PERS ASC ç1:12Ù
//* SFADR30 SORT FADR30-C-SIREN ASC ç13:9Ù
//* SFADR30 SORT FADR30-DATFIN-BP ASC ç57:8Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR30
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR30.FADR30
//SORTOUT  DD DSN=&MIG.ADR31.FADR30,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,9,CH,A,
               57,8,CH,A)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR13B DD DSN=&MIG.ADR31.FADR13B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR30  DD DSN=&MIG.ADR31.FADR30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR13B DD DSN=&MIG.ADR31.FADR13B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR31
//*----------------------------------------------------------------
//PPRADR31 EXEC PGM=PPRADR31,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR30  DD DISP=SHR,DSN=&MIG.ADR31.FADR30
//*SFADR01D DD DISP=SHR,DSN=&MIG.ADR31.FADR01D
//SFADR01D DD DUMMY
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR13B DD DSN=&MIG.ADR31.FADR13B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR30  DD DSN=&MIG.ADR31.FADR30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR13B DD DSN=&MIG.ADR31.FADR13B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR36
//*----------------------------------------------------------------
//DEL3601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR36.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR12A DD DSN=&MIG.ADR36.FADR12A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT3601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR36.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12A
//*----------------------------------------------------------------
//SORT3602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12A SORT FADR12-C-ID-PD ASC [13:12ó
//* SFADR12A SORT FADR12-C-TYPE-BP ASC [45:3ó
//* SFADR12A SORT FADR12-C-TRANCHE-PTS ASC [128:3ó
//* SFADR12A SORT FADR12-D-DEB-PER ASC [29:8ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR27.FADR12B
//SORTOUT  DD DSN=&MIG.ADR36.FADR12A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               45,3,CH,A,
               128,3,CH,A,
               29,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR12  DD DSN=&MIG.ADR36.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR36.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12A DD DSN=&MIG.ADR36.FADR12A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR36.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR36
//*----------------------------------------------------------------
//PPRADR36 EXEC PGM=PPRADR36,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR36.FADR03B
//SFADR12A DD DISP=SHR,DSN=&MIG.ADR36.FADR12A
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR12  DD DSN=&MIG.ADR36.FADR12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR36.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12A DD DSN=&MIG.ADR36.FADR12A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR36.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR29
//*----------------------------------------------------------------
//DEL2901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR29.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03ANO  DD DSN=&MIG.ADR29.J03ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B
//*----------------------------------------------------------------
//SORT2901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PERS ASC [239:12ó
//* SFADR03B SORT FADR03B-C-GRPT-GEST ASC [23:1ó
//* SFADR03B SORT FADR03B-C-ADH ASC [24:9ó
//* SFADR03B SORT FADR03B-C-ORDRE ASC [33:4ó
//* SFADR03B SORT FADR03B-D-DEB-PER ASC [68:8ó
//* SFADR03B SORT FADR03B-D-FIN-PER ASC [76:8ó
//* SFADR03B SORT FADR03B-C-LC-AGIRC ASC [258:3ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR04.FADR03B
//SORTOUT  DD DSN=&MIG.ADR29.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(239,12,CH,A,
               23,1,CH,A,
               24,9,CH,A,
               33,4,CH,A,
               68,8,CH,A,
               76,8,CH,A,
               258,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03ANO
//*----------------------------------------------------------------
//SORT2902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03ANO SORT J03-ANOMALIE-PD-C-ID-PD ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03ANOPD
//SORTIN   DD DISP=SHR,DSN=&SRC.J03ANOPD
//SORTOUT  DD DSN=&MIG.ADR29.J03ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(53,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR29  DD DSN=&MIG.ADR29.FADR29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR29.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03ANO  DD DSN=&MIG.ADR29.J03ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR29  DD DSN=&MIG.ADR29.FADR29.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR29
//*----------------------------------------------------------------
//PPRADR29 EXEC PGM=PPRADR29,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR29.FADR03B
//SJ03ANO  DD DISP=SHR,DSN=&MIG.ADR29.J03ANO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADR29  DD DSN=&MIG.ADR29.FADR29,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR29.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR29  DD DSN=&MIG.ADR29.FADR29.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADR03B DD DSN=&MIG.ADR14.FADR03B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR05  DD DSN=&MIG.ADR14.FADR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR07  DD DSN=&MIG.ADR14.FADR07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR08  DD DSN=&MIG.ADR14.FADR08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR10  DD DSN=&MIG.ADR14.FADR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR12  DD DSN=&MIG.ADR14.FADR12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR13  DD DSN=&MIG.ADR14.FADR13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ03ANO  DD DSN=&MIG.ADR14.J03ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR28  DD DSN=&MIG.ADR14.FADR28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR29  DD DSN=&MIG.ADR14.FADR29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADR13B DD DSN=&MIG.ADR14.FADR13B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STNPM0CF DD DSN=&MIG.ADR14.TNPM0CF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR03B - TRI{ AVANT POUR PPRADR36 MAINTENANT
//*REMIS EN PLACE DND-027
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR03B SORT FADR03B-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR03B
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR29.FADR03B
//SORTOUT  DD DSN=&MIG.ADR14.FADR03B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR05
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR05 SORT FADR05-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR05
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR06.FADR05
//SORTOUT  DD DSN=&MIG.ADR14.FADR05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR07
//*----------------------------------------------------------------
//SORT1403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR07 SORT FADR07-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR07
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR07.FADR07
//SORTOUT  DD DSN=&MIG.ADR14.FADR07,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR08
//*----------------------------------------------------------------
//SORT1404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR08 SORT FADR08-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR08
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR08.FADR08
//SORTOUT  DD DSN=&MIG.ADR14.FADR08,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR10
//*----------------------------------------------------------------
//SORT1405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR10 SORT FADR10-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR10
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR10.FADR10
//SORTOUT  DD DSN=&MIG.ADR14.FADR10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR12 - TRIE EN SORTIE DU PPRADR36 MAINTENANT
//*----------------------------------------------------------------
//SORT1406 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR12 SORT FADR12-C-ID-PD ASC [13:12ó
//* SFADR12 SORT FADR12-Q-EXE ASC [25:4ó
//* FIN   CRITERE XGEN
//*SORTWK01 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK02 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK03 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK04 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK05 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK06 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK07 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK08 DD    SPACE=(CYL,(2000,500),RLSE)
//*SORTWK09 DD    SPACE=(CYL,(2000,500),RLSE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR36.FADR12
//SORTOUT  DD DSN=&MIG.ADR14.FADR12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(100,100),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               29,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR13
//*----------------------------------------------------------------
//SORT1407 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR13 SORT FADR13-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR13
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR13.FADR13
//SORTOUT  DD DSN=&MIG.ADR14.FADR13,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J03ANO
//*----------------------------------------------------------------
//SORT1408 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ03ANO SORT J03-ANOMALIE-PD-C-ID-PD ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03ANOPD
//SORTIN   DD DISP=SHR,DSN=&SRC.J03ANOPD
//SORTOUT  DD DSN=&MIG.ADR14.J03ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR28
//*----------------------------------------------------------------
//SORT1409 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR10 SORT FADR28-C-ID-PD ASC ç31:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR28
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR28.FADR28
//SORTOUT  DD DSN=&MIG.ADR14.FADR28,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(31,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR29
//*----------------------------------------------------------------
//SORT1410 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR10 SORT FADR29-C-ID-PD ASC [27:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR29
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR29.FADR29
//SORTOUT  DD DSN=&MIG.ADR14.FADR29,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(27,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR13B
//*----------------------------------------------------------------
//SORT1410 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR10 SORT FADR13B-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADR13
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR31.FADR13B
//SORTOUT  DD DSN=&MIG.ADR14.FADR13B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,13,8,CH,A)
    SUM FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADR13B
//*----------------------------------------------------------------
//SORT1411 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADR10 SORT FADR13B-C-ID-PD ASC [1:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TNPM0CF
//SORTIN   DD DISP=SHR,DSN=&TAB.TNPM0CF
//SORTOUT  DD DSN=&MIG.ADR14.TNPM0CF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(12,9,CH,A)
    SUM FIELDS=NONE
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVAD1 DD DSN=&MIG.ADR14.FPIVAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPADR01 DD DSN=&CIB.FPADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFK25 DD DSN=&MIG.ADR14.FREFK25.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.ADR14.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFDR1 DD DSN=&MIG.ADR14.FREFDR1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFDR2 DD DSN=&MIG.ADR14.FREFDR2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFANO DD DSN=&MIG.ADR14.FREFANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREFBAC DD DSN=&MIG.ADR14.FREFBAC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR03B DD DSN=&MIG.ADR14.FADR03B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR05  DD DSN=&MIG.ADR14.FADR05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR07  DD DSN=&MIG.ADR14.FADR07.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR08  DD DSN=&MIG.ADR14.FADR08.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR10  DD DSN=&MIG.ADR14.FADR10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR12  DD DSN=&MIG.ADR14.FADR12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR13  DD DSN=&MIG.ADR14.FADR13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ03ANO  DD DSN=&MIG.ADR14.J03ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR28  DD DSN=&MIG.ADR14.FADR28.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR29  DD DSN=&MIG.ADR14.FADR29.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADR13B DD DSN=&MIG.ADR14.FADR13B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR14.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADR01 DD DSN=&MIG.ADR14.FPADR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR14
//*----------------------------------------------------------------
//PPRADR14 EXEC PGM=PPRADR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FREFK25  DD DISP=SHR,DSN=&TAB.N04K25
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//FREFDR1  DD DISP=SHR,DSN=&TAB.REFDR1
//FREFDR2  DD DISP=SHR,DSN=&TAB.REFDR2
//* code mort
//*FREFANO  DD DISP=SHR,DSN=&TAB.J21RCAR
//FREFANO  DD DUMMY
//J21PROF  DD DISP=SHR,DSN=&SRC.J21PROFI
//FREFBAC  DD DISP=SHR,DSN=&SRC.J03RFBAC
//SFADR03B DD DISP=SHR,DSN=&MIG.ADR14.FADR03B
//SFADR05  DD DISP=SHR,DSN=&MIG.ADR14.FADR05
//SFADR07  DD DISP=SHR,DSN=&MIG.ADR14.FADR07
//SFADR08  DD DISP=SHR,DSN=&MIG.ADR14.FADR08
//SFADR10  DD DISP=SHR,DSN=&MIG.ADR14.FADR10
//*SFADR12  DD DISP=SHR,DSN=&MIG.ADR14.FADR12
//SFADR12  DD DISP=SHR,DSN=&MIG.ADR14.FADR12
//SFADR13  DD DISP=SHR,DSN=&MIG.ADR14.FADR13
//SJ03ANO  DD DISP=SHR,DSN=&MIG.ADR14.J03ANO
//SFADR28  DD DISP=SHR,DSN=&MIG.ADR14.FADR28
//SFADR29  DD DISP=SHR,DSN=&MIG.ADR14.FADR29
//SFADR13B DD DISP=SHR,DSN=&MIG.ADR14.FADR13B
//L01GPEI  DD DISP=SHR,DSN=&SRC.L01GPEIN
//PTSMALA  DD DISP=SHR,DSN=&TAB.TPTSMALA
//* GCA temporaire BBRUTEX  DD DISP=SHR,DSN=&TAB.BBRUTEX
//BBRUTEX  DD DUMMY
//TCPICPN  DD DISP=SHR,DSN=&TAB.TCPICPN
//TNPM0CF  DD DISP=SHR,DSN=&MIG.ADR14.TNPM0CF
//TXARRCO  DD DISP=SHR,DSN=&PARTXA.TXARRCO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIVAD1 DD DSN=&MIG.ADR14.FPIVAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE)
//SFPADR01 DD DSN=&CIB.FPADR01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREFK25 DD DSN=&MIG.ADR14.FREFK25.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.ADR14.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREFDR1 DD DSN=&MIG.ADR14.FREFDR1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREFDR2 DD DSN=&MIG.ADR14.FREFDR2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREFANO DD DSN=&MIG.ADR14.FREFANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREFBAC DD DSN=&MIG.ADR14.FREFBAC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR03B DD DSN=&MIG.ADR14.FADR03B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR05  DD DSN=&MIG.ADR14.FADR05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR07  DD DSN=&MIG.ADR14.FADR07.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR08  DD DSN=&MIG.ADR14.FADR08.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR10  DD DSN=&MIG.ADR14.FADR10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR12  DD DSN=&MIG.ADR14.FADR12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR13  DD DSN=&MIG.ADR14.FADR13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ03ANO  DD DSN=&MIG.ADR14.J03ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR28  DD DSN=&MIG.ADR14.FADR28.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR29  DD DSN=&MIG.ADR14.FADR29.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADR13B DD DSN=&MIG.ADR14.FADR13B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR14.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADR01 DD DSN=&MIG.ADR14.FPADR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR21
//*----------------------------------------------------------------
//DEL2101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIV01  DD DSN=&MIG.ADR21.FPIV01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIV02  DD DSN=&MIG.ADR21.FPIV02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIV03  DD DSN=&MIG.ADR21.FPIV03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIV04  DD DSN=&MIG.ADR21.FPIV04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV01  DD DSN=&MIG.ADR21.FPIV01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV02  DD DSN=&MIG.ADR21.FPIV02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV03  DD DSN=&MIG.ADR21.FPIV03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV04  DD DSN=&MIG.ADR21.FPIV04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR21.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR21
//*----------------------------------------------------------------
//PPRADR21 EXEC PGM=PPRADR21,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FPIVAD1  DD DISP=SHR,DSN=&MIG.ADR14.FPIVAD1
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIV01  DD DSN=&MIG.ADR21.FPIV01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFPIV02  DD DSN=&MIG.ADR21.FPIV02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFPIV03  DD DSN=&MIG.ADR21.FPIV03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFPIV04  DD DSN=&MIG.ADR21.FPIV04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//ZFPIV01  DD DSN=&MIG.ADR21.FPIV01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV02  DD DSN=&MIG.ADR21.FPIV02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV03  DD DSN=&MIG.ADR21.FPIV03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV04  DD DSN=&MIG.ADR21.FPIV04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR21.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR22
//*----------------------------------------------------------------
//DEL2201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIV04  DD DSN=&MIG.ADR22.FPIV04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPIV04
//*----------------------------------------------------------------
//SORT2201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPIV04 SORT WFPIVAD0-ID-TECH-INDV ASC ç1:9Ù
//* SFPIV04 SORT WFPIVAD0-CLE2 ASC ç10:91Ù
//* SFPIV04 SORT WFPIVAD0-CO-REG-BASE ASC ç101:3Ù
//* SFPIV04 SORT WFPIVAD0-CO-STUT-CAT ASC ç104:2Ù
//* SFPIV04 SORT WFPIVAD0-ENR(197:10) ASC ç307:10Ù
//* SFPIV04 SORT WFPIVAD0-ENR(207:10) ASC ç317:10Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0796
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR21.FPIV04
//SORTOUT  DD DSN=&MIG.ADR22.FPIV04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A,
               10,91,CH,A,
               101,3,CH,A,
               104,2,CH,A,
               307,10,CH,A,
               317,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIV04B DD DSN=&MIG.ADR22.FPIV04B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV04  DD DSN=&MIG.ADR22.FPIV04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV04B DD DSN=&MIG.ADR22.FPIV04B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR22
//*----------------------------------------------------------------
//PPRADR22 EXEC PGM=PPRADR22,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPIV04  DD DISP=SHR,DSN=&MIG.ADR22.FPIV04
//TNPM0CF  DD DISP=SHR,DSN=&MIG.ADR14.TNPM0CF
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIV04B DD DSN=&MIG.ADR22.FPIV04B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//ZFPIV04  DD DSN=&MIG.ADR22.FPIV04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV04B DD DSN=&MIG.ADR22.FPIV04B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR23
//*----------------------------------------------------------------
//DEL2301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVCO  DD DSN=&MIG.ADR23.FPIVCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPIVCO
//*----------------------------------------------------------------
//SORT2301 EXEC PGM=SORT,COND=(0,NE)

//* DEBUT CRITERE XGEN
//* SFPIVCO SORT WFPIVAD0-ID-TECH-INDV ASC [1:9ó
//* SFPIVCO SORT WFPIVAD0-CLE2 ASC [10:91ó
//* SFPIVCO SORT WFPIVAD0-CO-REG-BASE ASC [101:3ó
//* SFPIVCO SORT WFPIVAD0-CO-STUT-CAT ASC [104:2ó
//* SFPIVCO SORT WFPIVAD0-TRI ASC [106:1ó
//* SFPIVCO SORT WFPIVAD0-ENR(207:10) DESC [317:10ó
//* SFPIVCO SORT WFPIVAD0-ENR(197:10) DESC [307:10ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0796
//SORTIN01 DD DISP=SHR,DSN=&MIG.ADR21.FPIV02
//SORTIN02 DD DISP=SHR,DSN=&MIG.ADR21.FPIV03
//SORTIN03 DD DISP=SHR,DSN=&MIG.ADR22.FPIV04B
//SORTOUT  DD DSN=&MIG.ADR23.FPIVCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,5000),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A,
               10,91,CH,A,
               101,3,CH,A,
               104,2,CH,A,
               106,1,CH,A,
               317,10,CH,D,
               307,10,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIV02C DD DSN=&MIG.ADR23.FPIV02C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIV03C DD DSN=&MIG.ADR23.FPIV03C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIV04C DD DSN=&MIG.ADR23.FPIV04C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVCO  DD DSN=&MIG.ADR23.FPIVCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV02C DD DSN=&MIG.ADR23.FPIV02C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV03C DD DSN=&MIG.ADR23.FPIV03C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIV04C DD DSN=&MIG.ADR23.FPIV04C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR23
//*----------------------------------------------------------------
//PPRADR23 EXEC PGM=PPRADR23,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPIVCO  DD DISP=SHR,DSN=&MIG.ADR23.FPIVCO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIV02C DD DSN=&MIG.ADR23.FPIV02C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFPIV03C DD DSN=&MIG.ADR23.FPIV03C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE)
//SFPIV04C DD DSN=&MIG.ADR23.FPIV04C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//ZFPIVCO  DD DSN=&MIG.ADR23.FPIVCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV02C DD DSN=&MIG.ADR23.FPIV02C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV03C DD DSN=&MIG.ADR23.FPIV03C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIV04C DD DSN=&MIG.ADR23.FPIV04C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR24
//*----------------------------------------------------------------
//DEL2401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVTO  DD DSN=&MIG.ADR24.FPIVTO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPIVTO
//*----------------------------------------------------------------
//SORT2401 EXEC PGM=SORT,COND=(0,NE)

//* DEBUT CRITERE XGEN
//* SFPIVTO SORT WFPIVAD0-ID-TECH-INDV ASC [1:9ó
//* SFPIVTO SORT WFPIVAD0-CLE2 ASC [10:91ó
//* SFPIVTO SORT WFPIVAD0-CO-REG-BASE ASC [101:3ó
//* SFPIVTO SORT WFPIVAD0-CO-STUT-CAT ASC [104:2ó
//* SFPIVTO SORT WFPIVAD0-TRI ASC [106:1ó
//* SFPIVTO SORT WFPIVAD0-ENR(197:10) ASC [307:10ó
//* SFPIVTO SORT WFPIVAD0-ENR(207:10) ASC [317:10ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0796
//SORTIN01 DD DISP=SHR,DSN=&MIG.ADR21.FPIV01
//SORTIN02 DD DISP=SHR,DSN=&MIG.ADR23.FPIV02C
//SORTIN03 DD DISP=SHR,DSN=&MIG.ADR23.FPIV03C
//SORTIN04 DD DISP=SHR,DSN=&MIG.ADR23.FPIV04C
//SORTOUT  DD DSN=&MIG.ADR24.FPIVTO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A,
               10,91,CH,A,
               101,3,CH,A,
               104,2,CH,A,
               106,1,CH,A,
               307,10,CH,A,
               317,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVAD1 DD DSN=&MIG.ADR24.FPIVAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVTO  DD DSN=&MIG.ADR24.FPIVTO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR24.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR24
//*----------------------------------------------------------------
//PPRADR24 EXEC PGM=PPRADR24,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPIVTO  DD DISP=SHR,DSN=&MIG.ADR24.FPIVTO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIVAD1 DD DSN=&MIG.ADR24.FPIVAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//ZFPIVTO  DD DSN=&MIG.ADR24.FPIVTO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR24.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR15
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFPIV   DD DSN=&MIG.ADR15.AFPIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENETB DD DSN=&MIG.ADR15.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFPIV
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFPIV SORT ID-TECH-UR-ETAB ASC ç217:9Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&MIG.ADR14.FPIVAD1
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPIVAD1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR24.FPIVAD1
//SORTOUT  DD DSN=&MIG.ADR15.AFPIV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(217,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENETB
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRENETB SORT TRENETB-ID-TECH-UR-ETAB(1:9) ASC ç1:9Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENETB
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENETB
//SORTOUT  DD DSN=&MIG.ADR15.TRENETB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*SFPIVAD1 DD DSN=&CIB.FPIVAD1,
//SFPIVAD1 DD DSN=&MIG.ADR15.FPIVAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFPIV   DD DSN=&MIG.ADR15.AFPIV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENETB DD DSN=&MIG.ADR15.TRENETB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR15.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR15
//*----------------------------------------------------------------
//PPRADR15 EXEC PGM=PPRADR15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFPIV   DD DISP=SHR,DSN=&MIG.ADR15.AFPIV
//STRENETB DD DISP=SHR,DSN=&MIG.ADR15.TRENETB
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIVAD1 DD DSN=&MIG.ADR15.FPIVAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFPIV   DD DSN=&MIG.ADR15.AFPIV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENETB DD DSN=&MIG.ADR15.TRENETB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR15.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFPIV   DD DSN=&MIG.ADR16.AFPIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFPIV
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFPIV SORT ID-TECH-INDV ASC ç16:9Ù
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//*POUR VOLUM{TRIE R{ELLE METTRE &MIG.ADR24.FPIVAD1
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPIVAD1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR15.FPIVAD1
//*SORTIN   DD DISP=SHR,DSN=&MIG.ADR24.FPIVAD1
//SORTOUT  DD DSN=&MIG.ADR16.AFPIV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(16,9,CH,A,
               25,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*SFPIVAD1 DD DSN=&CIB.FPIVAD1,
//SFPIVAD1 DD DSN=&MIG.ADR16.FPIVAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFPIV   DD DSN=&MIG.ADR16.AFPIV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR16.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR16
//*----------------------------------------------------------------
//PPRADR16 EXEC PGM=PPRADR16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFPIV   DD DISP=SHR,DSN=&MIG.ADR16.AFPIV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIVAD1 DD DSN=&MIG.ADR16.FPIVAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE)
//ZAFPIV   DD DSN=&MIG.ADR16.AFPIV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR16.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADR40
//*----------------------------------------------------------------
//DEL4001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVAD0 DD DSN=&MIG.ADR40.FPIVAD0,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPIVAD0
//*----------------------------------------------------------------
//SORT4001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPIVAD0 SORT ID-TECH-INDV ASC [16:9ó
//* SFPIVAD0 SORT ID-TECH-UR-ADHR ASC [25:9ó
//* SFPIVAD0 SORT CO-REG-BASE ASC [127:3ó
//* SFPIVAD0 SORT CO-STUT-CAT ASC [125:2ó
//* SFPIVAD0 SORT DT-FIN DESC [207:10ó
//* SFPIVAD0 SORT CO-PROVN-LIGN-PTS ASC [488:1ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPIVAD1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADR16.FPIVAD1
//SORTOUT  DD DSN=&MIG.ADR40.FPIVAD0,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,9,CH,A,
               25,9,CH,A,
               127,3,CH,A,
               125,2,CH,A,
               207,10,CH,D,
               488,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPIVAD1 DD DSN=&CIB.FPIVAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADR40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADR40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADR40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD0 DD DSN=&MIG.ADR40.FPIVAD0.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIVAD1 DD DSN=&MIG.ADR40.FPIVAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADR40
//*----------------------------------------------------------------
//PPRADR40 EXEC PGM=PPRADR40,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADR40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADR40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADR40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPIVAD0 DD DISP=SHR,DSN=&MIG.ADR40.FPIVAD0
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPIVAD1 DD DSN=&CIB.FPIVAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD0 DD DSN=&MIG.ADR40.FPIVAD0.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIVAD1 DD DSN=&MIG.ADR40.FPIVAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION