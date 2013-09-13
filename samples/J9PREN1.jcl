//J9PREN1  JOB UTI00TX0,'PR BREF A05-ENT 1',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0403,
//*         RESTART=PPRENT04,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03ADH  DD DSN=&MIG.ENT01.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-ID-PERS ASC ¬19:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*modif pour reel CVL / ALA
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.ENT01.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT1   DD DSN=&MIG.ENT01.FENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.ENT01.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT1   DD DSN=&MIG.ENT01.FENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT01
//*----------------------------------------------------------------
//PPRENT01 EXEC PGM=PPRENT01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03ADH  DD DISP=SHR,DSN=&MIG.ENT01.A03ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT1   DD DSN=&MIG.ENT01.FENT1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.ENT01.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT1   DD DSN=&MIG.ENT01.FENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT1   DD DSN=&MIG.ENT02.FENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTB DD DSN=&MIG.ENT02.A01ENTB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01CON  DD DSN=&MIG.ENT02.A01CON,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SIDFONC  DD DSN=&MIG.ENT02.CIDFONC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT1
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT1 SORT FENT1-A03ADH-C-ID-PERS ASC [19:12—
//* SFENT1 SORT FENT1-ORDRE-TYPE-AP ASC [469:1—
//* SFENT1 SORT FENT1-PRIORITE ASC [470:1—
//* SFENT1 SORT FENT1-DTRI ASC [471:8—
//* SFENT1 SORT FENT1-A03ADH-C-ID-ADH ASC [34:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT1
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT01.FENT1
//SORTOUT  DD DSN=&MIG.ENT02.FENT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A,
               469,1,CH,A,
               470,1,CH,A,
               471,8,CH,A,
               34,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTB
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTB SORT A01-ENTR-ETAB-C-ID-PERS-ENTR ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ENT02.A01ENTB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01CON
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01CON SORT A01-CONV-ENTR-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01CONV
//SORTIN   DD DISP=SHR,DSN=&SRC.A01CONV
//SORTOUT  DD DSN=&MIG.ENT02.A01CON,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CIDFONC
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.CIDFONC
//SORTIN   DD DISP=SHR,DSN=&TAB.CIDFONC
//SORTOUT  DD DSN=&MIG.ENT02.CIDFONC,
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
//*BREF-059 - AJO le 20/08/2013
//*STRENADH DD DSN=&CIB.TRENADH,
//STRENADH DD DSN=&MIG.ENT02.TRENADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT2   DD DSN=&MIG.ENT02.FENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT1  DD DSN=&CIB.FPENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT1   DD DSN=&MIG.ENT02.FENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTB DD DSN=&MIG.ENT02.A01ENTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01CON  DD DSN=&MIG.ENT02.A01CON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENADH DD DSN=&MIG.ENT02.TRENADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT2   DD DSN=&MIG.ENT02.FENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT1  DD DSN=&MIG.ENT02.FPENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT02
//*----------------------------------------------------------------
//PPRENT02 EXEC PGM=PPRENT02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFENT1   DD DISP=SHR,DSN=&MIG.ENT02.FENT1
//SA01ENTB DD DISP=SHR,DSN=&MIG.ENT02.A01ENTB
//SA01CON  DD DISP=SHR,DSN=&MIG.ENT02.A01CON
//SCIDFONC DD DISP=SHR,DSN=&MIG.ENT02.CIDFONC
//*--------------<FICHIERS CIBLES>---------------------------------
//*BREF-059 - AJO le 20/08/2013
//*STRENADH DD DSN=&CIB.TRENADH,
//STRENADH DD DSN=&MIG.ENT02.TRENADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT2   DD DSN=&MIG.ENT02.FENT2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT1  DD DSN=&CIB.FPENT1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT1   DD DSN=&MIG.ENT02.FENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTB DD DSN=&MIG.ENT02.A01ENTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01CON  DD DSN=&MIG.ENT02.A01CON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENADH DD DSN=&MIG.ENT02.TRENADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT2   DD DSN=&MIG.ENT02.FENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT1  DD DSN=&MIG.ENT02.FPENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03ETBC DD DSN=&MIG.ENT11.A03ETBC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.ENT11.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ETBC
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ETBC SORT A03-ETAB-CONTR-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETABC
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETABC
//SORTOUT  DD DSN=&MIG.ENT11.A03ETBC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CONT SORT A03-CONTRAT-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ENT11.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT11  DD DSN=&MIG.ENT11.FENT11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ETBC DD DSN=&MIG.ENT11.A03ETBC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ENT11.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT11  DD DSN=&MIG.ENT11.FENT11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT11
//*----------------------------------------------------------------
//PPRENT11 EXEC PGM=PPRENT11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03ETBC DD DISP=SHR,DSN=&MIG.ENT11.A03ETBC
//SA03CONT DD DISP=SHR,DSN=&MIG.ENT11.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT11  DD DSN=&MIG.ENT11.FENT11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ETBC DD DSN=&MIG.ENT11.A03ETBC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ENT11.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT11  DD DSN=&MIG.ENT11.FENT11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT2   DD DSN=&MIG.ENT03.FENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTB DD DSN=&MIG.ENT03.A01ENTB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01CON  DD DSN=&MIG.ENT03.A01CON,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01PERS DD DSN=&MIG.ENT03.A01PERS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01HADR DD DSN=&MIG.ENT03.A01HADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ETBC DD DSN=&MIG.ENT03.A03ETBC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT11  DD DSN=&MIG.ENT03.FENT11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ACTP DD DSN=&MIG.ENT03.A01ACTP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENS  DD DSN=&MIG.ENT03.A01ENS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01SIR  DD DSN=&MIG.ENT03.A01SIR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01RIBF DD DSN=&MIG.ENT03.A01RIBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT2
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT2 SORT FENT2-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT2
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT02.FENT2
//SORTOUT  DD DSN=&MIG.ENT03.FENT2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTB
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTB SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ENT03.A01ENTB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01CON
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01CON SORT A01-CONV-ENTR-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01CONV
//SORTIN   DD DISP=SHR,DSN=&SRC.A01CONV
//SORTOUT  DD DSN=&MIG.ENT03.A01CON,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01PERS
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01PERS SORT A01-ETAT-PERS-C-ID-PERS ASC ¬16:12¦
//* SA01PERS SORT A01-ETAT-PERS-D-DEB-EFFET ASC ¬35:8¦
//* FNC518 ON TRIE SUR D-FIN-EFFET (43:8)
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ETAT
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ETAT
//SORTOUT  DD DSN=&MIG.ENT03.A01PERS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               43,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01HADR
//*----------------------------------------------------------------
//SORT0305 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01HADR SORT A01-HIST-ADRESSE-C-ID-PERS ASC [17:12—
//* SA01HADR SORT A01-HIST-ADRESSE-C-TYPE-ADR DESC [29:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01HISTA
//SORTIN   DD DISP=SHR,DSN=&SRC.A01HISTA
//SORTOUT  DD DSN=&MIG.ENT03.A01HADR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A,
               29,1,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ETBC
//*----------------------------------------------------------------
//SORT0306 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ETBC SORT A03-ETAB-CONTR-C-ID-PERS ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETABC
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETABC
//SORTOUT  DD DSN=&MIG.ENT03.A03ETBC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT11
//*----------------------------------------------------------------
//SORT0307 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT11 SORT FENT11-ETAB-CONTR-C-ID-PERS ASC +1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT11
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT11.FENT11
//SORTOUT  DD DSN=&MIG.ENT03.FENT11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ACTP
//*----------------------------------------------------------------
//SORT0308 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ACTP SORT A01-ACTIV-PRINC-C-ID-PERS ASC ¬16:12¦
//* SA01ACTP SORT A01-ACTIV-PRINC-D-DEB-EFFET ASC ¬28:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ACTIV
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ACTIV
//SORTOUT  DD DSN=&MIG.ENT03.A01ACTP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENS
//*----------------------------------------------------------------
//SORT0309 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENS SORT A01-ENSEIGNE-C-ID-PERS ASC ¬16:12¦
//* FIN   CRITERE XGEN
//* FNC517 AJOUT SUR SUR D-FIN-EFFET
//* FNC649 TRI PLUTOT SUR D-DEB
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENSEI
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENSEI
//SORTOUT  DD DSN=&MIG.ENT03.A01ENS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01SIR
//*----------------------------------------------------------------
//SORT0310 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01SIR SORT A01-HISTOR-SIRET-C-ID-PERS ASC ¬16:12¦
//* SA01SIR SORT A01-HISTOR-SIRET-D-DEB-EFFET ASC ¬28:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01HISTS
//SORTIN   DD DISP=SHR,DSN=&SRC.A01HISTS
//SORTOUT  DD DSN=&MIG.ENT03.A01SIR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01RIBF
//*----------------------------------------------------------------
//SORT0311 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01RIBF SORT A01-RIB-FRANCE-C-ID-PERS ASC [94:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01RIBFR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01RIBFR
//SORTOUT  DD DSN=&MIG.ENT03.A01RIBF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENETB DD DSN=&MIG.ENT03.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENAPB DD DSN=&CIB.TRENAPB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENCVE DD DSN=&CIB.TRENCVE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENENS DD DSN=&CIB.TRENENS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIAB DD DSN=&CIB.TRENIAB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIEB DD DSN=&CIB.TRENIEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSGE DD DSN=&MIG.ENT03.TRENSGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADB DD DSN=&CIB.TRCOADB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOHAB DD DSN=&CIB.TRCOHAB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOMPB DD DSN=&MIG.ENT03.TRCOMPB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPIE DD DSN=&MIG.ENT03.TRCOPIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPRE DD DSN=&MIG.ENT03.TRCOPRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT3   DD DSN=&MIG.ENT03.FENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT2  DD DSN=&CIB.FPENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT61 DD DSN=&CIB.FPENT61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT7  DD DSN=&CIB.FPENT7,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT31  DD DSN=&MIG.ENT03.FENT31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADRS  DD DSN=&CIB.CHADR3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRE  DD DSN=&MIG.ENT03.CHADRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPAYS   DD DSN=&MIG.ENT03.CPAYS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCOZCA  DD DSN=&MIG.ENT03.TCOZCA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZCB DD DSN=&MIG.ENT03.TRCOZCB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZBI DD DSN=&MIG.ENT03.TRCOZBI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT2   DD DSN=&MIG.ENT03.FENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTB DD DSN=&MIG.ENT03.A01ENTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01CON  DD DSN=&MIG.ENT03.A01CON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01PERS DD DSN=&MIG.ENT03.A01PERS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01HADR DD DSN=&MIG.ENT03.A01HADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ETBC DD DSN=&MIG.ENT03.A03ETBC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT11  DD DSN=&MIG.ENT03.FENT11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ACTP DD DSN=&MIG.ENT03.A01ACTP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENS  DD DSN=&MIG.ENT03.A01ENS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01SIR  DD DSN=&MIG.ENT03.A01SIR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01RIBF DD DSN=&MIG.ENT03.A01RIBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENETB DD DSN=&MIG.ENT03.TRENETB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENAPB DD DSN=&MIG.ENT03.TRENAPB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENCVE DD DSN=&MIG.ENT03.TRENCVE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENENS DD DSN=&MIG.ENT03.TRENENS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENIAB DD DSN=&MIG.ENT03.TRENIAB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENIEB DD DSN=&MIG.ENT03.TRENIEB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENSGE DD DSN=&MIG.ENT03.TRENSGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADB DD DSN=&MIG.ENT03.TRCOADB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOHAB DD DSN=&MIG.ENT03.TRCOHAB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOMPB DD DSN=&MIG.ENT03.TRCOMPB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPIE DD DSN=&MIG.ENT03.TRCOPIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOPRE DD DSN=&MIG.ENT03.TRCOPRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT3   DD DSN=&MIG.ENT03.FENT3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT2  DD DSN=&MIG.ENT03.FPENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT61 DD DSN=&MIG.ENT03.FPENT61.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT7  DD DSN=&MIG.ENT03.FPENT7.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT31  DD DSN=&MIG.ENT03.FENT31.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRS  DD DSN=&MIG.ENT03.CHADRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT03
//*----------------------------------------------------------------
//PPRENT03 EXEC PGM=PPRENT03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CHADRE   DD DISP=SHR,DSN=&CIB.CHADR2
//CPAYS    DD DISP=SHR,DSN=&TAB.CPAY
//TCOZCA   DD DISP=SHR,DSN=&TAB.TRCOZCA
//TRCOZCB  DD DISP=SHR,DSN=&TAB.TRCOZCB
//* GCA ==> TRCOZBI  DD DISP=SHR,DSN=&TAB.TRCOZBI
//TRCOZBI  DD DUMMY
//SFENT2   DD DISP=SHR,DSN=&MIG.ENT03.FENT2
//SA01ENTB DD DISP=SHR,DSN=&MIG.ENT03.A01ENTB
//SA01CON  DD DISP=SHR,DSN=&MIG.ENT03.A01CON
//SA01PERS DD DISP=SHR,DSN=&MIG.ENT03.A01PERS
//SA01HADR DD DISP=SHR,DSN=&MIG.ENT03.A01HADR
//SA03ETBC DD DISP=SHR,DSN=&MIG.ENT03.A03ETBC
//SFENT11  DD DISP=SHR,DSN=&MIG.ENT03.FENT11
//SA01ACTP DD DISP=SHR,DSN=&MIG.ENT03.A01ACTP
//SA01ENS  DD DISP=SHR,DSN=&MIG.ENT03.A01ENS
//SA01SIR  DD DISP=SHR,DSN=&MIG.ENT03.A01SIR
//SA01RIBF DD DISP=SHR,DSN=&MIG.ENT03.A01RIBF
//SCIDFONC DD DISP=SHR,DSN=&MIG.ENT02.CIDFONC
//*--------------<FICHIERS CIBLES>---------------------------------
//* TRENSGE EST G{N{R{ SOUS MIG.ENT03.TRENSGE POUR
//* ETRE RETRI{ PAR LE PPRENT13 JUSTE APR}S
//STRENETB DD DSN=&MIG.ENT03.TRENETB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENAPB DD DSN=&CIB.TRENAPB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENCVE DD DSN=&CIB.TRENCVE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENENS DD DSN=&CIB.TRENENS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENIAB DD DSN=&CIB.TRENIAB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENIEB DD DSN=&CIB.TRENIEB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENSGE DD DSN=&MIG.ENT03.TRENSGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOADB DD DSN=&CIB.TRCOADB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOHAB DD DSN=&CIB.TRCOHAB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOMPB DD DSN=&MIG.ENT03.TRCOMPB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOPIE DD DSN=&MIG.ENT03.TRCOPIE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOPRE DD DSN=&MIG.ENT03.TRCOPRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT3   DD DSN=&MIG.ENT03.FENT3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT2  DD DSN=&CIB.FPENT2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT61 DD DSN=&CIB.FPENT61,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT7  DD DSN=&CIB.FPENT7,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT31  DD DSN=&MIG.ENT03.FENT31,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHADRS  DD DSN=&CIB.CHADR3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRE  DD DSN=&MIG.ENT03.CHADRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPAYS   DD DSN=&MIG.ENT03.CPAYS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCOZCA  DD DSN=&MIG.ENT03.TCOZCA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZCB DD DSN=&MIG.ENT03.TRCOZCB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZBI DD DSN=&MIG.ENT03.TRCOZBI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT2   DD DSN=&MIG.ENT03.FENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTB DD DSN=&MIG.ENT03.A01ENTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01CON  DD DSN=&MIG.ENT03.A01CON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01PERS DD DSN=&MIG.ENT03.A01PERS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01HADR DD DSN=&MIG.ENT03.A01HADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ETBC DD DSN=&MIG.ENT03.A03ETBC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT11  DD DSN=&MIG.ENT03.FENT11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ACTP DD DSN=&MIG.ENT03.A01ACTP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENS  DD DSN=&MIG.ENT03.A01ENS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01SIR  DD DSN=&MIG.ENT03.A01SIR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01RIBF DD DSN=&MIG.ENT03.A01RIBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENETB DD DSN=&MIG.ENT03.TRENETB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENAPB DD DSN=&MIG.ENT03.TRENAPB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENCVE DD DSN=&MIG.ENT03.TRENCVE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENENS DD DSN=&MIG.ENT03.TRENENS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENIAB DD DSN=&MIG.ENT03.TRENIAB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENIEB DD DSN=&MIG.ENT03.TRENIEB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENSGE DD DSN=&MIG.ENT03.TRENSGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADB DD DSN=&MIG.ENT03.TRCOADB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOHAB DD DSN=&MIG.ENT03.TRCOHAB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOMPB DD DSN=&MIG.ENT03.TRCOMPB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPIE DD DSN=&MIG.ENT03.TRCOPIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOPRE DD DSN=&MIG.ENT03.TRCOPRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT3   DD DSN=&MIG.ENT03.FENT3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT2  DD DSN=&MIG.ENT03.FPENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT61 DD DSN=&MIG.ENT03.FPENT61.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT7  DD DSN=&MIG.ENT03.FPENT7.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT31  DD DSN=&MIG.ENT03.FENT31.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRS  DD DSN=&MIG.ENT03.CHADRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENTSGE DD DSN=&MIG.ENT13.FENTSGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENTSGE
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENTSGE SORT TRENSGE-ID-TECH-UR-ETAB ASC [1:20—
//* SFENTSGE SORT TRENSGE-DT-DEFF ASC [25:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENSGE
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.TRENSGE
//SORTOUT  DD DSN=&MIG.ENT13.FENTSGE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               25,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENSGE DD DSN=&CIB.TRENSGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENTSGE DD DSN=&MIG.ENT13.FENTSGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENSGE DD DSN=&MIG.ENT13.TRENSGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT13
//*----------------------------------------------------------------
//PPRENT13 EXEC PGM=PPRENT13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFENTSGE DD DISP=SHR,DSN=&MIG.ENT13.FENTSGE
//*--------------<FICHIERS CIBLES>---------------------------------
//STRENSGE DD DSN=&CIB.TRENSGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENTSGE DD DSN=&MIG.ENT13.FENTSGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENSGE DD DSN=&MIG.ENT13.TRENSGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT1   DD DSN=&MIG.ENT04.FENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTR DD DSN=&MIG.ENT04.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01FJUR DD DSN=&MIG.ENT04.A01FJUR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01CGRO DD DSN=&MIG.ENT04.A01CGRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ACTP DD DSN=&MIG.ENT04.A01ACTP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ETPE DD DSN=&MIG.ENT04.A01ETPE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01RSOC DD DSN=&MIG.ENT04.A01RSOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT31  DD DSN=&MIG.ENT04.FENT31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01RIBF DD DSN=&MIG.ENT04.A01RIBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENADH DD DSN=&MIG.ENT04.TRENADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PROF DD DSN=&MIG.ENT04.J21PROF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENZNS DD DSN=&MIG.ENT04.TRENZNS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT1
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT1 SORT FENT1-A03ADH-C-ID-PERS ASC ¬19:12¦
//* SFENT1 SORT FENT1-PRIORITE ASC ¬470:1¦
//* SFENT1 SORT FENT1-DTRI ASC ¬471:8¦
//* SFENT1 SORT FENT1-A03ADH-C-ID-ADH ASC ¬34:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT1
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT01.FENT1
//SORTOUT  DD DSN=&MIG.ENT04.FENT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A,
               470,1,CH,A,
               471,8,CH,A,
               34,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ENT04.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01FJUR
//*----------------------------------------------------------------
//SORT0403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01FJUR SORT A01-FJUR-ENTR-C-ID-PERS ASC ¬16:12¦
//* SA01FJUR SORT A01-FJUR-ENTR-D-DEB-EFFET ASC ¬29:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01FORME
//SORTIN   DD DISP=SHR,DSN=&SRC.A01FORME
//SORTOUT  DD DSN=&MIG.ENT04.A01FJUR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               29,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01CGRO
//*----------------------------------------------------------------
//SORT0404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01CGRO SORT A01-COMPO-GRO-C-ID-PERS ASC ¬15:12¦
//* SA01CGRO SORT A01-COMPO-GRO-C-ID-PERS-GPE ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01COMPO
//SORTIN   DD DISP=SHR,DSN=&SRC.A01COMPO
//SORTOUT  DD DSN=&MIG.ENT04.A01CGRO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A,
               2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ACTP
//*----------------------------------------------------------------
//SORT0405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ACTP SORT A01-ACTIV-PRINC-C-ID-PERS ASC ¬16:12¦
//* SA01ACTP SORT A01-ACTIV-PRINC-D-DEB-EFFET ASC ¬28:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ACTIV
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ACTIV
//SORTOUT  DD DSN=&MIG.ENT04.A01ACTP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ETPE
//*----------------------------------------------------------------
//SORT0406 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ETPE SORT A01-ETAT-PERS-C-ID-PERS ASC ¬16:12¦
//* SA01ETPE SORT A01-ETAT-PERS-D-DEB-EFFET ASC ¬35:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ETAT
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ETAT
//SORTOUT  DD DSN=&MIG.ENT04.A01ETPE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               35,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01RSOC
//*----------------------------------------------------------------
//SORT0407 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01RSOC SORT A01-RAISON-SOCIALE-C-ID-PERS ASC ¬16:12¦
//* SA01RSOC SORT A01-RAISON-SOCIALE-D-DEB-EFFET ASC ¬28:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01RAISO
//SORTIN   DD DISP=SHR,DSN=&SRC.A01RAISO
//SORTOUT  DD DSN=&MIG.ENT04.A01RSOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT31
//*----------------------------------------------------------------
//SORT0408 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT31 SORT FENT31-ID-TECH-UR-ENTP ASC [21:12—
//* AJOUT TRI SUR ID-TECH-UR-ETAB
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT31
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.FENT31
//SORTOUT  DD DSN=&MIG.ENT04.FENT31,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A,
               1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01RIBF
//*----------------------------------------------------------------
//SORT0409 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01RIBF SORT A01-RIB-FRANCE-C-ID-PERS ASC [94:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01RIBFR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01RIBFR
//SORTOUT  DD DSN=&MIG.ENT04.A01RIBF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENADH
//*----------------------------------------------------------------
//SORT0410 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENADH
//SORTOUT  DD DSN=&MIG.ENT04.TRENADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PROF
//*----------------------------------------------------------------
//SORT0411 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PROFI
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PROFI
//SORTOUT  DD DSN=&MIG.ENT04.J21PROF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENZNS
//*----------------------------------------------------------------
//SORT0412 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TRENZNS
//SORTIN   DD DISP=SHR,DSN=&TAB.TRENZNS
//SORTOUT  DD DSN=&MIG.ENT04.TRENZNS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENENT DD DSN=&CIB.TRENENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIAE DD DSN=&CIB.TRENIAE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENCJU DD DSN=&CIB.TRENCJU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENFJU DD DSN=&CIB.TRENFJU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENRAT DD DSN=&CIB.TRENRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENGRP DD DSN=&CIB.TRENGRP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENAPE DD DSN=&CIB.TRENAPE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSIG DD DSN=&CIB.TRENSIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSOC DD DSN=&CIB.TRENSOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSOM DD DSN=&CIB.TRENSOM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT4   DD DSN=&MIG.ENT04.FENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT3  DD DSN=&CIB.FPENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT4  DD DSN=&CIB.FPENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFMPB2   DD DSN=&MIG.ENT04.FMPB2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIE2   DD DSN=&MIG.ENT04.FPIE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPRE2   DD DSN=&MIG.ENT04.FPRE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT62 DD DSN=&CIB.FPENT62,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGPE    DD DSN=&MIG.ENT04.FGPE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZCB DD DSN=&MIG.ENT04.TRCOZCB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOZBI DD DSN=&MIG.ENT04.TRCOZBI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT1   DD DSN=&MIG.ENT04.FENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.ENT04.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01FJUR DD DSN=&MIG.ENT04.A01FJUR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01CGRO DD DSN=&MIG.ENT04.A01CGRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ACTP DD DSN=&MIG.ENT04.A01ACTP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ETPE DD DSN=&MIG.ENT04.A01ETPE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01RSOC DD DSN=&MIG.ENT04.A01RSOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT31  DD DSN=&MIG.ENT04.FENT31.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01RIBF DD DSN=&MIG.ENT04.A01RIBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENENT DD DSN=&MIG.ENT04.TRENENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENIAE DD DSN=&MIG.ENT04.TRENIAE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENCJU DD DSN=&MIG.ENT04.TRENCJU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENFJU DD DSN=&MIG.ENT04.TRENFJU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENRAT DD DSN=&MIG.ENT04.TRENRAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENGRP DD DSN=&MIG.ENT04.TRENGRP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENAPE DD DSN=&MIG.ENT04.TRENAPE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENSIG DD DSN=&MIG.ENT04.TRENSIG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENSOC DD DSN=&MIG.ENT04.TRENSOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENSOM DD DSN=&MIG.ENT04.TRENSOM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT4   DD DSN=&MIG.ENT04.FENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT3  DD DSN=&MIG.ENT04.FPENT3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT4  DD DSN=&MIG.ENT04.FPENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFMPB2   DD DSN=&MIG.ENT04.FMPB2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIE2   DD DSN=&MIG.ENT04.FPIE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPRE2   DD DSN=&MIG.ENT04.FPRE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT62 DD DSN=&MIG.ENT04.FPENT62.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT04
//*----------------------------------------------------------------
//PPRENT04 EXEC PGM=PPRENT04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FGPE     DD DISP=SHR,DSN=&SRC.A01GPE
//TRCOZCB  DD DISP=SHR,DSN=&TAB.TRCOZCB
//* GCA : TRCOZBI ne sert plus dans ce PGM
//*TRCOZBI  DD DISP=SHR,DSN=&TAB.TRCOZBI
//TRCOZBI  DD DUMMY
//J21PROF  DD DISP=SHR,DSN=&MIG.ENT04.J21PROF
//TRENZNS  DD DISP=SHR,DSN=&MIG.ENT04.TRENZNS
//SFENT1   DD DISP=SHR,DSN=&MIG.ENT04.FENT1
//SA01ENTR DD DISP=SHR,DSN=&MIG.ENT04.A01ENTR
//SA01FJUR DD DISP=SHR,DSN=&MIG.ENT04.A01FJUR
//SA01CGRO DD DISP=SHR,DSN=&MIG.ENT04.A01CGRO
//SA01ACTP DD DISP=SHR,DSN=&MIG.ENT04.A01ACTP
//SA01ETPE DD DISP=SHR,DSN=&MIG.ENT04.A01ETPE
//SA01RSOC DD DISP=SHR,DSN=&MIG.ENT04.A01RSOC
//SFENT31  DD DISP=SHR,DSN=&MIG.ENT04.FENT31
//SA01RIBF DD DISP=SHR,DSN=&MIG.ENT04.A01RIBF
//STRENADH DD DISP=SHR,DSN=&MIG.ENT04.TRENADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STRENENT DD DSN=&CIB.TRENENT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENIAE DD DSN=&CIB.TRENIAE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENCJU DD DSN=&CIB.TRENCJU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENFJU DD DSN=&CIB.TRENFJU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENRAT DD DSN=&CIB.TRENRAT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENGRP DD DSN=&CIB.TRENGRP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENAPE DD DSN=&CIB.TRENAPE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENSIG DD DSN=&CIB.TRENSIG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENSOC DD DSN=&CIB.TRENSOC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENSOM DD DSN=&CIB.TRENSOM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT4   DD DSN=&MIG.ENT04.FENT4,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT3  DD DSN=&CIB.FPENT3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT4  DD DSN=&CIB.FPENT4,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFMPB2   DD DSN=&MIG.ENT04.FMPB2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPIE2   DD DSN=&MIG.ENT04.FPIE2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPRE2   DD DSN=&MIG.ENT04.FPRE2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPENT62 DD DSN=&CIB.FPENT62,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGPE    DD DSN=&MIG.ENT04.FGPE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZCB DD DSN=&MIG.ENT04.TRCOZCB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOZBI DD DSN=&MIG.ENT04.TRCOZBI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT1   DD DSN=&MIG.ENT04.FENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.ENT04.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01FJUR DD DSN=&MIG.ENT04.A01FJUR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01CGRO DD DSN=&MIG.ENT04.A01CGRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ACTP DD DSN=&MIG.ENT04.A01ACTP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ETPE DD DSN=&MIG.ENT04.A01ETPE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01RSOC DD DSN=&MIG.ENT04.A01RSOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT31  DD DSN=&MIG.ENT04.FENT31.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01RIBF DD DSN=&MIG.ENT04.A01RIBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENENT DD DSN=&MIG.ENT04.TRENENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENIAE DD DSN=&MIG.ENT04.TRENIAE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENCJU DD DSN=&MIG.ENT04.TRENCJU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENFJU DD DSN=&MIG.ENT04.TRENFJU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENRAT DD DSN=&MIG.ENT04.TRENRAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENGRP DD DSN=&MIG.ENT04.TRENGRP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENAPE DD DSN=&MIG.ENT04.TRENAPE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENSIG DD DSN=&MIG.ENT04.TRENSIG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENSOC DD DSN=&MIG.ENT04.TRENSOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENSOM DD DSN=&MIG.ENT04.TRENSOM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT4   DD DSN=&MIG.ENT04.FENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT3  DD DSN=&MIG.ENT04.FPENT3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT4  DD DSN=&MIG.ENT04.FPENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFMPB2   DD DSN=&MIG.ENT04.FMPB2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIE2   DD DSN=&MIG.ENT04.FPIE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPRE2   DD DSN=&MIG.ENT04.FPRE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT62 DD DSN=&MIG.ENT04.FPENT62.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION