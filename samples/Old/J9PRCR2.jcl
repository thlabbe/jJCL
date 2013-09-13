//J9VHCR2  JOB UTI00TX0,'VH GRECCO G02-CRE',CLASS=Z,MSGCLASS=X,         JOB00836
//*        RESTART=DEL3502,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE22
//*----------------------------------------------------------------
//DEL2201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.CRE22.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02DOSS DD DSN=&MIG.CRE22.C02DOSS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ADHE DD DSN=&MIG.CRE22.A03ADHE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT2201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-ID-PERS ASC [120:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TADHADH
//SORTOUT  DD DSN=&MIG.CRE22.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DOSS
//*----------------------------------------------------------------
//SORT2202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DOSS SORT C02-DOSS-CTX-C-ID-PERS ASC [32:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTOUT  DD DSN=&MIG.CRE22.C02DOSS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(32,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADHE
//*----------------------------------------------------------------
//SORT2203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB03ADHE SORT A03-ADHERENT-C-ID-PERS ASC [19:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REH.A3ADHER
//SORTOUT  DD DSN=&MIG.CRE22.A03ADHE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTCRE22 DD DSN=&MIG.CRE22.FTCRE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFTCRE23 DD DSN=&MIG.CRE22.FTCRE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE22
//*----------------------------------------------------------------
//PVHCRE22 EXEC PGM=PVHCRE22,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STADHADH DD DISP=SHR,DSN=&MIG.CRE22.TADHADH
//SC02DOSS DD DISP=SHR,DSN=&MIG.CRE22.C02DOSS
//SA03ADHE DD DISP=SHR,DSN=&MIG.CRE22.A03ADHE
//*--------------<FICHIERS CIBLES>---------------------------------
//SFTCRE22 DD DSN=&MIG.CRE22.FTCRE22,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFTCRE23 DD DSN=&MIG.CRE22.FTCRE23,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE33
//*----------------------------------------------------------------
//DEL3301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL33CRE  DD DSN=&MIG.CRE33.L33CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKGAR   DD DSN=&MIG.CRE33.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L33CRE
//*----------------------------------------------------------------
//SORT3301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL33CRE SORT WKECRE-C-ID-CONTRAT ASC [438:12—
//* SL33CRE SORT WKECRE-C-GAR ASC [450:5—
//* SL33CRE SORT WKECRE-C-CADRE ASC [458:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE21.WK1CRE
//SORTOUT  DD DSN=&MIG.CRE33.L33CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(438,12,CH,A,
               450,5,CH,A,
               458,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT3302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKGAR   SORT CNTGAR-C-ID-CONTRAT ASC [175:12—
//* SWKGAR   SORT CNTGAR-C-GAR ASC [223:5—
//* SWKGAR   SORT CNTGAR-C-CADRE ASC [228:1—
//* SWKGAR   SORT CNTGAR-C-CPI ASC [206:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE36.WKGAR
//SORTOUT  DD DSN=&MIG.CRE33.WKGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(27,12,CH,A,
               56,5,CH,A,
               61,1,CH,A,
               39,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS33CRE  DD DSN=&MIG.CRE33.S33CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE33
//*----------------------------------------------------------------
//PVHCRE33 EXEC PGM=PVHCRE33,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL33CRE  DD DISP=SHR,DSN=&MIG.CRE33.L33CRE
//SWKGAR   DD DISP=SHR,DSN=&MIG.CRE33.WKGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SS33CRE  DD DSN=&MIG.CRE33.S33CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE34
//*----------------------------------------------------------------
//DEL3401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL34CRE  DD DSN=&MIG.CRE34.L34CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE34.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKGAR   DD DSN=&MIG.CRE34.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB04APPE DD DSN=&MIG.CRE34.B04APPE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L34CRE
//*----------------------------------------------------------------
//SORT3401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL34CRE SORT WKECRE-C-GRP-GEST ASC [409:1—
//* SL34CRE SORT WKECRE-C-ADH ASC [410:9—
//* SL34CRE SORT WKECRE-C-ORDRE ASC [419:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE33.S33CRE
//SORTOUT  DD DSN=&MIG.CRE34.L34CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(409,1,CH,A,
               410,9,CH,A,
               419,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT3402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE34.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKGAR
//*----------------------------------------------------------------
//SORT3403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKGAR SORT WKGAR-C-GRPT-GEST ASC [91:1—
//* SWKGAR SORT WKGAR-C-ADH ASC [92:9—
//* SWKGAR SORT WKGAR-C-ORDRE ASC [101:4—
//* SWKGAR SORT WKGAR-C-CPI ASC [39:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE36.WKGAR
//SORTOUT  DD DSN=&MIG.CRE34.WKGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(91,1,CH,A,
               92,9,CH,A,
               101,4,CH,A,
               39,17,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B04APPE
//*----------------------------------------------------------------
//SORT3404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB04APPE SORT B04-APPEL-C-GRPT-GEST ASC [15:1—
//* SB04APPE SORT B04-APPEL-C-ADH ASC [17:9—
//* SB04APPE SORT B04-APPEL-C-ORDRE ASC [27:4—
//* SB04APPE SORT B04-APPEL-C-EXE-AFFECT ASC [32:4—
//* SB04APPE SORT B04-APPEL-C-PER-AFFECT ASC [37:2—
//* SB04APPE SORT B04-APPEL-C-TYPE-PER ASC [39:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B04APPEL
//SORTOUT  DD DSN=&MIG.CRE34.B04APPE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,1,CH,A,
               17,9,CH,A,
               27,4,CH,A,
               32,4,CH,A,
               37,2,CH,A,
               39,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS34CRE  DD DSN=&MIG.CRE34.S34CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE34
//*----------------------------------------------------------------
//PVHCRE34 EXEC PGM=PVHCRE34,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TACPGAR  DD DISP=SHR,DSN=&TABVH.TACPGAR
//SL34CRE  DD DISP=SHR,DSN=&MIG.CRE34.L34CRE
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE34.TCNTGAR
//SWKGAR   DD DISP=SHR,DSN=&MIG.CRE34.WKGAR
//SB04APPE DD DISP=SHR,DSN=&MIG.CRE34.B04APPE
//*--------------<FICHIERS CIBLES>---------------------------------
//SS34CRE  DD DSN=&MIG.CRE34.S34CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE39
//*----------------------------------------------------------------
//DEL3901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL39CRE  DD DSN=&MIG.CRE39.L39CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L39CRE
//*----------------------------------------------------------------
//SORT3901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL39CRE SORT WKECRE-C-GRP-GEST ASC [409:1—
//* SL39CRE SORT WKECRE-C-ADH ASC [410:9—
//* SL39CRE SORT WKECRE-C-ORDRE ASC [419:4—
//* SL39CRE SORT WKECRE-C-EXE-AFFECT ASC [423:4—
//* SL39CRE SORT WKECRE-C-TYPE-COTIS ASC [364:1—
//* SL39CRE SORT WKECRE-C-ID-INST-M ASC [496:7—
//* SL39CRE SORT WKECRE-C-PER-AFFECT-M ASC [430:2—
//* SL39CRE SORT WKECRE-C-TYPE-PER-M ASC [432:1—
//* SL39CRE SORT WKECRE-COMPTE-W6-E-DEBIT ASC [486:1—
//* SL39CRE SORT WKECRE-ENR(178:12) ASC [238:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE34.S34CRE
//SORTOUT  DD DSN=&MIG.CRE39.L39CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(409,1,CH,A,
               410,9,CH,A,
               419,4,CH,A,
               423,4,CH,A,
               364,1,CH,A,
               496,7,CH,A,
               430,2,CH,A,
               432,1,CH,A,
               486,1,CH,A,
               238,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS39CRE  DD DSN=&MIG.CRE39.S39CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE39
//*----------------------------------------------------------------
//PVHCRE39 EXEC PGM=PVHCRE39,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TACPGAR  DD DISP=SHR,DSN=&TABVH.TACPGAR
//SL39CRE  DD DISP=SHR,DSN=&MIG.CRE39.L39CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SS39CRE  DD DSN=&MIG.CRE39.S39CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE35
//*----------------------------------------------------------------
//DEL3501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL35CRE  DD DSN=&MIG.CRE35.L35CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBACGAR  DD DSN=&MIG.CRE35.BACGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&MIG.CRE35.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L35CRE
//*----------------------------------------------------------------
//SORT3501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL35CRE SORT WKECRE-ENR(16:20) ASC [76:20—
//* SL35CRE SORT WKECRE-ENR(36:3) ASC [96:3—
//* SL35CRE SORT WKECRE-ENR(45:3) ASC [105:3—
//* SL35CRE SORT WKECRE-ENR(99:4) ASC [159:4—
//* SL35CRE SORT WKECRE-ENR(39:5) ASC [99:5—
//* SL35CRE SORT WKECRE-ENR(134:3) ASC [194:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE39.S39CRE
//SORTOUT  DD DSN=&MIG.CRE35.L35CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(76,20,CH,A,
               96,3,CH,A,
               105,3,CH,A,
               159,4,CH,A,
               99,5,CH,A,
               194,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BACGAR
//*----------------------------------------------------------------
//SORT3502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBACGAR SORT ID-SI-ADHR ASC [1:20—
//* SBACGAR SORT NO-ORD-ADHS ASC [21:3—
//* SBACGAR SORT NO-INST ASC [71:3—
//* SBACGAR SORT ID-GARA-REF(6:4) ASC [51:4—
//* SBACGAR SORT NO-ORD-UEC ASC [24:5—
//* SBACGAR SORT MOD-CALC-GARA ASC [63:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.BACGAR
//SORTOUT  DD DSN=&MIG.CRE35.BACGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               71,3,CH,A,
               51,4,CH,A,
               24,5,CH,A,
               63,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TPOPCPI
//*----------------------------------------------------------------
//SORT3503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STPOPCPI SORT TAB-POP-CPI-ID-TECH-UR-ADHR ASC [21:20—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TPOPCPI
//SORTOUT  DD DSN=&MIG.CRE35.TPOPCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS35CRE  DD DSN=&MIG.CRE35.S35CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCTXN DD DSN=&CIB.FEXCL.TAUXNUL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE35
//*----------------------------------------------------------------
//PVHCRE35 EXEC PGM=PVHCRE35,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TC1RTDS  DD DISP=SHR,DSN=&TABVH.TC1RTDS
//TC1RDSP  DD DISP=SHR,DSN=&TABVH.TC1RDSP
//TRPTPLF  DD DISP=SHR,DSN=&TABVH.TRPTPLF
//SL35CRE  DD DISP=SHR,DSN=&MIG.CRE35.L35CRE
//SBACGAR  DD DISP=SHR,DSN=&MIG.CRE35.BACGAR
//STPOPCPI DD DISP=SHR,DSN=&MIG.CRE35.TPOPCPI
//*--------------<FICHIERS CIBLES>---------------------------------
//SS35CRE  DD DSN=&MIG.CRE35.S35CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCTXN DD DSN=&CIB.FEXCL.TAUXNUL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE37
//*----------------------------------------------------------------
//DEL3701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL37CRE  DD DSN=&MIG.CRE37.L37CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&MIG.CRE37.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBACGAR  DD DSN=&MIG.CRE37.BACGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&MIG.CRE37.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L37CRE
//*----------------------------------------------------------------
//SORT3701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL37CRE SORT WKECRE-ENR(16:20) ASC [76:20—
//* SL37CRE SORT WKECRE-ENR(36:3) ASC [96:3—
//* SL37CRE SORT WKECRE-ENR(39:5) ASC [99:5—
//* SL37CRE SORT WKECRE-ASST ASC [345:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE35.S35CRE
//SORTOUT  DD DSN=&MIG.CRE37.L37CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(76,20,CH,A,
               96,3,CH,A,
               99,5,CH,A,
               345,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TPOPCPI
//*----------------------------------------------------------------
//SORT3702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STPOPCPI SORT TAB-POP-CPI-ID-TECH-UR-ADHR ASC [21:20—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TPOPCPI
//SORTOUT  DD DSN=&MIG.CRE37.TPOPCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BACGAR
//*----------------------------------------------------------------
//SORT3703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBACGAR SORT ID-SI-ADHR ASC [1:20—
//* SBACGAR SORT NO-ORD-ADHS ASC [21:3—
//* SBACGAR SORT NO-ORD-UEC ASC [24:5—
//* SBACGAR SORT ASST ASC [67:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.BACGAR
//SORTOUT  DD DSN=&MIG.CRE37.BACGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,5,CH,A,
               67,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT3704 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-ID-TECH-UR-ADHR ASC [21:20—
//* STUECADH SORT TABUECAD-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TUECADH
//SORTOUT  DD DSN=&MIG.CRE37.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS37CRE  DD DSN=&MIG.CRE37.S37CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE37
//*----------------------------------------------------------------
//PVHCRE37 EXEC PGM=PVHCRE37,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL37CRE  DD DISP=SHR,DSN=&MIG.CRE37.L37CRE
//STPOPCPI DD DISP=SHR,DSN=&MIG.CRE37.TPOPCPI
//SBACGAR  DD DISP=SHR,DSN=&MIG.CRE37.BACGAR
//STUECADH DD DISP=SHR,DSN=&MIG.CRE37.TUECADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SS37CRE  DD DSN=&MIG.CRE37.S37CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE38
//*----------------------------------------------------------------
//DEL3801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL38CRE  DD DSN=&MIG.CRE38.L38CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L38CRE
//*----------------------------------------------------------------
//SORT3801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL38CRE SORT WKECRE-ENR(16:20) ASC [76:20—
//* SL38CRE SORT WKECRE-ENR(36:3) ASC [96:3—
//* SL38CRE SORT WKECRE-ENR(39:5) ASC [99:5—
//* SL38CRE SORT WKECRE-ENR(44:1) ASC [104:1—
//* SL38CRE SORT WKECRE-ENR(45:4) ASC [105:4—
//* SL38CRE SORT WKECRE-ENR(205:4) ASC [265:4—
//* SL38CRE SORT WKECRE-ENR(89:9) ASC [149:9—
//* SL38CRE SORT WKECRE-ENR(98:5) ASC [158:5—
//* SL38CRE SORT WKECRE-ENR(209:9) ASC [269:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE37.S37CRE
//SORTOUT  DD DSN=&MIG.CRE38.L38CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(76,20,CH,A,
               96,3,CH,A,
               99,5,CH,A,
               104,1,CH,A,
               105,4,CH,A,
               265,4,CH,A,
               149,9,CH,A,
               158,5,CH,A,
               269,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS38CRE  DD DSN=&MIG.CRE38.S38CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCCRR DD DSN=&CIB.FEXCL.CRENAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCCRA DD DSN=&CIB.FEXCL.CRENCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCCRC DD DSN=&CIB.FEXCL.CRENREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCCRN DD DSN=&CIB.FEXCL.CRENREA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLSN DD DSN=&CIB.FEXCL.SOLDENE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE38
//*----------------------------------------------------------------
//PVHCRE38 EXEC PGM=PVHCRE38,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL38CRE  DD DISP=SHR,DSN=&MIG.CRE38.L38CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SS38CRE  DD DSN=&MIG.CRE38.S38CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCCRR DD DSN=&CIB.FEXCL.CRENAFF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCCRA DD DSN=&CIB.FEXCL.CRENCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCCRC DD DSN=&CIB.FEXCL.CRENREP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCCRN DD DSN=&CIB.FEXCL.CRENREA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLSN DD DSN=&CIB.FEXCL.SOLDENE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE23
//*----------------------------------------------------------------
//DEL2301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL23CRE  DD DSN=&MIG.CRE23.L23CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFTCRE23 DD DSN=&MIG.CRE23.FTCRE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L23CRE
//*----------------------------------------------------------------
//SORT2301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL23CRE SORT WKECRE-C-GRP-GEST ASC [388:1—
//* SL23CRE SORT WKECRE-C-ADH ASC [389:9—
//* SL23CRE SORT WKECRE-C-ORDRE ASC [398:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE38.S38CRE
//SORTOUT  DD DSN=&MIG.CRE23.L23CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(388,1,CH,A,
               389,9,CH,A,
               398,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FTCRE23
//*----------------------------------------------------------------
//SORT2302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFTCRE23 SORT FTCRE23-C-GRPT-GEST ASC [21:1—
//* SFTCRE23 SORT FTCRE23-C-ADH ASC [22:9—
//* SFTCRE23 SORT FTCRE23-C-ORDRE ASC [31:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE22.FTCRE23
//SORTOUT  DD DSN=&MIG.CRE23.FTCRE23,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,1,CH,A,
               22,9,CH,A,
               31,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS23CRE  DD DSN=&MIG.CRE23.S23CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLDO DD DSN=&CIB.FEXCL.DOCCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE23
//*----------------------------------------------------------------
//PVHCRE23 EXEC PGM=PVHCRE23,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL23CRE  DD DISP=SHR,DSN=&MIG.CRE23.L23CRE
//SFTCRE23 DD DISP=SHR,DSN=&MIG.CRE23.FTCRE23
//*--------------<FICHIERS CIBLES>---------------------------------
//SS23CRE  DD DSN=&MIG.CRE23.S23CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLDO DD DSN=&CIB.FEXCL.DOCCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE30
//*----------------------------------------------------------------
//DEL3001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL30CRE  DD DSN=&MIG.CRE30.L30CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L30CRE
//*----------------------------------------------------------------
//SORT3001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL30CRE SORT WKECRE-ENR(16:20) ASC [76:20—
//* SL30CRE SORT WKECRE-ENR(36:3) ASC [96:3—
//* SL30CRE SORT WKECRE-ENR(39:5) ASC [99:5—
//* SL30CRE SORT WKECRE-ENR(44:1) ASC [104:1—
//* SL30CRE SORT WKECRE-ENR(45:4) ASC [105:4—
//* SL30CRE SORT WKECRE-ENR(205:4) ASC [265:4—
//* SL30CRE SORT WKECRE-ENR(204:1) ASC [264:1—
//* SL30CRE SORT WKECRE-ENR(193:1) ASC [253:1—
//* SL30CRE SORT WKECRE-ENR(209:3) ASC [269:3—
//* SL30CRE SORT WKECRE-POSIT ASC [462:1—
//* SL30CRE SORT WKECRE-ENR(212:10) ASC [272:10—
//* SL30CRE SORT WKECRE-ENR(222:10) ASC [282:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE23.S23CRE
//SORTOUT  DD DSN=&MIG.CRE30.L30CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(76,20,CH,A,
               96,3,CH,A,
               99,5,CH,A,
               104,1,CH,A,
               105,4,CH,A,
               265,4,CH,A,
               264,1,CH,A,
               253,1,CH,A,
               269,3,CH,A,
               462,1,CH,A,
               272,10,CH,A,
               282,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS30CRE  DD DSN=&MIG.CRE30.S30CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE30
//*----------------------------------------------------------------
//PVHCRE30 EXEC PGM=PVHCRE30,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL30CRE  DD DISP=SHR,DSN=&MIG.CRE30.L30CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SS30CRE  DD DSN=&MIG.CRE30.S30CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE31
//*----------------------------------------------------------------
//DEL3101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL31CRE  DD DSN=&MIG.CRE31.L31CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L31CRE
//*----------------------------------------------------------------
//SORT3101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL31CRE SORT WKDCRE-ENR(89:9) ASC [149:9—
//* SL31CRE SORT WKDCRE-ENR(49:20) ASC [109:20—
//* SL31CRE SORT WKDCRE-ENR(111:10) ASC [171:10—
//* SL31CRE SORT WKDCRE-ENR(212:20) ASC [272:20—
//**** SL31CRE SORT WKDCRE-ENR(222:10) ASC [282:10—
//* SL31CRE SORT WKDCRE-ENR(242:9) ASC [302:9—
//**** SL31CRE SORT WKDCRE-ENR(245:3) ASC [305:3—
//**** SL31CRE SORT WKDCRE-ENR(248:3) ASC [308:3—
//* SL31CRE SORT WKDCRE-ENR(137:1) ASC [197:1—
//* SL31CRE SORT WKDCRE-ENR(121:10) ASC [181:10—
//* SL31CRE SORT WKDCRE-ENR(232:10) ASC [292:10—
//* SL31CRE SORT WKDCRE-ENR(132:1) ASC [192:1—
//* SL31CRE SORT WKDCRE-ENR(131:1) ASC [191:1—
//* SL31CRE SORT WKDCRE-ENR(133:1) ASC [193:1—
//* SL31CRE SORT WKDCRE-ENR(134:3) ASC [194:3—
//* SL31CRE SORT WKDCRE-ENR(150:11) ASC [210:11—
//* SL31CRE SORT WKDCRE-ENR(138:12) ASC [198:12—
//* SL31CRE SORT WKDCRE-ENR(98:5) ASC [158:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE30.S30CRE
//SORTOUT  DD DSN=&MIG.CRE31.L31CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(149,9,CH,A,
               109,20,CH,A,
               171,10,CH,A,
               272,20,CH,A,
               302,9,CH,A,
               197,1,CH,A,
               181,10,CH,A,
               292,10,CH,A,
               192,1,CH,A,
               191,1,CH,A,
               193,1,CH,A,
               194,3,CH,A,
               210,11,CH,A,
               198,12,CH,A,
               158,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SS31CRE  DD DSN=&MIG.CRE31.S31CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SSCRE    DD DSN=&MIG.CRE31.SCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE31
//*----------------------------------------------------------------
//PVHCRE31 EXEC PGM=PVHCRE31,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL31CRE  DD DISP=SHR,DSN=&MIG.CRE31.L31CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SS31CRE  DD DSN=&MIG.CRE31.S31CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SSCRE    DD DSN=&MIG.CRE31.SCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE32
//*----------------------------------------------------------------
//DEL3201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL32CRE  DD DSN=&MIG.CRE32.L32CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L32CRE
//*----------------------------------------------------------------
//SORT3201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL32CRE SORT WKECRE-C-ID-PER-DEBIT ASC [1:12—
//* SL32CRE SORT WKECRE-IDOBJREC ASC [13:20—
//* SL32CRE SORT WKECRE-ENR(98:5) ASC +158:5]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE31.S31CRE
//SORTOUT  DD DSN=&MIG.CRE32.L32CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,20,CH,A,
               158,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRECCRE DD DSN=&MIG.CRE32.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE32
//*----------------------------------------------------------------
//PVHCRE32 EXEC PGM=PVHCRE32,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL32CRE  DD DISP=SHR,DSN=&MIG.CRE32.L32CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//STRECCRE DD DSN=&MIG.CRE32.TRECCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE52
//*----------------------------------------------------------------
//DEL5201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SL32CRE  DD DSN=&MIG.CRE52.L32CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERDT  DD DSN=&MIG.CRE52.FPERDT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER L32CRE
//*----------------------------------------------------------------
//SORT5201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL32CRE SORT WKECRE-C-ID-PER-DEBIT ASC [1:12—
//* SL32CRE SORT WKECRE-IDLIGCRE ASC [33:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE31.S31CRE
//SORTOUT  DD DSN=&MIG.CRE52.L32CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               33,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPERDT
//*----------------------------------------------------------------
//SORT5202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL32CRE SORT FIC-IDPERDEBIT-P ASC [1:12—
//* SL32CRE SORT FIC-IDPERDEBIT-S ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE16.FPERD1
//         DD DISP=SHR,DSN=&MIG.CRE17.FPERD2
//         DD DISP=SHR,DSN=&MIG.CRE18.FPERD3
//SORTOUT  DD DSN=&MIG.CRE52.FPERDT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STPERCTX DD DSN=&MIG.CRE52.TPERCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE52
//*----------------------------------------------------------------
//PVHCRE52 EXEC PGM=PVHCRE52,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SL32CRE  DD DISP=SHR,DSN=&MIG.CRE52.L32CRE
//SFPERDT  DD DISP=SHR,DSN=&MIG.CRE52.FPERDT
//*--------------<FICHIERS CIBLES>---------------------------------
//STPERCTX DD DSN=&MIG.CRE52.TPERCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE24
//*----------------------------------------------------------------
//DEL2401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCRE     DD DSN=&MIG.CRE24.CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CRE
//*----------------------------------------------------------------
//SORT2401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCRE SORT CRE-IDTECADH ASC [16:20—
//* SCRE SORT CRE-NOORDADS ASC [36:3—
//* SCRE SORT CRE-NOORDUEC ASC [39:5—
//* SCRE SORT CRE-TYINS ASC [44:1—
//* SCRE SORT CRE-NOINS ASC [45:4—
//* SCRE SORT CRE-ANPER ASC [205:4—
//* SCRE SORT CRE-TYLIGCRE ASC [204:1—
//* SCRE SORT CRE-TYREGCRE ASC [193:1—
//* SCRE SORT CRE-COPERREF ASC [209:3—
//* SCRE SORT WKFCRE-POSIT ASC [316:1—
//* SCRE SORT CRE-DTDEBPER ASC [212:10—
//* SCRE SORT CRE-DTFINPER ASC [222:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE31.SCRE
//SORTOUT  DD DSN=&MIG.CRE24.CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               44,1,CH,A,
               45,4,CH,A,
               205,4,CH,A,
               204,1,CH,A,
               193,1,CH,A,
               209,3,CH,A,
               316,1,CH,A,
               212,10,CH,A,
               222,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORC     DD DSN=&MIG.CRE24.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE24
//*----------------------------------------------------------------
//PVHCRE24 EXEC PGM=PVHCRE24,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TABVH.TPERIOD
//*TGESTEG  DD DISP=SHR,DSN=&TABVH.TGESTEG
//SCRE     DD DISP=SHR,DSN=&MIG.CRE24.CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SORC     DD DSN=&MIG.CRE24.ORC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE40
//*----------------------------------------------------------------
//DEL4001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.CRE40.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT4001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TADHADH
//SORTOUT  DD DSN=&MIG.CRE40.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKSIT   DD DSN=&MIG.CRE40.WKSIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE40
//*----------------------------------------------------------------
//PVHCRE40 EXEC PGM=PVHCRE40,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STADHADH DD DISP=SHR,DSN=&MIG.CRE40.TADHADH
//SB05ECRI DD DISP=SHR,DSN=&TRI.B05ECRI1
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKSIT   DD DSN=&MIG.CRE40.WKSIT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE41
//*----------------------------------------------------------------
//DEL4101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRECCRE DD DSN=&MIG.CRE41.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SL41CRE  DD DSN=&MIG.CRE41.L41CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRECCRE
//*----------------------------------------------------------------
//SORT4101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRECCRE SORT T-GR-RECCRE-C-GRPT-GEST ASC [26:9—
//* STRECCRE SORT T-GR-RECCRE-C-ADH ASC [26:9—
//* STRECCRE SORT T-GR-RECCRE-C-ORDRE ASC [35:4—
//* STRECCRE SORT T-GR-RECCRE-C-EXE-AFFECT ASC [39:4—
//* STRECCRE SORT T-GR-RECCRE-C-TYPE-PER ASC [45:1—
//* STRECCRE SORT T-GR-RECCRE-C-PER-AFFECT ASC [43:2—
//* STRECCRE SORT T-GR-RECCRE-C-PER-AFFECT ASC [126:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE32.TRECCRE
//SORTOUT  DD DSN=&MIG.CRE41.TRECCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A,
               39,4,CH,A,
               45,1,CH,A,
               43,2,CH,A,
               126,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER L41CRE
//*----------------------------------------------------------------
//SORT4102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SL41CRE SORT WKECRE-C-GRP-GEST ASC [388:1—
//* SL41CRE SORT WKECRE-C-ADH ASC [389:9—
//* SL41CRE SORT WKECRE-C-ORDRE ASC [398:4—
//* SL41CRE SORT WKECRE-C-EXE-AFFECT ASC [402:4—
//* SL41CRE SORT WKECRE-C-TYPE-PER ASC [408:1—
//* SL41CRE SORT WKECRE-C-PER-AFFECT ASC [406:2—
//* SL41CRE SORT WKECRE-COMPTE-D-CREATION DESC [462:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE38.S38CRE
//SORTOUT  DD DSN=&MIG.CRE41.L41CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(388,1,CH,A,
               389,9,CH,A,
               398,4,CH,A,
               402,4,CH,A,
               408,1,CH,A,
               406,2,CH,A,
               462,8,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKCTX   DD DSN=&MIG.CRE41.WKCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE41
//*----------------------------------------------------------------
//PVHCRE41 EXEC PGM=PVHCRE41,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRECCRE DD DISP=SHR,DSN=&MIG.CRE41.TRECCRE
//SL41CRE  DD DISP=SHR,DSN=&MIG.CRE41.L41CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKCTX   DD DSN=&MIG.CRE41.WKCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE42
//*----------------------------------------------------------------
//DEL4201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRECCRE DD DSN=&MIG.CRE42.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECO DD DSN=&MIG.CRE42.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07ACTI DD DSN=&MIG.CRE42.B07ACTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB04APPE DD DSN=&MIG.CRE42.B04APPE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRECCRE
//*----------------------------------------------------------------
//SORT4201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRECCRE SORT T-GR-RECCRE-C-ID-RECOUVR ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE32.TRECCRE
//SORTOUT  DD DSN=&MIG.CRE42.TRECCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT4202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT B07-RECOUVR-C-ID-RECOUVR ASC [2:12—
//* SB07RECO SORT B07-RECOUVR-C-ORDRE-CTX ASC [44:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRE42.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               44,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07ACTI
//*----------------------------------------------------------------
//SORT4203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07ACTI SORT B07-ACTION-REC-C-ID-RECOUVR ASC [2:12—
//* SB07ACTI SORT B07-ACTION-REC-D-EFFECTIVE ASC [26:8—
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B07ACTIO
//SORTOUT  DD DSN=&MIG.CRE42.B07ACTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               26,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B04APPE
//*----------------------------------------------------------------
//SORT4204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB04APPE SORT B04-APPEL-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B04APPEL
//SORTOUT  DD DSN=&MIG.CRE42.B04APPE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKDIV   DD DSN=&MIG.CRE42.WKDIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE42
//*----------------------------------------------------------------
//PVHCRE42 EXEC PGM=PVHCRE42,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRECCRE DD DISP=SHR,DSN=&MIG.CRE42.TRECCRE
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE42.B07RECO
//SB07ACTI DD DISP=SHR,DSN=&MIG.CRE42.B07ACTI
//SB04APPE DD DISP=SHR,DSN=&MIG.CRE42.B04APPE
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKDIV   DD DSN=&MIG.CRE42.WKDIV,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE43
//*----------------------------------------------------------------
//DEL4301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKORC   DD DSN=&MIG.CRE43.WKORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKSIT   DD DSN=&MIG.CRE43.WKSIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPCTX01 DD DSN=&MIG.CRE43.FPCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER WKORC
//*----------------------------------------------------------------
//SORT4301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKORC SORT WKORC-IDTECADH ASC [16:20—
//* SWKORC SORT WKORC-NOORDADS ASC [36:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE24.ORC
//SORTOUT  DD DSN=&MIG.CRE43.WKORC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKSIT
//*----------------------------------------------------------------
//SORT4302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKSIT SORT WKSIT-ID-TECH-UR-ADHR ASC [1:20—
//* SWKSIT SORT WKSIT-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE40.WKSIT
//SORTOUT  DD DSN=&MIG.CRE43.WKSIT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPCTX01
//*----------------------------------------------------------------
//SORT4303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKSIT SORT FPEXT01-IDTECADH ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPCTX01
//SORTOUT  DD DSN=&MIG.CRE43.FPCTX01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORC     DD DSN=&MIG.CRE43.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE43,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE43
//*----------------------------------------------------------------
//PVHCRE43 EXEC PGM=PVHCRE43,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE43,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SWKORC   DD DISP=SHR,DSN=&MIG.CRE43.WKORC
//SWKSIT   DD DISP=SHR,DSN=&MIG.CRE43.WKSIT
//SFPCTX01 DD DISP=SHR,DSN=&MIG.CRE43.FPCTX01
//*--------------<FICHIERS CIBLES>---------------------------------
//SORC     DD DSN=&MIG.CRE43.ORC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE44
//*----------------------------------------------------------------
//DEL4401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKORC   DD DSN=&MIG.CRE44.WKORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKREC   DD DSN=&MIG.CRE44.WKREC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKPER   DD DSN=&MIG.CRE44.WKPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKCTX   DD DSN=&MIG.CRE44.WKCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKDIV   DD DSN=&MIG.CRE44.WKDIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCRE     DD DSN=&MIG.CRE44.CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER WKORC
//*----------------------------------------------------------------
//SORT4401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKORC SORT WKORC-IDOBJREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE43.ORC
//SORTOUT  DD DSN=&MIG.CRE44.WKORC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKREC
//*----------------------------------------------------------------
//SORT4402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKREC SORT WKT-GR-RECCRE-IDOBJREC ASC [126:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE32.TRECCRE
//SORTOUT  DD DSN=&MIG.CRE44.WKREC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(126,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKPER
//*----------------------------------------------------------------
//SORT4403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKPER SORT WKPER-IDOBJREC ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE52.TPERCTX
//SORTOUT  DD DSN=&MIG.CRE44.WKPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKCTX
//*----------------------------------------------------------------
//SORT4404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKCTX SORT WKCTX-IDOBJREC ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE41.WKCTX
//SORTOUT  DD DSN=&MIG.CRE44.WKCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKDIV
//*----------------------------------------------------------------
//SORT4405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKDIV SORT WKDIV-IDOBJREC ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE42.WKDIV
//SORTOUT  DD DSN=&MIG.CRE44.WKDIV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CRE
//*----------------------------------------------------------------
//SORT4406 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCRE SORT CRE-IDOBJREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE31.SCRE
//SORTOUT  DD DSN=&MIG.CRE44.CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORC     DD DSN=&MIG.CRE44.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRECCRE DD DSN=&CIB.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPERCTX DD DSN=&CIB.TPERCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLCT DD DSN=&CIB.FEXCL.MAJCOCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE44,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE44
//*----------------------------------------------------------------
//PVHCRE44 EXEC PGM=PVHCRE44,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE44,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SWKORC   DD DISP=SHR,DSN=&MIG.CRE44.WKORC
//SWKREC   DD DISP=SHR,DSN=&MIG.CRE44.WKREC
//SWKPER   DD DISP=SHR,DSN=&MIG.CRE44.WKPER
//SWKCTX   DD DISP=SHR,DSN=&MIG.CRE44.WKCTX
//SWKDIV   DD DISP=SHR,DSN=&MIG.CRE44.WKDIV
//SCRE     DD DISP=SHR,DSN=&MIG.CRE44.CRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SORC     DD DSN=&MIG.CRE44.ORC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRECCRE DD DSN=&CIB.TRECCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STPERCTX DD DSN=&CIB.TPERCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLCT DD DSN=&CIB.FEXCL.MAJCOCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCRE     DD DSN=&MIG.CRE50.SCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SORC     DD DSN=&MIG.CRE50.SORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER SCRE
//*----------------------------------------------------------------
//SORT5001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCRE SORT WKZCRE-IDOBJREC ASC [49:20—
//* SCRE SORT WKZCRE-IDLIGCRE ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE31.SCRE
//SORTOUT  DD DSN=&MIG.CRE50.SCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               69,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER SORC
//*----------------------------------------------------------------
//SORT5002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SORC SORT SORC-IDOBJREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE44.ORC
//SORTOUT  DD DSN=&MIG.CRE50.SORC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCREANC  DD DSN=&CIB.CREANC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCCRE    DD DSN=&CIB.CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCORC    DD DSN=&CIB.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCRE    DD DSN=&CIB.PCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE50
//*----------------------------------------------------------------
//PVHCRE50 EXEC PGM=PVHCRE50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SCRE     DD DISP=SHR,DSN=&MIG.CRE50.SCRE
//SORC     DD DISP=SHR,DSN=&MIG.CRE50.SORC
//*--------------<FICHIERS CIBLES>---------------------------------
//SCREANC  DD DSN=&CIB.CREANC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCCRE    DD DSN=&CIB.CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCORC    DD DSN=&CIB.ORC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPCRE    DD DSN=&CIB.PCRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
