//J9VHCR1  JOB UTI00TX0,'VH GRECCO G02-CRE',CLASS=Z,MSGCLASS=X,         JOB01087
//*        RESTART=DEL0602,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.CRE01.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05SYNT DD DSN=&MIG.CRE01.B05SYNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS  DD DSN=&MIG.CRE01.J04DAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PERI DD DSN=&MIG.CRE01.B05PERI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CRE01.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SURCOMTI DD DSN=&MIG.CRE01.URCOMTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TADHADH
//SORTOUT  DD DSN=&MIG.CRE01.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05SYNT
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-GRPT-GEST ASC [1:1—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ADH ASC [3:9—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05SYNTH
//SORTOUT  DD DSN=&MIG.CRE01.B05SYNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS SORT J04-DAS-C-GRPT-GEST ASC [20:1—
//* SJ04DAS SORT J04-DAS-C-ADH ASC [22:9—
//* SJ04DAS SORT J04-DAS-C-ORDRE ASC [32:4—
//* SJ04DAS SORT J04-DAS-Q-EXE ASC [483:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE01.J04DAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(20,1,CH,A,
               22,9,CH,A,
               32,4,CH,A,
               483,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PERI
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT B05-PER-DEB-C-GRPT-GEST ASC [50:1—
//* SB05PERI SORT B05-PER-DEB-C-ADH ASC [52:9—
//* SB05PERI SORT B05-PER-DEB-C-ORDRE ASC [62:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE01.B05PERI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,1,CH,A,
               52,9,CH,A,
               62,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [53:4—
//* SB05OPER SORT B05-OPER-COMPTE-C-EXE-AFFECT ASC [87:4—
//* SB05OPER SORT B05-OPER-COMPTE-C-TYPE-PER ASC [94:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-PER-AFFECT ASC [92:2—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE-CTX ASC [96:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&ECH.B05OPER
//SORTOUT  DD DSN=&MIG.CRE01.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               43,9,CH,A,
               53,4,CH,A,
               87,4,CH,A,
               94,1,CH,A,
               92,2,CH,A,
               96,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER URCOMTI
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SURCOMTI SORT URCOMTIE-C-GRPT-GEST ASC [49:1—
//* SURCOMTI SORT URCOMTIE-C-ADH ASC [50:9—
//* SURCOMTI SORT URCOMTIE-C-ORDRE ASC [59:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&TABVH.URCOMTI             I
//SORTOUT  DD DSN=&MIG.CRE01.URCOMTI,
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
//SFCRE01D DD DSN=&MIG.CRE01.FCRE01D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01C DD DSN=&MIG.CRE01.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPER01  DD DSN=&MIG.CRE01.FPER01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFICPROF DD DSN=&MIG.CRE01.FICPROF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLAD DD DSN=&CIB.FEXCL.ADHER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE01
//*----------------------------------------------------------------
//PVHCRE01 EXEC PGM=PVHCRE01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//ADHSEL   DD DUMMY
//*ADH=H0181204540000
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SB05ECRI DD DISP=SHR,DSN=&TRI.B05ECRI1
//SB05SYNT DD DISP=SHR,DSN=&MIG.CRE01.B05SYNT
//SJ04DAS  DD DISP=SHR,DSN=&MIG.CRE01.J04DAS
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE01.B05PERI
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//SURCOMTI DD DISP=SHR,DSN=&MIG.CRE01.URCOMTI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01D DD DSN=&MIG.CRE01.FCRE01D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE01C DD DSN=&MIG.CRE01.FCRE01C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPER01  DD DSN=&MIG.CRE01.FPER01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLAD DD DSN=&CIB.FEXCL.ADHER,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFICPROF DD DSN=&MIG.CRE01.FICPROF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE60
//*----------------------------------------------------------------
//DEL6001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE60.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPEV DD DSN=&MIG.CRE60.B05OPEV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01
//*----------------------------------------------------------------
//SORT6001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01 SORT FCRE01-ECRITURE-COMPTE-ENR(24:12) ASC [77:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE01.FCRE01D
//         DD DISP=SHR,DSN=&MIG.CRE01.FCRE01C
//SORTOUT  DD DSN=&MIG.CRE60.FCRE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(77,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPEV
//*----------------------------------------------------------------
//SORT6002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPEV SORT B05-OPER-VENTIL-C-ID-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&ECH.B05OPERV
//SORTOUT  DD DSN=&MIG.CRE60.B05OPEV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL6002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01B DD DSN=&MIG.CRE60.FCRE01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL6003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE60
//*----------------------------------------------------------------
//PVHCRE60 EXEC PGM=PVHCRE60,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE01  DD DISP=SHR,DSN=&MIG.CRE60.FCRE01
//SB05OPEV DD DISP=SHR,DSN=&MIG.CRE60.B05OPEV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01B DD DSN=&MIG.CRE60.FCRE01B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE61
//*----------------------------------------------------------------
//DEL6101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01B DD DSN=&MIG.CRE61.FCRE01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CRE61.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPE2 DD DSN=&MIG.CRE61.B05OPE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01B
//*----------------------------------------------------------------
//SORT6101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01B SORT B-FCRE01-C-OPER ASC [290:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE60.FCRE01B
//SORTOUT  DD DSN=&MIG.CRE61.FCRE01B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(290,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT6102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&ECH.B05OPER
//SORTOUT  DD DSN=&MIG.CRE61.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPE2
//*----------------------------------------------------------------
//SORT6103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPE2 SORT BB05-OPER-COMPTE-C-OPER-ISSU ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&ECH.B05OPER
//SORTOUT  DD DSN=&MIG.CRE61.B05OPE2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL6102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL6103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE61
//*----------------------------------------------------------------
//PVHCRE61 EXEC PGM=PVHCRE61,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE01B DD DISP=SHR,DSN=&MIG.CRE61.FCRE01B
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE61.B05OPER
//SB05OPE2 DD DISP=SHR,DSN=&MIG.CRE61.B05OPE2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE01C DD DSN=&MIG.CRE61.FCRE01C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE59
//*----------------------------------------------------------------
//DEL5901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05PER1 DD DSN=&MIG.CRE59.B05PER1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PER2 DD DSN=&MIG.CRE59.B05PER2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECO DD DSN=&MIG.CRE59.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER1
//*----------------------------------------------------------------
//SORT5901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT F1-B05-PER-DEB-C-ID-REC-INI ASC [284:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE59.B05PER1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(284,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER2
//*----------------------------------------------------------------
//SORT5902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT F2-B05-PER-DEB-C-ID-RECOUVR ASC [17:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE59.B05PER2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT5903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT B07-RECOUVR-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRE59.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICRECO DD DSN=&MIG.CRE59.FICRECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE59
//*----------------------------------------------------------------
//PVHCRE59 EXEC PGM=PVHCRE59,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05PER1 DD DISP=SHR,DSN=&MIG.CRE59.B05PER1
//SB05PER2 DD DISP=SHR,DSN=&MIG.CRE59.B05PER2
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE59.B07RECO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFICRECO DD DSN=&MIG.CRE59.FICRECO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB07RECO DD DSN=&MIG.CRE02.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS1 DD DSN=&MIG.CRE02.J04DAS1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS2 DD DSN=&MIG.CRE02.J04DAS2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT FICRECO-C-ID-REC-ACCES ASC [13:12—
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
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE59.FICRECO
//SORTOUT  DD DSN=&MIG.CRE02.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(6000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS1
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS1 SORT J1-J04-DAS-C-ID-REC-SDA ASC [531:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE02.J04DAS1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(531,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS2
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS2 SORT J2-J04-DAS-C-ID-REC-SDA50 ASC [547:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE02.J04DAS2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(547,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE02  DD DSN=&MIG.CRE02.FCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE02
//*----------------------------------------------------------------
//PVHCRE02 EXEC PGM=PVHCRE02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE02.B07RECO
//SJ04DAS1 DD DISP=SHR,DSN=&MIG.CRE02.J04DAS1
//SJ04DAS2 DD DISP=SHR,DSN=&MIG.CRE02.J04DAS2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE02  DD DSN=&MIG.CRE02.FCRE02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,5000),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE03.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE02  DD DSN=&MIG.CRE03.FCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE02B DD DSN=&MIG.CRE03.FCRE02B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPER01  DD DSN=&MIG.CRE03.FPER01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01 SORT FCRE01-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE61.FCRE01
//SORTOUT  DD DSN=&MIG.CRE03.FCRE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE02
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE02 SORT FCRE02-PER-DEB-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE02.FCRE02
//SORTOUT  DD DSN=&MIG.CRE03.FCRE02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE02B
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE02B SORT FCRE02-PER-DEB-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE02.FCRE02
//SORTOUT  DD DSN=&MIG.CRE03.FCRE02B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPER01
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPER01 SORT FPER01-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE01.FPER01
//SORTOUT  DD DSN=&MIG.CRE03.FPER01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE03  DD DSN=&MIG.CRE03.FCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE03A DD DSN=&MIG.CRE03.FCRE03A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE03
//*----------------------------------------------------------------
//PVHCRE03 EXEC PGM=PVHCRE03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TABVH.TPERIOD
//SFCRE01  DD DISP=SHR,DSN=&MIG.CRE03.FCRE01
//SFCRE02  DD DISP=SHR,DSN=&MIG.CRE03.FCRE02
//SFCRE02B DD DISP=SHR,DSN=&MIG.CRE03.FCRE02B
//SFPER01  DD DISP=SHR,DSN=&MIG.CRE03.FPER01
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE03  DD DSN=&MIG.CRE03.FCRE03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE03A DD DSN=&MIG.CRE03.FCRE03A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE58
//*----------------------------------------------------------------
//DEL5801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE03W DD DSN=&MIG.CRE58.FCRE03W,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECO DD DSN=&MIG.CRE58.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE03W
//*----------------------------------------------------------------
//SORT5801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE03W SORT FCRE03-C-GRPT-GEST ASC [24:1—
//* SFCRE03W SORT FCRE03-C-ADH ASC [25:9—
//* SFCRE03W SORT FCRE03-C-ORDRE ASC [34:4—
//* SFCRE03W SORT FCRE03-OPERC-EXE-AFFECT ASC [430:4—
//* SFCRE03W SORT FCRE03-OPERC-PER-AFFECT ASC [434:2—
//* SFCRE03W SORT FCRE03-OPERC-TYPE-PER ASC [436:1—
//* SFCRE03W SORT FCRE03-OPERC-ORDRE-CTX ASC [437:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE03.FCRE03
//         DD DISP=SHR,DSN=&MIG.CRE03.FCRE03A
//SORTOUT  DD DSN=&MIG.CRE58.FCRE03W,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               430,4,CH,A,
               434,2,CH,A,
               436,1,CH,A,
               437,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT5802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT B07-RECOUVR-C-GRPT-GEST ASC [15:1—
//* SB07RECO SORT B07-RECOUVR-C-ADH ASC [17:9—
//* SB07RECO SORT B07-RECOUVR-C-ORDRE ASC [27:4—
//* SB07RECO SORT B07-RECOUVR-C-EXE-AFFECT ASC [35:4—
//* SB07RECO SORT B07-RECOUVR-C-PER-AFFECT ASC [40:2—
//* SB07RECO SORT B07-RECOUVR-C-TYPE-PER ASC [42:1—
//* SB07RECO SORT B07-RECOUVR-C-ORDRE-CTX ASC [44:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRE58.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,1,CH,A,
               17,9,CH,A,
               27,4,CH,A,
               35,4,CH,A,
               40,2,CH,A,
               42,1,CH,A,
               44,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE58  DD DSN=&MIG.CRE58.FCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE58
//*----------------------------------------------------------------
//PVHCRE58 EXEC PGM=PVHCRE58,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE03W DD DISP=SHR,DSN=&MIG.CRE58.FCRE03W
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE58.B07RECO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE58  DD DSN=&MIG.CRE58.FCRE58,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE58  DD DSN=&MIG.CRE04.FCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01C DD DSN=&MIG.CRE04.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE08
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE08 SORT FCRE03-C-GRPT-GEST ASC [24:1—
//* SFCRE08 SORT FCRE03-C-ADH ASC [25:9—
//* SFCRE08 SORT FCRE03-C-ORDRE ASC [34:4—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(71:2) ASC [124:2—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(73:1) ASC [126:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE58.FCRE58
//SORTOUT  DD DSN=&MIG.CRE04.FCRE58,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A,
               124,2,CH,A,
               126,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01C
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01C SORT FCRE01C-C-GRPT-GEST ASC [1:1—
//* SFCRE01C SORT FCRE01C-C-ADH ASC [2:9—
//* SFCRE01C SORT FCRE01C-C-ORDRE ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE61.FCRE01C
//SORTOUT  DD DSN=&MIG.CRE04.FCRE01C,
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
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE04  DD DSN=&MIG.CRE04.FCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE04
//*----------------------------------------------------------------
//PVHCRE04 EXEC PGM=PVHCRE04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TABVH.TPERIOD
//SFCRE58  DD DISP=SHR,DSN=&MIG.CRE04.FCRE58
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SFCRE01C DD DISP=SHR,DSN=&MIG.CRE04.FCRE01C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE04  DD DSN=&MIG.CRE04.FCRE04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE56
//*----------------------------------------------------------------
//DEL5601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE04  DD DSN=&MIG.CRE56.FCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCONTRAT DD DSN=&MIG.CRE56.CONTRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE04
//*----------------------------------------------------------------
//SORT5601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(171:12) ASC [224:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE04.FCRE04
//SORTOUT  DD DSN=&MIG.CRE56.FCRE04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(224,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CONTRAT
//*----------------------------------------------------------------
//SORT5602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCONTRAT SORT A03-CONTRAT-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.A03CONTR
//SORTOUT  DD DSN=&MIG.CRE56.CONTRAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE56  DD DSN=&MIG.CRE56.FCRE56,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE56,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE56,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE56,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE56
//*----------------------------------------------------------------
//PVHCRE56 EXEC PGM=PVHCRE56,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE56,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE56,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE56,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE04  DD DISP=SHR,DSN=&MIG.CRE56.FCRE04
//SCONTRAT DD DISP=SHR,DSN=&MIG.CRE56.CONTRAT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE56  DD DSN=&MIG.CRE56.FCRE56,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE36
//*----------------------------------------------------------------
//DEL3601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTGAR DD DSN=&MIG.CRE36.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&MIG.CRE36.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT3601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTGAR SORT CNTGAR-NO-ORD-ADHS ASC [41:3—
//* STCNTGAR SORT CNTGAR-NO-ORD-CPRO ASC [95:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE36.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               95,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TPOPCPI
//*----------------------------------------------------------------
//SORT3602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STPOPCPI SORT TAB-POP-CPI-ID-TECH-UR-ADHR ASC [21:20—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-ADHS ASC [41:3—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-CPRO ASC [64:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TPOPCPI
//SORTOUT  DD DSN=&MIG.CRE36.TPOPCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKGAR   DD DSN=&MIG.CRE36.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE36
//*----------------------------------------------------------------
//PVHCRE36 EXEC PGM=PVHCRE36,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE36.TCNTGAR
//STPOPCPI DD DISP=SHR,DSN=&MIG.CRE36.TPOPCPI
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKGAR   DD DSN=&MIG.CRE36.WKGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE58  DD DSN=&MIG.CRE05.FCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE05.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKGAR   DD DSN=&MIG.CRE05.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE04
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE04 SORT FCRE04-C-GRPT-GEST ASC [24:1—
//* SFCRE04 SORT FCRE04-C-ADH ASC [25:9—
//* SFCRE04 SORT FCRE04-C-ORDRE ASC [34:4—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(171:12) ASC [224:12—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(183:5) ASC [236:5—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(188:1) ASC [241:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE56.FCRE56
//SORTOUT  DD DSN=&MIG.CRE05.FCRE04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               224,12,CH,A,
               236,5,CH,A,
               241,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* STCNTGAR SORT CNTGAR-C-ID-CONTRAT ASC [175:12—
//* STCNTGAR SORT CNTGAR-C-GAR ASC [223:5—
//* STCNTGAR SORT CNTGAR-C-CADRE ASC [228:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE05.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A,
               175,12,CH,A,
               223,5,CH,A,
               228,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKGAR
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* SWKGAR SORT WKGAR-C-GRPT-GEST ASC [91:1—
//* SWKGAR SORT WKGAR-C-ADH ASC [92:9—
//* SWKGAR SORT WKGAR-C-ORDRE ASC [101:4—
//* SWKGAR SORT WKGAR-C-ID-CONTRAT ASC [27:12—
//* SWKGAR SORT WKGAR-C-GAR ASC [56:5—
//* SWKGAR SORT WKGAR-C-CADRE ASC [61:1—
//* SWKGAR SORT WKGAR-C-CPI ASC [39:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE36.WKGAR
//SORTOUT  DD DSN=&MIG.CRE05.WKGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(91,1,CH,A,
               92,9,CH,A,
               101,4,CH,A,
               27,12,CH,A,
               56,5,CH,A,
               61,1,CH,A,
               39,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05D DD DSN=&MIG.CRE05.FCRE05D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05C DD DSN=&MIG.CRE05.FCRE05C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05A DD DSN=&MIG.CRE05.FCRE05A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05B DD DSN=&MIG.CRE05.FCRE05B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SGARANN  DD DSN=&CIB.FEXCL.GARANN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SGARFER  DD DSN=&CIB.FEXCL.GARFER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE05
//*----------------------------------------------------------------
//PVHCRE05 EXEC PGM=PVHCRE05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE04  DD DISP=SHR,DSN=&MIG.CRE05.FCRE04
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE05.TCNTGAR
//SWKGAR   DD DISP=SHR,DSN=&MIG.CRE05.WKGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE05D DD DSN=&MIG.CRE05.FCRE05D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE05C DD DSN=&MIG.CRE05.FCRE05C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE05A DD DSN=&MIG.CRE05.FCRE05A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCRE05B DD DSN=&MIG.CRE05.FCRE05B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SGARANN  DD DSN=&CIB.FEXCL.GARANN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SGARFER  DD DSN=&CIB.FEXCL.GARFER,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE26
//*----------------------------------------------------------------
//DEL2601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE26.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE26.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT2601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE26.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT2602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE26.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE26  DD DSN=&MIG.CRE26.FCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE26 DD DSN=&MIG.CRE26.BFCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE26
//*----------------------------------------------------------------
//PVHCRE26 EXEC PGM=PVHCRE26,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE26.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE26.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE26  DD DSN=&MIG.CRE26.FCRE26,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SBFCRE26 DD DSN=&MIG.CRE26.BFCRE26,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE06.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE06.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-IDPOP [456:9—
//* SFCRE05 SORT FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE26.FCRE26
//SORTOUT  DD DSN=&MIG.CRE06.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-IDPOP ASC [456:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE26.BFCRE26
//SORTOUT  DD DSN=&MIG.CRE06.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE06  DD DSN=&MIG.CRE06.FCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD4  DD DSN=&MIG.CRE06.FPERD4,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE06
//*----------------------------------------------------------------
//PVHCRE06 EXEC PGM=PVHCRE06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE06.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE06.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE06  DD DSN=&MIG.CRE06.FCRE06,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD4  DD DSN=&MIG.CRE06.FPERD4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE57
//*----------------------------------------------------------------
//DEL5701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE57.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE57.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT5701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE57.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               128,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT5702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE57.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               128,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE57  DD DSN=&MIG.CRE57.FCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE57 DD DSN=&MIG.CRE57.BFCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE57
//*----------------------------------------------------------------
//PVHCRE57 EXEC PGM=PVHCRE57,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE57.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE57.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE57  DD DSN=&MIG.CRE57.FCRE57,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SBFCRE57 DD DSN=&MIG.CRE57.BFCRE57,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE07.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE07.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-IDPOP ASC [456:9—
//* SFCRE05 SORT F57-C-ACTION ASC [474:1—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* SFCRE05 SORT FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE57.FCRE57
//SORTOUT  DD DSN=&MIG.CRE07.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               474,1,CH,A,
               128,5,CH,A,
               38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-IDPOP ASC [456:9—
//* SBFCRE05 SORT BF57-C-ACTION ASC [474:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE57.BFCRE57
//SORTOUT  DD DSN=&MIG.CRE07.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               474,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE07  DD DSN=&MIG.CRE07.FCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCD21 DD DSN=&CIB.FEXCL.ECRD21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD5  DD DSN=&MIG.CRE07.FPERD5,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE07
//*----------------------------------------------------------------
//PVHCRE07 EXEC PGM=PVHCRE07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE07.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE07.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE07  DD DSN=&MIG.CRE07.FCRE07,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCD21 DD DSN=&CIB.FEXCL.ECRD21,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD5  DD DSN=&MIG.CRE07.FPERD5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE08.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE08.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE08.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT B-FCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT B-FCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT B-FCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* SBFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//         DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE08.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE08  DD DSN=&MIG.CRE08.FCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE08
//*----------------------------------------------------------------
//PVHCRE08 EXEC PGM=PVHCRE08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE08.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE08.BFCRE05
//*SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE08  DD DSN=&MIG.CRE08.FCRE08,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE08  DD DSN=&MIG.CRE09.FCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PERI DD DSN=&MIG.CRE09.B05PERI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE08
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE08 SORT F8-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE08.FCRE08
//SORTOUT  DD DSN=&MIG.CRE09.FCRE08,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PERI
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT B05-PER-DEB-C-ID-PER-DEBIT ASC [30:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE09.B05PERI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(30,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE09  DD DSN=&MIG.CRE09.FCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLSO DD DSN=&CIB.FEXCL.MAJPESO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLNE DD DSN=&CIB.FEXCL.MAJPENE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE09
//*----------------------------------------------------------------
//PVHCRE09 EXEC PGM=PVHCRE09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE08  DD DISP=SHR,DSN=&MIG.CRE09.FCRE08
//SB05ECRI DD DISP=SHR,DSN=&TRI.B05ECRI2
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE09.B05PERI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE09  DD DSN=&MIG.CRE09.FCRE09,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLSO DD DSN=&CIB.FEXCL.MAJPESO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLNE DD DSN=&CIB.FEXCL.MAJPENE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE19.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE19.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCFCRE05 DD DSN=&MIG.CRE19.CFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRP-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE19.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               135,3,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT1902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-GRP-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SCFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) ASC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE19.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               135,3,CH,A,
               140,21,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CFCRE05
//*----------------------------------------------------------------
//SORT1903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCFCRE05 SORT B-FCRE05-C-GRPT-GEST ASC [24:1—
//* SCFCRE05 SORT B-FCRE05-C-ADH ASC [25:9—
//* SCFCRE05 SORT B-FCRE05-C-ORDRE ASC [34:4—
//* SCFCRE05 SORT B-FCRE05-C-EXE-AFFECT ASC [50:4—
//* SCFCRE05 SORT B-FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SCFCRE05 SORT B-FCRE05-C-TYPE-PER-M ASC [345:1—
//* SCFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//         DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE19.CFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE19.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLSO DD DSN=&CIB.FEXCL.FRAISSO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLNE DD DSN=&CIB.FEXCL.FRAISNE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE19
//*----------------------------------------------------------------
//PVHCRE19 EXEC PGM=PVHCRE19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE19.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE19.BFCRE05
//SCFCRE05 DD DISP=SHR,DSN=&MIG.CRE19.CFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE19  DD DSN=&MIG.CRE19.FCRE19,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLSO DD DSN=&CIB.FEXCL.FRAISSO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLNE DD DSN=&CIB.FEXCL.FRAISNE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE10.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05C DD DSN=&MIG.CRE10.FCRE05C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE19
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19 SORT F19-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE19 SORT F19-FCRE05-C-ADH ASC [25:9—
//* SFCRE19 SORT F19-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE19 SORT F19-FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE19 SORT F19-FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE19 SORT F19-FCRE05-C-TYPE-PER-M ASC [345:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE19.FCRE19
//SORTOUT  DD DSN=&MIG.CRE10.FCRE19,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05C
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05C SORT CFCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05C SORT CFCRE05-C-ADH ASC [25:9—
//* SFCRE05C SORT CFCRE05-C-ORDRE ASC [34:4—
//* SFCRE05C SORT CFCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05C SORT CFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05C SORT CFCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05C SORT CFCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//         DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE10.FCRE05C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE10R DD DSN=&MIG.CRE10.FCRE10R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE10
//*----------------------------------------------------------------
//PVHCRE10 EXEC PGM=PVHCRE10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE19  DD DISP=SHR,DSN=&MIG.CRE10.FCRE19
//SFCRE05C DD DISP=SHR,DSN=&MIG.CRE10.FCRE05C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE10R DD DSN=&MIG.CRE10.FCRE10R,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE11.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE11.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE10R DD DSN=&MIG.CRE11.FCRE10R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE19
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19 SORT F19-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* SFCRE19 SORT F19-FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SFCRE19 SORT F19-FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE19.FCRE19
//SORTOUT  DD DSN=&MIG.CRE11.FCRE19,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A,
               135,3,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19B SORT F9B-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* SFCRE19B SORT F9B-FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE11.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE10R
//*----------------------------------------------------------------
//SORT1103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE10R SORT FCRE10R-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE10.FCRE10R
//SORTOUT  DD DSN=&MIG.CRE11.FCRE10R,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE11  DD DSN=&MIG.CRE11.FCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE11
//*----------------------------------------------------------------
//PVHCRE11 EXEC PGM=PVHCRE11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE19  DD DISP=SHR,DSN=&MIG.CRE11.FCRE19
//SFCRE19B DD DISP=SHR,DSN=&MIG.CRE11.FCRE19
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE11.BFCRE05
//SFCRE10R DD DISP=SHR,DSN=&MIG.CRE11.FCRE10R
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE11  DD DSN=&MIG.CRE11.FCRE11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05INFO DD DSN=&MIG.CRE12.B05INFO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPEV DD DSN=&MIG.CRE12.B05OPEV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFO
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFO SORT B05-INFOS-DEBIT-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOD
//SORTOUT  DD DSN=&MIG.CRE12.B05INFO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPEV
//*----------------------------------------------------------------
//SORT1202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPEV SORT B05-OPER-VENTIL-C-OPER ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&ECH.B05OPERV
//SORTOUT  DD DSN=&MIG.CRE12.B05OPEV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE12  DD DSN=&MIG.CRE12.FOPE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE12
//*----------------------------------------------------------------
//PVHCRE12 EXEC PGM=PVHCRE12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE61.B05OPER
//SB05INFO DD DISP=SHR,DSN=&MIG.CRE12.B05INFO
//SB05OPEV DD DISP=SHR,DSN=&MIG.CRE12.B05OPEV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFOPE12  DD DSN=&MIG.CRE12.FOPE12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE12  DD DSN=&MIG.CRE13.FOPE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FOPE12
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFOPE12 SORT FOPE12-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE12.FOPE12
//SORTOUT  DD DSN=&MIG.CRE13.FOPE12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE13  DD DSN=&MIG.CRE13.FOPE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE13
//*----------------------------------------------------------------
//PVHCRE13 EXEC PGM=PVHCRE13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFOPE12  DD DISP=SHR,DSN=&MIG.CRE13.FOPE12
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE59.B07RECO
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE59.B05PER2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFOPE13  DD DSN=&MIG.CRE13.FOPE13,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE13  DD DSN=&MIG.CRE14.FOPE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE14.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FOPE13
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(41:1) ASC [82:1—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(43:9) ASC [84:9—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(53:4) ASC [94:4—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(87:4) ASC [128:4—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(94:1) ASC [135:1—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(92:2) ASC [133:2—
//* SFOPE13 SORT FOPE13-C-ID-PER-DEBIT ASC [14:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE13.FOPE13
//SORTOUT  DD DSN=&MIG.CRE14.FOPE13,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(82,1,CH,A,
               84,9,CH,A,
               94,4,CH,A,
               128,4,CH,A,
               135,1,CH,A,
               133,2,CH,A,
               14,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE14.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE14  DD DSN=&MIG.CRE14.FCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD6  DD DSN=&MIG.CRE14.FPERD6,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE14
//*----------------------------------------------------------------
//PVHCRE14 EXEC PGM=PVHCRE14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TABVH.TPERIOD
//SFOPE13  DD DISP=SHR,DSN=&MIG.CRE14.FOPE13
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE14.TCNTGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE14  DD DSN=&MIG.CRE14.FCRE14,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD6  DD DSN=&MIG.CRE18.FPERD6,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE15
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE15.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05SYNT DD DSN=&MIG.CRE15.B05SYNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE15.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
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
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//         DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE15.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05SYNT
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-GRPT-GEST ASC [1:1—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ADH ASC [3:9—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ORDRE ASC [13:4—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-EXE ASC [18:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05SYNTH
//SORTOUT  DD DSN=&MIG.CRE15.B05SYNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A,
               18,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT1503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE15.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE15.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE15
//*----------------------------------------------------------------
//PVHCRE15 EXEC PGM=PVHCRE15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE15.FCRE05
//SB05SYNT DD DISP=SHR,DSN=&MIG.CRE15.B05SYNT
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE15.TCNTGAR
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE15  DD DSN=&MIG.CRE15.FCRE15,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE25
//*----------------------------------------------------------------
//DEL2501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE25.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT2501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE15.FCRE15
//SORTOUT  DD DSN=&MIG.CRE25.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE25  DD DSN=&MIG.CRE25.FCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE25
//*----------------------------------------------------------------
//PVHCRE25 EXEC PGM=PVHCRE25,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE25.FCRE15
//SB05ECRI DD DISP=SHR,DSN=&TRI.B05ECRI2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE25  DD DSN=&MIG.CRE25.FCRE25,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE16.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT F15-FCRE05-IDPOP ASC [456:9—
//* SFCRE05 SORT F15-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE16.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               456,9,CH,A,
               38,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE16  DD DSN=&MIG.CRE16.FCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD1  DD DSN=&MIG.CRE16.FPERD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE16
//*----------------------------------------------------------------
//PVHCRE16 EXEC PGM=PVHCRE16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE16.FCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE16  DD DSN=&MIG.CRE16.FCRE16,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD1  DD DSN=&MIG.CRE16.FPERD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE17
//*----------------------------------------------------------------
//DEL1701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE17.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(81:1) ASC [134:1—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(207:1) ASC [260:1—
//* SFCRE05 SORT F15-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE17.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               134,1,CH,A,
               260,1,CH,A,
               38,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE17  DD DSN=&MIG.CRE17.FCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD2  DD DSN=&MIG.CRE17.FPERD2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE17
//*----------------------------------------------------------------
//PVHCRE17 EXEC PGM=PVHCRE17,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE17.FCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE17  DD DSN=&MIG.CRE17.FCRE17,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD2  DD DSN=&MIG.CRE17.FPERD2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE18
//*----------------------------------------------------------------
//DEL1801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE18.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SFCRE05 SORT F15-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE18.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               135,3,CH,A,
               38,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE18  DD DSN=&MIG.CRE18.FCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPERD3  DD DSN=&MIG.CRE18.FPERD3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE18
//*----------------------------------------------------------------
//PVHCRE18 EXEC PGM=PVHCRE18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE18.FCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE18  DD DSN=&MIG.CRE18.FCRE18,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPERD3  DD DSN=&MIG.CRE18.FPERD3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICPROF DD DSN=&MIG.CRE20.FICPROF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FICPROF
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFICPROF SORT FICPROF-C-ID-PER-DEBIT ASC [35:12—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(16:7) ASC [62:7—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(66:4) ASC [112:4—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(71:2) ASC [117:2—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(73:1) ASC [119:1—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(75:5) ASC [121:5—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(81:1) ASC [127:1—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(82:3) ASC [128:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE01.FICPROF
//SORTOUT  DD DSN=&MIG.CRE20.FICPROF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(35,12,CH,A,
               62,7,CH,A,
               112,4,CH,A,
               117,2,CH,A,
               119,1,CH,A,
               121,5,CH,A,
               127,1,CH,A,
               128,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFEXCLPR DD DSN=&CIB.FEXCL.PROFIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE20
//*----------------------------------------------------------------
//PVHCRE20 EXEC PGM=PVHCRE20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFICPROF DD DISP=SHR,DSN=&MIG.CRE20.FICPROF
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE09.B05PERI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFEXCLPR DD DSN=&CIB.FEXCL.PROFIT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCRE21
//*----------------------------------------------------------------
//DEL2101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FCRE09   DD DSN=&MIG.CRE21.FCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE09
//*----------------------------------------------------------------
//SORT2101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FCRE09 SORT F09-FCRE05-C-GRPT-GEST ASC [24:1—
//* FCRE09 SORT F09-FCRE05-C-ADH ASC [25:9—
//* FCRE09 SORT F09-FCRE05-C-ORDRE ASC [34:4—
//* FCRE09 SORT F09-FCRE05-C-EXE-AFFECT ASC [50:4—
//* FCRE09 SORT F09-FCRE05-C-ID-INST-M ASC [263:7—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(171:12) ASC [224:12—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(183:5) ASC [236:5—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(188:1) ASC [241:1—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(81:1) ASC [134:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE09.FCRE09
//SORTOUT  DD DSN=&MIG.CRE21.FCRE09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               263,7,CH,A,
               224,12,CH,A,
               236,5,CH,A,
               241,1,CH,A,
               134,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWK1CRE  DD DSN=&MIG.CRE21.WK1CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCRE21
//*----------------------------------------------------------------
//PVHCRE21 EXEC PGM=PVHCRE21,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCRE06   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//FCRE07   DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//*FCRE09   DD DISP=SHR,DSN=&MIG.CRE09.FCRE09
//FCRE09   DD DISP=SHR,DSN=&MIG.CRE21.FCRE09
//FCRE11   DD DISP=SHR,DSN=&MIG.CRE11.FCRE11
//FCRE14   DD DISP=SHR,DSN=&MIG.CRE14.FCRE14
//FCRE16   DD DISP=SHR,DSN=&MIG.CRE16.FCRE16
//FCRE17   DD DISP=SHR,DSN=&MIG.CRE17.FCRE17
//FCRE18   DD DISP=SHR,DSN=&MIG.CRE18.FCRE18
//*--------------<FICHIERS CIBLES>---------------------------------
//SWK1CRE  DD DSN=&MIG.CRE21.WK1CRE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
