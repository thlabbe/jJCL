//J9VHTR3  JOB UTI00TX0,'VH TRI GRECCO',CLASS=Z,MSGCLASS=X,
//*        RESTART=SORTTR04,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHTRI
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SECRIT1  DD DSN=&TRI.B05ECRI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT2  DD DSN=&TRI.B05ECRI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT3  DD DSN=&TRI.B05ECRI3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT4  DD DSN=&TRI.B05ECRI4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI1 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORTTR01 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W2-PT-GEST ASC [49:1�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-ADH ASC [51:9�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-ORDRE ASC [61:4�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-D-CREATION ASC [129:8�
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK11 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK12 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK13 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK14 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK15 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK16 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK17 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK18 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK19 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK20 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK21 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK22 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK23 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK24 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&TRI.B05ECRI1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,1,CH,A,
               51,9,CH,A,
               61,4,CH,A,
               129,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI2 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORTTR02 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W1-R-DEBIT ASC [37:12�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W7-E-DEBIT ASC [82:3�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W14-ONTRAT ASC [171:12�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-GAR ASC [183:5�
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-CADRE ASC [188:1�
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK11 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK12 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK13 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK14 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK15 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK16 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK17 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK18 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK19 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK20 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK21 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK22 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK23 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK24 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&TRI.B05ECRI2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(37,12,CH,A,
               82,3,CH,A,
               171,12,CH,A,
               183,5,CH,A,
               188,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI3 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORTTR03 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-ID-OPER ASC [24:12�
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK11 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK12 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK13 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK14 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK15 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK16 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK17 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK18 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK19 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK20 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK21 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK22 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK23 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK24 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&TRI.B05ECRI3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI4 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORTTR04 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECR SORT B05-ECRITURE-COMPTE-W1-R-DEBIT(3:10) ASC [39:10�
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK11 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK12 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK13 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK14 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK15 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK16 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK17 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK18 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK19 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK20 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK21 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK22 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK23 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK24 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&TRI.B05ECRI4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,10,CH,A)
/*