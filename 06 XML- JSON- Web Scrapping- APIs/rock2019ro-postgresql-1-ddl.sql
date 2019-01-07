--------------------------------------------------------
--  2019-01-07  
--------------------------------------------------------
DROP TABLE IF EXISTS VANZARI_DISCURI ;
DROP TABLE IF EXISTS DISCURI_CLASAMENTE ;
DROP TABLE IF EXISTS REALIZARE_DISC ;
DROP TABLE IF EXISTS MUZICIENI ;
DROP TABLE IF EXISTS ALCATUIRE_DISC ;
DROP TABLE IF EXISTS DISCOGRAFIE ;
DROP TABLE IF EXISTS FORMATII ;
DROP TABLE IF EXISTS TARI ;



--------------------------------------------------------
--  DDL for Table TARI
--------------------------------------------------------

  CREATE TABLE TARI (	
  	COD_TARA VARCHAR(10), 
	NUME_TARA VARCHAR(255), 
	REGIUNE VARCHAR(255), 
	GRUP_VENIT VARCHAR(255)
   ) ;

--------------------------------------------------------
--  DDL for Table FORMATII
--------------------------------------------------------
  CREATE TABLE FORMATII (	
  	ID_FORMATIE INTEGER, 
	DEN_FORMATIE VARCHAR(255), 
	AN_INFIINTARE NUMERIC(4), 
	COD_TARA VARCHAR(10)
   ) ;

--------------------------------------------------------
--  DDL for Table DISCOGRAFIE
--------------------------------------------------------
  CREATE TABLE DISCOGRAFIE (
  	ID_DISC INTEGER, 
	TITLU_DISC VARCHAR(255), 
	ID_FORMATIE INTEGER,
	ALBUM_SINGLE_COMPILATIE CHAR(1) DEFAULT 'A'
		CONSTRAINT ck_album_single_comp CHECK (ALBUM_SINGLE_COMPILATIE IN ('A', 'S', 'C')), 	
	STUDIO_SAU_LIVE CHAR(1) DEFAULT 'S'
		CONSTRAINT ck_studio_sau_live CHECK (STUDIO_SAU_LIVE IN ('S', 'L')), 
	AN_LANSARE NUMERIC(4), 
	DATA_LANSARE DATE,
	ORDINE_AN NUMERIC(2), 	
	CASA_DISCURI VARCHAR(100),
	PRIMA_LUNA_VANZARI NUMERIC(2), 
	COEFICIENT_VOLUM_VANZARI NUMERIC(5,2)
   ) ;
   
--------------------------------------------------------
--  DDL for Table ALCATUIRE_DISC
--------------------------------------------------------
  CREATE TABLE ALCATUIRE_DISC (	
  	ID_DISC INTEGER, 
	NR_ORDINE NUMERIC(4), 
	FATA_DISC VARCHAR(2), 
	TITLU_PIESA VARCHAR(255), 
	COMPOZITORI VARCHAR(255), 
	VOCALISTI VARCHAR(255), 
	DURATA_SEC NUMERIC(10)
   ) ;

--------------------------------------------------------
--  DDL for Table MUZICIENI
--------------------------------------------------------
  CREATE TABLE MUZICIENI (	
   	ID_PERSOANA INTEGER, 
	PRENUME_SCENA VARCHAR(255), 
	NUME_SCENA VARCHAR(255), 
	PRENUME_REAL VARCHAR(255), 
	NUME_REAL VARCHAR(255), 
	DATA_NASTERE DATE, 
	DATA_DECES DATE, 
	COD_TARA VARCHAR(10)
   ) ;
   
--------------------------------------------------------
--  DDL for Table REALIZARE_DISC
--------------------------------------------------------
  CREATE TABLE REALIZARE_DISC (	
  	ID_DISC INTEGER, 
	ID_MUZICIAN INTEGER, 
	ROLURI VARCHAR(255)
   ) ;

--------------------------------------------------------
--  DDL for Table DISCURI_CLASAMENTE
--------------------------------------------------------
  CREATE TABLE DISCURI_CLASAMENTE (	
  	ID_DISC INTEGER, 
  	COD_TARA VARCHAR(10), 
	CEA_MAI_BUNA_CLASARE INTEGER,
	COMENTARII VARCHAR(255)
   ) ;


   
--------------------------------------------------------
--  DDL for Table VANZARI_DISCURI
--------------------------------------------------------
  CREATE TABLE VANZARI_DISCURI (	
  	ID_DISC INTEGER, 
	COD_TARA VARCHAR(10), 
	AN NUMERIC(4), 
	LUNA NUMERIC(2), 
	NR_BUCATI NUMERIC(15), 
	VANZARI_USD NUMERIC(20,2)
   ) ;

--------------------------------------------------------
--  Constraints for Table DISCURI_CLASAMENTE
--------------------------------------------------------
  ALTER TABLE DISCURI_CLASAMENTE ADD PRIMARY KEY (ID_DISC, COD_TARA);
   
   
 --------------------------------------------------------
--  Constraints for Table VANZARI_DISCURI
--------------------------------------------------------
  ALTER TABLE VANZARI_DISCURI ADD PRIMARY KEY (ID_DISC, COD_TARA, AN, LUNA);

--------------------------------------------------------
--  Constraints for Table REALIZARE_DISC
--------------------------------------------------------
  ALTER TABLE REALIZARE_DISC ADD PRIMARY KEY (ID_DISC, ID_MUZICIAN);

--------------------------------------------------------
--  Constraints for Table FORMATII
--------------------------------------------------------
  ALTER TABLE FORMATII ADD PRIMARY KEY (ID_FORMATIE);
  ALTER TABLE FORMATII ADD UNIQUE (DEN_FORMATIE);

--------------------------------------------------------
--  Constraints for Table ALCATUIRE_DISC
--------------------------------------------------------
  ALTER TABLE ALCATUIRE_DISC ADD PRIMARY KEY (ID_DISC, NR_ORDINE, TITLU_PIESA);

--------------------------------------------------------
--  Constraints for Table MUZICIENI
--------------------------------------------------------
  ALTER TABLE MUZICIENI ADD PRIMARY KEY (ID_PERSOANA) ;

--------------------------------------------------------
--  Constraints for Table DISCOGRAFIE
--------------------------------------------------------
  ALTER TABLE DISCOGRAFIE ADD PRIMARY KEY (ID_DISC);
  ALTER TABLE DISCOGRAFIE ADD CONSTRAINT TITLU_ALBUM_NN CHECK (TITLU_DISC IS  NOT NULL );
  ALTER TABLE DISCOGRAFIE ADD UNIQUE (ID_FORMATIE, TITLU_DISC) ;

--------------------------------------------------------
--  Constraints for Table TARI
--------------------------------------------------------
  ALTER TABLE TARI ADD PRIMARY KEY (COD_TARA);
  ALTER TABLE TARI ADD UNIQUE (NUME_TARA) ;

--------------------------------------------------------
--  Ref Constraints for Table ALCATUIRE_DISC
--------------------------------------------------------
  ALTER TABLE ALCATUIRE_DISC ADD CONSTRAINT ALCAT_DISC_DISC_REF FOREIGN KEY (ID_DISC)
	  REFERENCES DISCOGRAFIE (ID_DISC) ;
	  
--------------------------------------------------------
--  Ref Constraints for Table DISCOGRAFIE
--------------------------------------------------------
  ALTER TABLE DISCOGRAFIE ADD CONSTRAINT DISCOG_FORMATII_REF FOREIGN KEY (ID_FORMATIE)
	  REFERENCES FORMATII (ID_FORMATIE) ;

--------------------------------------------------------
--  Ref Constraints for Table FORMATII
--------------------------------------------------------
  ALTER TABLE FORMATII ADD CONSTRAINT FORMATII_TARI_REF FOREIGN KEY (COD_TARA)
	  REFERENCES TARI (COD_TARA) ;

--------------------------------------------------------
--  Ref Constraints for Table MUZICIENI
--------------------------------------------------------
  ALTER TABLE MUZICIENI ADD CONSTRAINT MUZICIENI_TARI_REF FOREIGN KEY (COD_TARA)
	  REFERENCES TARI (COD_TARA) ;
	  
--------------------------------------------------------
--  Ref Constraints for Table REALIZARE_DISC
--------------------------------------------------------
  ALTER TABLE REALIZARE_DISC ADD CONSTRAINT REALIZ_DISC_DISC_REF FOREIGN KEY (ID_DISC)
	  REFERENCES DISCOGRAFIE (ID_DISC) ;
  ALTER TABLE REALIZARE_DISC ADD CONSTRAINT REALIZ_DISC_MUZICIENI_REF FOREIGN KEY (ID_MUZICIAN)
	  REFERENCES MUZICIENI (ID_PERSOANA) ;

--------------------------------------------------------
--  Ref Constraints for Table DISCURI_CLASAMENTE
--------------------------------------------------------
  ALTER TABLE DISCURI_CLASAMENTE ADD CONSTRAINT DC_DISC_REF FOREIGN KEY (ID_DISC)
	  REFERENCES DISCOGRAFIE (ID_DISC) ;
  ALTER TABLE DISCURI_CLASAMENTE ADD CONSTRAINT DC_TARI_REF FOREIGN KEY (COD_TARA)
	  REFERENCES TARI (COD_TARA) ;

--------------------------------------------------------
--  Ref Constraints for Table VANZARI_DISCURI
--------------------------------------------------------
  ALTER TABLE VANZARI_DISCURI ADD CONSTRAINT VANZ_DISC_REF FOREIGN KEY (ID_DISC)
	  REFERENCES DISCOGRAFIE (ID_DISC) ;
  ALTER TABLE VANZARI_DISCURI ADD CONSTRAINT VANZ_TARI_REF FOREIGN KEY (COD_TARA)
	  REFERENCES TARI (COD_TARA) ;
  
  
  
  