--------------------------------------------------------
--  2018-10-18  
--------------------------------------------------------
DROP TABLE IF EXISTS VANZARI_DISCURI ;
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
  	ID_ALBUM INTEGER, 
	TITLU_ALBUM VARCHAR(255), 
	ID_FORMATIE INTEGER, 
	AN_LANSARE NUMERIC(4), 
	ORDINE_AN NUMERIC(2), 
	PRIMA_LUNA_VANZARI NUMERIC(2), 
	COEFICIENT_VOLUM_VANZARI NUMERIC(5,2)
   ) ;
   
--------------------------------------------------------
--  DDL for Table ALCATUIRE_DISC
--------------------------------------------------------
  CREATE TABLE ALCATUIRE_DISC (	
  	ID_ALBUM INTEGER, 
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
  	ID_ALBUM INTEGER, 
	ID_MUZICIAN INTEGER, 
	ROLURI VARCHAR(255)
   ) ;
   
--------------------------------------------------------
--  DDL for Table VANZARI_DISCURI
--------------------------------------------------------

  CREATE TABLE VANZARI_DISCURI (	
  	ID_ALBUM INTEGER, 
	COD_TARA VARCHAR(10), 
	AN NUMERIC(4), 
	LUNA NUMERIC(2), 
	NR_BUCATI NUMERIC(15), 
	VANZARI_USD NUMERIC(20,2)
   ) ;
   
   
 --------------------------------------------------------
--  Constraints for Table VANZARI_DISCURI
--------------------------------------------------------
  ALTER TABLE VANZARI_DISCURI ADD PRIMARY KEY (ID_ALBUM, COD_TARA, AN, LUNA);

--------------------------------------------------------
--  Constraints for Table REALIZARE_DISC
--------------------------------------------------------
  ALTER TABLE REALIZARE_DISC ADD PRIMARY KEY (ID_ALBUM, ID_MUZICIAN);

--------------------------------------------------------
--  Constraints for Table FORMATII
--------------------------------------------------------
  ALTER TABLE FORMATII ADD PRIMARY KEY (ID_FORMATIE);
  ALTER TABLE FORMATII ADD UNIQUE (DEN_FORMATIE);

--------------------------------------------------------
--  Constraints for Table ALCATUIRE_DISC
--------------------------------------------------------
  ALTER TABLE ALCATUIRE_DISC ADD PRIMARY KEY (ID_ALBUM, NR_ORDINE, TITLU_PIESA);

--------------------------------------------------------
--  Constraints for Table MUZICIENI
--------------------------------------------------------
  ALTER TABLE MUZICIENI ADD PRIMARY KEY (ID_PERSOANA) ;

--------------------------------------------------------
--  Constraints for Table DISCOGRAFIE
--------------------------------------------------------
  ALTER TABLE DISCOGRAFIE ADD PRIMARY KEY (ID_ALBUM);
  ALTER TABLE DISCOGRAFIE ADD CONSTRAINT TITLU_ALBUM_NN CHECK (TITLU_ALBUM IS  NOT NULL );
  ALTER TABLE DISCOGRAFIE ADD UNIQUE (ID_FORMATIE, TITLU_ALBUM) ;

--------------------------------------------------------
--  Constraints for Table TARI
--------------------------------------------------------
  ALTER TABLE TARI ADD PRIMARY KEY (COD_TARA);
  ALTER TABLE TARI ADD UNIQUE (NUME_TARA) ;

--------------------------------------------------------
--  Ref Constraints for Table ALCATUIRE_DISC
--------------------------------------------------------
  ALTER TABLE ALCATUIRE_DISC ADD CONSTRAINT ALCAT_DISC_DISC_REF FOREIGN KEY (ID_ALBUM)
	  REFERENCES DISCOGRAFIE (ID_ALBUM) ;
	  
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
  ALTER TABLE REALIZARE_DISC ADD CONSTRAINT REALIZ_DISC_DISC_REF FOREIGN KEY (ID_ALBUM)
	  REFERENCES DISCOGRAFIE (ID_ALBUM) ;
  ALTER TABLE REALIZARE_DISC ADD CONSTRAINT REALIZ_DISC_MUZICIENI_REF FOREIGN KEY (ID_MUZICIAN)
	  REFERENCES MUZICIENI (ID_PERSOANA) ;

--------------------------------------------------------
--  Ref Constraints for Table VANZARI_DISCURI
--------------------------------------------------------
  ALTER TABLE VANZARI_DISCURI ADD CONSTRAINT VANZ_DISC_REF FOREIGN KEY (ID_ALBUM)
	  REFERENCES DISCOGRAFIE (ID_ALBUM) ;
  ALTER TABLE VANZARI_DISCURI ADD CONSTRAINT VANZ_TARI_REF FOREIGN KEY (COD_TARA)
	  REFERENCES TARI (COD_TARA) ;
  
  
  
  