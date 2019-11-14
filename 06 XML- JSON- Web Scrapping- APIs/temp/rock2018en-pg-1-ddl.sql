--------------------------------------------------------
--  File created - Tuesday-October-16-2018   
--------------------------------------------------------


DROP TABLE IF EXISTS ALBUM_SALES ;
DROP TABLE IF EXISTS ALBUM_PARTICIPATION ;
DROP TABLE IF EXISTS MUSICIANS ;
DROP TABLE IF EXISTS TRACKS_ON_RECORDS ;
DROP TABLE IF EXISTS ALBUMS ;
DROP TABLE IF EXISTS BANDS ;
DROP TABLE IF EXISTS COUNTRIES ;



--------------------------------------------------------
--  DDL for Table COUNTRIES
--------------------------------------------------------

  CREATE TABLE COUNTRIES (	
  	COUNTRY_CODE VARCHAR(10), 
	COUNTRY_NAME VARCHAR(255), 
	REGION VARCHAR(255), 
	INCOME_GROUP VARCHAR(255)
   ) ;

--------------------------------------------------------
--  DDL for Table BANDS
--------------------------------------------------------
  CREATE TABLE BANDS (	
  	BAND_ID INTEGER, 
	BAND_NAME VARCHAR(255), 
	START_YEAR NUMERIC(4), 
	COUNTRY_CODE VARCHAR(10)
   ) ;

--------------------------------------------------------
--  DDL for Table ALBUMS
--------------------------------------------------------
  CREATE TABLE ALBUMS (
  	ALBUM_ID INTEGER, 
	ALBUM_TITLE VARCHAR(255), 
	BAND_ID INTEGER, 
	RELEASE_YEAR NUMERIC(4), 
	ORDER_WITHIN_YEAR NUMERIC(2), 
	FIRST_MONTH_WITH_SALES NUMERIC(2), 
	SALES_COEFF NUMERIC(5,2)
   ) ;
   
--------------------------------------------------------
--  DDL for Table TRACKS_ON_RECORDS
--------------------------------------------------------
  CREATE TABLE TRACKS_ON_RECORDS (	
  	ALBUM_ID INTEGER, 
	ORDER_ON_ALBUM NUMERIC(4), 
	ALBUM_SIDE VARCHAR(2), 
	TRACK_TITLE VARCHAR(255), 
	SONG_WRITERS VARCHAR(255), 
	LEAD_VOCALS VARCHAR(255), 
	DURATION_SECS NUMERIC(10)
   ) ;

--------------------------------------------------------
--  DDL for Table MUSICIANS
--------------------------------------------------------
  CREATE TABLE MUSICIANS (	
   	PERSON_ID INTEGER, 
	OFICIAL_FIRST_NAME VARCHAR(255), 
	OFICIAL_LAST_NAME VARCHAR(255), 
	REAL_FIRST_NAME VARCHAR(255), 
	REAL_LAST_NAME VARCHAR(255), 
	BIRTH_DATE DATE, 
	DEATH_DATE DATE, 
	COUNTRY_CODE VARCHAR(10)
   ) ;
   
--------------------------------------------------------
--  DDL for Table ALBUM_PARTICIPATION
--------------------------------------------------------
  CREATE TABLE ALBUM_PARTICIPATION (	
  	ALBUM_ID INTEGER, 
	MUSICIAN_ID INTEGER, 
	CONTRIBUTION VARCHAR(255)
   ) ;
   
--------------------------------------------------------
--  DDL for Table ALBUM_SALES
--------------------------------------------------------

  CREATE TABLE ALBUM_SALES (	
  	ALBUM_ID INTEGER, 
	COUNTRY_CODE VARCHAR(10), 
	YEAR NUMERIC(4), 
	MONTH NUMERIC(2), 
	UNITS_SOLD NUMERIC(15), 
	SALES_USD NUMERIC(20,2)
   ) ;
   
   
 --------------------------------------------------------
--  Constraints for Table ALBUM_SALES
--------------------------------------------------------
  ALTER TABLE ALBUM_SALES ADD PRIMARY KEY (ALBUM_ID, COUNTRY_CODE, YEAR, MONTH);

--------------------------------------------------------
--  Constraints for Table ALBUM_PARTICIPATION
--------------------------------------------------------
  ALTER TABLE ALBUM_PARTICIPATION ADD PRIMARY KEY (ALBUM_ID, MUSICIAN_ID);

--------------------------------------------------------
--  Constraints for Table BANDS
--------------------------------------------------------
  ALTER TABLE BANDS ADD PRIMARY KEY (BAND_ID);
  ALTER TABLE BANDS ADD UNIQUE (BAND_NAME);

--------------------------------------------------------
--  Constraints for Table TRACKS_ON_RECORDS
--------------------------------------------------------
  ALTER TABLE TRACKS_ON_RECORDS ADD PRIMARY KEY (ALBUM_ID, ORDER_ON_ALBUM, TRACK_TITLE);

--------------------------------------------------------
--  Constraints for Table MUSICIANS
--------------------------------------------------------
  ALTER TABLE MUSICIANS ADD PRIMARY KEY (PERSON_ID) ;

--------------------------------------------------------
--  Constraints for Table ALBUMS
--------------------------------------------------------
  ALTER TABLE ALBUMS ADD PRIMARY KEY (ALBUM_ID);
  ALTER TABLE ALBUMS ADD CONSTRAINT ALBUM_TITLE_NN CHECK (ALBUM_TITLE IS NOT NULL );
  ALTER TABLE ALBUMS ADD UNIQUE (BAND_ID, ALBUM_TITLE) ;

--------------------------------------------------------
--  Constraints for Table COUNTRIES
--------------------------------------------------------
  ALTER TABLE COUNTRIES ADD PRIMARY KEY (COUNTRY_CODE);
  ALTER TABLE COUNTRIES ADD UNIQUE (COUNTRY_NAME) ;

--------------------------------------------------------
--  Ref Constraints for Table TRACKS_ON_RECORDS
--------------------------------------------------------
  ALTER TABLE TRACKS_ON_RECORDS ADD CONSTRAINT TRACKS_RECS_ALBUMS_REF FOREIGN KEY (ALBUM_ID)
	  REFERENCES ALBUMS (ALBUM_ID) ;
	  
--------------------------------------------------------
--  Ref Constraints for Table ALBUMS
--------------------------------------------------------
  ALTER TABLE ALBUMS ADD CONSTRAINT ALBUMS_BANDS_REF FOREIGN KEY (BAND_ID)
	  REFERENCES BANDS (BAND_ID) ;

--------------------------------------------------------
--  Ref Constraints for Table BANDS
--------------------------------------------------------
  ALTER TABLE BANDS ADD CONSTRAINT BANDS_COUNTRIES_REF FOREIGN KEY (COUNTRY_CODE)
	  REFERENCES COUNTRIES (COUNTRY_CODE) ;

--------------------------------------------------------
--  Ref Constraints for Table MUSICIANS
--------------------------------------------------------
  ALTER TABLE MUSICIANS ADD CONSTRAINT MUSICIANS_COUNTRIES_REF FOREIGN KEY (COUNTRY_CODE)
	  REFERENCES COUNTRIES (COUNTRY_CODE) ;
	  
--------------------------------------------------------
--  Ref Constraints for Table ALBUM_PARTICIPATION
--------------------------------------------------------
  ALTER TABLE ALBUM_PARTICIPATION ADD CONSTRAINT PARTICIP_ALBUMS_REF FOREIGN KEY (ALBUM_ID)
	  REFERENCES ALBUMS (ALBUM_ID) ;
  ALTER TABLE ALBUM_PARTICIPATION ADD CONSTRAINT PARTICIP_MUSICIANS_REF FOREIGN KEY (MUSICIAN_ID)
	  REFERENCES MUSICIANS (PERSON_ID) ;

--------------------------------------------------------
--  Ref Constraints for Table ALBUM_SALES
--------------------------------------------------------
  ALTER TABLE ALBUM_SALES ADD CONSTRAINT SALES_ALBUMS_REF FOREIGN KEY (ALBUM_ID)
	  REFERENCES ALBUMS (ALBUM_ID) ;
  ALTER TABLE ALBUM_SALES ADD CONSTRAINT SALED_COUNTRIES_REF FOREIGN KEY (COUNTRY_CODE)
	  REFERENCES COUNTRIES (COUNTRY_CODE) ;
  
  