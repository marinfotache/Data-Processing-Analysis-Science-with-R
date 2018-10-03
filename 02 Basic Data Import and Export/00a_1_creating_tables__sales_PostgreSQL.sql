-- create tables of Sales sub-schema in PostgreSQL

DROP TABLE IF EXISTS receipt_details  ;
DROP TABLE IF EXISTS receipts  ;
DROP TABLE IF EXISTS invoice_details ;
DROP TABLE IF EXISTS invoices  ;
DROP TABLE IF EXISTS products  ;
DROP TABLE IF EXISTS contacts  ;
DROP TABLE IF EXISTS people  ;
DROP TABLE IF EXISTS customers  ;
DROP TABLE IF EXISTS postcodes  ;
DROP TABLE IF EXISTS counties  ;

CREATE TABLE counties (
    countyCode CHAR(2)
        CONSTRAINT pk_counties PRIMARY KEY
        CONSTRAINT ck_countyCode CHECK (countyCode=LTRIM(UPPER(countyCode))),
    countyName VARCHAR(25)
        CONSTRAINT un_countyName UNIQUE
        CONSTRAINT nn_countyName NOT NULL
        CONSTRAINT ck_countyName CHECK (countyName=LTRIM(INITCAP(countyName))),
    region VARCHAR(15)
        DEFAULT 'Moldova' CONSTRAINT nn_region NOT NULL
        CONSTRAINT ck_region CHECK (region IN ('Banat', 
	'Transilvania', 'Dobrogea','Oltenia', 'Muntenia', 'Moldova'))
    ) ;

CREATE TABLE postcodes (
    postCode CHAR(6)
        CONSTRAINT pk_coduri_post PRIMARY KEY
        CONSTRAINT ck_postCode CHECK (postCode=LTRIM(postCode)),
    place VARCHAR(25)
        CONSTRAINT nn_place NOT NULL
        CONSTRAINT ck_place CHECK (place=LTRIM(INITCAP(place))),
    countyCode CHAR(2)
        DEFAULT 'IS' NOT NULL
        CONSTRAINT fk_postcodes_counties REFERENCES counties(countyCode)
    ) ;


CREATE TABLE customers (
    customerId NUMERIC(6)
        CONSTRAINT pk_customers PRIMARY KEY
        CONSTRAINT ck_customerId CHECK (customerId > 1000),
    customerName VARCHAR(30)
        CONSTRAINT ck_customerName CHECK (SUBSTR(customerName,1,1) = UPPER(SUBSTR(customerName,1,1))),
    fiscalCode CHAR(9)
        CONSTRAINT ck_fiscalCode CHECK (SUBSTR(fiscalCode,1,1) = UPPER(SUBSTR(fiscalCode,1,1))),
    address VARCHAR(40)
        CONSTRAINT ck_address_customers CHECK (SUBSTR(address,1,1) = UPPER(SUBSTR(address,1,1))),
    postCode CHAR(6) NOT NULL
        CONSTRAINT fk_customers_postcodes REFERENCES postcodes(postCode),
    phone VARCHAR(10)
    ) ;


CREATE TABLE people (
    personalCode CHAR(14)
        CONSTRAINT pk_people PRIMARY KEY,
 --       CONSTRAINT ck_personalCode CHECK (personalCode=LTRIM(UPPER(personalCode))),
    lastName VARCHAR(20)
        CONSTRAINT ck_lastName CHECK (lastName=LTRIM(INITCAP(lastName))),
    firstName VARCHAR(20)
        CONSTRAINT ck_firstName CHECK (firstName=LTRIM(INITCAP(firstName))),
    address VARCHAR(40)
        CONSTRAINT ck_address_people 
            CHECK (SUBSTR(address,1,1) = UPPER(SUBSTR(address,1,1))),
    genre CHAR(1) DEFAULT 'B'
        CONSTRAINT ck_genre CHECK (genre IN ('F','B')),
    postCode CHAR(6) NOT NULL
        CONSTRAINT fk_people_postcodes REFERENCES postcodes(postCode),
    homePhone VARCHAR(10),
    officePhone VARCHAR(10),
    mobilePhone VARCHAR(10),
    email VARCHAR(50)
    ) ;


CREATE TABLE contacts (
    personalCode CHAR(14)
        CONSTRAINT fk_contacts_people REFERENCES people(personalCode),
    customerId NUMERIC(6) NOT NULL
        CONSTRAINT fk_contacts_customers REFERENCES customers(customerId),
    position VARCHAR(25)
        CONSTRAINT ck_position CHECK (SUBSTR(position,1,1) = 
              UPPER(SUBSTR(position,1,1))),
    CONSTRAINT pk_contacts PRIMARY KEY (personalCode, customerId, position)
    ) ;


CREATE TABLE products (
    productId NUMERIC(6)
        CONSTRAINT pk_products PRIMARY KEY
        CONSTRAINT ck_productId CHECK (productId > 0),
    productName VARCHAR(30) CONSTRAINT ck_productName
        CHECK (SUBSTR(productName,1,1) = UPPER(SUBSTR(productName,1,1))),
    unitOfMeasurement VARCHAR(10),
    category VARCHAR(15) CONSTRAINT ck_products_category
        CHECK (SUBSTR(category,1,1) = UPPER(SUBSTR(category,1,1))),
    VATpercent NUMERIC(2,2) DEFAULT .25
    )  ;


CREATE TABLE invoices (
    invoiceNo NUMERIC(8)
        CONSTRAINT pk_invoices PRIMARY KEY,
    invoiceDate DATE DEFAULT CURRENT_DATE,
        CONSTRAINT ck_invoiceDate CHECK 
          (invoiceDate >= TO_DATE('01/01/2016','DD/MM/YYYY')
            AND invoiceDate <= TO_DATE('31/12/2019','DD/MM/YYYY')),
    customerId NUMERIC(6) NOT NULL
        CONSTRAINT fk_invoices_customers REFERENCES customers(customerId) ,
    comments VARCHAR(50) 
	)  ;
--    invoiceVAT NUMERIC(12,2),	
--    invoiceAmount NUMERIC(12,2),
--    invoiceReceipt NUMERIC(12,2),
--    ) 


CREATE TABLE invoice_details (
    invoiceNo NUMERIC(8) NOT NULL
        CONSTRAINT fk_invoice_details_invoices REFERENCES invoices(invoiceNo),
    invoiceRowNumber NUMERIC(2) NOT NULL
        CONSTRAINT ck_invoiceRowNUMERIC CHECK (invoiceRowNumber > 0),
    productId NUMERIC(6) NOT NULL 
		CONSTRAINT fk_invoice_details_products REFERENCES products(productId),
    quantity NUMERIC(8),
    unitPrice NUMERIC (9,2),
--  invoiceRowVAT NUMERIC(10,2),
    CONSTRAINT pk_invoice_details PRIMARY KEY (invoiceNo,invoiceRowNumber)
    ) ;
    
    
CREATE TABLE receipts (
    receiptId NUMERIC(8)
        CONSTRAINT pk_receipts PRIMARY KEY,
    receiptDate DATE DEFAULT CURRENT_DATE
        CONSTRAINT ck_receiptDate CHECK (receiptDate >= TO_DATE('01/01/2016','DD/MM/YYYY')
            AND receiptDate <= TO_DATE('31/12/2019','DD/MM/YYYY')),
--  receiptType CHAR(1) CONSTRAINT ck_tipinc CHECK (tipinc IN ('P', 'C', 'A')),
    coddoc CHAR(4)
        CONSTRAINT ck_coddoc CHECK(coddoc=UPPER(LTRIM(coddoc))),
    nrdoc VARCHAR(16),
    datadoc DATE DEFAULT CURRENT_DATE - 7
        CONSTRAINT ck_datadoc CHECK (datadoc >= TO_DATE('01/01/2016','DD/MM/YYYY')
            AND datadoc <= TO_DATE('31/12/2019','DD/MM/YYYY'))
    ) ;


CREATE TABLE receipt_details (
    receiptId NUMERIC(8) NOT NULL 
	CONSTRAINT fk_receipt_details_receipts REFERENCES receipts(receiptId) ,
    invoiceNo NUMERIC(8) NOT NULL 
	CONSTRAINT fk_receipt_details_invoices REFERENCES invoices(invoiceNo),
    transa NUMERIC(16,2) NOT NULL,
    CONSTRAINT pk_receipt_details PRIMARY KEY (receiptId, invoiceNo)
     ) ;

