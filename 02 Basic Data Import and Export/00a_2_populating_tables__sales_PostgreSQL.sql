-- data loading (populating) of "sales" DB
DELETE FROM receipt_details ;
DELETE FROM receipts ;
DELETE FROM invoice_details ;
DELETE FROM invoices ;
DELETE FROM products ;
DELETE FROM contacts ;
DELETE FROM people ;
DELETE FROM customers ;
DELETE FROM postcodes ;
DELETE FROM counties ;

INSERT INTO counties VALUES ('IS', 'Iasi', 'Moldova') ;
INSERT INTO counties VALUES ('VN', 'Vrancea', 'Moldova') ;
INSERT INTO counties VALUES ('NT', 'Neamt', 'Moldova') ;
INSERT INTO counties VALUES ('SV', 'Suceava', 'Moldova') ;
INSERT INTO counties VALUES ('VS', 'Vaslui', 'Moldova') ;
INSERT INTO counties VALUES ('TM', 'Timis', 'Banat') ;

INSERT INTO postcodes VALUES ('700505', 'Iasi', 'IS')  ;
INSERT INTO postcodes VALUES ('701150', 'Pascani', 'IS') ;
INSERT INTO postcodes VALUES ('706500', 'Vaslui', 'VS') ;
INSERT INTO postcodes VALUES ('705300', 'Focsani', 'VN')  ;
INSERT INTO postcodes VALUES ('706400', 'Birlad', 'VS')  ;
INSERT INTO postcodes VALUES ('705800', 'Suceava', 'SV')  ;
INSERT INTO postcodes VALUES ('705550', 'Roman', 'NT')  ;
INSERT INTO postcodes VALUES ('701900', 'Timisoara', 'TM') ;

INSERT INTO customers VALUES (1001, 'Client 1 SRL', 'R1001', 'Tranzitiei, 13 bis', '700505', NULL) ;
INSERT INTO customers (customerId, customerName, fiscalCode, postCode, phone) VALUES (1002,'Client 2 SA', 'R1002', '700505', '032-212121') ;
INSERT INTO customers VALUES (1003, 'Client 3 SRL', 'R1003', 'Prosperitatii, 22',
    '706500','035-222222') 
 ;
INSERT INTO customers (customerId, customerName, address, postCode)
    VALUES (1004, 'Client 4', 'Sapientei, 56', '701150')
 ;
INSERT INTO customers VALUES (1005, 'Client 5 SRL', 'R1005', NULL,
    '701900', '056-111111')
 ;
INSERT INTO customers VALUES (1006, 'Client 6 SA', 'R1006', 'Pacientei, 33',
    '705550', NULL) 
 ;
INSERT INTO customers VALUES (1007, 'Client 7 SRL', 'R1007', 'Victoria Capitalismului, 2',
    '701900', '056-121212') 
 ;

INSERT INTO people VALUES ('personalCode1', 'Ioan', 'Vasile', 'I.L.Caragiale, 22',	'B',
    '700505', '123456', '987654', '094222222', NULL) 
 ;
INSERT INTO people VALUES ('personalCode2', 'Vasile', 'Ion', NULL,	'B',
    '700505', '234567', '876543', '094222223', 'Ion@a.ro') 
 ;
INSERT INTO people VALUES ('personalCode3', 'Popovici', 'Ioana', 'V.Micle, Bl.I, Sc.B,Ap.2', 'F',
    '701150', '345678', NULL, '094222224', NULL) 
 ;
INSERT INTO people VALUES ('personalCode4', 'Lazar', 'Caraion', 'M.Eminescu, 42', 'B',
    '706500', '456789', NULL, '094222225', NULL) 
 ;
INSERT INTO people VALUES ('personalCode5', 'Iurea', 'Simion', 'I.Creanga, 44 bis', 'B',
    '706500', '567890', '543210', NULL, NULL) 
 ;
INSERT INTO people VALUES ('personalCode6', 'Vasc', 'Simona', 'M.Eminescu, 13', 'F',
    '701150', NULL, '432109', '094222227', NULL) 
 ;
INSERT INTO people VALUES ('personalCode7', 'Popa', 'Ioanid', 'I.Ion, Bl.H2, Sc.C, Ap.45', 'B',
    '701900', '789012', '321098', NULL, NULL) 
 ;
INSERT INTO people VALUES ('personalCode8', 'Bogacs', 'Ildiko', 'I.V.Viteazu, 67', 'F',
    '705550', '890123', '210987', '094222229', NULL) 
 ;
INSERT INTO people VALUES ('personalCode9', 'Ioan', 'Vasilica', 'Garii, Bl.B4, Sc.A, Ap.1', 'F',
    '701900', '901234', '109876', '094222230', NULL) 
 ;

INSERT INTO contacts VALUES ('personalCode1', 1001, 'Director general')
 ;
INSERT INTO contacts VALUES ('personalCode2', 1002, 'Director general')
 ;
INSERT INTO contacts VALUES ('personalCode3', 1002, 'Sef aprovizionare')
 ;
INSERT INTO contacts VALUES ('personalCode4', 1003, 'Sef aprovizionare')
 ;
INSERT INTO contacts VALUES ('personalCode5', 1003, 'Director financiar')
 ;
INSERT INTO contacts VALUES ('personalCode6', 1004, 'Director general')
 ;
INSERT INTO contacts VALUES ('personalCode7', 1005, 'Sef aprovizionare')
 ;
INSERT INTO contacts VALUES ('personalCode8', 1006, 'Director financiar')
 ;
INSERT INTO contacts VALUES ('personalCode9', 1007, 'Sef aprovizionare')
 ;

INSERT INTO products VALUES (1, 'Product 1','b500ml', 'Category A', .24)  ;
INSERT INTO products VALUES (2, 'Product 2','kg', 'Category B', 0.12)  ;
INSERT INTO products VALUES (3, 'Product 3','kg', 'Category C', 0.24)  ;
INSERT INTO products VALUES (4, 'Product 4','l', 'Category B', 0.12)  ;
INSERT INTO products VALUES (5, 'Product 5','unit', 'Category A', .24)  ;
INSERT INTO products VALUES (6, 'Product 6','p250g', 'Category A', .24)  ;

INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1111, DATE'2016-08-01', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (1112, DATE'2016-08-01', 1005, 'Delivery problems');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1113, DATE'2016-08-01', 1002);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1114, DATE'2016-08-01', 1006);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1115, DATE'2016-08-02', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (1116, DATE'2016-08-02', 1007, 'Initial negociated price was modified');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1117, DATE'2016-08-03', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1118, DATE'2016-08-04', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1119, DATE'2016-08-07', 1003);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1120, DATE'2016-08-07', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1121, DATE'2016-08-07', 1004);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (1122, DATE'2016-08-07', 1005);


INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (2111, DATE'2016-08-14', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (2112, DATE'2016-08-14', 1005,
        'Delivery problems');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (2113, DATE'2016-08-14', 1002);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (2115, DATE'2016-08-15', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (2116, DATE'2016-08-15', 1007,
        'Initial negociated price was modified');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId)
    VALUES (2117, DATE'2016-08-16', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId)
    VALUES (2118, DATE'2016-08-16', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId)
    VALUES (2119, DATE'2016-08-21', 1003);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId)
    VALUES (2121, DATE'2016-08-21', 1004);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId)
    VALUES (2122, DATE'2016-08-22', 1005);


INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3111, DATE'2016-09-01', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (3112, DATE'2016-09-01', 1005,
        'Delivery problems');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3113, DATE'2016-09-02', 1002);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3115, DATE'2016-09-02', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId, comments) VALUES (3116, DATE'2016-09-10', 1007,
        'Initial negociated price was modified');
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3117, DATE'2016-09-10', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3118, DATE'2016-09-17', 1001);
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (3119, DATE'2016-10-07', 1003);


-- records for 2017 are generated from 2016, exclusing invoices 1111, 1114, 2113 and 3111
INSERT INTO invoices SELECT invoiceNo + 3000, invoiceDate + 360, customerId, comments FROM invoices 
    WHERE invoiceNo NOT IN (1111, 1114, 2113, 3111);
-- a new invoice for 2017    
INSERT INTO invoices (invoiceNo, invoiceDate, customerId) VALUES (6120, DATE'2017-10-08', 1003);

-- with minor modifications, we do the same for invoices in 2018 

INSERT INTO invoices SELECT invoiceNo + 6000, invoiceDate + 730, customerId, comments FROM invoices 
    WHERE invoiceNo BETWEEN 1111 AND 3500 AND invoiceNo NOT IN (1111, 1115, 2113, 3113);


--
--
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1111, 1, 1, 50, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1111, 2, 2, 75, 1050) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1111, 3, 5, 50, 7060) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1112, 1, 2, 80, 1030) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1112, 2, 3, 40, 750) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1113, 1, 2, 10, 975) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1114, 1, 2, 70, 1070) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1114, 2, 4, 30, 1705) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1114, 3, 5, 70, 7064) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1115, 1, 2, 15, 925) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1116, 1, 2, 12, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1117, 1, 2, 10, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1117, 2, 1, 10, 950) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1118, 1, 2, 30, 1100) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1118, 2, 1, 15, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1119, 1, 2, 35, 1090) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1119, 2, 3, 40, 700) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1119, 3, 4, 50, 1410) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1119, 4, 5, 5, 6300) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1120, 1, 2, 15, 1120) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1121, 1, 5, 5, 7064) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (1121, 2, 2, 10, 1050) ;

INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2111, 1, 1, 45, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2111, 2, 2, 60, 1050) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2111, 3, 5, 10, 7060) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2112, 1, 2, 15, 1030) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2112, 2, 3, 25, 750) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2113, 1, 2, 12, 975) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2115, 1, 2, 11, 925) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2116, 1, 2, 13, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2117, 1, 2, 15, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2117, 2, 1, 110, 950) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2118, 1, 2, 39, 1100) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2118, 2, 1, 20, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2119, 1, 2, 35, 1090) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2119, 2, 3, 40, 700) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2119, 3, 4, 55, 1410) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2119, 4, 5, 55, 6300) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2121, 1, 5, 50, 7064) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (2121, 2, 2, 11, 1050) ;


INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3111, 1, 1, 57, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3111, 2, 2, 79, 1050) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3111, 3, 5, 14, 7060) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3112, 1, 2, 18, 1030) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3112, 2, 3, 65, 750) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3113, 1, 2, 12, 975) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3115, 1, 2, 10, 925) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3116, 1, 2, 13, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3117, 1, 2, 15, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3117, 2, 1, 11, 950) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3118, 1, 2, 39, 1100) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3118, 2, 1, 17, 930) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3119, 1, 2, 25, 1090) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3119, 2, 3, 40, 700) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3119, 3, 4, 35, 1410) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (3119, 4, 5, 9, 6300) ;


-- similar to "invoices", records for 2017 are generated from 2016, excluding invoices 1111, 1114, 2113 si 3111
INSERT INTO invoice_details SELECT invoiceNo + 3000, invoiceRowNumber, productId, quantity, unitPrice FROM invoice_details 
            WHERE invoiceNo NOT IN (1111, 1114, 2113, 3111);
-- two records for invoice 6120    
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (6120, 1, 1, 50, 1000) ;
INSERT INTO invoice_details (invoiceNo, invoiceRowNumber, productId, quantity, unitPrice) VALUES (6120, 2, 2, 75, 1050) ;

-- records for 2018 (based on 2016)
INSERT INTO invoice_details SELECT invoiceNo + 6000, invoiceRowNumber, productId, quantity, unitPrice FROM invoice_details 
    WHERE invoiceNo BETWEEN 1111 AND 3500 AND invoiceNo NOT IN (1111, 1115, 2113, 3113);


INSERT INTO receipts VALUES (1234, DATE'2016-08-15', 'OP', '111', DATE'2016-08-10' ) ;
INSERT INTO receipts VALUES (1235, DATE'2016-08-15', 'CHIT', '222', DATE'2016-08-15') ;
INSERT INTO receipts VALUES (1236, DATE'2016-08-16', 'OP', '333', DATE'2016-08-09') ;
INSERT INTO receipts VALUES (1237, DATE'2016-08-17', 'CEC', '444', DATE'2016-08-10') ;
INSERT INTO receipts VALUES (1238, DATE'2016-08-17', 'OP', '555', DATE'2016-08-10') ;
INSERT INTO receipts VALUES (1239, DATE'2016-08-18', 'OP', '666', DATE'2016-08-11') ;

INSERT INTO receipt_details VALUES (1234, 1111, 53996) ;
INSERT INTO receipt_details VALUES (1234, 1118, 101975) ;
INSERT INTO receipt_details VALUES (1235, 1112, 125516) ;
INSERT INTO receipt_details VALUES (1236, 1117, 9754) ;
INSERT INTO receipt_details VALUES (1236, 1118, 100000) ;
INSERT INTO receipt_details VALUES (1236, 1120, 7315) ;
INSERT INTO receipt_details VALUES (1237, 1117, 9754) ;
INSERT INTO receipt_details VALUES (1238, 1113, 106275) ;
INSERT INTO receipt_details VALUES (1239, 1117, 3696) ;


