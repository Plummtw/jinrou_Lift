LOAD DATA INFILE 'D:/Lift/list0.gsp'
INTO TABLE jinrou.usericon
CHARACTER SET UTF8
FIELDS TERMINATED BY ',' ENCLOSED BY "'"
LINES TERMINATED BY '\r\n'
(icon_name,icon_filename,icon_width,icon_height,color)
SET icon_group = '0',created = CURRENT_TIMESTAMP;


LOAD DATA INFILE 'D:/Lift/list1.gsp'
INTO TABLE jinrou.usericon
CHARACTER SET UTF8
FIELDS TERMINATED BY ',' ENCLOSED BY "'"
LINES TERMINATED BY '\r\n'
(icon_name,icon_filename,icon_width,icon_height,color)
SET icon_group = '1',created = CURRENT_TIMESTAMP;

LOAD DATA INFILE 'D:/Lift/list2.gsp'
INTO TABLE jinrou.usericon
CHARACTER SET UTF8
FIELDS TERMINATED BY ',' ENCLOSED BY "'"
LINES TERMINATED BY '\r\n'
(icon_name,icon_filename,icon_width,icon_height,color)
SET icon_group = '2',created = CURRENT_TIMESTAMP;
