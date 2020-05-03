DROP DATABASE IF EXISTS `bantflags`;
CREATE DATABASE `bantflags`;

USE `bantflags`;

CREATE USER IF NOT EXISTS flags@localhost IDENTIFIED BY 'default';
GRANT ALL PRIVILEGES ON bantflags.* TO flags@localhost;
FLUSH PRIVILEGES;

CREATE TABLE `flags` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `flag` varchar(100) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `flag` (`flag`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `posts` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `post_nr` int(10) NOT NULL DEFAULT '0',
  `board` varchar(10) NOT NULL DEFAULT 'bant',
  PRIMARY KEY (`id`),
  UNIQUE KEY `post_nr_board` (`post_nr`,`board`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `postflags` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `post_nr` int(10) NOT NULL DEFAULT '0',
  `flag` int(10) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `flag` (`flag`),
  KEY `post_nr` (`post_nr`),
  CONSTRAINT `flag` FOREIGN KEY (`flag`) REFERENCES `flags` (`id`) ON DELETE CASCADE,
  CONSTRAINT `post_nr` FOREIGN KEY (`post_nr`) REFERENCES `posts` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

DELIMITER $$
  CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_post`(
    IN `@post_nr` INT,
    IN `@board` VARCHAR(5)
  )
  LANGUAGE SQL
  NOT DETERMINISTIC
  CONTAINS SQL
  SQL SECURITY DEFINER
  BEGIN
    INSERT IGNORE INTO `posts` (`post_nr`, `board`) VALUES (`@post_nr`, `@board`);
  END
$$

CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_post_flags`(
  IN `@post_nr` INT,
  IN `@flag` VARCHAR(100)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
BEGIN
  insert into postflags (post_nr, flag) VALUES (
    (select id from posts where post_nr = `@post_nr`),
    (select id from flags where flag = `@flag`)
  );
END
$$

CREATE DEFINER=`flags`@`localhost` PROCEDURE `rename_flag`(
  IN `@old` VARCHAR(100),
  IN `@new` VARCHAR(100)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
BEGIN
  UPDATE flags SET flags.flag = `@new` WHERE flags.flag = `@old`;
END
$$

CREATE DEFINER=`flags`@`localhost` PROCEDURE `delete_flag`(
  IN `@flag` VARCHAR(100)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
BEGIN
  DELETE flags.* FROM flags WHERE flags.flag = `@flag`;
END
$$

CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_flag`(
  IN `@flag` VARCHAR(100)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
BEGIN
  INSERT INTO `flags` (`flag`) VALUES (`@flag`);
END
$$
DELIMITER ;
