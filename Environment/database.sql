CREATE DATABASE IF NOT EXISTS bantflags;

USE bantflags;

CREATE USER IF NOT EXISTS flags@localhost IDENTIFIED BY 'default';
GRANT ALL PRIVILEGES ON bantflags.* TO flags@localhost;
FLUSH PRIVILEGES;


CREATE TABLE IF NOT EXISTS `flags` (
	`id` INT(10) NOT NULL AUTO_INCREMENT,
	`flag` VARCHAR(100) NOT NULL DEFAULT '0',
	PRIMARY KEY (`id`),
	UNIQUE INDEX `flag` (`flag`)
)
COLLATE='utf8_general_ci'
ENGINE=InnoDB
AUTO_INCREMENT=0
;


CREATE TABLE IF NOT EXISTS `posts` (
	`id` INT(10) NOT NULL AUTO_INCREMENT,
	`post_nr` INT(10) NOT NULL DEFAULT '0',
	`board` VARCHAR(5) NOT NULL DEFAULT 'bant',
	PRIMARY KEY (`id`),
	UNIQUE INDEX `post_nr` (`post_nr`)
)
COLLATE='utf8_general_ci'
ENGINE=InnoDB
AUTO_INCREMENT=0
;


CREATE TABLE IF NOT EXISTS `postflags` (
	`id` INT(10) NOT NULL AUTO_INCREMENT,
	`post_nr` INT(10) NOT NULL DEFAULT '0',
	`flag` INT(10) NOT NULL DEFAULT '0',
	PRIMARY KEY (`id`),
	INDEX `flag` (`flag`),
	INDEX `post_nr` (`post_nr`),
	CONSTRAINT `flag` FOREIGN KEY (`flag`) REFERENCES `flags` (`id`) ON DELETE CASCADE,
	CONSTRAINT `post_nr` FOREIGN KEY (`post_nr`) REFERENCES `posts` (`id`)
)
COLLATE='utf8_general_ci'
ENGINE=InnoDB
AUTO_INCREMENT=0
;


DROP PROCEDURE IF EXISTS insert_post;
DELIMITER $$
CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_post`(
	IN `@post_nr` INT,
	IN `@board` VARCHAR(5)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
COMMENT ''
BEGIN
	INSERT IGNORE INTO `posts` (`post_nr`, `board`) VALUES (`@post_nr`, `@board`);
END
$$
DELIMITER ;


DROP PROCEDURE IF EXISTS insert_post_flags;
DELIMITER $$
CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_post_flags`(
	IN `@post_nr` INT,
	IN `@flag` VARCHAR(100)
)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
COMMENT ''
BEGIN
insert into postflags (post_nr, flag) VALUES (
(select id from posts where post_nr = `@post_nr`),
(select id from flags where flag = `@flag`)
);
END
$$
DELIMITER ;


DROP PROCEDURE IF EXISTS rename_flag;
DELIMITER $$
CREATE DEFINER=`flags`@`localhost` PROCEDURE `rename_flag`(
	IN `@old` VARCHAR(100),
	IN `@new` VARCHAR(100)

)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
COMMENT ''
BEGIN
	UPDATE flags SET flags.flag = `@new` WHERE flags.flag = `@old`;
END
$$
DELIMITER ;


DROP PROCEDURE IF EXISTS delete_flag;
DELIMITER $$
CREATE DEFINER=`flags`@`localhost` PROCEDURE `delete_flag`(
	IN `@flag` VARCHAR(100)

)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
COMMENT ''
BEGIN
	DELETE flags.* FROM flags WHERE flags.flag = `@flag`;
END
$$
DELIMITER ;


DROP PROCEDURE IF EXISTS insert_flag;
DELIMITER $$
CREATE DEFINER=`flags`@`localhost` PROCEDURE `insert_flag`(
	IN `@flag` VARCHAR(100)


)
LANGUAGE SQL
NOT DETERMINISTIC
CONTAINS SQL
SQL SECURITY DEFINER
COMMENT ''
BEGIN
	INSERT INTO `flags` (`flag`) VALUES (`@flag`);
END
$$
DELIMITER ;