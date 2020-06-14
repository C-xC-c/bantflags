DROP DATABASE IF EXISTS `bantflags`;
CREATE DATABASE `bantflags`;

CREATE USER IF NOT EXISTS flags@localhost IDENTIFIED BY 'default';
GRANT ALL PRIVILEGES ON bantflags.* TO flags@localhost;
FLUSH PRIVILEGES;

USE `bantflags`;

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

INSERT INTO `flags` (`flag`) VALUES ('empty, or there were errors. Re-set your flags.');
