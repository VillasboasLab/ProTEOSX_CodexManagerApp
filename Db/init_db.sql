-- Primary Aggregation Object

CREATE TABLE IF NOT EXISTS `projects` (
	`id` int(10) NOT NULL auto_increment,
	`name` varchar(255),
	`description` text,
	`primary_contact` varchar(255),
	`created_on` date,
	`status` varchar(255),
	PRIMARY KEY( `id` )
);

-- Has many Multiplex Rounds & many ROI's
CREATE TABLE IF NOT EXISTS `acquisitions` (
	`id` int(10) NOT NULL auto_increment,
	`name` varchar(255),
	`thickness` numeric(9,2),
	`date_cut` date,
	`date_stained` date,
	`date_rendered` date,
	`date_processed` date,
	`h2o2_bleached` BOOLEAN,
	`section_to_image_interval` numeric(9,2),
	`z_stacks` numeric(9,2),
	`machine_name` varchar(255),
	`antigen_retrival` varchar(255),
	`sectioning_comment` text,
	`project_id` int,
	FOREIGN KEY (project_id) REFERENCES projects(id),
	PRIMARY KEY( `id` )
);

CREATE TABLE IF NOT EXISTS `antibody_catalogs` (
	`id` int(10) NOT NULL auto_increment,
	`name` varchar(255),
	`catalog_accession` varchar(255),
	`catalog_link` text,
	`conjugation_performed_by` varchar(255),
	`created_on` date,
	`status` varchar(255),
	PRIMARY KEY( `id` )
);


CREATE TABLE IF NOT EXISTS `barcodes` (
	`id` int(10) NOT NULL auto_increment,
	`name` varchar(255),
	`channel` int,
	`reporter_fluorphore` varchar(255),
	PRIMARY KEY( `id` )
);

INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX001',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX002',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX003',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX004',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX005',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX006',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX007',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX010',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX013',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX014',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX015',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX016',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX017',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX019',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX020',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX021',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX022',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX023',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX024',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX025',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX026',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX027',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX028',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX029',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX030',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX032',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX033',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX033',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX034',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX035',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX036',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX037',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX040',2,'AlexaFluor 750');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX041',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX042',4,'Cy5');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX043',3,'CAD570');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX045',4,'Atto 550*');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX046',3,'CAD780');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX047',3,'Atto 550');
INSERT INTO barcodes (name,channel,reporter_fluorphore) VALUES ('BX049',2,'AlexaFluor 750');

CREATE TABLE IF NOT EXISTS `multiplex` (
	`id` int(10) NOT NULL auto_increment,
	`cycle` int,
	`antibody` varchar(255),
	`vendor` varchar(255),
	`ab_clone` varchar(255),
	`ab_concentration` varchar(255),
	`exposure_time` numeric(9,2),
	`acquisition_id` int,
	`barcode_id` int,
	FOREIGN KEY (acquisition_id) REFERENCES acquisitions(id),
	FOREIGN KEY (barcode_id) REFERENCES barcodes(id),
	PRIMARY KEY( `id` )
);

CREATE TABLE IF NOT EXISTS `rois` (
	`id` int(10) NOT NULL auto_increment,
	`name` varchar(255),
	`tma` BOOLEAN,
	`specimen_id` varchar(255),
	`study_id` varchar(255),
	`lab_source` varchar(255),
	`tissue_type` varchar(255),
	`diagnosis` varchar(255),
	`tile_size` varchar(255),
	`acquisition_id` int,
	FOREIGN KEY (acquisition_id) REFERENCES acquisitions(id),
	PRIMARY KEY( `id` )
);

-- PPT metrics belong to 1 Mutliplex Round & 1 ROI
CREATE TABLE IF NOT EXISTS `processor_configurations` (
	`id` int(10) NOT NULL auto_increment,
  `acquisition_fullname` varchar(255),
	`title_date` date,
	`title_fullname` varchar(255),
	`expmeta_name` varchar(255),
	`expmeta_number_of_cycles` varchar(255),
	`expmeta_region_width` varchar(255),
	`expmeta_region_height` varchar(255),
	`expmeta_magnification` varchar(255),
	`expmeta_aperture` varchar(255),
	`expmeta_immersion` varchar(255),
	`expmeta_xy_resolution` varchar(255),
	`expmeta_z_pitch` varchar(255),
	`expmeta_number_of_z_slices` varchar(255),
	`expmeta_tile_overlap` varchar(255),
	`procparams_version` varchar(255),
	`procparams_time` varchar(255),
	`procparams_background_subtraction` varchar(255),
	`procparams_deconvolution` varchar(255),
	`procparams_extended_depth_of_field` varchar(255),
	`procparams_shading_correction` varchar(255),
	`procparams_diagnostic_output` varchar(255),
	`acquisition_id` int,
	`roi_id` int,
	FOREIGN KEY (acquisition_id) REFERENCES acquisitions(id),
	FOREIGN KEY (roi_id) REFERENCES rois(id),
	PRIMARY KEY( `id` )
);

CREATE TABLE IF NOT EXISTS `multiplex_metrics` (
	`id` int(10) NOT NULL auto_increment,
  `reg` int,
	`cyc` int,
	`ch` int,
	`marker` varchar(255),
	`exposure` varchar(255),
	`min` int,
	`median` int,
	`p95` varchar(255),
	`max` int,
	`mean` numeric(9,2),
	`std_dev` numeric(9,2),
	`threshold` varchar(255),
	`area` varchar(255),
	`signal_mean` numeric(9,2),
	`signal_stdev` numeric(9,2),
	`noise_mean` numeric(9,2),
	`noise_stdev` numeric(9,2),
	`snr` numeric(9,2),
	`acquisition_id` int,
	`roi_id` int,
	FOREIGN KEY (acquisition_id) REFERENCES acquisitions(id),
	FOREIGN KEY (roi_id) REFERENCES rois(id),
	PRIMARY KEY( `id` )
);

CREATE TABLE IF NOT EXISTS `metric_images` (
	`id` int(10) NOT NULL auto_increment,
  `title` varchar(255),
  `histogram` LONGTEXT,
  `histogram_dimensions` varchar(255),
  `raw_image` LONGTEXT,
  `raw_dimensions` varchar(255),
  `multiplex_metrics_id` int,
	FOREIGN KEY (multiplex_metrics_id) REFERENCES multiplex_metrics(id),
	PRIMARY KEY( `id` )
);



-- Fully manual interactive feature through app, to record user's impressions
CREATE TABLE IF NOT EXISTS `quality_comments` (
	`id` int(10) NOT NULL auto_increment,
  `qc_grade` varchar(255),
	`comment` varchar(255),
	`created_on` date,
	`user` varchar(255),
	`multiplex_metrics_id` int,
	`roi_id` int,
	FOREIGN KEY (multiplex_metrics_id) REFERENCES multiplex_metrics(id),
	FOREIGN KEY (roi_id) REFERENCES rois(id),
	PRIMARY KEY( `id` )
);

CREATE TABLE IF NOT EXISTS `user_roles` (
	`id` int(10) NOT NULL auto_increment,
  `user` varchar(255),
	`role` varchar(255),
	PRIMARY KEY( `id` )
);
INSERT INTO user_roles (user,role) VALUES ('dev','ADMIN');
INSERT INTO user_roles (user,role) VALUES ('M113984','ADMIN');
INSERT INTO user_roles (user,role) VALUES ('M183437','ADMIN');

CREATE TABLE IF NOT EXISTS `projects_roles` (
  `project_id` int(10),
	`role` varchar(255),
	FOREIGN KEY (project_id) REFERENCES projects(id)
);



