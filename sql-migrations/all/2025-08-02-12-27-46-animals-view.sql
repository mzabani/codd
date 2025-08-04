CREATE VIEW animals_by_name AS
	SELECT popular_name
	FROM animals
	ORDER BY popular_name;
