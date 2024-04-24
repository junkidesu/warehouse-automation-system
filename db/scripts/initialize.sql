DROP TABLE IF EXISTS placements;
DROP TABLE IF EXISTS parking_spots;
DROP TABLE IF EXISTS parking_lots;
DROP TABLE IF EXISTS cars;

CREATE TABLE IF NOT EXISTS parking_lots (
	id SERIAL PRIMARY KEY,
	city TEXT NOT NULL,
	n_rows INT NOT NULL,
	n_columns INT NOT NULL
);

CREATE TABLE IF NOT EXISTS parking_spots (
	n_row INT NOT NULL,
	n_column INT NOT NULL,
	parking_lot_id INT NOT NULL,
	number INT,
	latitude FLOAT,
	longitude FLOAT,
	FOREIGN KEY(parking_lot_id) REFERENCES parking_lots(id) ON DELETE CASCADE,
	UNIQUE (parking_lot_id, number)
);

CREATE TABLE IF NOT EXISTS cars (
	id TEXT NOT NULL PRIMARY KEY,
	model TEXT NOT NULL,
	color TEXT NOT NULL,
	destination TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS car_events ();

CREATE TABLE IF NOT EXISTS placements (
	car_id TEXT NOT NULL,
	parking_lot_id INT NOT NULL,
	number INT NOT NULL,
	FOREIGN KEY(car_id) REFERENCES cars(id) ON DELETE CASCADE,
	FOREIGN KEY(parking_lot_id, number) REFERENCES parking_spots(parking_lot_id, number) ON DELETE CASCADE
);

CREATE TYPE shipment_type AS ENUM ('truck', 'train', 'buyer');
