DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS parking_spots;
DROP TABLE IF EXISTS shipments;
DROP TABLE IF EXISTS parking_lots;
DROP TABLE IF EXISTS cars;

CREATE TABLE IF NOT EXISTS users (
	id INT NOT NULL,
	username TEXT NOT NULL,
	passwordHash TEXT NOT NULL
);

INSERT INTO users (id, username, passwordHash)
VALUES (1, 'admin', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

CREATE TABLE IF NOT EXISTS parking_lots (
	id SERIAL PRIMARY KEY,
	latitude DOUBLE PRECISION NOT NULL,
	longitude DOUBLE PRECISION NOT NULL,
	city TEXT NOT NULL
);

INSERT INTO parking_lots (latitude, longitude, city)
VALUES (20.0, 10.0, 'Fictional City');

CREATE TABLE IF NOT EXISTS cars (
	id TEXT NOT NULL PRIMARY KEY,
	manufacturer TEXT,
	model TEXT NOT NULL,
	color TEXT NOT NULL
);

INSERT INTO cars (id, manufacturer, model, color)
VALUES ('some-id-1', 'Ravon', 'Cobalt', 'white'),
('some-id-2', 'Chevrolet', 'Gentra', 'grey');

CREATE TABLE IF NOT EXISTS parking_spots (
	car_id TEXT NOT NULL,
	parking_lot_id INT NOT NULL,
	latitude DOUBLE PRECISION NOT NULL,
	longitude DOUBLE PRECISION NOT NULL,
	FOREIGN KEY(car_id) REFERENCES cars(id) ON DELETE CASCADE,
	FOREIGN KEY(parking_lot_id) REFERENCES parking_lots(id) ON DELETE CASCADE
);

INSERT INTO parking_spots (car_id, parking_lot_id, latitude, longitude)
VALUES ('some-id-1', 1, 20.1, 10.1);


CREATE TABLE IF NOT EXISTS shipments (
	car_id TEXT NOT NULL,
	destination TEXT NOT NULL,
	shipment_mode TEXT NOT NULL,
	shipment_state TEXT NOT NULL
);

INSERT INTO shipments (car_id, destination, shipment_mode, shipment_state)
VALUES ('some-id-1', 'Tashkent, Almazar District', 'train', 'on_parking');
