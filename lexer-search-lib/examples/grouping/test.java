String transportation_method = "bicycle";
Tranport transport = TransportGenerator.create(transportation_method);
Tranport transport2 = TransportGenerator.create(transportation_method);

transport.travel(100);
transport.travel(200);
transport.color(red);
transport.color(blue);

transport2.color(red);
