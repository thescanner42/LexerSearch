String v = "first";
String v = "la" + "st";
{
    String v = "out of scope!";
}
int key_size_1024 = 1024;
KeyPairGenerator kpg = KeyPairGenerator.getInstance(v);
KeyPairGenerator kpg = KeyPairGenerator.getInstance("inline");
kpg.initialize(key_size_1024);
int key_size_2048 = 2048;
kpg.initialize(key_size_2048);
kpg.initialize(4096);
kpg.enable();
kpg.enable();
