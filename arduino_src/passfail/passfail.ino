int in_byte;
int red_led = 7;
int green_led = 4;

void setup() {
  Serial.begin(9600);
  pinMode(red_led, OUTPUT);
  pinMode(green_led, OUTPUT);
  digitalWrite(green_led, HIGH);
}

void loop() {
    if(Serial.available() > 0) {
       in_byte = Serial.read();
       Serial.println(in_byte);
       switch(in_byte) {
         case 'F': // Fail
            digitalWrite(red_led, HIGH);
            digitalWrite(green_led, LOW);
            break;
         case 'P':
            digitalWrite(green_led, HIGH);
            digitalWrite(red_led, LOW);
            break;
       }
    }
}
