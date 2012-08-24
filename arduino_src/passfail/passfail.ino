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
   switch(in_byte) {
         case 'F': // Fail
            digitalWrite(green_led, LOW);
            digitalWrite(red_led, HIGH);
            delay(250);
            digitalWrite(red_led, LOW);
            delay(250);
            break;
         case 'P':
            digitalWrite(green_led, HIGH);
            digitalWrite(red_led, LOW);
            break;
       }
}

void serialEvent() {
    if(Serial.available() > 0) {
       in_byte = Serial.read();
       Serial.println(in_byte);   
    }
}
