#define DEBUG   0

#define FPIN    8   // first Pin
#define LPIN    13  // last Pin

void setup() {
    int i = 0;

    Serial.begin(9600);
    for (i = FPIN; i <= LPIN; i++)
        pinMode(i, OUTPUT);
}

void loop() {
    int val = 0;
    int inb = 0;

    if (Serial.available() > 0) {
        inb = Serial.read();

#ifdef DEBUG
        // for debugging from the serial monitor
        if ( (inb >= '0') && (inb <= '9'))
            inb -= '0';
#endif /* DEBUG */

        for (val = 0; val <= LPIN - FPIN; val++) 
            digitalWrite(val + FPIN, ( (inb & (1 << val)) ? HIGH : LOW));
    }
}
